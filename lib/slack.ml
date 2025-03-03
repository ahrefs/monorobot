open Printf
open Devkit
open Common
open Util
open Mrkdwn
open Github_j
open Slack_j

let log = Log.from "slack"

type file = {
  name : string;
  alt_txt : string option;
  content : string;
  title : string option;
}

type file_req = {
  files : file list;
  channel_id : Slack_t.channel_id option;
  initial_comment : string option;
  thread_ts : Slack_t.timestamp option;
}

let empty_attachments =
  {
    mrkdwn_in = None;
    fallback = None;
    color = None;
    pretext = None;
    author_name = None;
    author_link = None;
    author_icon = None;
    title = None;
    title_link = None;
    text = None;
    fields = None;
    image_url = None;
    thumb_url = None;
    ts = None;
    footer = None;
  }

let pp_link ~url text = sprintf "<%s|%s>" url (Mrkdwn.escape_mrkdwn text)

let show_labels = function
  | [] -> None
  | (labels : label list) ->
    Some (sprintf "**Labels**: %s" @@ String.concat ", " (List.map (fun (x : label) -> sprintf "*%s*" x.name) labels))

let pluralize ~suf ~len name = if len = 1 then sprintf "%s" name else String.concat "" [ name; suf ]

let markdown_text_attachment ~footer markdown_body =
  [
    {
      empty_attachments with
      mrkdwn_in = Some [ "text" ];
      color = Some "#ccc";
      footer;
      text = Some (Mrkdwn.mrkdwn_of_markdown markdown_body);
    };
  ]

let make_message ?username ?text ?attachments ?blocks ?thread ?handler ?(reply_broadcast = false) ~channel () =
  ( {
      channel;
      thread_ts = Option.map (fun (t : State_t.slack_thread) -> t.ts) thread;
      text;
      attachments;
      blocks;
      username;
      unfurl_links = Some false;
      unfurl_media = None;
      reply_broadcast;
    },
    handler )

let github_handle_regex = Re2.create_exn {|\B@([[:alnum:]][[:alnum:]-]{1,38})\b|}
(* Match GH handles in messages - a GitHub handle has at most 39 chars and no underscore *)

let add_slack_mentions_to_body slack_match_func body =
  let replace_match m =
    let gh_handle = Re2.Match.get_exn ~sub:(`Index 0) m in
    let gh_handle_without_at = Re2.Match.get_exn ~sub:(`Index 1) m in
    match slack_match_func gh_handle_without_at with
    | None -> gh_handle
    | Some user_id -> sprintf "<@%s>" (Slack_user_id.project user_id)
  in
  Re2.replace_exn github_handle_regex body ~f:replace_match

let format_attachments ~slack_match_func ~footer ~body =
  let format_mention_in_markdown (md : unfurl) =
    { md with text = Option.map (add_slack_mentions_to_body slack_match_func) md.text }
  in
  match body with
  | None | Some "" (* GitHub sometimes sends empty string as comment body *) -> None
  | Some body ->
    let attachments = markdown_text_attachment ~footer body in
    Some (List.map format_mention_in_markdown attachments)

let thread_state_handler ~ctx ~channel ~repo_url ~html_url action (response : Slack_t.post_message_res) =
  Lwt.return_ok
  @@
  match action with
  | `Add ->
    State.add_thread_if_new ctx.Context.state ~repo_url ~pr_url:html_url
      { cid = response.channel; channel; ts = response.ts }
  | `Delete -> State.delete_thread ctx.state ~repo_url ~pr_url:html_url
  | `Noop -> ()

let thread_state_action_of_pr_action : pr_action -> _ = function
  | Opened | Ready_for_review | Labeled
  | Reopened (* thread state deleted when PR closed, so need to re-add on reopen *) ->
    `Add
  | Closed -> `Delete
  | _ -> `Noop

let thread_state_action_of_issue_action : issue_action -> _ = function
  | Opened | Labeled | Reopened (* thread state deleted when PR closed, so need to re-add on reopen *) -> `Add
  | Closed -> `Delete
  | _ -> `Noop

let generate_pull_request_notification ~slack_match_func ~(ctx : Context.t) notification channel =
  let { action; number; sender; pull_request; repository } = notification in
  let ({ body; title; html_url; labels; merged; _ } : pull_request) = pull_request in
  let action_label, body =
    match action with
    | Opened | Ready_for_review ->
      let labels_banner = show_labels labels in
      ( "opened",
        body
        |> Option.map (fun body' ->
               Option.map_default (fun labels' -> sprintf "%s\n%s" body' labels') body' labels_banner) )
    | Closed -> (if merged then "merged" else "closed"), None
    | Reopened -> "reopened", None
    | Labeled -> "labeled", show_labels labels
    | _ ->
      invalid_arg
        (sprintf "Monorobot doesn't know how to generate notification for the unexpected event %s"
           (string_of_pr_action action))
  in
  let thread = State.get_thread ctx.state ~repo_url:repository.url ~pr_url:html_url channel in
  let summary =
    sprintf "<%s|[%s]> Pull request #%d %s %s by *%s*" repository.url repository.full_name number
      (pp_link ~url:html_url title) action_label sender.login
  in
  let attachments = format_attachments ~slack_match_func ~footer:None ~body in
  let handler =
    thread_state_handler ~ctx ~channel ~repo_url:repository.url ~html_url (thread_state_action_of_pr_action action)
  in
  let reply_broadcast =
    (* for closed/merged notifications, we want to notify the channel *)
    match action with
    | Closed -> true
    | _ -> false
  in
  make_message ~text:summary ?attachments ?thread ~handler ~reply_broadcast ~channel ()

let generate_pr_review_notification ~slack_match_func ~(ctx : Context.t) notification channel =
  let { action; sender; pull_request; review; repository } = notification in
  let ({ number; title; html_url; _ } : pull_request) = pull_request in
  let action_str =
    match action with
    | Submitted ->
      (match review.state with
      | "commented" -> "reviewed"
      | "approved" -> "approved"
      | "changes_requested" -> "requested changes on"
      | _ -> invalid_arg (sprintf "Error: unexpected review state %s" review.state))
    | _ ->
      invalid_arg
        (sprintf "Monorobot doesn't know how to generate notification for the unexpected event %s"
           (string_of_pr_review_action action))
  in
  let summary =
    sprintf "<%s|[%s]> *%s* <%s|%s> #%d %s" repository.url repository.full_name sender.login review.html_url action_str
      number (pp_link ~url:html_url title)
  in
  let thread = State.get_thread ctx.state ~repo_url:repository.url ~pr_url:html_url channel in
  make_message ~text:summary ?thread ~reply_broadcast:true
    ?attachments:(format_attachments ~slack_match_func ~footer:None ~body:review.body)
    ~channel ()

let generate_pr_review_comment_notification ~slack_match_func ~(ctx : Context.t) notification channel =
  let { action; pull_request; sender; comment; repository } = notification in
  let ({ number; title; html_url; _ } : pull_request) = pull_request in
  let action_str =
    match action with
    | Created -> "commented"
    | _ ->
      invalid_arg
        (sprintf "Monorobot doesn't know how to generate notification for the unexpected event %s"
           (string_of_comment_action action))
  in
  let summary =
    sprintf "<%s|[%s]> *%s* %s on #%d %s" repository.url repository.full_name sender.login action_str number
      (pp_link ~url:html_url title)
  in
  let file =
    match comment.path with
    | None -> None
    | Some a -> Some (sprintf "Commented in file <%s|%s>" comment.html_url a)
  in
  let thread = State.get_thread ctx.state ~repo_url:repository.url ~pr_url:html_url channel in
  make_message ~text:summary ?thread ~reply_broadcast:false (* send comments to thread only *)
    ?attachments:(format_attachments ~slack_match_func ~footer:file ~body:(Some comment.body))
    ~channel:(Slack_channel.to_any channel) ()

let generate_issue_notification ~slack_match_func ~ctx notification channel =
  let ({ action; sender; issue; repository } : issue_notification) = notification in
  let { number; body; title; html_url; labels; _ } = issue in
  let action_label, body =
    match action with
    | Opened -> "opened", body
    | Closed -> "closed", None
    | Reopened -> "reopened", None
    | Labeled -> "labeled", show_labels labels
    | _ ->
      invalid_arg
        (sprintf "Monorobot doesn't know how to generate notification for the unexpected event %s"
           (string_of_issue_action action))
  in
  let summary =
    sprintf "<%s|[%s]> Issue #%d %s %s by *%s*" repository.url repository.full_name number (pp_link ~url:html_url title)
      action_label sender.login
  in
  let handler =
    thread_state_handler ~ctx ~channel ~repo_url:repository.url ~html_url (thread_state_action_of_issue_action action)
  in
  let reply_broadcast =
    (* for closed notifications, we want to notify the channel *)
    match action with
    | Closed -> true
    | _ -> false
  in
  make_message ~text:summary
    ?attachments:(format_attachments ~slack_match_func ~footer:None ~body)
    ~handler ~reply_broadcast ~channel ()

let generate_issue_comment_notification ~(ctx : Context.t) ~slack_match_func notification channel =
  let { action; issue; sender; comment; repository } = notification in
  let { number; title; html_url; _ } = issue in
  let action_str =
    match action with
    | Created -> "commented"
    | _ ->
      invalid_arg
        (sprintf
           "Monorobot doesn't know how to generate pull request review comment notification for the unexpected event %s"
           (string_of_comment_action action))
  in
  let summary =
    sprintf "<%s|[%s]> *%s* <%s|%s> on #%d %s" repository.url repository.full_name sender.login comment.html_url
      action_str number (pp_link ~url:issue.html_url title)
  in
  let thread = State.get_thread ctx.state ~repo_url:repository.url ~pr_url:html_url channel in
  make_message ~text:summary ?thread ~reply_broadcast:true
    ?attachments:(format_attachments ~slack_match_func ~footer:None ~body:(Some comment.body))
    ~channel ()

let git_short_sha_hash hash = String.sub hash 0 8

(** pretty print github commit *)
let pp_commit_common url id message author =
  let title = first_line message in
  sprintf "`<%s|%s>` %s - %s" url (git_short_sha_hash id) (escape_mrkdwn @@ title) author

let pp_commit ({ url; id; message; author; _ } : commit) = pp_commit_common url id message (escape_mrkdwn @@ author.name)

let pp_api_commit ({ sha; commit; url; author; _ } : api_commit) =
  match author with
  | Some author ->
    pp_commit_common url sha commit.message
      (sprintf "<%s|%s>" (escape_mrkdwn author.html_url) (escape_mrkdwn commit.author.name))
  | None -> pp_commit_common url sha commit.message commit.author.name

(** pretty print list with previews of each item per line--will always show at most 7 and drop the rest*)
let pp_list_with_previews ~pp_item list =
  let num_items = List.length list in
  (* truncation point depends on line length, but 7+3 lines seems okay for most cases *)
  let num_shown = 7 in
  let dropped = num_items - num_shown in
  if dropped > 3 then begin
    let h, list' = ExtLib.List.split_nth (num_shown / 2) list in
    let t = ExtLib.List.drop dropped list' in
    List.concat [ List.map pp_item h; [ sprintf "+%d more...\n" dropped ]; List.map pp_item t ]
  end
  else List.map pp_item list

let generate_push_notification notification channel =
  let { sender; pusher; created; deleted; forced; compare; commits; repository; _ } = notification in
  let show_descriptive_title_min_commits = 4 in
  let branch_name = Github.commits_branch_of_ref notification.ref in
  let username = sprintf "%s:%s" repository.name branch_name in
  match deleted with
  | true ->
    let deleted_branch_title = sprintf "%s deleted <%s|branch>" sender.login compare in
    make_message ~username ~channel ~text:deleted_branch_title ()
  | false ->
    let commits_preview_lines = pp_list_with_previews ~pp_item:pp_commit commits in
    let num_commits = List.length commits in
    let lines =
      if
        num_commits < show_descriptive_title_min_commits
        && (not forced)
        && (not created)
        && List.for_all (fun commit -> String.equal commit.author.email pusher.email) commits
      then commits_preview_lines
      else begin
        let compare =
          match commits with
          | [ commit ] -> commit.url
          | _ -> compare
        in
        let descriptive_title =
          sprintf "<%s|%i %s> %spushed %sby %s" compare num_commits
            (pluralize ~suf:"s" ~len:num_commits "commit")
            (if forced then "force-" else "")
            (if created then "to new branch " else "")
            sender.login
        in
        descriptive_title :: commits_preview_lines
      end
    in
    make_message ~username ~channel ~text:(String.concat "\n" lines) ()

let buildkite_description_re = Re2.create_exn {|^Build #(\d+)(.*)|}

let generate_status_notification ~(job_log : (string * string) list) ~(cfg : Config_t.config) (n : status_notification)
  channel =
  let { commit; state; description; target_url; context; repository; _ } = n in
  let ({ commit : inner_commit; sha; html_url; author; _ } : status_commit) = commit in
  let ({ message; _ } : inner_commit) = commit in
  let is_buildkite = String.starts_with context ~prefix:"buildkite" in
  let color_info = if state = Success then "good" else "danger" in
  let build_desc =
    match description with
    | None -> ""
    | Some desc ->
    match target_url with
    | None -> desc
    | Some _ when not is_buildkite -> desc
    | Some target_url ->
    (* Specific to buildkite *)
    match Re2.find_submatches_exn buildkite_description_re desc with
    | [| Some _; Some build_nr; Some rest |] ->
      (* We use a zero-with space \u{200B} to prevent slack from interpreting #XXXXXX as a color *)
      sprintf "Build <%s|#\u{200B}%s>%s" target_url build_nr rest
    | _ | (exception _) ->
      (* we either match on the first case or get an exception *)
      desc
  in
  let author_mention =
    (* If the author's email is not associated with a github account the author will be missing.
            Using the information from the commit instead, which should be equivalent. *)
    Option.map_default (fun { login; _ } -> login) commit.author.name author
  in
  let commit_info = [ sprintf "*Commit*: `<%s|%s>` %s" html_url (git_short_sha_hash sha) author_mention ] in
  let branches_info =
    match List.map (fun ({ name } : branch) -> name) n.branches with
    | [] -> [] (* happens when branch is force-pushed by the time CI notification arrives *)
    | notification_branches ->
      let branches =
        match cfg.main_branch_name with
        | Some main when List.mem main notification_branches ->
          (* happens when new branches are branched before CI finishes *)
          [ main ]
        | _ -> notification_branches
      in
      [ sprintf "*%s*: %s" (pluralize ~suf:"es" ~len:(List.length branches) "Branch") (String.concat ", " branches) ]
  in
  let summary =
    let state_info =
      match state with
      | Success -> "succeeded"
      | Failure -> "failed"
      | Error -> "error"
      | _ ->
        invalid_arg
          (sprintf "Monorobot doesn't know how to generate notification for the unexpected event %s of %s"
             (string_of_status_state state) sha)
    in
    let commit_message = first_line message in
    match target_url with
    | None -> sprintf "<%s|[%s]> CI Build Status notification: %s" repository.url repository.full_name state_info
    | Some target_url ->
      let default_summary =
        sprintf "<%s|[%s]> CI Build Status notification for <%s|%s>: %s" repository.url repository.full_name target_url
          context state_info
      in
      if not is_buildkite then default_summary
      else (
        (* Keep only the portion of the url before /builds/... *)
        let pipeline_url =
          match String.split_on_char '/' target_url with
          | "https:" :: "" :: "buildkite.com" :: org :: pipeline :: "builds" :: _ ->
            Some (sprintf "https://buildkite.com/%s/%s" org pipeline)
          | _ -> None
        in
        match pipeline_url with
        | None -> default_summary
        | Some pipeline_url -> sprintf "<%s|[%s]>: %s for \"%s\"" pipeline_url context build_desc commit_message)
  in
  let text = Some (String.concat "\n" @@ List.concat [ commit_info; branches_info ]) in
  let attachment = { empty_attachments with mrkdwn_in = Some [ "fields"; "text" ]; color = Some color_info; text } in
  let reply_of_log log = log |> Text_cleanup.cleanup |> String.split_on_char '\r' |> String.concat "\n" in
  let handler =
    match job_log with
    | [] ->
      log#info "not including the failed jobs logs because there are none";
      None
    | _ :: _ when Status_notification.is_user channel ->
      log#info "failed jobs logs are going to be sent";
      let files =
        job_log
        |> List.map (fun (job_name, job_log) ->
               { name = job_name; title = None; alt_txt = None; content = reply_of_log job_log })
      in
      Some
        (fun (send_file : file:file_req -> (unit, string) result Lwt.t) (res : Slack_t.post_message_res) ->
          let ({ ts; channel; _ } : post_message_res) = res in
          let file = { files; channel_id = Some channel; initial_comment = None; thread_ts = Some ts } in
          send_file ~file)
    | _ ->
      log#info "not including the failed jobs logs because this is not a DM";
      None
  in
  make_message ~text:summary ~attachments:[ attachment ] ?handler
    ~channel:(Status_notification.to_slack_channel channel)
    ()

let generate_failed_build_notification ?slack_user_id ?(is_fix_build_notification = false) ~failed_steps
  (n : Util.Webhook.n) channel =
  let pipeline_name = Util.Webhook.pipeline_name n in
  let summary =
    let n_state =
      match n.build.state with
      | Passed -> "succeeded"
      | Failed -> "failed"
      | Canceled -> "failed (canceled)"
      | _ -> "pending"
    in
    let build_desc = sprintf "Build <%s|#\u{200B}%d> %s" n.build.web_url n.build.number n_state in
    let commit_message = Util.first_line n.build.message in
    match is_fix_build_notification with
    | true -> sprintf "<%s|[%s]>: %s for \"%s\"" n.pipeline.web_url pipeline_name build_desc commit_message
    | false ->
      let repo_url = Util.Build.git_ssh_to_https n.pipeline.repository in
      let commit_url = sprintf "%s/commit/%s" repo_url n.build.sha in
      let author_mention =
        match slack_user_id with
        | Some id -> sprintf " <@%s>" (Slack_user_id.project id)
        | None -> ""
      in
      sprintf "<%s|[%s]>: %s for \"<%s|%s>\"%s" n.pipeline.web_url pipeline_name build_desc commit_url commit_message
        author_mention
  in
  let text =
    match is_fix_build_notification with
    | true -> sprintf "*Commit*: `<%s|%s>`" n.build.web_url (git_short_sha_hash n.build.sha)
    | false ->
      let slack_step_link (s : Buildkite_t.failed_step) =
        let slugify s =
          (* replace spaces and underscores with dashes *)
          String.map
            (function
              | ' ' | '_' -> '-'
              | c -> c)
            (String.lowercase_ascii s)
        in
        let step = Stre.drop_prefix s.name (pipeline_name ^ "/") |> slugify in
        sprintf "<%s|%s>" s.build_url step
      in
      sprintf "*Steps broken*: %s" (String.concat ", " (List.map slack_step_link (FailedStepSet.elements failed_steps)))
  in
  let attachment =
    let color_info = if n.build.state = Passed then "good" else "danger" in
    { empty_attachments with mrkdwn_in = Some [ "fields"; "text" ]; color = Some color_info; text = Some text }
  in
  make_message ~text:summary ~attachments:[ attachment ] ~channel:(Status_notification.to_slack_channel channel) ()

let generate_commit_comment_notification ~slack_match_func api_commit notification channel =
  let { commit; _ } = api_commit in
  let { sender; comment; repository; _ } = notification in
  let commit_id =
    match comment.commit_id with
    | None -> invalid_arg "commit id not found"
    | Some c -> c
  in
  let summary =
    sprintf "<%s|[%s]> *%s* commented on `%s` %s" repository.url repository.full_name sender.login
      (pp_link ~url:comment.html_url (git_short_sha_hash commit_id))
      (first_line (Mrkdwn.escape_mrkdwn commit.message))
  in
  let footer = Option.map (sprintf "New comment by %s in <%s|%s>" sender.login comment.html_url) comment.path in
  make_message ~text:summary
    ?attachments:(format_attachments ~slack_match_func ~footer ~body:(Some comment.body))
    ~channel ()

let validate_signature ?(version = "v0") ?signing_key ~headers body =
  match signing_key with
  | None -> Ok ()
  | Some key ->
  match List.assoc_opt "x-slack-signature" headers with
  | None -> Error "unable to find header X-Slack-Signature"
  | Some signature ->
  match List.assoc_opt "x-slack-request-timestamp" headers with
  | None -> Error "unable to find header X-Slack-Request-Timestamp"
  | Some timestamp ->
    let basestring = sprintf "%s:%s:%s" version timestamp body in
    let expected_signature = sprintf "%s=%s" version (Util.sign_string_sha256 ~key ~basestring) in
    if String.equal expected_signature signature then Ok () else Error "signatures don't match"
