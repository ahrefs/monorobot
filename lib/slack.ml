open Printf
open Base
open Common
open Github_j
open Slack_j

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

let mrkdwn_of_markdown str = String.strip @@ Mrkdwn.mrkdwn_of_markdown str

let mrkdwn_of_markdown_opt = Option.map ~f:mrkdwn_of_markdown

let show_labels = function
  | [] -> None
  | (labels : label list) ->
    Some (sprintf "Labels: %s" @@ String.concat ~sep:", " (List.map ~f:(fun x -> x.name) labels))

let pluralize name num suffix = if num = 1 then sprintf "%s" name else String.concat [ name; suffix ]

let escape = Mrkdwn.escape_mrkdwn

let pp_link url text = sprintf "<%s|%s>" url (escape text)

let pp_repo (repo : repository) = sprintf "[%s]" repo.full_name

let pp_repo_mrkdwn (repo : repository) = pp_link repo.url @@ pp_repo repo

let generate_pull_request_notification notification channel =
  let { action; number; sender; pull_request; repository } = notification in
  let ({ body; title; html_url; labels; _ } : pull_request) = pull_request in
  let action, body =
    match action with
    | Opened -> "opened", Some body
    | Closed -> "closed", None
    | Reopened -> "reopened", None
    | Labeled -> "labeled", show_labels labels
    | _ ->
      invalid_arg
        (sprintf "Monorobot doesn't know how to generate notification for the unexpected event %s"
           (string_of_pr_action action))
  in
  let summary =
    Some
      (sprintf "%s Pull request #%d %s %s by *%s*" (pp_repo_mrkdwn repository) number (pp_link html_url title) action
         sender.login)
  in
  let fallback =
    Some (sprintf "%s Pull request #%d %s %s by %s" (pp_repo repository) number title action sender.login)
  in
  {
    channel;
    text = None;
    attachments =
      Some
        [
          {
            empty_attachments with
            mrkdwn_in = Some [ "text" ];
            fallback;
            color = Some "#ccc";
            pretext = summary;
            text = mrkdwn_of_markdown_opt body;
          };
        ];
    blocks = None;
  }

let generate_pr_review_notification notification channel =
  let { action; sender; pull_request; review; repository } = notification in
  let ({ number; title; html_url; _ } : pull_request) = pull_request in
  let action_str =
    match action with
    | Submitted ->
      ( match review.state with
      | "commented" -> "commented on"
      | "approved" -> "approved"
      | "changes_requested" -> "requested changes on"
      | _ -> invalid_arg (sprintf "Error: unexpected review state %s" review.state)
      )
    | _ ->
      invalid_arg
        (sprintf "Monorobot doesn't know how to generate notification for the unexpected event %s"
           (string_of_pr_review_action action))
  in
  let summary =
    Some
      (sprintf "%s *%s* %s #%d %s" (pp_repo_mrkdwn repository) sender.login (pp_link review.html_url action_str) number
         (pp_link html_url title))
  in
  let fallback = Some (sprintf "%s %s %s #%d %s" (pp_repo repository) sender.login action_str number title) in
  {
    channel;
    text = None;
    attachments =
      Some
        [
          {
            empty_attachments with
            mrkdwn_in = Some [ "text" ];
            fallback;
            color = Some "#ccc";
            pretext = summary;
            text = mrkdwn_of_markdown_opt review.body;
          };
        ];
    blocks = None;
  }

let generate_pr_review_comment_notification notification channel =
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
    Some
      (sprintf "%s *%s* %s on #%d %s" (pp_repo_mrkdwn repository) sender.login action_str number
         (pp_link html_url title))
  in
  let fallback = Some (sprintf "%s %s %s on #%d %s" (pp_repo repository) sender.login action_str number title) in
  let file =
    match comment.path with
    | None -> None
    | Some a -> Some (sprintf "New comment by %s in %s" sender.login (pp_link comment.html_url a))
  in
  {
    channel;
    text = None;
    attachments =
      Some
        [
          {
            empty_attachments with
            mrkdwn_in = Some [ "text" ];
            fallback;
            color = Some "#ccc";
            pretext = summary;
            footer = file;
            text = Some (mrkdwn_of_markdown comment.body);
          };
        ];
    blocks = None;
  }

let generate_issue_notification notification channel =
  let ({ action; sender; issue; repository } : issue_notification) = notification in
  let { number; body; title; html_url; labels; _ } = issue in
  let action, body =
    match action with
    | Opened -> "opened", Some body
    | Closed -> "closed", None
    | Reopened -> "reopened", None
    | Labeled -> "labeled", show_labels labels
    | _ ->
      invalid_arg
        (sprintf "Monorobot doesn't know how to generate notification for the unexpected event %s"
           (string_of_issue_action action))
  in
  let summary =
    Some
      (sprintf "%s Issue #%d %s %s by *%s*" (pp_repo_mrkdwn repository) number (pp_link html_url title) action
         sender.login)
  in
  let fallback = Some (sprintf "%s Issue #%d %s %s by %s" (pp_repo repository) number title action sender.login) in
  {
    channel;
    text = None;
    attachments =
      Some
        [
          {
            empty_attachments with
            mrkdwn_in = Some [ "text" ];
            fallback;
            color = Some "#ccc";
            pretext = summary;
            text = mrkdwn_of_markdown_opt body;
          };
        ];
    blocks = None;
  }

let generate_issue_comment_notification notification channel =
  let { action; issue; sender; comment; repository } = notification in
  let { number; title; _ } = issue in
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
    Some
      (sprintf "%s *%s* %s on #%d %s" (pp_repo_mrkdwn repository) sender.login (pp_link comment.html_url action_str)
         number (pp_link issue.html_url title))
  in
  let fallback = Some (sprintf "%s %s %s on #%d %s" (pp_repo repository) sender.login action_str number title) in
  {
    channel;
    text = None;
    attachments =
      Some
        [
          {
            empty_attachments with
            mrkdwn_in = Some [ "text" ];
            fallback;
            color = Some "#ccc";
            pretext = summary;
            text = Some (mrkdwn_of_markdown comment.body);
          };
        ];
    blocks = None;
  }

let git_short_sha_hash hash = String.sub ~pos:0 ~len:8 hash

let generate_push_notification notification channel =
  let { sender; created; deleted; forced; compare; commits; repository; _ } = notification in
  let commits_branch = Github.commits_branch_of_ref notification.ref in
  let tree_url = String.concat ~sep:"/" [ repository.url; "tree"; Uri.pct_encode commits_branch ] in
  let repo_branch = sprintf "[%s:%s]" repository.name commits_branch in
  let num_commits = sprintf "%d %s" (List.length commits) (pluralize "commit" (List.length commits) "s") in
  let action = if forced then "force-pushed" else "pushed" in
  let dest = if created then "to new branch " else "" in
  let title =
    if deleted then
      sprintf "%s %s deleted branch %s" (pp_link tree_url repository.name) sender.login (pp_link compare commits_branch)
    else
      sprintf "%s %s %s %sby %s" (pp_link tree_url repo_branch) (pp_link compare num_commits) action dest sender.login
  in
  let fallback =
    if deleted then sprintf "%s %s deleted branch %s" (pp_repo repository) sender.login commits_branch
    else sprintf "%s %s %s %sby %s" repo_branch num_commits action dest sender.login
  in
  let commits =
    List.map commits ~f:(fun { url; id; message; author; _ } ->
      let title = escape @@ first_line message in
      sprintf "`%s` %s - %s" (pp_link url (git_short_sha_hash id)) title author.name)
  in
  {
    channel;
    text = None;
    attachments =
      Some
        [
          {
            empty_attachments with
            mrkdwn_in = Some [ "fields" ];
            pretext = Some title;
            fallback = Some fallback;
            color = Some "#ccc";
            fields = Some [ { value = String.concat ~sep:"\n" commits; title = None; short = false } ];
          };
        ];
    blocks = None;
  }

let generate_status_notification (cfg : Config_t.config) (notification : status_notification) channel =
  let { commit; state; description; target_url; context; repository; _ } = notification in
  let ({ commit : inner_commit; sha; author; html_url; _ } : status_commit) = commit in
  let ({ message; _ } : inner_commit) = commit in
  let state_info =
    match state with
    | Success -> "success"
    | Failure -> "failure"
    | Error -> "error"
    | _ ->
      invalid_arg
        (sprintf "Monorobot doesn't know how to generate notification for the unexpected event %s of %s"
           (string_of_status_state state) sha)
  in
  let color_info =
    match state with
    | Success -> "good"
    | _ -> "danger"
  in
  let description_info =
    match description with
    | None -> None
    | Some s -> Some (sprintf "*Description*: %s." s)
  in
  let commit_info =
    [
      sprintf "*Commit*: `%s` %s - %s"
        (pp_link html_url (git_short_sha_hash sha))
        (escape @@ first_line message)
        author.login;
    ]
  in
  let branches_info =
    match List.map ~f:(fun { name } -> name) notification.branches with
    | [] -> [] (* happens when branch is force-pushed by the time CI notification arrives *)
    | notification_branches ->
      let branches =
        match cfg.main_branch_name with
        | Some main when List.mem notification_branches main ~equal:String.equal ->
          (* happens when new branches are branched before CI finishes *)
          [ main ]
        | _ -> notification_branches
      in
      [ sprintf "*%s*: %s" (pluralize "Branch" (List.length branches) "es") (String.concat ~sep:", " branches) ]
  in
  let summary =
    let target = Option.value_map target_url ~default:"" ~f:(fun url -> sprintf " for %s" (pp_link url context)) in
    Some (sprintf "%s CI Build Status notification%s: %s" (pp_repo_mrkdwn repository) target state_info)
  in
  let fallback =
    let target = Option.value_map target_url ~default:"" ~f:(fun _ -> sprintf " for %s" context) in
    Some (sprintf "%s CI Build Status notification%s: %s" (pp_repo repository) target state_info)
  in
  let msg = String.concat ~sep:"\n" @@ List.concat [ commit_info; branches_info ] in
  let attachment =
    {
      empty_attachments with
      mrkdwn_in = Some [ "fields"; "text" ];
      fallback;
      pretext = summary;
      color = Some color_info;
      text = description_info;
      fields = Some [ { title = None; value = msg; short = false } ];
    }
  in
  { channel; text = None; attachments = Some [ attachment ]; blocks = None }

let generate_commit_comment_notification api_commit notification channel =
  let { commit; _ } = api_commit in
  let { sender; comment; repository; _ } = notification in
  let commit_id =
    match comment.commit_id with
    | None -> invalid_arg "commit id not found"
    | Some c -> c
  in
  let summary =
    Some
      (sprintf "%s *%s* commented on `%s` %s" (pp_repo_mrkdwn repository) sender.login
         (pp_link comment.html_url (git_short_sha_hash commit_id))
         (escape @@ first_line commit.message))
  in
  let fallback =
    Some
      (sprintf "%s %s commented on `%s` %s" (pp_repo repository) sender.login (git_short_sha_hash commit_id)
         (first_line commit.message))
  in
  let path =
    match comment.path with
    | None -> None
    | Some p -> Some (sprintf "New comment by %s in %s" sender.login (pp_link comment.html_url p))
  in
  let attachment =
    {
      empty_attachments with
      mrkdwn_in = Some [ "pretext"; "text" ];
      fallback;
      color = Some "#ccc";
      pretext = summary;
      footer = path;
      text = Some (mrkdwn_of_markdown comment.body);
    }
  in
  { channel; text = None; attachments = Some [ attachment ]; blocks = None }

let validate_signature ?(version = "v0") ?signing_key ~headers body =
  match signing_key with
  | None -> Ok ()
  | Some key ->
  match List.Assoc.find headers "x-slack-signature" ~equal:String.equal with
  | None -> Error "unable to find header X-Slack-Signature"
  | Some signature ->
  match List.Assoc.find headers "x-slack-request-timestamp" ~equal:String.equal with
  | None -> Error "unable to find header X-Slack-Request-Timestamp"
  | Some timestamp ->
    let basestring = Printf.sprintf "%s:%s:%s" version timestamp body in
    let expected_signature = Printf.sprintf "%s=%s" version (Common.sign_string_sha256 ~key ~basestring) in
    if String.equal expected_signature signature then Ok () else Error "signatures don't match"
