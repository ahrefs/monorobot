open Devkit
open Slack
open Config_t
open Common
open Github_j

exception Action_error of string
exception Success_handler_error of string

let log = Log.from "action"

let action_error msg = raise (Action_error msg)
let handler_error msg = raise (Success_handler_error msg)

let try_process_notification body n =
  try%lwt n with
  | Yojson.Json_error msg ->
    log#error "failed to parse file as valid JSON (%s): %s" msg body;
    Lwt.return_unit
  | Action_error msg ->
    log#error "action error %s" msg;
    Lwt.return_unit
  | Success_handler_error msg ->
    log#error "success handler error %s" msg;
    Lwt.return_unit
  | Context.Context_error msg ->
    log#error "context error %s" msg;
    Lwt.return_unit
  | Util.Util_error msg ->
    log#error "util error %s" msg;
    Lwt.return_unit

module Action (Github_api : Api.Github) (Slack_api : Api.Slack) (Buildkite_api : Api.Buildkite) = struct
  let canonical_regex = Re2.create_exn {|\.|\-|\+.*|@.*|}
  (* Match email domain, everything after '+', as well as dots and hyphens *)

  let username_to_slack_id_tbl = Stringtbl.empty ()

  let canonicalize_email_username email =
    email |> Re2.rewrite_exn ~template:"" canonical_regex |> String.lowercase_ascii

  let refresh_username_to_slack_id_tbl ~ctx =
    log#info "updating github to slack username mapping";
    match%lwt Slack_api.list_users ~ctx () with
    | Error e ->
      log#warn "couldn't fetch list of Slack users: %s" e;
      Lwt.return_unit
    | Ok res ->
      List.iter
        (fun (user : Slack_t.user) ->
          match user.profile.email with
          | None -> ()
          | Some email ->
            let username = canonicalize_email_username email in
            Stringtbl.replace username_to_slack_id_tbl username user.id)
        res.members;
      Lwt.return_unit

  let rec refresh_username_to_slack_id_tbl_background_lwt ~ctx : unit Lwt.t =
    let%lwt () = refresh_username_to_slack_id_tbl ~ctx in
    let%lwt () = Lwt_unix.sleep (Time.days 1) in
    (* Updates mapping every 24 hours *)
    refresh_username_to_slack_id_tbl_background_lwt ~ctx

  let match_github_login_to_slack_id cfg login =
    let login = List.assoc_opt login cfg.user_mappings |> Option.default login in
    login |> canonicalize_email_username |> Stringtbl.find_opt username_to_slack_id_tbl

  let partition_push (cfg : Config_t.config) n =
    let default = Stdlib.Option.to_list cfg.prefix_rules.default_channel in
    let rules = cfg.prefix_rules.rules in
    let branch = Github.commits_branch_of_ref n.ref in
    let main_branch = if cfg.prefix_rules.filter_main_branch then cfg.main_branch_name else None in
    let filter_by_branch = Rule.Prefix.filter_by_branch ~branch ~main_branch in
    n.commits
    |> List.filter (fun c ->
           let skip = Github.is_merge_commit_to_ignore ~cfg ~branch c in
           if skip then log#info "main branch merge, ignoring %s: %s" c.id (Util.first_line c.message);
           not skip)
    |> List.concat_map (fun commit ->
           let rules = List.filter (filter_by_branch ~distinct:commit.distinct) rules in
           let matched_channel_names =
             Github.modified_files_of_commit commit
             |> List.filter_map (Rule.Prefix.match_rules ~rules)
             |> List.sort_uniq Slack_channel.compare
           in
           let channel_names =
             if matched_channel_names = [] && commit.distinct then default else matched_channel_names
           in
           List.map (fun n -> Slack_channel.to_any n, commit) channel_names)
    |> ChannelMap.of_list_multi
    |> ChannelMap.map (fun commits -> { n with commits })
    |> ChannelMap.to_list

  let partition_label (cfg : Config_t.config) (labels : label list) =
    let rules = cfg.label_rules.rules in
    let channel_names =
      labels |> List.concat_map (Rule.Label.match_rules ~rules) |> List.sort_uniq Slack_channel.compare
    in
    match channel_names with
    | [] -> Option.map_default (fun c -> [ Slack_channel.to_any c ]) [] cfg.label_rules.default_channel
    | channel_names -> List.map Slack_channel.to_any channel_names

  let partition_pr cfg (ctx : Context.t) (n : pr_notification) =
    match n.action with
    | (Opened | Closed | Reopened | Ready_for_review) when not n.pull_request.draft ->
      partition_label cfg n.pull_request.labels
    | Labeled when not n.pull_request.draft ->
      (* labels get notified by gh in addition the pr notification itself, which means that when we open a pr
          we have one `Open` notification and as many `Labeled` notifications as the pr has labels.
          we want to avoid having many notifications for a single `Opened` event. *)
      (match State.has_pr_thread ctx.state ~repo_url:n.repository.url ~pr_url:n.pull_request.html_url with
      | false ->
        (* we dont have a thread for the pr yet, these are the labels notifications before the PR *)
        []
      | true ->
        (* if we already have a thread on a certain channel, we already have received an open PR notification.
           If we have a new label that triggers a notification on a new channel, we'll notify the channel.
           If the label triggers a notification on a channel with an existing thread, the notification will go
           in the thread *)
        partition_label cfg n.pull_request.labels)
    | _ -> []

  let partition_issue cfg (n : issue_notification) =
    match n.action with
    | Opened | Closed | Reopened | Labeled -> partition_label cfg n.issue.labels
    | _ -> []

  let partition_pr_review_comment cfg (n : pr_review_comment_notification) =
    match n.action with
    | Created -> partition_label cfg n.pull_request.labels
    | _ -> []

  let partition_issue_comment cfg (n : issue_comment_notification) =
    match n.action with
    | Created -> partition_label cfg n.issue.labels
    | _ -> []

  let partition_pr_review cfg (n : pr_review_notification) =
    let { review; action; _ } = n in
    match action, review.state, review.body with
    | Submitted, "commented", (Some "" | None) ->
      (* the case (action = Submitted, review.state = "commented", review.body = "") happens when
           a reviewer starts a review by commenting on particular sections of the code, which triggars a pull_request_review_comment event simultaneouly,
           and then submits the review without submitting any general feedback or explicit approval/changes.

           the case (action = Submitted, review.state = "commented", review.body = null) happens when
           a reviewer adds a single comment on a particular section of the code, which triggars a pull_request_review_comment event simultaneouly.

         in both cases, since pull_request_review_comment is already handled by another type of event, information in the pull_request_review payload
         does not provide any insightful information and will thus be ignored. *)
      []
    | Submitted, _, _ -> partition_label cfg n.pull_request.labels
    | _ -> []

  let partition_commit (cfg : Config_t.config) files =
    let rules = cfg.prefix_rules.rules in
    let matched_channel_names =
      List.map (fun f -> f.filename) files
      |> List.filter_map (Rule.Prefix.match_rules ~rules)
      |> List.sort_uniq Slack_channel.compare
    in
    match matched_channel_names with
    | [] -> Option.map_default (fun c -> [ Slack_channel.to_any c ]) [] cfg.prefix_rules.default_channel
    | matched_channel_names -> List.map Slack_channel.to_any matched_channel_names

  let partition_status (ctx : Context.t) (n : status_notification) =
    let open Util.Build in
    let repo = n.repository in
    let cfg = Context.find_repo_config_exn ctx repo.url in
    let repo_state = State.find_or_add_repo ctx.state repo.url in
    let is_main_branch = is_main_branch cfg n in
    let get_dm_id ~notify_dm =
      let email = n.commit.commit.author.email in
      match notify_dm, dm_users_on_failures cfg n with
      | false, _ | _, false -> Lwt.return []
      | _ ->
        (match%lwt Slack_api.lookup_user ~ctx ~cfg ~email () with
        | Ok ({ user = { id; _ } } : Slack_t.lookup_user_res) ->
          (* Check if config holds Github to Slack email mapping for the commit author. The user id we get from slack
             is not an email, so we need to see if we can map the commit author email to a slack user's email. *)
          let author = List.assoc_opt email cfg.user_mappings |> Option.default email in
          let dm_after_failed_build =
            List.assoc_opt author cfg.notifications_configs.dm_after_failed_build
            |> (* dm_after_failed_build is opt in *)
            Option.default false
          in
          let dm_after_failing_build =
            List.assoc_opt author cfg.notifications_configs.dm_for_failing_build
            |> (* dm_for_failing_build is opt out *)
            Option.default true
          in
          let is_failing_build = is_failing_build n in
          let is_failed_build = is_failed_build n in
          (match (dm_after_failing_build && is_failing_build) || (dm_after_failed_build && is_failed_build) with
          | true ->
            (* if we send a dm for a failing build and we want another dm after the build is finished, we don't
               set the pipeline commit immediately. Otherwise, we wouldn't be able to notify later *)
            if (is_failing_build && not dm_after_failed_build) || is_failed_build then
              State.set_repo_pipeline_commit ctx.state n;
            Lwt.return [ Status_notification.User id ]
          | false -> Lwt.return [])
        | Error e ->
          log#warn "couldn't match commit email %s to slack profile: %s" n.commit.commit.author.email e;
          Lwt.return [])
    in
    let get_channel_ids ~notify_channels ~branches =
      match notify_channels, branches with
      | false, _ | _, [] -> Lwt.return []
      | _ ->
      (* non-main branch build notifications go to default channel to reduce spam in topic channels *)
      match is_main_branch with
      | false ->
        Lwt.return
          (Option.map_default (fun c -> [ Status_notification.inject_channel c ]) [] cfg.prefix_rules.default_channel)
      | true ->
        (match%lwt Github_api.get_api_commit ~ctx ~repo ~sha:n.commit.sha with
        | Error e -> action_error e
        | Ok commit ->
          let chans = partition_commit cfg commit.files in
          Lwt.return (List.map Status_notification.inject_channel chans))
    in
    let action_on_match (branches : branch list) ~notify_channels ~notify_dm =
      let%lwt direct_message = get_dm_id ~notify_dm in
      let%lwt chans = get_channel_ids ~notify_channels ~branches in
      Lwt.return (chans @ direct_message)
    in
    let rules = cfg.status_rules.rules in
    match Context.is_pipeline_allowed ctx n with
    | false -> Lwt.return []
    | true ->
    match Rule.Status.match_rules ~rules n with
    | Some (Ignore, _, _) | None -> Lwt.return []
    | Some (Allow, notify_channels, notify_dm) -> action_on_match n.branches ~notify_channels ~notify_dm
    | Some (Allow_once, notify_channels, notify_dm) ->
      let branches =
        match n.target_url with
        | None -> n.branches
        | Some build_url ->
          let status_switched (_ : branch) =
            match Util.Build.get_org_pipeline_build n with
            | Error e ->
              log#error "failed to get org/pipeline/build_nr from build url %s: %s" build_url e;
              false
            | Ok (org, pipeline, _build_nr) ->
              let repo_key = Util.Webhook.repo_key org pipeline in
              (match Stringtbl.find_opt repo_state.failed_steps repo_key with
              | None -> n.state = Github_t.Failure
              | Some failed when FailedStepSet.is_empty failed.steps -> n.state = Failure
              | Some _ -> n.state = Success)
          in
          List.filter status_switched n.branches
      in
      let notify_dm = notify_dm && not (State.mem_repo_pipeline_commits ctx.state n) in
      action_on_match branches ~notify_channels ~notify_dm

  let partition_commit_comment (ctx : Context.t) n =
    let cfg = Context.find_repo_config_exn ctx n.repository.url in
    match n.comment.commit_id with
    | None -> action_error "unable to find commit id for this commit comment event"
    | Some sha ->
      (match%lwt Github_api.get_api_commit ~ctx ~repo:n.repository ~sha with
      | Error e -> action_error e
      | Ok commit ->
        let rules = cfg.prefix_rules.rules in
        (match n.comment.path with
        | None -> Lwt.return (partition_commit cfg commit.files, commit)
        | Some filename ->
        match Rule.Prefix.match_rules filename ~rules with
        | None ->
          let default = Option.map_default (fun c -> [ Slack_channel.to_any c ]) [] cfg.prefix_rules.default_channel in
          Lwt.return (default, commit)
        | Some chan -> Lwt.return ([ Slack_channel.to_any chan ], commit)))

  let ignore_notifications_from_user cfg req =
    let sender_login =
      match req with
      | Github.Issue_comment n ->
        Some n.sender.login
        (*
          | Github.Push n -> Some n.sender.login
          | Pull_request n -> Some n.sender.login
          | PR_review n -> Some n.sender.login
          | PR_review_comment n -> Some n.sender.login
          | Issue n -> Some n.sender.login
          | Commit_comment n -> Some n.sender.login
        *)
      | _ -> None
    in
    match sender_login with
    | Some sender_login -> List.exists (String.equal sender_login) cfg.ignored_users
    | None -> false

  let get_job_log_name_and_content ~ctx n (job : Buildkite_t.job) =
    Lwt_result.map
      (fun (job_log : Buildkite_t.job_log) ->
        let log_name =
          let agent = Option.map_default (fun (agent : Buildkite_t.agent) -> agent.hostname) "unknown" job.agent in
          Printf.sprintf "%s on %s.txt" job.name agent
        in
        log_name, job_log.content)
      (Buildkite_api.get_job_log ~ctx n job)

  let get_logs ~ctx (n : status_notification) =
    let* build_url = Lwt.return @@ Util.Build.get_build_url n in
    let* build = Buildkite_api.get_build ~cache:`Refresh ~ctx build_url in
    let failed_jobs = Util.Build.filter_failed_jobs build.jobs in
    let%lwt logs_or_errors = Lwt_list.map_s (get_job_log_name_and_content ~ctx n) failed_jobs in
    Lwt.return_ok
    @@ List.filter_map
         (function
           | Ok content -> Some content
           | Error e ->
             log#warn "failed to get log content for job: %s" e;
             None)
         logs_or_errors

  let get_logs_if_failed ~ctx (n : status_notification) =
    match n.state with
    | Success | Pending -> Lwt.return_ok []
    | Failure | Error -> get_logs ~ctx n

  let generate_notifications (ctx : Context.t) (req : Github.t) =
    let repo = Github.repo_of_notification req in
    let cfg = Context.find_repo_config_exn ctx repo.url in
    let slack_match_func = match_github_login_to_slack_id cfg in
    match ignore_notifications_from_user cfg req with
    | true -> Lwt.return []
    | false ->
    match req with
    | Github.Push n ->
      partition_push cfg n |> List.map (fun (channel, n) -> generate_push_notification n channel) |> Lwt.return
    | Pull_request n ->
      partition_pr cfg ctx n |> List.map (generate_pull_request_notification ~ctx ~slack_match_func n) |> Lwt.return
    | PR_review n ->
      partition_pr_review cfg n |> List.map (generate_pr_review_notification ~ctx ~slack_match_func n) |> Lwt.return
    | PR_review_comment n ->
      partition_pr_review_comment cfg n
      |> List.map (generate_pr_review_comment_notification ~ctx ~slack_match_func n)
      |> Lwt.return
    | Issue n -> partition_issue cfg n |> List.map (generate_issue_notification ~ctx ~slack_match_func n) |> Lwt.return
    | Issue_comment n ->
      partition_issue_comment cfg n
      |> List.map (generate_issue_comment_notification ~ctx ~slack_match_func n)
      |> Lwt.return
    | Commit_comment n ->
      let%lwt channels, api_commit = partition_commit_comment ctx n in
      let notifs = List.map (generate_commit_comment_notification ~slack_match_func api_commit n) channels in
      Lwt.return notifs
    | Status n ->
      (try%lwt
         let%lwt channels = partition_status ctx n in
         let%lwt job_log =
           match cfg.include_logs_in_notifs && channels <> [] with
           | false -> Lwt.return []
           | true ->
             (match%lwt get_logs_if_failed ~ctx n with
             | Error e ->
               log#warn "couldn't fetch logs for build: %s" e;
               Lwt.return []
             | Ok job_log -> Lwt.return job_log)
         in
         let notifs =
           List.map
             (fun channel ->
               let req, handler = generate_status_notification ~job_log ~cfg n channel in
               req, Option.map (fun handler -> handler (Slack_api.send_file ~ctx)) handler)
             channels
         in
         (* We only care about maintaining status for notifications of allowed pipelines in the main branch *)
         let%lwt () =
           match Util.Build.is_main_branch cfg n && Context.is_pipeline_allowed ctx n with
           | false -> Lwt.return_unit
           | true -> State.set_repo_pipeline_status ctx.state n
         in
         Lwt.return notifs
       with exn ->
         log#error "failed to process status notification %d for %s: %s" n.id (Option.default n.context n.target_url)
           (Printexc.to_string exn);
         (* Backup, in case something went wrong while processing the notification.
            We need to update the pipeline status, otherwise we will get into a bad state *)
         let%lwt () =
           match Util.Build.is_main_branch cfg n && Context.is_pipeline_allowed ctx n with
           | true -> State.set_repo_pipeline_status ctx.state n
           | false -> Lwt.return_unit
         in
         Lwt.return [])
  let send_notifications (ctx : Context.t) notifications =
    let notify (msg, handler) =
      match%lwt Slack_api.send_notification ~ctx ~msg with
      | Ok (Some res) ->
        (match handler with
        | None -> Lwt.return_unit
        | Some handler ->
          (match%lwt handler res with
          | Result.Error e -> handler_error e
          | Ok () -> Lwt.return_unit
          | exception exn -> handler_error (Printexc.to_string exn)))
      | Ok None -> Lwt.return_unit
      | Error e -> action_error e
    in
    Lwt_list.iter_s notify notifications

  let fetch_config ~ctx ~repo =
    match%lwt Github_api.get_config ~ctx ~repo with
    | Ok config ->
      Context.set_repo_config ctx repo.url config;
      Context.print_config ctx repo.url;
      let%lwt () =
        (* initiate/terminate debug db via config change *)
        match config.debug_db, !Database.available with
        | true, Not_available ->
          let db_path = Option.default Database.db_path config.debug_db_path in
          log#info "initializing debug database";
          let%lwt () = Database.init db_path in
          Database.available := Available;
          log#info "debug database initialized at %s" db_path;
          Lwt.return_unit
        | false, Available ->
          Database.available := Not_available;
          log#info "shutting down debug database";
          let%lwt () = Database.Conn.Pool.shutdown (Option.get !Database.pool) in
          log#info "debug database: closed connection pool successfully";
          Lwt.return_unit
        | _, _ -> Lwt.return_unit
      in
      Lwt.return @@ Ok ()
    | Error e -> action_error e

  (** [refresh_repo_config ctx n] fetches the latest repo config if it's
      uninitialized, or if the incoming request [n] is a push
      notification containing commits that touched the config file. *)
  let refresh_repo_config (ctx : Context.t) notification =
    let repo = Github.repo_of_notification notification in
    match Context.find_repo_config ctx repo.url with
    | None -> fetch_config ~ctx ~repo
    | Some _ ->
    match notification with
    | Github.Push commit_pushed_notification ->
      let commits = commit_pushed_notification.commits in
      let modified_files = List.concat_map Github.modified_files_of_commit commits in
      let config_was_modified = List.exists (String.equal ctx.config_filename) modified_files in
      if config_was_modified then fetch_config ~ctx ~repo else Lwt.return @@ Ok ()
    | _ -> Lwt.return @@ Ok ()

  let do_github_tasks ctx (repo : repository) (req : Github.t) =
    let cfg = Context.find_repo_config_exn ctx repo.url in
    let project_owners (pull_request : pull_request) repository number =
      match Github.get_project_owners pull_request cfg.project_owners with
      | Some reviewers ->
        (match%lwt Github_api.request_reviewers ~ctx ~repo:repository ~number ~reviewers with
        | Ok () -> Lwt.return_unit
        | Error e -> action_error e)
      | None -> Lwt.return_unit
    in
    match req with
    | Github.Pull_request
        { action; pull_request = { draft = false; state = Open; _ } as pull_request; repository; number; _ } -> begin
      match action with
      | Ready_for_review | Labeled -> project_owners pull_request repository number
      | _ -> Lwt.return_unit
    end
    | _ -> Lwt.return_unit

  let repo_is_supported secrets (repo : Github_t.repository) =
    List.exists (fun (r : repo_config) -> String.equal r.url repo.url) secrets.repos

  let process_github_notification (ctx : Context.t) headers body =
    let validate_signature secrets payload =
      let repo = Github.repo_of_notification payload in
      let signing_key = Context.gh_hook_secret_token_of_secrets secrets repo.url in
      Github.validate_signature ?signing_key ~headers body
    in
    try_process_notification body
      (let secrets = Context.get_secrets_exn ctx in
       match%lwt Github.parse_exn headers body ~get_build_branch:(Buildkite_api.get_build_branch ~ctx) with
       | exception Failure msg ->
         log#warn "skipping event : %s" msg;
         Lwt.return_unit
       | payload ->
       match validate_signature secrets payload with
       | Error e -> action_error e
       | Ok () ->
         let repo = Github.repo_of_notification payload in
         (match repo_is_supported secrets repo with
         | false -> action_error @@ Printf.sprintf "unsupported repository %s" repo.url
         | true ->
           (match%lwt refresh_repo_config ctx payload with
           | Error e -> action_error e
           | Ok () ->
             let%lwt notifications = generate_notifications ctx payload in
             let%lwt () = Lwt.join [ send_notifications ctx notifications; do_github_tasks ctx repo payload ] in
             (match ctx.state_filepath with
             | None -> Lwt.return_unit
             | Some path ->
             match State.save ctx.state path with
             | Ok () -> Lwt.return_unit
             | Error e -> action_error e))))

  let process_link_shared_event (ctx : Context.t) (event : Slack_t.link_shared_event) =
    let fetch_bot_user_id () =
      match%lwt Slack_api.send_auth_test ~ctx () with
      | Ok { user_id; _ } ->
        State.set_bot_user_id ctx.state user_id;
        let%lwt () =
          ctx.state_filepath
          |> Option.map_default
               (fun path ->
                 match State.save ctx.state path with
                 | Ok () -> Lwt.return_unit
                 | Error msg ->
                   log#warn "failed to save state file %s : %s" path msg;
                   Lwt.return_unit)
               Lwt.return_unit
        in
        Lwt.return_some user_id
      | Error msg ->
        log#warn "failed to query slack auth.test : %s" msg;
        Lwt.return_none
    in
    let process link =
      let with_gh_result_populate_slack (type a) ~(api_result : (a, string) Result.t) ~populate ~repo =
        match api_result with
        | Error msg ->
          log#warn "failed to fetch info from github for %s: %s" link msg;
          Lwt.return_none
        | Ok item -> Lwt.return_some @@ (link, populate repo item)
      in
      match Github.gh_link_of_string link with
      | None -> Lwt.return_none
      | Some (repo, gh_resource) ->
      match gh_resource with
      | Pull_request number ->
        let%lwt result = Github_api.get_pull_request ~ctx ~repo ~number in
        with_gh_result_populate_slack ~api_result:result ~populate:Slack_message.populate_pull_request ~repo
      | Issue number ->
        let%lwt result = Github_api.get_issue ~ctx ~repo ~number in
        with_gh_result_populate_slack ~api_result:result ~populate:Slack_message.populate_issue ~repo
      | Commit sha ->
        let%lwt result = Github_api.get_api_commit ~ctx ~repo ~sha in
        with_gh_result_populate_slack ~api_result:result ~populate:Slack_message.populate_commit ~repo
      | Compare basehead ->
        let%lwt result = Github_api.get_compare ~ctx ~repo ~basehead in
        with_gh_result_populate_slack ~api_result:result ~populate:Slack_message.populate_compare ~repo
    in
    log#info "slack link shared: channel=%s, user=%s, message_ts=%s, links=[%s]"
      (Slack_channel.Ident.project event.channel)
      (Slack_user_id.project event.user)
      (Slack_timestamp.project event.message_ts)
      (Stre.catmap ~sep:"; " (fun (l : Slack_t.link_shared_link) -> l.url) event.links);
    let%lwt is_self_bot_user =
      match Slack_user_id.project event.user = "U00" with
      | true ->
        (* field has value "U00" when the message containing the link was sent by a bot with a custom username set *)
        Lwt.return_true
      | false ->
        let%lwt bot_user_id =
          match State.get_bot_user_id ctx.state with
          | Some id -> Lwt.return_some id
          | None -> fetch_bot_user_id ()
        in
        Lwt.return (Option.map_default (Slack_user_id.equal event.user) false bot_user_id)
    in
    if List.length event.links > 4 then Lwt.return "ignored: more than two links present"
    else if is_self_bot_user then Lwt.return "ignored: is bot user"
    else begin
      let links = List.map (fun (l : Slack_t.link_shared_link) -> l.url) event.links in
      let%lwt unfurls =
        List.map process links |> Lwt.all |> Lwt.map (List.filter_map id) |> Lwt.map StringMap.of_list
      in
      if StringMap.is_empty unfurls then Lwt.return "ignored: no links to unfurl"
      else begin
        match%lwt Slack_api.send_chat_unfurl ~ctx ~channel:event.channel ~ts:event.message_ts ~unfurls () with
        | Ok () -> Lwt.return "ok"
        | Error e ->
          log#error "%s" e;
          Lwt.return "ignored: failed to unfurl links"
      end
    end

  let process_slack_event (ctx : Context.t) headers body =
    let secrets = Context.get_secrets_exn ctx in
    match Slack_j.event_notification_of_string body with
    | Url_verification payload -> Lwt.return payload.challenge
    | Event_callback notification ->
    match Slack.validate_signature ?signing_key:secrets.slack_signing_secret ~headers body with
    | Error e -> action_error e
    | Ok () ->
    match notification.event with
    | Link_shared event -> process_link_shared_event ctx event

  let process_buildkite_webhook (ctx : Context.t) headers body =
    let open Util.Webhook in
    try_process_notification body
      (let n = Buildkite_j.webhook_build_payload_of_string body in
       log#info "[buildkite_webhook] [%s] event %s: state=%s, branch=%s, commit=%s, pipeline=%s, build_url=%s"
         n.pipeline.provider.settings.repository
         (Buildkite_j.string_of_webhook_event n.event)
         (Buildkite_j.string_of_build_state n.build.state)
         n.build.branch (String.sub n.build.sha 0 8) (pipeline_name n) n.build.web_url;
       let secrets = Context.get_secrets_exn ctx in
       match validate_signature ?signing_key:secrets.buildkite_signing_secret ~headers body with
       | Error e -> action_error e
       | Ok () ->
         let%lwt () = Database.Failed_builds.create ~ctx n in
         let repo_url = validate_repo_url secrets n in
         let%lwt cfg =
           match Context.find_repo_config ctx repo_url with
           | Some cfg -> Lwt.return cfg
           | None ->
             (* Fetch the config from github.
                Emulate a repository record with the necessary fields to fetch the config *)
             let (fake_repo : Github_t.repository) =
               {
                 name = "";
                 full_name = "";
                 url = repo_url;
                 commits_url = "";
                 contents_url = Util.Build.git_ssh_to_contents_url n.pipeline.repository;
                 pulls_url = "";
                 issues_url = "";
                 compare_url = "";
               }
             in
             (match%lwt fetch_config ~ctx ~repo:fake_repo with
             | Ok () -> Lwt.return @@ Context.find_repo_config_exn ctx repo_url
             | Error e -> action_error @@ Printf.sprintf "failed to fetch config for %s: %s" repo_url e)
         in
         let repo_state = State.find_or_add_repo ctx.state repo_url in
         let org, pipeline, _build_nr = Util.Build.get_org_pipeline_build' n.build.web_url in
         let repo_key = repo_key org pipeline in
         let is_main_branch = cfg.main_branch_name |> Option.map_default (String.equal n.build.branch) false in
         let%lwt is_timely =
           (* we don't want to consider builds notifications for builds that finish out of order *)
           match Stringtbl.find_opt repo_state.failed_steps repo_key with
           | Some state when n.build.number < state.last_build ->
             let%lwt () =
               Database.Failed_builds.update_state_after_notification ~repo_state ~has_state_update:false n
                 (Printf.sprintf "is_timely > false > build number < last build number")
             in
             Lwt.return false
           | _ -> Lwt.return true
         in
         let should_notify =
           is_main_branch && is_timely && (notify_fail cfg n || notify_success repo_state repo_key n)
         in
         (match should_notify with
         | false ->
           let%lwt () =
             Database.Failed_builds.update_state_after_notification ~repo_state ~has_state_update:false n
               "should notify > false"
           in
           Lwt.return_unit
         | true ->
           let channel = Common.Status_notification.inject_channel (failed_builds_channel_exn cfg n) in
           let%lwt notifications =
             match n.build.state with
             | Passed ->
               (* get the current failed steps *)
               (match Stringtbl.find_opt repo_state.failed_steps repo_key with
               | None ->
                 (* nothing to do, we shouldn't be here. We don't notify success if we don't have failed steps/state. *)
                 log#error "trying to notify success for %s but no failed steps found" n.build.web_url;
                 Lwt.return []
               | Some (state : State_t.failed_steps) ->
                 (* get the steps in the current build to compare against the failed steps. Sometimes builds on
                    the same pipeline don't run the exact same steps. We need to confirm if all steps are fixed. *)
                 (match%lwt Buildkite_api.get_build ~cache:`Refresh ~ctx n.build.web_url with
                 | Error e ->
                   log#error "failed to fetch build steps for %s. Error: %s" n.build.web_url e;
                   (* If we get an error while fetching the build steps of a success notification, let's just assume
                      that all the failed steps have been fixed and move on. It shouldn't happen often. *)
                   Stringtbl.replace repo_state.failed_steps repo_key
                     { steps = Common.FailedStepSet.empty; last_build = n.build.number };
                   Lwt.return []
                 | Ok (build : Buildkite_t.get_build_res) ->
                   let build_steps = Util.Webhook.to_failed_step_set (Util.Build.filter_passed_jobs build.jobs) n in
                   let state_failed_steps = FailedStepSet.diff state.steps build_steps in
                   (match FailedStepSet.is_empty state_failed_steps with
                   | true ->
                     Stringtbl.replace repo_state.failed_steps repo_key
                       { steps = FailedStepSet.empty; last_build = n.build.number };

                     let%lwt () =
                       Database.Failed_builds.update_state_after_notification ~repo_state ~has_state_update:true n
                         "should notify > true > build state = passed"
                     in
                     Lwt.return
                       [
                         Slack.generate_failed_build_notification ~cfg ~is_fix_build_notification:true
                           ~failed_steps:FailedStepSet.empty n channel;
                       ]
                   | false ->
                     (* the build was successful, but we haven't fixed all the steps. Update state, but don't notify *)
                     let%lwt () =
                       Database.Failed_builds.update_state_after_notification ~repo_state
                         ~has_state_update:(FailedStepSet.equal state_failed_steps state.steps)
                         n "should notify > false > build state = passed w/ failed steps"
                     in
                     Stringtbl.replace repo_state.failed_steps repo_key
                       { steps = state_failed_steps; last_build = n.build.number };
                     Lwt.return [])))
             | Failed ->
               (match%lwt
                  (* repo state is updated upon fetching new failed steps *)
                  new_failed_steps ~cfg ~repo_state
                    ~get_build:(Buildkite_api.get_build ~cache:`Refresh ~ctx)
                    ~db_update:(fun ~repo_state ~has_state_update n msg ->
                      let%lwt () =
                        Database.Failed_builds.update_state_after_notification ~repo_state ~has_state_update n
                          (Printf.sprintf "should notify > true > build state = failed > %s" msg)
                      in
                      Lwt.return_unit)
                    n
                with
               | Error e -> action_error e
               | Ok failed_steps ->
               match FailedStepSet.is_empty failed_steps with
               | true -> Lwt.return []
               | false ->
                 let%lwt slack_ids =
                   match mention_user_on_failed_builds cfg n with
                   | false -> Lwt.return []
                   | true ->
                     let to_slack_id email =
                       match%lwt Slack_api.lookup_user ~ctx ~cfg ~email () with
                       | Ok (res : Slack_t.lookup_user_res) -> Lwt.return_some res.user.id
                       | Error e ->
                         log#warn "couldn't match commit email %s to slack profile: %s" email e;
                         Lwt.return_none
                     in
                     let author_emails =
                       FailedStepSet.fold
                         (fun (step : Buildkite_t.failed_step) acc ->
                           if List.mem step.author acc then acc else step.author :: acc)
                         failed_steps []
                     in
                     Lwt_list.map_s to_slack_id author_emails
                 in
                 let slack_ids = List.filter_map Fun.id slack_ids in
                 Lwt.return [ Slack.generate_failed_build_notification ~slack_ids ~cfg ~failed_steps n channel ])
             | _ -> assert false
           in
           let%lwt () = send_notifications ctx notifications in
           (match ctx.state_filepath with
           | None -> Lwt.return_unit
           | Some path ->
           match State.save ctx.state path with
           | Ok () -> Lwt.return_unit
           | Error e -> action_error e)))

  (** debugging endpoint to return current in-memory repo config *)
  let print_config (ctx : Context.t) repo_url =
    log#info "finding config for repo_url: %s" repo_url;
    match Context.find_repo_config ctx repo_url with
    | None -> Lwt.return_error (`Not_found, Printf.sprintf "repo_url not found: %s" repo_url)
    | Some config -> Lwt.return_ok (Config_j.string_of_config config)
end
