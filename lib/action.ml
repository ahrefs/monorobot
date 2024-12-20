open Devkit
open Slack
open Config_t
open Common
open Github_j

exception Action_error of string
exception Success_handler_error of string

let action_error msg = raise (Action_error msg)
let handler_error msg = raise (Success_handler_error msg)
let log = Log.from "action"

module Action (Github_api : Api.Github) (Slack_api : Api.Slack) = struct
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
    let repo = n.repository in
    let cfg = Context.find_repo_config_exn ctx repo.url in
    let context = n.context in
    let email = n.commit.commit.author.email in
    let rules = cfg.status_rules.rules in
    let repo_state = State.find_or_add_repo ctx.state repo.url in
    let action_on_match (branches : branch list) ~notify_channels ~notify_dm =
      let is_main_branch =
        match cfg.main_branch_name with
        | None -> false
        | Some main_branch -> List.exists (fun ({ name } : branch) -> String.equal name main_branch) n.branches
      in
      let%lwt direct_message =
        if notify_dm then begin
          match%lwt Slack_api.lookup_user ~ctx ~cfg ~email () with
          | Ok res ->
            let is_failing_build = Util.Build.is_failing_build n in
            let is_failed_build = Util.Build.is_failed_build n in
            (* Check if config holds Github to Slack email mapping for the commit author *)
            let author = List.assoc_opt email cfg.user_mappings |> Option.default email in
            let dm_after_failed_build =
              List.assoc_opt author cfg.dev_notifications.dm_after_failed_build
              |> (* dm_after_failed_build is opt in *)
              Option.default false
            in
            let dm_for_failing_build =
              List.assoc_opt author cfg.dev_notifications.dm_for_failing_build
              |> (* dm_for_failing_build is opt out *)
              Option.default true
            in
            (match (dm_for_failing_build && is_failing_build) || (dm_after_failed_build && is_failed_build) with
            | true ->
              (* if we send a dm for a failing build and we want another dm after the build is finished, we don't
                 set the pipeline commit immediately. Otherwise, we wouldn't be able to notify later *)
              if (is_failing_build && not dm_after_failed_build) || is_failed_build (* TODO: test double opt in *) then
                State.set_repo_pipeline_commit ctx.state n;
              (* To send a DM, channel parameter is set to the user id of the recipient *)
              Lwt.return [ Status_notification.User res.user.id ]
            | false -> Lwt.return [])
          | Error e ->
            log#warn "couldn't match commit email %s to slack profile: %s" n.commit.commit.author.email e;
            Lwt.return []
        end
        else Lwt.return []
      in
      let%lwt chans =
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
      (* only notify the failed builds channels for full failed builds with new failed steps on the main branch *)
      let notify_failed_builds_channel =
        is_main_branch
        && Util.Build.is_failed_build n
        && Util.Build.new_failed_steps n repo_state <> []
        && Option.map_default
             (fun allowed_pipelines ->
               List.exists
                 (fun { failed_builds_channel; name } -> name = context && Option.is_some failed_builds_channel)
                 allowed_pipelines)
             false cfg.status_rules.allowed_pipelines
      in
      match notify_failed_builds_channel, cfg.status_rules.allowed_pipelines with
      | false, _ | _, None -> Lwt.return (direct_message @ chans)
      | true, Some allowed_pipelines ->
        (* if we have a failed builds channel configured, we send one notification there too,
           but we don't notify the same channel twice *)
        let chans =
          List.find_map
            (fun ({ name; failed_builds_channel } : Config_t.pipeline) ->
              match String.equal name context, failed_builds_channel with
              | true, Some failed_builds_channel ->
                Some (Status_notification.inject_channel failed_builds_channel :: chans)
              | _ -> None)
            allowed_pipelines
          |> Option.default chans
          |> List.sort_uniq Status_notification.compare
        in
        Lwt.return (direct_message @ chans)
    in
    let%lwt recipients =
      if Context.is_pipeline_allowed ctx repo.url n then begin
        match Rule.Status.match_rules ~rules n with
        | Some (Ignore, _, _) | None -> Lwt.return []
        | Some (Allow, notify_channels, notify_dm) -> action_on_match n.branches ~notify_channels ~notify_dm
        | Some (Allow_once, notify_channels, notify_dm) ->
          let branches =
            match n.target_url with
            | None -> n.branches
            | Some build_url ->
              let pipeline_name =
                (* We only want to track messages for the base pipeline, not the steps *)
                match Util.Build.parse_context ~context with
                | Some { pipeline_name; _ } -> pipeline_name
                | None -> context
              in
              (match StringMap.find_opt pipeline_name repo_state.pipeline_statuses with
              | None ->
                (* this is the first notification for a pipeline, so no need to filter branches *)
                n.branches
              | Some branch_statuses ->
                let has_same_status (branch : branch) =
                  match StringMap.find_opt branch.name branch_statuses with
                  | Some build_statuses ->
                    let current = Util.Build.get_build_number_exn ~build_url in
                    let previous_builds = IntMap.filter (fun build_num _ -> build_num < current) build_statuses in
                    (match IntMap.is_empty previous_builds with
                    | true ->
                      (* if we have no previous builds, it means they were successful and cleaned from state *)
                      n.state = Github_t.Success
                    | false ->
                      let _, previous_build = IntMap.max_binding previous_builds in
                      previous_build.status = n.state)
                  | None ->
                    (* if we don't have any builds for this branch yet, it's the first notification for this pipeline *)
                    false
                in
                List.filter (Fun.negate has_same_status) n.branches)
          in
          let notify_dm = notify_dm && not (State.mem_repo_pipeline_commits ctx.state n) in
          action_on_match branches ~notify_channels ~notify_dm
      end
      else Lwt.return []
    in
    State.set_repo_pipeline_status ctx.state n;
    Lwt.return recipients

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
      let%lwt channels = partition_status ctx n in
      let%lwt slack_user_id =
        match Util.Build.is_failed_build n with
        | false -> Lwt.return_none
        | true ->
          let email = n.commit.commit.author.email in
          (match%lwt Slack_api.lookup_user ~ctx ~cfg ~email () with
          | Ok (res : Slack_t.lookup_user_res) -> Lwt.return_some res.user.id
          | Error e ->
            log#warn "couldn't match commit email %s to slack profile: %s" email e;
            Lwt.return_none)
      in
      let notifs = List.map (generate_status_notification ?slack_user_id ~ctx cfg n) channels in
      Lwt.return notifs

  let send_notifications (ctx : Context.t) notifications =
    let notify (msg, handler) =
      match%lwt Slack_api.send_notification ~ctx ~msg with
      | Ok (Some res) ->
        (match handler with
        | None -> Lwt.return_unit
        | Some handler ->
        try
          handler res;
          Lwt.return_unit
        with exn -> handler_error (Printexc.to_string exn))
      | Ok None -> Lwt.return_unit
      | Error e -> action_error e
    in
    Lwt_list.iter_s notify notifications

  let fetch_config ~ctx ~repo =
    match%lwt Github_api.get_config ~ctx ~repo with
    | Ok config ->
      Context.set_repo_config ctx repo.url config;
      Context.print_config ctx repo.url;
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
    try%lwt
      let secrets = Context.get_secrets_exn ctx in
      match Github.parse_exn headers body with
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
            | Error e -> action_error e)))
    with
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

  (** debugging endpoint to return current in-memory repo config *)
  let print_config (ctx : Context.t) repo_url =
    log#info "finding config for repo_url: %s" repo_url;
    match Context.find_repo_config ctx repo_url with
    | None -> Lwt.return_error (`Not_found, Printf.sprintf "repo_url not found: %s" repo_url)
    | Some config -> Lwt.return_ok (Config_j.string_of_config config)
end
