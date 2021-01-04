open Devkit
open Base
open Slack
open Config_t
open Common
open Github_j

exception Action_error of string

let action_error msg = raise (Action_error msg)

let log = Log.from "action"

module Action (Github_api : Api.Github) (Slack_api : Api.Slack) = struct
  let partition_push (cfg : Config_t.config) n =
    let default = Option.to_list cfg.prefix_rules.default_channel in
    let rules = cfg.prefix_rules.rules in
    n.commits
    |> List.filter ~f:(fun c -> c.distinct)
    |> List.filter ~f:(fun c ->
         let branch = Github.commits_branch_of_ref n.ref in
         let skip = Github.is_main_merge_message ~msg:c.message ~branch cfg in
         if skip then log#info "main branch merge, ignoring %s: %s" c.id (first_line c.message);
         not skip)
    |> List.concat_map ~f:(fun commit ->
         let matched_channel_names =
           Github.modified_files_of_commit commit
           |> List.filter_map ~f:(Rule.Prefix.match_rules ~rules)
           |> List.dedup_and_sort ~compare:String.compare
         in
         let channel_names = if List.is_empty matched_channel_names then default else matched_channel_names in
         List.map channel_names ~f:(fun n -> n, commit))
    |> Map.of_alist_multi (module String)
    |> Map.map ~f:(fun commits -> { n with commits })
    |> Map.to_alist

  let partition_label (cfg : Config_t.config) (labels : label list) =
    let default = cfg.label_rules.default_channel in
    let rules = cfg.label_rules.rules in
    labels |> List.concat_map ~f:(Rule.Label.match_rules ~rules) |> List.dedup_and_sort ~compare:String.compare
    |> fun channel_names -> if List.is_empty channel_names then Option.to_list default else channel_names

  let partition_pr cfg (n : pr_notification) =
    match n.action with
    | Opened | Closed | Reopened | Labeled -> partition_label cfg n.pull_request.labels
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
    | Submitted, "commented", (Some "" | None) -> []
    (* the case (action = Submitted, review.state = "commented", review.body = "") happens when
       a reviewer starts a review by commenting on particular sections of the code, which triggars a pull_request_review_comment event simultaneouly,
       and then submits the review without submitting any general feedback or explicit approval/changes.

       the case (action = Submitted, review.state = "commented", review.body = null) happens when
       a reviewer adds a single comment on a particular section of the code, which triggars a pull_request_review_comment event simultaneouly.

       in both cases, since pull_request_review_comment is already handled by another type of event, information in the pull_request_review payload
       does not provide any insightful information and will thus be ignored. *)
    | Submitted, _, _ -> partition_label cfg n.pull_request.labels
    | _ -> []

  let partition_commit (cfg : Config_t.config) files =
    let default = Option.to_list cfg.prefix_rules.default_channel in
    let rules = cfg.prefix_rules.rules in
    let matched_channel_names =
      List.map ~f:(fun f -> f.filename) files
      |> List.filter_map ~f:(Rule.Prefix.match_rules ~rules)
      |> List.dedup_and_sort ~compare:String.compare
    in
    if List.is_empty matched_channel_names then default else matched_channel_names

  let partition_status (ctx : Context.t) (n : status_notification) =
    let cfg = Context.get_config_exn ctx in
    let pipeline = n.context in
    let current_status = n.state in
    let rules = cfg.status_rules.rules in
    let action_on_match (branches : branch list) =
      let default = Option.to_list cfg.prefix_rules.default_channel in
      let () = Context.refresh_pipeline_status ~pipeline ~branches ~status:current_status ctx in
      match List.is_empty branches with
      | true -> Lwt.return []
      | false ->
      match cfg.main_branch_name with
      | None -> Lwt.return default
      | Some main_branch_name ->
      (* non-main branch build notifications go to default channel to reduce spam in topic channels *)
      match List.exists branches ~f:(fun { name } -> String.equal name main_branch_name) with
      | false -> Lwt.return default
      | true ->
        let sha = n.commit.sha in
        let repo = n.repository in
        ( match%lwt Github_api.get_api_commit ~ctx ~repo ~sha with
        | Error e -> action_error e
        | Ok commit -> Lwt.return @@ partition_commit cfg commit.files
        )
    in
    if Context.is_pipeline_allowed ctx ~pipeline then begin
      match Rule.Status.match_rules ~rules n with
      | Some Ignore | None -> Lwt.return []
      | Some Allow -> action_on_match n.branches
      | Some Allow_once ->
      match Map.find ctx.state.pipeline_statuses pipeline with
      | Some branch_statuses ->
        let has_same_status_state_as_prev (branch : branch) =
          match Map.find branch_statuses branch.name with
          | None -> false
          | Some state -> Poly.equal state current_status
        in
        let branches = List.filter n.branches ~f:(Fn.non @@ has_same_status_state_as_prev) in
        action_on_match branches
      | None -> action_on_match n.branches
    end
    else Lwt.return []

  let partition_commit_comment (ctx : Context.t) n =
    let cfg = Context.get_config_exn ctx in
    match n.comment.commit_id with
    | None -> action_error "unable to find commit id for this commit comment event"
    | Some sha ->
      ( match%lwt Github_api.get_api_commit ~ctx ~repo:n.repository ~sha with
      | Error e -> action_error e
      | Ok commit ->
        let default = Option.to_list cfg.prefix_rules.default_channel in
        let rules = cfg.prefix_rules.rules in
        ( match n.comment.path with
        | None -> Lwt.return @@ (partition_commit cfg commit.files, commit)
        | Some filename ->
        match Rule.Prefix.match_rules filename ~rules with
        | None -> Lwt.return (default, commit)
        | Some chan -> Lwt.return ([ chan ], commit)
        )
      )

  let generate_notifications (ctx : Context.t) req =
    let cfg = Context.get_config_exn ctx in
    match req with
    | Github.Push n ->
      partition_push cfg n |> List.map ~f:(fun (channel, n) -> generate_push_notification n channel) |> Lwt.return
    | Pull_request n -> partition_pr cfg n |> List.map ~f:(generate_pull_request_notification n) |> Lwt.return
    | PR_review n -> partition_pr_review cfg n |> List.map ~f:(generate_pr_review_notification n) |> Lwt.return
    | PR_review_comment n ->
      partition_pr_review_comment cfg n |> List.map ~f:(generate_pr_review_comment_notification n) |> Lwt.return
    | Issue n -> partition_issue cfg n |> List.map ~f:(generate_issue_notification n) |> Lwt.return
    | Issue_comment n ->
      partition_issue_comment cfg n |> List.map ~f:(generate_issue_comment_notification n) |> Lwt.return
    | Commit_comment n ->
      let%lwt channels, api_commit = partition_commit_comment ctx n in
      let notifs = List.map ~f:(generate_commit_comment_notification api_commit n) channels in
      Lwt.return notifs
    | Status n ->
      let%lwt channels = partition_status ctx n in
      let notifs = List.map ~f:(generate_status_notification cfg n) channels in
      Lwt.return notifs
    | _ -> Lwt.return []

  let send_notifications (ctx : Context.t) notifications =
    let notify (msg : Slack_t.post_message_req) =
      match%lwt Slack_api.send_notification ~ctx ~msg with
      | Ok () -> Lwt.return_unit
      | Error e -> action_error e
    in
    Lwt_list.iter_s notify notifications

  (** `refresh_config_of_context ctx n` updates the current context if the configuration
      hasn't been loaded yet, or if the incoming request `n` is a push
      notification containing commits that touched the config file. *)
  let refresh_config_of_context (ctx : Context.t) notification =
    let fetch_config () =
      let repo = Github.repo_of_notification notification in
      match%lwt Github_api.get_config ~ctx ~repo with
      | Ok config ->
        ctx.config <- Some config;
        Context.print_config ctx;
        Lwt.return @@ Ok ()
      | Error e -> action_error e
    in
    match ctx.config with
    | None -> fetch_config ()
    | Some _ ->
    match notification with
    | Github.Push commit_pushed_notification ->
      let commits = commit_pushed_notification.commits in
      let modified_files = List.concat_map commits ~f:Github.modified_files_of_commit in
      let config_was_modified = List.exists modified_files ~f:(String.equal ctx.config_filename) in
      if config_was_modified then fetch_config () else Lwt.return @@ Ok ()
    | _ -> Lwt.return @@ Ok ()

  let process_github_notification (ctx : Context.t) headers body =
    try%lwt
      let secrets = Context.get_secrets_exn ctx in
      match Github.parse_exn ~secret:secrets.gh_hook_token headers body with
      | exception exn -> Exn_lwt.fail ~exn "failed to parse payload"
      | payload ->
        ( match%lwt refresh_config_of_context ctx payload with
        | Error e -> action_error e
        | Ok () ->
          let%lwt notifications = generate_notifications ctx payload in
          let%lwt () = send_notifications ctx notifications in
          ( match ctx.state_filepath with
          | None -> Lwt.return_unit
          | Some path ->
            ( match%lwt State.save ctx.state path with
            | Ok () -> Lwt.return_unit
            | Error e -> action_error e
            )
          )
        )
    with
    | Yojson.Json_error msg ->
      log#error "failed to parse file as valid JSON (%s)" msg;
      Lwt.return_unit
    | Action_error msg ->
      log#error "%s" msg;
      Lwt.return_unit
    | Context.Context_error msg ->
      log#error "%s" msg;
      Lwt.return_unit
end
