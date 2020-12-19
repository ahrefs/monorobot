open Devkit
open Base
open Slack
open Config_t
open Config
open Common
open Github_j

exception Action_error of string

let action_error msg = raise (Action_error msg)

let log = Log.from "action"

module Action (Github_api : Api.Github) (Slack_api : Api.Slack) = struct
  let partition_push cfg n =
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

  let partition_label (cfg : Config.t) (labels : label list) =
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

  let partition_commit (cfg : Config.t) files =
    let default = Option.to_list cfg.prefix_rules.default_channel in
    let rules = cfg.prefix_rules.rules in
    let matched_channel_names =
      List.map ~f:(fun f -> f.filename) files
      |> List.filter_map ~f:(Rule.Prefix.match_rules ~rules)
      |> List.dedup_and_sort ~compare:String.compare
    in
    if List.is_empty matched_channel_names then default else matched_channel_names

  let partition_status (ctx : Context.t) (n : status_notification) =
    match ctx.config with
    | None -> action_error "missing configuration file"
    | Some cfg ->
      let pipeline = n.context in
      let current_status = n.state in
      let updated_at = n.updated_at in
      let get_commit_info () =
        let default = Option.to_list cfg.prefix_rules.default_channel in
        let () =
          Context.refresh_pipeline_status ~pipeline ~branches:n.branches ~status:current_status ~updated_at ctx
        in
        match List.is_empty n.branches with
        | true -> Lwt.return []
        | false ->
        match cfg.main_branch_name with
        | None -> Lwt.return default
        | Some main_branch_name ->
        (* non-main branch build notifications go to default channel to reduce spam in topic channels *)
        match List.exists n.branches ~f:(fun { name } -> String.equal name main_branch_name) with
        | false -> Lwt.return default
        | true ->
          let sha = n.commit.sha in
          let repo = n.repository in
          ( match%lwt Github_api.get_api_commit ~ctx ~repo ~sha with
          | Error e -> action_error e
          | Ok commit -> Lwt.return @@ partition_commit cfg commit.files
          )
      in
      let res =
        match
          List.exists cfg.status_rules.status ~f:(fun x ->
            match x with
            | State s -> Poly.equal s n.state
            | HideConsecutiveSuccess -> Poly.equal Success n.state
            | _ -> false)
        with
        | false -> Lwt.return []
        | true ->
        match List.exists ~f:id [ Rule.Status.hide_cancelled n cfg; Rule.Status.hide_success n ctx ] with
        | true -> Lwt.return []
        | false ->
        match cfg.status_rules.title with
        | None -> get_commit_info ()
        | Some status_filter ->
        match List.exists status_filter ~f:(String.equal n.context) with
        | false -> Lwt.return []
        | true -> get_commit_info ()
      in
      res

  let partition_commit_comment (ctx : Context.t) n =
    match ctx.config with
    | None -> action_error "missing configuration file"
    | Some cfg ->
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
    match ctx.config with
    | None -> action_error "missing configuration file"
    | Some cfg ->
    match req with
    | Github.Push n ->
      partition_push cfg n |> List.map ~f:(fun (webhook, n) -> webhook, generate_push_notification n) |> Lwt.return
    | Pull_request n ->
      partition_pr cfg n |> List.map ~f:(fun webhook -> webhook, generate_pull_request_notification n) |> Lwt.return
    | PR_review n ->
      partition_pr_review cfg n |> List.map ~f:(fun webhook -> webhook, generate_pr_review_notification n) |> Lwt.return
    | PR_review_comment n ->
      partition_pr_review_comment cfg n
      |> List.map ~f:(fun webhook -> webhook, generate_pr_review_comment_notification n)
      |> Lwt.return
    | Issue n ->
      partition_issue cfg n |> List.map ~f:(fun webhook -> webhook, generate_issue_notification n) |> Lwt.return
    | Issue_comment n ->
      partition_issue_comment cfg n
      |> List.map ~f:(fun webhook -> webhook, generate_issue_comment_notification n)
      |> Lwt.return
    | Commit_comment n ->
      let%lwt webhooks, api_commit = partition_commit_comment ctx n in
      let%lwt notif = generate_commit_comment_notification api_commit n in
      let notifs = List.map ~f:(fun webhook -> webhook, notif) webhooks in
      Lwt.return notifs
    | Status n ->
      let%lwt webhooks = partition_status ctx n in
      let notifs = List.map ~f:(fun webhook -> webhook, generate_status_notification cfg n) webhooks in
      Lwt.return notifs
    | _ -> Lwt.return []

  let send_notifications (ctx : Context.t) notifications =
    let notify (chan, msg) =
      match Context.hook_of_channel ctx chan with
      | None ->
        log#error "webhook not defined for Slack channel '%s'" chan;
        Lwt.return_unit
      | Some url ->
        ( match%lwt Slack_api.send_notification ~chan ~msg ~url with
        | Ok () -> Lwt.return_unit
        | Error e -> action_error e
        )
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
        (* can remove this wrapper once status_rules doesn't depend on Config.t *)
        ctx.config <- Some (Config.make config);
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
      match Github.parse_exn ~secret:ctx.gh_hook_token headers body with
      | exception exn -> Exn_lwt.fail ~exn "failed to parse payload"
      | payload ->
        ( match%lwt refresh_config_of_context ctx payload with
        | Error e -> action_error e
        | Ok () ->
          let%lwt notifications = generate_notifications ctx payload in
          let%lwt () = send_notifications ctx notifications in
          ( match ctx.state_filepath with
          | None -> Lwt.return_unit
          | Some path -> Lwt.return @@ State.save path ctx.state
          )
        )
    with
    | Yojson.Json_error msg ->
      log#error "failed to parse file as valid JSON (%s)" msg;
      Lwt.return_unit
    | Action_error msg ->
      log#error "%s" msg;
      Lwt.return_unit
end
