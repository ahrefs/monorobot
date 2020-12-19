open Devkit
open Base
open Slack
open Config_t
open Rule_t
open Config
open Common
open Github_j

let log = Log.from "action"

module Action = struct
  let partition_push cfg n =
    let group_commit chan l =
      List.filter_map l ~f:(fun (chan', commit) ->
        match String.equal chan chan' with
        | false -> None
        | true -> Some commit)
    in
    let default commit =
      Option.value_map cfg.prefix_rules.default_channel ~default:[] ~f:(fun webhook -> [ webhook, commit ])
    in
    let rules = cfg.prefix_rules.rules in
    let channels =
      n.commits
      |> List.filter ~f:(fun c -> c.distinct)
      |> List.filter ~f:(fun c ->
           let branch = Github.commits_branch_of_ref n.ref in
           let skip = Github.is_main_merge_message ~msg:c.message ~branch cfg in
           if skip then log#info "main branch merge, ignoring %s: %s" c.id (first_line c.message);
           not skip)
      |> List.map ~f:(fun commit ->
           match Rule.Prefix.filter_push rules commit with
           | [] -> default commit
           | l -> l)
      |> List.concat
    in
    let prefix_chans =
      let chans =
        Option.to_list cfg.prefix_rules.default_channel
        @ List.map rules ~f:(fun (rule : prefix_rule) -> rule.channel_name)
      in
      List.dedup_and_sort chans ~compare:String.compare
    in
    List.filter_map prefix_chans ~f:(fun chan ->
      match group_commit chan channels with
      | [] -> None
      | l -> Some (chan, { n with commits = l }))

  let partition_label cfg (labels : Github_j.label list) =
    let default = Option.to_list cfg.label_rules.default_channel in
    match labels with
    | [] -> default
    | labels ->
      let rules = cfg.label_rules.rules in
      let channels =
        labels
        |> List.map ~f:(fun (label : Github_j.label) ->
             match Rule.Label.filter_label rules label with
             | [] -> default
             | l -> l)
      in
      List.dedup_and_sort ~compare:String.compare (List.concat channels)

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

  let partition_commit cfg files =
    let names = List.map ~f:(fun f -> f.filename) files in
    match Rule.Prefix.unique_chans_of_files cfg.prefix_rules.rules names with
    | _ :: _ as xs -> xs
    | [] -> Option.to_list cfg.prefix_rules.default_channel

  let partition_status (ctx : Context.t) (n : status_notification) =
    let cfg = ctx.cfg in
    let get_commit_info () =
      let default () = Lwt.return @@ Option.to_list cfg.prefix_rules.default_channel in
      match cfg.main_branch_name with
      | None -> default ()
      | Some main_branch_name ->
      (* non-main branch build notifications go to default channel to reduce spam in topic channels *)
      match List.exists n.branches ~f:(fun { name } -> String.equal name main_branch_name) with
      | false -> default ()
      | true ->
        ( match%lwt Github.generate_query_commit cfg ~url:n.commit.url ~sha:n.commit.sha with
        | None -> default ()
        | Some commit ->
          (*
      match
        List.exists n.branches ~f:(fun { name } -> Github.is_main_merge_message ~msg:commit.commit.message ~branch:name cfg)
      with
      | true ->
        log#info "main branch merge, ignoring status event %s: %s" n.context (first_line commit.commit.message);
        Lwt.return []
      | false ->
*)
          Lwt.return (partition_commit cfg commit.files)
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
    Context.update_state ctx (Github.Status n);
    res

  let partition_commit_comment cfg n =
    let default = Option.to_list cfg.prefix_rules.default_channel in
    match n.comment.path with
    | None ->
      ( match%lwt Github.generate_commit_from_commit_comment cfg n with
      | None -> Lwt.return default
      | Some commit -> Lwt.return (partition_commit cfg commit.files)
      )
    | Some p ->
    match Rule.Prefix.chan_of_file cfg.prefix_rules.rules p with
    | None -> Lwt.return default
    | Some chan -> Lwt.return [ chan ]

  let generate_notifications (ctx : Context.t) req =
    let cfg = ctx.cfg in
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
      let%lwt webhooks = partition_commit_comment cfg n in
      let%lwt notif = generate_commit_comment_notification cfg n in
      let notifs = List.map ~f:(fun webhook -> webhook, notif) webhooks in
      Lwt.return notifs
    | Status n ->
      let%lwt webhooks = partition_status ctx n in
      let notifs = List.map ~f:(fun webhook -> webhook, generate_status_notification cfg n) webhooks in
      Lwt.return notifs
    | _ -> Lwt.return []

  let send_notifications (cfg : Config.t) notifications =
    Lwt_list.iter_s
      (fun (chan, msg) ->
        let url = Config.Chan_map.find chan cfg.chans in
        let data = Slack_j.string_of_webhook_notification msg in
        log#info "sending to %s : %s" chan data;
        Slack.send_notification url data)
      notifications

  let update_config (ctx : Context.t) = function
    | Github.Push n ->
      let is_config_file f = String.equal f ctx.data.cfg_filename in
      let commit_contains_config_file (c : Github_t.commit) = List.exists ~f:is_config_file (c.added @ c.modified) in
      if List.exists ~f:commit_contains_config_file n.commits then Context.refresh_config ctx else Lwt.return_unit
    | _ -> Lwt.return_unit

  let process_github_notification (ctx_thunk : Context.context_thunk) headers body =
    match Github.parse_exn ~secret:ctx_thunk.secrets.gh_webhook_secret headers body with
    | exception exn -> Exn_lwt.fail ~exn "unable to parse payload"
    | payload ->
    try
      let%lwt ctx = Context.resolve_ctx_in_thunk ctx_thunk payload in
      let%lwt () = update_config ctx payload in
      let cfg = ctx.cfg in
      let%lwt notifications = generate_notifications ctx payload in
      send_notifications cfg notifications
    with
    | Context.Context_error s ->
      log#error "error creating context from payload: %s" s;
      Lwt.return_unit
    | Github.Remote_config_error s ->
      log#error "error retrieving config from payload: %s" s;
      Lwt.return_unit
end
