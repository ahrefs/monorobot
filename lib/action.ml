open Devkit
open Printf
open Base
open Slack
open Notabot_t
open Config
open Common
open Github_j

let log = Log.from "action"

type prefix_match =
  | Match of int
  | NoMatch

let chan_of_prefix_rule (r : prefix_rule) = r.channel_name

let touching_prefix (rule : Notabot_t.prefix_rule) name =
  let match_lengths filename prefixes =
    List.filter_map
      ~f:(fun prefix -> if String.is_prefix filename ~prefix then Some (String.length prefix) else None)
      prefixes
  in
  match match_lengths name rule.ignore with
  | _ :: _ -> NoMatch
  | [] ->
  match rule.allow with
  | [] -> Match 0
  | _ ->
  match List.max_elt (match_lengths name rule.allow) ~compare:Int.compare with
  | Some x -> Match x
  | None -> NoMatch

let longest_touching_prefix_rule rules name =
  let get_m rule = touching_prefix rule name in
  let reduce_to_longest_match longest_rule_match_pair current_rule =
    let _, longest_match = longest_rule_match_pair in
    let current_match = get_m current_rule in
    let current_rule_match_pair = current_rule, current_match in
    match longest_match with
    | NoMatch -> current_rule_match_pair
    | Match x ->
    match current_match with
    | NoMatch -> longest_rule_match_pair
    | Match y -> if y > x then current_rule_match_pair else longest_rule_match_pair
  in
  match rules with
  | [] -> None
  | (x : prefix_rule) :: xs ->
  match List.fold_left xs ~init:(x, get_m x) ~f:reduce_to_longest_match with
  | _, NoMatch -> None
  | r, Match _ -> Some r

let chan_of_file rules file = Option.map ~f:chan_of_prefix_rule @@ longest_touching_prefix_rule rules file

let unique_chans_of_files rules files =
  List.dedup_and_sort ~compare:String.compare @@ List.filter_map files ~f:(chan_of_file rules)

let touching_label rule name =
  let name_lc = String.lowercase name in
  let label_lc = List.map rule.allow ~f:(fun l -> String.lowercase l) in
  let ignore_lc = List.map rule.ignore ~f:(fun l -> String.lowercase l) in
  (* convert both labels and config into lowe-case to make label matching case-insensitive *)
  (List.is_empty label_lc || List.mem ~equal:String.equal label_lc name_lc)
  && not (List.mem ~equal:String.equal ignore_lc name_lc)

let is_main_merge_message ~msg:message ~branch cfg =
  match cfg.main_branch_name with
  | Some main_branch when String.equal branch main_branch ->
    (*
      handle "Merge <main branch> into <feature branch>" commits when they are merged into main branch
      we should have already seen these commits on the feature branch but for some reason they are distinct:true
    *)
    let prefix = sprintf "Merge branch '%s' into " main_branch in
    let prefix2 = sprintf "Merge remote-tracking branch 'origin/%s' into " main_branch in
    let title = first_line message in
    String.is_prefix title ~prefix || String.is_prefix title ~prefix:prefix2
  | Some main_branch ->
    let expect = sprintf "Merge branch '%s' into %s" main_branch branch in
    let expect2 = sprintf "Merge remote-tracking branch 'origin/%s' into %s" main_branch branch in
    let title = first_line message in
    String.equal title expect || String.equal title expect2
  | _ -> false

let filter_push rules commit =
  let files = List.concat [ commit.added; commit.removed; commit.modified ] in
  List.map ~f:(fun chan -> chan, commit) @@ unique_chans_of_files rules files

let group_commit chan l =
  List.filter_map l ~f:(fun (chan', commit) ->
    match String.equal chan chan' with
    | false -> None
    | true -> Some commit)

let partition_push cfg n =
  let default commit =
    Option.value_map cfg.prefix_rules.default_channel ~default:[] ~f:(fun webhook -> [ webhook, commit ])
  in
  let rules = cfg.prefix_rules.rules in
  let channels =
    n.commits
    |> List.filter ~f:(fun c -> c.distinct)
    |> List.filter ~f:(fun c ->
         let branch = Github.get_commits_branch n.ref in
         let skip = is_main_merge_message ~msg:c.message ~branch cfg in
         if skip then log#info "main branch merge, ignoring %s: %s" c.id (first_line c.message);
         not skip)
    |> List.map ~f:(fun commit ->
         match filter_push rules commit with
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

let filter_label rules (label : Github_j.label) =
  rules
  |> List.filter_map ~f:(fun rule ->
       match touching_label rule label.name with
       | false -> None
       | true -> Some rule.channel_name)

let partition_label cfg (labels : Github_j.label list) =
  let default = Option.to_list cfg.label_rules.default_channel in
  match labels with
  | [] -> default
  | labels ->
    let rules = cfg.label_rules.rules in
    let channels =
      labels
      |> List.map ~f:(fun (label : Github_j.label) ->
           match filter_label rules label with
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
  match unique_chans_of_files cfg.prefix_rules.rules names with
  | _ :: _ as xs -> xs
  | [] -> Option.to_list cfg.prefix_rules.default_channel

let hide_cancelled (notification : status_notification) cfg =
  let find_cancelled status_state =
    match status_state with
    | Config.Cancelled r -> Some r
    | _ -> None
  in
  let regexp_opt = List.find_map cfg.status_rules.status ~f:find_cancelled in
  match regexp_opt with
  | None -> false
  | Some regexp ->
    let { state; description; _ } = notification in
    let r = Re.Str.regexp_case_fold regexp in
    ( match description, state with
    | Some s, Failure when Re.Str.string_match r s 0 -> true
    | _ -> false
    )

let hide_success (n : status_notification) (ctx : Context.t) =
  match List.exists ctx.cfg.status_rules.status ~f:(Poly.equal Config.HideConsecutiveSuccess) with
  | false -> false
  | true ->
  match n.state with
  | Success ->
    List.exists
      ~f:(fun b ->
        match State.get_branch_state b.name ctx.state with
        | None | Some { last_build_state = Failure; _ } -> false
        | Some { last_build_state = Success; _ } -> true)
      n.branches
  | _ -> false

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
        List.exists n.branches ~f:(fun { name } -> is_main_merge_message ~msg:commit.commit.message ~branch:name cfg)
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
    match List.exists ~f:id [ hide_cancelled n cfg; hide_success n ctx ] with
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
  match chan_of_file cfg.prefix_rules.rules p with
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

let print_prefix_routing rules =
  let show_match l = String.concat ~sep:" or " @@ List.map ~f:(fun s -> s ^ "*") l in
  rules
  |> List.iter ~f:(fun (rule : prefix_rule) ->
       begin
         match rule.allow, rule.ignore with
         | [], [] -> Stdio.printf "  any"
         | l, [] -> Stdio.printf "  %s" (show_match l)
         | [], l -> Stdio.printf "  not %s" (show_match l)
         | l, i -> Stdio.printf "  %s and not %s" (show_match l) (show_match i)
       end;
       Stdio.printf " -> #%s\n%!" rule.channel_name)

let print_label_routing rules =
  let show_match l = String.concat ~sep:" or " l in
  rules
  |> List.iter ~f:(fun (rule : label_rule) ->
       begin
         match rule.allow, rule.ignore with
         | [], [] -> Stdio.printf "  any"
         | l, [] -> Stdio.printf "  %s" (show_match l)
         | [], l -> Stdio.printf "  not %s" (show_match l)
         | l, i -> Stdio.printf "  %s and not %s" (show_match l) (show_match i)
       end;
       Stdio.printf " -> #%s\n%!" rule.channel_name)
