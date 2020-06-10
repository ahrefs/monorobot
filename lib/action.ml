open Devkit
open Printf
open Base
open Slack
open Notabot_t
open Config
open Github_j

let log = Log.from "action"

let touching_prefix rule name =
  let has_prefix s = List.exists ~f:(fun prefix -> String.is_prefix s ~prefix) in
  (List.is_empty rule.prefix || has_prefix name rule.prefix) && not (has_prefix name rule.ignore)

let touching_label rule name =
  let name_lc = String.lowercase name in
  let label_lc = List.map rule.label_name ~f:(fun l -> String.lowercase l) in
  let ignore_lc = List.map rule.ignore ~f:(fun l -> String.lowercase l) in
  (* convert both labels and config into lowe-case to make label matching case-insensitive *)
  (List.is_empty label_lc || List.mem ~equal:String.equal label_lc name_lc)
  && not (List.mem ~equal:String.equal ignore_lc name_lc)

let first_line s =
  match String.split ~on:'\n' s with
  | x :: _ -> x
  | [] -> s

let is_main_merge_message message n cfg =
  match cfg.main_branch_name with
  | Some main_branch ->
    let branch = Github.get_commits_branch n in
    let expect = sprintf "Merge branch '%s' into %s" main_branch branch in
    let expect2 = sprintf "Merge remote-tracking branch 'origin/%s' into %s" main_branch branch in
    let title = first_line message in
    String.equal title expect || String.equal title expect2
  | _ -> false

let filter_push rules commit =
  let matching_push rule files = List.exists files ~f:(fun file -> touching_prefix rule file) in
  List.filter_map rules ~f:(fun rule ->
    let filter =
      matching_push rule commit.added || matching_push rule commit.removed || matching_push rule commit.modified
    in
    match filter with
    | false -> None
    | true -> Some (rule.chan, commit))

let group_commit webhook l =
  List.filter_map l ~f:(fun e ->
    match e with
    | chan, commit ->
      let filter = String.equal webhook chan in
      ( match filter with
      | false -> None
      | true -> Some commit
      ))

let partition_push cfg n =
  let default commit = Option.value_map cfg.prefix_rules.default ~default:[] ~f:(fun webhook -> [ webhook, commit ]) in
  let rules = cfg.prefix_rules.rules in
  let channels =
    n.commits
    |> List.filter ~f:(fun c -> c.distinct)
    |> List.filter ~f:(fun c ->
         let skip = is_main_merge_message c.message n cfg in
         if skip then log#info "main branch merge, ignoring %s: %s" c.id (first_line c.message);
         not skip)
    |> List.map ~f:(fun commit ->
         match filter_push rules commit with
         | [] -> default commit
         | l -> l)
  in
  let concat_chan = List.concat channels in
  let default_chan =
    Option.value_map cfg.prefix_rules.default ~default:[] ~f:(fun webhook ->
      match group_commit webhook concat_chan with
      | [] -> []
      | l -> [ webhook, { n with commits = l } ])
  in
  let prefix_chan =
    List.filter_map rules ~f:(fun rule ->
      match group_commit rule.chan concat_chan with
      | [] -> None
      | l -> Some (rule.chan, { n with commits = l }))
  in
  (* ideally, i think we should check and remove duplicates here too,
     but i'm not sure how to compare tuples of type (String, push_notification) *)
  List.append default_chan prefix_chan

let filter_label rules label =
  rules
  |> List.filter_map ~f:(fun rule ->
       match touching_label rule label.name with
       | false -> None
       | true -> Some rule.chan)

let partition_label cfg labels =
  let default = Option.value_map cfg.label_rules.default ~default:[] ~f:(fun webhook -> [ webhook ]) in
  match labels with
  | [] -> default
  | labels ->
    let rules = cfg.label_rules.rules in
    let channels =
      labels
      |> List.map ~f:(fun label ->
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

let filter_commit rules filename =
  rules
  |> List.filter_map ~f:(fun rule ->
       match touching_prefix rule filename with
       | false -> None
       | true -> Some rule.chan)

let partition_commit cfg files =
  let default = Option.value_map cfg.prefix_rules.default ~default:[] ~f:(fun webhook -> [ webhook ]) in
  match files with
  | [] ->
    log#error "this commit contains no files";
    []
  | files ->
    let rules = cfg.prefix_rules.rules in
    let channels =
      files
      |> List.map ~f:(fun file ->
           match filter_commit rules file.filename with
           | [] -> default
           | l -> l)
    in
    List.dedup_and_sort ~compare:String.compare (List.concat channels)

let partition_status cfg (n : status_notification) =
  match n.state with
  | Pending -> Lwt.return []
  | _ ->
    ( match%lwt Github.generate_query_commmit cfg n.commit.url n.commit.sha with
    | None ->
      Lwt.return []
      (* return [] here because this case happens when the path is unavailable and
         the commit cannot be queried via api. an error messege will be generated,
         and thus the bot should not notify any channels *)
    | Some commit -> Lwt.return (partition_commit cfg commit.files)
    )

let partition_commit_comment cfg (n : commit_comment_notification) =
  let url = n.repository.commits_url in
  let url_length = String.length url - 6 in
  (* remove {\sha} from the string *)
  let sha =
    match n.comment.commit_id with
    | None ->
      log#error "unable to find commit id for this commit comment event";
      ""
    | Some id -> id
  in
  let commit_url = String.sub ~pos:0 ~len:url_length url ^ "/" ^ sha in
  (* add sha hash to get the full api link *)
  let%lwt commit = Github.generate_query_commmit cfg commit_url sha in
  match n.comment.path with
  | None ->
    ( match commit with
    | None ->
      Lwt.return []
      (* return [] here because this case happens when the path is unavailable and
         the commit cannot be queried via api. an error messege will be generated,
         and notabot should not notify any channels *)
    | Some commit -> Lwt.return (partition_commit cfg commit.files)
    )
  | Some p ->
  match filter_commit cfg.prefix_rules.rules p with
  | [] ->
    let notifs = Option.value_map cfg.prefix_rules.default ~default:[] ~f:(fun webhook -> [ webhook ]) in
    Lwt.return notifs
  | l -> Lwt.return l

(*
let filter_commit rule files = files |> List.exists ~f:(fun file -> touching_prefix rule file.filename)

let partition_commit_comment cfg (n : commit_comment_notification) =
  let url = n.repository.commits_url in
  let url_length = String.length url - 6 in
  (* remove {\sha} from the string *)
  let sha =
    match n.comment.commit_id with
    | None ->
      log#error "unable to find commit id for this commit comment event";
      ""
    | Some id -> id
  in
  let commit_url = String.sub ~pos:0 ~len:url_length url ^ "/" ^ sha in
  (* add sha hash to get the full api link *)
  let path = n.comment.path in
  let%lwt commit = Github.generate_query_commmit cfg commit_url sha in
  let chan_sub =
    match path with
    | None ->
      ( match commit with
      | None -> []
      | Some commit ->
        cfg.prefix_rules.rules
        |> List.filter_map ~f:(fun rule ->
             match filter_commit rule commit.files with
             | false -> None
             | true -> Some rule.chan)
      )
    | Some p ->
      cfg.prefix_rules.rules
      |> List.filter_map ~f:(fun rule ->
           match touching_prefix rule p with
           | false -> None
           | true -> Some rule.chan)
  in
  match commit, chan_sub with
  | None, _ ->
    Lwt.return []
    (* return [] here because this case happens when the path is unavailable and
       the commit cannot be queried via api. an error messege will be generated,
       and thus the bot should not notify any channels *)
  | _, [] ->
    let notifs = Option.value_map cfg.prefix_rules.default ~default:[] ~f:(fun webhook -> [ webhook ]) in
    Lwt.return notifs
  | _, l -> Lwt.return l

let partition_commit cfg (n : status_notification) =
  match n.state with
  | Pending -> Lwt.return []
  | _ ->
    ( match%lwt Github.generate_query_commmit cfg n.commit.url n.commit.sha with
    | None ->
      Lwt.return []
      (* return [] here because this case happens when the path is unavailable and
         the commit cannot be queried via api. an error messege will be generated,
         and thus the bot should not notify any channels *)
    | Some commit ->
      let chan_sub =
        cfg.prefix_rules.rules
        |> List.filter_map ~f:(fun rule ->
             match filter_commit rule commit.files with
             | false -> None
             | true -> Some rule.chan)
      in
      ( match chan_sub with
      | [] ->
        let notifs = Option.value_map cfg.prefix_rules.default ~default:[] ~f:(fun webhook -> [ webhook ]) in
        Lwt.return notifs
      | l -> Lwt.return l
      )
    )*)

let generate_notifications cfg req =
  match req with
  | Github.Push n ->
    partition_push cfg n |> List.map ~f:(fun (webhook, n) -> webhook, generate_push_notification n) |> Lwt.return
  | Github.Pull_request n ->
    partition_pr cfg n |> List.map ~f:(fun webhook -> webhook, generate_pull_request_notification n) |> Lwt.return
  | Github.PR_review n ->
    partition_pr_review cfg n |> List.map ~f:(fun webhook -> webhook, generate_pr_review_notification n) |> Lwt.return
  | Github.PR_review_comment n ->
    partition_pr_review_comment cfg n
    |> List.map ~f:(fun webhook -> webhook, generate_pr_review_comment_notification n)
    |> Lwt.return
  | Github.Issue n ->
    partition_issue cfg n |> List.map ~f:(fun webhook -> webhook, generate_issue_notification n) |> Lwt.return
  | Github.Issue_comment n ->
    partition_issue_comment cfg n
    |> List.map ~f:(fun webhook -> webhook, generate_issue_comment_notification n)
    |> Lwt.return
  | Github.Commit_comment n ->
    let%lwt webhooks = partition_commit_comment cfg n in
    let notifs = List.map ~f:(fun webhook -> webhook, generate_commit_comment_notification n) webhooks in
    Lwt.return notifs
  | Github.Status n ->
    let%lwt webhooks = partition_status cfg n in
    let notifs = List.map ~f:(fun webhook -> webhook, generate_status_notification n) webhooks in
    Lwt.return notifs
  | _ -> Lwt.return []

let print_prefix_routing rules =
  let show_match l = String.concat ~sep:" or " @@ List.map ~f:(fun s -> s ^ "*") l in
  rules
  |> List.iter ~f:(fun rule ->
       begin
         match rule.prefix, rule.ignore with
         | [], [] -> Stdio.printf "  any"
         | l, [] -> Stdio.printf "  %s" (show_match l)
         | [], l -> Stdio.printf "  not %s" (show_match l)
         | l, i -> Stdio.printf "  %s and not %s" (show_match l) (show_match i)
       end;
       Stdio.printf " -> #%s\n%!" rule.chan)

let print_label_routing rules =
  let show_match l = String.concat ~sep:" or " @@ List.map ~f:(fun s -> s ^ "*") l in
  rules
  |> List.iter ~f:(fun rule ->
       begin
         match rule.label_name, rule.ignore with
         | [], [] -> Stdio.printf "  any"
         | l, [] -> Stdio.printf "  %s" (show_match l)
         | [], l -> Stdio.printf "  not %s" (show_match l)
         | l, i -> Stdio.printf "  %s and not %s" (show_match l) (show_match i)
       end;
       Stdio.printf " -> #%s\n%!" rule.chan)
