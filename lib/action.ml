open Devkit
open Printf
open Base
open Slack
open Notabot_t
open Config
open Github_j

let log = Log.from "action"

let touching_push rule files =
  let has_prefix s = List.exists ~f:(fun prefix -> String.is_prefix s ~prefix) in
  files
  |> List.exists ~f:(fun file ->
       (List.is_empty rule.prefix || has_prefix file rule.prefix) && not (has_prefix file rule.ignore))

let filter_push rule commits =
  commits
  |> List.filter ~f:(fun commit ->
       touching_push rule commit.added || touching_push rule commit.removed || touching_push rule commit.modified)

let touching_label rule name =
  let name_lc = String.lowercase name in
  let label_lc = List.map rule.label_name ~f:(fun l -> String.lowercase l) in
  let ignore_lc = List.map rule.ignore ~f:(fun l -> String.lowercase l) in
  (* convert both labels and config into lowe-case to make label matching case-insensitive *)
  (List.is_empty label_lc || List.mem ~equal:String.equal label_lc name_lc)
  && not (List.mem ~equal:String.equal ignore_lc name_lc)

let exist_label rule labels = labels |> List.exists ~f:(fun (label : label) -> touching_label rule label.name)

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

let partition_push cfg n =
  let commits =
    n.commits
    |> List.filter ~f:(fun c -> c.distinct)
    |> List.filter ~f:(fun c ->
         let skip = is_main_merge_message c.message n cfg in
         if skip then log#info "main branch merge, ignoring %s: %s" c.id (first_line c.message);
         not skip)
  in
  cfg.prefix_rules.rules
  |> List.filter_map ~f:(fun rule ->
       match filter_push rule commits with
       | [] -> None
       | l -> Some (rule, { n with commits = l }))

let partition_label cfg labels =
  match labels with
  | [] -> Option.value_map cfg.label_rules.default ~default:[] ~f:(fun webhook -> [ webhook ])
  | labels ->
    cfg.label_rules.rules
    |> List.filter_map ~f:(fun rule ->
         match exist_label rule labels with
         | false -> None
         | true -> Some rule.chan)

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

let touching_commit rule filename =
  let has_prefix s = List.exists ~f:(fun prefix -> String.is_prefix s ~prefix) in
  (List.is_empty rule.prefix || has_prefix filename rule.prefix) && not (has_prefix filename rule.ignore)

let partition_commit_comment cfg n =
  let path = n.comment.path in
  match path with
  | None -> Option.value_map cfg.prefix_rules.default ~default:[] ~f:(fun webhook -> [ webhook ])
  | Some p ->
    cfg.prefix_rules.rules
    |> List.filter_map ~f:(fun rule ->
         match touching_commit rule p with
         | false -> None
         | true -> Some rule.chan)

let filter_commit rule files = files |> List.exists ~f:(fun file -> touching_commit rule file.filename)

let partition_commit cfg (n : status_notification) =
  match n.state with
  | Pending -> Lwt.return []
  | _ ->
    ( match%lwt Github.generate_query_commmit cfg n with
    | None -> Lwt.return []
    | Some commit ->
      cfg.prefix_rules.rules
      |> List.filter_map ~f:(fun rule ->
           match filter_commit rule commit.files with
           | false -> None
           | true -> Some rule.chan)
      |> Lwt.return
    )

let generate_notifications cfg req =
  match req with
  | Github.Push n ->
    partition_push cfg n
    |> List.map ~f:(fun ((rule : prefix_rule), n) -> rule.chan, generate_push_notification n)
    |> Lwt.return
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
    partition_commit_comment cfg n
    |> List.map ~f:(fun webhook -> webhook, generate_commit_comment_notification n)
    |> Lwt.return
  | Github.Status n ->
    let%lwt webhooks = partition_commit cfg n in
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
