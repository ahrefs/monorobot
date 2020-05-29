open Printf
open Base
open Slack
open Notabot_t
open Github_t

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
  (List.is_empty rule.label_name || List.mem ~equal:String.equal rule.label_name name)
  && not (List.mem ~equal:String.equal rule.ignore name)

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
         if skip then Log.line "Main branch merge, ignoring %s : %s" c.id (first_line c.message);
         not skip)
  in
  cfg.push_rules
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
         | true -> Some rule.webhook)

let partition_pr cfg (n : pr_notification) =
  match n.action with
  | Opened | Closed | Reopened -> partition_label cfg n.pull_request.labels
  | _ -> []

let partition_issue cfg (n : issue_notification) =
  match n.action with
  | Opened | Closed | Reopened -> partition_label cfg n.issue.labels
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

let generate_notifications cfg req =
  match req with
  | Github.Push n ->
    partition_push cfg n |> List.map ~f:(fun ((rule : prefix_rule), n) -> rule.webhook, generate_push_notification n)
  | Github.Pull_request n ->
    partition_pr cfg n |> List.map ~f:(fun webhook -> webhook, generate_pull_request_notification n)
  | Github.PR_review n ->
    partition_pr_review cfg n |> List.map ~f:(fun webhook -> webhook, generate_pr_review_notification n)
  | Github.PR_review_comment n ->
    partition_pr_review_comment cfg n |> List.map ~f:(fun webhook -> webhook, generate_pr_review_comment_notification n)
  | Github.Issue n -> partition_issue cfg n |> List.map ~f:(fun webhook -> webhook, generate_issue_notification n)
  | Github.Issue_comment n ->
    partition_issue_comment cfg n |> List.map ~f:(fun webhook -> webhook, generate_issue_comment_notification n)
  (*   | CI_run n when Poly.(n.state <> Success) -> [slack_notabot, generate_ci_run_notification n] *)
  | _ -> []

let print_push_routing rules =
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
       Stdio.printf " -> #%s\n%!" rule.webhook.channel)

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
       Stdio.printf " -> #%s\n%!" rule.webhook.channel)
