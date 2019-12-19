open Base
open Slack
open Notabot_t

let touching rule files =
  let has_prefix s = List.exists ~f:(fun prefix -> String.is_prefix s ~prefix) in
  files |> List.exists ~f:(fun file -> (List.is_empty rule.prefix || has_prefix file rule.prefix) && not (has_prefix file rule.ignore))

let filter rule commits =
  let open Github_t in
  commits |> List.filter ~f:(fun commit -> touching rule commit.added || touching rule commit.removed || touching rule commit.modified)

let partition_push rules n =
  let open Github_t in
  let commits = n.commits |> List.filter ~f:(fun c -> c.distinct) in
  rules |> List.filter_map ~f:begin fun rule ->
    match filter rule commits with
    | [] -> None
    | l -> Some (rule, { n with commits = l })
  end

let generate_notifications cfg req =
  match req with
  | Github.Push n -> partition_push cfg.rules n |> List.map ~f:(fun (rule,n) -> rule.webhook, generate_push_notification n)
(*   | Pull_request n when Poly.(n.action = Opened) -> [slack_notabot, generate_pull_request_notification n] *)
(*   | CI_run n when Poly.(n.state <> Success) -> [slack_notabot, generate_ci_run_notification n] *)
  | _ -> []

let print_routing rules =
  let show_match l = String.concat ~sep:" or " @@ List.map ~f:(fun s -> s ^ "*") l in
  rules |> List.iter ~f:begin fun rule ->
    begin match rule.prefix, rule.ignore with
    | [], [] -> Stdio.printf "  any"
    | l, [] -> Stdio.printf "  %s" (show_match l)
    | [], l ->  Stdio.printf "  not %s" (show_match l)
    | l, i ->  Stdio.printf "  %s and not %s" (show_match l) (show_match i)
    end;
    Stdio.printf " -> #%s\n%!" rule.webhook.channel;
  end
