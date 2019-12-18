open Base
open Slack

let touching rule files =
  let open Notabot_t in
  files |> List.exists ~f:begin fun file ->
    String.is_prefix file ~prefix:rule.prefix && not (List.exists ~f:(fun ignore -> String.is_prefix file ~prefix:ignore) rule.ignore)
  end

let filter rule commits =
  let open Github_t in
  commits |> List.filter ~f:(fun commit -> touching rule commit.added || touching rule commit.removed || touching rule commit.modified)

let partition_push rules n =
  rules |> List.filter_map ~f:begin fun rule ->
    match filter rule n.Github_t.commits with
    | [] -> None
    | l -> Some (rule, { n with commits = l })
  end

let generate_notifications cfg req =
  let open Notabot_t in
  match req with
  | Github.Push n -> partition_push cfg.rules n |> List.map ~f:(fun (rule,n) -> rule.webhook, generate_push_notification n)
(*   | Pull_request n when Poly.(n.action = Opened) -> [slack_notabot, generate_pull_request_notification n] *)
(*   | CI_run n when Poly.(n.state <> Success) -> [slack_notabot, generate_ci_run_notification n] *)
  | _ -> []
