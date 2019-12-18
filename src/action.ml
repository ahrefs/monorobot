open Base
open Slack
open Notabot_t

let touching rule files =
  files |> List.exists ~f:begin fun file ->
    String.is_prefix file ~prefix:rule.prefix && not (List.exists ~f:(fun ignore -> String.is_prefix file ~prefix:ignore) rule.ignore)
  end

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
  rules |> List.iter ~f:begin fun rule ->
    Stdio.printf "  match %s*" rule.prefix;
    begin match rule.ignore with
    | [] -> ()
    | l -> Stdio.printf " and not %s" (String.concat ~sep:" or " @@ List.map ~f:(fun s -> s ^ "*") l)
    end;
    Stdio.printf " -> #%s\n%!" rule.webhook.channel;
  end
