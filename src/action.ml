open Slack

let generate_notification req =
  match req with
  | Github.Push n -> Ok (generate_push_notification n)
  | Pull_request n when n.action = Opened -> Ok (generate_pull_request_notification n)
  | CI_run n when is_success_or_failed n.state -> Ok (generate_ci_run_notification n)
  | _ -> Error ()
