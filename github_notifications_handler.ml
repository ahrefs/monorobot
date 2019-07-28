open Httpaf
open Base
open Github_events_notifications_t

type event_name =
  | Push_event
  | Pull_request_event
  | CI_run_event

type t =
  | Push of commit_pushed_notification
  | Pull_request of pr_notification
  | CI_run of ci_build_notification

let parse_notification_payload body event_name =
  match event_name with
  | Ok Push_event -> Ok (Push (Github_events_notifications_j.commit_pushed_notification_of_string body))
  | Ok Pull_request_event -> Ok (Pull_request (Github_events_notifications_j.pr_notification_of_string body))
  | Ok CI_run_event -> Ok (CI_run (Github_events_notifications_j.ci_build_notification_of_string body))
  | Error error_message -> Error error_message

let calculate_request_hash key body = Cstruct.of_string body |> Nocrypto.Hash.SHA1.hmac ~key |> Cstruct.to_string

let is_valid_request_signature headers_sig body =
  let github_webhook_secret = Lazy.force Configuration.Env.github_webhook_secret_token in
  let (`Hex request_hash) = calculate_request_hash (Cstruct.of_string github_webhook_secret) body |> Hex.of_string in
  String.equal headers_sig (Printf.sprintf "sha1=%s" request_hash)

let validate_request_event_headers headers body =
  let get_headers = Headers.get headers in
  match get_headers "X-Hub-Signature", get_headers "X-GitHub-Event" with
  | Some req_sig, Some event_type when is_valid_request_signature req_sig body ->
    ( match event_type with
      | "push" -> Ok Push_event
      | "pull_request" -> Ok Pull_request_event
      | "check_suite" -> Ok CI_run_event
      | _ -> Error (Printf.sprintf "Unsupported github event: %s" event_type) )
  | _, _ -> Error "Headers validation failed"
