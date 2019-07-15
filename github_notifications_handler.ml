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

let extract_header validate headers name =
  Option.value_map ~default:(Error (Printf.sprintf "Missing %s header\n" name)) ~f:validate (Headers.get headers name)

let validate_notification_type notification_type =
  match notification_type with
  | "push" -> Ok Push_event
  | "pull_request" -> Ok Pull_request_event
  | "check_suite" -> Ok CI_run_event
  | _ -> Error "Notification type not accepted"

let parse_notification_payload body event_name =
  match event_name with
  | Push_event -> Push (Github_events_notifications_j.commit_pushed_notification_of_string body)
  | Pull_request_event -> Pull_request (Github_events_notifications_j.pr_notification_of_string body)
  | CI_run_event -> CI_run (Github_events_notifications_j.ci_build_notification_of_string body)

let validate_request_headers headers =
  (* so far only getting the event type and not validating the other headers *)
  let github_event = extract_header validate_notification_type headers "X-GitHub-Event" in
  github_event
