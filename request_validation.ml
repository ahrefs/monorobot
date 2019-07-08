open Httpaf
open Base
module List = Caml.List
module Sys = Caml.Sys

let validate_github_payload_headers (headers : Headers.t) =
  let allowed_event_notifications = [ "push"; "pull_request"; "check_suite" ] in
  let user_agent = Headers.get headers "User-Agent" in
  let github_event = Headers.get headers "X-GitHub-Event" in
  let notification_signature = Headers.get headers "X-Hub-Signature" in
  let sha1_signature = Sys.getenv "SHA1_SIG" in
  let github_agent = Sys.getenv "GITHUB_AGENT" in
  let open Option in
  (* Check if the headers exist *)
  if is_some user_agent && is_some github_event && is_some notification_signature then (
    let is_allowed_event =
      List.exists
        (fun allowed_event ->
           match github_event with
           | Some event -> String.equal event allowed_event
           | None -> false)
        allowed_event_notifications
    in
    (* Check if the event and the headers values are allowed *)
    if is_allowed_event then (
      match user_agent, github_event, notification_signature with
      | Some user_agent', Some _, Some req_sig
        when String.equal req_sig sha1_signature && String.equal github_agent user_agent' ->
        Ok (Github_events.from_string (Option.value ~default:"" github_event))
      | _, _, _ -> Error (`Bad_request, "Request received is not from github or signature doesn't match config") )
    else Error (`Bad_request, "Request event type not allowed") )
  else Error (`Bad_request, "Necessary request headers not present")

let validate_payload_actions event_type payload =
  let open Github_events in
  match event_type with
  | Push -> Ok Push
  | Pull_request ->
    let pull_request = Events_notifications_j.pr_notification_of_string payload in
    ( match pull_request.action with
      | "review_requested" -> Ok Pull_request
      | _ -> Error (`Bad_request, "Pull request notification with wrong action") )
  | Check_suite ->
    let ci_notification = Events_notifications_j.ci_build_notification_of_string payload in
    ( match ci_notification.state with
      | "success" -> Ok Check_suite
      | "failure" -> Ok Check_suite
      | _ -> Error (`Bad_request, "CI Build Status: do we care about other statuses?") )
  | _ -> Error (`Bad_request, "Impossible state?")
