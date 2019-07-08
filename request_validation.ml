open Httpaf
open Base
module List = Caml.List
module Sys = Caml.Sys

exception Invalid_github_request of string

let validate_github_post_notification (headers : Headers.t) =
  let allowed_event_notifications = [ "push"; "pull_request"; "check_suite" ] in
  let user_agent = Headers.get headers "User-Agent" in
  let github_event = Headers.get headers "X-GitHub-Event" in
  let notification_signature = Headers.get headers "X-Hub-Signature" in
  let sha1_signature = Sys.getenv "SHA1_SIG" in
  let github_agent = Sys.getenv "GITHUB_AGENT" in
  let open Option in
  let open Status in
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
        Ok (Option.value ~default:"" github_event)
      | _, _, _ -> Error (`Bad_request, "Request received is not from github or signature doesn't match config") )
    else Error (`Bad_request, "Request event not allowed") )
  else Error (`Bad_request, "Necessary request headers not present")
