open Httpaf
open Base
open Github_events_notifications_t
open Configuration.Env

type event_name =
  | Push_event
  | Pull_request_event
  | CI_run_event

type t =
  | Push of commit_pushed_notification
  | Pull_request of pr_notification
  | CI_run of ci_build_notification

let extract_header validate headers name = Option.value_map ~default:(Error "") ~f:validate (Headers.get headers name)

let validate_notification_type notification_type =
  match notification_type with
  | "push" -> Ok Push_event
  | "pull_request" -> Ok Pull_request_event
  | "check_suite" -> Ok CI_run_event
  | _ -> Error ""

let validate_header_env_var optional_env_var header =
  Option.value_map ~default:(Error "")
    ~f:(fun env_var ->
      match String.equal header env_var with
      | true -> Ok ()
      | false -> Error "")
    optional_env_var

let parse_notification_payload body event_name =
  match event_name with
  | Ok Push_event -> Ok (Push (Github_events_notifications_j.commit_pushed_notification_of_string body))
  | Ok Pull_request_event -> Ok (Pull_request (Github_events_notifications_j.pr_notification_of_string body))
  | Ok CI_run_event -> Ok (CI_run (Github_events_notifications_j.ci_build_notification_of_string body))
  | Error _ -> Error "Invalid event type"

let validate_request_event_headers headers =
  (* Ensure env vars *)
  let () = List.iter ~f:ensure_env [ "SHA1_SIG"; "GITHUB_AGENT" ] in
  let github_event = extract_header validate_notification_type headers "X-GitHub-Event" in
  let validate_github_request_signature = validate_header_env_var Configuration.Env.github_sha1_signature in
  let validate_github_user_agent = validate_header_env_var Configuration.Env.github_user_agent in
  let notification_signature = extract_header validate_github_request_signature headers "X-Hub-Signature" in
  let user_agent = extract_header validate_github_user_agent headers "User-agent" in
  (* Temos de garantir que o tipo de request event bate certo com o payload *)
  match notification_signature, user_agent, github_event with
  | Ok _, Ok _, Ok _ -> github_event
  | _, _, _ -> Error "Headers validation failed"

(* validar as headers 👆
   // garantir vars ambiente
   let () = List.iter ensure_env ["SHA1_SIG"; "GITHUB_AGENT"] 

   // garantir que existem
   List.for_all ~f:is_some [user_agent; github_event; ...]

   // ?function to extract check the presence of an env variable.?
   Option.value_map ~default:Github_events.No_event ~f:Github_events.from_string github_event
*)
