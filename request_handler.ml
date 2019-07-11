open Httpaf
open Base
open Lwt.Infix
module Github_notifications = Github_notifications_handler

let read_body response_body =
  let open Httpaf in
  let buf = Buffer.create 0x2000 in
  let body_read, notify_body_read = Lwt.wait () in
  let rec read_fn () =
    Body.schedule_read response_body
      ~on_eof:(fun () ->
        Body.close_reader response_body;
        Lwt.wakeup_later notify_body_read (Buffer.contents buf))
      ~on_read:(fun response_fragment ~off ~len ->
        let response_fragment_bytes = Bytes.create len in
        Lwt_bytes.blit_to_bytes response_fragment off response_fragment_bytes 0 len;
        Buffer.add_bytes buf response_fragment_bytes;
        read_fn ())
  in
  read_fn ();
  body_read

let send_response reqd response_body status =
  let headers = Headers.of_list [ "Content-Length", Int.to_string (String.length response_body) ] in
  Reqd.respond_with_string reqd (Response.create ~headers status) response_body

let request_handler (_ : Unix.sockaddr) (reqd : Httpaf.Reqd.t) =
  let { Request.meth; target; headers; _ } = Reqd.request reqd in
  match meth with
  | `POST ->
    ( match target with
    | "/github" ->
      read_body (Reqd.request_body reqd)
      >|= (fun body ->
            let open Github_notifications_handler in
            let github_event = validate_request_headers headers in
            match github_event with
            | Ok event_name ->
              Reqd.try_with reqd (fun () ->
                  (* here we will map the payloads to the right notification creation function *)
                  let created_notification =
                    match parse_notification_payload body event_name with
                    | Push push_commit_notification -> push_commit_notification.ref
                    | Pull_request pr_notification -> Int.to_string pr_notification.number
                    | CI_run ci_run_notification -> ci_run_notification.target_url
                  in
                  Stdio.print_endline created_notification;
                  send_response reqd "" `OK)
              |> ignore
            | Error error_message ->
              Stdio.printf "Github notification bad request: %s\n" error_message;
              send_response reqd "" `Bad_request)
      |> ignore
    | _ -> send_response reqd "" `Not_found )
  | meth ->
    let response_body = Printf.sprintf "%s is not an allowed method\n" (Method.to_string meth) in
    send_response reqd response_body `Method_not_allowed
