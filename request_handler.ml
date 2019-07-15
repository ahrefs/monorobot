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
            let parsed_payload =
              try parse_notification_payload body (validate_request_event_headers headers)
              with exn ->
                Error
                  (Printf.sprintf "Error while parsing %s payload: %s"
                     ( match Headers.get headers "X-Github-Event" with
                     | Some event -> event
                     | None -> "" )
                     (Exn.to_string exn))
            in
            match parsed_payload with
            | Ok payload ->
              let notification_detail =
                match payload with
                | Push push_commit_notification -> push_commit_notification.ref
                | Pull_request pr_notification -> Int.to_string pr_notification.number
                | CI_run ci_run_notification -> ci_run_notification.target_url
              in
              Stdio.print_endline notification_detail;
              send_response reqd "" `OK
            | Error error_message ->
              Stdio.print_endline (Printf.sprintf "Github notification bad request: %s" error_message);
              send_response reqd "" `Bad_request)
      |> ignore
    | _ -> send_response reqd "" `Not_found )
  | meth ->
    let response_body = Printf.sprintf "%s is not an allowed method\n" (Method.to_string meth) in
    send_response reqd response_body `Method_not_allowed
