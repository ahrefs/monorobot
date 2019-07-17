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

let log_incoming_request reqd =
  let { Request.meth; target; _ } = Reqd.request reqd in
  Stdio.print_endline (Printf.sprintf "Request received: %s %s." (Method.to_string meth) target)

let headers_stringified_json headers =
  let headers_list = Headers.to_list headers in
  let headers_len = List.length headers_list in
  let end_string len i =
    match Int.equal (len - 1) i with
    | true -> " }"
    | false -> ", "
  in
  let stringify i acc header =
    let name, value = header in
    acc ^ Printf.sprintf "\"%s\": \"%s\"%s" name value (end_string headers_len i)
  in
  List.foldi ~init:"{ " ~f:stringify headers_list

let reply_with_bad_request reqd handler failing_function error_message headers =
  Stdio.print_endline
    (Printf.sprintf "%s notification bad request. While running %s: %s. Headers: %s" handler failing_function
       error_message (headers_stringified_json headers));
  send_response reqd "" `Bad_request

let request_handler (_ : Unix.sockaddr) (reqd : Httpaf.Reqd.t) =
  let { Request.meth; target; headers; _ } = Reqd.request reqd in
  log_incoming_request reqd;
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
              let notification = Notabot.Github.Console.generate_notification payload in
              let serialized_notification = Result.map ~f:Notabot.Github.Console.serialize_notification notification in
              ( match serialized_notification with
              | Ok serialized_notification' ->
                Stdio.print_endline serialized_notification';
                send_response reqd "" `OK
              | Error error_message ->
                reply_with_bad_request reqd "Github" "serialize_notification" error_message headers )
            | Error error_message ->
              reply_with_bad_request reqd "Github" "parse_notification_payload" error_message headers)
      |> ignore
    | _ -> send_response reqd "" `Not_found )
  | meth ->
    let response_body = Printf.sprintf "%s is not an allowed method\n" (Method.to_string meth) in
    send_response reqd response_body `Method_not_allowed
