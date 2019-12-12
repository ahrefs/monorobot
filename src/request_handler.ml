open Printf
open Httpaf
open Base
open Lwt.Infix

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
  Stdio.print_endline (sprintf "Request received: %s %s." (Method.to_string meth) target)

let headers_stringified_json headers =
  let headers_list = Headers.to_list headers in
  let json_headers = `Assoc (List.map ~f:(fun (name, value) -> name, `String value) headers_list) in
  Yojson.Safe.to_string json_headers

let reply_with_bad_request reqd handler failing_function error_message headers =
  Stdio.print_endline
    (sprintf "%s notification bad request. While %s: %s. Headers: %s" handler failing_function error_message
       (headers_stringified_json headers));
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
            let event =
              try Github_notifications_handler.parse_exn headers body
              with exn -> Error (sprintf "Error while parsing payload : %s" (Exn.to_string exn))
            in
            match event with
            | Ok payload ->
              let open Github.Slack in
              let () =
                match generate_notification payload with
                | Ok serialized_notification ->
                  send_notification serialized_notification
                  >|= (function
                        | Some (sc, txt) ->
                          Stdio.print_endline @@ sprintf "Sent notification to Slack. Code: %i. Response: %s." sc txt
                        | _ -> ())
                  |> ignore
                | Error _ -> ()
              in
              send_response reqd "" `OK
            | Error error_message -> reply_with_bad_request reqd "Github" "parsing payload" error_message headers)
      |> ignore
    | _ -> send_response reqd "" `Not_found )
  | meth ->
    let response_body = sprintf "%s is not an allowed method\n" (Method.to_string meth) in
    send_response reqd response_body `Method_not_allowed
