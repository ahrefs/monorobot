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
  Stdio.print_endline "";
  Stdio.print_endline @@ sprintf "Request received: %s %s." (Method.to_string meth) target

let reply_with_bad_request reqd handler failing_function error_message =
  Stdio.print_endline @@ sprintf "%s notification bad request. While %s: %s" handler failing_function error_message;
  send_response reqd "" `Bad_request

let request_handler cfg (_ : Unix.sockaddr) (reqd : Httpaf.Reqd.t) =
  let { Request.meth; target; headers; _ } = Reqd.request reqd in
  log_incoming_request reqd;
  match meth with
  | `POST ->
    ( match target with
    | "/github" ->
      read_body (Reqd.request_body reqd)
      >|= (fun body ->
            Headers.to_list headers |> List.iter ~f:(fun (k,v) -> Stdio.print_endline @@ sprintf "%s: %s" k v);
            Stdio.print_endline body;
            Stdio.print_endline "";
            let event =
              try Ok (Github.parse_exn ~secret:cfg.Notabot_t.gh_webhook_secret headers body)
              with exn -> Error (sprintf "Error while parsing payload : %s" (Exn.to_string exn))
            in
            match event with
            | Ok payload ->
              let () =
                Action.generate_notifications cfg payload |> List.iter ~f:begin fun (webhook, msg) ->
                  Slack.send_notification webhook msg
                  >|= (function
                        | Some (sc, txt) ->
                          Stdio.print_endline @@ sprintf "Sent notification to #%s. Code: %i. Response: %s." webhook.channel sc txt
                        | _ -> ())
                  |> ignore
                end
              in
              send_response reqd "" `OK
            | Error error_message -> reply_with_bad_request reqd "Github" "parsing payload" error_message)
      |> ignore
    | _ -> send_response reqd "" `Not_found )
  | meth ->
    let response_body = sprintf "%s is not an allowed method\n" (Method.to_string meth) in
    send_response reqd response_body `Method_not_allowed
