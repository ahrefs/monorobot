open Base
open Httpaf
module Sys = Caml.Sys

let request_handler (_ : Unix.sockaddr) (reqd : Reqd.t) =
  match
    Reqd.try_with reqd (fun () ->
        let { Request.meth; headers; _ } = Reqd.request reqd in
        match meth with
        | `POST ->
          let error_message = ref None in
          let payload_event = ref Github_events.No_event in
          let response_code =
            ref
              ( match Request_validation.validate_github_payload_headers headers with
              | Ok github_event ->
                payload_event := github_event;
                `OK
              | Error (response_status, msg) ->
                error_message := Some msg;
                response_status )
          in
          (* Still not parsing the body payload *)
          (* let body_payload = "" in
             ( match Request_validation.validate_payload_actions !payload_event body_payload with
             (* if ok, generate notification according to event type *)
             | Ok webhook_event_type -> ()
             | Error (status, message) ->
              error_message := Some message;
              response_code := status ); *)
          if Option.is_some !error_message then
            Stdio.Out_channel.print_endline ("Incoming Request blocked: " ^ Option.value_exn !error_message);
          let headers = Headers.of_list [ "Connection", "close" ] in
          Reqd.respond_with_string reqd (Response.create ~headers !response_code) ""
        | meth ->
          let response_body = Printf.sprintf "%s is not an allowed method\n" (Method.to_string meth) in
          let headers = Headers.of_list [ "Connection", "close" ] in
          Reqd.respond_with_string reqd (Response.create ~headers `Method_not_allowed) response_body)
  with
  | Ok _ -> ()
  | Error exn -> Stdio.print_endline ("Error Found: " ^ Exn.to_string exn)
