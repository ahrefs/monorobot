open Base
open Httpaf
module Sys = Caml.Sys

let request_handler (_ : Unix.sockaddr) (reqd : Reqd.t) =
  match
    Reqd.try_with reqd (fun () ->
        let { Request.meth; headers; _ } = Reqd.request reqd in
        match meth with
        | `POST ->
          let payload_event = ref "" in
          let response_code = ref (match Request_validation.validate_github_post_notification headers with
              | Ok github_event -> 
                payload_event := github_event;
                `OK
              | Error (status, msg) -> 
                Stdio.Out_channel.print_endline ("Incoming Request blocked: " ^ msg);
                status)
          in
          let headers = Headers.of_list [ "Connection", "close" ] in
          Reqd.respond_with_string reqd (Response.create ~headers !response_code) ""
        | meth ->
          let response_body = Printf.sprintf "%s is not an allowed method\n" (Method.to_string meth) in
          let headers = Headers.of_list [ "Connection", "close" ] in
          Reqd.respond_with_string reqd (Response.create ~headers `Method_not_allowed) response_body)
  with
  | Ok _ -> ()
  | Error exn -> Stdio.print_endline ("Error Found: " ^ Exn.to_string exn)

(* When we get the request, check user agent x-github-event and x-hub-signature headers *)
