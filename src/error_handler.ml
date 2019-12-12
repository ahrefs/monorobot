open Base
open Httpaf

let error_handler (_ : Unix.sockaddr) ?request:_ error start_response =
  let response_body = start_response Headers.empty in
  ( match error with
  | `Exn exn -> Stdio.printf "Request errored. Reason: %s.\n" (Exn.to_string exn)
  | #Status.standard as error -> Body.write_string response_body (Status.default_reason_phrase error) );
  Body.close_writer response_body
