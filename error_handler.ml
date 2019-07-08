open Httpaf

let error_handler (_ : Unix.sockaddr) ?request:_ _error start_response =
  let response_body = start_response Headers.empty in
  Body.close_writer response_body
