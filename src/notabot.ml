open Base
open Lwt.Infix
open Httpaf_lwt_unix
module Arg = Caml.Arg

let error_handler (_ : Unix.sockaddr) ?request:_ error start_response =
  let open Httpaf in
  let response_body = start_response Headers.empty in
  begin match error with
  | `Exn exn -> Stdio.printf "Request error. Reason: %s.\n" (Exn.to_string exn)
  | #Status.standard as error -> Body.write_string response_body (Status.default_reason_phrase error)
  end;
  Body.close_writer response_body

let main port =
  Stdio.print_endline "Notabot starting";
  let cfg = Notabot_j.config_of_string @@ Stdio.In_channel.read_all "notabot.json" in
  Stdio.print_endline "Using routing:";
  Action.print_routing cfg.rules;
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  let request_handler = Request_handler.request_handler cfg in
  Lwt.async (fun () ->
      Lwt_io.establish_server_with_client_socket listen_address
        (Server.create_connection_handler ~request_handler ~error_handler)
      >|= fun _server ->
      Stdio.printf "Listening on port %i\n%!" port);
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever

let () =
  let port = ref 8080 in
  Arg.parse
    [ "-p", Arg.Set_int port, " Listening port number (8080 by default)" ]
    ignore "Notabot, the notifications bot. Your options:";
  main !port
