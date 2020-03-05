open Base
open Lwt.Infix
open Httpaf_lwt_unix
module Arg = Caml.Arg

let error_handler (_ : Unix.sockaddr) ?request:_ error start_response =
  let open Httpaf in
  let response_body = start_response Headers.empty in
  begin match error with
  | `Exn exn -> Log.line "Request error. Reason: %s." (Exn.to_string exn)
  | #Status.standard as error -> Body.write_string response_body (Status.default_reason_phrase error)
  end;
  Body.close_writer response_body

let get_config () =
  let cfg = Notabot_j.config_of_string @@ Stdio.In_channel.read_all "notabot.json" in
  Stdio.print_endline "Using routing:";
  Action.print_routing cfg.rules;
  cfg

let main port =
  Log.line "Notabot starting";
  let cfg = get_config () in
  Log.line "Signature checking %s" (if Option.is_some cfg.gh_webhook_secret then "enabled" else "disabled");
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  let request_handler = Request_handler.request_handler cfg in
  Lwt.async (fun () ->
      Lwt_io.establish_server_with_client_socket listen_address
        (Server.create_connection_handler ~request_handler ~error_handler)
      >|= fun _server ->
      Log.line "Listening on port %i" port);
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever

let check file =
  let cfg = get_config () in
  let headers = Httpaf.Headers.of_list ["X-GitHub-Event","push"] in
  match Github.parse_exn ~secret:None headers (Stdio.In_channel.read_all file) with
  | exception exn -> Log.line "E: error while parsing payload : %s" (Exn.to_string exn)
  | event ->
  Action.generate_notifications cfg event |> List.iter ~f:begin fun (webhook, msg) ->
    Log.line "Will notify %s%s" webhook.Notabot_t.channel (match msg.Slack_t.text with None -> "" | Some s -> Printf.sprintf " with %S" s)
  end

let () =
  let port = ref 8080 in
  Arg.parse [
    "-p", Arg.Set_int port, " Listening port number (8080 by default)";
    "-check", Arg.String (fun file -> check file; Caml.exit 0), " Read github notification payload from file and show actions to be taken";
    ] ignore "Notabot, the notifications bot. Your options:";
  main !port
