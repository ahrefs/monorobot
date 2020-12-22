open Devkit
open Base
open Lib
module Arg = Caml.Arg
open Cmdliner

let log = Log.from "monorobot"

(* entrypoints *)

let http_server_action addr port config secrets state =
  log#info "monorobot starting";
  let ctx = Context.make ~config_filename:config ~secrets_filepath:secrets ?state_filepath:state () in
  Lwt_main.run
    ( match%lwt Context.refresh_secrets ctx with
    | Error e ->
      log#error "%s" e;
      Lwt.return_unit
    | Ok ctx ->
      ( match%lwt Context.refresh_state ctx with
      | Error e ->
        log#error "%s" e;
        Lwt.return_unit
      | Ok ctx -> Request_handler.run ~ctx ~addr ~port
      )
    )

(** In check mode, instead of actually sending the message to slack, we
    simply print it in the console *)
let check_gh_action file json config secrets state =
  Lwt_main.run
    begin
      match Github.event_of_filename file with
      | None ->
        log#error "aborting because payload %s is not named properly, named should be KIND.NAME_OF_PAYLOAD.json" file;
        Lwt.return_unit
      | Some kind ->
        let headers = [ "x-github-event", kind ] in
        ( match%lwt Common.get_local_file file with
        | Error e ->
          log#error "%s" e;
          Lwt.return_unit
        | Ok body ->
          let ctx = Context.make ~config_filename:config ~secrets_filepath:secrets ?state_filepath:state () in
          let%lwt () =
            if json then
              let module Action = Action.Action (Api_remote.Github) (Api_local.Slack_json) in
              Action.process_github_notification ctx headers body
            else
              let module Action = Action.Action (Api_remote.Github) (Api_local.Slack_simple) in
              Action.process_github_notification ctx headers body
          in
          Lwt.return_unit
        )
    end

let check_slack_action url file =
  let data = Stdio.In_channel.read_all file in
  let chan = Printf.sprintf "webhook %s" url in
  match Slack_j.webhook_notification_of_string data with
  | exception exn -> log#error ~exn "unable to parse notification"
  | msg ->
    Lwt_main.run
      ( match%lwt Api_remote.Slack.send_notification ~chan ~msg ~url with
      | Error e ->
        log#error "%s" e;
        Lwt.return_unit
      | Ok () -> Lwt.return_unit
      )

(* flags *)

let addr =
  let doc = "http listen addr" in
  Arg.(value & opt string "127.0.0.1" & info [ "a"; "addr" ] ~docv:"ADDR" ~doc)

let port =
  let doc = "http listen port" in
  Arg.(value & opt int 8080 & info [ "p"; "port" ] ~docv:"PORT" ~doc)

let config =
  let doc = "remote configuration file name" in
  Arg.(value & opt string "notabot.json" & info [ "config" ] ~docv:"CONFIG" ~doc)

let secrets =
  let doc = "configuration file containing secrets" in
  Arg.(value & opt file "secrets.json" & info [ "secrets" ] ~docv:"SECRETS" ~doc)

let state =
  let doc = "state file" in
  Arg.(value & opt (some file) None & info [ "state" ] ~docv:"STATE" ~doc)

let gh_payload =
  let doc = "JSON file containing a github webhook payload" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"GH_PAYLOAD" ~doc)

let slack_webhook_url =
  let doc = "slack webhook url" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"SLACK_WEBHOOK" ~doc)

let slack_payload =
  let doc = "JSON file containing a slack notification" in
  Arg.(required & pos 1 (some file) None & info [] ~docv:"SLACK_PAYLOAD" ~doc)

let json =
  let doc = "display output as json" in
  Arg.(value & flag & info [ "j"; "json" ] ~docv:"JSON" ~doc)

(* commands *)

let run =
  let doc = "launch the http server" in
  let info = Term.info "run" ~doc in
  let term = Term.(const http_server_action $ addr $ port $ config $ secrets $ state) in
  term, info

let check_gh =
  let doc = "read a Github notification from a file and display the actions that will be taken; used for testing" in
  let info = Term.info "check_gh" ~doc in
  let term = Term.(const check_gh_action $ gh_payload $ json $ config $ secrets $ state) in
  term, info

let check_slack =
  let doc = "read a Slack notification from a file and send it to a webhook; used for testing" in
  let info = Term.info "check_slack" ~doc in
  let term = Term.(const check_slack_action $ slack_webhook_url $ slack_payload) in
  term, info

let default_cmd =
  let doc = "the notification bot" in
  Term.(ret (const (`Help (`Pager, None)))), Term.info "monorobot" ~doc

let cmds = [ run; check_gh; check_slack ]

let () = Term.(exit @@ eval_choice default_cmd cmds)
