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
  match Context.refresh_secrets ctx with
  | Error e -> log#error "%s" e
  | Ok ctx ->
  match Context.refresh_state ctx with
  | Error e -> log#error "%s" e
  | Ok ctx -> Lwt_main.run (Request_handler.run ~ctx ~addr ~port)

(** In check mode, instead of actually sending the message to slack, we
    simply print it in the console *)
let check_gh_action file json config secrets state =
  match Github.event_of_filename (Caml.Filename.basename file) with
  | None ->
    log#error "aborting because payload %s is not named properly, named should be KIND.NAME_OF_PAYLOAD.json" file
  | Some kind ->
  match Common.get_local_file file with
  | Error e -> log#error "%s" e
  | Ok body ->
    let headers = [ "x-github-event", kind ] in
    let ctx = Context.make ~config_filename:config ~secrets_filepath:secrets ?state_filepath:state () in
    ( match Context.refresh_secrets ctx with
    | Error e -> log#error "%s" e
    | Ok ctx ->
      Lwt_main.run
        ( if json then
          let module Action = Action.Action (Api_remote.Github) (Api_local.Slack_json) in
          Action.process_github_notification ctx headers body
        else
          let module Action = Action.Action (Api_remote.Github) (Api_local.Slack_simple) in
          Action.process_github_notification ctx headers body
        )
    )

let check_slack_action file secrets =
  let data = Stdio.In_channel.read_all file in
  let ctx = Context.make ~secrets_filepath:secrets () in
  match Slack_j.post_message_req_of_string data with
  | exception exn -> log#error ~exn "unable to parse notification"
  | msg ->
  match Context.refresh_secrets ctx with
  | Error e -> log#error "%s" e
  | Ok ctx ->
    Lwt_main.run
      ( match%lwt Api_remote.Slack.send_notification ~ctx ~msg with
      | Error e ->
        log#error "%s" e;
        Lwt.return_unit
      | Ok (_res : Slack_t.post_message_res) -> Lwt.return_unit
      )

(* flags *)

let addr =
  let doc = "ip address that the http server should use" in
  Arg.(value & opt string "127.0.0.1" & info [ "a"; "addr" ] ~docv:"ADDR" ~doc)

let port =
  let doc = "port number that the http server should use" in
  Arg.(value & opt int 8080 & info [ "p"; "port" ] ~docv:"PORT" ~doc)

let config =
  let doc = "name of the remote configuration file to retrieve from the root directory of a repository's main branch" in
  Arg.(value & opt string ".monorobot.json" & info [ "config" ] ~docv:"CONFIG" ~doc)

let secrets =
  let doc = "path to a local configuration file containing secrets to load on startup" in
  Arg.(value & opt file "secrets.json" & info [ "secrets" ] ~docv:"SECRETS" ~doc)

let state =
  let doc =
    "path to a local file containing runtime state to load on startup; if specified file does not yet exist, a new one \
     will be created; if unspecified, no state will be persisted during execution"
  in
  Arg.(value & opt (some string) None & info [ "state" ] ~docv:"STATE" ~doc)

let gh_payload =
  let doc = "path to a JSON file containing a github webhook payload" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"GH_PAYLOAD" ~doc)

let slack_payload =
  let doc = "path to a JSON file containing a slack notification payload" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"SLACK_PAYLOAD" ~doc)

let json =
  let doc = "if set, will format output as json" in
  Arg.(value & flag & info [ "j"; "json" ] ~docv:"JSON" ~doc)

(* commands *)

let run =
  let doc = "launch the http server" in
  let info = Cmd.info "run" ~doc in
  let term = Term.(const http_server_action $ addr $ port $ config $ secrets $ state) in
  Cmd.v info term

let check_gh =
  let doc = "read a Github notification from a file and display the actions that will be taken; used for testing" in
  let info = Cmd.info "check_gh" ~doc in
  let term = Term.(const check_gh_action $ gh_payload $ json $ config $ secrets $ state) in
  Cmd.v info term

let check_slack =
  let doc = "read a Slack notification from a file and send it to a channel; used for testing" in
  let info = Cmd.info "check_slack" ~doc in
  let term = Term.(const check_slack_action $ slack_payload $ secrets) in
  Cmd.v info term

let default, info =
  let doc = "the notification bot" in
  Term.(ret (const (`Help (`Pager, None)))), Cmd.info "monorobot" ~doc ~version:Version.current

let () =
  let cmds = [ run; check_gh; check_slack ] in
  let group = Cmd.group ~default info cmds in
  Caml.exit @@ Cmd.eval group
