open Devkit
open Base
open Lib
open Action
module Arg = Caml.Arg
open Cmdliner

let log = Log.from "monorobot"

(* entrypoints *)

let cfg_action_after_refresh (cfg : Config.t) =
  log#info "using prefix routing:";
  (* Rule.Prefix.print_prefix_routing cfg.prefix_rules.rules; *)
  log#info "using label routing:";
  (* Rule.Label.print_label_routing cfg.label_rules.rules; *)
  log#info "signature checking %s" (if Option.is_some cfg.gh_hook_token then "enabled" else "disabled")

let update_state_at_path state_path state event = State.save state_path @@ State.update_state state event

let http_server_action addr port config secrets state =
  log#info "monorobot starting";
  let ctx_thunk =
    Context.make_thunk ~state_path:state ~cfg_path_or_remote_filename:config ~secrets_path:secrets
      ~cfg_action_after_refresh ()
  in
  Lwt_main.run (Request_handler.start_http_server ~ctx_thunk ~addr ~port ())

let check_slack_action webhook file =
  let data = Stdio.In_channel.read_all file in
  match Slack_j.webhook_notification_of_string data with
  | exception exn -> log#error ~exn "unable to parse notification"
  | _ -> Lwt_main.run (Slack.send_notification webhook data)

let check_common file print config secrets state_path =
  let ctx_thunk =
    Context.make_thunk ~state_path ~cfg_path_or_remote_filename:config ~secrets_path:secrets ~cfg_action_after_refresh
      ()
  in
  let filename = Caml.Filename.basename file in
  match Github.event_of_filename filename with
  | None ->
    log#error "aborting because payload %s is not named properly, named should be KIND.NAME_OF_PAYLOAD.json" file;
    Lwt.return_unit
  | Some kind ->
    let headers = [ "x-github-event", kind ] in
    (* read the event from a file and try to parse it *)
    ( match Github.parse_exn ~secret:None headers (Stdio.In_channel.read_all file) with
    | exception exn ->
      log#error ~exn "unable to parse payload";
      Lwt.return_unit
    | event ->
      let%lwt ctx = Context.resolve_ctx_in_thunk ctx_thunk event in
      let%lwt notifs = Action.generate_notifications ctx event in
      List.iter ~f:print notifs;
      Lwt.return_unit
    )

let print_simplified_message (chan, msg) =
  (* In check mode, instead of actually sending the message to slack, we
     simply print it in the console *)
  log#info "will notify %s%s" chan
    ( match msg.Slack_t.text with
    | None -> ""
    | Some s -> Printf.sprintf " with %S" s
    )

let print_json_message (chan, msg) =
  let json = Slack_j.string_of_webhook_notification msg in
  log#info "will notify %s" chan;
  let url = Uri.of_string "https://api.slack.com/docs/messages/builder" in
  let url = Uri.add_query_param url ("msg", [ json ]) in
  log#info "%s" (Uri.to_string url);
  log#info "%s" json

let check_gh_action file json config secrets state =
  Lwt_main.run
    ( match json with
    | false -> check_common file print_simplified_message config secrets state
    | true -> check_common file print_json_message config secrets state
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
  Arg.(value & opt file "monorobot.json" & info [ "config" ] ~docv:"CONFIG" ~doc)

let secrets =
  let doc = "configuration file containing secrets" in
  Arg.(value & opt file "secrets.json" & info [ "secrets" ] ~docv:"SECRETS" ~doc)

let state =
  let doc = "state file" in
  Arg.(value & opt file "notabot_state.json" & info [ "state" ] ~docv:"STATE" ~doc)

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
