open Devkit
open Base
open Lib
module Arg = Caml.Arg

let log = Log.from "notabot"

let action_after_cfg_refresh (cfg : Config.t) =
  log#info "using prefix routing:";
  Action.print_prefix_routing cfg.prefix_rules.rules;
  log#info "using label routing:";
  Action.print_label_routing cfg.label_rules.rules;
  log#info "signature checking %s" (if Option.is_some cfg.gh_webhook_secret then "enabled" else "disabled")

let update_state_at_path state_path state event = State.save state_path @@ State.update_state state event

let http_server addr port config secrets state_path =
  log#info "notabot starting";
  let ctx = Context.make ~state_path ~cfg_path:config ~secrets_path:secrets ~action_after_cfg_refresh () in
  Lwt_main.run (Request_handler.start_http_server ~ctx ~addr ~port ())

let send_slack_notification webhook file =
  let data = Stdio.In_channel.read_all file in
  match Slack_j.webhook_notification_of_string data with
  | exception exn -> log#error ~exn "unable to parse notification"
  | data -> Lwt_main.run (Slack.send_notification webhook data)

let check_common file print config secrets state_path =
  let ctx = Context.make ~state_path ~cfg_path:config ~secrets_path:secrets ~action_after_cfg_refresh () in
  match Mock.kind file with
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
      Context.refresh_config ctx ~req:event ();
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

let check file json config secrets state =
  Lwt_main.run
    ( match json with
    | false -> check_common file print_simplified_message config secrets state
    | true -> check_common file print_json_message config secrets state
    )

(** {2 cli} *)

open Cmdliner

let addr =
  let doc = "http listen addr" in
  Arg.(value & opt string "127.0.0.1" & info [ "a"; "addr" ] ~docv:"ADDR" ~doc)

let port =
  let doc = "http listen port" in
  Arg.(value & opt int 8080 & info [ "p"; "port" ] ~docv:"PORT" ~doc)

let config =
  let doc = "configuration file" in
  Arg.(value & opt file "notabot.json" & info [ "config" ] ~docv:"CONFIG" ~doc)

let secrets =
  let doc = "configuration file containing secrets" in
  Arg.(value & opt file "secrets.json" & info [ "secrets" ] ~docv:"secrets" ~doc)

let state =
  let doc = "state file" in
  Arg.(value & opt file "notabot_state.json" & info [ "state" ] ~docv:"STATE" ~doc)

let mock_payload =
  let doc = "mock github webhook payload" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"MOCK_PAYLOAD" ~doc)

let slack_webhook_url =
  let doc = "slack webhook url" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"SLACK_WEBHOOK" ~doc)

let slack_notif =
  let doc = "file containing a slack notification as a json" in
  Arg.(required & pos 1 (some file) None & info [] ~docv:"SLACK_NOTIF" ~doc)

let json =
  let doc = "display output as json" in
  Arg.(value & flag & info [ "j"; "json" ] ~docv:"JSON" ~doc)

let run =
  let doc = "launch the http server" in
  let info = Term.info "run" ~doc in
  let term = Term.(const http_server $ addr $ port $ config $ secrets $ state) in
  term, info

let check =
  let doc = "read github notification payload from file and show actions to be taken" in
  let info = Term.info "check" ~doc in
  let term = Term.(const check $ mock_payload $ json $ config $ secrets $ state) in
  term, info

let slack_notif =
  let doc = "read github notification payload from file and show actions to be taken" in
  let info = Term.info "send-slack-notif" ~doc in
  let term = Term.(const send_slack_notification $ slack_webhook_url $ slack_notif) in
  term, info

let default_cmd =
  let doc = "the notification bot" in
  Term.(ret (const (`Help (`Pager, None)))), Term.info "notabot" ~doc

let cmds = [ run; check; slack_notif ]

let () = Term.(exit @@ eval_choice default_cmd cmds)
