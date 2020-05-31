open Devkit
open Base
open Lib
module Arg = Caml.Arg

let log = Log.from "notabot"

let get_config () =
  let cfg = Notabot_j.config_of_string @@ Stdio.In_channel.read_all "notabot.json" in
  log#info "using push routing:";
  Action.print_push_routing cfg.push_rules;
  log#info "using pull request/issue routing:";
  Action.print_label_routing cfg.label_rules.rules;
  cfg

let http_server addr port =
  log#info "notabot starting";
  let cfg = get_config () in
  log#info "signature checking %s" (if Option.is_some cfg.gh_webhook_secret then "enabled" else "disabled");
  Lwt_main.run (Request_handler.start_http_server ~cfg ~addr ~port ())

let check_common file print =
  let cfg = get_config () in
  match Mock.kind file with
  | None ->
    log#error "aborting because payload %s is not named properly, named should be KIND.NAME_OF_PAYLOAD.json" file
  | Some kind ->
    let headers = [ "x-github-event", kind ] in
    (* read the event from a file and try to parse it *)
    ( match Github.parse_exn ~secret:None headers (Stdio.In_channel.read_all file) with
    | exception exn -> log#error ~exn "unable to parse payload"
    | event -> Action.generate_notifications cfg event |> List.iter ~f:print
    )

let print_simplified_message (webhook, msg) =
  (* In check mode, instead of actually sending the message to slack, we
     simply print it in the console *)
  log#info "will notify %s%s" webhook.Notabot_t.channel
    ( match msg.Slack_t.text with
    | None -> ""
    | Some s -> Printf.sprintf " with %S" s
    )

let print_json_message (webhook, msg) =
  let json = Slack_j.string_of_webhook_notification msg in
  log#info "will notify %s" webhook.Notabot_t.channel;
  let url = Uri.of_string "https://api.slack.com/docs/messages/builder" in
  let url = Uri.add_query_param url ("msg", [ json ]) in
  log#info "%s" (Uri.to_string url);
  log#info "%s" json

let check file json =
  match json with
  | false -> check_common file print_simplified_message
  | true -> check_common file print_json_message

(** {2 cli} *)

open Cmdliner

let addr =
  let doc = "http listen addr" in
  Arg.(value & opt string "127.0.0.1" & info [ "a"; "addr" ] ~docv:"ADDR" ~doc)

let port =
  let doc = "http listen port" in
  Arg.(value & opt int 8080 & info [ "p"; "port" ] ~docv:"PORT" ~doc)

let mock_payload =
  let doc = "mock github webhook payload" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"MOCK_PAYLOAD" ~doc)

let json =
  let doc = "display output as json" in
  Arg.(value & flag & info [ "j"; "json" ] ~docv:"JSON" ~doc)

let run =
  let doc = "launch the http server" in
  let info = Term.info "run" ~doc in
  let term = Term.(const http_server $ addr $ port) in
  term, info

let check =
  let doc = "read github notification payload from file and show actions to be taken" in
  let info = Term.info "check" ~doc in
  let term = Term.(const check $ mock_payload $ json) in
  term, info

let default_cmd =
  let doc = "the notification bot" in
  Term.(ret (const (`Help (`Pager, None)))), Term.info "notabot" ~doc

let cmds = [ run; check ]

let () = Term.(exit @@ eval_choice default_cmd cmds)
