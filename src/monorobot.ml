open Devkit
open Lib
open Cmdliner

let log = Log.from "monorobot"

(* entrypoints *)

let http_server_action addr port config secrets state logfile loglevel =
  Daemon.logfile := logfile;
  Option.may Log.set_loglevels loglevel;
  Log.reopen !Daemon.logfile;
  Signal.setup_lwt ();
  Daemon.install_signal_handlers ();
  Lwt_main.run
    (begin
       log#info "monorobot starting";
       let ctx = Context.make ~config_filename:config ~secrets_filepath:secrets ?state_filepath:state () in
       match Context.refresh_secrets ctx with
       | Error e ->
         log#error "failed to refresh secrets: %s" e;
         Lwt.return_unit
       | Ok ctx ->
       match Context.refresh_state ctx with
       | Error e ->
         log#error "failed to refresh state: %s" e;
         Lwt.return_unit
       | Ok ctx -> Request_handler.run ~ctx ~addr ~port
     end
       [%lwt.finally
         let open Database in
         match !available with
         | Not_available -> Lwt.return_unit
         | Available ->
           let%lwt () = Conn.Pool.shutdown (Option.get !pool) in
           log#info "database: closed connection pool successfully";
           Lwt.return_unit])

(** In check mode, instead of actually sending the message to slack, we simply print it in the console *)
let check_gh_action file json config secrets state =
  match Github.event_of_filename (Filename.basename file) with
  | None ->
    log#error "aborting because payload %s is not named properly, named should be KIND.NAME_OF_PAYLOAD.json" file
  | Some kind ->
  match Std.input_file file with
  | exception exn -> log#error ~exn "failed to read file %s" file
  | body ->
    let headers = [ "x-github-event", kind ] in
    let ctx = Context.make ~config_filename:config ~secrets_filepath:secrets ?state_filepath:state () in
    (match Context.refresh_secrets ctx with
    | Error e -> log#error "%s" e
    | Ok ctx ->
      Lwt_main.run
        (if json then
           let module Action = Action.Action (Api_remote.Github) (Api_local.Slack_json) (Api_remote.Buildkite) in
           let%lwt () = Action.refresh_username_to_slack_id_tbl ~ctx in
           Action.process_github_notification ctx headers body
         else
           let module Action = Action.Action (Api_remote.Github) (Api_local.Slack_simple) (Api_remote.Buildkite) in
           let%lwt () = Action.refresh_username_to_slack_id_tbl ~ctx in
           Action.process_github_notification ctx headers body))

let check_slack_action file secrets =
  let data = In_channel.(with_open_bin file input_all) in
  let ctx = Context.make ~secrets_filepath:secrets () in
  match Slack_j.post_message_req_of_string data with
  | exception exn -> log#error ~exn "unable to parse notification"
  | msg ->
  match Context.refresh_secrets ctx with
  | Error e -> log#error "%s" e
  | Ok ctx ->
    Lwt_main.run
      (match%lwt Api_remote.Slack.send_notification ~ctx ~msg with
      | Error e ->
        log#error "%s" e;
        Lwt.return_unit
      | Ok (Some res : Slack_t.post_message_res option) ->
        log#info "%s" (Slack_j.string_of_post_message_res res);
        Lwt.return_unit
      | Ok None -> Lwt.return_unit)

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

let logfile =
  let doc = "log file (output to stderr if absent)" in
  Arg.(value & opt (some string) None & info [ "logfile" ] ~docv:"LOGFILE" ~doc)

let loglevel =
  let doc = "log level, matching the following grammar: ([<facil|prefix*>=]debug|info|warn|error[,])+" in
  Arg.(value & opt (some string) None & info [ "loglevel" ] ~docv:"LOGLEVEL" ~doc)

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
  let term = Term.(const http_server_action $ addr $ port $ config $ secrets $ state $ logfile $ loglevel) in
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

let arg_value ~doc ~docv typ names = Arg.value Arg.(opt (some typ) None & info names ~docv ~doc)

let status_state_converter =
  let parse state = try Ok (Github_j.status_state_of_string state) with Failure s -> Error (`Msg s) in
  let print ppf p = Format.fprintf ppf "%s" (Github_j.string_of_status_state p) in
  Arg.conv (parse, print)

let db_path =
  arg_value
    ~doc:"path to the database file; if unspecified, the default `db/monorobot.db` path will be used"
    ~docv:"DB_PATH" Arg.string [ "db-path" ]
let pipeline = arg_value ~doc:"filter by pipeline name. required" ~docv:"PIPELINE_NAME" Arg.string [ "p"; "pipeline" ]
let branch = arg_value ~doc:"filter by branch name. required" ~docv:"BRANCH_NAME" Arg.string [ "b"; "branch" ]
let after = arg_value ~doc:"filter by build number" ~docv:"AFTER" Arg.int [ "after" ]
let state = arg_value ~doc:"filter by notification state" ~docv:"STATE" status_state_converter [ "state" ]
let build_number = arg_value ~doc:"filter by build number" ~docv:"BUILD_NUMBER" Arg.int [ "n"; "build-number" ]
let step_name =
  arg_value
    ~doc:"filter by step name. Needs to be in the same format as in the notification.context field"
    ~docv:"STEP_NAME" Arg.string [ "step" ]
let sha = arg_value ~doc:"filter by sha" ~docv:"SHA" Arg.string [ "sha" ]

let only_with_changes =
  let open Arg in
  let info = info [ "with-changes" ] ~doc:"Only replay notifications with state changes" in
  value (flag info)

let replay =
  let doc = "replay notifications from the debug database" in
  let info = Cmd.info "replay" ~doc in
  let term =
    Term.(
      const Debug_db.replay_action
      $ db_path
      $ pipeline
      $ branch
      $ only_with_changes
      $ after
      $ state
      $ build_number
      $ step_name
      $ sha)
  in
  Cmd.v info term

let debug_db =
  let doc = "debug database commands" in
  let info = Cmd.info "debug_db" ~doc in
  let cmds = [ replay ] in
  Cmd.group info cmds

let default, info =
  let doc = "the notification bot" in
  Term.(ret (const (`Help (`Pager, None)))), Cmd.info "monorobot" ~doc ~version:Version.current

let () =
  let cmds = [ run; check_gh; check_slack; debug_db ] in
  let group = Cmd.group ~default info cmds in
  exit @@ Cmd.eval group
