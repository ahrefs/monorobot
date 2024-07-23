open Printf
open Devkit
open Lib

let log = Log.from "request_handler"

module Action = Action.Action (Api_remote.Github) (Api_remote.Slack)

let run ~ctx ~addr ~port =
  let open Httpev in
  let ip = Unix.inet_addr_of_string addr in
  let signature = sprintf "listen %s:%d" (Unix.string_of_inet_addr ip) port in
  let connection = Unix.ADDR_INET (ip, port) in
  let request_handler_lwt =
    Httpev.setup_lwt { default with name = "monorobot"; connection; access_log_enabled = false } (fun _http request ->
        let module Arg = Args (struct
          let req = request
        end) in
        let body r = Lwt.return (`Body r) in
        let ret ?(status = `Ok) ?(typ = "text/plain") ?extra r = body @@ serve ~status ?extra request typ r in
        let ret_err status s = body @@ serve_text ~status request s in
        try%lwt
          let path =
            match String.split_on_char '/' request.path with
            | "" :: p -> p
            | _ -> Exn.fail "you are on a wrong path"
          in
          match request.meth, List.map Web.urldecode path with
          | _, [ "stats" ] -> ret (sprintf "%s %s uptime\n" signature Devkit.Action.uptime#get_str)
          | _, [ "ping" ] -> ret ""
          | `GET, [ "config" ] ->
            let repo_url = Arg.str "repo" |> Web.urldecode in
            (match%lwt Action.print_config ctx repo_url with
            | Error (code, msg) -> ret_err code msg
            | Ok res -> ret ~typ:"application/json" res)
          | _, [ "github" ] | _, [ "external"; "github" ] ->
            log#debug "github event: %s" request.body;
            let%lwt () = Action.process_github_notification ctx request.headers request.body in
            ret "ok"
          | _, [ "slack"; "events" ] | _, [ "external"; "slack"; "events" ] ->
            log#debug "slack event: %s" request.body;
            let%lwt res = Action.process_slack_event ctx request.headers request.body in
            ret res
          | _, _ ->
            log#error "unknown path : %s" (Httpev.show_request request);
            ret_err `Not_found "not found"
        with
        | Arg.Bad s ->
          log#error "bad parameter %S : %s" s (Httpev.show_request request);
          ret_err `Not_found (sprintf "bad parameter %s" s)
        | exn ->
          log#error ~exn "internal error : %s" (Httpev.show_request request);
          ret_err `Internal_server_error
            (match exn with
            | Failure s -> s
            | Invalid_argument s -> s
            | exn -> Exn.str exn))
  in
  let refresh_username_to_slack_id_tbl_background_lwt =
    try%lwt Daemon.unless_exit @@ Action.refresh_username_to_slack_id_tbl_background_lwt ~ctx
    with Daemon.ShouldExit -> Lwt.return_unit
  in
  let%lwt () = Lwt.join [ refresh_username_to_slack_id_tbl_background_lwt; request_handler_lwt ] in
  Lwt.return_unit
