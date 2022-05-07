open Printf
open Devkit
open Lib

let log = Log.from "request_handler"

module Action = Action.Action (Api_remote.Github) (Api_remote.Slack)

let setup_http ~ctx ~signature ~port ~ip =
  let open Httpev in
  let connection = Unix.ADDR_INET (ip, port) in
  let%lwt () =
    Httpev.setup_lwt { default with name = "monorobot"; connection; access_log_enabled = false } (fun _http request ->
      let module Arg = Args (struct let req = request end) in
      let body r = Lwt.return (`Body r) in
      let ret ?(status = `Ok) ?(typ = "text/plain") ?extra r =
        let%lwt r = r in
        body @@ serve ~status ?extra request typ r
      in
      let _ret' ?extra r =
        let%lwt typ, r = r in
        body @@ serve ~status:`Ok ?extra request typ r
      in
      let _ret'' ?extra r =
        let%lwt status, typ, r = r in
        body @@ serve ~status ?extra request typ r
      in
      let ret_err status s = body @@ serve_text ~status request s in
      try%lwt
        let path =
          match Stre.nsplitc request.path '/' with
          | "" :: p -> p
          | _ -> Exn.fail "you are on a wrong path"
        in
        match request.meth, List.map Web.urldecode path with
        | _, [ "stats" ] -> ret @@ Lwt.return (sprintf "%s %s uptime\n" signature Devkit.Action.uptime#get_str)
        | _, [ "github" ] ->
          log#info "%s" request.body;
          let%lwt () = Action.process_github_notification ctx request.headers request.body in
          ret (Lwt.return "ok")
        | _, [ "slack"; "events" ] ->
          log#info "%s" request.body;
          ret @@ Action.process_slack_event ctx request.headers request.body
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
          ( match exn with
          | Failure s -> s
          | Invalid_argument s -> s
          | exn -> Exn.str exn
          )
    )
  in
  Lwt.return_unit

let users_refresh_interval = Time.hours 4

let refresh_users ctx =
  let rec loop ctx =
    let%lwt () =
      match%lwt Api_remote.Slack.get_users_list ~ctx () with
      | Ok users ->
        Common.Stringtbl.clear ctx.users;
        users.members
        |> List.filter_map Context.user_of_slack_user
        |> List.iter (fun (email, user) -> Common.Stringtbl.add_exn ctx.users ~key:email ~data:user)
        |> Lwt.return
      | Error e ->
        log#error "refresh_users: %s" e;
        Lwt.return_unit
    in
    let%lwt () = Lwt_unix.sleep users_refresh_interval in
    loop ctx
  in
  match Context.slack_auth_mode_exn ctx with
  | Context.Webhook ->
    log#warn "slack: not using token auth, user DM feature will be disabled";
    Lwt.return_unit
  | Token -> loop ctx

let run ~ctx ~addr ~port =
  let ip = Unix.inet_addr_of_string addr in
  let signature = sprintf "listen %s:%d" (Unix.string_of_inet_addr ip) port in
  Lwt.join [ setup_http ~ctx ~signature ~port ~ip; refresh_users ctx ]
