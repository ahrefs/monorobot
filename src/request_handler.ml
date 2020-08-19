open Printf
open Devkit

let log = Log.from "request_handler"

let process_github_notification (ctx : Lib.Context.t) headers body =
  let open Lib in
  let cfg = ctx.cfg in
  match Github.parse_exn ~secret:cfg.Config.gh_webhook_secret headers body with
  | exception exn -> Exn_lwt.fail ~exn "unable to parse payload"
  | payload ->
    let%lwt notifications = Action.generate_notifications ctx payload in
    Lwt_list.iter_s
      (fun (chan, msg) ->
        let url = Config.Chan_map.find chan cfg.chans in
        let data = Slack_j.string_of_webhook_notification msg in
        log#info "sending to %s : %s" chan data;
        Slack.send_notification url data)
      notifications

let setup_http ~ctx ~signature ~port ~ip =
  let open Httpev in
  let connection = Unix.ADDR_INET (ip, port) in
  let%lwt () =
    Httpev.setup_lwt { default with name = "notabot"; connection; access_log_enabled = false } (fun _http request ->
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
          log#info "body: %s" request.body;
          let%lwt () = process_github_notification ctx request.headers request.body in
          ret (Lwt.return "ok")
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
          ))
  in
  Lwt.return_unit

let start_http_server ~ctx ~addr ~port () =
  let ip = Unix.inet_addr_of_string addr in
  let signature = sprintf "listen %s:%d" (Unix.string_of_inet_addr ip) port in
  setup_http ~ctx ~signature ~port ~ip
