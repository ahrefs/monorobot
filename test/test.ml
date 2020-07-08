open Base
open Lib

let print_notif (chan, msg) =
  let json =
    msg |> Slack_j.string_of_webhook_notification |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string
  in
  Stdio.printf "will notify #%s\n" chan;
  Stdio.printf "%s\n" json

let load_initial_state state_dir file =
  let basename = Caml.Filename.basename file in
  let state_file = Caml.Filename.concat state_dir basename in
  State.load ~silent:true state_file

let process cfg state_dir file =
  Stdio.printf "===== file %s =====\n" file;
  match Mock.kind file with
  | None -> Lwt.return_unit
  | Some kind ->
    let headers = [ "x-github-event", kind ] in
    ( match Github.parse_exn ~secret:None headers (Stdio.In_channel.read_all file) with
    | exception exn ->
      Stdio.printf "exception when parsing %s: %s\n" file (Exn.to_string exn);
      Lwt.return_unit
    | event ->
      let state = ref (load_initial_state state_dir file) in
      let load_state () = !state in
      let update_state s e = state := State.update_state s e in
      let%lwt notifs = Action.generate_notifications cfg load_state update_state event in
      List.iter notifs ~f:print_notif;
      Lwt.return_unit
    )

let () =
  let cfg = Config.load "notabot.json" "secrets.json" in
  let mock_dir = "../mock_payloads" in
  let state_dir = "../mock_states" in
  let jsons = Caml.Sys.readdir mock_dir in
  let jsons = Array.map ~f:(fun p -> Caml.Filename.concat mock_dir p) jsons in
  Array.sort jsons ~compare:String.compare;
  Lwt_main.run
    (let jsons = Array.to_list jsons in
     Lwt_list.iter_s (process cfg state_dir) jsons)
