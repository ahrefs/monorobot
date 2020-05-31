open Base
open Lib

let print_notif (webhook, msg) =
  let json =
    msg |> Slack_j.string_of_webhook_notification |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string
  in
  Stdio.printf "will notify #%s\n" webhook.Notabot_t.channel;
  Stdio.printf "%s\n" json

let process cfg file =
  Stdio.printf "===== file %s =====\n" file;
  match Mock.kind file with
  | None -> ()
  | Some kind ->
    let headers = [ "x-github-event", kind ] in
    ( match Github.parse_exn ~secret:None headers (Stdio.In_channel.read_all file) with
    | exception exn ->
      Stdio.printf "exception when parsing %s: %s\n" file (Exn.to_string exn);
      ()
    | event ->
      let notifs = Action.generate_notifications cfg event in
      List.iter notifs ~f:print_notif
    )

let () =
  let mock_dir = "../mock_payloads" in
  let jsons = Caml.Sys.readdir mock_dir in
  let jsons = Array.map ~f:(fun p -> Caml.Filename.concat mock_dir p) jsons in
  Array.sort jsons ~compare:String.compare;
  let cfg = Notabot_j.config_of_string @@ Stdio.In_channel.read_all "notabot.json" in
  Array.iter ~f:(process cfg) jsons
