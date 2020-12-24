open Base
open Common
open Devkit

let cwd = Caml.Sys.getcwd ()

let cache_dir = Caml.Filename.concat cwd "github-api-cache"

module Github : Api.Github = struct
  let get_config ~(ctx : Context.t) ~repo:_ =
    let url = Caml.Filename.concat cwd ctx.config_filename in
    match get_local_file url with
    | Error e -> Lwt.return @@ fmt_error "error while getting local file: %s\nfailed to get config %s" e url
    | Ok file -> Lwt.return @@ Ok (Config_j.config_of_string file)

  let get_api_commit ~ctx:_ ~repo:_ ~sha =
    let url = Caml.Filename.concat cache_dir sha in
    match get_local_file url with
    | Error e -> Lwt.return @@ fmt_error "error while getting local file: %s\nfailed to get api commit %s" e url
    | Ok file -> Lwt.return @@ Ok (Github_j.api_commit_of_string file)
end

module Slack : Api.Slack = struct
  let send_notification ~chan ~msg ~url:_ =
    let json =
      msg |> Slack_j.string_of_webhook_notification |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string
    in
    Stdio.printf "will notify #%s\n" chan;
    Stdio.printf "%s\n" json;
    Lwt.return @@ Ok ()
end

module Slack_simple : Api.Slack = struct
  let log = Log.from "slack"

  let send_notification ~chan ~msg ~url:_ =
    log#info "will notify %s%s" chan
      ( match msg.Slack_t.text with
      | None -> ""
      | Some s -> Printf.sprintf " with %S" s
      );
    Lwt.return @@ Ok ()
end

module Slack_json : Api.Slack = struct
  let log = Log.from "slack"

  let send_notification ~chan ~msg ~url:_ =
    let json = Slack_j.string_of_webhook_notification msg in
    log#info "will notify %s" chan;
    let url = Uri.of_string "https://api.slack.com/docs/messages/builder" in
    let url = Uri.add_query_param url ("msg", [ json ]) in
    log#info "%s" (Uri.to_string url);
    log#info "%s" json;
    Lwt.return @@ Ok ()
end
