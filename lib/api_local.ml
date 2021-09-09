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

  let get_pull_request ~ctx:_ ~repo:_ ~number:_ = Lwt.return @@ Error "undefined for local setup"

  let get_issue ~ctx:_ ~repo:_ ~number:_ = Lwt.return @@ Error "undefined for local setup"
end

module Slack_base : Api.Slack = struct
  let send_notification ~ctx:_ ~msg:_ = Lwt.return @@ Error "undefined for local setup"

  let send_chat_unfurl ~ctx:_ _ = Lwt.return @@ Error "undefined for local setup"

  let send_auth_test ~ctx:_ () = Lwt.return @@ Error "undefined for local setup"
end

module Slack : Api.Slack = struct
  include Slack_base

  let send_notification ~ctx:_ ~msg =
    let json = msg |> Slack_j.string_of_post_message_req |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string in
    Stdio.printf "will notify #%s\n" msg.channel;
    Stdio.printf "%s\n" json;
    Lwt.return @@ Ok ()
end

module Slack_simple : Api.Slack = struct
  include Slack_base

  let log = Log.from "slack"

  let send_notification ~ctx:_ ~(msg : Slack_t.post_message_req) =
    log#info "will notify %s%s" msg.channel
      ( match msg.Slack_t.text with
      | None -> ""
      | Some s -> Printf.sprintf " with %S" s
      );
    Lwt.return @@ Ok ()
end

module Slack_json : Api.Slack = struct
  include Slack_base

  let log = Log.from "slack"

  let send_notification ~ctx:_ ~(msg : Slack_t.post_message_req) =
    log#info "will notify %s" msg.channel;
    let json = Slack_j.string_of_post_message_req msg in
    let url = Uri.of_string "https://api.slack.com/docs/messages/builder" in
    let url = Uri.add_query_param url ("msg", [ json ]) in
    log#info "%s" (Uri.to_string url);
    log#info "%s" json;
    Lwt.return @@ Ok ()
end
