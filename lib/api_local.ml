open Base
open Common
open Devkit
open Printf

let cwd = Caml.Sys.getcwd ()
let cache_dir = Caml.Filename.concat cwd "github-api-cache"

(** return the file with a function f applied *)
let get_cache_file_f url f =
  match get_local_file url with
  | Error e ->
    let err_msg = sprintf "error while getting local file: %s\ncached for url: %s" e url in
    Stdio.print_endline err_msg;
    Lwt.return @@ Error err_msg
  | Ok file -> Lwt.return @@ Ok (f file)

module Github : Api.Github = struct
  let get_config ~(ctx : Context.t) ~repo:_ =
    let url = Caml.Filename.concat cwd ctx.config_filename in
    get_cache_file_f url Config_j.config_of_string

  let get_branch ~ctx:_ ~(repo : Github_t.repository) ~name =
    let repo_branch = sprintf "%s_branch_%s" repo.full_name name in
    let clean_repo_branch = String.substr_replace_all ~pattern:"/" ~with_:"_" repo_branch in
    let url = Caml.Filename.concat cache_dir clean_repo_branch in
    get_cache_file_f url Github_j.branch_of_string

  let get_api_commit ~ctx:_ ~repo:_ ~sha =
    let url = Caml.Filename.concat cache_dir sha in
    get_cache_file_f url Github_j.api_commit_of_string

  let get_pull_request ~ctx:_ ~(repo : Github_t.repository) ~number =
    let url = Caml.Filename.concat cache_dir (sprintf "%s_pull_%d" repo.name number) in
    get_cache_file_f url Github_j.pull_request_of_string

  let get_issue ~ctx:_ ~(repo : Github_t.repository) ~number =
    let url = Caml.Filename.concat cache_dir (sprintf "%s_issue_%d" repo.name number) in
    get_cache_file_f url Github_j.issue_of_string

  let get_compare ~ctx:_ ~(repo : Github_t.repository) ~basehead =
    let clean_basehead = String.substr_replace_all ~pattern:"/" ~with_:"_" basehead in
    let url = Caml.Filename.concat cache_dir (sprintf "%s_compare_%s" repo.name clean_basehead) in
    get_cache_file_f url Github_j.compare_of_string

  let request_reviewers ~ctx:_ ~repo:_ ~number:_ ~reviewers:_ = Lwt.return @@ Error "undefined for local setup"
end

module Slack_base : Api.Slack = struct
  let send_notification ~ctx:_ ~msg:_ = Lwt.return @@ Error "undefined for local setup"

  let send_chat_unfurl ~ctx:_ ~channel ~ts ~unfurls () =
    let req = Slack_j.{ channel; ts; unfurls } in
    let data = req |> Slack_j.string_of_chat_unfurl_req |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string in
    Stdio.printf "will unfurl in #%s\n" channel;
    Stdio.printf "%s\n" data;
    Lwt.return @@ Ok ()

  let send_auth_test ~ctx:_ () =
    Lwt.return
    @@ Ok ({ url = ""; team = ""; user = ""; team_id = ""; user_id = "test_slack_user" } : Slack_t.auth_test_res)
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
