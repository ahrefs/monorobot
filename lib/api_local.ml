open Common
open Devkit
open Printf
module Filename = Stdlib.Filename
module Sys = Stdlib.Sys

let cwd = Sys.getcwd ()
let github_cache_dir = Filename.concat cwd "github-api-cache"
let slack_cache_dir = Filename.concat cwd "slack-api-cache"

(** return the file with a function f applied unless the file is empty;
 empty file:this is needed to simulate 404 returns from github *)
let with_cache_file cache_filepath f =
  match Std.input_file cache_filepath with
  | "" -> Lwt.return_error "empty file"
  | file -> Lwt.return_ok (f file)
  | exception exn -> Exn.fail ~exn "failed to get local cache file : %s" cache_filepath

let rec clean_forward_slashes str =
  let cont, ns = ExtLib.String.replace ~str ~sub:"/" ~by:"_" in
  if cont then clean_forward_slashes ns else ns

(** get a member of the repo cached API call providing
the member kind (pull, issue, commit, compare, etc),
_ref (pr number, issue number, commit sha, compare basehead, etc),
and its Github_j.<kind>_of_string function.
NB: please save the cache file in the same format *)
let get_repo_member_cache ~(repo : Github_t.repository) ~kind ~ref_ ~of_string =
  let file = clean_forward_slashes (sprintf "%s_%s_%s" repo.full_name kind ref_) in
  let url = Filename.concat github_cache_dir file in
  with_cache_file url of_string

module Github : Api.Github = struct
  let get_config ~(ctx : Context.t) ~repo:_ =
    let url = Filename.concat cwd ctx.config_filename in
    with_cache_file url Config_j.config_of_string

  let get_api_commit ~ctx:_ ~repo ~sha =
    get_repo_member_cache ~repo ~kind:"commit" ~ref_:sha ~of_string:Github_j.api_commit_of_string

  let get_pull_request ~ctx:_ ~(repo : Github_t.repository) ~number =
    get_repo_member_cache ~repo ~kind:"pull" ~ref_:(Int.to_string number) ~of_string:Github_j.pull_request_of_string

  let get_issue ~ctx:_ ~(repo : Github_t.repository) ~number =
    get_repo_member_cache ~repo ~kind:"issue" ~ref_:(Int.to_string number) ~of_string:Github_j.issue_of_string

  let get_compare ~ctx:_ ~(repo : Github_t.repository) ~basehead:(base, merge) =
    get_repo_member_cache ~repo ~kind:"compare" ~ref_:(sprintf "%s...%s" base merge)
      ~of_string:Github_j.compare_of_string

  let request_reviewers ~ctx:_ ~repo:_ ~number:_ ~reviewers:_ = Lwt.return @@ Error "undefined for local setup"
end

(** The base implementation for local check payload debugging and mocking tests *)
module Slack_base : Api.Slack = struct
  let lookup_user ?cache:_ ~ctx:_ ~cfg:_ ~email:_ () = Lwt.return @@ Error "undefined for local setup"
  let list_users ?cursor:_ ?limit:_ ~ctx:_ () = Lwt.return @@ Error "undefined for local setup"
  let send_notification ~ctx:_ ~msg:_ = Lwt.return @@ Error "undefined for local setup"
  let send_chat_unfurl ~ctx:_ ~channel:_ ~ts:_ ~unfurls:_ () = Lwt.return @@ Error "undefined for local setup"
  let send_auth_test ~ctx:_ () = Lwt.return @@ Error "undefined for local setup"
end

(** Module for mocking test requests to slack--will output on Stdio *)
module Slack : Api.Slack = struct
  include Slack_base

  let lookup_user ?cache:_ ~ctx:_ ~(cfg : Config_t.config) ~email () =
    let email = List.assoc_opt email cfg.user_mappings |> Option.default email in
    let mock_user = { Slack_t.id = sprintf "id[%s]" email; profile = { email = Some email } } in
    let mock_response = { Slack_t.user = mock_user } in
    Lwt.return @@ Ok mock_response

  let list_users ?cursor:_ ?limit:_ ~ctx:_ () =
    let url = Filename.concat slack_cache_dir "users-list" in
    with_cache_file url Slack_j.list_users_res_of_string

  let send_notification ~ctx:_ ~msg =
    let json = msg |> Slack_j.string_of_post_message_req |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string in
    Printf.printf "will notify #%s\n" msg.channel;
    Printf.printf "%s\n" json;
    Lwt.return @@ Ok ()

  let send_chat_unfurl ~ctx:_ ~channel ~ts ~unfurls () =
    let req = Slack_j.{ channel; ts; unfurls } in
    let data = req |> Slack_j.string_of_chat_unfurl_req |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string in
    Printf.printf "will unfurl in #%s\n" channel;
    Printf.printf "%s\n" data;
    Lwt.return @@ Ok ()

  let send_auth_test ~ctx:_ () =
    Lwt.return
    @@ Ok ({ url = ""; team = ""; user = ""; team_id = ""; user_id = "test_slack_user" } : Slack_t.auth_test_res)
end

(** Simple messages (only the actual text messages that users see) output to log for checking payload commands *)
module Slack_simple : Api.Slack = struct
  include Slack_base

  let log = Log.from "slack"

  let send_notification ~ctx:_ ~(msg : Slack_t.post_message_req) =
    log#info "will notify %s%s" msg.channel
      ( match msg.Slack_t.text with
      | None -> ""
      | Some s -> sprintf " with %S" s
      );
    Lwt.return @@ Ok ()

  let send_chat_unfurl ~ctx:_ ~channel ~ts:_ ~(unfurls : Slack_t.message_attachment Common.StringMap.t) () =
    Printf.printf "will unfurl in #%s\n" channel;
    let unfurl_text =
      List.map (fun ((_, unfurl) : string * Slack_t.message_attachment) -> unfurl.text) (StringMap.to_list unfurls)
    in
    Printf.printf "%s\n" (String.concat "\n" (List.filter_map id unfurl_text));
    Lwt.return @@ Ok ()

  let send_auth_test ~ctx:_ () =
    Lwt.return
    @@ Ok ({ url = ""; team = ""; user = ""; team_id = ""; user_id = "test_slack_user" } : Slack_t.auth_test_res)
end

(** Messages payload in json output to log for checking payload commands *)
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

  let send_chat_unfurl ~ctx:_ ~channel ~ts:_ ~(unfurls : Slack_t.message_attachment Common.StringMap.t) () =
    log#info "will notify %s" channel;
    let json = List.map (fun (_, unfurl) -> Slack_j.string_of_unfurl unfurl) (StringMap.to_list unfurls) in
    let url = Uri.of_string "https://slack.com/api/chat.unfurl" in
    log#info "%s" (Uri.to_string url);
    log#info "%s" (String.concat ";\n" json);
    Lwt.return @@ Ok ()

  let send_auth_test ~ctx:_ () =
    Lwt.return
    @@ Ok ({ url = ""; team = ""; user = ""; team_id = ""; user_id = "test_slack_user" } : Slack_t.auth_test_res)
end
