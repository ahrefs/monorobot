open Base
open Common
open Devkit
open Printf

let cwd = Caml.Sys.getcwd ()
let cache_dir = Caml.Filename.concat cwd "github-api-cache"

(** return the file with a function f applied unless the file is empty;
 empty file:this is needed to simulate 404 returns from github *)
let with_cache_file url f =
  match get_local_file url with
  | Error e ->
    let err_msg = sprintf "error while getting local file: %s\ncached for url: %s" e url in
    Stdio.print_endline err_msg;
    Lwt.return_error err_msg
  | Ok "" -> Lwt.return_error "empty file"
  | Ok file -> Lwt.return_ok (f file)

let clean_forward_slashes = String.substr_replace_all ~pattern:"/" ~with_:"_"

(** get a member of the repo cached API call providing
the member kind (pull, issue, commit, compare, etc),
_ref (pr number, issue number, commit sha, compare basehead, etc),
and its Github_j.<kind>_of_string function.
NB: please save the cache file in the same format *)
let get_repo_member_cache ~(repo : Github_t.repository) ~kind ~ref_ ~of_string =
  let file = clean_forward_slashes (sprintf "%s_%s_%s" repo.full_name kind ref_) in
  let url = Caml.Filename.concat cache_dir file in
  with_cache_file url of_string

module Github : Api.Github = struct
  let get_config ~(ctx : Context.t) ~repo:_ =
    let url = Caml.Filename.concat cwd ctx.config_filename in
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
  let lookup_user ~ctx:_ ~cfg:_ ~email:_ = Lwt.return @@ Error "undefined for local setup"
  let send_notification ~ctx:_ ~msg:_ = Lwt.return @@ Error "undefined for local setup"
  let send_chat_unfurl ~ctx:_ ~channel:_ ~ts:_ ~unfurls:_ () = Lwt.return @@ Error "undefined for local setup"
  let send_auth_test ~ctx:_ () = Lwt.return @@ Error "undefined for local setup"
end

(** Module for mocking test requests to slack--will output on Stdio *)
module Slack : Api.Slack = struct
  include Slack_base

  let lookup_user ~ctx:_ ~(cfg : Config_t.config) ~email =
    let email = List.Assoc.find cfg.user_mappings ~equal:String.equal email |> Option.value ~default:email in
    let mock_user =
      {
        Slack_t.id = sprintf "id[%s]" email;
        name = sprintf "name[%s]" email;
        real_name = sprintf "real_name[%s]" email;
      }
    in
    let mock_response = { Slack_t.user = mock_user } in
    Lwt.return @@ Ok mock_response

  let send_notification ~ctx:_ ~msg =
    let json = msg |> Slack_j.string_of_post_message_req |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string in
    Stdio.printf "will notify #%s\n" msg.channel;
    Stdio.printf "%s\n" json;
    Lwt.return @@ Ok ()

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
    Stdio.printf "will unfurl in #%s\n" channel;
    let unfurl_text = List.map (StringMap.to_list unfurls) ~f:(fun (_, unfurl) -> unfurl.text) in
    Stdio.printf "%s\n" (String.concat ~sep:"\n" (List.filter_opt unfurl_text));
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
    let json = List.map (StringMap.to_list unfurls) ~f:(fun (_, unfurl) -> Slack_j.string_of_unfurl unfurl) in
    let url = Uri.of_string "https://slack.com/api/chat.unfurl" in
    log#info "%s" (Uri.to_string url);
    log#info "%s" (String.concat ~sep:";\n" json);
    Lwt.return @@ Ok ()

  let send_auth_test ~ctx:_ () =
    Lwt.return
    @@ Ok ({ url = ""; team = ""; user = ""; team_id = ""; user_id = "test_slack_user" } : Slack_t.auth_test_res)
end
