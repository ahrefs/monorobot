open Common
open Devkit
open Printf

let cwd = Sys.getcwd ()
let github_cache_dir = Filename.concat cwd "github-api-cache"
let slack_cache_dir = Filename.concat cwd "slack-api-cache"
let buildkite_cache_dir = Filename.concat cwd "buildkite-api-cache"

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

module Github : Api.Github = struct
  (** get a member of the repo cached API call providing
the member kind (pull, issue, commit, compare, etc),
_ref (pr number, issue number, commit sha, compare basehead, etc),
and its Github_j.<kind>_of_string function.
NB: please save the cache file in the same format *)
  let get_repo_member_cache ~(repo : Github_t.repository) ~kind ~ref_ ~of_string =
    let file = clean_forward_slashes (sprintf "%s_%s_%s" repo.full_name kind ref_) in
    let url = Filename.concat github_cache_dir file in
    with_cache_file url of_string

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
  let get_thread_permalink ~ctx:_ (_thread : State_t.slack_thread) = Lwt.return_none

  let send_file ~ctx:_ ~file:_ = Lwt.return (Error "undefined for local setup")
end

let string_of_opt = function
  | None -> "null"
  | Some s -> sprintf "%S" s

let string_of_file file =
  let { Slack.name; alt_txt; content; title } = file in

  sprintf
    {|    {
      name: "%s",
      alt_txt: %s,
      title: %s,
      content: %S
    }|}
    name (string_of_opt alt_txt) (string_of_opt title) content

let string_of_file_req (file : Slack.file_req) =
  let { Slack.files; channel_id; initial_comment; thread_ts } = file in

  sprintf {|{
  channel_id: %s,
  initial_comment: %s,
  thread_ts: %s,
  files: [
%s
  ]
}|}
    (channel_id |> Option.map Slack_channel.Ident.project |> string_of_opt)
    (string_of_opt initial_comment)
    (thread_ts |> Option.map Slack_timestamp.project |> string_of_opt)
    (files |> List.map string_of_file |> String.concat ",\n")

(** Module for mocking test requests to slack--will output on Stdio *)

(** Simple messages (only the actual text messages that users see) output to log for checking payload commands *)
module Slack_simple : Api.Slack = struct
  include Slack_base

  let log = Log.from "slack"

  let send_notification ~ctx:_ ~(msg : Slack_t.post_message_req) =
    log#info "will notify %s%s" (Slack_channel.Any.project msg.channel)
      (match msg.Slack_t.text with
      | None -> ""
      | Some s -> sprintf " with %S" s);
    Lwt.return @@ Ok None

  let send_file ~ctx:_ ~(file : Slack.file_req) =
    let json = string_of_file_req file in
    Printf.printf "will upload file\n";
    Printf.printf "%s\n" json;
    Lwt.return_ok ()

  let send_chat_unfurl ~ctx:_ ~channel ~ts:_ ~(unfurls : Slack_t.message_attachment Common.StringMap.t) () =
    Printf.printf "will unfurl in #%s\n" (Slack_channel.Ident.project channel);
    let unfurl_text =
      List.map (fun ((_, unfurl) : string * Slack_t.message_attachment) -> unfurl.text) (StringMap.to_list unfurls)
    in
    Printf.printf "%s\n" (String.concat "\n" (List.filter_map id unfurl_text));
    Lwt.return @@ Ok ()

  let send_auth_test ~ctx:_ () =
    Lwt.return
    @@ Ok
         ({ url = ""; team = ""; user = ""; team_id = ""; user_id = Slack_user_id.inject "test_slack_user" }
           : Slack_t.auth_test_res)

  let get_thread_permalink ~ctx:_ (_thread : State_t.slack_thread) = Lwt.return_none
end

(** Messages payload in json output to log for checking payload commands *)
module Slack_json : Api.Slack = struct
  include Slack_base

  let log = Log.from "slack"

  let send_notification ~ctx:_ ~(msg : Slack_t.post_message_req) =
    log#info "will notify %s" (Slack_channel.Any.project msg.channel);
    let json = Slack_j.string_of_post_message_req msg in
    let url = Uri.of_string "https://api.slack.com/docs/messages/builder" in
    let url = Uri.add_query_param url ("msg", [ json ]) in
    log#info "%s" (Uri.to_string url);
    log#info "%s" json;
    let channel = Slack_channel.Ident.inject (Slack_channel.Any.project msg.channel) in
    let res = ({ Slack_t.channel; ts = Slack_timestamp.inject "mock_ts" } : Slack_t.post_message_res) in
    Lwt.return_ok (Some res)

  let send_file ~ctx:_ ~(file : Slack.file_req) =
    let json = string_of_file_req file in
    Printf.printf "will upload file\n";
    Printf.printf "%s\n" json;
    Lwt.return @@ Ok ()

  let send_chat_unfurl ~ctx:_ ~channel ~ts:_ ~(unfurls : Slack_t.message_attachment Common.StringMap.t) () =
    log#info "will notify %s" (Slack_channel.Ident.project channel);
    let json = List.map (fun (_, unfurl) -> Slack_j.string_of_unfurl unfurl) (StringMap.to_list unfurls) in
    let url = Uri.of_string "https://slack.com/api/chat.unfurl" in
    log#info "%s" (Uri.to_string url);
    log#info "%s" (String.concat ";\n" json);
    Lwt.return_ok ()

  let send_auth_test ~ctx:_ () =
    Lwt.return_ok
      ({ url = ""; team = ""; user = ""; team_id = ""; user_id = Slack_user_id.inject "test_slack_user" }
        : Slack_t.auth_test_res)

  let get_thread_permalink ~ctx:_ (_thread : State_t.slack_thread) = Lwt.return_none
end

module Slack : Api.Slack = struct
  include Slack_base

  let lookup_user ?cache:_ ~ctx:_ ~(cfg : Config_t.config) ~email () =
    let email = List.assoc_opt email cfg.user_mappings |> Option.default email in
    let mock_user = { Slack_t.id = Slack_user_id.inject (sprintf "id[%s]" email); profile = { email = Some email } } in
    let mock_response = { Slack_t.user = mock_user } in
    Lwt.return @@ Ok mock_response

  let list_users ?cursor:_ ?limit:_ ~ctx:_ () =
    let url = Filename.concat slack_cache_dir "users-list" in
    with_cache_file url Slack_j.list_users_res_of_string

  let send_notification ~ctx:_ ~msg =
    let json = msg |> Slack_j.string_of_post_message_req |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string in
    Printf.printf "will notify #%s\n" (Slack_channel.Any.project msg.channel);
    Printf.printf "%s\n" json;
    let channel = Slack_channel.Ident.inject (Slack_channel.Any.project msg.channel) in
    let res = ({ Slack_t.channel; ts = Slack_timestamp.inject "mock_ts" } : Slack_t.post_message_res) in
    Lwt.return @@ Ok (Some res)

  let send_file ~ctx:_ ~(file : Slack.file_req) =
    let json = string_of_file_req file in
    Printf.printf "will upload file\n";
    Printf.printf "%s\n" json;
    Lwt.return @@ Ok ()

  let send_chat_unfurl ~ctx:_ ~channel ~ts ~unfurls () =
    let req = Slack_j.{ channel; ts; unfurls } in
    let data = req |> Slack_j.string_of_chat_unfurl_req |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string in
    Printf.printf "will unfurl in #%s\n" (Slack_channel.Ident.project channel);
    Printf.printf "%s\n" data;
    Lwt.return @@ Ok ()

  let send_auth_test ~ctx:_ () =
    Lwt.return
    @@ Ok
         ({ url = ""; team = ""; user = ""; team_id = ""; user_id = Slack_user_id.inject "test_slack_user" }
           : Slack_t.auth_test_res)

  let get_thread_permalink ~ctx:_ (thread : State_t.slack_thread) =
    Lwt.return_some
    @@ Printf.sprintf "https://monorobot.slack.com/archives/%s/p%s?thread_ts=%s&cid=%s"
         (Slack_channel.Ident.project thread.cid)
         (Stre.replace_all ~str:(Slack_timestamp.project thread.ts) ~sub:"." ~by:"")
         (Slack_timestamp.project thread.ts)
         (Slack_channel.Ident.project thread.cid)
end

module Buildkite : Api.Buildkite = struct
  let get_job_log ~ctx:_ (_ : Github_t.status_notification) (job : Buildkite_t.job) =
    match job.log_url with
    | None -> Lwt.return_error "Unable to get job log, job has no log_url field"
    | Some log_url ->
    match Re2.find_submatches_exn Util.Build.buildkite_api_org_pipeline_build_job_re log_url with
    | exception exn -> Exn.fail ~exn "failed to parse buildkite build url %s" log_url
    | [| Some _; Some org; Some pipeline; Some build_nr; Some job_nbr |] ->
      let file =
        clean_forward_slashes
          (sprintf "organizations/%s/pipelines/%s/builds/%s/jobs/%s/logs" org pipeline build_nr job_nbr)
      in
      let url = Filename.concat buildkite_cache_dir file in
      with_cache_file url Buildkite_j.job_log_of_string
    | _ -> failwith "failed to get all job log details from the job."

  [@@@warning "-27"]
  let get_build ?(cache : [ `Use | `Refresh ] option) ~ctx build_url =
    let org, pipeline, build_nr = Util.Build.get_org_pipeline_build' build_url in
    let file = clean_forward_slashes (sprintf "organizations/%s/pipelines/%s/builds/%s" org pipeline build_nr) in
    let url = Filename.concat buildkite_cache_dir file in
    with_cache_file url Buildkite_j.get_build_res_of_string

  let get_build_branch ~ctx (n : Github_t.status_notification) =
    let* build_url = Lwt.return @@ Util.Build.get_build_url n in
    get_build ~ctx build_url |> Lwt_result.map (fun { Buildkite_t.branch; _ } : Github_t.branch -> { name = branch })
end
