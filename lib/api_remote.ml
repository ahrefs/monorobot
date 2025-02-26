open Printf
open Devkit
open Common
open Util

module Github : Api.Github = struct
  let commits_url ~(repo : Github_t.repository) ~sha =
    let _, url = ExtLib.String.replace ~sub:"{/sha}" ~by:("/" ^ sha) ~str:repo.commits_url in
    url

  let contents_url ~(repo : Github_t.repository) ~path =
    let _, url = ExtLib.String.replace ~sub:"{+path}" ~by:path ~str:repo.contents_url in
    url

  let pulls_url ~(repo : Github_t.repository) ~number =
    let _, url = ExtLib.String.replace ~sub:"{/number}" ~by:(sprintf "/%d" number) ~str:repo.pulls_url in
    url

  let issues_url ~(repo : Github_t.repository) ~number =
    let _, url = ExtLib.String.replace ~sub:"{/number}" ~by:(sprintf "/%d" number) ~str:repo.issues_url in
    url

  let compare_url ~(repo : Github_t.repository) ~basehead:(base, merge) =
    let _, url = ExtLib.String.replace ~sub:"{/basehead}" ~by:(sprintf "/%s...%s" base merge) ~str:repo.compare_url in
    url

  let build_headers ?token () =
    let headers = [ "Accept: application/vnd.github.v3+json" ] in
    Option.map_default (fun v -> sprintf "Authorization: token %s" v :: headers) headers token

  let prepare_request ~secrets ~(repo : Github_t.repository) url =
    let token = Context.gh_token_of_secrets secrets repo.url in
    let headers = build_headers ?token () in
    let url =
      match Context.gh_repo_of_secrets secrets repo.url with
      | None -> url
      | Some repo_config ->
        (* The url might have been built based on information received through an untrusted source such as a slack message.
           Normalizing it using trusted secrets. *)
        let repo_config_url_scheme = repo_config.url |> Uri.of_string |> Uri.scheme in
        url |> Uri.of_string |> flip Uri.with_scheme repo_config_url_scheme |> Uri.to_string
    in
    headers, url

  let get_resource ~secrets ~repo url =
    let headers, url = prepare_request ~secrets ~repo url in
    http_request ~headers `GET url
    |> Lwt_result.map_error (fun e -> sprintf "error while querying remote: %s\nfailed to get resource from %s" e url)

  let post_resource ~secrets ~repo body url =
    let headers, url = prepare_request ~secrets ~repo url in
    http_request ~headers ~body:(`Raw ("application/json; charset=utf-8", body)) `POST url
    |> Lwt_result.map_error (sprintf "POST to %s failed : %s" url)

  let get_config ~(ctx : Context.t) ~repo =
    let secrets = Context.get_secrets_exn ctx in
    let url = contents_url ~repo ~path:ctx.config_filename in
    let* res =
      get_resource ~secrets ~repo url
      |> Lwt_result.map_error (fun e ->
             sprintf "error while querying remote: %s\nfailed to get config from file %s" e url)
    in
    let response = Github_j.content_api_response_of_string res in
    match response.encoding with
    | "base64" -> begin
      try
        response.content
        |> Re2.rewrite_exn (Re2.create_exn "\n") ~template:""
        |> decode_string_pad
        |> Config_j.config_of_string
        |> fun res -> Lwt.return @@ Ok res
      with exn ->
        let e = Exn.to_string exn in
        Lwt.return
        @@ fmt_error "error while reading config from GitHub response: %s\nfailed to get config from file %s" e url
    end
    | encoding ->
      Lwt.return
      @@ fmt_error "unexpected encoding '%s' in Github response\nfailed to get config from file %s" encoding url

  let get_api_commit ~(ctx : Context.t) ~(repo : Github_t.repository) ~sha =
    let%lwt res = commits_url ~repo ~sha |> get_resource ~secrets:(Context.get_secrets_exn ctx) ~repo in
    Lwt.return @@ Result.map Github_j.api_commit_of_string res

  let get_pull_request ~(ctx : Context.t) ~(repo : Github_t.repository) ~number =
    let%lwt res = pulls_url ~repo ~number |> get_resource ~secrets:(Context.get_secrets_exn ctx) ~repo in
    Lwt.return @@ Result.map Github_j.pull_request_of_string res

  let get_issue ~(ctx : Context.t) ~(repo : Github_t.repository) ~number =
    let%lwt res = issues_url ~repo ~number |> get_resource ~secrets:(Context.get_secrets_exn ctx) ~repo in
    Lwt.return @@ Result.map Github_j.issue_of_string res

  let get_compare ~(ctx : Context.t) ~(repo : Github_t.repository) ~basehead =
    let%lwt res = compare_url ~repo ~basehead |> get_resource ~secrets:(Context.get_secrets_exn ctx) ~repo in
    Lwt.return @@ Result.map Github_j.compare_of_string res

  let request_reviewers ~(ctx : Context.t) ~(repo : Github_t.repository) ~number ~reviewers =
    let body = Github_j.string_of_request_reviewers_req reviewers in
    let%lwt res =
      pulls_url ~repo ~number ^ "/requested_reviewers"
      |> post_resource ~secrets:(Context.get_secrets_exn ctx) ~repo body
    in
    Lwt.return @@ Result.map ignore res
end

module Slack : Api.Slack = struct
  let log = Log.from "slack"

  let slack_api_request ?headers ?body meth url read =
    let* s = http_request ?headers ?body meth url |> Lwt_result.map_error (query_error_msg url) in
    match Slack_j.slack_response_of_string read s with
    | res -> Lwt.return res
    | exception exn -> Lwt.return_error (query_error_msg url (Exn.to_string exn))

  let default_read_error name e = sprintf "%s: failure : %s" name e

  let request_token_auth_err ~name ?headers ?body ~ctx meth path ~read_error read =
    log#info "%s: starting request" name;
    let secrets = Context.get_secrets_exn ctx in
    match secrets.slack_access_token with
    | None -> Lwt.return_error @@ read_error (sprintf "%s: failed to retrieve Slack access token" name)
    | Some access_token ->
      let headers = bearer_token_header access_token :: Option.default [] headers in
      let url = sprintf "https://slack.com/api/%s" path in
      slack_api_request ?body ~headers meth url read |> Lwt_result.map_error read_error

  let request_token_auth ~name ?headers ?body ~ctx meth path read =
    request_token_auth_err ~name ?headers ?body ~ctx meth path ~read_error:(default_read_error name) read

  let read_unit s l =
    (* must read whole response to update lexer state *)
    ignore (Slack_j.read_ok_res s l)

  let join_channel ~(ctx : Context.t) (channel : Slack_channel.Ident.t) =
    let data = Slack_j.(string_of_join_channel_req { channel }) in
    let body = `Raw ("application/json; charset=utf-8", data) in
    request_token_auth ~name:"join channel" ~ctx `POST ~body "conversations.join" Slack_j.read_ok_res

  let lookup_user_cache = Hashtbl.create 50

  let lookup_user' ~(ctx : Context.t) ~(cfg : Config_t.config) ~email () =
    (* Check if config holds the Github to Slack email mapping  *)
    let email = List.assoc_opt email cfg.user_mappings |> Option.default email in
    let url_args = Web.make_url_args [ "email", email ] in
    let* user =
      request_token_auth ~name:"lookup user by email" ~ctx `GET
        (sprintf "users.lookupByEmail?%s" url_args)
        Slack_j.read_lookup_user_res
    in
    Hashtbl.replace lookup_user_cache email user;
    Lwt.return_ok user

  (** [lookup_user cfg email] queries slack for a user profile with [email] *)
  let lookup_user ?(cache : [ `Use | `Refresh ] = `Use) ~(ctx : Context.t) ~(cfg : Config_t.config) ~email () =
    match cache with
    | `Refresh -> lookup_user' ~ctx ~cfg ~email ()
    | `Use ->
    match Hashtbl.find_opt lookup_user_cache email with
    | Some user -> Lwt.return_ok user
    | None -> lookup_user' ~ctx ~cfg ~email ()

  let list_users ?cursor ?limit ~(ctx : Context.t) () =
    let cursor_option = Option.map (fun c -> "cursor", c) cursor in
    let limit_option = Option.map (fun l -> "limit", Int.to_string l) limit in
    let url_args = Web.make_url_args @@ List.filter_map id [ cursor_option; limit_option ] in
    request_token_auth ~name:"list users" ~ctx `GET (sprintf "users.list?%s" url_args) Slack_j.read_list_users_res

  (** [send_notification ctx msg] notifies [msg.channel] with the payload [msg];
      uses web API with access token if available, or with webhook otherwise *)
  let send_notification ~(ctx : Context.t) ~(msg : Slack_t.post_message_req) =
    log#info "sending to %s" (Slack_channel.Any.project msg.channel);
    let build_error e = sprintf "%s\nfailed to send Slack notification" e in
    let secrets = Context.get_secrets_exn ctx in
    let headers, url =
      match Context.hook_of_channel ctx msg.channel with
      | Some url -> [], Some url
      | None ->
      match secrets.slack_access_token with
      | Some access_token -> [ bearer_token_header access_token ], Some "https://slack.com/api/chat.postMessage"
      | None -> [], None
    in
    match url with
    | None ->
      Lwt.return
      @@ Error
           (build_error
           @@ sprintf "no token or webhook configured to notify channel %s" (Slack_channel.Any.project msg.channel))
    | Some url ->
      let data = Slack_j.string_of_post_message_req msg in
      let body = `Raw ("application/json", data) in
      log#info "data: %s" data;
      let* res =
        slack_api_request ~body ~headers `POST url Slack_j.read_post_message_res |> Lwt_result.map_error build_error
      in
      Lwt.return @@ Ok res

  let send_chat_unfurl ~(ctx : Context.t) ~channel ~ts ~unfurls () =
    let req = Slack_j.{ channel; ts; unfurls } in
    let data = Slack_j.string_of_chat_unfurl_req req in
    request_token_auth ~name:"unfurl slack links"
      ~body:(`Raw ("application/json", data))
      ~ctx `POST "chat.unfurl" read_unit

  let send_auth_test ~(ctx : Context.t) () =
    request_token_auth ~name:"retrieve bot information" ~ctx `POST "auth.test" Slack_j.read_auth_test_res

  let get_thread_permalink ~(ctx : Context.t) (thread : State_t.slack_thread) =
    let url_args =
      Web.make_url_args
        [ "channel", Slack_channel.Ident.project thread.cid; "message_ts", Slack_timestamp.project thread.ts ]
    in
    match%lwt
      request_token_auth ~name:"retrieve message permalink" ~ctx `GET
        (sprintf "chat.getPermalink?%s" url_args)
        Slack_j.read_permalink_res
    with
    | Error (s : string) ->
      log#warn "couldn't fetch permalink for slack thread %s: %s" (Slack_timestamp.project thread.ts) s;
      Lwt.return_none
    | Ok (res : Slack_t.permalink_res) when res.ok = false ->
      log#warn "bad request fetching permalink for slack thread %s: %s" (Slack_timestamp.project thread.ts)
        (Option.default "" res.error);
      Lwt.return_none
    | Ok ({ permalink; _ } : Slack_t.permalink_res) -> Lwt.return_some permalink

  let get_upload_URL_external ~ctx ~filename ~size =
    let url_args = Web.make_url_args [ "filename", filename; "length", Int.to_string size ] in
    request_token_auth ~name:"get_upload_URL_external" ~ctx `GET
      (sprintf "files.getUploadURLExternal?%s" url_args)
      Slack_j.read_upload_url_res

  let post_file_content ~upload_url ~filename ~content =
    let* s = http_request ~body:(`Raw ("text/plain", content)) `POST upload_url in
    log#info "uploaded file for %s. Reply: %s" filename s;
    Lwt.return_ok ()

  let get_upload_URL_external_and_post_file_content ~ctx (file : Slack.file) =
    let { Slack.name; alt_txt = _; content; title } = file in
    let* { Slack_t.upload_url; file_id } = get_upload_URL_external ~ctx ~filename:name ~size:(String.length content) in
    let* () = post_file_content ~upload_url ~filename:name ~content in
    Lwt.return_ok ({ id = file_id; title } : Slack_t.file)

  let complete_upload_external ~(ctx : Context.t) ?channel_id ?thread_ts ?initial_comment files =
    let name = "complete_upload_external" in
    let req ~read_error () =
      log#info "send file: complete_upload_external to channel: %S and thread %S"
        (match channel_id with
        | None -> "None"
        | Some c -> Slack_channel.Ident.project c)
        (match thread_ts with
        | None -> "None"
        | Some ts -> Slack_timestamp.project ts);
      let req = { Slack_t.files; channel_id; thread_ts; initial_comment } in
      let data = Slack_j.string_of_complete_upload_external_req req in
      log#info "data: %s" data;
      let body = `Raw ("application/json; charset=utf-8", data) in
      let* _res =
        request_token_auth_err ~name ~ctx `POST ~body
          (sprintf "files.completeUploadExternal")
          ~read_error Slack_j.read_complete_upload_external_res
      in
      Lwt.return_ok ()
    in
    match%lwt
      req
        ~read_error:(function
          | "not_in_channel" -> `Not_in_channel
          | e -> `Other e)
        ()
    with
    | Error `Not_in_channel ->
      let channel = Option.get channel_id in
      let* _res = join_channel ~ctx channel in
      req ~read_error:(default_read_error name) ()
    | Error (`Other e) -> Lwt.return_error e
    | Ok () -> Lwt.return_ok ()

  let send_file ~(ctx : Context.t) ~(file : Slack.file_req) =
    let { Slack.files; channel_id; initial_comment; thread_ts } = file in
    let%lwt files = Lwt_list.map_s (get_upload_URL_external_and_post_file_content ~ctx) files in
    let files, errors =
      List.partition_map
        (function
          | Ok v -> Left v
          | Error e -> Right e)
        files
    in
    match errors with
    | e :: _ -> Lwt.return_error e
    | [] -> complete_upload_external ~ctx ?channel_id ?thread_ts ?initial_comment files
end

module Buildkite : Api.Buildkite = struct
  let log = Log.from "buildkite"

  module Builds_cache = Cache (struct
    type t = Buildkite_t.get_build_res
  end)

  (* 24h cache ttl and purge interval. We store so little data per build that it's not worth cleaning up entries sooner. *)
  let builds_cache = Builds_cache.create ~ttl:Builds_cache.default_purge_interval ()

  let buildkite_api_request ?headers ?body meth url read =
    let* s = http_request ?headers ?body meth url |> Lwt_result.map_error (query_error_msg url) in
    match read s with
    | res -> Lwt.return_ok res
    | exception exn -> Lwt.return_error (query_error_msg url (Exn.to_string exn))

  let request_token_auth ~name ?headers ?body ~ctx meth path read =
    log#info "%s: starting request" name;
    let secrets = Context.get_secrets_exn ctx in
    match secrets.buildkite_access_token with
    | None -> Lwt.return @@ fmt_error "%s: failed to retrieve Buildkite access token" name
    | Some access_token ->
      let headers = bearer_token_header access_token :: Option.default [] headers in
      let url = sprintf "https://api.buildkite.com/v2/%s" path in
      buildkite_api_request ?body ~headers meth url read
      |> Lwt_result.map_error (fun e -> sprintf "%s: failure : %s" name e)

  let get_job_log ~ctx n (job : Buildkite_t.job) =
    let* org, pipeline, build_nr = Lwt.return @@ Util.Build.get_org_pipeline_build n in
    let url = sprintf "organizations/%s/pipelines/%s/builds/%s/jobs/%s/log" org pipeline build_nr job.id in
    request_token_auth ~name:"get buildkite job logs" ~ctx `GET url Buildkite_j.job_log_of_string

  let get_build' ~ctx ~org ~pipeline ~build_nr map =
    let build_url = sprintf "organizations/%s/pipelines/%s/builds/%s" org pipeline build_nr in
    let* build = request_token_auth ~name:"get build details" ~ctx `GET build_url Buildkite_j.get_build_res_of_string in
    Builds_cache.set builds_cache build_nr build;
    Lwt.return_ok (map build)

  let get_build ?(cache : [ `Use | `Refresh ] = `Use) ~(ctx : Context.t) (n : Github_t.status_notification) =
    let* org, pipeline, build_nr = Lwt.return @@ Util.Build.get_org_pipeline_build n in
    match cache with
    | `Use ->
      (match Builds_cache.get builds_cache build_nr with
      | Some build -> Lwt.return_ok build
      | None -> get_build' ~ctx ~org ~pipeline ~build_nr id)
    | `Refresh -> get_build' ~ctx ~org ~pipeline ~build_nr id

  let get_build_branch ~(ctx : Context.t) (n : Github_t.status_notification) =
    let* org, pipeline, build_nr = Lwt.return @@ Util.Build.get_org_pipeline_build n in
    let map_branch { Buildkite_t.branch; _ } : Github_t.branch = { name = branch } in
    match Builds_cache.get builds_cache build_nr with
    | Some { Buildkite_t.branch; _ } -> Lwt.return_ok ({ name = branch } : Github_t.branch)
    | None ->
      log#info "Fetching branch details for build %s in pipeline %s" build_nr pipeline;
      get_build' ~ctx ~org ~pipeline ~build_nr map_branch
end
