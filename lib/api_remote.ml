open Printf
open Devkit
open Common

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
    match%lwt http_request ~headers `GET url with
    | Ok res -> Lwt.return @@ Ok res
    | Error e -> Lwt.return @@ fmt_error "error while querying remote: %s\nfailed to get resource from %s" e url

  let post_resource ~secrets ~repo body url =
    let headers, url = prepare_request ~secrets ~repo url in
    match%lwt http_request ~headers ~body:(`Raw ("application/json; charset=utf-8", body)) `POST url with
    | Ok res -> Lwt.return @@ Ok res
    | Error e -> Lwt.return @@ fmt_error "POST to %s failed : %s" url e

  let get_config ~(ctx : Context.t) ~repo =
    let secrets = Context.get_secrets_exn ctx in
    let url = contents_url ~repo ~path:ctx.config_filename in
    match%lwt get_resource ~secrets ~repo url with
    | Error e -> Lwt.return @@ fmt_error "error while querying remote: %s\nfailed to get config from file %s" e url
    | Ok res ->
      let response = Github_j.content_api_response_of_string res in
      (match response.encoding with
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
        @@ fmt_error "unexpected encoding '%s' in Github response\nfailed to get config from file %s" encoding url)

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
  let query_error_msg url e = sprintf "error while querying %s: %s" url e

  let slack_api_request ?headers ?body meth url read =
    match%lwt http_request ?headers ?body meth url with
    | Error e -> Lwt.return_error (query_error_msg url e)
    | Ok s ->
    match Slack_j.slack_response_of_string read s with
    | res -> Lwt.return res
    | exception exn -> Lwt.return_error (query_error_msg url (Exn.to_string exn))

  let bearer_token_header access_token = sprintf "Authorization: Bearer %s" (Uri.pct_encode access_token)

  let request_token_auth ~name ?headers ?body ~ctx meth path read =
    log#info "%s: starting request" name;
    let secrets = Context.get_secrets_exn ctx in
    match secrets.slack_access_token with
    | None -> Lwt.return @@ fmt_error "%s: failed to retrieve Slack access token" name
    | Some access_token ->
      let headers = bearer_token_header access_token :: Option.default [] headers in
      let url = sprintf "https://slack.com/api/%s" path in
      (match%lwt slack_api_request ?body ~headers meth url read with
      | Ok res -> Lwt.return @@ Ok res
      | Error e -> Lwt.return @@ fmt_error "%s: failure : %s" name e)

  let read_unit s l =
    (* must read whole response to update lexer state *)
    ignore (Slack_j.read_ok_res s l)

  let lookup_user_cache = Hashtbl.create 50

  let lookup_user' ~(ctx : Context.t) ~(cfg : Config_t.config) ~email () =
    (* Check if config holds the Github to Slack email mapping  *)
    let email = List.assoc_opt email cfg.user_mappings |> Option.default email in
    let url_args = Web.make_url_args [ "email", email ] in
    match%lwt
      request_token_auth ~name:"lookup user by email" ~ctx `GET
        (sprintf "users.lookupByEmail?%s" url_args)
        Slack_j.read_lookup_user_res
    with
    | Error _ as e -> Lwt.return e
    | Ok user ->
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
    log#info "sending to %s" msg.channel;
    let build_error e = fmt_error "%s\nfailed to send Slack notification" e in
    let secrets = Context.get_secrets_exn ctx in
    let headers, url, webhook_mode =
      match Context.hook_of_channel ctx msg.channel with
      | Some url -> [], Some url, true
      | None ->
      match secrets.slack_access_token with
      | Some access_token -> [ bearer_token_header access_token ], Some "https://slack.com/api/chat.postMessage", false
      | None -> [], None, false
    in
    match url with
    | None -> Lwt.return @@ build_error @@ sprintf "no token or webhook configured to notify channel %s" msg.channel
    | Some url ->
      let data = Slack_j.string_of_post_message_req msg in
      let body = `Raw ("application/json", data) in
      log#info "data: %s" data;
      if webhook_mode then begin
        match%lwt http_request ~body ~headers `POST url with
        | Ok _res ->
          (* Webhooks reply only 200 `ok`. We can't generate anything useful for notification success handlers *)
          Lwt.return @@ Ok None
        | Error e -> Lwt.return @@ build_error (query_error_msg url e)
      end
      else begin
        match%lwt slack_api_request ~body ~headers `POST url Slack_j.read_post_message_res with
        | Ok res -> Lwt.return @@ Ok (Some res)
        | Error e -> Lwt.return @@ build_error e
      end

  let send_chat_unfurl ~(ctx : Context.t) ~channel ~ts ~unfurls () =
    let req = Slack_j.{ channel; ts; unfurls } in
    let data = Slack_j.string_of_chat_unfurl_req req in
    request_token_auth ~name:"unfurl slack links"
      ~body:(`Raw ("application/json", data))
      ~ctx `POST "chat.unfurl" read_unit

  let send_auth_test ~(ctx : Context.t) () =
    request_token_auth ~name:"retrieve bot information" ~ctx `POST "auth.test" Slack_j.read_auth_test_res

  let get_thread_permalink ~(ctx : Context.t) (thread : State_t.slack_thread) =
    let url_args = Web.make_url_args [ "channel", thread.cid; "message_ts", thread.ts ] in
    match%lwt
      request_token_auth ~name:"retrieve message permalink" ~ctx `GET
        (sprintf "chat.getPermalink?%s" url_args)
        Slack_j.read_permalink_res
    with
    | Error (s : string) ->
      log#warn "couldn't fetch permalink for slack thread %s: %s" thread.ts s;
      Lwt.return_none
    | Ok (res : Slack_t.permalink_res) when res.ok = false ->
      log#warn "bad request fetching permalink for slack thread %s: %s" thread.ts (Option.default "" res.error);
      Lwt.return_none
    | Ok ({ permalink; _ } : Slack_t.permalink_res) -> Lwt.return_some permalink
end
