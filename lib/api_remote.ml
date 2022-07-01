open Base
open Printf
open Devkit
open Common

module Github : Api.Github = struct
  let commits_url ~(repo : Github_t.repository) ~sha =
    String.substr_replace_first ~pattern:"{/sha}" ~with_:("/" ^ sha) repo.commits_url

  let contents_url ~(repo : Github_t.repository) ~path =
    String.substr_replace_first ~pattern:"{+path}" ~with_:path repo.contents_url

  let pulls_url ~(repo : Github_t.repository) ~number =
    String.substr_replace_first ~pattern:"{/number}" ~with_:(sprintf "/%d" number) repo.pulls_url

  let issues_url ~(repo : Github_t.repository) ~number =
    String.substr_replace_first ~pattern:"{/number}" ~with_:(sprintf "/%d" number) repo.issues_url

  let build_headers ?token () =
    let headers = [ "Accept: application/vnd.github.v3+json" ] in
    Option.value_map token ~default:headers ~f:(fun v -> sprintf "Authorization: token %s" v :: headers)

  let get_config ~(ctx : Context.t) ~repo =
    let secrets = Context.get_secrets_exn ctx in
    let url = contents_url ~repo ~path:ctx.config_filename in
    let token = Context.gh_token_of_secrets secrets repo.url in
    let headers = build_headers ?token () in
    match%lwt http_request ~headers `GET url with
    | Error e -> Lwt.return @@ fmt_error "error while querying remote: %s\nfailed to get config from file %s" e url
    | Ok res ->
      let response = Github_j.content_api_response_of_string res in
      ( match response.encoding with
      | "base64" ->
        begin
          try
            response.content |> String.split_lines |> String.concat |> decode_string_pad |> Config_j.config_of_string
            |> fun res -> Lwt.return @@ Ok res
          with exn ->
            let e = Exn.to_string exn in
            Lwt.return
            @@ fmt_error "error while reading config from GitHub response: %s\nfailed to get config from file %s" e url
        end
      | encoding ->
        Lwt.return
        @@ fmt_error "unexpected encoding '%s' in Github response\nfailed to get config from file %s" encoding url
      )

  let get_resource ~secrets ~repo_url url =
    let token = Context.gh_token_of_secrets secrets repo_url in
    let headers = build_headers ?token () in
    match%lwt http_request ~headers `GET url with
    | Ok res -> Lwt.return @@ Ok res
    | Error e -> Lwt.return @@ fmt_error "error while querying remote: %s\nfailed to get resource from %s" e url

  let post_resource ~secrets ~repo_url body url =
    let token = Context.gh_token_of_secrets secrets repo_url in
    let headers = build_headers ?token () in
    match%lwt http_request ~headers ~body:(`Raw ("application/json; charset=utf-8", body)) `POST url with
    | Ok res -> Lwt.return @@ Ok res
    | Error e -> Lwt.return @@ fmt_error "POST to %s failed : %s" url e

  let get_api_commit ~(ctx : Context.t) ~(repo : Github_t.repository) ~sha =
    let%lwt res = commits_url ~repo ~sha |> get_resource ~secrets:(Context.get_secrets_exn ctx) ~repo_url:repo.url in
    Lwt.return @@ Result.map res ~f:Github_j.api_commit_of_string

  let get_pull_request ~(ctx : Context.t) ~(repo : Github_t.repository) ~number =
    let%lwt res = pulls_url ~repo ~number |> get_resource ~secrets:(Context.get_secrets_exn ctx) ~repo_url:repo.url in
    Lwt.return @@ Result.map res ~f:Github_j.pull_request_of_string

  let get_issue ~(ctx : Context.t) ~(repo : Github_t.repository) ~number =
    let%lwt res = issues_url ~repo ~number |> get_resource ~secrets:(Context.get_secrets_exn ctx) ~repo_url:repo.url in
    Lwt.return @@ Result.map res ~f:Github_j.issue_of_string

  let request_reviewers ~(ctx : Context.t) ~repo ~number ~reviewers =
    let body = Github_j.string_of_request_reviewers_req reviewers in
    let%lwt res =
      pulls_url ~repo ~number ^ "/requested_reviewers"
      |> post_resource ~secrets:(Context.get_secrets_exn ctx) ~repo_url:repo.url body
    in
    Lwt.return @@ Result.map res ~f:(fun _ -> ())
end

module Slack : Api.Slack = struct
  let log = Log.from "slack"
  let query_error_msg url e = sprintf "error while querying %s: %s" url e

  let slack_api_request ?headers ?body meth url read =
    match%lwt http_request ?headers ?body meth url with
    | Error e -> Lwt.return @@ Error (query_error_msg url e)
    | Ok s -> Lwt.return @@ Slack_j.slack_response_of_string read s

  let bearer_token_header access_token = sprintf "Authorization: Bearer %s" (Uri.pct_encode access_token)

  let request_token_auth ~name ?headers ?body ~ctx meth path read =
    log#info "%s: starting request" name;
    let secrets = Context.get_secrets_exn ctx in
    match secrets.slack_access_token with
    | None -> Lwt.return @@ fmt_error "%s: failed to retrieve Slack access token" name
    | Some access_token ->
      let headers = bearer_token_header access_token :: Option.value ~default:[] headers in
      let url = sprintf "https://slack.com/api/%s" path in
      ( match%lwt slack_api_request ?body ~headers meth url read with
      | Ok res -> Lwt.return @@ Ok res
      | Error e -> Lwt.return @@ fmt_error "%s: failure : %s" name e
      )

  let read_unit s l =
    (* must read whole response to update lexer state *)
    ignore (Slack_j.read_ok_res s l)

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
        | Ok _res -> Lwt.return @@ Ok ()
        | Error e -> Lwt.return @@ build_error (query_error_msg url e)
      end
      else begin
        match%lwt slack_api_request ~body ~headers `POST url Slack_j.read_post_message_res with
        | Ok _res -> Lwt.return @@ Ok ()
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
end
