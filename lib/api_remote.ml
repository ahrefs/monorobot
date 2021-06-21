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
    let headers = build_headers ?token:secrets.gh_token () in
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

  let get_resource (ctx : Context.t) url =
    let secrets = Context.get_secrets_exn ctx in
    let headers = build_headers ?token:secrets.gh_token () in
    match%lwt http_request ~headers `GET url with
    | Ok res -> Lwt.return @@ Ok res
    | Error e -> Lwt.return @@ fmt_error "error while querying remote: %s\nfailed to get resource from %s" e url

  let get_api_commit ~(ctx : Context.t) ~repo ~sha =
    let%lwt res = commits_url ~repo ~sha |> get_resource ctx in
    Lwt.return @@ Result.map res ~f:Github_j.api_commit_of_string

  let get_pull_request ~(ctx : Context.t) ~repo ~number =
    let%lwt res = pulls_url ~repo ~number |> get_resource ctx in
    Lwt.return @@ Result.map res ~f:Github_j.pull_request_of_string

  let get_issue ~(ctx : Context.t) ~repo ~number =
    let%lwt res = issues_url ~repo ~number |> get_resource ctx in
    Lwt.return @@ Result.map res ~f:Github_j.issue_of_string
end

module Slack : Api.Slack = struct
  let log = Log.from "slack"

  let bearer_token_header access_token = sprintf "Authorization: Bearer %s" (Uri.pct_encode access_token)

  (** `send_notification ctx msg` notifies `msg.channel` with the payload `msg`;
      uses web API with access token if available, or with webhook otherwise *)
  let send_notification ~(ctx : Context.t) ~(msg : Slack_t.post_message_req) =
    log#info "sending to %s" msg.channel;
    let build_error e = fmt_error "%s\nfailed to send Slack notification" e in
    let build_query_error url e = build_error @@ sprintf "error while querying %s: %s" url e in
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
      ( match%lwt http_request ~body ~headers `POST url with
      (* error detection in response: slack uses status codes for webhooks versus a 200 code w/ `error` field for web api *)
      | Ok s ->
        if webhook_mode then Lwt.return @@ Ok ()
        else (
          let res = Slack_j.post_message_res_of_string s in
          if res.ok then Lwt.return @@ Ok ()
          else Lwt.return @@ build_query_error url (Option.value ~default:"an unknown error occurred" res.error)
        )
      | Error e -> Lwt.return @@ build_query_error url e
      )

  let send_chat_unfurl ~(ctx : Context.t) req =
    log#info "unfurling Slack links";
    let secrets = Context.get_secrets_exn ctx in
    match secrets.slack_access_token with
    | None -> Lwt.return @@ fmt_error "failed to retrieve Slack access token"
    | Some access_token ->
      let data = Slack_j.string_of_chat_unfurl_req req in
      log#info "%s" data;
      let url = "https://slack.com/api/chat.unfurl" in
      let headers = [ bearer_token_header access_token ] in
      let body = `Raw ("application/json", data) in
      ( match%lwt http_request ~body ~headers `POST url with
      | Ok s ->
        let res = Slack_j.chat_unfurl_res_of_string s in
        if res.ok then Lwt.return @@ Ok ()
        else (
          let msg = Option.value ~default:"an unknown error occurred" res.error in
          Lwt.return @@ fmt_error "%s\nfailed to unfurl Slack links" msg
        )
      | Error e -> Lwt.return @@ fmt_error "error while querying %s: %s\nfailed to unfurl Slack links" url e
      )
end
