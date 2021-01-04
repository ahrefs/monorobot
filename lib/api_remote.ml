open Base
open Printf
open Devkit
open Common

module Github : Api.Github = struct
  let commits_url ~(repo : Github_t.repository) ~sha =
    String.substr_replace_first ~pattern:"{/sha}" ~with_:("/" ^ sha) repo.commits_url

  let contents_url ~(repo : Github_t.repository) ~path =
    String.substr_replace_first ~pattern:"{+path}" ~with_:path repo.contents_url

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
          with Base64.Invalid_char as exn ->
            let e = Exn.to_string exn in
            Lwt.return
            @@ fmt_error "error while decoding base64 in GitHub response: %s\nfailed to get config from file %s" e url
        end
      | encoding ->
        Lwt.return
        @@ fmt_error "unexpected encoding '%s' in Github response\nfailed to get config from file %s" encoding url
      )

  let get_api_commit ~(ctx : Context.t) ~repo ~sha =
    let secrets = Context.get_secrets_exn ctx in
    let url = commits_url ~repo ~sha in
    let headers = build_headers ?token:secrets.gh_token () in
    match%lwt http_request ~headers `GET url with
    | Ok res -> Lwt.return @@ Ok (Github_j.api_commit_of_string res)
    | Error e -> Lwt.return @@ fmt_error "error while querying remote: %s\nfailed to get api commit from file %s" e url
end

module Slack : Api.Slack = struct
  let log = Log.from "slack"

  let send_notification ~chan ~msg ~url =
    let data = Slack_j.string_of_post_message_req msg in
    let body = `Raw ("application/json", data) in
    log#info "sending to %s : %s" chan data;
    match%lwt http_request ~body `POST url with
    | Ok _ -> Lwt.return @@ Ok ()
    | Error e ->
      Lwt.return @@ fmt_error "error while querying remote: %s\nfailed to send Slack notification to %s" e url
end
