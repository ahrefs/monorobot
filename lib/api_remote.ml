open Base
open Printf
open Devkit
open Common

module Github : Api.Github = struct
  let log = Log.from "github"

  let commits_url ~(repo : Github_t.repository) ~sha =
    String.substr_replace_first ~pattern:"{/sha}" ~with_:sha repo.commits_url

  let contents_url ~(repo : Github_t.repository) ~path =
    String.substr_replace_first ~pattern:"{+path}" ~with_:path repo.contents_url

  let build_headers ?token () =
    let headers = [ "Accept: application/vnd.github.v3+json" ] in
    Option.value_map token ~default:headers ~f:(fun v -> sprintf "Authorization: token %s" v :: headers)

  let get_config ~(ctx : Context.t) ~repo =
    let url = contents_url ~repo ~path:ctx.config_filename in
    let headers = build_headers ?token:ctx.gh_token () in
    match%lwt http_get ~headers url with
    | Error e ->
      log#error "error while querying %s: %s" url e;
      Lwt.return @@ fmt_error "failed to get config from file %s" url
    | Ok res ->
      let response = Github_j.content_api_response_of_string res in
      ( match response.encoding with
      | "base64" ->
        begin
          try
            response.content |> String.split_lines |> String.concat |> decode_string_pad |> Config_j.config_of_string
            |> fun res -> Lwt.return @@ Ok res
          with Base64.Invalid_char as exn ->
            log#error ~exn "failed to decode base64 in Github response";
            Lwt.return @@ fmt_error "failed to get config from file %s" url
        end
      | encoding ->
        log#error "unexpected encoding '%s' in Github response" encoding;
        Lwt.return @@ fmt_error "failed to get config from file %s" url
      )

  let get_api_commit ~(ctx : Context.t) ~repo ~sha =
    let url = commits_url ~repo ~sha in
    let headers = build_headers ?token:ctx.gh_token () in
    match%lwt http_get ~headers url with
    | Ok res -> Lwt.return @@ Ok (Github_j.api_commit_of_string res)
    | Error e ->
      log#error "error while querying %s: %s" url e;
      Lwt.return @@ fmt_error "failed to get api commit %s" sha
end

module Slack : Api.Slack = struct
  let log = Log.from "slack"

  let send_notification ~chan ~msg ~url =
    let data = Slack_j.string_of_webhook_notification msg in
    log#info "sending to %s : %s" chan data;
    match%lwt http_post ~path:url ~data with
    | Ok _ -> Lwt.return @@ Ok ()
    | Error e ->
      log#error "error while querying %s: %s" url e;
      Lwt.return @@ fmt_error "failed to send Slack notification"
end
