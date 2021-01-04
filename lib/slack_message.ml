open Base
open Printf
open Github_t
open Slack_t
open Common
open Mrkdwn

let empty_attachment =
  {
    mrkdwn_in = None;
    fallback = None;
    color = None;
    pretext = None;
    author_name = None;
    author_link = None;
    author_icon = None;
    title = None;
    title_link = None;
    text = None;
    fields = None;
    image_url = None;
    thumb_url = None;
    ts = None;
    footer = None;
  }

let base_attachment (repository : repository) =
  { empty_attachment with footer = Some (sprintf "<%s|%s>" repository.url (escape_mrkdwn repository.full_name)) }

let populate_commit repository (commit : api_commit) =
  let ({ sha; commit; url; author; files; _ } : api_commit) = commit in
  let title =
    sprintf "`<%s|%s>` *%s - %s*" url (Slack.git_short_sha_hash sha)
      (escape_mrkdwn @@ first_line commit.message)
      (escape_mrkdwn commit.author.name)
  in
  let num_change = List.length files in
  let prefix_path =
    List.map files ~f:(fun f -> f.filename)
    |> Common.longest_common_prefix
    |> String.split ~on:'/'
    |> List.drop_last_exn
    |> String.concat ~sep:"/"
  in
  let changes = sprintf "%d changed %s in `%s/`" num_change (Slack.pluralize "file" num_change "s") prefix_path in
  let text = sprintf "%s\n%s" title changes in
  let fallback = sprintf "[%s] %s - %s" (Slack.git_short_sha_hash sha) commit.message commit.author.name in
  {
    (base_attachment repository) with
    author_name = Some author.login;
    author_link = Some author.html_url;
    author_icon = Some author.avatar_url;
    color = Some Colors.gray;
    mrkdwn_in = Some [ "text" ];
    text = Some text;
    fallback = Some fallback;
  }
