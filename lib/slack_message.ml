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

let pp_file (file : file) = sprintf "<%s|%s>" file.url (Mrkdwn.escape_mrkdwn file.filename)

let populate_commit repository (commit : api_commit) =
  let ({ sha; commit; url; author; files; stats } : api_commit) = commit in
  let get_files () = List.map files ~f:pp_file in
  let title =
    sprintf "`<%s|%s>` *%s - %s*" url (Slack.git_short_sha_hash sha)
      (escape_mrkdwn @@ first_line commit.message)
      (escape_mrkdwn commit.author.name)
  in
  let num_change = List.length files in
  let changes =
    sprintf "%d %s with %d %s and %d %s:" num_change
      (Slack.pluralize "changed file" num_change "s")
      stats.additions
      (Slack.pluralize "addition" stats.additions "s")
      stats.deletions
      (Slack.pluralize "deletion" stats.deletions "s")
  in
  let files = get_files () |> String.concat ~sep:"\n" in
  let text = sprintf "%s\n%s\n%s" title changes files in
  let fallback = sprintf "[%s] %s - %s" (git_short_sha_hash sha) commit.message commit.author.name in
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
