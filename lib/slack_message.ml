open Base
open Printf
open Github_t
open Slack_t
open Common
open Mrkdwn

let color_of_state ?(draft = false) ?(merged = false) state =
  match draft with
  | true -> Colors.gray
  | false ->
  match merged with
  | true -> Colors.purple
  | false ->
  match state with
  | Open -> Colors.green
  | Closed -> Colors.red

let gh_name_of_string = sprintf "@%s"

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

let simple_footer (repository : repository) = sprintf "<%s|%s>" repository.url (escape_mrkdwn repository.full_name)
let base_attachment repository = { empty_attachment with footer = Some (simple_footer repository) }
let pp_label (label : label) = label.name
let pp_github_user (user : github_user) = gh_name_of_string user.login
let pp_github_team (team : github_team) = gh_name_of_string team.slug

let populate_pull_request repository (pull_request : pull_request) =
  let ({
         title;
         number;
         html_url;
         user;
         assignees;
         comments;
         labels;
         requested_reviewers;
         requested_teams;
         state;
         draft;
         merged;
         _;
       }
        : pull_request
        )
    =
    pull_request
  in
  let get_reviewers () =
    List.concat [ List.map requested_reviewers ~f:pp_github_user; List.map requested_teams ~f:pp_github_team ]
  in
  let fields =
    [
      "Assignees", List.map assignees ~f:pp_github_user;
      "Labels", List.map labels ~f:pp_label;
      ("Comments", if comments > 0 then [ Int.to_string comments ] else []);
      "Reviewers", get_reviewers ();
    ]
    |> List.filter_map ~f:(fun (t, v) -> if List.is_empty v then None else Some (t, String.concat v ~sep:", "))
    |> List.map ~f:(fun (t, v) -> { title = Some t; value = v; short = true })
  in
  let get_title () = sprintf "#%d %s" number (Mrkdwn.escape_mrkdwn title) in
  {
    (base_attachment repository) with
    author_name = Some user.login;
    author_link = Some user.html_url;
    author_icon = Some user.avatar_url;
    color = Some (color_of_state ~draft ~merged state);
    fields = Some fields;
    mrkdwn_in = Some [ "text" ];
    title = Some (get_title ());
    title_link = Some html_url;
    fallback = Some (sprintf "[%s] %s" repository.full_name title);
  }

let populate_issue repository (issue : issue) =
  let ({ title; number; html_url; user; assignees; comments; labels; state; _ } : issue) = issue in
  let fields =
    [
      "Assignees", List.map assignees ~f:pp_github_user;
      "Labels", List.map labels ~f:pp_label;
      ("Comments", if comments > 0 then [ Int.to_string comments ] else []);
    ]
    |> List.filter_map ~f:(fun (t, v) -> if List.is_empty v then None else Some (t, String.concat v ~sep:", "))
    |> List.map ~f:(fun (t, v) -> { title = Some t; value = v; short = true })
  in
  let get_title () = sprintf "#%d %s" number (Mrkdwn.escape_mrkdwn title) in
  {
    (base_attachment repository) with
    author_name = Some user.login;
    author_link = Some user.html_url;
    author_icon = Some user.avatar_url;
    color = Some (color_of_state state);
    fields = Some fields;
    mrkdwn_in = Some [ "text" ];
    title = Some (get_title ());
    title_link = Some html_url;
    fallback = Some (sprintf "[%s] %s" repository.full_name title);
  }

(* use some date library :see_no_evil: *)
let month = function
  | 1 -> "Jan"
  | 2 -> "Feb"
  | 3 -> "Mar"
  | 4 -> "Apr"
  | 5 -> "May"
  | 6 -> "Jun"
  | 7 -> "Jul"
  | 8 -> "Aug"
  | 9 -> "Sep"
  | 10 -> "Oct"
  | 11 -> "Nov"
  | 12 -> "Dec"
  | _ -> assert false

let condense_file_changes files =
  match files with
  | [ f ] -> sprintf "_modified `%s` (+%d-%d)_" (escape_mrkdwn f.filename) f.additions f.deletions
  | _ ->
    let prefix_path =
      List.map files ~f:(fun f -> f.filename)
      |> Common.longest_common_prefix
      |> String.split ~on:'/'
      |> List.drop_last_exn
      |> String.concat ~sep:"/"
    in
    sprintf "modified %d files%s" (List.length files)
      (if String.is_empty prefix_path then "" else sprintf " in `%s/`" prefix_path)

let populate_commit ?(include_changes = true) repository (commit : api_commit) =
  let ({ sha; commit; url; author; files; _ } : api_commit) = commit in
  let title =
    match author with
    | Some author ->
      sprintf "`<%s|%s>` %s - <%s|%s>" url (Slack.git_short_sha_hash sha)
        (escape_mrkdwn @@ first_line commit.message)
        (escape_mrkdwn author.html_url) (escape_mrkdwn commit.author.name)
    | None ->
      sprintf "`<%s|%s>` %s - %s" url (Slack.git_short_sha_hash sha)
        (escape_mrkdwn @@ first_line commit.message)
        (escape_mrkdwn commit.author.name)
  in
  let changes () =
    let where = condense_file_changes files in
    let when_ =
      (*
        use "today" on same day, "Month Day" during same year
        even better would be to have "N units ago" and tooltip computed by slack at time of presentation
        but looks like slack doesn't provide such functionality
      *)
      try
        match String.split commit.author.date ~on:'T' with
        | [ date; _ ] ->
          let yy, mm, dd =
            let tm = Unix.gmtime @@ Devkit.Time.now () in
            tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday
          in
          ( match List.map ~f:Int.of_string @@ String.split date ~on:'-' with
          | [ y; m; d ] when y = yy && m = mm && d = dd -> "today"
          | [ y; m; d ] when y = yy -> sprintf "on %s %d" (month m) d
          | _ -> "on " ^ date
          )
        | _ -> failwith "wut"
      with _ -> "on " ^ commit.author.date
    in
    match where, when_ with
    | "", when_ -> when_
    | where, when_ -> sprintf "%s %s" where when_
  in
  let text = sprintf "%s\n%s" title (if include_changes then changes () else "") in
  let fallback = sprintf "[%s] %s - %s" (Slack.git_short_sha_hash sha) commit.message commit.author.name in
  {
    (base_attachment repository) with
    footer =
      Some
        (simple_footer repository
        ^ if String.equal commit.committer.date commit.author.date then "" else " " ^ commit.committer.date
        );
    (*
    author_name = Some author.login;
    author_link = Some author.html_url;
    *)
    author_icon =
      ( match author with
      | Some author -> Some author.avatar_url
      | None -> None
      );
    color = Some Colors.gray;
    mrkdwn_in = Some [ "text" ];
    text = Some text;
    fallback = Some fallback;
  }

let populate_compare repository (compare : compare) =
  let base =
    {
      (base_attachment repository) with
      footer = Some (simple_footer repository);
      author_icon = None;
      color = Some Colors.gray;
      mrkdwn_in = Some [ "text" ];
      text = None;
      fallback = None;
    }
  in
  match compare.total_commits = 0 with
  | true ->
    let no_commit_msg = "There are no commit difference in this compare!" in
    { base with text = Some no_commit_msg; fallback = Some no_commit_msg }
  | false ->
    let commits_unfurl = List.map compare.commits ~f:(populate_commit ~include_changes:false repository) in
    let commits_unfurl_text =
      Slack.pp_list_with_previews
        ~pp_item:(fun (commit_unfurl : unfurl) -> Option.value commit_unfurl.text ~default:"")
        commits_unfurl
    in

    let commits_unfurl_fallback =
      List.map commits_unfurl ~f:(fun commit_unfurl -> Option.value commit_unfurl.fallback ~default:"")
    in
    let file_stats =
      match condense_file_changes compare.files with
      | "" -> ""
      | text -> sprintf "\n\n%s" text
    in
    let text = sprintf "%s%s" (String.concat commits_unfurl_text) file_stats in
    let fallback = String.concat commits_unfurl_fallback in
    { base with text = Some text; fallback = Some fallback }
