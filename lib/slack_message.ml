open Printf
open Github_t
open Slack_t
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
let pretext_slack_mention = Option.map (sprintf "<@%s>")

let unfurl_text_of_body = function
  | None | Some "" -> None
  | Some body -> Some (Mrkdwn.mrkdwn_of_markdown body)

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
         body;
         _;
       }
        : pull_request) =
    pull_request
  in
  let get_reviewers () =
    List.concat [ List.map pp_github_user requested_reviewers; List.map pp_github_team requested_teams ]
  in
  let fields =
    [
      "Assignees", List.map pp_github_user assignees;
      "Labels", List.map pp_label labels;
      ("Comments", if comments > 0 then [ Int.to_string comments ] else []);
      "Reviewers", get_reviewers ();
    ]
    |> List.filter_map (fun (t, v) -> if v = [] then None else Some (t, String.concat ", " v))
    |> List.map (fun (t, v) -> { title = Some t; value = v; short = true })
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
    text = unfurl_text_of_body body;
    fallback = Some (sprintf "[%s] %s" repository.full_name title);
  }

let populate_issue repository (issue : issue) =
  let ({ title; number; html_url; user; assignees; comments; labels; state; body; _ } : issue) = issue in
  let fields =
    [
      "Assignees", List.map pp_github_user assignees;
      "Labels", List.map pp_label labels;
      ("Comments", if comments > 0 then [ Int.to_string comments ] else []);
    ]
    |> List.filter_map (fun (t, v) -> if v = [] then None else Some (t, String.concat ", " v))
    |> List.map (fun (t, v) -> { title = Some t; value = v; short = true })
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
    text = unfurl_text_of_body body;
    fallback = Some (sprintf "[%s] %s" repository.full_name title);
  }

let populate_comment ?(kind = "Comment") repository ~subject ~color (comment : api_comment) =
  let footer =
    let author = Slack.pp_link ~url:comment.user.html_url comment.user.login in
    match comment.path with
    | None -> sprintf "%s · %s" author (simple_footer repository)
    | Some path -> sprintf "%s · %s · %s" author (simple_footer repository) (escape_mrkdwn path)
  in
  {
    (base_attachment repository) with
    footer = Some footer;
    color = Some color;
    mrkdwn_in = Some [ "text" ];
    title = Some (sprintf "%s on %s" kind (Mrkdwn.escape_mrkdwn subject));
    title_link = Some comment.html_url;
    text = unfurl_text_of_body (Some comment.body);
    fallback = Some (sprintf "[%s] %s on %s" repository.full_name kind subject);
  }

let populate_pull_request_comment repository ((pull_request : pull_request), comment) =
  let { number; title; draft; merged; state; _ } = pull_request in
  populate_comment repository ~subject:(sprintf "#%d %s" number title) ~color:(color_of_state ~draft ~merged state)
    comment

let populate_issue_comment repository ((issue : issue), comment) =
  let { number; title; state; _ } = issue in
  populate_comment repository ~subject:(sprintf "#%d %s" number title) ~color:(color_of_state state) comment

let populate_pull_request_review repository ((pull_request : pull_request), (review : api_review)) =
  let state, color =
    match String.lowercase_ascii review.state with
    | "approved" -> "Approved", Colors.green
    | "changes_requested" -> "Changes requested", Colors.red
    | "dismissed" -> "Dismissed", Colors.gray
    | _ -> "Commented", Colors.gray
  in
  let body =
    match review.body with
    | None | Some "" -> sprintf "**%s**" state
    | Some body -> sprintf "**%s**\n\n%s" state body
  in
  populate_comment ~kind:"Review" repository
    ~subject:(sprintf "#%d %s" pull_request.number pull_request.title)
    ~color
    { user = review.user; body; html_url = review.html_url; path = None }

let populate_commit_comment repository ((api_commit : api_commit), comment) =
  let subject =
    sprintf "commit %s %s" (Slack.git_short_sha_hash api_commit.sha) (Util.first_line api_commit.commit.message)
  in
  populate_comment repository ~subject ~color:Colors.gray comment

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
  | [] -> "_no files modified_"
  | first_file :: fl ->
    let rec longest_prefix_of_two_lists l1 l2 =
      match l1, l2 with
      | e1 :: l1', e2 :: l2' when e1 = e2 -> e1 :: longest_prefix_of_two_lists l1' l2'
      | _ -> []
    in
    let prefix_path =
      List.map (fun f -> String.split_on_char '/' f.filename) fl
      |> List.fold_left longest_prefix_of_two_lists (String.split_on_char '/' first_file.filename)
      |> String.concat "/"
    in
    sprintf "modified %d files%s" (List.length files) (if prefix_path = "" then "" else sprintf " in `%s/`" prefix_path)

let populate_commit ?(include_changes = true) repository (api_commit : api_commit) =
  let ({ sha; commit; author; files; _ } : api_commit) = api_commit in
  let title = Slack.pp_api_commit api_commit in
  let changes () =
    let where = condense_file_changes files in
    let when_ =
      (*
        use "today" on same day, "Month Day" during same year
        even better would be to have "N units ago" and tooltip computed by slack at time of presentation
        but looks like slack doesn't provide such functionality
      *)
      try
        match String.split_on_char 'T' commit.author.date with
        | [ date; _ ] ->
          let yy, mm, dd =
            let tm = Unix.gmtime @@ Devkit.Time.now () in
            tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday
          in
          (match List.map int_of_string @@ String.split_on_char '-' date with
          | [ y; m; d ] when y = yy && m = mm && d = dd -> "today"
          | [ y; m; d ] when y = yy -> sprintf "on %s %d" (month m) d
          | _ -> "on " ^ date)
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
    footer = Some (simple_footer repository ^ " " ^ commit.committer.date);
    author_icon =
      (match author with
      | Some author -> Some author.avatar_url
      | None -> None);
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
    let commits_unfurl = List.map (populate_commit ~include_changes:false repository) compare.commits in
    let commits_unfurl_text =
      Slack.pp_list_with_previews
        ~pp_item:(fun (commit_unfurl : unfurl) -> Option.default "" commit_unfurl.text)
        commits_unfurl
    in
    let commits_unfurl_fallback =
      List.map (fun commit_unfurl -> Option.default "" commit_unfurl.fallback) commits_unfurl
    in
    let file_stats = sprintf "\n%s" (condense_file_changes compare.files) in
    let text = sprintf "%s%s" (String.concat "" commits_unfurl_text) file_stats in
    let fallback = String.concat "" commits_unfurl_fallback in
    { base with text = Some text; fallback = Some fallback }
