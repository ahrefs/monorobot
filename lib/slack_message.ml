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
    fallback = Some (sprintf "[%s] %s" repository.full_name title);
  }

let populate_issue repository (issue : issue) =
  let ({ title; number; html_url; user; assignees; comments; labels; state; _ } : issue) = issue in
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

let buildkite_unfurl_line_cap = 50

let color_of_buildkite_state (state : Buildkite_t.build_state) =
  match state with
  | Buildkite_t.Passed -> "good"
  | Buildkite_t.Failed | Buildkite_t.Failing | Buildkite_t.Canceled | Buildkite_t.Canceling -> "danger"
  | _ -> Colors.gray

let string_of_buildkite_state : Buildkite_t.build_state -> string = function
  | Blocked -> "blocked"
  | Canceled -> "canceled"
  | Canceling -> "canceling"
  | Failed -> "failed"
  | Failing -> "failing"
  | Finished -> "finished"
  | Not_run -> "not_run"
  | Passed -> "passed"
  | Running -> "running"
  | Scheduled -> "scheduled"
  | Skipped -> "skipped"
  | Other state -> state

let string_of_buildkite_job_state : Buildkite_t.job_state -> string = function
  | Pending -> "pending"
  | Waiting -> "waiting"
  | Waiting_failed -> "waiting_failed"
  | Blocked -> "blocked"
  | Blocked_failed -> "blocked_failed"
  | Unblocked -> "unblocked"
  | Unblocked_failed -> "unblocked_failed"
  | Limiting -> "limiting"
  | Limited -> "limited"
  | Scheduled -> "scheduled"
  | Assigned -> "assigned"
  | Accepted -> "accepted"
  | Running -> "running"
  | Finished -> "finished"
  | Canceling -> "canceling"
  | Canceled -> "canceled"
  | Expired -> "expired"
  | Timing_out -> "timing_out"
  | Timed_out -> "timed_out"
  | Skipped -> "skipped"
  | Broken -> "broken"
  | Passed -> "passed"
  | Failed -> "failed"
  | Other state -> state

let find_buildkite_job jobs job_id =
  List.find_map
    (function
      | Buildkite_t.Script ({ id; _ } as job) | Buildkite_t.Trigger ({ id; _ } as job) when String.equal id job_id ->
        Some job
      | _ -> None)
    jobs

let buildkite_log_field (lines : Util.Build.line_range) (job_log : Buildkite_t.job_log) =
  let requested_len = lines.last_line - lines.first_line + 1 in
  let truncated = requested_len > buildkite_unfurl_line_cap in
  let last_line =
    if truncated then lines.first_line + buildkite_unfurl_line_cap - 1 else lines.last_line
  in
  let selected_lines =
    job_log.content |> Text_cleanup.cleanup |> String.split_on_char '\n'
    |> List.mapi (fun idx line -> idx + 1, line)
    |> List.filter_map (fun (line_nr, line) ->
           if line_nr >= lines.first_line && line_nr <= last_line then Some line else None)
  in
  let title =
    if lines.first_line = lines.last_line then sprintf "Log line %d" lines.first_line
    else sprintf "Log lines %d-%d" lines.first_line lines.last_line
  in
  let value =
    match selected_lines with
    | [] -> "_No log lines found for requested range._"
    | lines ->
      sprintf "```%s```%s" (String.concat "\n" lines)
        (if truncated then sprintf "\n_truncated to first %d requested lines_" buildkite_unfurl_line_cap else "")
  in
  { title = Some title; value; short = false }

let populate_buildkite_build ?job_log (link : Util.Build.buildkite_link) (build : Buildkite_t.get_build_res) =
  let state = string_of_buildkite_state build.state in
  let pipeline_url = sprintf "https://buildkite.com/%s/%s" link.org link.pipeline in
  let title = sprintf "buildkite/%s #%d %s" link.pipeline build.number state in
  let job_lines =
    match link.fragment with
    | None -> []
    | Some { job_id; _ } ->
      (match find_buildkite_job build.jobs job_id with
      | Some job ->
        [
          sprintf "*Step*: <%s|%s> (%s)" job.web_url (escape_mrkdwn job.name)
            (string_of_buildkite_job_state job.state);
        ]
      | None -> [ sprintf "*Step*: `%s`" (escape_mrkdwn job_id) ])
  in
  let text =
    [
      [ sprintf "*Message*: %s" (escape_mrkdwn (Util.first_line build.message)) ];
      [ sprintf "*Branch*: `%s`" (escape_mrkdwn build.branch) ];
      [ sprintf "*Commit*: `%s`" (Slack.git_short_sha_hash build.sha) ];
      job_lines;
    ]
    |> List.concat |> String.concat "\n"
  in
  let fields =
    match link.fragment, job_log with
    | Some { lines = Some lines; _ }, Some job_log -> Some [ buildkite_log_field lines job_log ]
    | _ -> None
  in
  {
    empty_attachment with
    fallback = Some (sprintf "[buildkite/%s] Build #%d %s: %s" link.pipeline build.number state (Util.first_line build.message));
    mrkdwn_in = Some [ "fields"; "text" ];
    color = Some (color_of_buildkite_state build.state);
    title = Some title;
    title_link = Some link.original_url;
    text = Some text;
    fields;
    footer = Some (sprintf "<%s|buildkite/%s>" pipeline_url (escape_mrkdwn link.pipeline));
  }
