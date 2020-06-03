open Base
open Devkit
open Printf
open Github_j
open Slack_j

let log = Log.from "slack"

let empty_attachments =
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
  }

let generate_pull_request_notification notification =
  let { action; number; sender; pull_request } = notification in
  let ({ body; title; html_url; _ } : pull_request) = pull_request in
  let action_str =
    match action with
    | Opened -> "opened"
    | Closed -> "closed"
    | Reopened -> "reopened"
    | Labeled -> "labeled"
    | _ ->
      invalid_arg
        (sprintf "Notabot doesn't know how to generate notification for the unexpected event %s"
           (string_of_pr_action action))
  in
  let summary =
    Some (sprintf "Pull request #%d <%s|[%s]> %s by <%s|%s>" number html_url title action_str sender.url sender.login)
  in
  {
    text = None;
    attachments =
      Some [ { empty_attachments with fallback = summary; color = Some "#ccc"; pretext = summary; text = Some body } ];
    blocks = None;
  }

let generate_pr_review_notification notification =
  let { action; sender; pull_request; review } = notification in
  let ({ user; number; title; html_url; _ } : pull_request) = pull_request in
  let action_str =
    match action with
    | Submitted ->
      ( match review.state with
      | "commented" -> "commented on"
      | "approved" -> "approved"
      | "changes_requested" -> "requested changes on"
      | _ -> invalid_arg (sprintf "Error: unexpected review state %s" review.state)
      )
    | _ ->
      invalid_arg
        (sprintf "Notabot doesn't know how to generate notification for the unexpected event %s"
           (string_of_pr_review_action action))
  in
  let summary =
    Some
      (sprintf "<%s|%s> <%s|%s> <%s|%s>'s pull request #%d <%s|[%s]>" sender.url sender.login review.html_url action_str
         user.url user.login number html_url title)
  in
  {
    text = None;
    attachments =
      Some [ { empty_attachments with fallback = summary; color = Some "#ccc"; pretext = summary; text = review.body } ];
    blocks = None;
  }

let generate_pr_review_comment_notification notification =
  let { action; pull_request; sender; comment } = notification in
  let ({ user; number; title; html_url; _ } : pull_request) = pull_request in
  let action_str =
    match action with
    | Created -> "commented"
    | _ ->
      invalid_arg
        (sprintf "Notabot doesn't know how to generate notification for the unexpected event %s"
           (string_of_comment_action action))
  in
  let summary =
    Some
      (sprintf "<%s|%s> %s on <%s|%s>'s pull request #%d <%s|[%s]>" sender.url sender.login action_str user.url
         user.login number html_url title)
  in
  let file =
    match comment.path with
    | None -> None
    | Some a -> Some (sprintf "Comment by %s in %s" sender.login a)
  in
  {
    text = None;
    attachments =
      Some
        [
          {
            empty_attachments with
            fallback = summary;
            color = Some "#ccc";
            pretext = summary;
            title = file;
            title_link = Some comment.html_url;
            text = Some comment.body;
          };
        ];
    blocks = None;
  }

let generate_issue_notification notification =
  let ({ action; sender; issue } : issue_notification) = notification in
  let { number; body; title; html_url; _ } = issue in
  let action_str =
    match action with
    | Opened -> "opened"
    | Closed -> "closed"
    | Reopened -> "reopened"
    | Labeled -> "labeled"
    | _ ->
      invalid_arg
        (sprintf "Notabot doesn't know how to generate notification for the unexpected event %s"
           (string_of_issue_action action))
  in
  let summary =
    Some (sprintf "Issue #%d <%s|[%s]> %s by <%s|%s>" number html_url title action_str sender.url sender.login)
  in
  {
    text = None;
    attachments =
      Some [ { empty_attachments with fallback = summary; color = Some "#ccc"; pretext = summary; text = Some body } ];
    blocks = None;
  }

let generate_issue_comment_notification notification =
  let { action; issue; sender; comment } = notification in
  let { user; number; title; _ } = issue in
  let action_str =
    match action with
    | Created -> "commented"
    | _ ->
      invalid_arg
        (sprintf
           "Notabot doesn't know how to generate pull request review comment notification for the unexpected event %s"
           (string_of_comment_action action))
  in
  let kind =
    match issue.pull_request with
    | Some _ -> sprintf "<%s|%s>'s pull request" user.url user.login
    | None -> "issue"
  in
  let summary =
    Some
      (sprintf "<%s|%s> <%s|%s> on %s #%d <%s|[%s]>" sender.url sender.login comment.html_url action_str kind number
         issue.html_url title)
  in
  {
    text = None;
    attachments =
      Some
        [
          {
            empty_attachments with
            fallback = summary;
            color = Some "#ccc";
            pretext = summary;
            text = Some comment.body;
          };
        ];
    blocks = None;
  }

let git_short_sha_hash hash = String.sub ~pos:0 ~len:8 hash

let generate_push_notification notification =
  let { sender; created; deleted; forced; compare; commits; repository; _ } = notification in
  let commits_branch = Github.get_commits_branch notification in
  let tree_url = String.concat ~sep:"/" [ repository.url; "tree"; Uri.pct_encode commits_branch ] in
  let title =
    if deleted then
      sprintf "<%s|[%s]> <%s|%s> deleted branch <%s|%s>" tree_url repository.name sender.url sender.login compare
        commits_branch
    else
      sprintf "<%s|[%s:%s]> <%s|%i commit%s> %spushed %sby <%s|%s>" tree_url repository.name commits_branch compare
        (List.length commits)
        ( match commits with
        | [ _ ] -> ""
        | _ -> "s"
        )
        (if forced then "force-" else "")
        (if created then "to new branch " else "")
        sender.url sender.login
  in
  let commits =
    List.map commits ~f:(fun { url; id; message; author; _ } ->
      let title = Option.value ~default:"" @@ List.hd @@ String.split_lines message in
      sprintf "`<%s|%s>` %s - %s" url (git_short_sha_hash id) title author.name)
  in
  {
    text = Some title;
    attachments =
      Some
        [
          {
            empty_attachments with
            mrkdwn_in = Some [ "fields" ];
            fallback = Some "Commit pushed notification";
            color = Some "#ccc";
            fields = Some [ { value = String.concat ~sep:"\n" commits; title = None } ];
          };
        ];
    blocks = None;
  }

let generate_ci_run_notification (notification : ci_build_notification) =
  let { commit; state; target_url; description; context; _ } = notification in
  let { commit : inner_commit; url; sha; author } = commit in
  let ({ message; _ } : inner_commit) = commit in
  let title = sprintf "*<%s|CI Build on %s>*" target_url context in
  let status = sprintf "*Status*: %s" @@ string_of_ci_build_state state in
  let description = sprintf "*Description*: %s." description in
  let commit_info = sprintf "*Commit*: `<%s|%s>` - %s" url (git_short_sha_hash sha) message in
  let author_info = sprintf "*Author*: %s" author.login in
  {
    text = None;
    attachments =
      Some
        [
          {
            empty_attachments with
            mrkdwn_in = Some [ "fields"; "text" ];
            fallback = Some "CI run notification";
            color = Some "#ccc";
            fields =
              Some
                [
                  {
                    title = None;
                    value = String.concat ~sep:"\n" @@ [ title; status; description; commit_info; author_info ];
                  };
                ];
          };
        ];
    blocks = None;
  }

let send_notification webhook_url data =
  let data = Slack_j.string_of_webhook_notification data in
  let body = `Raw ("application/json", data) in
  match%lwt Web.http_request_lwt ~verbose:true ~body `POST webhook_url with
  | `Ok _ -> Lwt.return_unit
  | `Error e ->
    log#error "error when posting notification to slack: %s" e;
    Exn_lwt.fail "unable to send notification to slack"
