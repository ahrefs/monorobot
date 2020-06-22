open Printf
open Base
open Devkit
open Common
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
    footer = None;
  }

let generate_pull_request_notification notification =
  let { action; number; sender; pull_request; repository } = notification in
  let ({ body; title; html_url; labels; _ } : pull_request) = pull_request in
  let label_str =
    match labels with
    | [] -> None
    | labels ->
      let value = String.concat ~sep:", " (List.map ~f:(fun x -> x.name) labels) in
      Some (sprintf "Labels: %s" value)
  in
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
    Some
      (sprintf "<%s|[%s]> Pull request #%d <%s|%s> %s by %s" repository.url repository.full_name number html_url title
         action_str sender.login)
  in
  {
    text = None;
    attachments =
      Some
        [
          {
            empty_attachments with
            mrkdwn_in = Some [ "text" ];
            fallback = summary;
            color = Some "#ccc";
            pretext = summary;
            text =
              ( match action with
              | Labeled -> label_str
              | Closed -> None
              | _ -> Some body
              );
          };
        ];
    blocks = None;
  }

let generate_pr_review_notification notification =
  let { action; sender; pull_request; review; repository } = notification in
  let ({ number; title; html_url; _ } : pull_request) = pull_request in
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
      (sprintf "<%s|[%s]> %s <%s|%s> #%d <%s|%s>" repository.url repository.full_name sender.login
         review.html_url action_str number html_url title)
  in
  {
    text = None;
    attachments =
      Some
        [
          {
            empty_attachments with
            mrkdwn_in = Some [ "text" ];
            fallback = summary;
            color = Some "#ccc";
            pretext = summary;
            text = review.body;
          };
        ];
    blocks = None;
  }

let generate_pr_review_comment_notification notification =
  let { action; pull_request; sender; comment; repository } = notification in
  let ({ number; title; html_url; _ } : pull_request) = pull_request in
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
      (sprintf "<%s|[%s]> %s %s on #%d <%s|%s>" repository.url repository.full_name sender.login
         action_str number html_url title)
  in
  let file =
    match comment.path with
    | None -> None
    | Some a -> Some (sprintf "New comment by %s in <%s|%s>" sender.login comment.html_url a)
  in
  {
    text = None;
    attachments =
      Some
        [
          {
            empty_attachments with
            mrkdwn_in = Some [ "text" ];
            fallback = summary;
            color = Some "#ccc";
            pretext = summary;
            footer = file;
            text = Some comment.body;
          };
        ];
    blocks = None;
  }

let generate_issue_notification notification =
  let ({ action; sender; issue; repository } : issue_notification) = notification in
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
    Some
      (sprintf "<%s|[%s]> Issue #%d <%s|%s> %s by %s" repository.url repository.full_name number html_url title
         action_str sender.login)
  in
  {
    text = None;
    attachments =
      Some
        [
          {
            empty_attachments with
            mrkdwn_in = Some [ "text" ];
            fallback = summary;
            color = Some "#ccc";
            pretext = summary;
            text = Some body;
          };
        ];
    blocks = None;
  }

let generate_issue_comment_notification notification =
  let { action; issue; sender; comment; repository } = notification in
  let { number; title; _ } = issue in
  let action_str =
    match action with
    | Created -> "commented"
    | _ ->
      invalid_arg
        (sprintf
           "Notabot doesn't know how to generate pull request review comment notification for the unexpected event %s"
           (string_of_comment_action action))
  in
  let summary =
    Some
      (sprintf "<%s|[%s]> %s <%s|%s> on #%d <%s|%s>" repository.url repository.full_name sender.login
         comment.html_url action_str number issue.html_url title)
  in
  {
    text = None;
    attachments =
      Some
        [
          {
            empty_attachments with
            mrkdwn_in = Some [ "text" ];
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
      sprintf "<%s|[%s]> %s deleted branch <%s|%s>" tree_url repository.name sender.login compare commits_branch
    else
      sprintf "<%s|[%s:%s]> <%s|%i commit%s> %spushed %sby %s" tree_url repository.name commits_branch compare
        (List.length commits)
        ( match commits with
        | [ _ ] -> ""
        | _ -> "s"
        )
        (if forced then "force-" else "")
        (if created then "to new branch " else "")
        sender.login
  in
  let commits =
    List.map commits ~f:(fun { url; id; message; author; _ } ->
      let title = first_line message in
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

let generate_status_notification (notification : status_notification) =
  let { commit; state; description; target_url; context; repository; _ } = notification in
  let ({ commit : inner_commit; sha; author; html_url; _ } : status_commit) = commit in
  let ({ message; _ } : inner_commit) = commit in
  let state_info =
    match state with
    | Success -> "success"
    | Failure -> "failure"
    | Error -> "error"
    | _ ->
      invalid_arg
        (sprintf "Notabot doesn't know how to generate notification for the unexpected event %s"
           (string_of_status_state state))
  in
  let color_info =
    match state with
    | Success -> "good"
    | _ -> "danger"
  in
  let description_info =
    match description with
    | None -> None
    | Some s -> Some (sprintf "*Description*: %s." s)
  in
  let commit_info = sprintf "*Commit*: `<%s|%s>` %s - %s" html_url (git_short_sha_hash sha) (first_line message) author.login in
  let branches_info =
    let branches = notification.branches |> List.map ~f:(fun ({ name } : branch) -> name) |> String.concat ~sep:", " in
    sprintf "*Branches*: %s" branches
  in
  let summary =
    match target_url with
    | None ->
      Some (sprintf "<%s|[%s]> CI Build Status notification: %s" repository.url repository.full_name state_info)
      (* in case the CI run is not using buildkite *)
    | Some t ->
      Some
        (sprintf "<%s|[%s]> CI Build Status notification for <%s|%s>: %s" repository.url repository.full_name t context
           state_info)
  in
  {
    text = None;
    attachments =
      Some
        [
          {
            empty_attachments with
            mrkdwn_in = Some [ "fields"; "text" ];
            fallback = summary;
            pretext = summary;
            color = Some color_info;
            text = description_info;
            fields =
              Some [ { title = None; value = String.concat ~sep:"\n" @@ [ commit_info; branches_info ] } ];
          };
        ];
    blocks = None;
  }

let generate_commit_comment_notification cfg notification =
  match%lwt Github.generate_commit_from_commit_comment cfg notification with
  | None -> invalid_arg "no commits found"
  | Some api_commit ->
    let { commit; files; _ } = api_commit in
    let { sender; comment; repository; _ } = notification in
    let commit_id =
      match comment.commit_id with
      | None -> invalid_arg "commit id not found"
      | Some c -> c
    in
    let summary =
      Some
        (sprintf "<%s|[%s]> %s commented on `<%s|%s>` %s" repository.url repository.full_name sender.login
           comment.html_url (git_short_sha_hash commit_id) (first_line commit.message))
    in
    let path =
      match comment.path with
      | None ->
        ( match files with
        | [ file ] -> Some (sprintf "New comment by %s in <%s|%s>" sender.login file.url file.filename)
        | _ -> None
        )
      | Some p -> Some (sprintf "New comment by %s in <%s|%s>" sender.login comment.html_url p)
    in
    let notifs =
      {
        text = None;
        attachments =
          Some
            [
              {
                empty_attachments with
                mrkdwn_in = Some [ "pretext"; "text" ];
                fallback = summary;
                color = Some "#ccc";
                pretext = summary;
                footer = path;
                text = Some comment.body;
              };
            ];
        blocks = None;
      }
    in
    Lwt.return notifs

let send_notification webhook_url data =
  let data = Slack_j.string_of_webhook_notification data in
  let body = `Raw ("application/json", data) in
  match%lwt Web.http_request_lwt ~verbose:true ~body `POST webhook_url with
  | `Ok _ -> Lwt.return_unit
  | `Error e ->
    log#error "error when posting notification to slack: %s" e;
    Exn_lwt.fail "unable to send notification to slack"
