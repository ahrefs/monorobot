open Base
open Printf
open Github_j
open Slack_j

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
  let { action; sender; pull_request; _ } = notification in
  let { body; title; url; labels; _ } = pull_request in
  let fields =
    match List.length labels with
    | n when n > 0 ->
      let value = String.concat ~sep:", " (List.map ~f:(fun x -> x.name) labels) in
      [ { title = Some "Labels"; value } ]
    | _ -> []
  in
  {
    text = None;
    attachments =
      Some
        [
          {
            empty_attachments with
            fallback = Some "Pull request notification";
            color = Some "#ccc";
            pretext =
              Some
                (sprintf "Pull request %s %s %s"
                   (String.filter (string_of_pr_action action) ~f:(fun c -> Char.to_int c > 96 && Char.to_int c < 123))
                   "by" sender.login);
            author_name = Some sender.login;
            author_link = Some sender.url;
            author_icon = Some sender.avatar_url;
            title = Some title;
            title_link = Some url;
            text = Some body;
            fields = Some fields;
          };
        ];
    blocks = None;
  }

let generate_pr_review_comment_notification notification =
  let { pull_request; sender; comment; _ } = notification in
  let { body; url; _ } = comment in
  let fields =
    match List.length pull_request.labels with
    | n when n > 0 ->
      let value = String.concat ~sep:", " (List.map ~f:(fun x -> x.name) pull_request.labels) in
      [ { title = Some "Labels"; value } ]
    | _ -> []
  in
  {
    text = None;
    attachments =
      Some
        [
          {
            empty_attachments with
            fallback = Some "Pull Request Review Comment notification";
            color = Some "#ccc";
            pretext = Some (sprintf "PR Review Comment created by %s" sender.login);
            author_name = Some sender.login;
            author_link = Some sender.url;
            author_icon = Some sender.avatar_url;
            title_link = Some url;
            text = Some body;
            fields = Some fields;
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

let () = Curl.(global_init CURLINIT_GLOBALALL)

let curl_init url =
  let open Curl in
  let r = Buffer.create 1024 in
  let c = init () in
  set_timeout c 10;
  set_sslverifypeer c true;
  set_sslverifyhost c SSLVERIFYHOST_HOSTNAME;
  set_writefunction c (fun s ->
    Buffer.add_string r s;
    String.length s);
  set_tcpnodelay c true;
  set_verbose c false;
  set_url c url;
  r, c

let send_notification ?(content_type = "application/json") webhook data =
  let data = Slack_j.string_of_webhook_notification data in
  let r, c = curl_init webhook.Notabot_t.url in
  Curl.set_post c true;
  Curl.set_httpheader c [ "Content-Type: " ^ content_type ];
  Curl.set_postfields c data;
  Curl.set_postfieldsize c (String.length data);
  ( try%lwt
      match%lwt Curl_lwt.perform c with
      | Curl.CURLE_OK when not @@ (Curl.get_httpcode c = 200) ->
        Log.line "Slack notification status code <> 200: %d" (Curl.get_httpcode c);
        Lwt.return None
      | Curl.CURLE_OK -> Lwt.return (Some (Curl.get_responsecode c, Buffer.contents r))
      | code ->
        Log.line "Slack notification request errored: %s" (Curl.strerror code);
        Lwt.return None
    with exn ->
      Log.line "Exn when posting notification to Slack: %s" (Exn.to_string exn);
      Lwt.fail exn
  )
    [%lwt.finally
      Curl.cleanup c;
      Lwt.return ()]
