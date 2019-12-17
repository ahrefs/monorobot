open Base

module Slack = struct
  open Github_events_notifications_t
  open Notabot_github_j

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
    let { sender; pull_request; _ } = notification in
    let { body; title; url; labels; _ } = pull_request in
    let fields =
      match List.length labels with
      | n when n > 0 ->
        let value = String.concat ~sep:", " (List.map ~f:(fun x -> x.name) labels) in
        [ { title = Some "Labels"; value } ]
      | _ -> []
    in
    string_of_slack_webhook_notification
      {
        text = None;
        attachments =
          Some
            [ {
                empty_attachments with
                fallback = Some "Pull request notification";
                color = Some "#ccc";
                pretext = Some (Printf.sprintf "Pull request opened by %s" sender.login);
                author_name = Some sender.login;
                author_link = Some sender.url;
                author_icon = Some sender.avatar_url;
                title = Some title;
                title_link = Some url;
                text = Some body;
                fields = Some fields;
              }
            ];
        blocks = None;
      }

  let git_short_sha_hash hash = String.sub ~pos:0 ~len:8 (Github_events_notifications_j.string_of_commit_hash hash)

  let generate_push_notification notification =
    let { ref; sender; compare; commits; repository; _ } = notification in
    let ref_tokens = Array.of_list @@ String.split ~on:'/' ref in
    let commit_branch =
      (* take out refs/heads and get the full name of the branch *)
      String.concat ~sep:"/" @@ Array.to_list @@ Array.subo ~pos:2 ref_tokens
    in
    let title =
      Printf.sprintf "*<%s|%i new commit%s> pushed to `<%s|%s>`*" compare (List.length commits)
        ( match commits with
        | [ _ ] -> "s"
        | _ -> "" )
        repository.url commit_branch
    in
    let commits_text_list =
      List.map commits ~f:(fun { url; id; message; _ } ->
          Printf.sprintf "`<%s|%s>` - `%s`" url (git_short_sha_hash id) message)
    in
    string_of_slack_webhook_notification
      {
        text = None;
        attachments =
          Some
            [ {
                empty_attachments with
                mrkdwn_in = Some [ "fields" ];
                fallback = Some "Commit pushed notification";
                color = Some "#ccc";
                author_name = Some sender.login;
                author_link = Some sender.url;
                author_icon = Some sender.avatar_url;
                fields = Some [ { value = String.concat ~sep:"\n" @@ (title :: commits_text_list); title = None } ];
              }
            ];
        blocks = None;
      }

  let generate_ci_run_notification (notification : ci_build_notification) =
    let { commit; state; target_url; description; context; _ } = notification in
    let { commit : inner_commit; url; sha; author } = commit in
    let ({ message; _ } : inner_commit) = commit in
    let title = Printf.sprintf "*<%s|CI Build on %s>*" target_url context in
    let status = Printf.sprintf "*Status*: %s" @@ Github_events_notifications_j.string_of_ci_build_state state in
    let description = Printf.sprintf "*Description*: %s." description in
    let commit_info = Printf.sprintf "*Commit*: `<%s|%s>` - %s" url (git_short_sha_hash sha) message in
    let author_info = Printf.sprintf "*Author*: %s" author.login in
    string_of_slack_webhook_notification
      {
        text = None;
        attachments =
          Some
            [ {
                empty_attachments with
                mrkdwn_in = Some [ "fields"; "text" ];
                fallback = Some "CI run notification";
                color = Some "#ccc";
                fields =
                  Some
                    [ {
                        title = None;
                        value = String.concat ~sep:"\n" @@ [ title; status; description; commit_info; author_info ];
                      }
                    ];
              }
            ];
        blocks = None;
      }

  let is_success_or_failed state =
    match state with
    | Success | Failed -> true
    | _ -> false

  let generate_notification request_notification =
    let open Github_notifications_handler in
    match request_notification with
    | Push notification -> Ok (generate_push_notification notification)
    | Pull_request notification -> Ok (generate_pull_request_notification notification)
    | CI_run n when is_success_or_failed n.state -> Ok (generate_ci_run_notification n)
    | _ -> Error ()

  let send_notification ?(content_type = "application/json") data =
    let slack_url = Lazy.force Configuration.Env.slack_webhook_url in
    let r, c = Configuration.Curl.init_conn slack_url in
    Curl.set_post c true;
    Curl.set_httpheader c [ "Content-Type: " ^ content_type ];
    Curl.set_postfields c data;
    Curl.set_postfieldsize c (String.length data);
    ( try%lwt
        match%lwt Curl_lwt.perform c with
        | Curl.CURLE_OK when not @@ (Curl.get_httpcode c = 200) ->
          Stdio.print_endline (Printf.sprintf "Slack notification status code <> 200: %d" (Curl.get_httpcode c));
          Lwt.return None
        | Curl.CURLE_OK -> Lwt.return (Some (Curl.get_responsecode c, Buffer.contents r))
        | code ->
          Stdio.print_endline @@ Printf.sprintf "Slack notification request errored: %s" (Curl.strerror code);
          Lwt.return None
      with exn ->
        Stdio.print_endline @@ Printf.sprintf "Exn when posting notification to Slack: %s" (Exn.to_string exn);
        Lwt.fail exn )
      [%lwt.finally
        Curl.cleanup c;
        Lwt.return ()]
end
