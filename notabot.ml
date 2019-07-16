open Base

module Github = struct
  open Github_events_notifications_t
  open Notabot_github_t

  (* Going down the separation route as i'm not sure slack and console notifications
     structure will be equal  *)
  module Console = struct
    type console_notification =
      | Pull_request of pr_review_requested
      | Push of commit_pushed
      | CI_run of ci_build_status_changed

    let generate_pull_request_notification (notification : pr_notification) =
      let { sender; pull_request; _ } = notification in
      let { body; title; url; _ } = pull_request in
      let notif : Notabot_github_t.pr_review_requested =
        { author = sender.login; title; description = body; link = url }
      in
      Ok (Pull_request notif)

    let generate_push_notification notification =
      let { pusher; after; head_commit; _ } = notification in
      let ({ message; url; _ } : Github_events_notifications_t.commit) = head_commit in
      let notif = { author = pusher; hash = after; commit_message = message; link = url } in
      Ok (Push notif)

    let generate_ci_run_notification (notification : ci_build_notification) =
      let { commit; state; branches; target_url } = notification in
      let { commit : inner_commit; url; sha } = commit in
      let ({ author; message; _ } : inner_commit) = commit in
      let ({ name; _ } : Github_events_notifications_t.branch) = List.hd_exn branches in
      let notif : Notabot_github_t.ci_build_status_changed =
        { commit = { sha; message; url; author }; build_status = state; branch = name; link = target_url }
      in
      Ok (CI_run notif)

    let is_review_requested action =
      match action with
      | Review_requested -> true
      | _ -> false

    let is_success_or_failed state =
      match state with
      | Success | Failed -> true
      | _ -> false

    let generate_notification request_notification =
      let open Github_notifications_handler in
      match request_notification with
      | Push notification -> generate_push_notification notification
      | Pull_request n when is_review_requested n.action -> generate_pull_request_notification n
      | CI_run n when is_success_or_failed n.state -> generate_ci_run_notification n
      | Pull_request n ->
        Error
          (Printf.sprintf "Unsupported pull request action: %s"
             (Github_events_notifications_j.string_of_pr_action n.action))
      | CI_run n ->
        Error
          (Printf.sprintf "Unsupported CI run state: %s"
             (Github_events_notifications_j.string_of_ci_build_state n.state))

    let serialize_notification notification =
      match notification with
      | Pull_request pr_review_requested -> Notabot_github_j.string_of_pr_review_requested pr_review_requested
      | Push commit_pushed -> Notabot_github_j.string_of_commit_pushed commit_pushed
      | CI_run ci_build_status_changed -> Notabot_github_j.string_of_ci_build_status_changed ci_build_status_changed
  end

  module Slack = struct (* ... *)  end
end
