(* Auto-generated from "github_events_notifications.atd" *)
[@@@ocaml.warning "-27-32-35-39"]

type user = {
  login : string;
  id : int;
}

type pull_request = {
  id : int;
  body : string;
  title : string;
  url : string;
}

type pr_action =
  [ `Assigned
  | `Unassigned
  | `Review_requested
  | `Review_request_removed
  | `Labeled
  | `Unlabeled
  | `Opened
  | `Edited
  | `Closed
  | `Ready_for_review
  | `Locked
  | `Unlocked
  | `Reopened
  ]

type pr_notification = {
  action : pr_action;
  number : int;
  sender : user;
  pull_request : pull_request;
}

type author = {
  name : string;
  email : string;
  username : string option;
}

type inner_commit = {
  author : author;
  committer : author;
  message : string;
}

type commit_hash = Sha1.t

type commit = {
  id : commit_hash;
  message : string;
  timestamp : string;
  url : string;
  author : author;
  committer : author;
}

type commit_pushed_notification = {
  ref : string;
  after : commit_hash;
  commits : commit list;
  head_commit : commit;
  pusher : author;
  sender : user;
}

type ci_commit = {
  sha : commit_hash;
  commit : inner_commit;
  url (*atd html_url *) : string;
}

type ci_build_state =
  [ `Success
  | `Failure
  | `Neutral
  | `Cancelled
  | `Timed_out
  | `Action_required
  ]

type branch = { name : string }

type ci_build_notification = {
  commit : ci_commit;
  state : ci_build_state;
  target_url : string;
  branches : branch list;
}
