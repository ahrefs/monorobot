(* Auto-generated from "events_notifications.atd" *)
[@@@ocaml.warning "-27-32-35-39"]

type user = {
  user_login (*atd login *) : string;
  user_id (*atd id *) : int;
}

type pull_request = {
  pull_request_id (*atd id *) : int;
  pull_request_body (*atd body *) : string;
  pull_request_title (*atd title *) : string;
  pull_request_url (*atd url *) : string;
}

type pr_notification = {
  action : string;
  number : int;
  sender : user;
  pull_request : pull_request;
}

type author = {
  name : string;
  email : string;
  username : string option;
}

type commit = {
  commit_id (*atd id *) : string;
  commit_message (*atd message *) : string;
  commit_timestamp (*atd timestamp *) : string;
  commit_url (*atd url *) : string;
  commit_author (*atd author *) : author;
  commit_committer (*atd committer *) : author;
}

type commit_pushed_notification = {
  ref : string;
  after : string;
  commits : commit list;
  head_commit : commit;
  pusher : author;
  sender : user;
}
