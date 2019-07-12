(* Auto-generated from "notabot_github.atd" *)
[@@@ocaml.warning "-27-32-35-39"]

type user = {
  login : string;
  id : int;
}

type success_status =
  | Success
  | Failed

type pr_review_requested = {
  author : string;
  title : string;
  description : string;
  link : string;
}

type commit_hash = Sha1.t

type author = {
  name : string;
  email : string;
}

type commit_pushed = {
  author : author;
  hash : commit_hash;
  commit_message : string;
  link : string;
}

type commit = {
  sha : commit_hash;
  message : string;
  url : string;
  author : author;
}

type ci_build_status_changed = {
  commit : commit;
  build_status : success_status;
  branch : string;
  link : string;
}
