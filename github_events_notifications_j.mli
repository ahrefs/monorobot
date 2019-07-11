(* Auto-generated from "github_events_notifications.atd" *)
[@@@ocaml.warning "-27-32-35-39"]

type user = Github_events_notifications_t.user = {
  login : string;
  id : int;
}

type pull_request = Github_events_notifications_t.pull_request = {
  id : int;
  body : string;
  title : string;
  url : string;
}

type pr_action = Github_events_notifications_t.pr_action

type pr_notification = Github_events_notifications_t.pr_notification = {
  action : pr_action;
  number : int;
  sender : user;
  pull_request : pull_request;
}

type author = Github_events_notifications_t.author = {
  name : string;
  email : string;
  username : string option;
}

type inner_commit = Github_events_notifications_t.inner_commit = {
  author : author;
  committer : author;
  message : string;
}

type commit_hash = Github_events_notifications_t.commit_hash

type commit = Github_events_notifications_t.commit = {
  id : commit_hash;
  message : string;
  timestamp : string;
  url : string;
  author : author;
  committer : author;
}

type commit_pushed_notification = Github_events_notifications_t.commit_pushed_notification = {
  ref : string;
  after : commit_hash;
  commits : commit list;
  head_commit : commit;
  pusher : author;
  sender : user;
}

type ci_commit = Github_events_notifications_t.ci_commit = {
  sha : commit_hash;
  commit : inner_commit;
  url (*atd html_url *) : string;
}

type ci_build_state = Github_events_notifications_t.ci_build_state

type branch = Github_events_notifications_t.branch = { name : string }

type ci_build_notification = Github_events_notifications_t.ci_build_notification = {
  commit : ci_commit;
  state : ci_build_state;
  target_url : string;
  branches : branch list;
}

(** Output a JSON value of type {!user}. *)
val write_user : Bi_outbuf.t -> user -> unit

(** Serialize a value of type {!user}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)
val string_of_user : ?len:int -> user -> string

(** Input JSON data of type {!user}. *)
val read_user : Yojson.Safe.lexer_state -> Lexing.lexbuf -> user

(** Deserialize JSON data of type {!user}. *)
val user_of_string : string -> user

(** Output a JSON value of type {!pull_request}. *)
val write_pull_request : Bi_outbuf.t -> pull_request -> unit

(** Serialize a value of type {!pull_request}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)
val string_of_pull_request : ?len:int -> pull_request -> string

(** Input JSON data of type {!pull_request}. *)
val read_pull_request : Yojson.Safe.lexer_state -> Lexing.lexbuf -> pull_request

(** Deserialize JSON data of type {!pull_request}. *)
val pull_request_of_string : string -> pull_request

(** Output a JSON value of type {!pr_action}. *)
val write_pr_action : Bi_outbuf.t -> pr_action -> unit

(** Serialize a value of type {!pr_action}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)
val string_of_pr_action : ?len:int -> pr_action -> string

(** Input JSON data of type {!pr_action}. *)
val read_pr_action : Yojson.Safe.lexer_state -> Lexing.lexbuf -> pr_action

(** Deserialize JSON data of type {!pr_action}. *)
val pr_action_of_string : string -> pr_action

(** Output a JSON value of type {!pr_notification}. *)
val write_pr_notification : Bi_outbuf.t -> pr_notification -> unit

(** Serialize a value of type {!pr_notification}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)
val string_of_pr_notification : ?len:int -> pr_notification -> string

(** Input JSON data of type {!pr_notification}. *)
val read_pr_notification : Yojson.Safe.lexer_state -> Lexing.lexbuf -> pr_notification

(** Deserialize JSON data of type {!pr_notification}. *)
val pr_notification_of_string : string -> pr_notification

(** Output a JSON value of type {!author}. *)
val write_author : Bi_outbuf.t -> author -> unit

(** Serialize a value of type {!author}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)
val string_of_author : ?len:int -> author -> string

(** Input JSON data of type {!author}. *)
val read_author : Yojson.Safe.lexer_state -> Lexing.lexbuf -> author

(** Deserialize JSON data of type {!author}. *)
val author_of_string : string -> author

(** Output a JSON value of type {!inner_commit}. *)
val write_inner_commit : Bi_outbuf.t -> inner_commit -> unit

(** Serialize a value of type {!inner_commit}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)
val string_of_inner_commit : ?len:int -> inner_commit -> string

(** Input JSON data of type {!inner_commit}. *)
val read_inner_commit : Yojson.Safe.lexer_state -> Lexing.lexbuf -> inner_commit

(** Deserialize JSON data of type {!inner_commit}. *)
val inner_commit_of_string : string -> inner_commit

(** Output a JSON value of type {!commit_hash}. *)
val write_commit_hash : Bi_outbuf.t -> commit_hash -> unit

(** Serialize a value of type {!commit_hash}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)
val string_of_commit_hash : ?len:int -> commit_hash -> string

(** Input JSON data of type {!commit_hash}. *)
val read_commit_hash : Yojson.Safe.lexer_state -> Lexing.lexbuf -> commit_hash

(** Deserialize JSON data of type {!commit_hash}. *)
val commit_hash_of_string : string -> commit_hash

(** Output a JSON value of type {!commit}. *)
val write_commit : Bi_outbuf.t -> commit -> unit

(** Serialize a value of type {!commit}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)
val string_of_commit : ?len:int -> commit -> string

(** Input JSON data of type {!commit}. *)
val read_commit : Yojson.Safe.lexer_state -> Lexing.lexbuf -> commit

(** Deserialize JSON data of type {!commit}. *)
val commit_of_string : string -> commit

(** Output a JSON value of type {!commit_pushed_notification}. *)
val write_commit_pushed_notification : Bi_outbuf.t -> commit_pushed_notification -> unit

(** Serialize a value of type {!commit_pushed_notification}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)
val string_of_commit_pushed_notification : ?len:int -> commit_pushed_notification -> string

(** Input JSON data of type {!commit_pushed_notification}. *)
val read_commit_pushed_notification : Yojson.Safe.lexer_state -> Lexing.lexbuf -> commit_pushed_notification

(** Deserialize JSON data of type {!commit_pushed_notification}. *)
val commit_pushed_notification_of_string : string -> commit_pushed_notification

(** Output a JSON value of type {!ci_commit}. *)
val write_ci_commit : Bi_outbuf.t -> ci_commit -> unit

(** Serialize a value of type {!ci_commit}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)
val string_of_ci_commit : ?len:int -> ci_commit -> string

(** Input JSON data of type {!ci_commit}. *)
val read_ci_commit : Yojson.Safe.lexer_state -> Lexing.lexbuf -> ci_commit

(** Deserialize JSON data of type {!ci_commit}. *)
val ci_commit_of_string : string -> ci_commit

(** Output a JSON value of type {!ci_build_state}. *)
val write_ci_build_state : Bi_outbuf.t -> ci_build_state -> unit

(** Serialize a value of type {!ci_build_state}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)
val string_of_ci_build_state : ?len:int -> ci_build_state -> string

(** Input JSON data of type {!ci_build_state}. *)
val read_ci_build_state : Yojson.Safe.lexer_state -> Lexing.lexbuf -> ci_build_state

(** Deserialize JSON data of type {!ci_build_state}. *)
val ci_build_state_of_string : string -> ci_build_state

(** Output a JSON value of type {!branch}. *)
val write_branch : Bi_outbuf.t -> branch -> unit

(** Serialize a value of type {!branch}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)
val string_of_branch : ?len:int -> branch -> string

(** Input JSON data of type {!branch}. *)
val read_branch : Yojson.Safe.lexer_state -> Lexing.lexbuf -> branch

(** Deserialize JSON data of type {!branch}. *)
val branch_of_string : string -> branch

(** Output a JSON value of type {!ci_build_notification}. *)
val write_ci_build_notification : Bi_outbuf.t -> ci_build_notification -> unit

(** Serialize a value of type {!ci_build_notification}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)
val string_of_ci_build_notification : ?len:int -> ci_build_notification -> string

(** Input JSON data of type {!ci_build_notification}. *)
val read_ci_build_notification : Yojson.Safe.lexer_state -> Lexing.lexbuf -> ci_build_notification

(** Deserialize JSON data of type {!ci_build_notification}. *)
val ci_build_notification_of_string : string -> ci_build_notification
