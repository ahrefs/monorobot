(* Auto-generated from "events_notifications.atd" *)
[@@@ocaml.warning "-27-32-35-39"]

type user = Events_notifications_t.user = {
  user_login (*atd login *) : string;
  user_id (*atd id *) : int;
}

type pull_request = Events_notifications_t.pull_request = {
  pull_request_id (*atd id *) : int;
  pull_request_body (*atd body *) : string;
  pull_request_title (*atd title *) : string;
  pull_request_url (*atd url *) : string;
}

type pr_notification = Events_notifications_t.pr_notification = {
  action : string;
  number : int;
  sender : user;
  pull_request : pull_request;
}

type author = Events_notifications_t.author = {
  name : string;
  email : string;
  username : string option;
}

type commit = Events_notifications_t.commit = {
  commit_id (*atd id *) : string;
  commit_message (*atd message *) : string;
  commit_timestamp (*atd timestamp *) : string;
  commit_url (*atd url *) : string;
  commit_author (*atd author *) : author;
  commit_committer (*atd committer *) : author;
}

type commit_pushed_notification = Events_notifications_t.commit_pushed_notification = {
  ref : string;
  after : string;
  commits : commit list;
  head_commit : commit;
  pusher : author;
  sender : user;
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
