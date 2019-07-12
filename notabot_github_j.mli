(* Auto-generated from "notabot_github.atd" *)
[@@@ocaml.warning "-27-32-35-39"]

type user = Notabot_github_t.user = {
  login : string;
  id : int;
}

type success_status = Notabot_github_t.success_status =
  | Success
  | Failed

type pr_review_requested = Notabot_github_t.pr_review_requested = {
  author : string;
  title : string;
  description : string;
  link : string;
}

type commit_hash = Notabot_github_t.commit_hash

type author = Notabot_github_t.author = {
  name : string;
  email : string;
}

type commit_pushed = Notabot_github_t.commit_pushed = {
  author : author;
  hash : commit_hash;
  commit_message : string;
  link : string;
}

type commit = Notabot_github_t.commit = {
  sha : commit_hash;
  message : string;
  url : string;
  author : author;
}

type ci_build_status_changed = Notabot_github_t.ci_build_status_changed = {
  commit : commit;
  build_status : success_status;
  branch : string;
  link : string;
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

(** Output a JSON value of type {!success_status}. *)
val write_success_status : Bi_outbuf.t -> success_status -> unit

(** Serialize a value of type {!success_status}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)
val string_of_success_status : ?len:int -> success_status -> string

(** Input JSON data of type {!success_status}. *)
val read_success_status : Yojson.Safe.lexer_state -> Lexing.lexbuf -> success_status

(** Deserialize JSON data of type {!success_status}. *)
val success_status_of_string : string -> success_status

(** Output a JSON value of type {!pr_review_requested}. *)
val write_pr_review_requested : Bi_outbuf.t -> pr_review_requested -> unit

(** Serialize a value of type {!pr_review_requested}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)
val string_of_pr_review_requested : ?len:int -> pr_review_requested -> string

(** Input JSON data of type {!pr_review_requested}. *)
val read_pr_review_requested : Yojson.Safe.lexer_state -> Lexing.lexbuf -> pr_review_requested

(** Deserialize JSON data of type {!pr_review_requested}. *)
val pr_review_requested_of_string : string -> pr_review_requested

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

(** Output a JSON value of type {!commit_pushed}. *)
val write_commit_pushed : Bi_outbuf.t -> commit_pushed -> unit

(** Serialize a value of type {!commit_pushed}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)
val string_of_commit_pushed : ?len:int -> commit_pushed -> string

(** Input JSON data of type {!commit_pushed}. *)
val read_commit_pushed : Yojson.Safe.lexer_state -> Lexing.lexbuf -> commit_pushed

(** Deserialize JSON data of type {!commit_pushed}. *)
val commit_pushed_of_string : string -> commit_pushed

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

(** Output a JSON value of type {!ci_build_status_changed}. *)
val write_ci_build_status_changed : Bi_outbuf.t -> ci_build_status_changed -> unit

(** Serialize a value of type {!ci_build_status_changed}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)
val string_of_ci_build_status_changed : ?len:int -> ci_build_status_changed -> string

(** Input JSON data of type {!ci_build_status_changed}. *)
val read_ci_build_status_changed : Yojson.Safe.lexer_state -> Lexing.lexbuf -> ci_build_status_changed

(** Deserialize JSON data of type {!ci_build_status_changed}. *)
val ci_build_status_changed_of_string : string -> ci_build_status_changed
