open Base
open Devkit
open Printf
open Github_j

let log = Log.from "github"

type t =
  | Push of commit_pushed_notification
  | Pull_request of pr_notification
  | PR_review of pr_review_notification
  | PR_review_comment of pr_review_comment_notification
  | Issue of issue_notification
  | Issue_comment of issue_comment_notification
  | Status of status_notification
  | Event of string

(* all other events *)

let is_valid_signature ~secret headers_sig body =
  let request_hash =
    let key = Cstruct.of_string secret in
    Cstruct.to_string @@ Nocrypto.Hash.SHA1.hmac ~key (Cstruct.of_string body)
  in
  let (`Hex request_hash) = Hex.of_string request_hash in
  String.equal headers_sig (Printf.sprintf "sha1=%s" request_hash)

(* Parse a payload. The type of the payload is detected from the headers. *)
let parse_exn ~secret headers body =
  begin
    match secret with
    | None -> ()
    | Some secret ->
    match List.Assoc.find headers "x-hub-signature" ~equal:String.equal with
    | None -> Exn.fail "unable to find header x-hub-signature"
    | Some req_sig -> if not @@ is_valid_signature ~secret req_sig body then failwith "request signature invalid"
  end;
  match List.Assoc.find_exn headers "x-github-event" ~equal:String.equal with
  | exception exn -> Exn.fail ~exn "unable to read x-github-event"
  | "push" -> Push (commit_pushed_notification_of_string body)
  | "pull_request" -> Pull_request (pr_notification_of_string body)
  | "pull_request_review" -> PR_review (pr_review_notification_of_string body)
  | "pull_request_review_comment" -> PR_review_comment (pr_review_comment_notification_of_string body)
  | "issues" -> Issue (issue_notification_of_string body)
  | "issue_comment" -> Issue_comment (issue_comment_notification_of_string body)
  | "status" -> Status (status_notification_of_string body)
  | ("commit_comment" | "member" | "create" | "delete" | "release") as event -> Event event
  | event -> failwith @@ sprintf "unsupported event : %s" event

let get_commits_branch n =
  match String.split ~on:'/' n.ref with
  | "refs" :: "heads" :: l -> String.concat ~sep:"/" l
  | _ -> n.ref

let query_github_api ~token ~url parse =
  let headers = [ sprintf "Authorization: token %s" token ] in
  match%lwt Web.http_request_lwt ~verbose:true ~headers `GET url with
  | `Error e ->
    log#error "error while querying github api %s: %s" url e;
    Lwt.return_none
  | `Ok s ->
  try Lwt.return_some (parse s)
  with exn ->
    log#error ~exn "impossible to parse github api answer to %s" url;
    Lwt.return_none

let generate_query_commmit cfg (n : status_notification) =
  (* the expected output is a payload containing content about commits *)
  let mytoken = "some_token" in
  match cfg.Config.offline with
  | None -> query_github_api ~token:mytoken ~url:n.commit.url query_commit_of_string
  | Some path ->
    let f = Caml.Filename.concat path n.commit.sha in
    ( match Caml.Sys.file_exists f with
    | false ->
      log#error "unable to find offline file %s" f;
      Lwt.return_none
    | true ->
      Stdio.In_channel.with_file f ~f:(fun ic ->
        try
          let content = Stdio.In_channel.input_all ic in
          Lwt.return_some (query_commit_of_string content)
        with exn ->
          log#error ~exn "unable to read offline file %s" f;
          Lwt.return_none)
    )
