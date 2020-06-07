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
  | CI_run of ci_build_notification
  | Commit_comment of commit_comment_notification
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
  | "status" -> CI_run (ci_build_notification_of_string body)
  | "commit_comment" -> Commit_comment (commit_comment_notification_of_string body)
  | ("member" | "create" | "delete" | "release") as event -> Event event
  | event -> failwith @@ sprintf "unsupported event : %s" event

let get_commits_branch n =
  match String.split ~on:'/' n.ref with
  | "refs" :: "heads" :: l -> String.concat ~sep:"/" l
  | _ -> n.ref
