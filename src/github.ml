open Httpaf
open Base
open Printf
open Github_j

type t =
  | Push of commit_pushed_notification
  | Pull_request of pr_notification
  | CI_run of ci_build_notification
  | Event of string (* all other events *)

let is_valid_signature ~secret headers_sig body =
  let request_hash =
    let key = Cstruct.of_string secret in
    Cstruct.to_string @@ Nocrypto.Hash.SHA1.hmac ~key (Cstruct.of_string body)
  in
  let (`Hex request_hash) = Hex.of_string request_hash in
  String.equal headers_sig (Printf.sprintf "sha1=%s" request_hash)

let parse_exn ~secret headers body =
  begin match secret with
  | None -> ()
  | Some secret ->
    let req_sig = Headers.get_exn headers "X-Hub-Signature" in
    if not @@ is_valid_signature ~secret req_sig body then failwith "request signature invalid"
  end;
  match Headers.get_exn headers "X-GitHub-Event" with
  | "push" -> Push (commit_pushed_notification_of_string body)
  | "pull_request" -> Pull_request (pr_notification_of_string body)
  | "status" -> CI_run (ci_build_notification_of_string body)
  | ("issue_comment" | "create" | "delete" | "pull_request_review_comment" | "pull_request_review" as event) -> Event event
  | event -> failwith @@ sprintf "unsupported event : %s" event

let get_commits_branch n =
  match String.split ~on:'/' n.ref with
  | "refs"::"heads"::l -> String.concat ~sep:"/" l
  | _ -> n.ref
