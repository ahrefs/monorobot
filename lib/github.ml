open Base
open Devkit
open Printf
open Github_j

type t =
  | Push of commit_pushed_notification
  | Pull_request of pr_notification
  | PR_review of pr_review_notification
  | PR_review_comment of pr_review_comment_notification
  | Issue of issue_notification
  | Issue_comment of issue_comment_notification
  | Commit_comment of commit_comment_notification
  | Status of status_notification
  (* all other events *)
  | Event of event_notification

let repo_of_notification = function
  | Push n -> n.repository
  | Pull_request n -> n.repository
  | PR_review n -> n.repository
  | PR_review_comment n -> n.repository
  | Issue n -> n.repository
  | Issue_comment n -> n.repository
  | Commit_comment n -> n.repository
  | Status n -> n.repository
  | Event n -> n.repository

let commits_branch_of_ref ref =
  match String.split ~on:'/' ref with
  | "refs" :: "heads" :: l -> String.concat ~sep:"/" l
  | _ -> ref

let event_of_filename filename =
  match String.split_on_chars ~on:[ '.' ] filename with
  | [ kind; _; "json" ] -> Some kind
  | _ -> None

let is_main_merge_message ~msg:message ~branch (cfg : Config.t) =
  match cfg.main_branch_name with
  | Some main_branch when String.equal branch main_branch ->
    (*
      handle "Merge <main branch> into <feature branch>" commits when they are merged into main branch
      we should have already seen these commits on the feature branch but for some reason they are distinct:true
    *)
    let prefix = sprintf "Merge branch '%s' into " main_branch in
    let prefix2 = sprintf "Merge remote-tracking branch 'origin/%s' into " main_branch in
    let title = Common.first_line message in
    String.is_prefix title ~prefix || String.is_prefix title ~prefix:prefix2
  | Some main_branch ->
    let expect = sprintf "Merge branch '%s' into %s" main_branch branch in
    let expect2 = sprintf "Merge remote-tracking branch 'origin/%s' into %s" main_branch branch in
    let title = Common.first_line message in
    String.equal title expect || String.equal title expect2
  | _ -> false

let modified_files_of_commit commit = List.concat [ commit.added; commit.removed; commit.modified ]

let is_valid_signature ~secret headers_sig body =
  let request_hash =
    let key = Cstruct.of_string secret in
    Cstruct.to_string @@ Nocrypto.Hash.SHA1.hmac ~key (Cstruct.of_string body)
  in
  let (`Hex request_hash) = Hex.of_string request_hash in
  String.equal headers_sig (sprintf "sha1=%s" request_hash)

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
  | "commit_comment" -> Commit_comment (commit_comment_notification_of_string body)
  | "member" | "create" | "delete" | "release" -> Event (event_notification_of_string body)
  | event -> failwith @@ sprintf "unsupported event : %s" event
