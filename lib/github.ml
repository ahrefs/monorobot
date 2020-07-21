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
  | Commit_comment of commit_comment_notification
  | Status of status_notification
  | Event of string

(* all other events *)

let to_repo = function
  | Push n -> Some n.repository
  | Pull_request n -> Some n.repository
  | PR_review n -> Some n.repository
  | PR_review_comment n -> Some n.repository
  | Issue n -> Some n.repository
  | Issue_comment n -> Some n.repository
  | Commit_comment n -> Some n.repository
  | Status n -> Some n.repository
  | Event _ -> None

let api_url_of_repo (repo : repository) =
  Option.map ~f:(fun host ->
    match host with
    | "api.github.com" -> Printf.sprintf "https://%s" host
    | _ -> Printf.sprintf "https://%s/api/v3" host)
  @@ Uri.host
  @@ Uri.of_string repo.url

let user_of_full_name_parts full_name_parts =
  match full_name_parts with
  | user :: _ -> Some user
  | _ -> None

let name_of_full_name_parts full_name_parts =
  match full_name_parts with
  | _ :: repo_name :: _ -> Some repo_name
  | _ -> None

let load_config token req =
  let headers = Some [ "Accept: application/vnd.github.v3+json" ] in
  match to_repo req with
  | None ->
    log#warn "unable to resolve repository from request";
    Lwt.return_none
  | Some repo ->
    let full_name_parts = String.split ~on:'/' repo.full_name in
    ( match user_of_full_name_parts full_name_parts with
    | None ->
      log#warn "unable to resolve repository owner";
      Lwt.return_none
    | Some owner ->
    match name_of_full_name_parts full_name_parts with
    | None ->
      log#warn "unable to resolve repository name";
      Lwt.return_none
    | Some repo_name ->
    match api_url_of_repo repo with
    | None ->
      log#warn "unable to resolve github api url from repository url";
      Lwt.return_none
    | Some base_url ->
      let url = Printf.sprintf "%s/repos/%s/%s/contents/notabot.json?access_token=%s" base_url owner repo_name token in
      ( match%lwt Web.http_request_lwt ?headers `GET url with
      | `Error e ->
        log#warn "error while querying github api %s: %s" url e;
        Lwt.return_none
      | `Ok s ->
        let response = Github_j.content_api_response_of_string s in
        ( match response.encoding with
        | "base64" ->
          Lwt.return_some
          @@ Notabot_j.config_of_string
          @@ Base64.decode_exn
          @@ String.concat
          @@ String.split ~on:'\n'
          @@ response.content
        | e ->
          log#warn "unknown encoding format '%s'." e;
          Lwt.return_none
        )
      )
    )

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
  | "commit_comment" -> Commit_comment (commit_comment_notification_of_string body)
  | ("member" | "create" | "delete" | "release") as event -> Event event
  | event -> failwith @@ sprintf "unsupported event : %s" event

let get_commits_branch ref =
  match String.split ~on:'/' ref with
  | "refs" :: "heads" :: l -> String.concat ~sep:"/" l
  | _ -> ref

let query_api ?token ~url parse =
  let headers = Option.map token ~f:(fun t -> [ sprintf "Authorization: token %s" t ]) in
  match%lwt Web.http_request_lwt ~ua:"notabot" ~verbose:true ?headers `GET url with
  | `Error e ->
    log#error "error while querying github api %s: %s" url e;
    Lwt.return_none
  | `Ok s ->
  try Lwt.return_some (parse s)
  with exn ->
    log#error ~exn "impossible to parse github api answer to %s" url;
    Lwt.return_none

let generate_query_commmit cfg ~url ~sha =
  (* the expected output is a payload containing content about commits *)
  match cfg.Config.offline with
  | None -> query_api ?token:cfg.Config.gh_token ~url api_commit_of_string
  | Some path ->
    let f = Caml.Filename.concat path sha in
    ( match Caml.Sys.file_exists f with
    | false ->
      log#error "unable to find offline file %s, try fetching it from %s" f url;
      Lwt.return_none
    | true ->
      Stdio.In_channel.with_file f ~f:(fun ic ->
        try
          let content = Stdio.In_channel.input_all ic in
          Lwt.return_some (api_commit_of_string content)
        with exn ->
          log#error ~exn "unable to read offline file %s" f;
          Lwt.return_none)
    )

let generate_commit_from_commit_comment cfg n =
  let url = n.repository.commits_url in
  let url_length = String.length url - 6 in
  (* remove {\sha} from the string *)
  let sha =
    match n.comment.commit_id with
    | None ->
      log#error "unable to find commit id for this commit comment event";
      ""
    | Some id -> id
  in
  let commit_url = String.sub ~pos:0 ~len:url_length url ^ "/" ^ sha in
  (* add sha hash to get the full api link *)
  generate_query_commmit cfg ~url:commit_url ~sha
