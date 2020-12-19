open Base
open Devkit
open Printf
open Github_j

exception Remote_config_error of string

let remote_config_error fmt = ksprintf (fun s -> raise (Remote_config_error s)) fmt

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
  (* all other events *)
  | Event of string

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

let commits_branch_of_ref ref =
  match String.split ~on:'/' ref with
  | "refs" :: "heads" :: l -> String.concat ~sep:"/" l
  | _ -> ref

let event_of_filename filename =
  match String.split_on_chars filename ~on:[ '.' ] with
  | [ kind; _name; ext ] when String.equal ext "json" -> Some kind
  | _ -> None

let api_url_of_repo (repo : repository) =
  Option.map ~f:(fun host ->
    match host with
    | "api.github.com" -> sprintf "https://%s" host
    | _ -> sprintf "https://%s/api/v3" host)
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

let get_remote_config_json_url filename ?token req =
  match to_repo req with
  | None -> remote_config_error "unable to resolve repository from request"
  | Some repo ->
  match String.split ~on:'/' repo.full_name with
  | full_name_parts ->
  match user_of_full_name_parts full_name_parts with
  | None -> remote_config_error "unable to resolve repository owner"
  | Some owner ->
  match name_of_full_name_parts full_name_parts with
  | None -> remote_config_error "unable to resolve repository name"
  | Some repo_name ->
  match api_url_of_repo repo with
  | None -> remote_config_error "unable to resolve github api url from repository url"
  | Some base_url ->
    let url = sprintf "%s/repos/%s/%s/contents/%s" base_url owner repo_name filename in
    begin
      match token with
      | None -> url
      | Some token -> sprintf "%s?access_token=%s" url token
    end

let config_of_content_api_response response =
  try%lwt
    match response.encoding with
    | "base64" ->
      Lwt.return
      @@ Config_j.config_of_string
      @@ Common.decode_string_pad
      @@ String.concat
      @@ String.split_lines
      @@ response.content
    | e -> remote_config_error "unknown encoding format '%s'" e
  with
  | Base64.Invalid_char -> remote_config_error "unable to decode configuration file from base64"
  | Yojson.Json_error msg -> remote_config_error "unable to parse configuration file as valid JSON (%s)" msg

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

let is_valid_signature ~secret headers_sig body =
  let request_hash =
    let key = Cstruct.of_string secret in
    Cstruct.to_string @@ Nocrypto.Hash.SHA1.hmac ~key (Cstruct.of_string body)
  in
  let (`Hex request_hash) = Hex.of_string request_hash in
  String.equal headers_sig (sprintf "sha1=%s" request_hash)

let load_config_json url =
  let headers = [ "Accept: application/vnd.github.v3+json" ] in
  match%lwt Web.http_request_lwt ~headers `GET url with
  | `Error e -> remote_config_error "error while querying github api %s: %s" url e
  | `Ok s -> config_of_content_api_response @@ Github_j.content_api_response_of_string s

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

let generate_query_commit cfg ~url ~sha =
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
  generate_query_commit cfg ~url:commit_url ~sha

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
