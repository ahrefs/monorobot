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

let merge_commit_re = Re2.create_exn {|^Merge(?: remote-tracking)? branch '(?:origin/)?.+'(?: of .+)? into (.+)$|}

let is_merge_commit_to_ignore ~(cfg : Config_t.config) ~branch commit =
  match cfg.main_branch_name with
  | Some main_branch when String.equal branch main_branch ->
    (*
      handle "Merge <any branch> into <feature branch>" commits when they are merged into main branch
      we should have already seen these commits on the feature branch but for some reason they are distinct:true
    *)
    let title = Common.first_line commit.message in
    begin
      try
        let receiving_branch = Re2.find_first_exn ~sub:(`Index 1) merge_commit_re title in
        not @@ String.equal branch receiving_branch
      with Re2.Exceptions.Regex_match_failed _ -> false
    end
  | Some _ | None -> false

let modified_files_of_commit commit = List.concat [ commit.added; commit.removed; commit.modified ]

let is_valid_signature ~secret headers_sig body =
  let request_hash =
    let key = Cstruct.of_string secret in
    Cstruct.to_string @@ Nocrypto.Hash.SHA1.hmac ~key (Cstruct.of_string body)
  in
  let (`Hex request_hash) = Hex.of_string request_hash in
  String.equal headers_sig (sprintf "sha1=%s" request_hash)

let validate_signature ?signing_key ~headers body =
  match signing_key with
  | None -> Ok ()
  | Some secret ->
  match List.Assoc.find headers "x-hub-signature" ~equal:String.equal with
  | None -> Error "unable to find header x-hub-signature"
  | Some signature -> if is_valid_signature ~secret signature body then Ok () else Error "signatures don't match"

(* Parse a payload. The type of the payload is detected from the headers. *)
let parse_exn headers body =
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
  | "create" | "delete" | "member" | "ping" | "release" -> Event (event_notification_of_string body)
  | event -> failwith @@ sprintf "unsupported event : %s" event

type basehead = string * string

type gh_link =
  | Pull_request of repository * int
  | Issue of repository * int
  | Commit of repository * commit_hash
  | Compare of repository * basehead

let gh_link_re = Re2.create_exn {|^(.*)/(.+)/(.+)/(commit|pull|issues|compare)/([a-zA-Z0-9/:\-_.~\^%]+)$|}
let commit_sha_re = Re2.create_exn {|[a-f0-9]{4,40}|}
let comparer_re = {|([a-zA-Z0-9/:\-_.~\^]+)|}
let compare_basehead_re = Re2.create_exn (sprintf {|%s([.]{3})%s|} comparer_re comparer_re)
let gh_org_team_re = Re2.create_exn {|[a-zA-Z0-9\-]+/([a-zA-Z0-9\-]+)|}

(** [gh_link_of_string s] parses a URL string [s] to try to match a supported
    GitHub link type, generating repository endpoints if necessary *)
let gh_link_of_string url_str =
  let url = Uri.of_string url_str in
  let path = Uri.path url in
  let gh_com_html_base owner name = sprintf "https://github.com/%s/%s" owner name in
  let gh_com_api_base owner name = sprintf "https://api.github.com/repos/%s/%s" owner name in
  let custom_html_base ?(scheme = "https") base owner name = sprintf "%s://%s/%s/%s" scheme base owner name in
  let custom_api_base ?(scheme = "https") base owner name =
    sprintf "%s://%s/api/v3/repos/%s/%s" scheme base owner name
  in
  match Uri.host url with
  | None -> None
  | Some host ->
  match Re2.find_submatches_exn gh_link_re path with
  | [| _; prefix; Some owner; Some name; Some link_type; Some item |] ->
    let item = Base.String.chop_suffix_if_exists item ~suffix:"/" in
    let repo =
      let base = Option.value_map prefix ~default:host ~f:(fun p -> String.concat [ host; p ]) in
      let scheme = Uri.scheme url in
      let html_base, api_base =
        if String.is_suffix base ~suffix:"github.com" then gh_com_html_base owner name, gh_com_api_base owner name
        else custom_html_base ?scheme base owner name, custom_api_base ?scheme base owner name
      in
      {
        name;
        full_name = sprintf "%s/%s" owner name;
        url = html_base;
        commits_url = sprintf "%s/commits{/sha}" api_base;
        contents_url = sprintf "%s/contents/{+path}" api_base;
        pulls_url = sprintf "%s/pulls{/number}" api_base;
        issues_url = sprintf "%s/issues{/number}" api_base;
        compare_url = sprintf "%s/compare{/basehead}" api_base;
      }
    in
    let verify_commit_sha repo item =
      try
        match Re2.find_submatches_exn commit_sha_re item with
        | [| Some sha |] -> Some (Commit (repo, sha))
        | _ -> None
      with _ -> None
    in
    let verify_compare_basehead repo basehead =
      match Re2.find_submatches_exn compare_basehead_re basehead with
      | [| _; Some base; _; Some merge |] -> Some (Compare (repo, (base, merge)))
      | _ | (exception Re2.Exceptions.Regex_match_failed _) -> None
    in
    begin
      try
        match link_type with
        | "pull" -> Some (Pull_request (repo, Int.of_string item))
        | "issues" -> Some (Issue (repo, Int.of_string item))
        | "commit" -> verify_commit_sha repo item
        | "compare" ->
          let item = Uri.pct_decode item in
          verify_compare_basehead repo item
        | _ -> None
      with _ -> None
    end
  | _ | (exception Re2.Exceptions.Regex_match_failed _) -> None

let get_project_owners (pr : pull_request) ({ rules } : Config_t.project_owners) =
  Rule.Project_owners.match_rules pr.labels rules
  |> List.dedup_and_sort ~compare:String.compare
  |> List.partition_map ~f:(fun reviewer ->
       try
         let team = Re2.find_first_exn ~sub:(`Index 1) gh_org_team_re reviewer in
         Second team
       with Re2.Exceptions.Regex_match_failed _ -> First reviewer
     )
  |> fun (reviewers, team_reviewers) ->
  let already_requested_or_author = pr.user.login :: List.map ~f:(fun r -> r.login) pr.requested_reviewers in
  let already_requested_team = List.map ~f:(fun r -> r.slug) pr.requested_teams in
  let reviewers = List.filter ~f:(not $ List.mem already_requested_or_author ~equal:String.equal) reviewers in
  let team_reviewers = List.filter ~f:(not $ List.mem already_requested_team ~equal:String.equal) team_reviewers in
  if List.is_empty reviewers && List.is_empty team_reviewers then None else Some { reviewers; team_reviewers }
