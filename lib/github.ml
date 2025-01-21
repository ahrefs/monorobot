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

let repo_of_notification = function
  | Push n -> n.repository
  | Pull_request n -> n.repository
  | PR_review n -> n.repository
  | PR_review_comment n -> n.repository
  | Issue n -> n.repository
  | Issue_comment n -> n.repository
  | Commit_comment n -> n.repository
  | Status n -> n.repository

let commits_branch_of_ref ref =
  match String.split_on_char '/' ref with
  | "refs" :: "heads" :: l -> String.concat "/" l
  | _ -> ref

let event_of_filename filename =
  match String.split_on_char '.' filename with
  | [ kind; _; "json" ] -> Some kind
  | _ -> None

let merge_commit_re = Re2.create_exn {|^Merge(?: remote-tracking)? branch '(?:origin/)?(.+)'(?: of [^ ]+)?( into .+)?$|}

let is_merge_commit_to_ignore ~(cfg : Config_t.config) ~branch commit =
  match cfg.main_branch_name with
  | Some main_branch when String.equal branch main_branch ->
    (*
      handle "Merge <any branch> into <feature branch>" commits when they are merged into main branch
      we should have already seen these commits on the feature branch but for some reason they are distinct:true

      some possible patterns:
      Merge branch 'develop' into feature_branch
      Merge branch 'develop' of github.com:org/repo into feature_branch
      Merge remote-tracking branch 'origin/develop' into feature_branch
      Merge remote-tracking branch 'origin/develop' (the default message pattern generated by GitHub "Update with merge commit" button)
    *)
    let title = Util.first_line commit.message in
    begin
      match Re2.find_submatches_exn merge_commit_re title with
      | [| Some _; Some incoming_branch; receiving_branch |] ->
        let receiving_branch = Option.map (fun s -> Stre.drop_prefix s " into ") receiving_branch in
        (* Should this raise when prefix isn't present? *)
        String.equal branch incoming_branch || Option.map_default (not $ String.equal branch) false receiving_branch
      | _ -> false
      | exception Re2.Exceptions.Regex_match_failed _ -> false
    end
  | Some _ | None -> false

let modified_files_of_commit commit = List.concat [ commit.added; commit.removed; commit.modified ]

let is_valid_signature ~secret headers_sig body =
  let request_hash = Digestif.SHA1.(hmac_string ~key:secret body |> to_hex) in
  String.equal headers_sig (sprintf "sha1=%s" request_hash)

let validate_signature ?signing_key ~headers body =
  match signing_key with
  | None -> Ok ()
  | Some secret ->
  match List.assoc_opt "x-hub-signature" headers with
  | None -> Error "unable to find header x-hub-signature"
  | Some signature -> if is_valid_signature ~secret signature body then Ok () else Error "signatures don't match"

(** Parse a payload. The type of the payload is detected from the headers.

    @raise Failure if unable to extract event from header *)
let parse_exn ~get_build headers body =
  let string_of_abstract_issue_state = function
    | Open -> "open"
    | Closed -> "closed"
  in
  let string_of_comment_action = function
    | Created -> "created"
    | Edited -> "edited"
    | Deleted -> "deleted"
  in
  let string_of_pr_review_action = function
    | Submitted -> "submitted"
    | Dismissed -> "dismissed"
    | Edited -> "edited"
  in
  let string_of_status_state = function
    | Success -> "success"
    | Failure -> "failure"
    | Pending -> "pending"
    | Error -> "error"
  in
  let print_opt f v = Option.map_default f "none" v in
  let print_comment_preview = Stre.shorten ~escape:true 40 in
  let print_commit_hash s = String.sub s 0 (min 8 @@ String.length s) in
  match List.assoc_opt "x-github-event" headers with
  | None -> Exn.fail "header x-github-event not found"
  | Some event ->
  match event with
  | "push" ->
    let n = commit_pushed_notification_of_string body in
    log#info "[%s] event %s: sender=%s, head=%s, ref=%s" n.repository.full_name event n.sender.login
      (print_opt (fun c -> print_commit_hash c.id) n.head_commit)
      n.ref;
    Lwt.return @@ Push n
  | "pull_request" ->
    let n = pr_notification_of_string body in
    log#info "[%s] event %s: number=%d, state=%s" n.repository.full_name event n.pull_request.number
      (string_of_abstract_issue_state n.pull_request.state);
    Lwt.return @@ Pull_request n
  | "pull_request_review" ->
    let n = pr_review_notification_of_string body in
    log#info "[%s] event %s: number=%d, sender=%s, action=%s, body=%S" n.repository.full_name event
      n.pull_request.number n.sender.login (string_of_pr_review_action n.action)
      (print_opt print_comment_preview n.review.body);
    Lwt.return @@ PR_review n
  | "pull_request_review_comment" ->
    let n = pr_review_comment_notification_of_string body in
    log#info "[%s] event %s: number=%d, sender=%s, action=%s, body=%S" n.repository.full_name event
      n.pull_request.number n.sender.login (string_of_comment_action n.action) (print_comment_preview n.comment.body);
    Lwt.return @@ PR_review_comment n
  | "issues" ->
    let n = issue_notification_of_string body in
    log#info "[%s] event %s: number=%d, state=%s" n.repository.full_name event n.issue.number
      (string_of_abstract_issue_state n.issue.state);
    Lwt.return @@ Issue n
  | "issue_comment" ->
    let n = issue_comment_notification_of_string body in
    log#info "[%s] event %s: number=%d, sender=%s, action=%s, body=%S" n.repository.full_name event n.issue.number
      n.sender.login (string_of_comment_action n.action) (print_comment_preview n.comment.body);
    Lwt.return @@ Issue_comment n
  | "status" ->
    let n = status_notification_of_string body in
    (* some commits live in multiple branches, so `n.branches` will be a list of those branches.
       However, builds are associated with a single branch, so we need to get the correct branch,
       to correctly track build/branch state.
       For buildkite notifications, we need to get the branch from the buildkite api and update
       the value on the notification before we handle it.
       At the moment we don't need to handle this logic in other notification types. *)
    let%lwt built_branch =
      match n.branches, n.target_url with
      | [], _ | [ _ ], _ | _, None -> Lwt.return n.branches
      | branches, Some build_url ->
      match Stre.exists build_url "buildkite" with
      | false -> Lwt.return branches
      | true ->
        log#info "Found multiple branches in notification, calling buildkite API to get the one for %s" build_url;
        (match%lwt get_build n with
        | Ok (build : Buildkite_t.get_build_res) -> Lwt.return [ ({ name = build.branch } : Github_t.branch) ]
        | Error e ->
          log#error "failed to get buildkite build details: %s" e;
          Lwt.return branches)
    in
    let n = { n with branches = built_branch } in
    let branches_str = sprintf "[%s]" @@ String.concat ", " (List.map (fun (b : branch) -> b.name) n.branches) in
    log#info "[%s] event %s: commit=%s, state=%s, context=%s, target_url=%s, branches=%s" n.repository.full_name event
      (print_commit_hash n.commit.sha) (string_of_status_state n.state) n.context (print_opt id n.target_url)
      branches_str;
    Lwt.return @@ Status n
  | "commit_comment" ->
    let n = commit_comment_notification_of_string body in
    log#info "[%s] event %s: commit=%s, sender=%s, action=%s, body=%S" n.repository.full_name event
      (print_opt print_commit_hash n.comment.commit_id)
      n.sender.login n.action (print_comment_preview n.comment.body);
    Lwt.return @@ Commit_comment n
  | event -> Lwt.return @@ Exn.fail "unhandled event type : %s" event

type basehead = string * string

type gh_resource =
  | Pull_request of int
  | Issue of int
  | Commit of commit_hash
  | Compare of basehead

type gh_link = repository * gh_resource

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
  match String.starts_with path ~prefix:"/" with
  | false -> None
  | true ->
    let path =
      Stre.drop_prefix path "/" |> flip Stre.drop_suffix "/" |> flip Stre.nsplitc '/' |> List.map Web.urldecode
    in
    let make_repo ~prefix ~owner ~name =
      let base = String.concat "/" (List.rev prefix) in
      let scheme = Uri.scheme url in
      let html_base, api_base =
        if String.ends_with base ~suffix:"github.com" then gh_com_html_base owner name, gh_com_api_base owner name
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
    let rec extract_link_type ~prefix path =
      try
        match path with
        | [ owner; name; "pull"; n ] ->
          let repo = make_repo ~prefix ~owner ~name in
          Some (repo, Pull_request (int_of_string n))
        | [ owner; name; "issues"; n ] ->
          let repo = make_repo ~prefix ~owner ~name in
          Some (repo, Issue (int_of_string n))
        | [ owner; name; "commit"; commit_hash ] | [ owner; name; "pull"; _; "commits"; commit_hash ] ->
          let repo = make_repo ~prefix ~owner ~name in
          if Re2.matches commit_sha_re commit_hash then Some (repo, Commit commit_hash) else None
        | owner :: name :: "compare" :: base_head | owner :: name :: "pull" :: _ :: "files" :: base_head ->
          let base_head = String.concat "/" base_head in
          let repo = make_repo ~prefix ~owner ~name in
          begin
            match Re2.find_submatches_exn compare_basehead_re base_head with
            | [| _; Some base; _; Some merge |] -> Some (repo, Compare (base, merge))
            | _ | (exception Re2.Exceptions.Regex_match_failed _) -> None
          end
        | [] -> None
        | next :: path -> extract_link_type ~prefix:(next :: prefix) path
      with _exn -> (* no hard fail when invalid format, slack user can compose any url string *) None
    in
    extract_link_type ~prefix:[ host ] path

let get_project_owners (pr : pull_request) ({ rules } : Config_t.project_owners) =
  Rule.Project_owners.match_rules pr.labels rules
  |> List.partition_map (fun reviewer ->
         try
           let team = Re2.find_first_exn ~sub:(`Index 1) gh_org_team_re reviewer in
           Right team
         with Re2.Exceptions.Regex_match_failed _ -> Left reviewer)
  |> fun (reviewers, team_reviewers) ->
  let already_requested_or_author = pr.user.login :: List.map (fun r -> r.login) pr.requested_reviewers in
  let already_requested_team = List.map (fun r -> r.slug) pr.requested_teams in
  let reviewers = List.filter (fun r -> not @@ List.mem r already_requested_or_author) reviewers in
  let team_reviewers = List.filter (fun tr -> not @@ List.mem tr already_requested_team) team_reviewers in
  if reviewers = [] && team_reviewers = [] then None else Some { reviewers; team_reviewers }
