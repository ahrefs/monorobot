open Devkit
open Common
open Printf

exception Util_error of string
let log = Log.from "util"

let fmt_error_string ?exn handler fmt =
  ksprintf
    (fun s ->
      match exn with
      | Some exn -> handler (sprintf "%s : exn %s" s (Exn.str exn))
      | None -> handler s)
    fmt

let util_error ?exn fmt = fmt_error_string ?exn (fun s -> raise (Util_error s)) fmt
let fmt_error ?exn fmt = fmt_error_string ?exn Result.error fmt

let first_line s =
  match String.split_on_char '\n' s with
  | x :: _ -> x
  | [] -> s

let decode_string_pad s = Stre.rstrip ~chars:"= \n\r\t" s |> Base64.decode_exn ~pad:false

let http_request ?headers ?body meth path =
  let setup h =
    Curl.set_followlocation h true;
    Curl.set_maxredirs h 1
  in
  match%lwt Web.http_request_lwt ~setup ~ua:"monorobot" ~verbose:true ?headers ?body meth path with
  | `Ok s -> Lwt.return @@ Ok s
  | `Error e -> Lwt.return @@ Error e

let bearer_token_header access_token = sprintf "Authorization: Bearer %s" (Uri.pct_encode access_token)

let query_error_msg url e = sprintf "error while querying %s: %s" url e

let sign_string_sha256 ~key ~basestring = Digestif.SHA256.(hmac_string ~key basestring |> to_hex)

module Build = struct
  type pipeline_check = {
    is_pipeline_step : bool;
    pipeline_name : string;
  }

  let buildkite_is_failed_re = Re2.create_exn {|^Build #\d+ failed|}
  let buildkite_is_failing_re = Re2.create_exn {|^Build #(\d+) is failing|}
  let buildkite_is_canceled_re = Re2.create_exn {|^Build #\d+ canceled by .+|}
  let buildkite_is_success_re = Re2.create_exn {|^Build #\d+ passed|}

  let buildkite_org_pipeline_build_re =
    (* buildkite.com/<org_name>/<pipeline_name>/builds/<build_number> *)
    Re2.create_exn {|buildkite.com/([\w_-]+)/([\w_-]+)/builds/(\d+)|}

  let buildkite_api_org_pipeline_build_job_re =
    (* https://api.buildkite.com/v2/organizations/<org_name>/pipelines/<pipeline_name>/builds/<build_number>/jobs/<job_number>/log *)
    Re2.create_exn
      {|https://api.buildkite.com/v2/organizations/([\w_-]+)/pipelines/([\w_-]+)/builds/(\d+)/jobs/([\d\w_-]+)/log|}

  let buildkite_is_step_re =
    (* Checks if a pipeline or build step, by looking into the buildkite context
       buildkite/<pipeline_name>/<step_name>(/<substep_name>?) *)
    Re2.create_exn {|buildkite/[\w_-]+/([\w_-]+(/[\w_-]+)*)|}

  let buildkite_pipeline_name_re =
    (* Gets the pipeline name from the buildkite context *)
    Re2.create_exn {|buildkite/([\w_-]+)|}

  let git_ssh_re =
    (* matches git ssh clone links *)
    Re2.create_exn {|^git@([A-Za-z0-9.-]+):([A-Za-z0-9._-]+)\/([A-Za-z0-9._-]+)\.git$|}

  let git_ssh_to_https url =
    match Re2.find_submatches_exn git_ssh_re url with
    | exception exn -> util_error ~exn "failed to parse git ssh link %s" url
    | [| Some _; Some url; Some user; Some repo |] -> sprintf "https://%s/%s/%s" url user repo
    | _ -> util_error "failed to get repo details from the ssh link."

  let git_ssh_to_contents_url url =
    match Re2.find_submatches_exn git_ssh_re url with
    | exception exn -> util_error ~exn "failed to parse git ssh link %s" url
    | [| Some _; Some "github.com"; Some user; Some repo |] ->
      sprintf "https://api.github.com/repos/%s/%s/contents/{+path}" user repo
    | [| Some _; Some url; Some user; Some repo |] ->
      (* GHE links *)
      sprintf "https://%s/api/v3/repos/%s/%s/contents/{+path}" url user repo
    | _ -> util_error "failed to get repo details from the ssh link."

  let is_pipeline_step context = Re2.matches buildkite_is_step_re context

  (** For now we only care about buildkite pipelines and steps. Other CI systems are not supported yet. *)
  let parse_context ~context =
    match Stre.starts_with context "buildkite/" with
    | false -> None
    | true ->
    try
      let pipeline_name = Re2.find_first_exn ~sub:(`Index 1) buildkite_pipeline_name_re context in
      Some { is_pipeline_step = is_pipeline_step context; pipeline_name }
    with _ -> None

  let parse_context_exn ~context =
    match parse_context ~context with
    | Some c -> c
    | None -> util_error "failed to get pipeline name from notification. Context: %s" context

  let buildkite_build_number_re =
    (* buildkite.com/<org_name>/<pipeline_name>/builds/<build_number> *)
    Re2.create_exn {|buildkite.com/[\w_-]+/[\w_-]+/builds/(\d+)|}

  (** For now we only care about buildkite pipelines and steps. Other CI systems are not supported yet. *)
  let get_build_number_exn ~build_url =
    match Re2.find_first_exn ~sub:(`Index 1) buildkite_build_number_re build_url with
    | build_number -> int_of_string build_number
    | exception _ -> util_error "failed to get build number from url"

  let get_org_pipeline_build' build_url =
    match Re2.find_submatches_exn buildkite_org_pipeline_build_re build_url with
    | exception _ -> util_error "failed to parse Buildkite build url: %s" build_url
    | [| Some _; Some org; Some pipeline; Some build_nr |] -> Ok (org, pipeline, build_nr)
    | _ -> util_error "failed to get the build details from the notification. Is this a Buildkite notification?"

  let get_org_pipeline_build (n : Github_t.status_notification) =
    match n.target_url with
    | None -> Error "no build url. Is this a Buildkite notification?"
    | Some build_url -> get_org_pipeline_build' build_url

  let is_failed_build (n : Github_t.status_notification) =
    n.state = Failure && Re2.matches buildkite_is_failed_re (Option.default "" n.description)

  let is_failing_build (n : Github_t.status_notification) =
    n.state = Failure && Re2.matches buildkite_is_failing_re (Option.default "" n.description)

  let is_canceled_build (n : Github_t.status_notification) =
    n.state = Failure && Re2.matches buildkite_is_canceled_re (Option.default "" n.description)

  let is_main_branch (cfg : Config_t.config) (n : Github_t.status_notification) =
    match cfg.main_branch_name with
    | None -> false
    | Some main_branch -> List.exists (fun ({ name } : Github_t.branch) -> String.equal name main_branch) n.branches

  let get_pipeline_config (cfg : Config_t.config) (n : Github_t.status_notification) =
    match cfg.status_rules.allowed_pipelines with
    | None -> None
    | Some allowed_pipelines ->
      List.find_opt (fun ({ name; _ } : Config_t.pipeline) -> name = n.context) allowed_pipelines

  let get_branch_builds (n : Github_t.status_notification) (repo_state : State_t.repo_state) =
    match n.branches, parse_context ~context:n.context with
    | [ branch ], Some { pipeline_name; _ } ->
      (match StringMap.find_opt pipeline_name repo_state.pipeline_statuses with
      | None -> None
      | Some branch_statuses -> StringMap.find_opt branch.name branch_statuses)
    | _ -> None

  let get_branch_commits (n : Github_t.status_notification) (repo_state : State_t.repo_state) =
    let pipeline_name =
      match parse_context ~context:n.context with
      | Some { pipeline_name; _ } ->
        (* if we can parse the context, we want to use the base pipeline name, not the steps *)
        pipeline_name
      | None -> n.context
    in
    match StringMap.find_opt pipeline_name repo_state.pipeline_commits with
    | None -> None
    | Some pipeline_commits ->
      List.find_map (fun ({ name; _ } : Github_t.branch) -> StringMap.find_opt name pipeline_commits) n.branches

  let get_current_build (n : Github_t.status_notification) (repo_state : State_t.repo_state) =
    match n.target_url, get_branch_builds n repo_state with
    | Some build_url, Some builds_maps ->
      let n = get_build_number_exn ~build_url in
      IntMap.find_opt n builds_maps
    | _ -> None

  let stale_build_threshold =
    (* 2h as the threshold for long running or stale builds *)
    Ptime.Span.of_int_s (60 * 60 * 2)

  let filter_failed_jobs jobs =
    List.filter_map
      (function
        | Buildkite_t.Script ({ state = Failed; _ } as job) | Trigger ({ state = Failed; _ } as job) -> Some job
        | _ -> None)
      jobs
end

module type Cache_t = sig
  type t
end

module Cache (T : Cache_t) = struct
  open Ptime

  type cache_entry = {
    value : T.t;
    expires_at : Ptime.t;
  }

  type t = {
    table : (string, cache_entry) Hashtbl.t;
    mutable last_purge : Ptime.t;
    ttl : Span.t;
    purge_interval : Span.t;
  }

  let now = Ptime_clock.now

  (* purge every 24h by default *)
  let default_purge_interval = Ptime.Span.of_int_s (60 * 60 * 24)

  let create ?(ttl = Build.stale_build_threshold) ?(purge_interval = default_purge_interval) () : t =
    { table = Hashtbl.create 128; ttl; last_purge = now (); purge_interval }

  let set cache key value =
    let expires_at = add_span (now ()) cache.ttl in
    match expires_at with
    | Some expires_at -> Hashtbl.replace cache.table key { value; expires_at }
    | None -> util_error "Cache: Ptime overflow adding expiration"

  let purge cache =
    Hashtbl.filter_map_inplace
      (fun _key entry -> if is_later ~than:entry.expires_at (now ()) then None else Some entry)
      cache.table;
    cache.last_purge <- now ()

  (* Get an entry from the cache, purging if the purge interval has passed *)
  let get cache key =
    let should_purge cache =
      match add_span cache.last_purge cache.purge_interval with
      | Some threshold_time -> is_later ~than:threshold_time (now ())
      | None -> false
    in
    if should_purge cache then purge cache;
    match Hashtbl.find_opt cache.table key with
    | Some entry ->
      (match is_later ~than:entry.expires_at (now ()) with
      | true ->
        Hashtbl.remove cache.table key;
        None
      | false -> Some entry.value)
    | None -> None
end

module Webhook = struct
  type n = Buildkite_webhook_t.webhook_build_payload

  let parse_signature_header header =
    let timestamp, signature =
      String.split_on_char ',' header
      |> List.fold_left
           (fun acc part ->
             match String.split_on_char '=' part with
             | [ "timestamp"; timestamp ] -> Some timestamp, snd acc
             | [ "signature"; signature ] -> fst acc, Some signature
             | _ -> acc)
           (None, None)
    in
    match timestamp, signature with
    | Some timestamp, Some signature -> Ok (timestamp, signature)
    | _ -> Error "missing timestamp or signature value in header"

  let is_valid_signature ~secret ~timestamp ~signature body =
    let message = sprintf "%s.%s" timestamp body in
    let expected = Digestif.SHA256.(hmac_string ~key:secret message |> to_hex) in
    match String.equal signature expected with
    | true -> Ok ()
    | false -> Error "webhook signature is not valid"

  let validate_signature ?signing_key ~headers body =
    match signing_key with
    | None -> Ok ()
    | Some secret ->
    match List.assoc_opt "x-buildkite-signature" headers with
    | None -> Error "unable to find header x-buildkite-signature"
    | Some header ->
    match parse_signature_header header with
    | Error e -> Error (sprintf "invalid signature header: %s" e)
    | Ok (timestamp, signature) -> is_valid_signature ~secret ~timestamp ~signature body

  let validate_repo_url (secrets : Config_t.secrets) (n : Buildkite_webhook_t.webhook_build_payload) =
    let repo_url = Build.git_ssh_to_https n.pipeline.repository in
    match List.exists (fun (r : Config_t.repo_config) -> String.equal r.url repo_url) secrets.repos with
    | true -> repo_url
    | false -> util_error "unsupported repository %s" repo_url

  let get_pipeline_config (cfg : Config_t.config) pipeline_name =
    match cfg.status_rules.allowed_pipelines with
    | None -> None
    | Some allowed_pipelines ->
      List.find_opt (fun ({ name; _ } : Config_t.pipeline) -> String.equal name pipeline_name) allowed_pipelines

  let pipeline_name ({ pipeline = { name; slug; _ }; _ } : n) = sprintf "buildkite/%s" (Option.default name slug)

  let repo_key org pipeline = Printf.sprintf "%s/%s" org pipeline

  let extract_metadata_email commit_str =
    let lines = String.split_on_char '\n' commit_str in
    let author_line = List.find_opt (String.starts_with ~prefix:"Author:") lines in
    match author_line with
    | None -> None
    | Some line ->
      (* Find the position of < and > and extract the email address from within them *)
      let email_start = String.index_opt line '<' in
      let email_end = String.index_opt line '>' in
      (match email_start, email_end with
      | Some start, Some end_pos -> Some (String.sub line (start + 1) (end_pos - start - 1))
      | _ -> None)

  let failed_builds_channel_exn cfg n =
    let pipeline_name = pipeline_name n in
    match get_pipeline_config cfg pipeline_name with
    | Some ({ failed_builds_channel = Some channel; _ } : Config_t.pipeline) -> channel
    | Some _ | None -> util_error "no failed builds channel defined for pipeline %s" pipeline_name

  (** builds should be notified in the builds failed channel if:
     -  the build is failed OR the build is canceled and notify_canceled_builds is true
     -  the build is for the main branch
     -  the current pipeline is in the allowed_pipelines list
     -  a failed_builds_channel is defined for the pipeline
  *)
  let notify_fail (cfg : Config_t.config) (n : n) =
    let pipeline_name = pipeline_name n in
    let pipeline_config = get_pipeline_config cfg pipeline_name in
    (* TODO: For now we don't notify for canceled builds. Can we get more value than troubles from it? *)
    let _notify_canceled_build =
      match pipeline_config with
      | None -> false
      | Some ({ notify_canceled_builds; _ } : Config_t.pipeline) -> notify_canceled_builds && n.build.state = Canceled
    in
    let has_failed_builds_channel =
      match pipeline_config with
      | Some ({ failed_builds_channel = Some _channel; _ } : Config_t.pipeline) -> true
      | Some _ | None -> false
    in
    n.build.state = Failed && has_failed_builds_channel

  let notify_success (repo_state : State_t.repo_state) (repo_key : string) (n : n) =
    n.build.state = Passed
    && Option.map_default
         (fun { State_t.steps; _ } -> not (Common.FailedStepSet.is_empty steps))
         false
         (Stringtbl.find_opt repo_state.failed_steps repo_key)

  let new_failed_steps ~(repo_state : State_t.repo_state) ~get_build (n : n) =
    let* org, pipeline, build_nr = Lwt.return @@ Build.get_org_pipeline_build' n.build.web_url in
    let repo_key = repo_key org pipeline in
    let get_failed_steps () =
      log#info "Fetching failed steps for org=%s, pipeline=%s, build=%s" org pipeline build_nr;
      let build_url = Printf.sprintf "organizations/%s/pipelines/%s/builds/%s" org pipeline build_nr in
      let* (build : Buildkite_t.get_build_res) = get_build ~build_url in
      let to_failed_step (job : Buildkite_t.job) = { Buildkite_t.name = job.name; build_url = job.web_url } in
      Lwt.return_ok @@ (Build.filter_failed_jobs build.jobs |> List.map to_failed_step |> Common.FailedStepSet.of_list)
    in
    match Stringtbl.find_opt repo_state.failed_steps repo_key with
    | Some state when n.build.number < state.last_build ->
      (* discard if current build is older than the last build that was notified *)
      Lwt.return_ok Common.FailedStepSet.empty
    | None ->
      let* failed_steps = get_failed_steps () in
      Stringtbl.replace repo_state.failed_steps repo_key { steps = failed_steps; last_build = n.build.number };
      Lwt.return_ok failed_steps
    | Some state ->
      let* build_failed_steps = get_failed_steps () in

      (* return only the new failed steps. Keep all the failed steps in state, but keep the
         original failed steps urls for the ones that intersect, instead of the new ones. *)
      let steps_intersect = Common.FailedStepSet.inter state.steps build_failed_steps in
      let new_failed_steps = Common.FailedStepSet.diff build_failed_steps state.steps in
      let updated_steps = Common.FailedStepSet.union steps_intersect new_failed_steps in
      Stringtbl.replace repo_state.failed_steps repo_key { steps = updated_steps; last_build = n.build.number };
      Lwt.return_ok new_failed_steps
end
