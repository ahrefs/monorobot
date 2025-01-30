open Devkit
open Common
open Printf

let fmt_error ?exn fmt =
  ksprintf
    (fun s ->
      match exn with
      | Some exn -> Error (s ^ " :  exn " ^ Exn.str exn)
      | None -> Error s)
    fmt

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

  let buildkite_is_step_re =
    (* Checks if a pipeline or build step, by looking into the buildkite context
       buildkite/<pipeline_name>/<step_name>(/<substep_name>?) *)
    Re2.create_exn {|buildkite/[\w_-]+/([\w_-]+(/[\w_-]+)*)|}

  let buildkite_pipeline_name_re =
    (* Gets the pipeline name from the buildkite context *)
    Re2.create_exn {|buildkite/([\w_-]+)|}

  (** For now we only care about buildkite pipelines and steps. Other CI systems are not supported yet. *)
  let parse_context ~context =
    match Stre.starts_with context "buildkite/" with
    | false -> None
    | true ->
    try
      let is_pipeline_step = Re2.matches buildkite_is_step_re context in
      let pipeline_name = Re2.find_first_exn ~sub:(`Index 1) buildkite_pipeline_name_re context in
      Some { is_pipeline_step; pipeline_name }
    with _ -> None

  let parse_context_exn ~context =
    match parse_context ~context with
    | Some c -> c
    | None -> failwith (sprintf "failed to get pipeline name from notification. Context: %s" context)

  let buildkite_build_number_re =
    (* buildkite.com/<org_name>/<pipeline_name>/builds/<build_number> *)
    Re2.create_exn {|buildkite.com/[\w_-]+/[\w_-]+/builds/(\d+)|}

  (** For now we only care about buildkite pipelines and steps. Other CI systems are not supported yet. *)
  let get_build_number_exn ~build_url =
    match Re2.find_first_exn ~sub:(`Index 1) buildkite_build_number_re build_url with
    | build_number -> int_of_string build_number
    | exception _ -> failwith "failed to get build number from url"

  let get_org_pipeline_build (n : Github_t.status_notification) =
    match n.target_url with
    | None -> Error "no build url. Is this a Buildkite notification?"
    | Some build_url ->
    match Re2.find_submatches_exn buildkite_org_pipeline_build_re build_url with
    | exception _ -> failwith (sprintf "failed to parse Buildkite build url: %s" build_url)
    | [| Some _; Some org; Some pipeline; Some build_nr |] -> Ok (org, pipeline, build_nr)
    | _ -> failwith "failed to get the build details from the notification. Is this a Buildkite notification?"

  let is_failed_build (n : Github_t.status_notification) =
    n.state = Failure && Re2.matches buildkite_is_failed_re (Option.default "" n.description)

  let is_failing_build (n : Github_t.status_notification) =
    n.state = Failure && Re2.matches buildkite_is_failing_re (Option.default "" n.description)

  let is_canceled_build (n : Github_t.status_notification) =
    n.state = Failure && Re2.matches buildkite_is_canceled_re (Option.default "" n.description)

  let is_success_build (n : Github_t.status_notification) =
    n.state = Success && Re2.matches buildkite_is_success_re (Option.default "" n.description)

  let is_main_branch (cfg : Config_t.config) (n : Github_t.status_notification) =
    match cfg.main_branch_name with
    | None -> false
    | Some main_branch -> List.exists (fun ({ name } : Github_t.branch) -> String.equal name main_branch) n.branches

  let get_pipeline_config (cfg : Config_t.config) (n : Github_t.status_notification) =
    match cfg.status_rules.allowed_pipelines with
    | None -> None
    | Some allowed_pipelines ->
      List.find_opt (fun ({ name; _ } : Config_t.pipeline) -> name = n.context) allowed_pipelines

  let has_failed_builds_channel (cfg : Config_t.config) (n : Github_t.status_notification) =
    match get_pipeline_config cfg n with
    | Some ({ failed_builds_channel = Some _; _ } : Config_t.pipeline) -> true
    | Some _ | None -> false

  let get_branch_builds (n : Github_t.status_notification) (repo_state : State_t.repo_state) =
    match n.branches, parse_context ~context:n.context with
    | [ branch ], Some { pipeline_name; _ } ->
      (match StringMap.find_opt pipeline_name repo_state.pipeline_statuses with
      | None -> None
      | Some pipeline_statuses -> StringMap.find_opt branch.name pipeline_statuses)
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

  let new_failed_steps ~get_build (n : Github_t.status_notification) (repo_state : State_t.repo_state) =
    let log = Log.from "new_failed_steps" in
    match n.target_url, get_branch_builds n repo_state with
    | None, _ | _, None ->
      (* if we don't have a target_url value, we don't have a build number and cant't track build state *)
      Lwt.return []
    | Some build_url, Some builds_maps ->
    match is_failed_build n || is_canceled_build n with
    | false -> failwith (sprintf "can't calculate failed steps: build %s is not failed or canceled" build_url)
    | true ->
      let current_build_number = get_build_number_exn ~build_url in
      let previous_failed_steps =
        IntMap.fold
          (fun build_number (build_status : State_t.build_status) acc ->
            match build_number >= current_build_number with
            | true -> acc
            | false -> FailedStepSet.union build_status.failed_steps acc)
          builds_maps FailedStepSet.empty
      in
      let%lwt failed_steps =
        let get_failed_steps_from_buildkite () =
          log#info "getting failed steps from buildkite for %s" build_url;
          match%lwt get_build ?cache:(Some `Refresh) n with
          | Error e ->
            log#error "failed to get build %s from buildkite API: %s" build_url e;
            Lwt.return @@ FailedStepSet.empty
          | Ok (build : Buildkite_t.get_build_res) ->
          match build.state with
          | Failed | Canceled ->
            (* if we get here, we know we can use parse_context_exn *)
            let { pipeline_name; _ } = parse_context_exn ~context:n.context in
            let failed_steps =
              FailedStepSet.of_list
              @@ List.filter_map
                   (fun (job : Buildkite_t.job_type) ->
                     match job with
                     | Manual _ | Waiter _ -> None
                     | Script { name; state; web_url; _ } | Trigger { name; state; web_url; _ } ->
                     match state with
                     | Failed ->
                       let name =
                         (* replace spaces and underscores with dashes, like buildkite does *)
                         String.map
                           (function
                             | ' ' | '_' -> '-'
                             | c -> c)
                           (String.lowercase_ascii name)
                       in
                       Some
                         {
                           Buildkite_t.build_url = web_url;
                           name =
                             (* mimic notification context structure so that steps can match with the existing ones *)
                             sprintf "buildkite/%s/%s" pipeline_name name;
                         }
                     | _ -> None)
                   build.jobs
            in
            (* we may not have this build in state, so we need to create one in that case.
               We need to have a little bit of duplication with state.ml to avoid circular dependencies *)
            let init_build_state =
              {
                State_t.status = n.state;
                build_number = current_build_number;
                build_url;
                commit =
                  { sha = n.sha; author = n.commit.commit.author.email; commit_message = n.commit.commit.message };
                is_finished = true;
                is_canceled = build.state = Canceled;
                failed_steps;
                created_at = Timestamp.wrap build.created_at;
                finished_at = Option.map Timestamp.wrap build.finished_at;
              }
            in
            let update_build_status builds_map =
              let updated_status =
                match IntMap.find_opt current_build_number builds_map with
                | Some (current_build_status : State_t.build_status) -> { current_build_status with failed_steps }
                | None -> init_build_state
              in
              IntMap.add current_build_number updated_status builds_map
            in
            let update_pipeline_status branches_statuses =
              let init_branch_state = IntMap.singleton current_build_number init_build_state in
              let current_statuses = Option.default StringMap.empty branches_statuses in
              let updated_statuses =
                List.map
                  (fun (branch : Github_t.branch) ->
                    let builds_map =
                      Option.map_default
                        (fun branches_statuses ->
                          match StringMap.find_opt branch.name branches_statuses with
                          | Some builds_map -> update_build_status builds_map
                          | None -> init_branch_state)
                        init_branch_state branches_statuses
                    in
                    branch.name, builds_map)
                  n.branches
              in
              Some (List.fold_left (fun m (key, data) -> StringMap.add key data m) current_statuses updated_statuses)
            in
            (* we need to update the state with the failed steps, otherwise we can't calculate
               the new failed steps in future builds *)
            repo_state.pipeline_statuses <-
              StringMap.update pipeline_name update_pipeline_status repo_state.pipeline_statuses;
            Lwt.return failed_steps
          | _ ->
            log#warn "build state for %s is not failed in buildkite. We will not calculate failed steps" build_url;
            Lwt.return @@ FailedStepSet.empty
        in
        (* if the current build isn't in state, or doesn't have any failed steps in state, it might be that
           we didn't get all the notifications, or that the step notifications might arrive after the build
           failed notification. We need to get the build from the api and parse it to get the failed steps *)
        match get_current_build n repo_state with
        | Some b when not @@ FailedStepSet.is_empty b.failed_steps -> Lwt.return b.failed_steps
        | Some _ -> get_failed_steps_from_buildkite ()
        | None ->
          log#warn "failed to find build %s in state, maybe it was cleaned up?" build_url;
          get_failed_steps_from_buildkite ()
      in
      Lwt.return @@ FailedStepSet.(diff failed_steps previous_failed_steps |> elements)

  (* builds should be notified in the builds failed channel if:
     -  the build is failed OR the build is canceled and notify_canceled_builds is true
     -  the build is for the main branch
     -  the pipeline is in the allowed_pipelines list
     -  a failed_builds_channel is defined for the pipeline
  *)
  let notify_fail (n : Github_t.status_notification) (cfg : Config_t.config) =
    let notify_canceled_build =
      match get_pipeline_config cfg n with
      | None -> false
      | Some ({ notify_canceled_builds; _ } : Config_t.pipeline) -> notify_canceled_builds && is_canceled_build n
    in
    (is_failed_build n || notify_canceled_build) && has_failed_builds_channel cfg n && is_main_branch cfg n

  let stale_build_threshold =
    (* 2h as the threshold for long running or stale builds *)
    Ptime.Span.of_int_s (60 * 60 * 2)
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
    | None -> Devkit.Exn.fail "Cache: Ptime overflow adding expiration"

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
