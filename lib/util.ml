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

  let is_failed_build (n : Github_t.status_notification) =
    n.state = Failure && Re2.matches buildkite_is_failed_re (Option.default "" n.description)

  let is_failing_build (n : Github_t.status_notification) =
    n.state = Failure && Re2.matches buildkite_is_failing_re (Option.default "" n.description)

  let is_success_build (n : Github_t.status_notification) =
    n.state = Success && Re2.matches buildkite_is_success_re (Option.default "" n.description)

  let new_failed_steps (n : Github_t.status_notification) (repo_state : State_t.repo_state) =
    if not (is_failed_build n) then failwith "Error: new_failed_steps fn must be called on a finished build";
    match n.target_url with
    | None ->
      (* if we don't have a target_url value, we don't have a build number and cant't track build state *)
      []
    | Some build_url ->
      let { pipeline_name; _ } = parse_context_exn ~context:n.context in
      (match n.state = Failure, n.branches with
      | false, _ -> []
      | true, [ branch ] ->
        (match StringMap.find_opt pipeline_name repo_state.pipeline_statuses with
        | Some branches_statuses ->
          (match StringMap.find_opt branch.name branches_statuses with
          | Some builds_maps ->
            let current_build_number = get_build_number_exn ~build_url in
            let to_previous_failed_steps n build_number (build_status : State_t.build_status) acc =
              match build_number >= n with
              | true -> acc
              | false -> build_status.failed_steps @ acc
            in
            let previous_failed_steps =
              IntMap.fold (to_previous_failed_steps current_build_number) builds_maps []
              |> List.sort_uniq (fun (s1 : State_t.failed_step) (s2 : State_t.failed_step) ->
                     String.compare s1.name s2.name)
            in
            let current_build =
              try IntMap.find current_build_number builds_maps
              with _ ->
                (* edge case: we got a notification for a build that ran longer than the defined threshold
                   and was cleaned from state. This shouldn't happen, but adding an error message to make
                   clearer what is happening if it does. *)
                failwith "Error: failed to find current build in state, maybe it was cleaned up?"
            in
            List.filter
              (fun (step : State_t.failed_step) ->
                not @@ List.exists (fun (prev : State_t.failed_step) -> prev.name = step.name) previous_failed_steps)
              current_build.failed_steps
          | None -> [])
        | None -> [])
      | true, _ -> [])

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
  }

  let now = Ptime_clock.now

  let create ?(ttl = Build.stale_build_threshold) () : t = { table = Hashtbl.create 128; ttl; last_purge = now () }

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

  (* Get an entry from the cache, purging if the ttl has passed *)
  let get cache key =
    let should_purge cache =
      match add_span cache.last_purge cache.ttl with
      | Some threshold_time -> is_later ~than:threshold_time (now ())
      | None -> false
    in
    if should_purge cache then purge cache;
    match Hashtbl.find_opt cache.table key with
    | Some entry -> Some entry.value
    | None -> None
end
