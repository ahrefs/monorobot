open Common
open Devkit

let log = Log.from "state"

type t = { state : State_t.state }

let empty_repo_state () : State_t.repo_state =
  { pipeline_statuses = StringMap.empty; pipeline_commits = StringMap.empty; slack_threads = StringMap.empty }

let empty () : t =
  let state = State_t.{ repos = Stringtbl.empty (); bot_user_id = None } in
  { state }

let find_or_add_repo' state repo_url =
  match Stringtbl.find_opt state.State_t.repos repo_url with
  | Some repo -> repo
  | None ->
    let new_repo = empty_repo_state () in
    Stringtbl.add state.State_t.repos repo_url new_repo;
    new_repo

let set_repo_state { state } repo_url repo_state = Stringtbl.replace state.repos repo_url repo_state
let find_or_add_repo { state } repo_url = find_or_add_repo' state repo_url

(** Updates the builds map in the branches statuses.
     [default_builds_map] (optional, defaults to an empty map) is the builds map to use if we don't have the current
     branch in the branches statuses.
     [f] is the function to use to update the builds map. Takes the [State_t.build_status] Map for the current branch
     as argument.
     [branches_statuses] is the branches statuses Map to update. Returns the updated branches statuses Map. *)
let update_builds_in_branches ~branches ?(default_builds_map = IntMap.empty) ~f branches_statuses =
  let current_statuses = Option.default StringMap.empty branches_statuses in
  let updated_statuses =
    List.map
      (fun (branch : Github_t.branch) ->
        let builds_map =
          Option.map_default
            (fun branches_statuses ->
              match StringMap.find_opt branch.name branches_statuses with
              | Some builds_map -> f builds_map
              | None -> default_builds_map)
            default_builds_map branches_statuses
        in
        branch.name, builds_map)
      branches
  in
  Some (List.fold_left (fun m (key, data) -> StringMap.add key data m) current_statuses updated_statuses)

let set_repo_pipeline_status { state } (n : Github_t.status_notification) =
  match n.target_url with
  | None ->
    (* if we don't have a target_url value, we don't have a build number and cant't track build state *)
    ()
  | Some build_url ->
    let context = n.context in
    let { Util.Build.is_pipeline_step; pipeline_name } = Util.Build.parse_context_exn ~context in
    let build_number = Util.Build.get_build_number_exn ~build_url in
    let is_finished =
      match is_pipeline_step, n.state with
      | false, (Success | Failure | Error) -> true
      | _ -> false
    in
    let finished_at =
      match is_finished with
      | true -> Some (Timestamp.wrap_with_fallback n.updated_at)
      | false -> None
    in
    (* Even if this is an initial build state, we can't just set an "empty" state because we don't know
       the order we will get the status notifications in. *)
    let init_build_state =
      let commit =
        { State_t.sha = n.sha; author = n.commit.commit.author.email; commit_message = n.commit.commit.message }
      in
      let failed_steps =
        match is_pipeline_step, n.state with
        | true, Failure -> [ { State_t.name = n.context; build_url } ]
        | _ -> []
      in
      {
        State_t.status = n.state;
        build_number;
        build_url;
        commit;
        is_finished;
        failed_steps;
        created_at = Timestamp.wrap_with_fallback n.updated_at;
        finished_at;
      }
    in
    let update_build_status builds_map build_number =
      match IntMap.find_opt build_number builds_map with
      | Some ({ failed_steps; is_finished; _ } as current_build_status : State_t.build_status) ->
        let failed_steps =
          match is_pipeline_step, n.state with
          | true, Failure -> { State_t.name = n.context; build_url } :: failed_steps
          | _ -> failed_steps
        in
        (* we need to figure out the value of is_finished here too because of step notifications that might
           arrive after the build notification and revert the value to false *)
        let is_finished =
          match is_pipeline_step, n.state with
          | false, (Success | Failure | Error) -> true
          | _ -> is_finished
        in
        { current_build_status with status = n.state; is_finished; finished_at; failed_steps }
      | None -> init_build_state
    in
    let update_failing_build_status builds_map build_number =
      match IntMap.find_opt build_number builds_map with
      | Some (current_build_status : State_t.build_status) -> { current_build_status with status = Failure }
      | None -> init_build_state
    in
    let update_pipeline_status =
      update_builds_in_branches ~branches:n.branches
        ~default_builds_map:(IntMap.singleton build_number init_build_state) ~f:(fun builds_map ->
          let updated_status =
            match Util.Build.is_failing_build n with
            | true -> update_failing_build_status builds_map build_number
            | _ -> update_build_status builds_map build_number
          in
          IntMap.add build_number updated_status builds_map)
    in
    let rm_successful_build =
      update_builds_in_branches ~branches:n.branches ~f:(fun builds_map ->
          let threshold = Util.Build.stale_build_threshold in
          let is_past_threshold (build_status : State_t.build_status) threshold =
            let open Ptime in
            let now = Ptime_clock.now () in
            match add_span build_status.created_at threshold with
            | Some t_plus_threshold -> is_earlier ~than:now t_plus_threshold
            | None -> false
          in
          IntMap.remove build_number builds_map
          |> IntMap.filter (fun build_number' build_status ->
                 match build_status.State_t.is_finished, build_number' < build_number with
                 | true, true ->
                   (* remove all finished older builds *)
                   false
                 | false, true when is_past_threshold build_status threshold ->
                   (* remove older builds that ran for longer than the threshold,
                      or for which we might not have received a final notification *)
                   false
                 | _ ->
                   (* keep all other builds *)
                   true))
    in
    let rm_successful_step =
      update_builds_in_branches ~branches:n.branches ~f:(fun builds_map ->
          IntMap.mapi
            (fun build_number' (build_status : State_t.build_status) ->
              let failed_steps =
                List.filter
                  (* remove the fixed step from previous finished builds *)
                    (fun (s : State_t.failed_step) ->
                    not (build_number' < build_number && s.name = n.context && build_status.is_finished))
                  build_status.failed_steps
              in
              { build_status with failed_steps })
            builds_map)
    in
    let repo_state = find_or_add_repo' state n.repository.url in
    (match n.state with
    | Success ->
      (* If a build step is successful, we remove it from the failed steps list of past builds.
         If old builds have no more failed steps, we remove them.
         If the whole build is successful, we remove it from state to avoid the state file on disk growing too much.
      *)
      (match is_pipeline_step with
      | true ->
        repo_state.pipeline_statuses <- StringMap.update pipeline_name rm_successful_step repo_state.pipeline_statuses
      | false ->
        repo_state.pipeline_statuses <- StringMap.update pipeline_name rm_successful_build repo_state.pipeline_statuses)
    | Error | Failure | Pending ->
      repo_state.pipeline_statuses <- StringMap.update pipeline_name update_pipeline_status repo_state.pipeline_statuses)

let set_repo_pipeline_commit { state } (n : Github_t.status_notification) =
  let rotation_threshold = 1000 in
  let repo_state = find_or_add_repo' state n.repository.url in
  let pipeline_name =
    match Util.Build.parse_context ~context:n.context with
    | Some { pipeline_name; _ } ->
      (* We only want to track messages for the base pipeline, not the steps *)
      pipeline_name
    | None -> n.context
  in
  let to_branch_commits branches_commits (branch : Github_t.branch) =
    let single_commit = { State_t.s1 = StringSet.add n.sha StringSet.empty; s2 = StringSet.empty } in
    let updated_commits =
      Option.map_default
        (fun (branches_commits : State_t.commit_sets StringMap.t) ->
          match StringMap.find_opt branch.name branches_commits with
          | None -> single_commit
          | Some branch_commits ->
            let { State_t.s1; s2 } = branch_commits in
            let s1 = StringSet.add n.sha s1 in
            let s1, s2 = if StringSet.cardinal s1 > rotation_threshold then StringSet.empty, s1 else s1, s2 in
            { State_t.s1; s2 })
        single_commit branches_commits
    in
    branch.name, updated_commits
  in
  let set_commit commits =
    let current_commits = Option.default StringMap.empty commits in
    let updated_commits = List.map (to_branch_commits commits) n.branches in
    Some (List.fold_left (fun m (key, data) -> StringMap.add key data m) current_commits updated_commits)
  in
  repo_state.pipeline_commits <- StringMap.update pipeline_name set_commit repo_state.pipeline_commits

let mem_repo_pipeline_commits { state } (n : Github_t.status_notification) =
  let pipeline_name =
    match Util.Build.parse_context ~context:n.context with
    | Some { pipeline_name; _ } ->
      (* We only want to track messages for the base pipeline, not the steps *)
      pipeline_name
    | None -> n.context
  in
  let repo_state = find_or_add_repo' state n.repository.url in
  match StringMap.find_opt pipeline_name repo_state.pipeline_commits with
  | None -> false
  | Some pipeline_commits ->
    List.exists
      (fun (b : Github_t.branch) ->
        match StringMap.find_opt b.name pipeline_commits with
        | None -> false
        | Some { State_t.s1; s2 } -> StringSet.mem n.sha s1 || StringSet.mem n.sha s2)
      n.branches

let has_pr_thread { state } ~repo_url ~pr_url =
  let repo_state = find_or_add_repo' state repo_url in
  StringMap.mem pr_url repo_state.slack_threads

let get_thread { state } ~repo_url ~pr_url channel =
  let repo_state = find_or_add_repo' state repo_url in
  match StringMap.find_opt pr_url repo_state.slack_threads with
  | None -> None
  | Some threads ->
    List.find_map
      (fun (thread : State_t.slack_thread) ->
        match Slack_channel.equal channel thread.channel with
        | false -> None
        | true -> Some thread)
      threads

let add_thread_if_new { state } ~repo_url ~pr_url (msg : State_t.slack_thread) =
  let repo_state = find_or_add_repo' state repo_url in
  let set_threads threads =
    match threads with
    | None -> Some [ msg ]
    | Some threads ->
    match
      List.exists (fun (thread : State_t.slack_thread) -> Slack_channel.equal msg.channel thread.channel) threads
    with
    | true -> Some threads
    | false -> Some (msg :: threads)
  in
  repo_state.slack_threads <- StringMap.update pr_url set_threads repo_state.slack_threads

let delete_thread { state } ~repo_url ~pr_url =
  let repo_state = find_or_add_repo' state repo_url in
  repo_state.slack_threads <- StringMap.remove pr_url repo_state.slack_threads

let set_bot_user_id { state; _ } user_id = state.State_t.bot_user_id <- Some user_id
let get_bot_user_id { state; _ } = state.State_t.bot_user_id

let save { state; _ } path =
  let data = State_j.string_of_state state |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string in
  try
    Files.save_as path (fun oc -> output_string oc data);
    Ok ()
  with exn -> Util.fmt_error ~exn "failed to save state to file %s" path
