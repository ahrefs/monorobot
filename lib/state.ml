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

let set_repo_pipeline_status { state } repo_url ~pipeline (notification : Github_t.status_notification) =
  let branches = notification.branches in
  let set_branch_status per_branch_statuses =
    let current_fail_state =
      match notification.state with
      | Failure | Error ->
        Some
          {
            State_t.sha = notification.sha;
            author = notification.commit.commit.author.email;
            commit_message = notification.commit.commit.message;
            last_updated = notification.updated_at;
            build_link = notification.target_url;
          }
      | _ -> None
    in
    let initial_build_status_state =
      { State_t.status = notification.state; original_failed_commit = None; current_failed_commit = None }
    in
    let new_statuses =
      List.map
        (fun (branch : Github_t.branch) ->
          let step_status =
            Option.map_default
              (fun pipeline_branches_statuses ->
                match StringMap.find_opt branch.name pipeline_branches_statuses with
                | Some (current_build_status : State_t.build_status) ->
                  let new_state = notification.state in
                  let original_failed_commit, current_failed_commit =
                    match new_state with
                    | Success -> None, None
                    | Pending ->
                      (* when new jobs are pending, we keep the existing state *)
                      current_build_status.original_failed_commit, current_build_status.current_failed_commit
                    | Failure | Error ->
                    (* if we don't have a failed step yet, set it *)
                    (* if we have a failed build and are retrying failed jobs: *)
                    (* - if we retried the original commit job, update the timestamp *)
                    (* - if we have a different commit that is failing that step, update the new failing commit *)
                    match current_build_status.original_failed_commit with
                    | None -> current_fail_state, None
                    | Some original_failed_commit ->
                    match original_failed_commit.sha = notification.sha with
                    | true -> current_fail_state, current_build_status.current_failed_commit
                    | false -> current_build_status.original_failed_commit, current_fail_state
                  in
                  { State_t.status = new_state; original_failed_commit; current_failed_commit }
                | None -> initial_build_status_state)
              initial_build_status_state per_branch_statuses
          in
          branch.name, step_status)
        branches
    in
    let init = Option.default StringMap.empty per_branch_statuses in
    Some (List.fold_left (fun m (key, data) -> StringMap.add key data m) init new_statuses)
  in
  let repo_state = find_or_add_repo' state repo_url in
  match notification.state with
  | Success ->
    (* if the build/step is successful, we remove it from the state to avoid the file to grow too much *)
    repo_state.pipeline_statuses <- StringMap.remove pipeline repo_state.pipeline_statuses
  | _ -> repo_state.pipeline_statuses <- StringMap.update pipeline set_branch_status repo_state.pipeline_statuses

let set_repo_pipeline_commit { state } repo_url ~pipeline ~commit =
  let rotation_threshold = 1000 in
  let repo_state = find_or_add_repo' state repo_url in
  let set_commit commits =
    let { State_t.s1; s2 } = Option.default { State_t.s1 = StringSet.empty; s2 = StringSet.empty } commits in
    let s1 = StringSet.add commit s1 in
    let s1, s2 = if StringSet.cardinal s1 > rotation_threshold then StringSet.empty, s1 else s1, s2 in
    Some { State_t.s1; s2 }
  in
  repo_state.pipeline_commits <- StringMap.update pipeline set_commit repo_state.pipeline_commits

let mem_repo_pipeline_commits { state } repo_url ~pipeline ~commit =
  let repo_state = find_or_add_repo' state repo_url in
  match StringMap.find_opt pipeline repo_state.pipeline_commits with
  | None -> false
  | Some { State_t.s1; s2 } -> StringSet.mem commit s1 || StringSet.mem commit s2

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
        match String.equal channel thread.channel with
        | false -> None
        | true -> Some thread)
      threads

let add_thread_if_new { state } ~repo_url ~pr_url (msg : State_t.slack_thread) =
  let repo_state = find_or_add_repo' state repo_url in
  let set_threads threads =
    match threads with
    | None -> Some [ msg ]
    | Some threads ->
    match List.exists (fun (thread : State_t.slack_thread) -> String.equal msg.channel thread.channel) threads with
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
