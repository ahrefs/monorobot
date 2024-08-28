open Common
open Devkit

let log = Log.from "state"

type t = { state : State_t.state }

let empty_repo_state () : State_t.repo_state =
  { pipeline_statuses = StringMap.empty; pipeline_commits = StringMap.empty }

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

let set_repo_pipeline_status { state } repo_url ~pipeline ~(branches : Github_t.branch list) ~status =
  let set_branch_status branch_statuses =
    let new_statuses = List.map (fun (b : Github_t.branch) -> b.name, status) branches in
    let init = Option.default StringMap.empty branch_statuses in
    Some (List.fold_left (fun m (key, data) -> StringMap.add key data m) init new_statuses)
  in
  let repo_state = find_or_add_repo' state repo_url in
  repo_state.pipeline_statuses <- StringMap.update pipeline set_branch_status repo_state.pipeline_statuses

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

let set_bot_user_id { state; _ } user_id = state.State_t.bot_user_id <- Some user_id
let get_bot_user_id { state; _ } = state.State_t.bot_user_id

let save { state; _ } path =
  let data = State_j.string_of_state state |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string in
  try
    Files.save_as path (fun oc -> output_string oc data);
    Ok ()
  with exn -> fmt_error ~exn "failed to save state to file %s" path
