open Base
open Common
open Devkit

let empty : State_t.state = { pipeline_statuses = StringMap.empty }

(** take the cross product of build steps and branches, and set each entry
    to that build step's status state  *)
let refresh_pipeline_status (state : State_t.state) ~branches ~statuses =
  (* give a branch the specified status *)
  let set_status_of_branch status_state branch_statuses (branch : Github_t.branch) =
    Map.set branch_statuses ~key:branch.name ~data:status_state
  in
  (* give all branches in a branch list the specified status state *)
  let set_status_of_branches status_state ?(branch_statuses = StringMap.empty) branches =
    List.fold_left branches ~init:branch_statuses ~f:(set_status_of_branch status_state)
  in
  (* give all branches in a branch list the specified status, for a given build step *)
  let set_status_of_build_step branches pipeline_statuses (build_step_status : Github_t.api_status) =
    Map.update pipeline_statuses build_step_status.context ~f:(fun branch_statuses ->
      set_status_of_branches build_step_status.state ?branch_statuses branches)
  in
  state.pipeline_statuses <-
    List.fold_left statuses ~init:state.pipeline_statuses ~f:(set_status_of_build_step branches)

let log = Log.from "state"

let save state path =
  let data = State_j.string_of_state state |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string in
  match write_to_local_file ~data path with
  | Ok () -> Lwt.return @@ Ok ()
  | Error e -> Lwt.return @@ fmt_error "error while writing to local file %s: %s\nfailed to save state" path e
