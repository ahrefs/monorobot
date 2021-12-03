open Base
open Common
open Devkit

let empty_repo_state () : State_t.repo_state = { pipeline_statuses = StringMap.empty }

let empty () : State_t.state = { repos = Stringtbl.empty (); bot_user_id = None }

let find_or_add_repo (state : State_t.state) repo_url =
  Stringtbl.find_or_add state.repos repo_url ~default:empty_repo_state

let set_repo_pipeline_status (state : State_t.state) repo_url ~pipeline ~(branches : Github_t.branch list) ~status =
  let set_branch_status branch_statuses =
    let new_statuses = List.map branches ~f:(fun b -> b.name, status) in
    let init = Option.value branch_statuses ~default:(Map.empty (module String)) in
    List.fold_left new_statuses ~init ~f:(fun m (key, data) -> Map.set m ~key ~data)
  in
  let repo_state = find_or_add_repo state repo_url in
  repo_state.pipeline_statuses <- Map.update repo_state.pipeline_statuses pipeline ~f:set_branch_status

let log = Log.from "state"

let save state path =
  let data = State_j.string_of_state state |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string in
  match write_to_local_file ~data path with
  | Ok () -> Lwt.return @@ Ok ()
  | Error e -> Lwt.return @@ fmt_error "error while writing to local file %s: %s\nfailed to save state" path e
