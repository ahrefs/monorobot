open Base
open Common
open Devkit

let empty : State_t.state = { pipeline_statuses = StringMap.empty }

let refresh_pipeline_status (state : State_t.state) ~pipeline ~(branches : Github_t.branch list) ~status =
  let update_pipeline_status branch_statuses =
    let new_statuses = List.map branches ~f:(fun b -> b.name, status) in
    let init = Option.value branch_statuses ~default:(Map.empty (module String)) in
    List.fold_left new_statuses ~init ~f:(fun m (key, data) -> Map.set m ~key ~data)
  in
  state.pipeline_statuses <- Map.update state.pipeline_statuses pipeline ~f:update_pipeline_status

let log = Log.from "state"

let save state path =
  let data = State_j.string_of_state state |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string in
  match write_to_local_file ~data path with
  | Ok () -> Lwt.return @@ Ok ()
  | Error e -> Lwt.return @@ fmt_error "error while writing to local file %s: %s\nfailed to save state" path e
