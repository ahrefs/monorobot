open Devkit

let log = Log.from "state"

let empty : State_t.state = { pipeline_statuses = [] }

let get_branch_state name (state : State_t.state) = List.assoc_opt name state.pipeline_statuses

let set_branch_state name (branch_state : State_t.branch_info) (state : State_t.state) : State_t.state =
  let removed_list = List.remove_assoc name state.pipeline_statuses in
  { pipeline_statuses = (name, branch_state) :: removed_list }

let set_branch_last_build_state name build_state timestamp (state : State_t.state) : State_t.state =
  match get_branch_state name state with
  | None ->
    {
      pipeline_statuses = (name, { last_build_state = build_state; updated_at = timestamp }) :: state.pipeline_statuses;
    }
  | Some _ -> set_branch_state name { last_build_state = build_state; updated_at = timestamp } state

let build_state_of_status_state = function
  | Github_t.Success -> State_t.Success
  | Failure | Pending | Error -> Failure

let refresh_pipeline_status (state : State_t.state) ~pipeline:_ ~(branches : Github_t.branch list) ~status ~updated_at =
  let last_build_state = build_state_of_status_state status in
  List.fold_left
    (fun state (b : Github_t.branch) -> set_branch_last_build_state b.name last_build_state updated_at state)
    state branches

let save path state =
  let str = State_j.string_of_state state in
  Stdio.Out_channel.write_all path ~data:str
