open Devkit

let log = Log.from "state"

let default_state : Notabot_t.state = { branches = [] }

let default_branch_state timestamp =
  let branch_info : Notabot_t.branch_info = { last_build_state = Failure; updated_at = timestamp } in
  branch_info

let get_branch_state name (state : Notabot_t.state) = List.assoc_opt name state.branches

let set_branch_state name (branch_state : Notabot_t.branch_info) (state : Notabot_t.state) : Notabot_t.state =
  let removed_list = List.remove_assoc name state.branches in
  { branches = (name, branch_state) :: removed_list }

let set_branch_last_build_state name build_state timestamp (state : Notabot_t.state) : Notabot_t.state =
  match get_branch_state name state with
  | None -> { branches = (name, default_branch_state timestamp) :: state.branches }
  | Some _ -> set_branch_state name { last_build_state = build_state; updated_at = timestamp } state

let build_state_of_status_state = function
  | Github_t.Success -> Notabot_t.Success
  | Failure | Pending | Error -> Failure

let update_state_status (state : Notabot_t.state) (n : Github_t.status_notification) =
  let last_build_state = build_state_of_status_state n.state in
  List.fold_left
    (fun state (b : Github_t.branch) -> set_branch_last_build_state b.name last_build_state n.updated_at state)
    state n.branches

let update_state (state : Notabot_t.state) event =
  match event with
  | Github.Push _ | Pull_request _ | PR_review _ | PR_review_comment _ | Issue _ | Issue_comment _ | Commit_comment _
  | Event _ ->
    state
  | Status n -> update_state_status state n

let load_unsafe path = Notabot_j.state_of_string @@ Stdio.In_channel.read_all path

let load ?(silent = false) path =
  try load_unsafe path
  with Sys_error _ ->
    ( match silent with
    | true -> ()
    | false -> log#info "unable to load state at '%s', falling back to default" path
    );
    default_state

let save path state =
  let str = Notabot_j.string_of_state state in
  Stdio.Out_channel.write_all path ~data:str
