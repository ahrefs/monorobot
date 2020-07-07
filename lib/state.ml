type t = { branches : (string * Notabot_t.branch_info) list }

let state_of_json (json_state : Notabot_t.state) : t =
  let branches = List.map (fun (branch : Notabot_t.branch_info) -> branch.name, branch) json_state.branches in
  { branches }

let json_of_state (state : t) : Notabot_t.state =
  let branches = List.map (fun (_, branch) -> branch) state.branches in
  { branches }

let string_of_state state =
  let json = json_of_state state in
  Notabot_j.string_of_state json

let default_state = { branches = [] }

let default_branch_state name =
  let branch_info : Notabot_t.branch_info = { name; last_build_state = Failure } in
  name, branch_info

let get_branch_state name state = List.assoc_opt name state.branches

let set_branch_state (branch_state : Notabot_t.branch_info) state =
  let removed_list = List.remove_assoc branch_state.name state.branches in
  { branches = (branch_state.name, branch_state) :: removed_list }

let set_branch_last_build_state name build_state state =
  match get_branch_state name state with
  | None -> { branches = default_branch_state name :: state.branches }
  | Some branch_state -> set_branch_state { branch_state with last_build_state = build_state } state

let update_state (state : t) event =
  match event with
  | Github.Push _ | Pull_request _ | PR_review _ | PR_review_comment _ | Issue _ | Issue_comment _ | Commit_comment _
  | Event _ ->
    state
  | Status n ->
    let last_build_state : Notabot_t.build_state =
      match n.state with
      | Success -> Success
      | Failure | Pending | Error -> Failure
    in
    List.fold_left
      (fun (state' : t) (b : Github_t.branch) -> set_branch_last_build_state b.name last_build_state state')
      state n.branches

let load path = state_of_json @@ Notabot_j.state_of_string @@ Stdio.In_channel.read_all path

let load_safe path = try load path with Sys_error _ -> default_state

let save path state =
  let str = string_of_state state in
  Stdio.Out_channel.write_all path ~data:str
