type branch_state = {
  name : string;
  last_build_state : Notabot_t.build_state option;
}

type t = { branches : branch_state list }

let state_of_json (json_state : Notabot_t.state) : t =
  let branches =
    List.map
      (fun (branch : Notabot_t.branch_info) ->
        let name = branch.name in
        let last_build_state = branch.last_build_state in
        { name; last_build_state })
      json_state.branches
  in
  { branches }

let json_of_state (state : t) : Notabot_t.state =
  let branches : Notabot_t.branch_info list =
    List.map
      (fun (branch : branch_state) ->
        let name = branch.name in
        let last_build_state : Notabot_t.build_state option = branch.last_build_state in
        let branch_state : Notabot_t.branch_info = { name; last_build_state } in
        branch_state)
      state.branches
  in
  { branches }

let string_of_state state =
  let json = json_of_state state in
  Notabot_j.string_of_state json

let default_state = { branches = [] }

let default_branch_state name = { name; last_build_state = None }

let get_branch_state state name =
  match List.filter (fun branch -> branch.name = name) state.branches with
  | b :: _ -> Some b
  | [] -> None

let set_branch_state state branch_state =
  let branches =
    match
      List.fold_left
        (fun (bs, found) b -> if b.name = branch_state.name then branch_state :: bs, true else b :: bs, found)
        ([], false) state.branches
    with
    | bs, true -> bs
    | bs, false -> branch_state :: bs
  in
  { branches }

let update_state state_record event =
  match event with
  | Github.Push _ | Pull_request _ | PR_review _ | PR_review_comment _ | Issue _ | Issue_comment _ | Commit_comment _
  | Event _ ->
    state_record
  | Status n ->
    List.fold_left
      (fun s (b : Github_t.branch) ->
        let last_build_state : Notabot_t.build_state option =
          match n.state with
          | Success -> Some Success
          | _ -> Some Failure
        in
        let branch_state =
          match get_branch_state s b.name with
          | Some b -> b
          | None -> default_branch_state b.name
        in
        let branch_state' = { branch_state with last_build_state } in
        set_branch_state s branch_state')
      state_record n.branches

let load path = state_of_json @@ Notabot_j.state_of_string @@ Stdio.In_channel.read_all path

let save path state =
  let str = string_of_state state in
  Stdio.Out_channel.write_all path ~data:str
