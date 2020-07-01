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
        let branch_info : Notabot_t.branch_info = { name; last_build_state } in
        branch_info)
      state.branches
  in
  { branches }

let default_state = { branches = [] }

let default_branch_info name = { name; last_build_state = None }

let get_branch_state name state =
  match List.filter (fun branch -> branch.name = name) state.branches with
  | b :: _ -> Some b
  | [] -> None

let set_branch_state name branch_info state =
  let branches =
    match
      List.fold_left
        (fun (bs, found) b -> if b.name = name then branch_info :: bs, true else b :: bs, found)
        ([], false) state.branches
    with
    | bs, true -> bs
    | bs, false -> branch_info :: bs
  in
  { branches }

(* { state with branches = branches } *)

let load path =
  try state_of_json @@ Notabot_j.state_of_string @@ Stdio.In_channel.read_all path with Sys_error _ -> default_state

let save path state =
  let json = json_of_state state in
  let str = Notabot_j.string_of_state json in
  Stdio.Out_channel.write_all path ~data:str

(* TODO set values to records *)
