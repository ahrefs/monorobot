type branch_state = { last_build_state : Notabot_t.build_state option }

type t = { branches : branch_state list }

let state_of_json (json_state : Notabot_t.state) : t =
  let branches =
    List.map
      (fun (branch : Notabot_t.branch_info) ->
        let last_build_state = branch.last_build_state in
        { last_build_state })
      json_state.branches
  in
  { branches }

let json_of_state (state : t) : Notabot_t.state =
  let branches : Notabot_t.branch_info list =
    List.map
      (fun (branch : branch_state) ->
        let last_build_state : Notabot_t.build_state option = branch.last_build_state in
        let branch_info : Notabot_t.branch_info = { last_build_state } in
        branch_info)
      state.branches
  in
  { branches }

let load path =
  let state = Notabot_j.state_of_string @@ Stdio.In_channel.read_all path in
  state_of_json state

let save path state =
  let json = json_of_state state in
  let str = Notabot_j.string_of_state json in
  Stdio.Out_channel.write_all path ~data:str
