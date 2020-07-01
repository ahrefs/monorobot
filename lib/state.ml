type branch_state = { mutable last_build_state : Github_t.status_state option }

type t = { branches : branch_state list }

let state_of_json (json_state : Notabot_t.state) : t =
  let branches =
    List.map
      (fun (branch : Notabot_t.branch_info) ->
        let last_build_state : Github_t.status_state option =
          match branch.last_build_state with
          | Some { success = true; failure = _; pending = _; error = _ } -> Some Success
          | Some { success = _; failure = true; pending = _; error = _ } -> Some Failure
          | Some { success = _; failure = _; pending = true; error = _ } -> Some Pending
          | Some { success = _; failure = _; pending = _; error = _ } -> Some Error
          | None -> None
        in
        { last_build_state })
      json_state.branches
  in
  { branches }

let json_of_state (state : t) : Notabot_t.state =
  let branches : Notabot_t.branch_info list =
    List.map
      (fun (branch : branch_state) ->
        let last_build_state_json s f p e : Notabot_t.status_state =
          { success = s; failure = f; pending = p; error = e }
        in
        let last_build_state : Notabot_t.status_state option =
          match branch.last_build_state with
          | None -> None
          | Some Success -> Some (last_build_state_json true false false false)
          | Some Failure -> Some (last_build_state_json false true false false)
          | Some Pending -> Some (last_build_state_json false false true false)
          | Some Error -> Some (last_build_state_json false false false true)
        in
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
