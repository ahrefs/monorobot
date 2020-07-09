type t = {
  mutable state : Notabot_t.state;
  mutable cfg : Config.t;
  load_state : unit -> Notabot_t.state;
  update_state : Notabot_t.state -> Github.t -> unit;
  get_config : unit -> Config.t;
}

let make ~state_path ~path ~secrets =
  let get_config' () = Config.load path secrets in
  let load_state' () = State.load state_path in

  let cfg_setter r x = r.cfg <- x in
  let state_setter r x = r.state <- x in
  let set_and_id r setter x =
    setter r x;
    x
  in

  let rec get_config () = set_and_id r cfg_setter @@ get_config' ()
  and cfg = get_config' ()
  and load_state () = set_and_id r state_setter @@ load_state' ()
  and update_state state event = State.save state_path @@ set_and_id r state_setter @@ State.update_state state event
  and state = load_state' ()
  and r = { get_config; cfg; load_state; update_state; state } in
  r
