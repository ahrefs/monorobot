type t = {
  mutable state : Notabot_t.state;
  mutable cfg : Config.t;
  load_state : unit -> Notabot_t.state;
  update_state : Github.t -> unit;
  load_config : unit -> Config.t;
}

let make ~state_path ~cfg_path ~secrets_path ?(disable_write = false) () =
  let load_config' () = Config.load cfg_path secrets_path in
  let load_state' () = State.load state_path in
  let update_state' state event = State.update_state state event in
  (* context values helper functions *)
  let cfg = load_config' () in
  let state = load_state' () in
  (* initial context values *)
  let cfg_setter r x = r.cfg <- x in
  let state_setter r x = r.state <- x in
  let set r setter x =
    setter r x;
    x
  in
  (* mutable field helper functions *)
  let rec load_config () = set r cfg_setter @@ load_config' ()
  and load_state () = set r state_setter @@ load_state' ()
  and update_state event =
    let act = if disable_write then ignore else State.save state_path in
    act @@ set r state_setter @@ update_state' r.state event
  and r = { load_config; cfg; load_state; update_state; state } in
  r
