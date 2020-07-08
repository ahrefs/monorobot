module Context = struct
  type t = {
    mutable state : Notabot_t.state;
    mutable cfg : Config.t;
    load_state : unit -> Notabot_t.state;
    update_state : Notabot_t.state -> Github.t -> unit;
    get_config : string -> string -> Config.t;
  }

  let make_context ~state_path ~path ~secrets =
    let get_config path secrets = Config.load path secrets in
    let load_state () = State.load state_path in
    let rec r =
      lazy
        {
          get_config =
            (fun path secrets ->
              let cfg = get_config path secrets in
              let r = Lazy.force r in
              r.cfg <- cfg;
              cfg);
          cfg = get_config path secrets;
          load_state =
            (fun () ->
              let s = load_state () in
              let r = Lazy.force r in
              r.state <- s;
              s);
          update_state =
            (fun state event ->
              let s = State.update_state state event in
              let r = Lazy.force r in
              r.state <- s;
              State.save state_path s);
          state = load_state ();
        }
    in
    r
end
