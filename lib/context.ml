type data = {
  cfg : string;
  secrets : string;
  state : string;
  disable_write : bool;
}

type t = {
  mutable state : Notabot_t.state;
  mutable cfg : Config.t;
  data : data;
}

let refresh_config ctx = ctx.cfg <- Config.load ctx.data.cfg ctx.data.secrets

let refresh_and_get_config ctx =
  refresh_config ctx;
  ctx.cfg

let refresh_state ctx = ctx.state <- State.load ctx.data.state

let refresh_and_get_state ctx =
  refresh_state ctx;
  ctx.state

let update_state ctx event =
  ctx.state <- State.update_state ctx.state event;
  match ctx.data.disable_write with
  | false -> State.save ctx.data.state ctx.state
  | true -> ()

let make ~state_path ~cfg_path ~secrets_path ?(disable_write = false) () =
  let cfg = Config.load cfg_path secrets_path in
  let state = State.load state_path in
  let data = { cfg = cfg_path; secrets = secrets_path; state = state_path; disable_write } in
  { cfg; state; data }
