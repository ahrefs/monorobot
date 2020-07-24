let log = Devkit.Log.from "context"

exception Context_Error of string

type cfg_sources =
  | Local
  | Remote

type data = {
  mutable cfg_path : string;
  cfg_source : cfg_sources;
  cfg_action_after_refresh : Config.t -> unit;
  secrets_path : string;
  state_path : string;
  disable_write : bool;
}

type t = {
  mutable state : Notabot_t.state;
  mutable cfg : Config.t;
  secrets : Notabot_t.secrets;
  data : data;
}

type context_thunk = {
  secrets : Notabot_j.secrets;
  thunk : ?req:Github.t -> unit -> t;
  mutable ctx : t option;
}

let _get_secrets secrets_path = Config.load_secrets_file secrets_path

let _get_remote_cfg_json_url url = Lwt_main.run (Github.load_config_json url)

let _get_remote_cfg_json_req gh_token req = _get_remote_cfg_json_url @@ Github.get_remote_config_json_url gh_token req

let _get_local_cfg_json cfg_path = Config.load_config_file cfg_path

let _resolve_cfg_getter = function
  | Local -> _get_local_cfg_json
  | Remote -> _get_remote_cfg_json_url

let refresh_config ctx =
  let cfg_json = (_resolve_cfg_getter ctx.data.cfg_source) ctx.data.cfg_path in
  ctx.cfg <- Config.make cfg_json ctx.secrets

let refresh_and_get_config ctx =
  refresh_config ctx;
  ctx.cfg

let change_remote_url ctx req =
  match ctx.data.cfg_source with
  | Local -> raise @@ Context_Error "cant load remote config from a local context"
  | Remote ->
  match ctx.secrets.gh_token with
  | None -> raise @@ Context_Error "context must have `gh_token` to load remote config"
  | Some token ->
    ctx.data.cfg_path <- Github.get_remote_config_json_url token req;
    refresh_config ctx

let refresh_state ctx = ctx.state <- State.load ctx.data.state_path

let refresh_and_get_state ctx =
  refresh_state ctx;
  ctx.state

let update_state ctx event =
  ctx.state <- State.update_state ctx.state event;
  match ctx.data.disable_write with
  | false -> State.save ctx.data.state_path ctx.state
  | true -> ()

let update_and_get_state ctx event =
  update_state ctx event;
  ctx.state

let make_with_secrets ~state_path ?cfg_path ~secrets_path ~(secrets : Notabot_t.secrets) ?(disable_write = false)
  ?(cfg_action_after_refresh = fun _ -> ()) ?req ()
  =
  let data_cfg_path, cfg_source, cfg_json =
    match req with
    | None ->
      ( match cfg_path with
      | Some p -> p, Local, _get_local_cfg_json p
      | None -> raise @@ Context_Error "if ?req is not provided ?cfg_path must be provided"
      )
    | Some r ->
    match secrets.gh_token with
    | Some token -> Github.get_remote_config_json_url token r, Remote, _get_remote_cfg_json_req token r
    | None -> raise @@ Context_Error "if ?req is provided secrets must provide gh_token"
  in
  let data =
    { cfg_path = data_cfg_path; cfg_source; cfg_action_after_refresh; secrets_path; state_path; disable_write }
  in
  let cfg = Config.make cfg_json secrets in
  let state = State.load state_path in
  { cfg; state; secrets; data }

let make ~state_path ?cfg_path ~secrets_path ?(disable_write = false) ?(cfg_action_after_refresh = fun _ -> ()) ?req () =
  let secrets = _get_secrets secrets_path in
  make_with_secrets ~state_path ?cfg_path ~secrets_path ~secrets ~disable_write ~cfg_action_after_refresh ?req ()

let make_thunk ~state_path ?cfg_path ~secrets_path ?(disable_write = false) ?(cfg_action_after_refresh = fun _ -> ()) ()
  =
  let secrets = _get_secrets secrets_path in
  {
    secrets;
    thunk = make_with_secrets ~state_path ?cfg_path ~secrets_path ~secrets ~disable_write ~cfg_action_after_refresh;
    ctx = None;
  }

let resolve_ctx_in_thunk ctx_thunk req =
  match ctx_thunk.ctx with
  | Some ctx -> ctx
  | None ->
    let ctx = ctx_thunk.thunk ~req () in
    ctx_thunk.ctx <- Some ctx;
    ctx
