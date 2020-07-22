let log = Devkit.Log.from "context"

type data = {
  mutable cfg_file_path : string option;
  mutable cfg_remote_url : string option;
  mutable cfg_current_source : Config.cfg_sources option;
  action_after_cfg_refresh : Config.t -> unit;
  secrets : string;
  state : string;
  disable_write : bool;
}

type t = {
  mutable state : Notabot_t.state;
  mutable cfg : Config.t option;
  secrets : Notabot_t.secrets;
  data : data;
}

let _get_remote_cfg_json ?gh_token ?req () =
  match gh_token with
  | None -> None
  | Some token ->
  match req with
  | None -> None
  | Some r -> Lwt_main.run (Github.load_config_json token r)

let _get_local_cfg_json ?cfg_path () =
  match cfg_path with
  | None -> None
  | Some path -> try Some (Config.load_config_file path) with Sys_error _ -> None

let _resolve_cfg_json ?cfg_path ?gh_token ?req () =
  match _get_remote_cfg_json ?gh_token ?req () with
  | Some _ as cs -> cs
  | None -> _get_local_cfg_json ?cfg_path ()

let _update_ctx_cfg_data ctx = function
  | Config.Local, path, _ ->
    ctx.data.cfg_file_path <- Some path;
    ctx.data.cfg_current_source <- Some Local
  | Remote, url, _ ->
    ctx.data.cfg_remote_url <- Some url;
    ctx.data.cfg_current_source <- Some Remote

let refresh_config ctx ?req () =
  match _resolve_cfg_json ?cfg_path:ctx.data.cfg_file_path ?gh_token:ctx.secrets.gh_token ?req () with
  | Some cs ->
    _update_ctx_cfg_data ctx cs;
    let cfg_json = Config.cfg_json_of_cfg_source cs in
    let cfg = Config.make cfg_json ctx.secrets in
    ctx.data.action_after_cfg_refresh cfg;
    ctx.cfg <- Some cfg
  | None -> log#warn "unable to resolve both local remote config json sources"

let refresh_and_get_config ctx ?req () =
  refresh_config ctx ?req ();
  ctx.cfg

let init_remote_config ctx req =
  match ctx.data.cfg_current_source with
  | Some Remote -> ()
  | None | Some Local ->
    refresh_config ctx ~req ();
    ( match ctx.data.cfg_current_source with
    | Some Local | None -> log#warn "failed to init_remote_config"
    | Some Remote -> ()
    )

let init_and_get_remote_config ctx req =
  init_remote_config ctx req;
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

let update_and_get_state ctx event =
  update_state ctx event;
  ctx.state

let make ~state_path ?cfg_path ~secrets_path ?(disable_write = false) ?(action_after_cfg_refresh = fun _ -> ()) ?req () =
  let state = State.load state_path in
  let secrets = Config.load_secrets_file secrets_path in
  let data =
    {
      cfg_file_path = cfg_path;
      cfg_remote_url = None;
      cfg_current_source = None;
      action_after_cfg_refresh;
      secrets = secrets_path;
      state = state_path;
      disable_write;
    }
  in
  let ctx = { cfg = None; state; secrets; data } in
  refresh_config ctx ?req ();
  ctx
