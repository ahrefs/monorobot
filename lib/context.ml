exception Context_Error of string

type cfg_make_args =
  | LocalMake of string
  | RemoteMake of string * Github.t

type cfg_sources =
  | Local
  | Remote

type data = {
  mutable cfg_path : string;
  mutable cfg_filename : string;
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
  thunk : ?req:Github.t -> unit -> t Lwt.t;
  mutable ctx : t option;
}

let get_secrets secrets_path = Config.load_secrets_file ~secrets_path

let get_remote_cfg_json_url url = Github.load_config_json url

let get_remote_cfg_json_req filename gh_token req =
  get_remote_cfg_json_url @@ Github.get_remote_config_json_url filename gh_token req

let get_local_cfg_json config_path = Lwt.return @@ Config.load_config_file ~config_path

let resolve_cfg_getter = function
  | Local -> get_local_cfg_json
  | Remote -> get_remote_cfg_json_url

let refresh_config ctx =
  let getter = resolve_cfg_getter ctx.data.cfg_source in
  let%lwt cfg_json = getter ctx.data.cfg_path in
  ctx.cfg <- Config.make cfg_json ctx.secrets;
  ctx.data.cfg_action_after_refresh ctx.cfg;
  Lwt.return_unit

let refresh_and_get_config ctx =
  let%lwt () = refresh_config ctx in
  Lwt.return ctx.cfg

let change_remote_url filename ctx req =
  match ctx.data.cfg_source with
  | Local -> raise @@ Context_Error "cant load remote config from a local context"
  | Remote ->
  match ctx.secrets.gh_token with
  | None -> raise @@ Context_Error "context must have `gh_token` to load remote config"
  | Some token ->
    ctx.data.cfg_filename <- filename;
    let url = Github.get_remote_config_json_url filename token req in
    ctx.data.cfg_path <- url;
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

let make_with_secrets ~state_path ~cfg_args ~secrets_path ~(secrets : Notabot_t.secrets) ?(disable_write = false)
  ?(cfg_action_after_refresh = fun _ -> ()) ()
  =
  let data_cfg_path, cfg_source, cfg_json, cfg_filename =
    match cfg_args with
    | LocalMake p -> p, Local, get_local_cfg_json p, p
    | RemoteMake (filename, r) ->
    match secrets.gh_token with
    | None -> raise @@ Context_Error "if RemoteMake cfg_args is provided secrets must provide gh_token"
    | Some token ->
      Github.get_remote_config_json_url filename token r, Remote, get_remote_cfg_json_req filename token r, filename
  in
  let%lwt cfg_json = cfg_json in
  let data =
    {
      cfg_path = data_cfg_path;
      cfg_source;
      cfg_action_after_refresh;
      secrets_path;
      state_path;
      disable_write;
      cfg_filename;
    }
  in
  let cfg = Config.make cfg_json secrets in
  let state = State.load state_path in
  Lwt.return { cfg; state; secrets; data }

let make ~state_path ~cfg_args ~secrets_path ?disable_write ?cfg_action_after_refresh () =
  let secrets = get_secrets secrets_path in
  make_with_secrets ~state_path ~cfg_args ~secrets_path ~secrets ?disable_write ?cfg_action_after_refresh ()

let make_thunk ~state_path ~cfg_path_or_remote_filename ~secrets_path ?disable_write ?cfg_action_after_refresh () =
  let secrets = get_secrets secrets_path in
  let make_args ?req () =
    match req with
    | None -> LocalMake cfg_path_or_remote_filename
    | Some r -> RemoteMake (cfg_path_or_remote_filename, r)
  in
  let thunk ?req () =
    make_with_secrets ~state_path ~cfg_args:(make_args ?req ()) ~secrets_path ~secrets ?disable_write
      ?cfg_action_after_refresh ()
  in
  { secrets; thunk; ctx = None }

let resolve_ctx_in_thunk ctx_thunk req =
  match ctx_thunk.ctx with
  | Some ctx -> Lwt.return ctx
  | None ->
    let%lwt ctx = ctx_thunk.thunk ~req () in
    let%lwt () = Lwt.return @@ (ctx_thunk.ctx <- Some ctx) in
    Lwt.return ctx
