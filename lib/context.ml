open Devkit

let log = Log.from "context"

type data = {
  cfg : string option;
  secrets : string;
  state : string;
  disable_write : bool;
}

type t = {
  mutable state : Notabot_t.state;
  mutable cfg : Config.t;
  secrets : Notabot_t.secrets;
  data : data;
}

(* order of resolution: remote, local file, failure *)
let _resolve_cfg_json ?cfg_path ?gh_token ?req () =
  let remote_resolution =
    match gh_token with
    | None -> None
    | Some token ->
    match req with
    | None -> None
    | Some r ->
    match Lwt_main.run (Github.load_config token r) with
    | Some cfg -> Some cfg
    | None -> None
  in
  match remote_resolution with
  | Some cfg -> Some cfg
  | None ->
  match cfg_path with
  | Some path -> Some (Config.load_config_file path)
  | None -> None

let refresh_config ctx ?req () =
  match _resolve_cfg_json ?cfg_path:ctx.data.cfg ?gh_token:ctx.secrets.gh_token ?req () with
  | Some cfg_json -> ctx.cfg <- Config.make cfg_json ctx.secrets
  | None -> log#error "context malformed unable to resolve config json"

let refresh_and_get_config ctx ?req () =
  refresh_config ctx ?req ();
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

let make ~state_path ?cfg_path ~secrets_path ?(disable_write = false) ?req () =
  let state = State.load state_path in
  let secrets = Config.load_secrets_file secrets_path in
  let data = { cfg = cfg_path; secrets = secrets_path; state = state_path; disable_write } in
  match _resolve_cfg_json ?cfg_path ?gh_token:secrets.gh_token ?req () with
  | Some cfg_json ->
    let cfg = Config.make cfg_json secrets in
    Some { cfg; state; secrets; data }
  | None -> None
