open Base
open Common
open Devkit

exception Context_error of string

let context_error msg = raise (Context_error msg)

type t = {
  config_filename : string;
  secrets_filepath : string;
  state_filepath : string option;
  mutable secrets : Config_t.secrets option;
  mutable config : Config.t option;
  mutable state : State_t.state;
}

let default : t =
  {
    config_filename = "notabot.json";
    secrets_filepath = "secrets.json";
    state_filepath = None;
    secrets = None;
    config = None;
    state = State.empty;
  }

let make ?config_filename ?secrets_filepath ?state_filepath () =
  let config_filename = Option.value config_filename ~default:default.config_filename in
  let secrets_filepath = Option.value secrets_filepath ~default:default.secrets_filepath in
  { default with config_filename; secrets_filepath; state_filepath }

let get_secrets_exn ctx =
  match ctx.secrets with
  | None -> context_error "secrets is uninitialized"
  | Some secrets -> secrets

let get_config_exn ctx =
  match ctx.config with
  | None -> context_error "config is uninitialized"
  | Some config -> config

let hook_of_channel ctx channel_name =
  let secrets = get_secrets_exn ctx in
  match List.find secrets.slack_hooks ~f:(fun webhook -> String.equal webhook.channel channel_name) with
  | Some hook -> Some hook.url
  | None -> None

let refresh_pipeline_status ctx ~pipeline ~(branches : Github_t.branch list) ~status ~updated_at =
  ctx.state <- State.refresh_pipeline_status ctx.state ~pipeline ~branches ~status ~updated_at

let log = Log.from "context"

let refresh_secrets ctx =
  let path = ctx.secrets_filepath in
  match get_local_file path with
  | Error e -> fmt_error "error while getting local file: %s\nfailed to get secrets from file %s" e path
  | Ok file ->
    ctx.secrets <- Some (Config_j.secrets_of_string file);
    Ok ctx

let refresh_state ctx =
  match ctx.state_filepath with
  | None -> Ok ctx
  | Some path ->
    log#info "loading saved state from file %s" path;
    ( match get_local_file path with
    | Error e -> fmt_error "error while getting local file: %s\nfailed to get state from file %s" e path
    | Ok file ->
      let state = State_j.state_of_string file in
      Ok { ctx with state }
    )
