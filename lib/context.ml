open Base
open Common
open Devkit

type t = {
  config_filename : string;
  secrets_filepath : string;
  state_filepath : string option;
  gh_token : string option;
  gh_hook_token : string option;
  slack_hooks : Slack.channel_hook StringMap.t;
  mutable config : Config.t option;
  mutable state : State_t.state;
}

let default : t =
  {
    config_filename = "notabot.json";
    secrets_filepath = "secrets.json";
    state_filepath = None;
    gh_token = None;
    gh_hook_token = None;
    slack_hooks = StringMap.empty;
    config = None;
    state = State.empty;
  }

let make ?config_filename ?secrets_filepath ?state_filepath () =
  let config_filename = Option.value config_filename ~default:default.config_filename in
  let secrets_filepath = Option.value secrets_filepath ~default:default.secrets_filepath in
  { default with config_filename; secrets_filepath; state_filepath }

let hook_of_channel ctx channel_name = Map.find ctx.slack_hooks channel_name

let refresh_pipeline_status ctx ~pipeline ~(branches : Github_t.branch list) ~status ~updated_at =
  ctx.state <- State.refresh_pipeline_status ctx.state ~pipeline ~branches ~status ~updated_at

let log = Log.from "context"

let refresh_secrets ctx =
  let path = ctx.secrets_filepath in
  match%lwt get_local_file path with
  | Ok res ->
    let secrets = Config_j.secrets_of_string res in
    let slack_hooks =
      List.fold_left secrets.slack_hooks ~init:StringMap.empty ~f:(fun m hook ->
        Map.set m ~key:hook.channel ~data:hook.url)
    in
    let gh_token = secrets.gh_token in
    let gh_hook_token = secrets.gh_hook_token in
    Lwt.return @@ Ok { ctx with slack_hooks; gh_token; gh_hook_token }
  | Error e ->
    log#error "error while getting local file %s: %s" path e;
    Lwt.return @@ fmt_error "failed to get secrets from file %s" path

let refresh_state ctx =
  match ctx.state_filepath with
  | None -> Lwt.return @@ Ok ctx
  | Some path ->
    ( match%lwt get_local_file path with
    | Ok res ->
      log#info "loading saved state from file %s" path;
      let state = State_j.state_of_string res in
      Lwt.return @@ Ok { ctx with state }
    | Error e ->
      log#error "error while getting local file %s: %s" path e;
      Lwt.return @@ fmt_error "failed to get state from file %s" path
    )
