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
  mutable config : Config_t.config option;
  state : State_t.state;
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

(** `is_pipeline_allowed ctx p` returns `true` if ctx.config.status_rules
    doesn't define a whitelist of allowed pipelines, or if the list
    contains pipeline `p`; returns `false` otherwise. *)
let is_pipeline_allowed ctx ~pipeline =
  match ctx.config with
  | None -> false
  | Some config ->
  match config.status_rules.allowed_pipelines with
  | Some allowed_pipelines when not @@ List.exists allowed_pipelines ~f:(String.equal pipeline) -> false
  | _ -> true

let refresh_pipeline_status ctx ~pipeline ~(branches : Github_t.branch list) ~status =
  if is_pipeline_allowed ctx ~pipeline then State.refresh_pipeline_status ctx.state ~pipeline ~branches ~status else ()

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
    if Caml.Sys.file_exists path then begin
      log#info "loading saved state from file %s" path;
      match get_local_file path with
      | Error e -> fmt_error "error while getting local file: %s\nfailed to get state from file %s" e path
      | Ok file ->
        let state = State_j.state_of_string file in
        Ok { ctx with state }
    end
    else Ok ctx

let print_config ctx =
  let cfg = get_config_exn ctx in
  let secrets = get_secrets_exn ctx in
  log#info "using prefix routing:";
  Rule.Prefix.print_prefix_routing cfg.prefix_rules.rules;
  log#info "using label routing:";
  Rule.Label.print_label_routing cfg.label_rules.rules;
  log#info "signature checking %s" (if Option.is_some secrets.gh_hook_token then "enabled" else "disabled")
