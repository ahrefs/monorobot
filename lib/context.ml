open Base
open Common
open Devkit

exception Context_error of string

let context_error fmt = Printf.ksprintf (fun msg -> raise (Context_error msg)) fmt

type t = {
  config_filename : string;
  secrets_filepath : string;
  state_filepath : string option;
  mutable secrets : Config_t.secrets option;
  config : Config_t.config Table.t;
  state : State_t.state;
}

let default_config_filename = "monorobot.json"

let default_secrets_filepath = "secrets.json"

let make ?config_filename ?secrets_filepath ?state_filepath () =
  {
    config_filename = Option.value config_filename ~default:default_config_filename;
    secrets_filepath = Option.value secrets_filepath ~default:default_secrets_filepath;
    state_filepath;
    secrets = None;
    config = Table.empty ();
    state = State.empty ();
  }

let get_secrets_exn ctx =
  match ctx.secrets with
  | None -> context_error "secrets is uninitialized"
  | Some secrets -> secrets

let find_repo_config ctx repo_url = Hashtbl.find ctx.config repo_url

let find_repo_config_exn ctx repo_url =
  match find_repo_config ctx repo_url with
  | None -> context_error "config uninitialized for repo %s" repo_url
  | Some config -> config

let set_repo_config ctx repo_url config = Hashtbl.set ctx.config ~key:repo_url ~data:config

let gh_token_of_secrets (secrets : Config_t.secrets) repo_url =
  match Map.find secrets.repos repo_url with
  | None -> secrets.gh_token
  | Some repo_secrets -> repo_secrets.gh_token

let gh_hook_token_of_secrets (secrets : Config_t.secrets) repo_url =
  match Map.find secrets.repos repo_url with
  | None -> secrets.gh_hook_token
  | Some repo_secrets -> repo_secrets.gh_hook_token

let hook_of_channel ctx channel_name =
  let secrets = get_secrets_exn ctx in
  match List.find secrets.slack_hooks ~f:(fun webhook -> String.equal webhook.channel channel_name) with
  | Some hook -> Some hook.url
  | None -> None

(** `is_pipeline_allowed s r p` returns `true` if
    `status_rules` doesn't define a whitelist of allowed
    pipelines in the config of repo `r`, or if the list
    contains pipeline `p`; returns `false` otherwise. *)
let is_pipeline_allowed ctx repo_url ~pipeline =
  match find_repo_config ctx repo_url with
  | None -> false
  | Some config ->
  match config.status_rules.allowed_pipelines with
  | Some allowed_pipelines when not @@ List.exists allowed_pipelines ~f:(String.equal pipeline) -> false
  | _ -> true

let log = Log.from "context"

let refresh_secrets ctx =
  let path = ctx.secrets_filepath in
  match get_local_file path with
  | Error e -> fmt_error "error while getting local file: %s\nfailed to get secrets from file %s" e path
  | Ok file ->
    let secrets = Config_j.secrets_of_string file in
    begin
      match secrets.slack_access_token, secrets.slack_hooks with
      | None, [] -> fmt_error "either slack_access_token or slack_hooks must be defined in file '%s'" path
      | _ ->
        ctx.secrets <- Some secrets;
        Ok ctx
    end

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

let print_config ctx repo_url =
  let cfg = find_repo_config_exn ctx repo_url in
  let secrets = get_secrets_exn ctx in
  let token = gh_hook_token_of_secrets secrets repo_url in
  log#info "using prefix routing:";
  Rule.Prefix.print_prefix_routing cfg.prefix_rules.rules;
  log#info "using label routing:";
  Rule.Label.print_label_routing cfg.label_rules.rules;
  log#info "signature checking %s" (if Option.is_some token then "enabled" else "disabled")
