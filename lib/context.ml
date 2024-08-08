open Common
open Devkit

let log = Log.from "context"

exception Context_error of string

let context_error fmt = Printf.ksprintf (fun msg -> raise (Context_error msg)) fmt

type t = {
  config_filename : string;
  secrets_filepath : string;
  state_filepath : string option;
  mutable secrets : Config_t.secrets option;
  config : Config_t.config Stringtbl.t;
  state : State.t;
}

let default_config_filename = "monorobot.json"
let default_secrets_filepath = "secrets.json"

let make ?config_filename ?secrets_filepath ?state_filepath () =
  {
    config_filename = Option.default default_config_filename config_filename;
    secrets_filepath = Option.default default_secrets_filepath secrets_filepath;
    state_filepath;
    secrets = None;
    config = Stringtbl.empty ();
    state = State.empty ();
  }

let get_secrets_exn ctx =
  match ctx.secrets with
  | None -> context_error "secrets is uninitialized"
  | Some secrets -> secrets

let find_repo_config ctx repo_url = Stringtbl.find_opt ctx.config repo_url

let find_repo_config_exn ctx repo_url =
  match find_repo_config ctx repo_url with
  | None -> context_error "config uninitialized for repo %s" repo_url
  | Some config -> config

let set_repo_config ctx repo_url config = Stringtbl.replace ctx.config repo_url config

let gh_repo_of_secrets (secrets : Config_t.secrets) repo_url =
  let drop_scheme url =
    let uri = Uri.of_string url in
    Uri.with_scheme uri None
  in
  let repo_url = drop_scheme repo_url in
  match List.find_opt (fun r -> Uri.equal (drop_scheme r.Config_t.url) repo_url) secrets.repos with
  | None -> None
  | Some repo -> Some repo

let gh_token_of_secrets (secrets : Config_t.secrets) repo_url =
  match gh_repo_of_secrets secrets repo_url with
  | None -> None
  | Some repo -> repo.gh_token

let gh_hook_secret_token_of_secrets (secrets : Config_t.secrets) repo_url =
  match gh_repo_of_secrets secrets repo_url with
  | None -> None
  | Some repo -> repo.gh_hook_secret

let hook_of_channel ctx channel_name =
  let secrets = get_secrets_exn ctx in
  match
    List.find_opt (fun (webhook : Config_t.webhook) -> String.equal webhook.channel channel_name) secrets.slack_hooks
  with
  | Some hook -> Some hook.url
  | None -> None

(** [is_pipeline_allowed ctx repo_url ~pipeline] returns [true] if [status_rules]
    doesn't define a whitelist of allowed pipelines in the config of [repo_url],
    or if the list contains [pipeline]; returns [false] otherwise. *)
let is_pipeline_allowed ctx repo_url ~pipeline =
  match find_repo_config ctx repo_url with
  | None -> true
  | Some config ->
  match config.status_rules.allowed_pipelines with
  | Some allowed_pipelines when not @@ List.exists (String.equal pipeline) allowed_pipelines -> false
  | _ -> true

let refresh_secrets ctx =
  let path = ctx.secrets_filepath in
  match Config_j.secrets_of_string (Std.input_file path) with
  | exception exn -> fmt_error ~exn "failed to read secrets from file %s" path
  | secrets ->
  match secrets.slack_access_token, secrets.slack_hooks with
  | None, [] -> fmt_error "either slack_access_token or slack_hooks must be defined in file %s" path
  | _ ->
  match secrets.repos with
  | [] -> fmt_error "at least one repository url must be specified in the 'repos' list in file %s" path
  | _ :: _ ->
    ctx.secrets <- Some secrets;
    Ok ctx

let refresh_state ctx =
  match ctx.state_filepath with
  | None -> Ok ctx
  | Some path ->
    if Sys.file_exists path then begin
      log#info "loading saved state from file %s" path;
      (* todo: extract state related parts to state.ml *)
      match State_j.state_of_string (Std.input_file path) with
      | exception exn -> fmt_error ~exn "failed to read state from file %s" path
      | state -> Ok { ctx with state = { State.state } }
    end
    else Ok ctx

let print_config ctx repo_url =
  let cfg = find_repo_config_exn ctx repo_url in
  let secrets = get_secrets_exn ctx in
  let secret_token = gh_hook_secret_token_of_secrets secrets repo_url in
  log#info "using prefix routing:";
  Rule.Prefix.print_prefix_routing cfg.prefix_rules.rules;
  log#info "using label routing:";
  Rule.Label.print_label_routing cfg.label_rules.rules;
  log#info "signature checking %s" (if Option.is_some secret_token then "enabled" else "disabled")
