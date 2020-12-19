open Devkit
module Chan_map = Map.Make (String)

type config_status_state =
  | State of Github_t.status_state
  | Cancelled of string
  | HideConsecutiveSuccess

type status_rules = {
  title : string list option;
  status : config_status_state list;
}

type t = {
  chans : string Chan_map.t;
  prefix_rules : Config_t.prefix_rules;
  label_rules : Config_t.label_rules;
  gh_hook_token : string option;
  main_branch_name : string option;
  gh_token : string option;
  offline : string option;
  status_rules : status_rules;
}

let make (json_config : Config_t.config) (secrets : Config_t.secrets) =
  let chans =
    List.fold_left
      (fun acc (webhook : Config_t.webhook) ->
        match Chan_map.find_opt webhook.channel acc with
        | None -> Chan_map.add webhook.channel webhook.url acc
        | Some c -> Exn.fail "chan %s is defined multiple time in the config" c)
      Chan_map.empty secrets.slack_hooks
  in
  let () =
    List.iteri
      (fun i ({ channel_name; _ } : Config_t.prefix_rule) ->
        match Chan_map.find_opt channel_name chans with
        | None -> Exn.fail "chan %s in prefix_rules %d is missing from slack_hooks" channel_name i
        | Some _ -> ())
      json_config.prefix_rules.rules
  in
  let () =
    List.iteri
      (fun i ({ channel_name; _ } : Config_t.label_rule) ->
        match Chan_map.find_opt channel_name chans with
        | None -> Exn.fail "chan %s in labels_rules %d is missing from slack_hooks" channel_name i
        | Some _ -> ())
      json_config.label_rules.rules
  in
  let () =
    match json_config.prefix_rules.default_channel with
    | None -> ()
    | Some d ->
    match Chan_map.find_opt d chans with
    | None -> Exn.fail "default chan %s in prefix_rules is missing from slack_hooks" d
    | Some _ -> ()
  in
  let () =
    match json_config.label_rules.default_channel with
    | None -> ()
    | Some d ->
    match Chan_map.find_opt d chans with
    | None -> Exn.fail "default chan %s in label_rules is missing from slack_hooks" d
    | Some _ -> ()
  in
  let status_rules =
    let status =
      let open Github_t in
      let j = json_config.status_rules.rules in
      List.filter_map id
        [
          ( match j.success with
          | False -> None
          | True -> Some (State Success)
          | Once -> Some HideConsecutiveSuccess
          );
          (if j.failure then Some (State Failure) else None);
          (if j.pending then Some (State Pending) else None);
          (if j.error then Some (State Error) else None);
          ( match j.cancelled with
          | Some r -> Some (Cancelled r)
          | None -> None
          );
        ]
    in
    { title = json_config.status_rules.allowed_pipelines; status }
  in
  {
    chans;
    prefix_rules = json_config.prefix_rules;
    label_rules = json_config.label_rules;
    gh_hook_token = secrets.gh_hook_token;
    main_branch_name = json_config.main_branch_name;
    gh_token = secrets.gh_token;
    offline = json_config.offline;
    status_rules;
  }

let load_config_file ~config_path = Config_j.config_of_string @@ Stdio.In_channel.read_all config_path

let load_secrets_file ~secrets_path = Config_j.secrets_of_string @@ Stdio.In_channel.read_all secrets_path

let load ~config_path ~secrets_path =
  let config = load_config_file ~config_path in
  let secrets = load_secrets_file ~secrets_path in
  make config secrets
