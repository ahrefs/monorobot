open Devkit
module Chan_map = Map.Make (String)

type t = {
  chans : string Chan_map.t;
  prefix_rules : Notabot_t.prefix_rule list;
  label_rules : Notabot_t.label_config;
  gh_webhook_secret : string option;
  main_branch_name : string option;
}

let make (json_config : Notabot_t.config) =
  let chans =
    List.fold_left
      (fun acc (webhook : Notabot_t.webhook) ->
        match Chan_map.find_opt webhook.channel acc with
        | None -> Chan_map.add webhook.channel webhook.url acc
        | Some c -> Exn.fail "chan %s is defined multiple time in the config" c)
      Chan_map.empty json_config.slack_channels
  in
  {
    chans;
    prefix_rules = json_config.prefix_rules;
    label_rules = json_config.label_rules;
    gh_webhook_secret = json_config.gh_webhook_secret;
    main_branch_name = json_config.main_branch_name;
  }

let load path =
  let cfg = Notabot_j.config_of_string @@ Stdio.In_channel.read_all path in
  make cfg
