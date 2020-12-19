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

(* identical to underlying json except for status_rules *)
type t = {
  prefix_rules : Config_t.prefix_rules;
  label_rules : Config_t.label_rules;
  status_rules : status_rules;
  main_branch_name : string option;
}

let make (json_config : Config_t.config) =
  let status_rules : status_rules =
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
    prefix_rules = json_config.prefix_rules;
    label_rules = json_config.label_rules;
    main_branch_name = json_config.main_branch_name;
    status_rules;
  }
