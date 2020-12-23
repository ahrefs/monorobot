open Base
open Lib

let log = Devkit.Log.from "test"

let mock_payload_dir = Caml.Filename.concat Caml.Filename.parent_dir_name "mock_payloads"

let mock_state_dir = Caml.Filename.concat Caml.Filename.parent_dir_name "mock_states"

module Action_local = Action.Action (Api_local.Github) (Api_local.Slack)

let get_mock_payloads () =
  let files = Caml.Sys.readdir mock_payload_dir in
  Array.sort files ~compare:String.compare;
  Array.to_list files
  |> List.filter_map ~f:(fun fn -> Github.event_of_filename fn |> Option.map ~f:(fun kind -> kind, fn))
  |> List.map ~f:(fun (kind, fn) ->
       let payload_path = Caml.Filename.concat mock_payload_dir fn in
       let state_path = Caml.Filename.concat mock_state_dir fn in
       if Caml.Sys.file_exists state_path then kind, payload_path, Some state_path else kind, payload_path, None)

let process ~(ctx : Context.t) (kind, path, state_path) =
  let%lwt ctx =
    match state_path with
    | None -> Lwt.return { ctx with state = State.empty }
    | Some state_path ->
    try
      let state = state_path |> Common.get_local_file |> State_j.state_of_string in
      Lwt.return { ctx with state }
    with Sys_error e ->
      log#error "failed to read %s: %s" state_path e;
      Lwt.return ctx
  in
  Stdio.printf "===== file %s =====\n" path;
  let headers = [ "x-github-event", kind ] in
  try
    let event = Common.get_local_file path in
    let%lwt _ctx = Action_local.process_github_notification ctx headers event in
    Lwt.return_unit
  with Sys_error e -> Lwt.return @@ log#error "failed to read %s: %s" path e

let () =
  let payloads = get_mock_payloads () in
  let repo : Github_t.repository = { name = ""; full_name = ""; url = ""; commits_url = ""; contents_url = "" } in
  let ctx = Context.make ~state_filepath:"state.json" () in
  Lwt_main.run
    ( match%lwt Api_local.Github.get_config ~ctx ~repo with
    | Error e ->
      log#error "%s" e;
      Lwt.return_unit
    | Ok config ->
      (* can remove this wrapper once status_rules doesn't depend on Config.t *)
      let config = Config.make config in
      let ctx = { ctx with config = Some config } in
      ( match Context.refresh_secrets ctx with
      | Ok ctx -> Lwt_list.iter_s (process ~ctx) payloads
      | Error e ->
        log#error "failed to read secrets:";
        log#error "%s" e;
        Lwt.return_unit
      )
    )
