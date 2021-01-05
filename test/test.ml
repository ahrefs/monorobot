open Base
open Lib
open Common

let log = Devkit.Log.from "test"

let () = Devkit.Log.set_loglevels "error"

let mock_payload_dir = Caml.Filename.concat Caml.Filename.parent_dir_name "mock_payloads"

let mock_state_dir = Caml.Filename.concat Caml.Filename.parent_dir_name "mock_states"

let mock_secrets_dir = Caml.Filename.concat Caml.Filename.parent_dir_name "mock_secrets"

let mock_config_dir = Caml.Filename.concat Caml.Filename.parent_dir_name "mock_config"

module Action_local = Action.Action (Api_local.Github) (Api_local.Slack)

let get_mock_payloads () =
  let files = Caml.Sys.readdir mock_payload_dir in
  Array.sort files ~compare:String.compare;
  Array.to_list files
  |> List.filter_map ~f:(fun fn -> Github.event_of_filename fn |> Option.map ~f:(fun kind -> kind, fn))
  |> List.map ~f:(fun (kind, fn) ->
       let payload_path = Caml.Filename.concat mock_payload_dir fn in
       let state_filepath =
         let path = Caml.Filename.concat mock_state_dir fn in
         if Caml.Sys.file_exists path then Some path else None
       in
       let secrets_filepath =
         let path = Caml.Filename.concat mock_secrets_dir fn in
         if Caml.Sys.file_exists path then Some path else None
       in
       let config_filename =
         let path = Caml.Filename.concat mock_config_dir fn in
         if Caml.Sys.file_exists path then Some fn else None
       in
       kind, payload_path, state_filepath, secrets_filepath, config_filename)

let process (kind, path, state_filepath, secrets_filepath, config_filename) =
  let make_test_context () =
    let secrets_filepath =
      Option.value ~default:(Caml.Filename.concat mock_secrets_dir Context.default_secrets_filepath) secrets_filepath
    in
    let ctx = Context.make ?config_filename ~secrets_filepath ?state_filepath () in
    match Context.refresh_state ctx with
    | Error e -> fmt_error "failed to read state: %s" e
    | Ok ctx ->
    match Context.refresh_secrets ctx with
    | Error e -> fmt_error "failed to read secrets: %s" e
    | Ok ctx -> Ok ctx
  in
  Stdio.printf "===== file %s =====\n" path;
  let headers = [ "x-github-event", kind ] in
  match get_local_file path with
  | Error e -> Lwt.return @@ log#error "failed to read %s: %s" path e
  | Ok event ->
  match make_test_context () with
  | Error e -> Lwt.return @@ log#error "%s" e
  | Ok ctx ->
    let%lwt _ctx = Action_local.process_github_notification ctx headers event in
    Lwt.return_unit

let () =
  let payloads = get_mock_payloads () in
  Lwt_main.run (Lwt_list.iter_s process payloads)
