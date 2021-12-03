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
       if Caml.Sys.file_exists state_path then kind, payload_path, Some state_path else kind, payload_path, None
     )

let process ~(secrets : Config_t.secrets) ~config (kind, path, state_path) =
  let headers = [ "x-github-event", kind ] in
  let make_test_context event =
    let repo = Github.repo_of_notification @@ Github.parse_exn headers event in
    let ctx = Context.make () in
    ctx.secrets <- Some secrets;
    ignore (State.find_or_add_repo ctx.state repo.url);
    match state_path with
    | None ->
      Context.set_repo_config ctx repo.url config;
      Lwt.return ctx
    | Some state_path ->
    match Common.get_local_file state_path with
    | Error e ->
      log#error "failed to read %s: %s" state_path e;
      Lwt.return ctx
    | Ok file ->
      let repo_state = State_j.repo_state_of_string file in
      Common.Stringtbl.set ctx.state.repos ~key:repo.url ~data:repo_state;
      Context.set_repo_config ctx repo.url config;
      Lwt.return ctx
  in
  Stdio.printf "===== file %s =====\n" path;
  let headers = [ "x-github-event", kind ] in
  match Common.get_local_file path with
  | Error e -> Lwt.return @@ log#error "failed to read %s: %s" path e
  | Ok event ->
    let%lwt ctx = make_test_context event in
    let%lwt _ctx = Action_local.process_github_notification ctx headers event in
    Lwt.return_unit

let () =
  let payloads = get_mock_payloads () in
  let repo : Github_t.repository =
    { name = ""; full_name = ""; url = ""; commits_url = ""; contents_url = ""; pulls_url = ""; issues_url = "" }
  in
  let ctx = Context.make ~state_filepath:"state.json" () in
  Lwt_main.run
    ( match%lwt Api_local.Github.get_config ~ctx ~repo with
    | Error e ->
      log#error "%s" e;
      Lwt.return_unit
    | Ok config ->
    match Context.refresh_secrets ctx with
    | Ok ctx -> Lwt_list.iter_s (process ~secrets:(Option.value_exn ctx.secrets) ~config) payloads
    | Error e ->
      log#error "failed to read secrets:";
      log#error "%s" e;
      Lwt.return_unit
    )
