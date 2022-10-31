open Base
open Lib

let log = Devkit.Log.from "test"
let mock_payload_dir = Caml.Filename.concat Caml.Filename.parent_dir_name "mock_payloads"
let mock_state_dir = Caml.Filename.concat Caml.Filename.parent_dir_name "mock_states"
let mock_slack_event_dir = Caml.Filename.concat Caml.Filename.parent_dir_name "mock_slack_events"

module Action_local = Action.Action (Api_local.Github) (Api_local.Slack)

let get_sorted_files_from dir =
  let files = Caml.Sys.readdir dir in
  Array.sort files ~compare:String.compare;
  Array.to_list files

let get_mock_payloads () =
  get_sorted_files_from mock_payload_dir
  |> List.filter_map ~f:(fun fn -> Github.event_of_filename fn |> Option.map ~f:(fun kind -> kind, fn))
  |> List.map ~f:(fun (kind, fn) ->
       let payload_path = Caml.Filename.concat mock_payload_dir fn in
       let state_path = Caml.Filename.concat mock_state_dir fn in
       if Caml.Sys.file_exists state_path then kind, payload_path, Some state_path else kind, payload_path, None
     )

let get_mock_slack_events () =
  List.map (get_sorted_files_from mock_slack_event_dir) ~f:(Caml.Filename.concat mock_slack_event_dir)

let process_gh_payload ~(secrets : Config_t.secrets) ~config (kind, path, state_path) =
  let headers = [ "x-github-event", kind ] in
  let make_test_context event =
    let repo = Github.repo_of_notification @@ Github.parse_exn headers event in
    (* overwrite repo url in secrets with that of notification for this test case *)
    let secrets = { secrets with repos = [ { url = repo.url; gh_token = None; gh_hook_token = None } ] } in
    let ctx = Context.make () in
    ctx.secrets <- Some secrets;
    let%lwt _ = State.find_or_add_repo ctx.state repo.url in
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
      let%lwt () = State.set_repo_state ctx.state repo.url repo_state in
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

let process_slack_event ~(secrets : Config_t.secrets) path =
  let ctx = Context.make () in
  ctx.secrets <- Some secrets;
  State.set_bot_user_id ctx.state "bot_user";
  Stdio.printf "===== file %s =====\n" path;
  match Common.get_local_file path with
  | Error e -> Lwt.return @@ log#error "failed to read %s: %s" path e
  | Ok body ->
  match Slack_j.event_notification_of_string body with
  | Url_verification _ -> Lwt.return ()
  | Event_callback notification ->
  match notification.event with
  | Link_shared event ->
    let%lwt _ctx = Action_local.process_link_shared_event ctx event in
    Lwt.return_unit

let () =
  let payloads = get_mock_payloads () in
  let repo : Github_t.repository =
    {
      name = "";
      full_name = "";
      url = "";
      commits_url = "";
      contents_url = "";
      pulls_url = "";
      issues_url = "";
      compare_url = "";
      branches_url = "";
      releases_url = "";
    }
  in
  let ctx = Context.make ~state_filepath:"state.json" () in
  let slack_events = get_mock_slack_events () in
  Lwt_main.run
    ( match%lwt Api_local.Github.get_config ~ctx ~repo with
    | Error e ->
      log#error "%s" e;
      Lwt.return_unit
    | Ok config ->
    match Context.refresh_secrets ctx with
    | Ok ctx ->
      let%lwt () = Lwt_list.iter_s (process_gh_payload ~secrets:(Option.value_exn ctx.secrets) ~config) payloads in
      Lwt_list.iter_s (process_slack_event ~secrets:(Option.value_exn ctx.secrets)) slack_events
    | Error e ->
      log#error "failed to read secrets:";
      log#error "%s" e;
      Lwt.return_unit
    )
