open Monorobotlib

let () = Devkit.Log.set_filter `Error

module Action_local = Action.Action (Api_local.Github) (Api_local.Slack_json) (Api_local.Buildkite)

let mock_buildkite_event_dir = Filename.concat Filename.parent_dir_name "mock_buildkite_events"
let mock_state_dir = Filename.concat Filename.parent_dir_name "mock_states"
let mock_payload_dir = Filename.concat Filename.parent_dir_name "mock_payloads"
let config_path = "monorobot.json"
let secrets_path = "secrets.json"

let config = Config_j.config_of_string (Std.input_file config_path)
let secrets = Config_j.secrets_of_string (Std.input_file secrets_path)

let check name cond =
  if cond then Printf.printf "PASS %s\n" name
  else begin
    Printf.printf "FAIL %s\n" name;
    exit 1
  end

let context_for ~repo_url =
  let ctx = Context.make () in
  ctx.secrets <- Some { secrets with repos = [ { url = repo_url; auth = None; gh_hook_secret = None } ] };
  Context.set_repo_config ctx repo_url config;
  ctx

let load_payload name =
  Buildkite_j.webhook_build_payload_of_string (Std.input_file (Filename.concat mock_buildkite_event_dir name))

let activated_payload_for ~job_id ~label =
  let n = load_payload "job.activated_deploy.json" in
  let job = Stdlib.Option.get n.job in
  Buildkite_j.string_of_webhook_build_payload { n with job = Some { job with id = job_id; label = Some label } }

let thread_after_events ~name events =
  let n = load_payload name in
  let repo_url = Util.Webhook.git_ssh_to_https n.pipeline.repository in
  let ctx = context_for ~repo_url in
  State.set_repo_state ctx.state repo_url
    (State_j.repo_state_of_string (Std.input_file (Filename.concat mock_state_dir name)));
  List.iter (fun body -> Lwt_main.run (Action_local.process_buildkite_webhook ctx [] body)) events;
  State.get_release_thread ctx.state ~repo_url ~tag:n.build.branch

let release_thread_after ~name =
  thread_after_events ~name [ Std.input_file (Filename.concat mock_buildkite_event_dir name) ]

let unblocked_jobs_after events =
  match thread_after_events ~name:"job.activated_deploy.json" events with
  | Some t -> t.unblocked_jobs
  | None -> []

let scheduled_payload_for ~branch =
  let n = load_payload "build.scheduled_matching.json" in
  Buildkite_j.string_of_webhook_build_payload { n with build = { n.build with branch } }

let announce_then_schedule ~push_fixture =
  let push_body = Std.input_file (Filename.concat mock_payload_dir push_fixture) in
  let push = Github_j.commit_pushed_notification_of_string push_body in
  let repo_url = push.repository.url in
  let tag = Stdlib.Option.get (Github.tag_of_ref push.ref) in
  let ctx = context_for ~repo_url in
  Lwt_main.run (Action_local.process_github_notification ctx [ "x-github-event", "push" ] push_body);
  let announced = State.get_release_thread ctx.state ~repo_url ~tag in
  Lwt_main.run (Action_local.process_buildkite_webhook ctx [] (scheduled_payload_for ~branch:tag));
  announced, State.get_release_thread ctx.state ~repo_url ~tag

let escapes_handler body =
  let ctx = context_for ~repo_url:"https://github.com/ahrefs/monorepo" in
  match Lwt_main.run (Action_local.process_buildkite_webhook ctx [] body) with
  | () -> false
  | exception _ -> true

let () =
  check "a payload that is not a build event does not escape the handler"
    (not (escapes_handler {|{"not": "a build payload"}|}));
  check "an unparseable body does not escape the handler" (not (escapes_handler "not json at all"));

  check "an unblock posts the build link the scheduled event never delivered"
    (release_thread_after ~name:"job.activated_deploy.json"
    |> Option.map_default (fun (t : State_t.release_thread) -> t.build_url_posted) false);

  check "release thread survives a blocked build" (release_thread_after ~name:"build.finished_blocked.json" <> None);
  check "release thread survives a passed build, which is also what a build waiting at a gate reports"
    (release_thread_after ~name:"build.finished_passed.json" <> None);
  check "release thread is gone after a canceled build"
    (release_thread_after ~name:"build.finished_canceled.json" = None);

  let deploy = activated_payload_for ~job_id:"job-deploy" ~label:"Deploy Ahrefs to prod" in
  let revert = activated_payload_for ~job_id:"job-revert" ~label:"Revert to previous release" in
  check "two different unblocks both announce"
    (List.sort compare (unblocked_jobs_after [ deploy; revert ]) = [ "job-deploy"; "job-revert" ]);
  check "a repeated delivery of the same unblock announces once"
    (unblocked_jobs_after [ deploy; deploy ] = [ "job-deploy" ]);
  check "a repeat does not suppress a later different unblock"
    (List.sort compare (unblocked_jobs_after [ deploy; deploy; revert ]) = [ "job-deploy"; "job-revert" ]);

  let announced, posted = announce_then_schedule ~push_fixture:"push.tag_release.json" in
  check "announcing a tag records its thread" (announced <> None);
  check "a scheduled build finds the thread the announcement recorded"
    (posted |> Option.map_default (fun (t : State_t.release_thread) -> t.build_url_posted) false)
