open Monorobotlib
open Common

let repo_url = "https://github.com/ahrefs/monorepo"

let thread ~created_at : State_t.release_thread =
  {
    thread =
      {
        ts = Slack_timestamp.inject "1760000000.000100";
        channel = Slack_channel.Any.inject "software-status";
        cid = Slack_channel.Ident.inject "Csoftware";
      };
    created_at;
    build_url_posted = false;
    unblocked_jobs = [];
  }

let now = Ptime_clock.now ()

let ago days =
  match Ptime.sub_span now (Ptime.Span.of_int_s (days * 24 * 60 * 60)) with
  | Some t -> t
  | None -> failwith "bad span"

let check name cond =
  if cond then Printf.printf "PASS %s\n" name
  else begin
    Printf.printf "FAIL %s\n" name;
    exit 1
  end

let () =
  let state = State.empty () in
  let tag = "release-20261009-1632" in
  State.add_release_thread state ~repo_url ~tag (thread ~created_at:now);
  check "round trips" (State.get_release_thread state ~repo_url ~tag <> None);

  let build_url_posted () =
    match State.get_release_thread state ~repo_url ~tag with
    | Some t -> t.build_url_posted
    | None -> false
  in
  State.set_release_thread_build_url_posted state ~repo_url ~tag ~posted:true;
  check "flag is set" (build_url_posted ());

  State.set_release_thread_build_url_posted state ~repo_url ~tag ~posted:false;
  check "flag is cleared again" (not (build_url_posted ()));
  State.set_release_thread_build_url_posted state ~repo_url ~tag ~posted:true;

  let record job_id = State.record_release_job_unblocked state ~repo_url ~tag ~job_id in
  let recorded () =
    match State.get_release_thread state ~repo_url ~tag with
    | Some t -> List.sort compare t.unblocked_jobs
    | None -> []
  in
  check "recording a job for the first time claims it" (record "job-deploy");
  check "recording the same job again does not claim it" (not (record "job-deploy"));
  check "a different job is claimed despite the first" (record "job-revert");
  check "both jobs are remembered independently" (recorded () = [ "job-deploy"; "job-revert" ]);
  State.forget_release_job_unblocked state ~repo_url ~tag ~job_id:"job-deploy";
  check "forgetting a job drops only that one" (recorded () = [ "job-revert" ]);
  check "a forgotten job can be claimed again" (record "job-deploy");

  State.delete_release_thread state ~repo_url ~tag;
  check "deletes" (State.get_release_thread state ~repo_url ~tag = None);

  check "missing tag is none" (State.get_release_thread state ~repo_url ~tag:"release-nope" = None);

  State.add_release_thread state ~repo_url ~tag:"fresh" (thread ~created_at:now);
  State.add_release_thread state ~repo_url ~tag:"old" (thread ~created_at:(ago 8));
  State.purge_stale_release_threads state ~repo_url;
  check "purge keeps fresh" (State.get_release_thread state ~repo_url ~tag:"fresh" <> None);
  check "purge drops old" (State.get_release_thread state ~repo_url ~tag:"old" = None);

  let open Buildkite_t in
  List.iter
    (fun (state, expected) ->
      check
        (Printf.sprintf "%s is %s"
           (Buildkite_j.string_of_build_state state)
           (if expected then "terminal" else "not terminal"))
        (Bool.equal (State.release_build_is_terminal state) expected))
    [
      Canceled, true;
      Skipped, true;
      Not_run, true;
      Passed, false;
      Failed, false;
      Running, false;
      Blocked, false;
      Canceling, false;
      Failing, false;
      Finished, false;
      Scheduled, false;
      Other "something_new", false;
    ]
