open Monorobotlib

let push_fixture = Filename.concat Filename.parent_dir_name "mock_payloads/push.tag_release.json"

let push_repo_url () =
  let n = Github_j.commit_pushed_notification_of_string (Std.input_file push_fixture) in
  n.repository.url

let () =
  let expected = push_repo_url () in
  let actual = Util.Webhook.git_ssh_to_https "git@github.com:ahrefs/monorepo.git" in
  if String.equal expected actual then Printf.printf "PASS push and buildkite agree on %s\n" expected
  else begin
    Printf.printf "FAIL push key %s, buildkite key %s\n" expected actual;
    exit 1
  end
