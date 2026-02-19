open Monorobotlib

(** Test blockquote extraction *)
let () =
  (* Simple quote *)
  let quoted, body = Slack_to_github.extract_blockquote "> quoted line\nreply text" in
  assert (quoted = "quoted line");
  assert (body = "reply text");

  (* Multiple quote lines *)
  let quoted, body = Slack_to_github.extract_blockquote "> line 1\n> line 2\nreply" in
  assert (quoted = "line 1\nline 2");
  assert (body = "reply");

  (* No quote *)
  let quoted, body = Slack_to_github.extract_blockquote "just a reply" in
  assert (quoted = "");
  assert (body = "just a reply");

  (* Quote only *)
  let quoted, body = Slack_to_github.extract_blockquote "> only a quote" in
  assert (quoted = "only a quote");
  assert (body = "");

  Printf.printf "blockquote extraction: OK\n"

(** Test mrkdwn to GitHub markdown conversion *)
let () =
  let resolve_user = function
    | "U12345" -> Some "octocat"
    | _ -> None
  in

  (* Bold conversion *)
  let result = Slack_to_github.to_github_markdown ~resolve_user "*bold text*" in
  assert (result = "**bold text**");

  (* Link conversion *)
  let result = Slack_to_github.to_github_markdown ~resolve_user "<https://example.com|Example>" in
  assert (result = "[Example](https://example.com)");

  (* User mention conversion *)
  let result = Slack_to_github.to_github_markdown ~resolve_user "<@U12345>" in
  assert (result = "@octocat");

  (* Unknown user mention *)
  let result = Slack_to_github.to_github_markdown ~resolve_user "<@U99999>" in
  assert (result = "@U99999");

  (* Channel mention conversion *)
  let result = Slack_to_github.to_github_markdown ~resolve_user "<#C12345|general>" in
  assert (result = "#general");

  (* Bare URL *)
  let result = Slack_to_github.to_github_markdown ~resolve_user "<https://example.com>" in
  assert (result = "https://example.com");

  Printf.printf "mrkdwn to markdown conversion: OK\n"

(** Test comment mapping state operations *)
let () =
  let open Common in
  let state = State.empty () in
  let repo_url = "https://github.com/test/repo" in
  let pr_url = "https://github.com/test/repo/pull/1" in

  (* Initially empty *)
  let msgs = State.get_pr_messages state ~repo_url ~pr_url in
  assert (msgs = []);

  (* Add a message *)
  State.add_pr_message state ~repo_url ~pr_url
    {
      State_t.slack_ts = Slack_timestamp.inject "1234.5678";
      github_comment_id = 42;
      comment_type = Review_comment;
      body = "test comment";
    };
  let msgs = State.get_pr_messages state ~repo_url ~pr_url in
  assert (List.length msgs = 1);
  assert ((List.hd msgs).github_comment_id = 42);

  (* Add another *)
  State.add_pr_message state ~repo_url ~pr_url
    {
      State_t.slack_ts = Slack_timestamp.inject "1234.5679";
      github_comment_id = 43;
      comment_type = Issue_comment;
      body = "another comment";
    };
  let msgs = State.get_pr_messages state ~repo_url ~pr_url in
  assert (List.length msgs = 2);

  (* Clear *)
  State.clear_pr_messages state ~repo_url ~pr_url;
  let msgs = State.get_pr_messages state ~repo_url ~pr_url in
  assert (msgs = []);

  Printf.printf "comment mapping state operations: OK\n"

(** Test cap at 100 entries *)
let () =
  let open Common in
  let state = State.empty () in
  let repo_url = "https://github.com/test/repo" in
  let pr_url = "https://github.com/test/repo/pull/2" in

  for i = 1 to 105 do
    State.add_pr_message state ~repo_url ~pr_url
      {
        State_t.slack_ts = Slack_timestamp.inject (Printf.sprintf "ts.%d" i);
        github_comment_id = i;
        comment_type = Issue_comment;
        body = Printf.sprintf "comment %d" i;
      }
  done;
  let msgs = State.get_pr_messages state ~repo_url ~pr_url in
  assert (List.length msgs = 100);
  (* Most recent should be first *)
  assert ((List.hd msgs).github_comment_id = 105);

  Printf.printf "comment mapping cap at 100: OK\n"

(** Test find_pr_by_thread *)
let () =
  let open Common in
  let state = State.empty () in
  let repo_url = "https://github.com/test/repo" in
  let pr_url = "https://github.com/test/repo/pull/3" in
  let channel_id = Slack_channel.Ident.inject "C12345" in
  let thread_ts = Slack_timestamp.inject "1234.5678" in

  (* No thread -> None *)
  let result = State.find_pr_by_thread state ~channel_id ~thread_ts in
  assert (result = None);

  (* Add a thread *)
  State.add_thread_if_new state ~repo_url ~pr_url
    { State_t.ts = thread_ts; channel = Slack_channel.to_any channel_id; cid = channel_id; merged_at = None };

  (* Now should find it *)
  let result = State.find_pr_by_thread state ~channel_id ~thread_ts in
  assert (result = Some (repo_url, pr_url));

  (* Wrong channel -> None *)
  let other_channel = Slack_channel.Ident.inject "C99999" in
  let result = State.find_pr_by_thread state ~channel_id:other_channel ~thread_ts in
  assert (result = None);

  (* Wrong ts -> None *)
  let other_ts = Slack_timestamp.inject "9999.0000" in
  let result = State.find_pr_by_thread state ~channel_id ~thread_ts:other_ts in
  assert (result = None);

  Printf.printf "find_pr_by_thread: OK\n"
