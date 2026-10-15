open Monorobotlib

let failures = ref 0

let check name expected actual =
  if String.equal expected actual then Printf.printf "PASS %s\n" name
  else begin
    Printf.printf "FAIL %s:\n  expected %S\n  got      %S\n" name expected actual;
    incr failures
  end

let message =
  "1a2b3c4d5e6f core: retry failed uploads instead of dropping them (#101)\n\
   2b3c4d5e6f70 docs: describe the config file layout\n\n\
   Previous-Release: release-20260929-1848\n"

let () =
  let parsed = Release.parse message in
  check "commit count" "2" (string_of_int (Release.commit_count parsed));
  check "first commit" "1a2b3c4d5e6f core: retry failed uploads instead of dropping them (#101)"
    (List.hd parsed.commits);
  check "previous release" "release-20260929-1848" (Stdlib.Option.value ~default:"none" parsed.previous_release);

  (* A tag cut before the trailer existed, or tagged by hand. *)
  let no_trailer = Release.parse "a57cc7ff42fd one thing\nbbbbbbbbbbbb another thing\n" in
  check "count without trailer" "2" (string_of_int (Release.commit_count no_trailer));
  check "no previous release" "none" (Stdlib.Option.value ~default:"none" no_trailer.previous_release);

  let empty = Release.parse "" in
  check "empty message has no commits" "0" (string_of_int (Release.commit_count empty));
  check "empty message has no previous release" "none" (Stdlib.Option.value ~default:"none" empty.previous_release);

  (* The trailer alone, with no commits, happens on a release that changed nothing. *)
  let trailer_only = Release.parse "Previous-Release: release-20260929-1848\n" in
  check "trailer only has no commits" "0" (string_of_int (Release.commit_count trailer_only));
  check "trailer only keeps the trailer" "release-20260929-1848"
    (Stdlib.Option.value ~default:"none" trailer_only.previous_release);

  (* make release -m "..." puts free text after a blank line. It is not a commit. *)
  let with_suffix = Release.parse "a one\nb two\n\nhotfix for login\n\nPrevious-Release: release-20260929-1848\n" in
  check "message suffix is not counted" "2" (string_of_int (Release.commit_count with_suffix));
  check "message suffix is kept as the note" "hotfix for login" (Stdlib.Option.value ~default:"none" with_suffix.note);
  check "no suffix means no note" "none" (Stdlib.Option.value ~default:"none" parsed.note);
  check "trailer only has no note" "none" (Stdlib.Option.value ~default:"none" trailer_only.note);
  check "message suffix keeps the trailer" "release-20260929-1848"
    (Stdlib.Option.value ~default:"none" with_suffix.previous_release);

  check "links a pull request reference"
    "3c4d5e6f7081 ci: cache the build dir (<https://github.com/ahrefs/monorobot/pull/102|#102>)"
    (Release.link_pull_requests ~repo_url:"https://github.com/ahrefs/monorobot"
       "3c4d5e6f7081 ci: cache the build dir (#102)");
  check "leaves a bare issue reference alone" "4d5e6f708192 core: drop the #39 workaround"
    (Release.link_pull_requests ~repo_url:"https://github.com/ahrefs/monorobot"
       "4d5e6f708192 core: drop the #39 workaround");

  let repo : Github_t.repository =
    {
      name = "monorepo";
      full_name = "ahrefs/monorobot";
      url = "https://github.com/ahrefs/monorobot";
      commits_url = "";
      contents_url = "";
      pulls_url = "";
      issues_url = "";
      compare_url = "";
    }
  in
  let rule : Rule_t.tag_rule =
    {
      prefix = "release-";
      channel_name = Common.Slack_channel.Name.inject "releases";
      label = "core";
      inline_commit_limit = Some 2;
      pipeline = None;
    }
  in
  (* generate_tag_notification passes the raw address; canonicalising is the
     caller's job in production. *)
  let slack_match_func = function
    | "dev.one@example.com" -> Some (Common.Slack_user_id.inject "U123")
    | _ -> None
  in
  let parsed = Release.parse message in
  let generate ?(release = Some parsed) () =
    Slack.generate_tag_notification ~slack_match_func ~repo ~rule ~tag:"release-20260930-0951" ~release
      ~tagger_email:(Some "dev.one@example.com") ~tagger_name:(Some "Dev One") ~sender_login:"dev-one_enterprise" ()
  in
  let req, reply = generate () in
  let text = Stdlib.Option.value ~default:"" req.Slack_t.text in
  check "parent mentions the releaser" "true" (string_of_bool (Devkit.Stre.exists text "<@U123>"));
  check "parent names the label" "true" (string_of_bool (Devkit.Stre.exists text "`core`"));
  check "parent carries the tag" "true" (string_of_bool (Devkit.Stre.exists text "release-20260930-0951"));
  check "parent carries the commit count" "true" (string_of_bool (Devkit.Stre.exists text "2 commits"));
  check "parent links the compare" "true"
    (string_of_bool (Devkit.Stre.exists text "/compare/release-20260929-1848...release-20260930-0951"));
  check "parent carries no note line when there is none" "1"
    (string_of_int (List.length (String.split_on_char '\n' text)));

  check "two commits are inline at a limit of two" "Inline"
    (match reply with
    | Some (Slack.Inline _) -> "Inline"
    | Some (Slack.Upload _) -> "Upload"
    | None -> "none");
  check "inline content is not fenced" "false"
    (string_of_bool
       (match reply with
       | Some (Slack.Inline content) -> Devkit.Stre.exists content "```"
       | _ -> true));
  check "inline content wraps each sha in inline code" "true"
    (string_of_bool
       (match reply with
       | Some (Slack.Inline content) -> Devkit.Stre.exists content "`1a2b3c4d5e6f` core: retry"
       | _ -> false));
  check "inline content links the pull request" "true"
    (string_of_bool
       (match reply with
       | Some (Slack.Inline content) -> Devkit.Stre.exists content "<https://github.com/ahrefs/monorobot/pull/101|#101>"
       | _ -> false));

  (* The -m note from release.sh is what the releaser says the release is for,
     which the commit subjects do not say. It is free text, so it is escaped. *)
  let noted =
    Release.parse "1a2b core: a change (#101)\n\nfrontend & api_docs\n\nPrevious-Release: release-20260929-1848\n"
  in
  let noted_req, _ = generate ~release:(Some noted) () in
  let noted_text = Stdlib.Option.value ~default:"" noted_req.Slack_t.text in
  check "note appears on its own line" "2" (string_of_int (List.length (String.split_on_char '\n' noted_text)));
  check "note is the second line" "frontend &amp; api_docs" (List.nth (String.split_on_char '\n' noted_text) 1);
  check "note does not disturb the headline" "true"
    (string_of_bool (Devkit.Stre.exists (List.nth (String.split_on_char '\n' noted_text) 0) "1 commit"));

  (* Past the inline limit the changelog becomes a file in the thread. *)
  let long = Release.parse "a one\nb two\nc three\nPrevious-Release: release-20260929-1848\n" in
  let _req, long_reply = generate ~release:(Some long) () in
  check "three commits at a limit of two upload" "Upload"
    (match long_reply with
    | Some (Slack.Inline _) -> "Inline"
    | Some (Slack.Upload _) -> "Upload"
    | None -> "none");
  (match long_reply with
  | Some (Slack.Upload ({ name; title; content; _ } : Slack.file)) ->
    check "upload file name carries the tag" "release-20260930-0951.txt" name;
    check "upload file title carries the tag" "release-20260930-0951 changelog" (Stdlib.Option.value ~default:"" title);
    check "upload content is the raw commit lines joined by newlines" "a one\nb two\nc three" content;
    check "upload content carries no link markup" "false" (string_of_bool (Devkit.Stre.exists content "<"))
  | _ -> check "upload reply is present" "Upload" "not Upload");

  (* An absent inline_commit_limit falls back to the default limit, not zero. *)
  let no_limit_rule = { rule with Rule_t.inline_commit_limit = None } in
  let _req, no_limit_reply =
    Slack.generate_tag_notification ~slack_match_func ~repo ~rule:no_limit_rule ~tag:"release-20260930-0951"
      ~release:(Some parsed) ~tagger_email:(Some "dev.one@example.com") ~tagger_name:(Some "Dev One")
      ~sender_login:"dev-one_enterprise" ()
  in
  check "no inline_commit_limit falls back to the default and stays inline" "Inline"
    (match no_limit_reply with
    | Some (Slack.Inline _) -> "Inline"
    | Some (Slack.Upload _) -> "Upload"
    | None -> "none");

  let escape_subject = "a57cc7ff42fd fix: handle <nil> in parser & reporter" in
  let _req, escape_inline_reply =
    Slack.generate_tag_notification ~slack_match_func ~repo ~rule ~tag:"release-20260930-0951"
      ~release:(Some (Release.parse (escape_subject ^ "\n")))
      ~tagger_email:(Some "dev.one@example.com") ~tagger_name:(Some "Dev One") ~sender_login:"dev-one_enterprise" ()
  in
  check "inline content escapes mrkdwn-sensitive characters" "true"
    (string_of_bool
       (match escape_inline_reply with
       | Some (Slack.Inline content) -> Devkit.Stre.exists content "fix: handle &lt;nil&gt; in parser &amp; reporter"
       | _ -> false));
  let escape_upload_release = Release.parse (Printf.sprintf "a one\n%s\nc three\n" escape_subject) in
  let _req, escape_upload_reply =
    Slack.generate_tag_notification ~slack_match_func ~repo ~rule ~tag:"release-20260930-0951"
      ~release:(Some escape_upload_release) ~tagger_email:(Some "dev.one@example.com") ~tagger_name:(Some "Dev One")
      ~sender_login:"dev-one_enterprise" ()
  in
  check "upload content keeps mrkdwn-sensitive characters raw" "true"
    (string_of_bool
       (match escape_upload_reply with
       | Some (Slack.Upload ({ content; _ } : Slack.file)) -> Devkit.Stre.exists content escape_subject
       | _ -> false));

  (* No tag object was readable. *)
  let bare_req, bare_reply = generate ~release:None () in
  let bare_text = Stdlib.Option.value ~default:"" bare_req.Slack_t.text in
  check "parent without a tag object has no count" "false" (string_of_bool (Devkit.Stre.exists bare_text "commits"));
  check "parent without a tag object has no reply" "none"
    (match bare_reply with
    | Some _ -> "some"
    | None -> "none");

  (* Fallback to sender_login: tagger email resolves nobody, but the sender's
     GitHub login does. *)
  let sender_login_resolves = function
    | "dev-two_enterprise" -> Some (Common.Slack_user_id.inject "U456")
    | _ -> None
  in
  let by_login_req, _ =
    Slack.generate_tag_notification ~slack_match_func:sender_login_resolves ~repo ~rule ~tag:"release-20260930-0951"
      ~release:(Some parsed) ~tagger_email:(Some "ghost@example.com") ~tagger_name:(Some "Unknown Tagger")
      ~sender_login:"dev-two_enterprise" ()
  in
  let by_login_text = Stdlib.Option.value ~default:"" by_login_req.Slack_t.text in
  check "falls back to sender login mention" "true" (string_of_bool (Devkit.Stre.exists by_login_text "<@U456>"));

  (* Fallback to a plain name: neither the tagger email nor the sender login
     resolves, so the tagger's plain name is used as text, not a mention. *)
  let resolves_nobody _ = None in
  let plain_name_req, _ =
    Slack.generate_tag_notification ~slack_match_func:resolves_nobody ~repo ~rule ~tag:"release-20260930-0951"
      ~release:(Some parsed) ~tagger_email:(Some "ghost@example.com") ~tagger_name:(Some "Dev Three")
      ~sender_login:"dev-three_enterprise" ()
  in
  let plain_name_text = Stdlib.Option.value ~default:"" plain_name_req.Slack_t.text in
  check "falls back to plain tagger name" "true" (string_of_bool (Devkit.Stre.exists plain_name_text "Dev Three"));
  check "plain tagger name is not a mention" "false" (string_of_bool (Devkit.Stre.exists plain_name_text "<@"));

  (* Fallback to the bare sender login: nothing resolves and there is no
     tagger name either. *)
  let no_name_req, _ =
    Slack.generate_tag_notification ~slack_match_func:resolves_nobody ~repo ~rule ~tag:"release-20260930-0951"
      ~release:(Some parsed) ~tagger_email:(Some "ghost@example.com") ~tagger_name:None
      ~sender_login:"dev-four_enterprise" ()
  in
  let no_name_text = Stdlib.Option.value ~default:"" no_name_req.Slack_t.text in
  check "falls back to bare sender login" "true"
    (string_of_bool (Devkit.Stre.exists no_name_text "dev-four_enterprise"));
  check "bare sender login is not a mention" "false" (string_of_bool (Devkit.Stre.exists no_name_text "<@"));

  let unblock ?label ?unblocked_by id =
    Slack.unblock_message ~slack_match_func ~job:{ Buildkite_t.id; label; unblocked_by }
  in
  check "unblock mentions the slack user behind the email" ":unlock: *Deploy Ahrefs to prod* unblocked by <@U123>"
    (unblock "j1" ~label:"Deploy Ahrefs to prod"
       ~unblocked_by:{ Buildkite_t.name = Some "Dev One"; email = Some "dev.one@example.com" });
  check "unblock falls back to the buildkite name" ":unlock: *Deploy Ahrefs to prod* unblocked by Dev Four"
    (unblock "j2" ~label:"Deploy Ahrefs to prod"
       ~unblocked_by:{ Buildkite_t.name = Some "Dev Four"; email = Some "dev.four@example.com" });
  check "unblock without an unblocker names only the step" ":unlock: *Deploy Ahrefs to prod* unblocked"
    (unblock "j3" ~label:"Deploy Ahrefs to prod");
  check "unblock without a label still reads" ":unlock: *a blocked step* unblocked" (unblock "j4");
  check "unblock escapes mrkdwn in the step name" ":unlock: *Deploy &lt;prod&gt;* unblocked"
    (unblock "j5" ~label:"Deploy <prod>");

  let signed =
    Release.parse
      "a57cc7ff42fd fix: handle the thing\n\
       b68dd8ee53ae api: tidy up\n\n\
       Previous-Release: release-20261013-1418\n\
       -----BEGIN SSH SIGNATURE-----\n\
       U1NIU0lHAAAAQAAAADMAAAALc3NoLWVkMjU1MTkAAAAgmcsfxEDo5Rxt6MZG7qjeGLO4zC\n\
       YV9fE1PRr199BtRsPbkwQ=\n\
       -----END SSH SIGNATURE-----"
  in
  check "a signed tag keeps its commits" "2" (string_of_int (Release.commit_count signed));
  check "a signed tag keeps its previous release" "release-20261013-1418"
    (Stdlib.Option.value ~default:"none" signed.previous_release);
  check "a signature is not a release note" "none" (Stdlib.Option.value ~default:"none" signed.note);

  let signed_with_note =
    Release.parse
      "a57cc7ff42fd fix: handle the thing\n\n\
       frontend + api_docs\n\
       -----BEGIN PGP SIGNATURE-----\n\
       iQIzBAABCgAdFiEE\n\
       -----END PGP SIGNATURE-----"
  in
  check "a signed tag keeps a real note" "frontend + api_docs"
    (Stdlib.Option.value ~default:"none" signed_with_note.note);

  if !failures > 0 then exit 1
