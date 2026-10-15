open Monorobotlib
open Rule_t

let rule ?inline_commit_limit ?pipeline ~prefix ~label channel =
  { prefix; channel_name = Common.Slack_channel.Name.inject channel; label; inline_commit_limit; pipeline }

let rules = [ rule ~prefix:"release-" ~label:"core" "releases"; rule ~prefix:"v" ~label:"library" "releases" ]

let check name expected actual =
  let show = function
    | None -> "none"
    | Some (r : tag_rule) -> r.label
  in
  if String.equal (show expected) (show actual) then Printf.printf "PASS %s\n" name
  else begin
    Printf.printf "FAIL %s: expected %s, got %s\n" name (show expected) (show actual);
    exit 1
  end

let () =
  check "matches the release prefix" (Some (List.nth rules 0)) (Rule.Tag.match_rules "release-20260930-0951" ~rules);
  check "matches a different prefix" (Some (List.nth rules 1)) (Rule.Tag.match_rules "v1.2.3" ~rules);
  check "no match" None (Rule.Tag.match_rules "nightly-20260930" ~rules);
  check "empty rule list" None (Rule.Tag.match_rules "release-20260930-0951" ~rules:[])
