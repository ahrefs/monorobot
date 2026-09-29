open Monorobotlib

let targets s =
  let ({ channels; dms; _ } : Rule_t.label_rule) = Rule_j.label_rule_of_string s in
  Rule.Target.of_rule ~channels ~dms |> List.map Rule.Target.to_string

let () =
  (* a single channel, as in existing configs *)
  assert (targets {|{ "channel": "a" }|} = [ "#a" ]);
  assert (targets {|{ "channel": ["a", "b"] }|} = [ "#a"; "#b" ]);
  assert (targets {|{ "dm": "x@example.com" }|} = [ "@x@example.com" ]);
  assert (
    targets {|{ "channel": "a", "dm": ["x@example.com", "y@example.com"] }|}
    = [ "#a"; "@x@example.com"; "@y@example.com" ]);
  (* a rule must have somewhere to send notifications *)
  let rejected s =
    match targets s with
    | _ -> false
    | exception Failure _ -> true
  in
  assert (rejected {|{ "match": ["backend"] }|});
  assert (rejected {|{ "channel": [] }|});
  assert (rejected {|{ "channel": [], "dm": [] }|});
  assert (targets {|{ "channel": [], "dm": "x@example.com" }|} = [ "@x@example.com" ]);
  (* single values are written back as strings, and branch filters are still handled *)
  let rule = Rule_j.prefix_rule_of_string {|{ "channel": "a", "dm": ["x", "y"], "branch_filters": "any" }|} in
  assert (
    Yojson.Safe.from_string (Rule_j.string_of_prefix_rule rule)
    = `Assoc [ "branch_filters", `String "any"; "channel", `String "a"; "dm", `List [ `String "x"; `String "y" ] ])
