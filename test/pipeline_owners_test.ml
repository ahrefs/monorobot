open Monorobotlib

(* base pipeline with no owners; individual tests override the owner fields *)
let base_pipeline : Config_t.pipeline =
  {
    name = "buildkite/test";
    failed_builds_channel = None;
    notify_canceled_builds = false;
    mention_user_on_failed_builds = true;
    escalate_notifications = false;
    escalate_notification_threshold = 2;
    dm_users_on_failures = true;
    pipeline_owner = None;
    pipeline_owners = [];
    mention_owner_on_failed_builds = false;
  }

let merged pipeline_owner pipeline_owners =
  Util.Webhook.merged_pipeline_owners { base_pipeline with pipeline_owner; pipeline_owners }

let () =
  (* neither field set *)
  assert (merged None [] = []);
  (* only the deprecated singular field set *)
  assert (merged (Some "a@x.com") [] = [ "a@x.com" ]);
  (* only the plural field set *)
  assert (merged None [ "a@x.com"; "b@x.com" ] = [ "a@x.com"; "b@x.com" ]);
  (* both set, disjoint: singular is prepended to the plural list *)
  assert (merged (Some "a@x.com") [ "b@x.com"; "c@x.com" ] = [ "a@x.com"; "b@x.com"; "c@x.com" ]);
  (* both set, singular already present in plural: no duplicate *)
  assert (merged (Some "a@x.com") [ "a@x.com"; "b@x.com" ] = [ "a@x.com"; "b@x.com" ])
