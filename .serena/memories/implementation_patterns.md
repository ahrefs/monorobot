# Implementation Patterns & Gotchas

## Opaque wrapper types
This codebase uses `Fresh` wrapper types from devkit extensively (e.g. `Slack_user_id.t`, `Slack_channel.Name.t`, `Slack_channel.Any.t`). These are not plain strings — don't use `String.compare` or other string operations directly on them. Use polymorphic `compare` or module-specific functions.

## ATD codegen is automatic
Adding fields to `.atd` files doesn't require manual codegen. `dune build` regenerates `_t.ml` and `_j.ml` automatically. LSP diagnostics may show false "unbound value" errors for new ATD-generated types/functions until a build runs.

## Adding per-pipeline config fields
Consistent pattern:
1. Add field to `pipeline` type in `lib/config.atd`
2. Add a reader function in `Util.Webhook` (in `lib/util.ml`) that calls `get_pipeline_config` and extracts the field with `Option.map_default`
3. Examples: `mention_user_on_failed_builds`, `failed_builds_channel_exn`, `get_escalation_threshold`, `mention_owner_on_failed_builds`, `pipeline_owner`
