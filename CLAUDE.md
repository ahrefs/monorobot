# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

Monorobot is a Slackbot for GitHub monorepos (by Ahrefs). It routes repo notifications (webhooks, CI build statuses) to Slack channels based on file prefixes, labels, and pipeline configs.

**Tech stack:** OCaml (>= 4.14.0), Dune 3.9, ATD (atdgen) for JSON types, Lwt for async, SQLite3/sqlgg, Curl.

## Build & Test Commands

```sh
make build              # Build (dune build src/monorobot.exe)
make test               # Run all tests (dune runtest)
make test_promote       # Run tests and auto-promote expected output (dune runtest --auto-promote)
make fmt                # Format code (dune build @fmt --auto-promote)
make start              # Run the bot (dune exec -- ./src/monorobot.exe)
dune build @fmt         # Check formatting without fixing
```

Individual test executables: `dune exec test/test.exe`, `dune exec test/github_link_test.exe`, `dune exec test/longest_prefix_test.exe`. The main test (`test.exe`) compares stdout against `test/slack_payloads.expected`.

**After any code change:** run `make fmt`, `make build`, `make test` in that order. If tests fail due to intentional output changes, review the diff and run `make test_promote`.

## Architecture

- **`src/`** — Entry point. `monorobot.ml` (CLI via cmdliner), `request_handler.ml` (HTTP webhook handler).
- **`lib/`** — Core library (`monorobotlib`).
  - **ATD files** (`github.atd`, `slack.atd`, `config.atd`, `state.atd`, `rule.atd`, `common.atd`, `buildkite.atd`, `debug_db.atd`) define all JSON types. `dune build` auto-generates `*_t.ml` (types) and `*_j.ml` (JSON serialization). LSP may show false errors for generated symbols until a build runs.
  - **`action.ml`** — Main orchestrator. `Action` module contains notification routing logic.
  - **`slack_message.ml`** — Builds Slack Block Kit messages.
  - **`github.ml`** — GitHub API interactions.
  - **`slack.ml`** — Slack API interactions.
  - **`state.ml`** — Bot state management.
  - **`rule.ml`** — Routing rule matching (file prefixes, labels, etc.).
  - **`api.ml`**, **`api_local.ml`**, **`api_remote.ml`** — API abstraction layer (local mock vs remote).
  - **`util.ml`** — Utilities including `Util.Webhook` for pipeline config field access.
  - **`context.ml`** — Request context.
  - **`lib/text_cleanup/`** — Text processing submodule.
- **`test/`** — Cram-style tests. Mock data in `mock_payloads/`, `mock_slack_events/`, `mock_states/`. API cache dirs (`test/*-api-cache/`) for reproducible tests.

## Key Patterns

- **Opaque wrapper types:** The codebase uses `Fresh` wrapper types from devkit (e.g., `Slack_user_id.t`, `Slack_channel.Name.t`). These are not plain strings — use polymorphic `compare`, not `String.compare`.
- **ATD codegen is automatic:** Adding fields to `.atd` files only requires `dune build` to regenerate types.
- **Adding per-pipeline config fields:** (1) Add field to `pipeline` type in `lib/config.atd`, (2) Add a reader function in `Util.Webhook` that calls `get_pipeline_config` and extracts the field with `Option.map_default`.
- **Async:** Lwt with `lwt_ppx` (`let%lwt` syntax).
- **Formatting:** ocamlformat 0.26.2, max line width 120, max indent 2. See `.ocamlformat` for full config.

**CRUCIAl**: read and comply with @AGENTS.md
