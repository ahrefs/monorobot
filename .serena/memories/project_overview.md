# Monorobot - Project Overview

## Purpose
Monorobot is a Slackbot for GitHub monorepos. It routes repo notifications to specified Slack channels based on file prefixes, issue/PR labels, and CI build statuses. Developed by Ahrefs.

## Tech Stack
- **Language**: OCaml (>= 4.14.0)
- **Build system**: Dune 3.9
- **Serialization**: ATD (atdgen) for JSON types
- **HTTP/Networking**: Curl, Lwt (async)
- **Database**: SQLite3 with sqlgg
- **Key libraries**: cmdliner, devkit, re2, omd, ptime, digestif, x509, mirage-crypto

## Repository
- Source: https://github.com/ahrefs/monorobot
- License: MIT
- Version: 0.1

## Project Structure
```
src/               - Main executable entry point (monorobot.ml, request_handler.ml)
lib/               - Core library code
  *.atd            - ATD type definitions (github.atd, slack.atd, config.atd, etc.)
  *.ml             - OCaml implementations
  text_cleanup/    - Text processing submodule
test/              - Tests (cram-style via dune)
mock_payloads/     - Mock GitHub webhook payloads for testing
mock_slack_events/ - Mock Slack event payloads
mock_states/       - Mock state files
db/                - Database related files
documentation/     - Config and secrets documentation
```

## Architecture
- ATD files define JSON types for GitHub webhooks, Slack API, configs, state, etc.
- `action.ml` contains the main `Action` module with notification routing logic
- `slack_message.ml` builds Slack messages
- `github.ml` handles GitHub API interactions
- `state.ml` manages bot state
- `rule.ml` handles routing rules
- `api.ml`, `api_local.ml`, `api_remote.ml` define API abstractions
- `request_handler.ml` handles incoming HTTP requests
