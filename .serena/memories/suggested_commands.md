# Suggested Commands

## Build & Run
- `make build` or `dune build src/monorobot.exe` - Build the project
- `make start` or `dune exec -- ./src/monorobot.exe` - Run the bot
- `make release` - Build in release profile
- `make watch` - Build with file watching

## Testing
- `make test` or `dune runtest` - Run all tests
- `make test_promote` or `dune runtest --auto-promote` - Run tests and auto-promote expected output

## Code Formatting
- `make fmt` or `dune build @fmt --auto-promote` - Format code (ocamlformat 0.26.2)
- `dune build @fmt` - Check formatting without fixing

## Cleaning
- `make clean` or `dune clean` - Clean build artifacts

## System Utilities
- `git`, `ls`, `cd`, `grep`, `find` - Standard Linux tools

## Testing Manually (dev)
```sh
curl -X POST http://localhost:8080/github \
  -H 'Content-Type: application/json' \
  -H 'X-Github-Event: pull_request' \
  -d @mock_payloads/pr_notification.json
```
