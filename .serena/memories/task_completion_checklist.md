# Task Completion Checklist

When a coding task is completed, run the following:

1. **Format code**: `make fmt` (or `dune build @fmt --auto-promote`)
2. **Build**: `make build` (or `dune build src/monorobot.exe`)
3. **Run tests**: `make test` (or `dune runtest`)
4. **If tests fail with expected output changes**: Review changes and run `make test_promote` if appropriate

## Notes
- Formatting is enforced via dune's `@fmt` rule with ocamlformat
- Cram tests may need `--auto-promote` if expected output changes intentionally
- Always verify build succeeds before committing
