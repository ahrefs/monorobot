# Code Style & Conventions

## Formatting
- Uses **ocamlformat 0.26.2** (enforced via dune)
- Config in `.ocamlformat`:
  - Max line width: 120 (`m=120`)
  - Max indent: 2
  - Profile: default
  - Parens on if-then-else: yes
  - Doc comments: before
  - Let-and: sparse
  - Type-decl: sparse

## Naming Conventions
- OCaml standard: `snake_case` for values, functions, modules use `PascalCase`
- Module names match file names (OCaml convention)
- ATD types use `snake_case`

## Code Patterns
- Heavy use of ATD for type definitions and JSON serialization
- Lwt for async programming (with `lwt_ppx` for `let%lwt` syntax)
- Modules organized by domain (github, slack, config, state, etc.)
- `Action` module in `action.ml` is the main orchestrator
- Pattern matching extensively used for routing decisions

## Type Generation
- `.atd` files generate `_j.ml` (JSON) and `_t.ml` (types) via atdgen
- Types: `github_t`, `slack_t`, `config_t`, `state_t`, etc.

## Testing
- Cram tests enabled in dune-project
- Expected output files (`.expected`) for test comparison
- Mock payloads in `mock_payloads/`, `mock_slack_events/`, `mock_states/`
- API cache directories in `test/` for reproducible tests
