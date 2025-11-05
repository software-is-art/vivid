# Repository Guidelines

## Project Structure & Module Organization
- `spec.md` captures the evolving language contract; read it before touching interpreter or parser code.
- `src/` holds production Rust code. Key modules: `parser.rs` (surface grammar), `runtime/` (stream evaluator), and `ast.rs` (syntax tree definitions).
- `tests/` contains integration specs, currently focused on interpreter behaviour; add new end-to-end cases here.
- `notes/progress.md` records daily milestones; append concise bullet updates after significant work.

## Build, Test, and Development Commands
- `devbox run -- cargo fmt` — format all Rust sources using the project toolchain.
- `devbox run -- cargo test` — run parser, runtime, and integration suites; ensure they pass before proposing changes.
- `devbox run -- cargo clippy` *(recommended)* — lint for common Rust issues; fix warnings or document deferrals.

## Coding Style & Naming Conventions
- Aim for code that feels beautiful: balance elegance with clarity, favouring small, composable functions over sprawling blocks.
- Embrace pragmatism—prefer the simplest construct that communicates intent; avoid cleverness that obscures the temporal semantics.
- Keep modules and helpers logically grouped (`runtime/` for stream mechanics, `parser.rs` for syntax); name new utilities with `make_*` or `*_stream` for symmetry.
- Let `rustfmt` lead formatting, but prune noise: comments should be terse, purposeful, and anchored to `spec.md` concepts.
- Tests and identifiers stay lowercase snake_case; reserve CamelCase for nominal types defined in the AST.
- Apply “parse, don’t validate” rigor: parsing/desugaring should produce well-typed, constraint-respecting structures so downstream stages can drop defensive checks. Where lifecycle or invariants matter, encode them as Rust typestates (distinct structs/enums) instead of boolean flags or runtime assertions. When you encounter legacy code that violates this, bias toward refactoring it into stronger types/typestates rather than piling on additional runtime validation.

## Testing Guidelines
- Use the existing integration harness in `tests/interpreter.rs` to exercise stream semantics and policies.
- Write golden-stream assertions with helper functions like `assert_stream_eq` and extend them if new value types demand richer checks.
- When adding modules, include unit tests alongside implementation files and confirm via `devbox run -- cargo test --tests <name>`.

## Commit & Pull Request Guidelines
- Craft commits in the imperative mood (`Add hold builtin`, `Document policy replace syntax`) and keep related changes grouped.
- Ensure PR descriptions summarize intent, list testing commands executed, and reference relevant sections of `spec.md` or `notes/progress.md`.
- Attach before/after snippets or terminal output when behaviour changes, especially for interpreter semantics.
- Update `notes/progress.md` within the same PR to keep the project journal in sync.
