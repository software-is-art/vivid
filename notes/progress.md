# Progress Log

## 2025-11-02
- Initialized planning for Vivid interpreter implementation.
- Established work breakdown: setup, front end, evaluator.
- Next: set up devbox environment and Rust workspace.
- Set up devbox environment with Rust toolchain and initialized Cargo project.
- Added core front-end modules (span, diagnostics, AST, lexer, parser, type scaffolding).
- Parser handles value blocks, functions, structs, sources/sinks, let bindings, and core expressions with tests.
- Added runtime interpreter scaffold with stream semantics, arithmetic/temporal builtins, and placeholder value-type wiring.
- Added parser unit tests and ensured cargo test passes.
- Added interpreter integration tests covering `fby`, `when`, `upon`, and `defined` semantics.
- Implemented value block runtime semantics (normalize/require/policy handling) with method dispatch for `.apply`, `.guard`, `.use`, and `.errors`.
- Added integration test ensuring value blocks normalize input and drop invalid samples per policy.
- Added parser support for record literals and unary `hold` to match policy DSL needs.
- Implemented runtime invalid-policy handling (drop/hold/replace), structured error emission, and `hold` builtin stream repair.
- Added interpreter tests for `hold` repairs, custom replacement values, and error side-channel records; `cargo test` passing.
- Extended policy grammar to accept `policy invalid = replace <expr>` and wired interpreter support to treat both explicit replacements and raw expressions uniformly.
- Implemented `unique` monitor builtin and added interpreter coverage for duplicate suppression.
- Added `monotone` monitor builtin with direction selector and interpreter tests for rising/descending guards.
- Implemented `stabilizes` monitor with windowed plateau detection and integration tests for delayed admits.
- Added `rateLimit` monitor enforcing spacing between events plus interpreter regression case.
- Added guard composition builtins with keyed `unique` support and regression tests for combined monitors.
- Added `noGaps` monitor builtin and regression ensuring sparse streams trigger policy drops.
- Enabled labeled monitor arguments (`by:`, `within:`, `per:`) and updated interpreter semantics/tests to keep guard code terse.
- Implemented count-based `window(last: N, of: stream)` builtin with sliding list output and coverage in interpreter tests.

## 2025-11-03
- Added window aggregation builtins (`sum`, `avg`, `countDistinct`) backed by interpreter stateful folds, plus integration tests validating sums, means, and distinct counts over sliding windows.
- Documented window aggregators in `spec.md`, including sample pipelines showing `sum`/`avg` usage alongside existing distinct counts.
- Implemented `foldWindow` builtin with interpreter-aware step/finalizer hooks, added coverage proving parity with `sum` and custom average folds, and expanded `spec.md` with guidance and samples.
- Introduced `tickEvery` clock builtin to surface periodic pulses, wired runtime support and documentation, and added regression ensuring correct cadence.
- Extended `window` builtin with time-based windows via `time:` streams, including runtime buffering, documentation updates, and integration tests for timestamp trimming semantics.
- Documented the reversible sublanguage design (`rev {}` regions, reversible values, budgets, diagnostics) in `spec.md` so the roadmap captures the opt-in bidirectional story.

## 2025-11-04
- Extended the `unique` monitor with an optional `within:` window bound, ensuring duplicate tracking can be capped without sacrificing correctness.
- Added interpreter regressions covering both unkeyed and keyed `unique` monitors with window bounds to confirm the new semantics.
- Updated `spec.md` to call out the new `unique(..., within: N)` syntax for memory-bounded duplicate detection.
- Evaluated `source` declarations during interpretation so their providers bind to usable streams, plus regression ensuring downstream expressions can consume them.
- Implemented sink evaluation, capturing `to` streams inside the interpreter and adding coverage so we can assert on emitted values before wiring real adapters.
- Replaced the placeholder binary with an interactive REPL (prompt, expression evaluation, `:show` previews) so the interpreter can be exercised iteratively while we build out diagnostics and stepping features.
- Extended the REPL with `:first`, `:at`, and `:rest` stepping commands to make spec-driven stream inspection possible while richer tooling comes online.
