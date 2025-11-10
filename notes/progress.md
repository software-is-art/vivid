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
- Started the type system rollout: added a module-level checker that rejects mismatched stream operations (numeric/logical guards, `fby`, etc.) before interpretation, and updated tests to cover the first type error case.
- Extended the checker to cover function bodies and value blocks, ensuring parameters, `normalize`, `require/always`, and policy expressions respect their expected stream types; added regression coverage for the new diagnostics.
- Added a project README that documents goals, workflow commands, and the parse-don't-validate development philosophy.

## 2025-11-09
- Added compiler runtime helpers (`Outputs`, `step`, `init_runtime`, `run_ticks`) so generated C exposes a deterministic stepper host callers can drive per spec §2.2.
- Consolidated the day’s learning into `notes/spec-sync-2025-11-09-summary.md`, replacing the stack of reminder notes.
- Remaining work: build the higher-level runner/CLI from §2.4 (to manage inputs/outputs) and then begin the reversible effect checker (Section 3).
- Re-read the archive + `spec.md` (`notes/spec-sync-2025-11-25b.md`), finished the §2.4 runner by emitting CSV headers, honoring `--ticks/--start/--quiet/--log`, and added compiler tests so `cargo test` now covers the new CLI surface.
- Landed `notes/spec-sync-2025-11-25c.md`, then added an end-to-end compiler test that writes the generated C to disk, builds it with `cc`, runs `--quiet --ticks --start --log`, and diffs the CSV rows against interpreter samples per spec §2.4’s conformance checklist.
- Fixed `run_ticks` to warm the runtime state whenever `--start` is non-zero so compiled binaries now produce the same streams as the interpreter even when skipping initial ticks.
- Logged `notes/spec-sync-2025-11-25d.md`, refactored the compiler conformance harness into a reusable helper, and added guard/boolean coverage (`compiled_runner_respects_guards_and_bools`) so the generated CLI is validated beyond simple arithmetic streams (undefined samples coerce to `0` to match the C backend’s `when` semantics).
- Added `notes/spec-sync-2025-11-25e.md` to capture the latest spec reread; future work is to port window/monitor programs into the compiled-runner harness so §2.4’s “run all tests through both backends” goal is steadily approached before resuming the reversible checker.
- Captured `notes/spec-sync-2025-11-25f.md`/`25g.md`, expanded the `compile_and_compare` helper to tolerate extra CSV columns, and documented that windows/sum remain unimplemented in the compiler; added a regression (`compiler_rejects_window_and_sum_for_now`) so we surface the precise diagnostics until the lowering work lands.
- Logged `notes/spec-sync-2025-11-25h.md` after lowering `sum(window(last: N, ...))` into a new `WindowSum` core form; state extraction now synthesizes the sliding history slots so compiled binaries can stream the same sliding sums as the interpreter (`compiled_runner_handles_window_sum`).
- Logged `notes/spec-sync-2025-11-25i.md`, implemented `monotone(...)` lowering + state extraction (previous-value + seen flag), and added `compiled_runner_handles_monotone_guard` so the compiled CLI now demonstrates monitor coverage per spec §2.4.
- Logged `notes/spec-sync-2025-11-25j.md`, wired `guard_and`/`guard_or` lowering straight to boolean core ops, and added the end-to-end `compiled_runner_handles_guard_and` so composed monitor traces now match across interpreter + compiled runner; next up is keyed `unique(by:)`.
- Logged `notes/spec-sync-2025-11-25k.md`, added keyed `unique(value, by:, within:)` lowering/state extraction (value/key/TTL ring buffer) plus interpreter + compiler coverage, keeping the host runner aligned with spec §2.4 monitor semantics.
- Logged `notes/spec-sync-2025-11-25l.md`, allowing `guard_and/guard_or` to accept arbitrary fan-in and adding a compiled-runner regression so composed monitors continue matching interpreter traces.
- Logged `notes/spec-sync-2025-11-25m.md`, added interpreter/compiler tests for three-way `guard_or`, and confirmed the new multi-operand lowering keeps the compiled runner aligned with spec §2.4’s guard composition.

## 2025-11-26
- Extended the compiler lowering/state extraction path so `unique(...)` without a `within:` bound lowers to a dedicated unbounded guard form instead of requiring fake windows.
- Added C codegen support for the new guard by emitting heap-backed `UniqueBuffer`/`UniqueKeyBuffer` helpers and wiring interpreter parity tests for keyed/unkeyed cases.
- Kicked off the compiler regression suite, but `cargo test --test compiler` currently hangs while the generated C harness is being built/run; need to bisect whether repeated `cc` invocations or tempdir IO is causing the stall before landing.
