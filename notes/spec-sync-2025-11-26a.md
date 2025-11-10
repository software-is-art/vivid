# Spec Sync — 2025-11-26a

- Re-read spec §2.4 monitors to verify unbounded `unique` is meant to act like an online set keyed by `(by:, value)` with no window when budgets allow.
- Interpreter already keeps HashSet/HashMap state for the unbounded case, so compiler parity just needs a backing store that grows monotonically with new distinct samples.
- New lowering path introduces `CoreExpr::UniqueUnbounded { store, value, key }` plus `UniqueSpec` metadata so codegen can embed heap buffers per guard; this keeps the pipeline incremental and leaves `unique(... within:)` on the existing finite ring buffer state machine.
- Codegen now emits C helpers (`UniqueBuffer`, `UniqueKeyBuffer`) with `realloc` growth; this is intentionally naive but good enough for the §2.4 conformance suite (no deletes, monotone append).
- Tests still missing: the new compiler regressions hang because `tests/compiler.rs` runs full `compile_and_compare` which repeatedly spawns `cc`; need to instrument with `RUST_LOG`/`println!` or temporarily limit that suite so the hanging command can be reproduced outside Devbox before merging.
