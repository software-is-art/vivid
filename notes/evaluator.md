# Evaluator Notes

## Current shape
- Streams are memoized closures keyed by tick (`runtime::stream::Stream`), returning `ScalarValue` with `Undefined` representing `⊥`.
- Runtime values include functions, builtins, value types, and streams; builtins receive a slice of `CallArg` so they can see optional labels.
- Temporal primitives implemented: `next`, `fby`, `when`, `upon`, `first`, `rest`, `defined`, `hold`.
- Policy pipeline for value blocks is active: `.apply` masks with guard, `.use` applies invalid policy (`drop`, `hold`, `replace`), `.errors` emits structured diagnostics.
- Monitor builtins:
  - `unique(value:, by:)` with optional key stream (per-key sets).
  - `monotone(value:, direction:)` enforcing non-decreasing/increasing.
  - `stabilizes(value:, within:)` gating until plateau.
  - `rateLimit(events:, per:)` enforcing minimum spacing between truthy ticks.
  - `noGaps(value:)` demanding defined samples.
  - `guard_and`, `guard_or` zip boolean guard streams for composition.
- Count-based windows: `window(last:, of:)` returns the trailing samples as a list stream (size `N`, capped by count); implemented with an internal `VecDeque` per stream.
- Labeled arguments are optional but preferred; unlabeled fall back to positional order with validation.

## Follow-up
- Time-based windows (`window(last: Δt, of:)`) and window aggregations (`sum`, `avg`, `countDistinct`).
- Richer error reporting for monitor violations (explain which guard failed).
- Surface sugar for guard composition beyond `&&` (e.g., `always{ unique ... and stabilizes ... }`).
- Performance profiling: consider arena caches or chunked memoization for large windows.
