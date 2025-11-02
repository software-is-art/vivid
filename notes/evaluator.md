# Evaluator Plan

## Goals
- Represent streams as memoized functions over discrete time.
- Support temporal primitives: `next`, `fby`, `when`, `upon`, `first`, `rest`, `defined`.
- Provide infrastructure for policies (`drop`, `hold`, `replace`) and monitor hooks.
- Allow evaluation of declarations (value blocks, functions, let bindings) into runtime graph.

## Open Questions
- How to represent `⊥`? Use `Value::Undefined` marker.
- Evaluate expressions lazily or demand-driven? Implement `StreamCell` with internal cache keyed by tick index.
- Error channel: keep alongside primary stream using `StreamEvent`.

## Immediate Tasks
1. Define runtime value representation (`Value`, `Function`, `Stream` types).
2. Implement environment and evaluator skeleton with support for literals, identifiers, basic arithmetic, and function application.
3. Add temporal operators with memoized stream objects.
4. Extend with policy handling and monitor placeholders.
5. Write unit tests covering primitive streams and sample value block semantics.
