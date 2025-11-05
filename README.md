# Vivid

Vivid is an interpreter-first implementation of a Lucid-inspired temporal dataflow language. Every expression denotes an infinite stream, and the runtime evaluates programs eductively (demand-driven) with memoised stream transformers. The current focus is to complete a feature-complete interpreter and then bootstrap a self-hosted compiler.

## Getting Started

The repository is managed via [Devbox](https://www.jetpack.io/devbox/). All commands below assume you are inside the project root and have Devbox available.

```bash
# Format all Rust sources
devbox run -- cargo fmt

# Run the entire test suite (parser, runtime, integrations)
devbox run -- cargo test

# Optional: run the linter
devbox run -- cargo clippy
```

## Project Layout

- `spec.md` — evolving language contract and milestone plan.
- `src/` — Rust implementation (parser, AST, runtime, type checker, CLI REPL).
- `tests/` — integration tests for interpreter behaviour.
- `notes/progress.md` — daily journal of milestones achieved and remaining work.
- `AGENTS.md` — house style and contribution guidelines (parse-don’t-validate with typestates, test discipline, commit expectations).

## Development Guidelines

1. **Read the spec:** Keep `spec.md` open when modifying parser, interpreter, or type checker logic. The milestones in WP3–WP8 define the contract we’re chasing.
2. **Parse, don’t validate:** When adding features, prefer building typed/validated IRs over sprinkling runtime checks. Use typestates or distinct structs/enums to encode invariants, and refactor legacy code toward the same pattern.
3. **Tests and journal:** Every meaningful change should land with targeted tests (`tests/interpreter.rs`) and a progress entry in `notes/progress.md`.
4. **Interpreter first:** The REPL in `src/main.rs` is the canonical entry point during interpreter development. Keep it ergonomic (stepping commands, diagnostics) as new language features appear.
5. **Keep runtime pure:** Sources and sinks are evaluated at the boundary; internal stream transformations remain deterministic, memoised, and side-effect-free.

## Roadmap Snapshot

- ✅ Core stream runtime with temporal primitives (`next`, `fby`, `when`, `upon`, `hold`).
- ✅ Monitors, windows, and aggregations (`unique`, `monotone`, `window`, `foldWindow`, etc.).
- ✅ REPL with stepping commands (`:show`, `:first`, `:at`, `:rest`).
- ✅ Type checker covering core expressions, functions, and value blocks.
- ⬜ Tooling polish (diagnostics, `why ⊥`, richer REPL visualisations).
- ⬜ Compiler bootstrap (Core IR, lowering passes, native backend).

For deeper design notes, references, and future work, see `spec.md`.
