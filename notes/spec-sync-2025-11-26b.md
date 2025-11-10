# Spec Sync — 2025-11-26b

- Rechecked §2.4 after fixing the compiler hang: the compiled runner must be safe by construction, meaning every runtime structure is initialised before the first tick. The earlier failure violated that assumption by `free`ing uninitialised unique buffers.
- Added explicit `unique_buffer_init` / `unique_key_buffer_init` emitters and taught `init_state` to call them. Now the compiled CLI obeys the "deterministic state + step" contract and all CSV comparison tests pass.
- With unbounded `unique` stable, the next spec gap on the compiler side is lowering the remaining guards (`stabilizes`, `rateLimit`, budgets) and moving toward the reversible checker described in §3.
