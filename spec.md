Awesome—let’s turn the **Vivid** sketch into a concrete build plan. Below I break it into two major tracks:

1. a **feature‑complete interpreter** (spec + engine + tooling), and
2. a **self‑hosted compiler** bootstrapped via that interpreter.

I include specific deliverables, suggested internal APIs, test plans, and references to the classic literature we’re standing on.

---

## 0) Grounding principles (what we’re implementing)

* **Denotation:** every expression denotes an **infinite stream** `ℕ → Value`, with `⊥` (undefined) allowed—this is Lucid’s core idea. Operators speak only about streams; application is implicitly pointwise. ([worrydream.com][1])
* **Temporal core:** `next`, `fby`, and gating operators like `whenever/when` and `upon` (sample‑and‑hold); these are definable from the Lucid base and used pervasively in the Lucid literature. ([cs.uwaterloo.ca][2])
* **Clocked synchronous step:** execution advances in discrete instants; semantically this aligns with the synchronous dataflow tradition (e.g., Lustre’s `pre` and `->` correspond closely to `next` and `fby`). ([homepage.cs.uiowa.edu][3])
* **Windows:** finite memory monitors and streaming windows (count‑ and time‑based) use the CQL/STREAM semantics as a north star. ([infolab.stanford.edu][4])
* **Runtime monitors:** temporal constraints (`unique`, `monotone`, `stabilizes`, `rateLimit`) are realized as **online monitors** for safety/co‑safety properties (runtime verification). ([sciencedirect.com][5])

---

# 1) Interpreter with full functionality

### 1.1 Milestone map (10 work packages)

**WP1 — Core spec (formal surface + desugaring)**

* **Surface syntax**: `value` blocks, `struct`, `fn`, `source/sink`, temporal ops (`next`, `fby`, `when`, `upon`, `hold`, `first`, `rest`, `defined`), windows, and the “policy” DSL.
* **Desugaring**: a value block `value Name : Rep { normalize …; require …; always …; policy … }` compiles to:

  * `Name.apply : S<Rep> → S<Rep⊥>` (normalize + local `require`),
  * `Name.guard  : S<Rep⊥> → S<Bool>` (composed monitor),
  * `Name.use    : S<Rep> → S<Name>` (apply → policy → gate),
  * `Name.errors : S<Rep> → S<List<Error>>` (side‑channel).
    Document this mapping precisely (tables + typing rules).
    *Grounding:* variables denote streams; base temporal ops mirror Lucid’s semantics, while `pre`/`->` parallels show up as `next`/`fby`. ([worrydream.com][1])

**WP2 — Parser & AST**

* Implement a clean grammar; produce a typed AST with source spans.
* AST nodes for expressions, value blocks, monitors, sources/sinks, tests.

**WP3 — Type system**

* Simple HM‑style type inference with `S<T>` as a **first‑class** type.
  * Current implementation wires a first-pass checker that enforces numeric/logical operator compatibility, validates function bodies against their declared stream types, and ensures value blocks gate on `Bool` streams before interpretation; richer inference (polymorphism, value-type contracts) remains to be layered on.
* Nominal `value` types with representation type `Rep` and an **erased** runtime tag (compile‑time only).
* Type rules:

  * Application is lifted: `f : A→B, x : S<A> ⟹ f(x) : S<B>`.
  * Temporal ops: e.g., `next : S<T>→S<T>`, `fby : S<T>×S<T>→S<T>`, `when : S<T>×S<Bool>→S<T>`.
  * `⊥` is tracked with `defined : S<T⊥>→S<Bool>`.
    *Grounding:* Lucid’s universe is streams with ⊥; Lustre has similar “undefined at first instant” when using `pre`, motivating the `defined` operator. ([worrydream.com][1])

**WP4 — Core evaluator (eductive, memoized)**

* Represent `Stream<T>` as `(t: Int) → T⊥` with **memoization** (lazy, demand‑driven), which matches Lucid’s eductive evaluation model.
* Each temporal primitive is a stream transformer:

  * `next(x)(t) = x(t+1)`,
  * `fby(a,b)(0)=a(0)`, `fby(a,b)(t>0)=b(t-1)`,
  * `when(x,g)` thins (only defined where `g(t)=true`),
  * `upon(x,g)` updates when `g(t)=true`, otherwise holds previous defined value.
* Deterministic, total, causal.
  *Grounding:* “each variable is an infinite stream; every function is a filter/transformer; evaluation is demand‑driven (eductive).” ([Wikipedia][6])

**WP5 — Monitors (temporal constraints)**

* A `Guard<T>` is a Mealy machine: `{ init; step: State×T⊥→State; ok: State→Bool }`.
* Built‑ins:

  * `unique(x)` (keep a set; option to window it),
  * `monotone(x, by ≤|≥)`,
  * `stabilizes(x, within N)`,
  * `rateLimit(x, per Δt)` (requires a time stream),
  * `noGaps(x) ≡ □ defined(x)`.
  * All monitors accept labeled arguments for readability: e.g. `unique(value: x, by: key)`, `monotone(it, direction: ">=")`, `stabilizes(value: it, within: 3)`, `rateLimit(events: flag, per: 5)`. Positional calls remain valid, but labels are preferred in examples.
  * `unique` additionally accepts `within:` to bound duplicate memory; `unique(samples, within: 3)` only rejects repeats that occur within the last three ticks (inclusive of the current one).
* Compose guards (`and`, `or`, `by key`). Output a boolean stream for `policy`.
  *Grounding:* online RV for LTL/TLTL (three‑valued semantics, causality, finite memory where possible). ([isp.uni-luebeck.de][7])

**WP6 — Policies & error side‑channels**

* Implement `drop` (`when(x, guard)`), `hold` (`upon(repair(x), guard)`), and `replace v` (pointwise replacement where `defined` fails).
* `errors`: a stream of small lists `{t, code, detail}` emitted on invalid samples/violations.

**WP7 — Windows & aggregations**

* `window(last: N, of x)` (count‑based) and `window(last: Δt, of x, time: clock)` (time‑based). Count windows return the trailing `N` samples. Time windows accept a monotonically increasing `clock : S<Int>` stream (ticks or physical time) and include samples whose timestamp is within the last `Δt` units. Both variants return `S<List<Value>>`, i.e. each tick sees the trailing window as a list.
* Aggregators over window lists:
  * `sum(window_stream)` keeps integer accumulators in `i64` where possible, promotes to `f64` on overflow or mixed numeric types, and skips `⊥` samples. A window with no defined numeric elements yields `⊥`.
  * `avg(window_stream)` divides the window sum by the count of defined numeric elements, returning `⊥` if the window is empty after filtering undefined samples or contains heterogeneous numeric types.
  * `countDistinct(window_stream)` counts unique, defined elements using equality on concrete values; undefined entries are ignored.
  All three aggregators return `⊥` if the window contains non-numeric (for `sum`/`avg`) or inconsistent element types. They treat heterogeneity as invalid rather than best-effort coercion.
* `foldWindow(window, init, step[, out])` lets authors express custom reducers in Vivid:
  * `window` is an `S<List<Value>>` stream (typically from `window(last: …)`).
  * `init` seeds the accumulator each tick (constants become constant streams).
  * `step(acc, sample)` runs for every element in the window list; both arguments arrive as constant streams at the current tick.
  * `out(acc)` runs once after the fold; omit it to return the accumulator directly.
  The fold yields `⊥` if any stage returns `⊥`, a non-stream result, or if `init` is `⊥`. This mirrors the new built-ins and keeps user-defined reducers honest about undefined/mixed data.
* Semantics line up with CQL (streams + relations + windows). ([infolab.stanford.edu][4])

**WP8 — Sources, sinks, and clocks**

* `source name : S<T> from …` adapters (files, Kafka, HTTP, IMAP).
  * In the interpreter, `from …` is evaluated eagerly and bound as a stream so downstream declarations can treat the source name like any other stream.
* `sink name : S<T> to …` with idempotent delivery guarantees.
  * Interpreter baseline records the `to …` stream so tests and tooling can inspect what would be emitted, paving the way for real adapters later.
* Standard clocks: `tickEvery(period) : S<Bool>` is live—returns `true` every `period` steps starting at tick `0` and rejects non-positive periods. Higher-level helpers like `minuteTick`, `hourlyTick`, etc., can layer on top once we wire real time.
* Keep core pure: adapters run at the boundary.

**WP9 — Tooling (REPL, tests, docs)**

* REPL with stepping (`at t`, `first`, `rest`), stream visualization.
  * Current REPL supports interactive entry of declarations/expressions plus `:show`, `:first`, `:at`, and `:rest` helpers for quick inspection; deeper stepping (`rest` chains, diagnostics) will continue to grow.
* Test DSL: `test { let s=streamOf(...); assert … }`.
* “Why undefined?” diagnostics: annotate `⊥` with origin chain.

**WP10 — Performance & correctness**

* Benchmarks (micro + pipeline), memoization strategy (arena or LRU cache).
* Determinism tests; coinductive reasoning sanity checks.

### 1.2 Suggested internal APIs (host implementation in Rust or TypeScript)

```ts
// Core stream
type Stream<T> = (t: number) => T | Bottom;

// Temporal primitives
function next<T>(s: Stream<T>): Stream<T>;
function fby<T>(a: Stream<T>, b: Stream<T>): Stream<T>;
function when<T>(x: Stream<T>, g: Stream<boolean>): Stream<T>;
function upon<T>(x: Stream<T>, tick: Stream<boolean>): Stream<T>;
function defined<T>(x: Stream<T | Bottom>): Stream<boolean>;
function hold<T>(x: Stream<T | Bottom>): Stream<T>; // last defined repair

// Monitors
interface Guard<T> { init(): State; step(s: State, x: T | Bottom): State; ok(s: State): boolean; }
function unique<T>(key?: (t: T) => string): Guard<T>;
function monotone<T>(cmp: (a: T, b: T) => boolean): Guard<T>;
function stabilizes<T>(within: number): Guard<T>;
```

### 1.3 Acceptance tests (excerpt)

* **Lucid laws:**
  `first(x fby y) == first(x)`; `rest(x fby y) == y`; `rest(x) == next(x)`; **hold/when** sanity. ([progwww.vub.ac.be][8])
* **Lustre parity:**
  `x = a fby b` behaves like `a -> b` and `next(x)` like `pre x` after the first instant. ([homepage.cs.uiowa.edu][3])
* **Windows correctness:**
  Count/time windows match CQL expectations on synthetic streams. ([infolab.stanford.edu][4])
* **Monitors:**
  Properties classified as safety/co‑safety behave as online checks (no look‑ahead). ([sciencedirect.com][5])

---

# 2) Bootstrap a self‑hosted compiler (via the interpreter)

### 2.1 Strategy overview

We write **Compiler₀ (C₀)** in the host (e.g., Rust/TS) only for the parser + minimal front end needed to run programs in the interpreter. Then we write **Compilerᵥ (Cᵥ)** *in Vivid itself*, using the interpreter to run it. **Cᵥ** lowers Vivid to a small **Core IR** and **generates C/Rust** (or VM bytecode). Finally we self‑compile:

```
Interpreter(host) runs Cᵥ (written in Vivid)  → builds Cᵥ.native
Cᵥ.native compiles itself again               → self-host achieved
```

Use **nanopasses** to keep each transformation tiny and testable; normalize to **ANF** (or CPS) to make dataflow explicit before codegen. ([Department of Computer Science][9])

### 2.2 Core IR (target of front end)

**CoreVivid** keeps only:

* Streams, lambdas, let, tuples/records.
* Temporal ops: `next`, `fby`, `when`, `upon`, `defined`, `first`, `rest`.
* Primitive windows (`windowN`, `windowΔt`). (A reversible `scan` drops in alongside the reversible sublanguage.)
* No `value` blocks (they’re desugared to `apply/guard/use/errors`).

IR is explicitly **clocked**: every top‑level program compiles to a **step function** `(state, input_t) → (state', output_t)`, like Lustre’s generated code. ([homepage.cs.uiowa.edu][3])

**Recommended pass pipeline (nanopass style):** ([Department of Computer Science][9])

1. **Parse → Surface AST**
2. **Desugar Values** (emit `apply/guard/use/errors`)
3. **Clock inference** (ensure all `when/upon` align; insert `hold`/`defined` repairs)
4. **ANF** (bind all non‑atomic subexpressions) — simplifies codegen. ([users.soe.ucsc.edu][10])
5. **State extraction** (collect things that need memory: `fby`, `upon`, windows, monitors)
6. **Node scheduling** (topo‑order; check no combinational cycles)
7. **Codegen IR** (SSA‑ish or straight‑line with explicit state)
8. **Backend**:

   * **C backend**: emit `init()`, `step()` with `static` state (matches synchronous dataflow practice), or
   * **VM bytecode**: tiny stack machine with opcodes for temporal ops.

### 2.3 What Compilerᵥ (in Vivid) contains

* **Lexer & parser**: hand‑rolled combinators are fine (streams of chars → token stream).
* **Type checker**: replicate the interpreter’s typing rules; produce typed AST.
* **Desugaring of `value` blocks** to the core forms.
* **Clock/temporal checker**: ensure `next/fby` and `when/upon` are well‑formed. (Lustre has an initialization analysis for `pre`; we mimic this to keep first‑instant definedness correct.) ([ResearchGate][11])
* **ANF transform** & **state extraction**. ([users.soe.ucsc.edu][10])
* **Backends**: start with **C** (simple and portable) or **Rust** (safer state), emitting a single‑rate stepper.

**Why this works:** synchronous dataflow compilers (e.g., Lustre) compile each program into a **deterministic step function** with explicit state for `pre/->` (our `next/fby`). We’re doing the same. ([homepage.cs.uiowa.edu][3])

### 2.4 Bootstrapping steps (checklist)

1. **Interpreter complete** (Section 1) with an FFI to:

   * read/write files,
   * spawn processes (to assemble/link C output),
   * basic timing (for tests).
2. **Write Compilerᵥ** in Vivid:

   * Implement parser, type checker, desugarer, ANF, state extraction, C backend.
3. **Stage‑1 compile** (hosted):
   Run Compilerᵥ on the interpreter to compile **its own source** to C; build `Cᵥ.native`.
4. **Stage‑2 self‑compile**:
   Use `Cᵥ.native` to compile the same sources again; verify byte‑for‑byte stability of outputs (**reproducible build**).
5. **Conformance suite**:
   Run all interpreter tests through both backends (interpreted and compiled) and diff the resulting traces step‑by‑step.

### 2.5 Deliverables & artifacts

* `vividc` (compiler) with:

  * `--emit=c` (or `--emit=vm`), `--opt=anf`, `--opt=inline`, `--check-clocks`.
  * `--explain` to show desugaring of `value` blocks and inferred policies.
* A small **runtime** (`librt`) for windows, monitors, clocks (pure, small).
* **Samples**: the Email pipeline, Money balance, per‑user uniqueness, sliding‑window analytics.
* **Docs**: spec of surface syntax + formal desugaring; runtime guarantees.

---

## 3) Reversible sublanguage (opt-in)

### 3.1 Model at a glance

* **`rev { … }` regions:** inside, every operator must be information-preserving. Lossy operators are permitted only when they emit a complement (mask, rejects, deltas, tick streams, permutations, etc.) that lets the step be inverted.
* **Reversible values:** value types can opt into a `reversible` form that derives bidirectional `get/put` behaviour and tracks complements per field.
* **Effect checking:** the compiler tracks a `rev` effect plus a loss ledger. A `rev` block compiles only if the ledger closes to zero—every lossy step must be justified by a complement.
* **Budgets:** developers bound complement retention with `within` / `max_memory` policies; the checker enforces them.
* **Ergonomics:** compiler diagnostics explain why an operator is not reversible and offer fix-its; inspectors visualise complements.

Reversibility is entirely opt-in. The default language stays pragmatic and lossy.

### 3.2 Surface: reversible regions and values

#### 3.2.1 Reversible regions

```vivid
rev {
  let cleaned =
    emails
    |> map Email.normalize
    |> when(it, looksValid)
    |> unique(by: _.id)
    |> scan(group: Sum, seed: 0, _.size)

  emit cleaned
}
```

* `rev { … }` ensures each operator either preserves information or emits complements automatically (masks, rejects, per-step deltas, tick streams, permutations).
* Operators without a reversible interpretation fail the effect check with actionable diagnostics.

#### 3.2.2 Reversible values (bidirectional views)

```vivid
value Email : String reversible {
  normalize it = it.trim().toLowerAscii()
  require looksLikeEmail(it)
  policy  invalid = drop keep rejects

  putback desired using { originalCase } =
    restoreCase(desired, originalCase)
}
```

* In `rev` regions, `Email.normalize` emits complements such as `originalCase`, masks, and rejects so edits can flow backwards.
* The compiler derives `get/put` laws and checks they satisfy bidirectional invariants (`get ∘ put = id`, `put ∘ get ≈ id`).

#### 3.2.3 Operator sugar

Inside `rev`, the usual operators automatically capture complements:

* `when(p)` ⇒ mask + rejects.
* `unique(by:)` ⇒ mask + rejects.
* `scan(group: G)` ⇒ per-step inverse elements (groups expose `combine`, `inverse`, `identity`).
* `window(last: N | Δt, …)` ⇒ per-window contents (checker warns about memory).
* `upon(x, tick)` ⇒ retains tick stream.
* `next` / `fby` ⇒ require paired `uncons/cons` helpers so head/tail are preserved.

Outside `rev`, the same operators behave exactly as they do today—fast and lossy.

### 3.3 Reversible operator rules

| Operator / Pattern        | Reversible?                | How it remains reversible inside `rev`                                                    |
| ------------------------- | -------------------------- | ----------------------------------------------------------------------------------------- |
| `map f`                   | Only if `f` is an iso      | Declare `iso` or supply `putback` + complements (e.g., `originalCase`).                  |
| `when(x, g)`              | Lossy filter               | Emit mask + rejects.                                                                     |
| `unique`                  | Lossy                      | Emit mask + rejects.                                                                     |
| `next` / `fby`            | Lossy alone                | Pair with `uncons/cons` to keep head and tail streams.                                   |
| `upon(x, tick)`           | Lossy                      | Retain tick stream (update mask).                                                        |
| `scan(op, seed, x)`       | Lossy in general           | Require group operations and per-step inverses; otherwise rejected in `rev`.             |
| `window(last: N | Δt)`    | Often lossy                | Retain per-window contents or sufficient summary; otherwise rejected.                    |
| Aggregates `sum`/`product`| Group-based                | Reversible via `unscan` (requires inverse).                                              |
| Aggregates `min/max/avg`  | Not group-based            | Demand full window contents or reject inside `rev`.                                      |
| Approx ops (Bloom, etc.)  | Approx + lossy             | Only allowed with masks/rejects; otherwise rejected.                                     |
| `join`                    | Depends on join            | Keep provenance (left/right indices, multiplicities).                                    |
| `groupBy` / `sort`        | Reorder                    | Keep the stable permutation to invert order.                                             |

### 3.4 Diagnostics

Examples of messages emitted by the effect checker:

```
rev error: `when(it, looksValid)` discards 13.2% of items.
→ Fix: use the reversible filter (mask + rejects). This is automatic in `rev`.
→ Or move this op outside the `rev` block.
```

```
rev error: `avg` is not invertible.
→ Options:
   1) Wrap it in a window that keeps full contents: window(last: N) { avg(it) }
   2) Use `sum` + `count` with `unscan` (both invertible).
   3) Move `avg` outside `rev`.
```

```
rev budget exceeded: complements for `window(last: 7d)` would exceed 64MB (est. 88MB).
→ Options: tighten window, raise `max_memory`, or mark this step `irrevocable`.
```

### 3.5 Budgets & policies

Budgets attach at any scope:

```vivid
rev(policy: { complement.within = 7d, max_memory = 64MB }) {
  ...
}

value Email : String reversible {
  policy complement.within = 30d
}
```

If the effect checker projects a budget violation, it produces guidance (shrink window, relax policy, or mark the step `irrevocable`).

### 3.6 Editing backwards

Complements allow edits to flow from views back to sources:

```vivid
rev {
  let users =
    raw
    |> map Email.normalize
    |> unique(by: _.id)

  users[42].email := "AliCe@Example.org"
  commit users
}
```

The compiler uses derived `putback` functions and stored complements (e.g., `originalCase`, masks) to generate a deterministic update script.

### 3.7 Effect sketch

* Operators inside `rev` have shape `Op : A → (B × C)` with an inverse `Op⁻¹ : (B × C) → A`; `C` is the complement.
* Complements compose: `(b, c) ← Op(a)` then `(d, e) ← Op₂(b)` gives complement `c ⊗ e`.
* Outside `rev`, complements are erased entirely.

### 3.8 Library support

* Group instances (`Sum`, `Product`, `Xor`, user-defined) for reversible scans.
* Permutation utilities for `sort` / `groupBy`.
* Join provenance structures (which side, multiplicity) for reversible joins.
* Mask algebra (`Mask`, `and`, `or`, `invert`, `replay`).

### 3.9 Tooling

* **Rev Inspector:** inspect complements (mask heatmaps, reject samples, window buffers) with size estimates.
* **Time-travel slider:** replay `upon` / `scan` state across ticks.
* **Property tests:** `@rev_law` auto-generates round-trip tests for reversible value types and pipelines.
* **Explainers:** “Why isn’t this reversible?” surfaces minimal counterexamples.

### 3.10 Examples

**Auditable email cleaning**

```vivid
rev(policy: { complement.within = 14d }) {
  emails
  |> map Email.normalize
  |> when(it, looksValid)
  |> unique(by: _.hash)
  |> emit
}
```

The pipeline retains masks and rejects, enabling audit trails and backward edits.

**Pragmatic analytics (lossy)**

```vivid
let activeUsers =
  events
  |> window(last: 7d)
  |> groupBy(_.userId)
  |> map { userId, w -> { userId, count: w.count() } }
```

Outside `rev`, nothing changes: lossy operators remain cheap.

### 3.11 Implementation notes

1. **Effect checker:** mark operators with `rev` capabilities (`loss[kind]`) and ensure complements close the ledger.
2. **Lowering:** inside `rev`, operators return `(value, complement)`; complements stream alongside values.
3. **Retention:** store complements in a pluggable store obeying `within` / `max_memory` policies.
4. **Optimisations:** fuse masks, compress permutations, delta-encode window contents.
5. **Erasure:** outside `rev`, strip complements to keep default pipelines lightweight.

### 3.12 Bottom line

Reversibility becomes a first-class sublanguage: opt into `rev { … }`, leverage reversible value types, let the checker enforce budgets, and enjoy bidirectional editing where you need it—without burdening the default, lossy stream algebra.

---

## 4) Risks & decisions (and how the references inform them)

* **Undefinedness (`⊥`) & policy semantics:**
  We follow Lucid’s stream‑with‑⊥ model and make `when/upon/hold` the only ways to repair/propagate; first‑instant behavior mirrors Lustre’s `pre/->` rules to avoid surprises. ([worrydream.com][1])
* **Monitor memory growth (e.g., `unique`)**:
  Provide `unique(x) within window` to bound memory; aligns with STREAM/CQL window semantics. ([infolab.stanford.edu][4])
* **Scheduler correctness:**
  Topological scheduling and “no combinational cycles” borrow from synchronous language practice. ([homepage.cs.uiowa.edu][3])
* **Compiler architecture:**
  Many tiny passes (nanopass) and ANF normalization are well‑worn, testable strategies. ([Department of Computer Science][9])
* **Runtime verification scope:**
  We restrict built‑in properties to **safety/co‑safety** (monitorable online) per RV guidance. ([sciencedirect.com][5])

---

## 5) Concrete examples (end‑to‑end)

**A. Email (drop invalid, unique over time)**

```vivid
value Email : String {
  normalize it = it.trim().toLowerAscii()
  require   it.contains("@") && count(it,"@")==1 && hasDotAfterAt(it)
  always    unique(it)
  policy    invalid = drop
  policy    error   = InvalidEmail(it)
}
source inbox : S<String> from Imap("imap.example","INBOX")
sink   alerts: S<String> to   Slack("#ops")

emails = Email.use(inbox)
alerts <- ("New signup: " + show(first(emails))) upon tickEvery(60)
```

**B. Money with “hold on invalid” policy**

```vivid
value Money<CCY> : Decimal {
  normalize it = round(it, 2)
  require   isFinite(it) && abs(it) < 1_000_000_000
  policy    invalid = hold
}

flows   : S<Decimal> = payments.netFlows()
net_flows : S<Money<USD>> = Money<USD>.use(flows)
balance : S<Money<USD>> = 0.0 fby (balance + net_flows)
```

**C. Sliding window analytics**

```vivid
pulses : S<Bool> = tickEvery(1)
seconds : S<Int> = 0 fby (if pulses { seconds + 1 } else { seconds })

hourly_emails : S<List<Email>> =
  window(last: 3600, of: Email.use(inbox), time: seconds)
uniques1h : S<Int> = countDistinct(hourly_emails)
spike     : S<Bool> = uniques1h.map(_ > 100)

payload_sizes : S<Float> = inbox.map(it.sizeBytes().toFloat())
hourly_payloads : S<List<Float>> = window(last: 3600, of: payload_sizes, time: seconds)
total_bytes  : S<Float> = sum(hourly_payloads)
mean_bytes   : S<Float> = avg(hourly_payloads)

fn payload_stats_step(state, bytes) {
  let valid = defined(bytes)
  let total = if valid { state.total + bytes } else { state.total }
  let count = if valid { state.count + 1 } else { state.count }
  { total: total, count: count }
}

fn payload_stats_out(state) {
  let ready = state.count > 0
  (state.total / state.count) when ready
}

mean_bytes_via_fold : S<Float> =
  foldWindow(window: hourly_payloads,
             init: { total: 0.0, count: 0 },
             step: payload_stats_step,
             out: payload_stats_out)
```

Aggregators yield `⊥` when a window is empty or contains mixed/undefined samples, so policies typically pair them with `hold` or `replace` to smooth transient gaps.

(CQL‑style windows with stream semantics.) ([infolab.stanford.edu][4])

---

## 6) References (most influential for this plan)

* **Lucid core semantics & operators:** Wadge & Ashcroft, *Lucid, the Dataflow Programming Language*; plus UWaterloo TR clarifying `fby`, `upon`, `whenever`. ([worrydream.com][1])
* **Demand‑driven (eductive) execution & Lucid systems:** Lucid overview + history (pLucid, GIPSY). ([Wikipedia][6])
* **Synchronous dataflow alignment (pre/->):** Lustre’s seminal paper (step function codegen model we adopt). ([homepage.cs.uiowa.edu][3])
* **Windows semantics for streams:** CQL/STREAM work (relations, streams, windows, continuous queries). ([infolab.stanford.edu][4])
* **Runtime verification (monitors):** Leucker, Bauer, Schallhart surveys (three‑valued RV, safety/co‑safety). ([sciencedirect.com][5])
* **Compiler architecture:** ANF and nanopass frameworks for clean, testable pipelines. ([users.soe.ucsc.edu][10])

---

### Final notes

* Build the **interpreter first** (eductive, memoized), with a rock‑solid spec and desugaring for `value` blocks.
* Then write the **compiler in Vivid** using the interpreter—lower to a synchronous stepper in C/Rust; adopt ANF + nanopasses.
* The result is a language that’s **beautiful to write**, **precise to reason about**, and **practical to deploy** (pure step function with explicit state).

If you’d like, I can turn WP1 into a short “spec.md” (surface grammar + typing + desugaring tables) and WP4 into a 200‑line reference evaluator you can drop into a repo to start executing the examples.

[1]: https://worrydream.com/refs/Wadge_1995_-_Lucid%2C_the_Dataflow_Programming_Language.pdf?utm_source=chatgpt.com "Lucid, the Dataflow Programming Langugage1"
[2]: https://cs.uwaterloo.ca/research/tr/1979/CS-79-38.pdf?utm_source=chatgpt.com "SOME COMMON MISCONCEPTIONS ABOUT LUCID"
[3]: https://homepage.cs.uiowa.edu/~tinelli/classes/181/Spring08/Papers/Halb91.pdf?utm_source=chatgpt.com "The synchronous data flow programming language LUSTRE"
[4]: https://infolab.stanford.edu/~arvind/papers/cql-vldbj.pdf?utm_source=chatgpt.com "The CQL Continuous Query Language: Semantic Foundations ..."
[5]: https://www.sciencedirect.com/science/article/pii/S1567832608000775?utm_source=chatgpt.com "A brief account of runtime verification"
[6]: https://en.wikipedia.org/wiki/Lucid_%28programming_language%29?utm_source=chatgpt.com "Lucid (programming language)"
[7]: https://www.isp.uni-luebeck.de/sites/default/files/publications/tosem09_prelim_1.pdf?utm_source=chatgpt.com "Runtime Verification for LTL and TLTL"
[8]: https://progwww.vub.ac.be/~tjdhondt/ESL/Lucid_files/Lucid%2C%20the%20Dataflow%20Programming%20Language.pdf?utm_source=chatgpt.com "Lucid, the Dataflow Programming Langugage1"
[9]: https://cs.indiana.edu/~dyb/pubs/nano-jfp.pdf?utm_source=chatgpt.com "A Nanopass Framework for Compiler Education"
[10]: https://users.soe.ucsc.edu/~cormac/papers/pldi93.pdf?utm_source=chatgpt.com "The Essence of Compiling with Continuations Abstract 1 ..."
[11]: https://www.researchgate.net/publication/222407600_Type-based_Initialization_Analysis_of_a_Synchronous_Data-flow_Language?utm_source=chatgpt.com "Type-based Initialization Analysis of a Synchronous Data ..."
