use std::collections::BTreeMap;

use vivid::parser::parse_module;
use vivid::runtime::Interpreter;
use vivid::runtime::stream::Stream;
use vivid::runtime::value::ScalarValue;

fn stream_samples(stream: &Stream, ticks: usize) -> Vec<ScalarValue> {
    (0..ticks).map(|tick| stream.value_at(tick)).collect()
}

fn assert_stream_eq(stream: &Stream, expected: &[ScalarValue]) {
    let actual = stream_samples(stream, expected.len());
    assert_eq!(actual, expected);
}

fn error_record(code: &str, detail: ScalarValue, tick: i64) -> ScalarValue {
    let mut map = BTreeMap::new();
    map.insert("code".to_string(), ScalarValue::String(code.to_string()));
    map.insert("detail".to_string(), detail);
    map.insert("t".to_string(), ScalarValue::Int(tick));
    ScalarValue::Record(map)
}

fn user_record(user: &str, score: i64) -> ScalarValue {
    let mut map = BTreeMap::new();
    map.insert("user".to_string(), ScalarValue::String(user.to_string()));
    map.insert("score".to_string(), ScalarValue::Int(score));
    ScalarValue::Record(map)
}

#[test]
fn fby_combines_streams() {
    let source = "let result = 1 fby 2;";
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");
    let result = interpreter
        .get_global("result")
        .and_then(|value| value.as_stream())
        .expect("result stream");
    assert_stream_eq(
        &result,
        &[
            ScalarValue::Int(1),
            ScalarValue::Int(2),
            ScalarValue::Int(2),
        ],
    );
}

#[test]
fn when_gates_stream_with_guard() {
    let source = r#"
        let data = 42;
        let guard = true fby false;
        let result = data when guard;
    "#;
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");
    let result = interpreter
        .get_global("result")
        .and_then(|value| value.as_stream())
        .expect("result stream");
    assert_stream_eq(
        &result,
        &[
            ScalarValue::Int(42),
            ScalarValue::Undefined,
            ScalarValue::Undefined,
        ],
    );
}

#[test]
fn upon_holds_last_defined_value() {
    let source = r#"
        let src = 10 fby 20;
        let guard = true fby false;
        let result = src upon guard;
    "#;
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");
    let result = interpreter
        .get_global("result")
        .and_then(|value| value.as_stream())
        .expect("result stream");
    assert_stream_eq(
        &result,
        &[
            ScalarValue::Int(10),
            ScalarValue::Int(10),
            ScalarValue::Int(10),
        ],
    );
}

#[test]
fn defined_reports_presence() {
    let source = r#"
        let guard = true fby false;
        let payload = 5 when guard;
        let flags = defined payload;
    "#;
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");
    let flags = interpreter
        .get_global("flags")
        .and_then(|value| value.as_stream())
        .expect("flags stream");
    assert_stream_eq(
        &flags,
        &[
            ScalarValue::Bool(true),
            ScalarValue::Bool(false),
            ScalarValue::Bool(false),
        ],
    );
}

#[test]
fn value_block_apply_and_guard() {
    let source = r#"
        value Simple : Int {
            normalize it = it + 1
            require it < 5
            policy invalid = drop
        }

        let input = 0 fby 10;
        let applied = Simple.apply(input);
        let guard = Simple.guard(input);
        let repaired = Simple.use(input);
    "#;
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");

    let applied = interpreter
        .get_global("applied")
        .and_then(|value| value.as_stream())
        .expect("applied stream");
    assert_stream_eq(
        &applied,
        &[
            ScalarValue::Int(1),
            ScalarValue::Undefined,
            ScalarValue::Undefined,
        ],
    );

    let guard = interpreter
        .get_global("guard")
        .and_then(|value| value.as_stream())
        .expect("guard stream");
    assert_stream_eq(
        &guard,
        &[
            ScalarValue::Bool(true),
            ScalarValue::Bool(false),
            ScalarValue::Bool(false),
        ],
    );

    let repaired = interpreter
        .get_global("repaired")
        .and_then(|value| value.as_stream())
        .expect("repaired stream");
    assert_stream_eq(
        &repaired,
        &[
            ScalarValue::Int(1),
            ScalarValue::Undefined,
            ScalarValue::Undefined,
        ],
    );
}

#[test]
fn hold_repairs_missing_values() {
    let source = r#"
        let base = 1 fby 2;
        let gate = true fby false;
        let gated = base when gate;
        let repaired = hold gated;
    "#;
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");

    let repaired = interpreter
        .get_global("repaired")
        .and_then(|value| value.as_stream())
        .expect("repaired stream");
    assert_stream_eq(
        &repaired,
        &[
            ScalarValue::Int(1),
            ScalarValue::Int(1),
            ScalarValue::Int(1),
        ],
    );
}

#[test]
fn value_use_applies_custom_replacement_and_errors() {
    let source = r#"
        value Temperature : Int {
            require it < 10
            policy invalid = replace 42
            policy error = { code: "temp.invalid", detail: it }
        }

        let input = 5 fby 12;
        let used = Temperature.use(input);
        let errs = Temperature.errors(input);
    "#;
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");

    let used = interpreter
        .get_global("used")
        .and_then(|value| value.as_stream())
        .expect("used stream");
    assert_stream_eq(
        &used,
        &[
            ScalarValue::Int(5),
            ScalarValue::Int(42),
            ScalarValue::Int(42),
        ],
    );

    let errs = interpreter
        .get_global("errs")
        .and_then(|value| value.as_stream())
        .expect("errs stream");
    assert_stream_eq(
        &errs,
        &[
            ScalarValue::List(Vec::new()),
            ScalarValue::List(vec![error_record("temp.invalid", ScalarValue::Int(12), 1)]),
            ScalarValue::List(vec![error_record("temp.invalid", ScalarValue::Int(12), 2)]),
        ],
    );
}

#[test]
fn value_errors_emit_default_record_when_no_policy_specified() {
    let source = r#"
        value Basic : Int {
            require it > 0
        }

        let input = 0 fby 1;
        let errs = Basic.errors(input);
    "#;
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");

    let errs = interpreter
        .get_global("errs")
        .and_then(|value| value.as_stream())
        .expect("errs stream");
    assert_stream_eq(
        &errs,
        &[
            ScalarValue::List(vec![error_record(
                "Basic::invalid",
                ScalarValue::String("value guard failed".to_string()),
                0,
            )]),
            ScalarValue::List(Vec::new()),
            ScalarValue::List(Vec::new()),
        ],
    );
}

#[test]
fn unique_guard_flags_duplicate_samples() {
    let source = r#"
        value Email : String {
            always unique(it)
            policy invalid = drop
        }

        let tail = "dup" fby "fresh";
        let input = "dup" fby tail;
        let guard = Email.guard(input);
        let repaired = Email.use(input);
    "#;
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");

    let guard = interpreter
        .get_global("guard")
        .and_then(|value| value.as_stream())
        .expect("guard stream");
    assert_stream_eq(
        &guard,
        &[
            ScalarValue::Bool(true),
            ScalarValue::Bool(false),
            ScalarValue::Bool(true),
        ],
    );

    let repaired = interpreter
        .get_global("repaired")
        .and_then(|value| value.as_stream())
        .expect("repaired stream");
    assert_stream_eq(
        &repaired,
        &[
            ScalarValue::String("dup".to_string()),
            ScalarValue::Undefined,
            ScalarValue::String("fresh".to_string()),
        ],
    );
}

#[test]
fn unique_guard_within_window_limits_history() {
    let source = r#"
        value Limited : String {
            always unique(it, within: 2)
            policy invalid = drop
        }

        let input = "dup" fby ("dup" fby ("fresh" fby "dup"));
        let guard = Limited.guard(input);
        let repaired = Limited.use(input);
    "#;
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");

    let guard = interpreter
        .get_global("guard")
        .and_then(|value| value.as_stream())
        .expect("guard stream");
    assert_stream_eq(
        &guard,
        &[
            ScalarValue::Bool(true),
            ScalarValue::Bool(false),
            ScalarValue::Bool(true),
            ScalarValue::Bool(true),
        ],
    );

    let repaired = interpreter
        .get_global("repaired")
        .and_then(|value| value.as_stream())
        .expect("repaired stream");
    assert_stream_eq(
        &repaired,
        &[
            ScalarValue::String("dup".to_string()),
            ScalarValue::Undefined,
            ScalarValue::String("fresh".to_string()),
            ScalarValue::String("dup".to_string()),
        ],
    );
}

#[test]
fn unique_guard_within_window_respects_keys() {
    let source = r#"
        struct Event { user: String, score: Int }

        value LimitedScores : Event {
            always unique(it.score, by: it.user, within: 2)
            policy invalid = drop
        }

        let rec0 = { user: "alice", score: 1 };
        let rec1 = { user: "alice", score: 1 };
        let rec2 = { user: "alice", score: 1 };
        let rec3 = { user: "bob", score: 2 };
        let input = rec0 fby (rec1 fby (rec2 fby rec3));
        let guard = LimitedScores.guard(input);
        let repaired = LimitedScores.use(input);
    "#;
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");

    let guard = interpreter
        .get_global("guard")
        .and_then(|value| value.as_stream())
        .expect("guard stream");
    assert_stream_eq(
        &guard,
        &[
            ScalarValue::Bool(true),
            ScalarValue::Bool(false),
            ScalarValue::Bool(true),
            ScalarValue::Bool(true),
        ],
    );

    let repaired = interpreter
        .get_global("repaired")
        .and_then(|value| value.as_stream())
        .expect("repaired stream");
    assert_stream_eq(
        &repaired,
        &[
            user_record("alice", 1),
            ScalarValue::Undefined,
            user_record("alice", 1),
            user_record("bob", 2),
        ],
    );
}

#[test]
fn monotone_guard_detects_trend_violations() {
    let source = r#"
        value Rising : Int {
            always monotone(it)
            policy invalid = drop
        }

        let input = 1 fby (2 fby 1);
        let guard = Rising.guard(input);
        let repaired = Rising.use(input);

        value Desc : Int {
            always monotone(it, "decreasing")
            policy invalid = drop
        }

        let desc_input = 5 fby (4 fby 6);
        let desc_guard = Desc.guard(desc_input);
        let desc_repaired = Desc.use(desc_input);
    "#;
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");

    let guard = interpreter
        .get_global("guard")
        .and_then(|value| value.as_stream())
        .expect("guard stream");
    assert_stream_eq(
        &guard,
        &[
            ScalarValue::Bool(true),
            ScalarValue::Bool(true),
            ScalarValue::Bool(false),
            ScalarValue::Bool(true),
        ],
    );

    let repaired = interpreter
        .get_global("repaired")
        .and_then(|value| value.as_stream())
        .expect("repaired stream");
    assert_stream_eq(
        &repaired,
        &[
            ScalarValue::Int(1),
            ScalarValue::Int(2),
            ScalarValue::Undefined,
            ScalarValue::Int(1),
        ],
    );

    let desc_guard = interpreter
        .get_global("desc_guard")
        .and_then(|value| value.as_stream())
        .expect("desc guard stream");
    assert_stream_eq(
        &desc_guard,
        &[
            ScalarValue::Bool(true),
            ScalarValue::Bool(true),
            ScalarValue::Bool(false),
            ScalarValue::Bool(true),
        ],
    );

    let desc_repaired = interpreter
        .get_global("desc_repaired")
        .and_then(|value| value.as_stream())
        .expect("desc repaired stream");
    assert_stream_eq(
        &desc_repaired,
        &[
            ScalarValue::Int(5),
            ScalarValue::Int(4),
            ScalarValue::Undefined,
            ScalarValue::Int(6),
        ],
    );
}

#[test]
fn stabilizes_guard_waits_for_plateau() {
    let source = r#"
        value Stable : Int {
            always stabilizes(it, 1)
            policy invalid = drop
        }

        let input = 3 fby (5 fby (5 fby 5));
        let guard = Stable.guard(input);
        let repaired = Stable.use(input);
    "#;
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");

    let guard = interpreter
        .get_global("guard")
        .and_then(|value| value.as_stream())
        .expect("guard stream");
    assert_stream_eq(
        &guard,
        &[
            ScalarValue::Bool(false),
            ScalarValue::Bool(false),
            ScalarValue::Bool(true),
            ScalarValue::Bool(true),
        ],
    );

    let repaired = interpreter
        .get_global("repaired")
        .and_then(|value| value.as_stream())
        .expect("repaired stream");
    assert_stream_eq(
        &repaired,
        &[
            ScalarValue::Undefined,
            ScalarValue::Undefined,
            ScalarValue::Int(5),
            ScalarValue::Int(5),
        ],
    );
}

#[test]
fn rate_limit_guard_throttles_events() {
    let source = r#"
        value Rate : Bool {
            always rateLimit(it, per: 2)
            policy invalid = drop
        }

        let input = true fby (true fby (false fby true));
        let guard = Rate.guard(input);
        let repaired = Rate.use(input);
    "#;
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");

    let guard = interpreter
        .get_global("guard")
        .and_then(|value| value.as_stream())
        .expect("guard stream");
    assert_stream_eq(
        &guard,
        &[
            ScalarValue::Bool(true),
            ScalarValue::Bool(false),
            ScalarValue::Bool(true),
            ScalarValue::Bool(true),
        ],
    );

    let repaired = interpreter
        .get_global("repaired")
        .and_then(|value| value.as_stream())
        .expect("repaired stream");
    assert_stream_eq(
        &repaired,
        &[
            ScalarValue::Bool(true),
            ScalarValue::Undefined,
            ScalarValue::Bool(false),
            ScalarValue::Bool(true),
        ],
    );
}

#[test]
fn guard_composition_and_keyed_unique() {
    let source = r#"
        struct Event { user: String, score: Int }

        value Combined : Event {
            always unique(it.score, by: it.user) && monotone(it.score)
            policy invalid = drop
        }

        let rec0 = { user: "alice", score: 1 };
        let rec1 = { user: "alice", score: 1 };
        let rec2 = { user: "bob", score: 2 };
        let rec3 = { user: "bob", score: 1 };
        let input = rec0 fby (rec1 fby (rec2 fby rec3));
        let guard = Combined.guard(input);
        let repaired = Combined.use(input);
    "#;
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");

    let guard = interpreter
        .get_global("guard")
        .and_then(|value| value.as_stream())
        .expect("guard stream");
    assert_stream_eq(
        &guard,
        &[
            ScalarValue::Bool(true),
            ScalarValue::Bool(false),
            ScalarValue::Bool(true),
            ScalarValue::Bool(false),
        ],
    );

    let repaired = interpreter
        .get_global("repaired")
        .and_then(|value| value.as_stream())
        .expect("repaired stream");
    assert_stream_eq(
        &repaired,
        &[
            user_record("alice", 1),
            ScalarValue::Undefined,
            user_record("bob", 2),
            ScalarValue::Undefined,
        ],
    );
}

#[test]
fn no_gaps_guard_requires_defined_samples() {
    let source = r#"
        value Strict : Int {
            always noGaps(it)
            policy invalid = drop
        }

        let base = 1 fby 2;
        let gate = true fby false;
        let sparse = base when gate;
        let guard = Strict.guard(sparse);
        let repaired = Strict.use(sparse);
    "#;
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");

    let guard = interpreter
        .get_global("guard")
        .and_then(|value| value.as_stream())
        .expect("guard stream");
    assert_stream_eq(
        &guard,
        &[
            ScalarValue::Bool(true),
            ScalarValue::Bool(false),
            ScalarValue::Bool(false),
        ],
    );

    let repaired = interpreter
        .get_global("repaired")
        .and_then(|value| value.as_stream())
        .expect("repaired stream");
    assert_stream_eq(
        &repaired,
        &[
            ScalarValue::Int(1),
            ScalarValue::Undefined,
            ScalarValue::Undefined,
        ],
    );
}

#[test]
fn source_declaration_exposes_stream() {
    let source = r#"
        source numbers : S<Int> from (1 fby 2);

        let doubled = numbers + numbers;
    "#;
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");

    let numbers = interpreter
        .get_global("numbers")
        .and_then(|value| value.as_stream())
        .expect("numbers stream");
    assert_stream_eq(
        &numbers,
        &[
            ScalarValue::Int(1),
            ScalarValue::Int(2),
            ScalarValue::Int(2),
        ],
    );

    let doubled = interpreter
        .get_global("doubled")
        .and_then(|value| value.as_stream())
        .expect("doubled stream");
    assert_stream_eq(
        &doubled,
        &[
            ScalarValue::Int(2),
            ScalarValue::Int(4),
            ScalarValue::Int(4),
        ],
    );
}

#[test]
fn sink_declaration_captures_stream() {
    let source = r#"
        let base = 1 fby 2;
        sink outbox : S<Int> to (base + base);
    "#;
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");

    let sink = interpreter
        .get_sink("outbox")
        .expect("sink stream available");
    assert_stream_eq(
        &sink,
        &[
            ScalarValue::Int(2),
            ScalarValue::Int(4),
            ScalarValue::Int(4),
        ],
    );
}

#[test]
fn window_last_emits_sliding_lists() {
    let source = r#"
        let data = 1 fby (2 fby 3);
        let wins = window(last: 2, of: data);
    "#;
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");

    let wins = interpreter
        .get_global("wins")
        .and_then(|value| value.as_stream())
        .expect("wins stream");
    assert_stream_eq(
        &wins,
        &[
            ScalarValue::List(vec![ScalarValue::Int(1)]),
            ScalarValue::List(vec![ScalarValue::Int(1), ScalarValue::Int(2)]),
            ScalarValue::List(vec![ScalarValue::Int(2), ScalarValue::Int(3)]),
            ScalarValue::List(vec![ScalarValue::Int(3), ScalarValue::Int(3)]),
        ],
    );
}

#[test]
fn sum_over_window_accumulates_values() {
    let source = r#"
        let data = 1 fby (2 fby (3 fby 4));
        let wins = window(last: 3, of: data);
        let totals = sum(wins);
    "#;
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");

    let totals = interpreter
        .get_global("totals")
        .and_then(|value| value.as_stream())
        .expect("totals stream");
    assert_stream_eq(
        &totals,
        &[
            ScalarValue::Int(1),
            ScalarValue::Int(3),
            ScalarValue::Int(6),
            ScalarValue::Int(9),
            ScalarValue::Int(11),
        ],
    );
}

#[test]
fn avg_over_window_returns_mean() {
    let source = r#"
        let data = 1 fby (2 fby (3 fby 4));
        let wins = window(last: 3, of: data);
        let averages = avg(wins);
    "#;
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");

    let averages = interpreter
        .get_global("averages")
        .and_then(|value| value.as_stream())
        .expect("averages stream");
    assert_stream_eq(
        &averages,
        &[
            ScalarValue::Float(1.0),
            ScalarValue::Float(1.5),
            ScalarValue::Float(2.0),
            ScalarValue::Float(3.0),
            ScalarValue::Float(11.0 / 3.0),
        ],
    );
}

#[test]
fn count_distinct_over_window_tracks_uniques() {
    let source = r#"
        let data = 1 fby (1 fby (2 fby 3));
        let wins = window(last: 3, of: data);
        let uniques = countDistinct(wins);
    "#;
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");

    let uniques = interpreter
        .get_global("uniques")
        .and_then(|value| value.as_stream())
        .expect("uniques stream");
    assert_stream_eq(
        &uniques,
        &[
            ScalarValue::Int(1),
            ScalarValue::Int(1),
            ScalarValue::Int(2),
            ScalarValue::Int(3),
            ScalarValue::Int(2),
        ],
    );
}

#[test]
fn tick_every_pulses_on_interval() {
    let source = r#"
        let clock = tickEvery(3);
    "#;
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");

    let clock = interpreter
        .get_global("clock")
        .and_then(|value| value.as_stream())
        .expect("clock stream");
    assert_stream_eq(
        &clock,
        &[
            ScalarValue::Bool(true),
            ScalarValue::Bool(false),
            ScalarValue::Bool(false),
            ScalarValue::Bool(true),
            ScalarValue::Bool(false),
            ScalarValue::Bool(false),
            ScalarValue::Bool(true),
        ],
    );
}

#[test]
fn window_with_time_argument_trims_by_timestamp() {
    let source = r#"
        let data = 1 fby (2 fby (3 fby (4 fby 5)));
        let times = 0 fby (1 fby (3 fby (6 fby 7)));
        let wins = window(last: 2, of: data, time: times);
    "#;
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");

    let wins = interpreter
        .get_global("wins")
        .and_then(|value| value.as_stream())
        .expect("wins stream");
    assert_stream_eq(
        &wins,
        &[
            ScalarValue::List(vec![ScalarValue::Int(1)]),
            ScalarValue::List(vec![ScalarValue::Int(1), ScalarValue::Int(2)]),
            ScalarValue::List(vec![ScalarValue::Int(2), ScalarValue::Int(3)]),
            ScalarValue::List(vec![ScalarValue::Int(4)]),
            ScalarValue::List(vec![ScalarValue::Int(4), ScalarValue::Int(5)]),
        ],
    );
}

#[test]
fn fold_window_can_reproduce_sum() {
    let source = r#"
        fn sum_step(total, sample) {
            total + sample
        }

        let data = 1 fby (2 fby (3 fby 4));
        let wins = window(last: 3, of: data);
        let totals = foldWindow(wins, 0, sum_step);
        let builtin_totals = sum(wins);
    "#;
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");

    let totals = interpreter
        .get_global("totals")
        .and_then(|value| value.as_stream())
        .expect("totals stream");
    let builtin_totals = interpreter
        .get_global("builtin_totals")
        .and_then(|value| value.as_stream())
        .expect("builtin stream");

    let expected = &[
        ScalarValue::Int(1),
        ScalarValue::Int(3),
        ScalarValue::Int(6),
        ScalarValue::Int(9),
        ScalarValue::Int(11),
    ];
    assert_stream_eq(&totals, expected);
    assert_eq!(
        stream_samples(&totals, expected.len()),
        stream_samples(&builtin_totals, expected.len())
    );
}

#[test]
fn fold_window_with_finalizer_skips_undefined_samples() {
    let source = r#"
        fn avg_step(state, sample) {
            let total = state.total;
            let count = state.count;
            let valid = defined(sample);
            let next_total = if valid { total + sample } else { total };
            let next_count = if valid { count + 1 } else { count };
            { total: next_total, count: next_count }
        }

        fn avg_out(state) {
            let valid = state.count > 0;
            (state.total / state.count) when valid
        }

        let gap = 0 / 0;
        let data = gap fby (1.0 fby (gap fby (3.0 fby gap)));
        let wins = window(last: 2, of: data);
        let averages = foldWindow(window: wins, init: { total: 0.0, count: 0 }, step: avg_step, out: avg_out);
    "#;
    let module = parse_module(source).expect("parse");
    let mut interpreter = Interpreter::new();
    interpreter.evaluate_module(&module).expect("evaluate");

    let averages = interpreter
        .get_global("averages")
        .and_then(|value| value.as_stream())
        .expect("averages stream");

    assert_stream_eq(
        &averages,
        &[
            ScalarValue::Undefined,
            ScalarValue::Float(1.0),
            ScalarValue::Float(1.0),
            ScalarValue::Float(3.0),
            ScalarValue::Float(3.0),
            ScalarValue::Undefined,
        ],
    );
}
