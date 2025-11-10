use std::fs;
use std::process::Command;

use tempfile::tempdir;
use vivid::compiler::Compiler;
use vivid::parser::parse_module;
use vivid::runtime::Interpreter;
use vivid::runtime::value::ScalarValue;
use vivid::types::TypeChecker;

fn compile(source: &str) -> Result<String, Vec<String>> {
    let module = parse_module(source).map_err(|diags| {
        diags
            .into_iter()
            .map(|d| format!("{:?}: {}", d.kind, d.message))
            .collect::<Vec<_>>()
    })?;
    let mut checker = TypeChecker::new();
    checker.infer_module(&module).map_err(|diags| {
        diags
            .into_iter()
            .map(|d| format!("{:?}: {}", d.kind, d.message))
            .collect::<Vec<_>>()
    })?;
    let compiler = Compiler::new();
    compiler.compile_to_c(&module).map_err(|diags| {
        diags
            .into_iter()
            .map(|d| format!("{:?}: {}", d.kind, d.message))
            .collect::<Vec<_>>()
    })
}

#[test]
fn compiler_emits_c_for_simple_expression() {
    let c = compile("let score = 1 + 2 * 3;").expect("compile");
    assert!(c.contains("double score(State* state, int64_t tick);"));
    assert!(
        c.contains("} Outputs;"),
        "expected Outputs struct in generated C: {}",
        c
    );
    assert!(c.contains("void init_runtime(State* state, Outputs* outputs);"));
    assert!(
        c.contains("void run_ticks(State* state, Outputs* outputs, int64_t start_tick, int64_t tick_count, bool quiet, FILE* log_file);")
    );
    assert!(
        c.contains(
            "void emit_outputs(const Outputs* outputs, int64_t tick, bool quiet, FILE* log_file);"
        ),
        "missing emit_outputs declaration: {}",
        c
    );
    assert!(
        c.contains("fprintf(log_file, \",%s\", \"score\");"),
        "missing CSV header emission: {}",
        c
    );
    assert!(
        c.contains("fprintf(log_file, \",%f\", outputs->score);"),
        "missing CSV data emission: {}",
        c
    );
    assert!(
        c.contains("run_ticks(&state, &outputs, start_tick, tick_count, quiet, log_file);"),
        "main should call run_ticks: {}",
        c
    );
    assert!(c.contains("((double)(1))"));
}

#[test]
fn compiler_rejects_strings_for_now() {
    let err = compile("let label = \"hi\";").expect_err("expected failure");
    assert!(
        err.iter()
            .any(|msg| msg.contains("supports only numeric and Bool literals")),
        "missing literal diagnostic: {:?}",
        err
    );
}

#[test]
fn compiler_rejects_bare_window_for_now() {
    let err = compile(
        r#"
        let samples = 1 fby (2 fby 3);
        let wins = window(last: 3, of: samples);
    "#,
    )
    .expect_err("expected failure");
    assert!(
        err.iter()
            .any(|msg| msg.contains("lowers windowed aggregates only via sum")),
        "expected window diagnostic for bare window usage: {:?}",
        err
    );
}

#[test]
fn compiler_handles_fby_and_next() {
    let source = r#"
        let pulses = 1 fby 0;
        let shifted = next pulses;
    "#;
    let c = compile(source).expect("compile");
    assert!(
        c.contains("state->state_0"),
        "missing state slot ref: {}",
        c
    );
    assert!(
        c.contains("pulses(state, (tick + 1))"),
        "expected next translation in generated C: {}",
        c
    );
    assert!(
        c.contains("outputs->pulses = pulses(state, tick);"),
        "step helper should write pulses output: {}",
        c
    );
}

#[test]
fn compiler_handles_when_guard() {
    let source = r#"
        let pulses = 1 fby 0;
        let gated = pulses when (pulses > 0);
    "#;
    let c = compile(source).expect("compile");
    assert!(
        c.contains("? (pulses(state, tick))"),
        "expected gated expression: {}",
        c
    );
    assert!(
        c.contains("outputs->gated = gated(state, tick);"),
        "step helper should write gated output: {}",
        c
    );
}

#[test]
fn compiler_flags_unsupported_temporal_builtin() {
    let err = compile("let repaired = hold(1);").expect_err("expected failure");
    assert!(
        err.iter()
            .any(|msg| msg.contains("does not yet lower calls"))
    );
}

#[test]
fn compiler_emits_state_struct_for_fby() {
    let c = compile("let pulses = 1 fby 0;").expect("compile");
    assert!(
        c.contains("typedef struct"),
        "expected state struct in generated C: {}",
        c
    );
    assert!(
        c.contains("double state_0"),
        "missing state slot field: {}",
        c
    );
    assert!(c.contains("init_state"), "missing init_state helper: {}", c);
    assert!(
        c.contains("void step(State* state, Outputs* outputs, int64_t tick)"),
        "missing step helper: {}",
        c
    );
    assert!(c.contains("void init_runtime(State* state, Outputs* outputs)"));
    assert!(
        c.contains("void run_ticks(State* state, Outputs* outputs, int64_t start_tick, int64_t tick_count, bool quiet, FILE* log_file)")
    );
}

#[test]
fn compiled_runner_emits_same_csv_as_interpreter() {
    compile_and_compare(
        r#"
        let pulses = 1 fby 0;
        let shifted = next pulses;
    "#,
        &["pulses", "shifted"],
        1,
        4,
    );
}

#[test]
fn compiled_runner_respects_guards_and_bools() {
    compile_and_compare(
        r#"
        let pulses = 1 fby 0;
        let positive = pulses > 0;
        let gated = pulses when positive;
    "#,
        &["pulses", "positive", "gated"],
        0,
        5,
    );
}

#[test]
fn compiled_runner_handles_monotone_guard() {
    compile_and_compare(
        r#"
        let samples = 1 fby (2 fby (1 fby 3));
        let ok = monotone(samples);
    "#,
        &["samples", "ok"],
        0,
        5,
    );
}

#[test]
fn compiled_runner_handles_unique_guard() {
    compile_and_compare(
        r#"
        let samples = 1 fby (2 fby (1 fby 3));
        let buckets = samples + 10;
        let ok = unique(samples, by: buckets, within: 2);
    "#,
        &["samples", "buckets", "ok"],
        0,
        6,
    );
}

#[test]
fn compiled_runner_handles_unique_guard_unbounded() {
    compile_and_compare(
        r#"
        let samples = 1 fby (2 fby (1 fby (3 fby 2)));
        let ok = unique(samples);
    "#,
        &["samples", "ok"],
        0,
        6,
    );
}

#[test]
fn compiled_runner_handles_unique_guard_unbounded_keyed() {
    compile_and_compare(
        r#"
        let values = 1 fby (2 fby (1 fby (2 fby 3)));
        let users = 42 fby (42 fby (7 fby (7 fby 42)));
        let ok = unique(values, by: users);
    "#,
        &["values", "users", "ok"],
        0,
        6,
    );
}

#[test]
fn compiled_runner_handles_guard_and() {
    compile_and_compare(
        r#"
        let a = true fby (false fby (true fby true));
        let b = true fby (true fby (false fby true));
        let c = false fby (true fby (true fby true));
        let both = guard_and(a, b, c);
    "#,
        &["a", "b", "c", "both"],
        0,
        4,
    );
}

#[test]
fn compiled_runner_handles_guard_or() {
    compile_and_compare(
        r#"
        let a = false fby (true fby false);
        let b = false fby (false fby true);
        let c = true fby (false fby false);
        let any = guard_or(a, b, c);
    "#,
        &["a", "b", "c", "any"],
        0,
        4,
    );
}

#[test]
fn compiled_runner_handles_window_sum() {
    compile_and_compare(
        r#"
        let samples = 1 fby (2 fby (3 fby 4));
        let totals = sum(window(last: 3, of: samples));
    "#,
        &["samples", "totals"],
        0,
        5,
    );
}

fn compile_and_compare(source: &str, binding_names: &[&str], start_tick: usize, tick_count: usize) {
    let module = parse_module(source).expect("parse");
    let mut checker = TypeChecker::new();
    checker.infer_module(&module).expect("type");

    let mut interpreter = Interpreter::new();
    interpreter
        .evaluate_module(&module)
        .expect("interpreter evaluation");
    let expected: Vec<Vec<f64>> = binding_names
        .iter()
        .map(|name| sample_stream_range(&interpreter, name, start_tick, tick_count))
        .collect();

    let compiler = Compiler::new();
    let c_source = compiler.compile_to_c(&module).expect("codegen");
    let tempdir = tempdir().expect("temp dir");
    let c_path = tempdir.path().join("program.c");
    let bin_path = tempdir.path().join("program");
    fs::write(&c_path, c_source).expect("write c file");

    let status = Command::new("cc")
        .arg("-std=c99")
        .arg(&c_path)
        .arg("-lm")
        .arg("-o")
        .arg(&bin_path)
        .status()
        .expect("invoke cc");
    assert!(status.success(), "cc failed with status {:?}", status);

    let log_path = tempdir.path().join("out.csv");
    let output = Command::new(&bin_path)
        .arg("--quiet")
        .arg(format!("--ticks={}", tick_count))
        .arg(format!("--start={}", start_tick))
        .arg(format!("--log={}", log_path.display()))
        .output()
        .expect("run compiled program");
    assert!(
        output.status.success(),
        "program failed: status {:?}, stderr: {}",
        output.status.code(),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        output.stdout.is_empty(),
        "expected --quiet to suppress stdout, saw: {}",
        String::from_utf8_lossy(&output.stdout)
    );

    let csv = fs::read_to_string(&log_path).expect("read log file");
    let mut lines = csv.lines();
    let header = lines.next().expect("csv header");
    let header_cols: Vec<&str> = header.split(',').collect();
    assert_eq!(
        header_cols.first().copied().unwrap_or(""),
        "tick",
        "CSV header missing tick column"
    );
    let mut column_lookup = Vec::new();
    for name in binding_names {
        let idx = header_cols
            .iter()
            .position(|col| col == name)
            .unwrap_or_else(|| panic!("column '{}' missing from CSV header {}", name, header));
        column_lookup.push(idx);
    }

    for (row_idx, line) in lines.enumerate() {
        if line.trim().is_empty() {
            continue;
        }
        let cols: Vec<&str> = line.split(',').collect();
        let tick_value: usize = cols
            .get(0)
            .expect("tick column")
            .parse()
            .expect("tick integer");
        assert_eq!(
            tick_value,
            start_tick + row_idx,
            "tick column mismatch at row {}",
            row_idx
        );

        for (col_idx, column_index) in column_lookup.iter().enumerate() {
            let value_str = cols
                .get(*column_index)
                .unwrap_or_else(|| panic!("row {} missing column index {}", row_idx, column_index));
            let actual: f64 = value_str.parse().expect("numeric column");
            let expected_value = expected[col_idx][row_idx];
            assert!(
                (actual - expected_value).abs() < 1e-9,
                "value mismatch for {} at tick {}: expected {}, saw {} (row {})",
                binding_names[col_idx],
                tick_value,
                expected_value,
                actual,
                row_idx
            );
        }
    }
}

fn sample_stream_range(
    interpreter: &Interpreter,
    name: &str,
    start: usize,
    count: usize,
) -> Vec<f64> {
    let stream = interpreter
        .get_global(name)
        .and_then(|value| value.as_stream())
        .unwrap_or_else(|| panic!("missing stream for binding '{}'", name));
    (0..count)
        .map(|offset| {
            let tick = start + offset;
            scalar_to_f64(stream.value_at(tick))
        })
        .collect()
}

fn scalar_to_f64(value: ScalarValue) -> f64 {
    match value {
        ScalarValue::Int(v) => v as f64,
        ScalarValue::Float(v) => v,
        ScalarValue::Bool(true) => 1.0,
        ScalarValue::Bool(false) => 0.0,
        ScalarValue::Undefined => 0.0,
        other => panic!("expected numeric scalar, got {:?}", other),
    }
}
