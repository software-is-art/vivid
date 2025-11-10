use std::fmt::Write;

use super::core::{CoreBinaryOp, CoreExpr, CoreModule, CoreUnaryOp};
use crate::runtime::value::ScalarValue;

pub fn emit_c(module: &CoreModule) -> String {
    let mut out = String::new();
    writeln!(&mut out, "#include <stdint.h>").unwrap();
    writeln!(&mut out, "#include <stddef.h>").unwrap();
    writeln!(&mut out, "#include <stdbool.h>").unwrap();
    writeln!(&mut out, "#include <math.h>").unwrap();
    writeln!(&mut out, "#include <stdio.h>").unwrap();
    writeln!(&mut out, "#include <stdlib.h>").unwrap();
    writeln!(&mut out, "#include <string.h>").unwrap();
    writeln!(&mut out, "").unwrap();

    emit_unique_helpers(&mut out, module);
    emit_state_struct(&mut out, module);
    emit_outputs_struct(&mut out, module);
    emit_forward_decls(&mut out, module);
    emit_state_helpers(&mut out, module);
    emit_binding_defs(&mut out, module);
    emit_step_function(&mut out, module);
    emit_runtime_helpers(&mut out, module);
    emit_main(&mut out, module);

    out
}

fn emit_unique_helpers(out: &mut String, module: &CoreModule) {
    if module.unique_specs.is_empty() {
        return;
    }
    writeln!(
        out,
        "typedef struct {{ double* values; size_t len; size_t cap; }} UniqueBuffer;"
    )
    .unwrap();
    writeln!(
        out,
        "typedef struct {{ double* keys; double* values; size_t len; size_t cap; }} UniqueKeyBuffer;"
    )
    .unwrap();
    writeln!(out, "").unwrap();
    writeln!(
        out,
        "static inline void unique_buffer_init(UniqueBuffer* buf) {{"
    )
    .unwrap();
    writeln!(out, "    buf->values = NULL;").unwrap();
    writeln!(out, "    buf->len = 0;").unwrap();
    writeln!(out, "    buf->cap = 0;").unwrap();
    writeln!(out, "}}").unwrap();
    writeln!(out, "").unwrap();
    writeln!(
        out,
        "static inline void unique_buffer_reset(UniqueBuffer* buf) {{"
    )
    .unwrap();
    writeln!(out, "    if (buf->values) {{ free(buf->values); }}").unwrap();
    writeln!(out, "    buf->values = NULL;").unwrap();
    writeln!(out, "    buf->len = 0;").unwrap();
    writeln!(out, "    buf->cap = 0;").unwrap();
    writeln!(out, "}}").unwrap();
    writeln!(out, "").unwrap();
    writeln!(
        out,
        "static inline double unique_buffer_eval(UniqueBuffer* buf, double value) {{"
    )
    .unwrap();
    writeln!(out, "    for (size_t i = 0; i < buf->len; ++i) {{").unwrap();
    writeln!(
        out,
        "        if (buf->values[i] == value) {{ return 0.0; }}"
    )
    .unwrap();
    writeln!(out, "    }}").unwrap();
    writeln!(out, "    if (buf->len == buf->cap) {{").unwrap();
    writeln!(
        out,
        "        size_t new_cap = buf->cap == 0 ? 8 : (buf->cap * 2);"
    )
    .unwrap();
    writeln!(
        out,
        "        double* new_values = (double*)realloc(buf->values, sizeof(double) * new_cap);"
    )
    .unwrap();
    writeln!(
        out,
        "        if (!new_values) {{ fprintf(stderr, \"failed to grow unique buffer\\n\"); exit(1); }}"
    )
    .unwrap();
    writeln!(out, "        buf->values = new_values;").unwrap();
    writeln!(out, "        buf->cap = new_cap;").unwrap();
    writeln!(out, "    }}").unwrap();
    writeln!(out, "    buf->values[buf->len++] = value;").unwrap();
    writeln!(out, "    return 1.0;").unwrap();
    writeln!(out, "}}").unwrap();
    writeln!(out, "").unwrap();
    writeln!(
        out,
        "static inline void unique_key_buffer_init(UniqueKeyBuffer* buf) {{"
    )
    .unwrap();
    writeln!(out, "    buf->keys = NULL;").unwrap();
    writeln!(out, "    buf->values = NULL;").unwrap();
    writeln!(out, "    buf->len = 0;").unwrap();
    writeln!(out, "    buf->cap = 0;").unwrap();
    writeln!(out, "}}").unwrap();
    writeln!(out, "").unwrap();
    writeln!(
        out,
        "static inline void unique_key_buffer_reset(UniqueKeyBuffer* buf) {{"
    )
    .unwrap();
    writeln!(out, "    if (buf->keys) {{ free(buf->keys); }}").unwrap();
    writeln!(out, "    if (buf->values) {{ free(buf->values); }}").unwrap();
    writeln!(out, "    buf->keys = NULL;").unwrap();
    writeln!(out, "    buf->values = NULL;").unwrap();
    writeln!(out, "    buf->len = 0;").unwrap();
    writeln!(out, "    buf->cap = 0;").unwrap();
    writeln!(out, "}}").unwrap();
    writeln!(out, "").unwrap();
    writeln!(
        out,
        "static inline double unique_key_buffer_eval(UniqueKeyBuffer* buf, double key, double value) {{"
    )
    .unwrap();
    writeln!(out, "    for (size_t i = 0; i < buf->len; ++i) {{").unwrap();
    writeln!(
        out,
        "        if (buf->keys[i] == key && buf->values[i] == value) {{ return 0.0; }}"
    )
    .unwrap();
    writeln!(out, "    }}").unwrap();
    writeln!(out, "    if (buf->len == buf->cap) {{").unwrap();
    writeln!(
        out,
        "        size_t new_cap = buf->cap == 0 ? 8 : (buf->cap * 2);"
    )
    .unwrap();
    writeln!(
        out,
        "        double* new_keys = (double*)realloc(buf->keys, sizeof(double) * new_cap);"
    )
    .unwrap();
    writeln!(
        out,
        "        double* new_values = (double*)realloc(buf->values, sizeof(double) * new_cap);"
    )
    .unwrap();
    writeln!(
        out,
        "        if (!new_keys || !new_values) {{ fprintf(stderr, \"failed to grow unique key buffer\\n\"); exit(1); }}"
    )
    .unwrap();
    writeln!(out, "        buf->keys = new_keys;").unwrap();
    writeln!(out, "        buf->values = new_values;").unwrap();
    writeln!(out, "        buf->cap = new_cap;").unwrap();
    writeln!(out, "    }}").unwrap();
    writeln!(out, "    buf->keys[buf->len] = key;").unwrap();
    writeln!(out, "    buf->values[buf->len] = value;").unwrap();
    writeln!(out, "    buf->len += 1;").unwrap();
    writeln!(out, "    return 1.0;").unwrap();
    writeln!(out, "}}").unwrap();
    writeln!(out, "").unwrap();
}

fn emit_state_struct(out: &mut String, module: &CoreModule) {
    writeln!(out, "typedef struct {{").unwrap();
    for slot in &module.state_slots {
        writeln!(out, "    double {};", slot.name).unwrap();
    }
    for spec in &module.unique_specs {
        if spec.keyed {
            writeln!(out, "    UniqueKeyBuffer {};", spec.name).unwrap();
        } else {
            writeln!(out, "    UniqueBuffer {};", spec.name).unwrap();
        }
    }
    writeln!(out, "}} State;\n").unwrap();
}

fn emit_outputs_struct(out: &mut String, module: &CoreModule) {
    writeln!(out, "typedef struct {{").unwrap();
    for binding in &module.bindings {
        writeln!(out, "    double {};", binding.name).unwrap();
    }
    writeln!(out, "}} Outputs;\n").unwrap();
}

fn emit_forward_decls(out: &mut String, module: &CoreModule) {
    for binding in &module.bindings {
        writeln!(out, "double {}(State* state, int64_t tick);", binding.name).unwrap();
    }
    writeln!(
        out,
        "void step(State* state, Outputs* outputs, int64_t tick);"
    )
    .unwrap();
    writeln!(out, "void init_runtime(State* state, Outputs* outputs);").unwrap();
    writeln!(out, "void run_ticks(State* state, Outputs* outputs, int64_t start_tick, int64_t tick_count, bool quiet, FILE* log_file);").unwrap();
    writeln!(
        out,
        "void emit_outputs(const Outputs* outputs, int64_t tick, bool quiet, FILE* log_file);"
    )
    .unwrap();
    writeln!(
        out,
        "void print_outputs(const Outputs* outputs, int64_t tick);"
    )
    .unwrap();
    writeln!(out, "int main(int argc, char** argv);").unwrap();
    writeln!(out, "").unwrap();
}

fn emit_state_helpers(out: &mut String, module: &CoreModule) {
    writeln!(out, "static inline void init_state(State* state) {{").unwrap();
    if module.state_slots.is_empty() {
        if module.unique_specs.is_empty() {
            writeln!(out, "    (void)state;").unwrap();
        }
    } else {
        for slot in &module.state_slots {
            let init_expr = emit_expr(&slot.init, "state", "0");
            writeln!(out, "    state->{} = {};", slot.name, init_expr).unwrap();
        }
    }
    for spec in &module.unique_specs {
        if spec.keyed {
            writeln!(out, "    unique_key_buffer_init(&state->{});", spec.name).unwrap();
        } else {
            writeln!(out, "    unique_buffer_init(&state->{});", spec.name).unwrap();
        }
    }
    writeln!(out, "}}\n").unwrap();

    writeln!(
        out,
        "static inline void step_state(State* state, int64_t tick) {{"
    )
    .unwrap();
    if module.state_slots.is_empty() {
        writeln!(out, "    (void)state;").unwrap();
        writeln!(out, "    (void)tick;").unwrap();
    } else {
        for slot in &module.state_slots {
            let next_expr = emit_expr(&slot.next, "state", "tick");
            writeln!(out, "    double next_{} = {};", slot.name, next_expr).unwrap();
        }
        for slot in &module.state_slots {
            writeln!(out, "    state->{} = next_{};", slot.name, slot.name).unwrap();
        }
    }
    writeln!(out, "}}\n").unwrap();
}

fn emit_binding_defs(out: &mut String, module: &CoreModule) {
    for binding in &module.bindings {
        writeln!(
            out,
            "double {}(State* state, int64_t tick) {{",
            binding.name
        )
        .unwrap();
        let body = emit_expr(&binding.expr, "state", "tick");
        writeln!(out, "    return {};", body).unwrap();
        writeln!(out, "}}\n").unwrap();
    }
}

fn emit_step_function(out: &mut String, module: &CoreModule) {
    writeln!(
        out,
        "void step(State* state, Outputs* outputs, int64_t tick) {{"
    )
    .unwrap();
    if module.bindings.is_empty() {
        writeln!(out, "    (void)outputs;").unwrap();
    } else {
        for binding in &module.bindings {
            writeln!(
                out,
                "    outputs->{name} = {name}(state, tick);",
                name = binding.name
            )
            .unwrap();
        }
    }
    writeln!(out, "    step_state(state, tick);").unwrap();
    writeln!(out, "}}\n").unwrap();
}

fn emit_runtime_helpers(out: &mut String, module: &CoreModule) {
    writeln!(out, "static inline void init_outputs(Outputs* outputs) {{").unwrap();
    if module.bindings.is_empty() {
        writeln!(out, "    (void)outputs;").unwrap();
    } else {
        for binding in &module.bindings {
            writeln!(out, "    outputs->{name} = 0.0;", name = binding.name).unwrap();
        }
    }
    writeln!(out, "}}\n").unwrap();

    writeln!(out, "void init_runtime(State* state, Outputs* outputs) {{").unwrap();
    writeln!(out, "    init_state(state);").unwrap();
    writeln!(out, "    init_outputs(outputs);").unwrap();
    writeln!(out, "}}\n").unwrap();

    writeln!(out, "void run_ticks(State* state, Outputs* outputs, int64_t start_tick, int64_t tick_count, bool quiet, FILE* log_file) {{").unwrap();
    writeln!(out, "    if (start_tick > 0) {{").unwrap();
    writeln!(
        out,
        "        for (int64_t warm_tick = 0; warm_tick < start_tick; ++warm_tick) {{"
    )
    .unwrap();
    writeln!(out, "            step(state, outputs, warm_tick);").unwrap();
    writeln!(out, "        }}").unwrap();
    writeln!(out, "    }}").unwrap();
    writeln!(out, "    for (int64_t i = 0; i < tick_count; ++i) {{").unwrap();
    writeln!(out, "        int64_t tick = start_tick + i;").unwrap();
    writeln!(out, "        step(state, outputs, tick);").unwrap();
    writeln!(out, "        emit_outputs(outputs, tick, quiet, log_file);").unwrap();
    writeln!(out, "    }}").unwrap();
    writeln!(out, "}}\n").unwrap();
}

fn emit_expr(expr: &CoreExpr, state_ident: &str, tick_expr: &str) -> String {
    match expr {
        CoreExpr::Literal(value) => emit_literal(value),
        CoreExpr::Ref(name) => format!("{}({}, {})", name, state_ident, tick_expr),
        CoreExpr::StateRef(slot) => format!("{}->{}", state_ident, slot),
        CoreExpr::Unary { op, expr } => {
            let inner = emit_expr(expr, state_ident, tick_expr);
            match op {
                CoreUnaryOp::Neg => format!("(-({}))", inner),
                CoreUnaryOp::Not => format!("((({}) == 0.0) ? 1.0 : 0.0)", inner),
            }
        }
        CoreExpr::Binary { op, left, right } => {
            let lhs = emit_expr(left, state_ident, tick_expr);
            let rhs = emit_expr(right, state_ident, tick_expr);
            match op {
                CoreBinaryOp::Add => format!("(({}) + ({}))", lhs, rhs),
                CoreBinaryOp::Sub => format!("(({}) - ({}))", lhs, rhs),
                CoreBinaryOp::Mul => format!("(({}) * ({}))", lhs, rhs),
                CoreBinaryOp::Div => format!("(({}) / ({}))", lhs, rhs),
                CoreBinaryOp::Mod => format!("fmod(({}), ({}))", lhs, rhs),
                CoreBinaryOp::Eq => format!("((({}) == ({})) ? 1.0 : 0.0)", lhs, rhs),
                CoreBinaryOp::Neq => format!("((({}) != ({})) ? 1.0 : 0.0)", lhs, rhs),
                CoreBinaryOp::Lt => format!("((({}) < ({})) ? 1.0 : 0.0)", lhs, rhs),
                CoreBinaryOp::Lte => format!("((({}) <= ({})) ? 1.0 : 0.0)", lhs, rhs),
                CoreBinaryOp::Gt => format!("((({}) > ({})) ? 1.0 : 0.0)", lhs, rhs),
                CoreBinaryOp::Gte => format!("((({}) >= ({})) ? 1.0 : 0.0)", lhs, rhs),
                CoreBinaryOp::And => {
                    format!("((({}) != 0.0 && ({}) != 0.0) ? 1.0 : 0.0)", lhs, rhs)
                }
                CoreBinaryOp::Or => format!("((({}) != 0.0 || ({}) != 0.0) ? 1.0 : 0.0)", lhs, rhs),
            }
        }
        CoreExpr::Next(stream) => {
            let next_tick = format!("({} + 1)", tick_expr);
            emit_expr(stream, state_ident, &next_tick)
        }
        CoreExpr::First(stream) => emit_expr(stream, state_ident, "0"),
        CoreExpr::Fby { .. } => unreachable!("Fby should be rewritten into state slots"),
        CoreExpr::When { value, guard } => {
            let guard_expr = emit_expr(guard, state_ident, tick_expr);
            let value_expr = emit_expr(value, state_ident, tick_expr);
            format!("((({}) != 0.0) ? ({}) : 0.0)", guard_expr, value_expr)
        }
        CoreExpr::WindowSum { .. } => {
            unreachable!("WindowSum expressions should be rewritten before codegen")
        }
        CoreExpr::Monotone { .. } => {
            unreachable!("Monotone expressions should be rewritten before codegen")
        }
        CoreExpr::UniqueWithin { .. } => {
            unreachable!("UniqueWithin expressions should be rewritten before codegen")
        }
        CoreExpr::UniqueUnbounded { store, value, key } => {
            let value_expr = emit_expr(value, state_ident, tick_expr);
            if let Some(key_expr) = key {
                let key_expr = emit_expr(key_expr, state_ident, tick_expr);
                format!(
                    "unique_key_buffer_eval(&({}->{}), {}, {})",
                    state_ident, store, key_expr, value_expr
                )
            } else {
                format!(
                    "unique_buffer_eval(&({}->{}), {})",
                    state_ident, store, value_expr
                )
            }
        }
    }
}

fn emit_literal(value: &ScalarValue) -> String {
    match value {
        ScalarValue::Int(v) => format!("((double)({}))", v),
        ScalarValue::Float(v) => format!("({})", v),
        ScalarValue::Bool(true) => "1.0".to_string(),
        ScalarValue::Bool(false) => "0.0".to_string(),
        _ => "0.0".to_string(),
    }
}

fn emit_main(out: &mut String, module: &CoreModule) {
    writeln!(
        out,
        "void emit_outputs(const Outputs* outputs, int64_t tick, bool quiet, FILE* log_file) {{"
    )
    .unwrap();
    if module.bindings.is_empty() {
        writeln!(out, "    (void)outputs;").unwrap();
        writeln!(out, "    (void)tick;").unwrap();
        writeln!(out, "    (void)quiet;").unwrap();
        writeln!(out, "    (void)log_file;").unwrap();
    } else {
        writeln!(out, "    if (!quiet) {{").unwrap();
        writeln!(out, "        printf(\"tick %lld\\n\", (long long)tick);").unwrap();
        for binding in &module.bindings {
            writeln!(
                out,
                "        printf(\"  {name} = %f\\n\", outputs->{name});",
                name = binding.name
            )
            .unwrap();
        }
        writeln!(out, "        fflush(stdout);").unwrap();
        writeln!(out, "    }}").unwrap();
        writeln!(out, "    if (log_file) {{").unwrap();
        writeln!(out, "        fprintf(log_file, \"%lld\", (long long)tick);").unwrap();
        for binding in &module.bindings {
            writeln!(
                out,
                "        fprintf(log_file, \",%f\", outputs->{name});",
                name = binding.name
            )
            .unwrap();
        }
        writeln!(out, "        fputc('\\n', log_file);").unwrap();
        writeln!(out, "        fflush(log_file);").unwrap();
        writeln!(out, "    }}").unwrap();
    }
    writeln!(out, "}}\n").unwrap();

    writeln!(
        out,
        "void print_outputs(const Outputs* outputs, int64_t tick) {{"
    )
    .unwrap();
    if module.bindings.is_empty() {
        writeln!(out, "    (void)outputs;").unwrap();
        writeln!(out, "    (void)tick;").unwrap();
    } else {
        writeln!(out, "    printf(\"tick %lld\\n\", (long long)tick);").unwrap();
        for binding in &module.bindings {
            writeln!(
                out,
                "    printf(\"  {name} = %f\\n\", outputs->{name});",
                name = binding.name
            )
            .unwrap();
        }
        writeln!(out, "    fflush(stdout);").unwrap();
    }
    writeln!(out, "}}\n").unwrap();

    writeln!(out, "int main(int argc, char** argv) {{").unwrap();
    writeln!(out, "    int64_t tick_count = 10;").unwrap();
    writeln!(out, "    int64_t start_tick = 0;").unwrap();
    writeln!(out, "    bool quiet = false;").unwrap();
    writeln!(out, "    const char* log_path = NULL;").unwrap();
    writeln!(out, "    for (int i = 1; i < argc; ++i) {{").unwrap();
    writeln!(out, "        const char* arg = argv[i];").unwrap();
    writeln!(
        out,
        "        if (strcmp(arg, \"--quiet\") == 0) {{ quiet = true; continue; }}"
    )
    .unwrap();
    writeln!(out, "        if (strncmp(arg, \"--ticks=\", 8) == 0) {{ long long parsed = atoll(arg + 8); if (parsed > 0) tick_count = parsed; continue; }}").unwrap();
    writeln!(out, "        if (strncmp(arg, \"--start=\", 8) == 0) {{ long long parsed = atoll(arg + 8); if (parsed >= 0) start_tick = parsed; continue; }}").unwrap();
    writeln!(
        out,
        "        if (strncmp(arg, \"--log=\", 6) == 0) {{ log_path = arg + 6; continue; }}"
    )
    .unwrap();
    writeln!(
        out,
        "        long long parsed = atoll(arg); if (parsed > 0) tick_count = parsed;"
    )
    .unwrap();
    writeln!(out, "    }}").unwrap();
    writeln!(out, "    State state;").unwrap();
    writeln!(out, "    Outputs outputs;").unwrap();
    writeln!(out, "    init_runtime(&state, &outputs);").unwrap();
    writeln!(out, "    FILE* log_file = NULL;").unwrap();
    writeln!(out, "    if (log_path) {{").unwrap();
    writeln!(out, "        log_file = fopen(log_path, \"w\");").unwrap();
    writeln!(out, "        if (!log_file) {{ fprintf(stderr, \"failed to open %s\\n\", log_path); return 1; }}").unwrap();
    writeln!(out, "        fprintf(log_file, \"tick\");").unwrap();
    for binding in &module.bindings {
        writeln!(
            out,
            "        fprintf(log_file, \",%s\", \"{name}\");",
            name = binding.name
        )
        .unwrap();
    }
    writeln!(out, "        fputc('\\n', log_file);").unwrap();
    writeln!(out, "        fflush(log_file);").unwrap();
    writeln!(out, "    }}").unwrap();
    writeln!(
        out,
        "    run_ticks(&state, &outputs, start_tick, tick_count, quiet, log_file);"
    )
    .unwrap();
    writeln!(out, "    if (log_file) {{ fclose(log_file); }}").unwrap();
    writeln!(out, "    return 0;").unwrap();
    writeln!(out, "}}").unwrap();
}
