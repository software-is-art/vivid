use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use std::rc::Rc;

use crate::ast::*;
use crate::diagnostics::{Diagnostic, DiagnosticKind};

use super::env::Environment;
use super::stream::Stream;
use super::value::{
    BuiltinFunction, CallArg, FunctionValue, RuntimeValue, ScalarValue, ValueTypeMethod,
    ValueTypeMethodRuntime, ValueTypeRuntime,
};

pub struct Interpreter {
    globals: HashMap<String, RuntimeValue>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut interpreter = Self {
            globals: HashMap::new(),
        };
        interpreter.install_builtins();
        interpreter
    }

    fn install_builtins(&mut self) {
        self.register_builtin(BuiltinFunction::new("next", 1, Some(1), |args| {
            ensure_no_labels("next", args)?;
            let stream = expect_stream(&args[0].value, "next")?;
            Ok(RuntimeValue::Stream(Stream::new(move |tick| {
                stream.value_at(tick + 1)
            })))
        }));

        self.register_builtin(BuiltinFunction::new("first", 1, Some(1), |args| {
            ensure_no_labels("first", args)?;
            let stream = expect_stream(&args[0].value, "first")?;
            let first = stream.value_at(0);
            Ok(RuntimeValue::Stream(Stream::constant(first)))
        }));

        self.register_builtin(BuiltinFunction::new("rest", 1, Some(1), |args| {
            ensure_no_labels("rest", args)?;
            let stream = expect_stream(&args[0].value, "rest")?;
            Ok(RuntimeValue::Stream(Stream::new(move |tick| {
                stream.value_at(tick + 1)
            })))
        }));

        self.register_builtin(BuiltinFunction::new("defined", 1, Some(1), |args| {
            ensure_no_labels("defined", args)?;
            let stream = expect_stream(&args[0].value, "defined")?;
            Ok(RuntimeValue::Stream(Stream::new(move |tick| {
                let value = stream.value_at(tick);
                ScalarValue::Bool(value.is_defined())
            })))
        }));

        self.register_builtin(BuiltinFunction::new("when", 2, Some(2), |args| {
            ensure_no_labels("when", args)?;
            let value_stream = expect_stream(&args[0].value, "when value")?;
            let guard_stream = expect_stream(&args[1].value, "when guard")?;
            Ok(RuntimeValue::Stream(Stream::new(move |tick| {
                let guard = guard_stream.value_at(tick);
                if guard.as_bool().unwrap_or(false) {
                    value_stream.value_at(tick)
                } else {
                    ScalarValue::Undefined
                }
            })))
        }));

        self.register_builtin(BuiltinFunction::new("upon", 2, Some(2), |args| {
            ensure_no_labels("upon", args)?;
            let value_stream = expect_stream(&args[0].value, "upon value")?;
            let guard_stream = expect_stream(&args[1].value, "upon guard")?;
            Ok(RuntimeValue::Stream(make_upon_stream(
                value_stream,
                guard_stream,
            )))
        }));

        self.register_builtin(BuiltinFunction::new("hold", 1, Some(1), |args| {
            ensure_no_labels("hold", args)?;
            let source_stream = expect_stream(&args[0].value, "hold source")?;
            Ok(RuntimeValue::Stream(make_hold_stream(source_stream)))
        }));

        self.register_builtin(BuiltinFunction::new("unique", 1, Some(2), |args| {
            let mut value_stream = None;
            let mut key_stream = None;

            for arg in args.iter() {
                match arg.label.as_deref() {
                    Some("value") => {
                        if value_stream.is_some() {
                            return Err(Diagnostic::new(
                                DiagnosticKind::Eval,
                                "duplicate 'value' argument to unique",
                            ));
                        }
                        value_stream = Some(expect_stream(&arg.value, "unique value")?);
                    }
                    Some("by") | Some("key") => {
                        if key_stream.is_some() {
                            return Err(Diagnostic::new(
                                DiagnosticKind::Eval,
                                "duplicate key argument to unique",
                            ));
                        }
                        key_stream = Some(expect_stream(&arg.value, "unique key")?);
                    }
                    Some(label) => {
                        return Err(Diagnostic::new(
                            DiagnosticKind::Eval,
                            format!("unknown label '{}' for unique", label),
                        ));
                    }
                    None => {
                        if value_stream.is_none() {
                            value_stream = Some(expect_stream(&arg.value, "unique value")?);
                        } else if key_stream.is_none() {
                            key_stream = Some(expect_stream(&arg.value, "unique key")?);
                        } else {
                            return Err(Diagnostic::new(
                                DiagnosticKind::Eval,
                                "unique accepts at most two arguments",
                            ));
                        }
                    }
                }
            }

            let value_stream = value_stream.ok_or_else(|| {
                Diagnostic::new(
                    DiagnosticKind::Eval,
                    "unique requires a value stream argument",
                )
            })?;

            Ok(RuntimeValue::Stream(make_unique_guard(
                value_stream,
                key_stream,
            )))
        }));

        self.register_builtin(BuiltinFunction::new("monotone", 1, Some(2), |args| {
            let mut source_stream = None;
            let mut direction = None;

            for arg in args.iter() {
                match arg.label.as_deref() {
                    Some("value") => {
                        if source_stream.is_some() {
                            return Err(Diagnostic::new(
                                DiagnosticKind::Eval,
                                "duplicate 'value' argument to monotone",
                            ));
                        }
                        source_stream = Some(expect_stream(&arg.value, "monotone value")?);
                    }
                    Some("direction") | Some("by") => {
                        if direction.is_some() {
                            return Err(Diagnostic::new(
                                DiagnosticKind::Eval,
                                "duplicate direction argument to monotone",
                            ));
                        }
                        let stream = expect_stream(&arg.value, "monotone direction")?;
                        direction = Some(parse_monotone_direction(&stream.value_at(0))?);
                    }
                    Some(label) => {
                        return Err(Diagnostic::new(
                            DiagnosticKind::Eval,
                            format!("unknown label '{}' for monotone", label),
                        ));
                    }
                    None => {
                        if source_stream.is_none() {
                            source_stream = Some(expect_stream(&arg.value, "monotone value")?);
                        } else if direction.is_none() {
                            let stream = expect_stream(&arg.value, "monotone direction")?;
                            direction = Some(parse_monotone_direction(&stream.value_at(0))?);
                        } else {
                            return Err(Diagnostic::new(
                                DiagnosticKind::Eval,
                                "monotone accepts at most two arguments",
                            ));
                        }
                    }
                }
            }

            let source_stream = source_stream.ok_or_else(|| {
                Diagnostic::new(DiagnosticKind::Eval, "monotone requires a value stream")
            })?;

            let direction = direction.unwrap_or(MonotoneDirection::NonDecreasing);
            Ok(RuntimeValue::Stream(make_monotone_guard(
                source_stream,
                direction,
            )))
        }));

        self.register_builtin(BuiltinFunction::new("stabilizes", 2, Some(2), |args| {
            let mut source_stream = None;
            let mut window_value = None;

            for arg in args {
                match arg.label.as_deref() {
                    Some("value") => {
                        if source_stream.is_some() {
                            return Err(Diagnostic::new(
                                DiagnosticKind::Eval,
                                "duplicate 'value' argument to stabilizes",
                            ));
                        }
                        source_stream = Some(expect_stream(&arg.value, "stabilizes value")?);
                    }
                    Some("within") | Some("window") => {
                        if window_value.is_some() {
                            return Err(Diagnostic::new(
                                DiagnosticKind::Eval,
                                "duplicate window argument to stabilizes",
                            ));
                        }
                        let stream = expect_stream(&arg.value, "stabilizes window")?;
                        window_value = Some(stream.value_at(0));
                    }
                    Some(label) => {
                        return Err(Diagnostic::new(
                            DiagnosticKind::Eval,
                            format!("unknown label '{}' for stabilizes", label),
                        ));
                    }
                    None => {
                        if source_stream.is_none() {
                            source_stream = Some(expect_stream(&arg.value, "stabilizes value")?);
                        } else if window_value.is_none() {
                            let stream = expect_stream(&arg.value, "stabilizes window")?;
                            window_value = Some(stream.value_at(0));
                        } else {
                            return Err(Diagnostic::new(
                                DiagnosticKind::Eval,
                                "stabilizes expects exactly two arguments",
                            ));
                        }
                    }
                }
            }

            let source_stream = source_stream.ok_or_else(|| {
                Diagnostic::new(DiagnosticKind::Eval, "stabilizes requires a value stream")
            })?;
            let window_raw = window_value.ok_or_else(|| {
                Diagnostic::new(
                    DiagnosticKind::Eval,
                    "stabilizes requires a window argument",
                )
            })?;
            let window = parse_stabilizes_window(&window_raw)?;
            Ok(RuntimeValue::Stream(make_stabilizes_guard(
                source_stream,
                window,
            )))
        }));

        self.register_builtin(BuiltinFunction::new("rateLimit", 2, Some(2), |args| {
            let mut event_stream = None;
            let mut interval_value = None;

            for arg in args {
                match arg.label.as_deref() {
                    Some("events") => {
                        if event_stream.is_some() {
                            return Err(Diagnostic::new(
                                DiagnosticKind::Eval,
                                "duplicate 'events' argument to rateLimit",
                            ));
                        }
                        event_stream = Some(expect_stream(&arg.value, "rateLimit events")?);
                    }
                    Some("per") | Some("interval") => {
                        if interval_value.is_some() {
                            return Err(Diagnostic::new(
                                DiagnosticKind::Eval,
                                "duplicate interval argument to rateLimit",
                            ));
                        }
                        let stream = expect_stream(&arg.value, "rateLimit interval")?;
                        interval_value = Some(stream.value_at(0));
                    }
                    Some(label) => {
                        return Err(Diagnostic::new(
                            DiagnosticKind::Eval,
                            format!("unknown label '{}' for rateLimit", label),
                        ));
                    }
                    None => {
                        if event_stream.is_none() {
                            event_stream = Some(expect_stream(&arg.value, "rateLimit events")?);
                        } else if interval_value.is_none() {
                            let stream = expect_stream(&arg.value, "rateLimit interval")?;
                            interval_value = Some(stream.value_at(0));
                        } else {
                            return Err(Diagnostic::new(
                                DiagnosticKind::Eval,
                                "rateLimit expects exactly two arguments",
                            ));
                        }
                    }
                }
            }

            let event_stream = event_stream.ok_or_else(|| {
                Diagnostic::new(DiagnosticKind::Eval, "rateLimit requires an event stream")
            })?;
            let interval_raw = interval_value.ok_or_else(|| {
                Diagnostic::new(
                    DiagnosticKind::Eval,
                    "rateLimit requires an interval argument",
                )
            })?;
            let interval = parse_rate_limit_interval(&interval_raw)?;
            Ok(RuntimeValue::Stream(make_rate_limit_guard(
                event_stream,
                interval,
            )))
        }));

        self.register_builtin(BuiltinFunction::new("window", 2, Some(2), |args| {
            let mut value_stream = None;
            let mut size_value = None;

            for arg in args {
                match arg.label.as_deref() {
                    Some("of") | Some("value") => {
                        if value_stream.is_some() {
                            return Err(Diagnostic::new(
                                DiagnosticKind::Eval,
                                "duplicate 'of' argument to window",
                            ));
                        }
                        value_stream = Some(expect_stream(&arg.value, "window value")?);
                    }
                    Some("last") | Some("size") => {
                        if size_value.is_some() {
                            return Err(Diagnostic::new(
                                DiagnosticKind::Eval,
                                "duplicate window size argument",
                            ));
                        }
                        let stream = expect_stream(&arg.value, "window size")?;
                        size_value = Some(stream.value_at(0));
                    }
                    Some(label) => {
                        return Err(Diagnostic::new(
                            DiagnosticKind::Eval,
                            format!("unknown label '{}' for window", label),
                        ));
                    }
                    None => {
                        if size_value.is_none() {
                            let stream = expect_stream(&arg.value, "window size")?;
                            size_value = Some(stream.value_at(0));
                        } else if value_stream.is_none() {
                            value_stream = Some(expect_stream(&arg.value, "window value")?);
                        } else {
                            return Err(Diagnostic::new(
                                DiagnosticKind::Eval,
                                "window expects exactly two arguments",
                            ));
                        }
                    }
                }
            }

            let value_stream = value_stream.ok_or_else(|| {
                Diagnostic::new(DiagnosticKind::Eval, "window requires an input stream")
            })?;
            let size_raw = size_value.ok_or_else(|| {
                Diagnostic::new(DiagnosticKind::Eval, "window requires a size argument")
            })?;
            let size = parse_window_size(&size_raw)?;
            Ok(RuntimeValue::Stream(make_window_stream(value_stream, size)))
        }));

        self.register_builtin(BuiltinFunction::new("guard_and", 2, Some(2), |args| {
            ensure_no_labels("guard_and", args)?;
            let left = expect_stream(&args[0].value, "guard_and left")?;
            let right = expect_stream(&args[1].value, "guard_and right")?;
            Ok(RuntimeValue::Stream(make_guard_and(left, right)))
        }));

        self.register_builtin(BuiltinFunction::new("guard_or", 2, Some(2), |args| {
            ensure_no_labels("guard_or", args)?;
            let left = expect_stream(&args[0].value, "guard_or left")?;
            let right = expect_stream(&args[1].value, "guard_or right")?;
            Ok(RuntimeValue::Stream(make_guard_or(left, right)))
        }));

        self.register_builtin(BuiltinFunction::new("noGaps", 1, Some(1), |args| {
            ensure_no_labels("noGaps", args)?;
            let stream = expect_stream(&args[0].value, "noGaps value")?;
            Ok(RuntimeValue::Stream(make_no_gaps_guard(stream)))
        }));
    }

    fn register_builtin(&mut self, builtin: BuiltinFunction) {
        self.globals
            .insert(builtin.name.to_string(), RuntimeValue::Builtin(builtin));
    }

    pub fn get_global(&self, name: &str) -> Option<RuntimeValue> {
        self.globals.get(name).cloned()
    }

    pub fn evaluate_module(&mut self, module: &Module) -> Result<(), Vec<Diagnostic>> {
        let mut diagnostics = Vec::new();
        for item in &module.items {
            if let Err(mut diag) = self.evaluate_item(item) {
                diagnostics.append(&mut diag);
            }
        }
        if diagnostics.is_empty() {
            Ok(())
        } else {
            Err(diagnostics)
        }
    }

    fn evaluate_item(&mut self, item: &Item) -> Result<(), Vec<Diagnostic>> {
        match item {
            Item::Function(func) => {
                let mut params = Vec::new();
                for pattern in &func.params {
                    if let Pattern::Identifier(ident) = pattern {
                        params.push(ident.name.clone());
                    } else {
                        return Err(vec![
                            Diagnostic::new(
                                DiagnosticKind::Eval,
                                "function parameters must be simple identifiers",
                            )
                            .with_span(pattern.span()),
                        ]);
                    }
                }
                let function = FunctionValue {
                    name: func.name.name.clone(),
                    params,
                    body: func.body.clone(),
                    env: Environment::from_map(self.globals.clone()),
                };
                self.globals
                    .insert(func.name.name.clone(), RuntimeValue::Function(function));
                Ok(())
            }
            Item::Let(let_decl) => {
                let env = Environment::from_map(self.globals.clone());
                let value = self.evaluate_expr(&let_decl.expr, &env)?;
                if let Pattern::Identifier(ident) = &let_decl.pattern {
                    self.globals.insert(ident.name.clone(), value);
                    Ok(())
                } else {
                    Err(vec![
                        Diagnostic::new(
                            DiagnosticKind::Eval,
                            "top-level let supports identifier patterns only",
                        )
                        .with_span(let_decl.pattern.span()),
                    ])
                }
            }
            Item::Value(value_decl) => {
                let runtime = Rc::new(self.build_value_type(value_decl)?);
                self.globals.insert(
                    value_decl.name.name.clone(),
                    RuntimeValue::ValueType(runtime),
                );
                Ok(())
            }
            Item::Struct(_) | Item::Source(_) | Item::Sink(_) => Ok(()),
        }
    }

    fn build_value_type(&self, decl: &ValueDecl) -> Result<ValueTypeRuntime, Vec<Diagnostic>> {
        Ok(ValueTypeRuntime {
            decl: Rc::new(decl.clone()),
            env: Environment::from_map(self.globals.clone()),
        })
    }

    pub fn evaluate_expr(
        &self,
        expr: &Expr,
        env: &Environment,
    ) -> Result<RuntimeValue, Vec<Diagnostic>> {
        match expr {
            Expr::Literal(literal, _) => {
                Ok(RuntimeValue::Stream(Stream::constant(match literal {
                    Literal::Int(value) => ScalarValue::Int(*value),
                    Literal::Float(value) => ScalarValue::Float(*value),
                    Literal::Bool(value) => ScalarValue::Bool(*value),
                    Literal::String(value) => ScalarValue::String(value.clone()),
                    Literal::Null => ScalarValue::Undefined,
                })))
            }
            Expr::Identifier(ident) => env
                .get(&ident.name)
                .or_else(|| self.globals.get(&ident.name).cloned())
                .ok_or_else(|| {
                    vec![
                        Diagnostic::new(
                            DiagnosticKind::Eval,
                            format!("unbound identifier '{}'", ident.name),
                        )
                        .with_span(ident.span),
                    ]
                }),
            Expr::Unary { op, expr, .. } => {
                let value = self.evaluate_expr(expr, env)?;
                match op {
                    UnaryOp::Neg => {
                        let stream =
                            expect_stream(&value, "negation").map_err(|diag| vec![diag])?;
                        Ok(RuntimeValue::Stream(Stream::new(
                            move |tick| match stream.value_at(tick) {
                                ScalarValue::Int(v) => ScalarValue::Int(-v),
                                ScalarValue::Float(v) => ScalarValue::Float(-v),
                                _ => ScalarValue::Undefined,
                            },
                        )))
                    }
                    UnaryOp::Not => {
                        let stream =
                            expect_stream(&value, "logical not").map_err(|diag| vec![diag])?;
                        Ok(RuntimeValue::Stream(Stream::new(move |tick| match stream
                            .value_at(tick)
                            .as_bool()
                        {
                            Some(v) => ScalarValue::Bool(!v),
                            None => ScalarValue::Undefined,
                        })))
                    }
                }
            }
            Expr::Binary {
                op, left, right, ..
            } => {
                let left_val = self.evaluate_expr(left, env)?;
                let right_val = self.evaluate_expr(right, env)?;
                self.evaluate_binary(*op, left_val, right_val)
            }
            Expr::Call {
                callee, arguments, ..
            } => {
                let callee_value = self.evaluate_expr(callee, env)?;
                let mut arg_values = Vec::new();
                for argument in arguments {
                    let value = self.evaluate_expr(&argument.expr, env)?;
                    arg_values.push(CallArg {
                        label: argument.label.as_ref().map(|id| id.name.clone()),
                        value,
                    });
                }
                self.apply_callable(callee_value, arg_values)
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                let condition_value = self.evaluate_expr(condition, env)?;
                let condition_stream =
                    expect_stream(&condition_value, "if condition").map_err(|diag| vec![diag])?;
                let then_value = self.evaluate_expr(then_branch, env)?;
                let else_value = if let Some(else_branch) = else_branch {
                    self.evaluate_expr(else_branch, env)?
                } else {
                    RuntimeValue::Stream(Stream::constant(ScalarValue::Undefined))
                };
                let then_stream =
                    expect_stream(&then_value, "if then").map_err(|diag| vec![diag])?;
                let else_stream =
                    expect_stream(&else_value, "if else").map_err(|diag| vec![diag])?;
                Ok(RuntimeValue::Stream(Stream::new(
                    move |tick| match condition_stream.value_at(tick).as_bool() {
                        Some(true) => then_stream.value_at(tick),
                        Some(false) => else_stream.value_at(tick),
                        None => ScalarValue::Undefined,
                    },
                )))
            }
            Expr::Let { bindings, body, .. } => {
                let mut scoped_env = env.clone();
                for binding in bindings {
                    let value = self.evaluate_expr(&binding.expr, &scoped_env)?;
                    if let Pattern::Identifier(ident) = &binding.pattern {
                        scoped_env = scoped_env.extend(ident.name.clone(), value);
                    } else {
                        return Err(vec![
                            Diagnostic::new(
                                DiagnosticKind::Eval,
                                "only identifier patterns supported in let bindings",
                            )
                            .with_span(binding.pattern.span()),
                        ]);
                    }
                }
                self.evaluate_expr(body, &scoped_env)
            }
            Expr::Block {
                statements, tail, ..
            } => {
                let mut scoped_env = env.clone();
                let mut last_value = RuntimeValue::Scalar(ScalarValue::Undefined);
                for statement in statements {
                    match statement {
                        Stmt::Let(binding) => {
                            let value = self.evaluate_expr(&binding.expr, &scoped_env)?;
                            if let Pattern::Identifier(ident) = &binding.pattern {
                                scoped_env = scoped_env.extend(ident.name.clone(), value);
                            } else {
                                return Err(vec![
                                    Diagnostic::new(
                                        DiagnosticKind::Eval,
                                        "only identifier patterns supported in let statements",
                                    )
                                    .with_span(binding.pattern.span()),
                                ]);
                            }
                        }
                        Stmt::Expr(expr) => {
                            last_value = self.evaluate_expr(expr, &scoped_env)?;
                        }
                    }
                }
                if let Some(tail) = tail {
                    self.evaluate_expr(tail, &scoped_env)
                } else {
                    Ok(last_value)
                }
            }
            Expr::Tuple(elements, _) => {
                let streams = elements
                    .iter()
                    .map(|expr| self.evaluate_expr(expr, env))
                    .collect::<Result<Vec<_>, _>>()?;
                let mut stream_values = Vec::new();
                for value in streams {
                    stream_values
                        .push(expect_stream(&value, "tuple element").map_err(|diag| vec![diag])?);
                }
                Ok(RuntimeValue::Stream(Stream::new(move |tick| {
                    let values = stream_values
                        .iter()
                        .map(|stream| stream.value_at(tick))
                        .collect();
                    ScalarValue::List(values)
                })))
            }
            Expr::List(elements, _) => {
                let streams = elements
                    .iter()
                    .map(|expr| self.evaluate_expr(expr, env))
                    .collect::<Result<Vec<_>, _>>()?;
                let mut stream_values = Vec::new();
                for value in streams {
                    stream_values
                        .push(expect_stream(&value, "list element").map_err(|diag| vec![diag])?);
                }
                Ok(RuntimeValue::Stream(Stream::new(move |tick| {
                    let values = stream_values
                        .iter()
                        .map(|stream| stream.value_at(tick))
                        .collect();
                    ScalarValue::List(values)
                })))
            }
            Expr::Record(fields, _) => {
                let mut stream_fields = Vec::new();
                for (ident, expr) in fields {
                    let value = self.evaluate_expr(expr, env)?;
                    let stream =
                        expect_stream(&value, "record field").map_err(|diag| vec![diag])?;
                    stream_fields.push((ident.name.clone(), stream));
                }
                Ok(RuntimeValue::Stream(Stream::new(move |tick| {
                    let mut record = BTreeMap::new();
                    for (name, stream) in &stream_fields {
                        record.insert(name.clone(), stream.value_at(tick));
                    }
                    ScalarValue::Record(record)
                })))
            }
            Expr::Access { target, field, .. } => {
                let target_value = self.evaluate_expr(target, env)?;
                if let Some(value_type) = target_value.as_value_type() {
                    let method = match field.name.as_str() {
                        "apply" => Some(ValueTypeMethod::Apply),
                        "guard" => Some(ValueTypeMethod::Guard),
                        "use" => Some(ValueTypeMethod::Use),
                        "use_fn" => Some(ValueTypeMethod::Use),
                        "errors" => Some(ValueTypeMethod::Errors),
                        _ => None,
                    };
                    if let Some(method) = method {
                        Ok(RuntimeValue::ValueTypeMethod(ValueTypeMethodRuntime {
                            value_type,
                            method,
                        }))
                    } else {
                        Err(vec![
                            Diagnostic::new(
                                DiagnosticKind::Eval,
                                format!(
                                    "value type '{}' has no member '{}'",
                                    value_type.decl.name.name, field.name
                                ),
                            )
                            .with_span(field.span),
                        ])
                    }
                } else {
                    let stream =
                        expect_stream(&target_value, "record access").map_err(|diag| vec![diag])?;
                    let field_name = field.name.clone();
                    Ok(RuntimeValue::Stream(Stream::new(
                        move |tick| match stream.value_at(tick) {
                            ScalarValue::Record(ref record) => record
                                .get(&field_name)
                                .cloned()
                                .unwrap_or(ScalarValue::Undefined),
                            _ => ScalarValue::Undefined,
                        },
                    )))
                }
            }
            Expr::Index { target, index, .. } => {
                let target_value = self.evaluate_expr(target, env)?;
                let index_value = self.evaluate_expr(index, env)?;
                let target_stream =
                    expect_stream(&target_value, "index target").map_err(|diag| vec![diag])?;
                let index_stream =
                    expect_stream(&index_value, "index value").map_err(|diag| vec![diag])?;
                Ok(RuntimeValue::Stream(Stream::new(move |tick| {
                    let container = target_stream.value_at(tick);
                    let index_val = index_stream.value_at(tick);
                    match (container, index_val.as_int()) {
                        (ScalarValue::List(items), Some(idx)) => items
                            .into_iter()
                            .nth(idx as usize)
                            .unwrap_or(ScalarValue::Undefined),
                        _ => ScalarValue::Undefined,
                    }
                })))
            }
            _ => Err(vec![Diagnostic::new(
                DiagnosticKind::Eval,
                "expression form not yet supported",
            )]),
        }
    }

    fn evaluate_binary(
        &self,
        op: BinaryOp,
        left: RuntimeValue,
        right: RuntimeValue,
    ) -> Result<RuntimeValue, Vec<Diagnostic>> {
        let left_stream = expect_stream(&left, "binary left").map_err(|diag| vec![diag])?;
        let right_stream = expect_stream(&right, "binary right").map_err(|diag| vec![diag])?;
        Ok(RuntimeValue::Stream(match op {
            BinaryOp::Add => left_stream
                .clone()
                .zip(right_stream.clone(), |a, b| match (a, b) {
                    (ScalarValue::Int(x), ScalarValue::Int(y)) => ScalarValue::Int(x + y),
                    (ScalarValue::Float(x), ScalarValue::Float(y)) => ScalarValue::Float(x + y),
                    (ScalarValue::Int(x), ScalarValue::Float(y)) => {
                        ScalarValue::Float(x as f64 + y)
                    }
                    (ScalarValue::Float(x), ScalarValue::Int(y)) => {
                        ScalarValue::Float(x + y as f64)
                    }
                    _ => ScalarValue::Undefined,
                }),
            BinaryOp::Sub => left_stream
                .clone()
                .zip(right_stream.clone(), |a, b| match (a, b) {
                    (ScalarValue::Int(x), ScalarValue::Int(y)) => ScalarValue::Int(x - y),
                    (ScalarValue::Float(x), ScalarValue::Float(y)) => ScalarValue::Float(x - y),
                    (ScalarValue::Int(x), ScalarValue::Float(y)) => {
                        ScalarValue::Float(x as f64 - y)
                    }
                    (ScalarValue::Float(x), ScalarValue::Int(y)) => {
                        ScalarValue::Float(x - y as f64)
                    }
                    _ => ScalarValue::Undefined,
                }),
            BinaryOp::Mul => left_stream
                .clone()
                .zip(right_stream.clone(), |a, b| match (a, b) {
                    (ScalarValue::Int(x), ScalarValue::Int(y)) => ScalarValue::Int(x * y),
                    (ScalarValue::Float(x), ScalarValue::Float(y)) => ScalarValue::Float(x * y),
                    (ScalarValue::Int(x), ScalarValue::Float(y)) => {
                        ScalarValue::Float(x as f64 * y)
                    }
                    (ScalarValue::Float(x), ScalarValue::Int(y)) => {
                        ScalarValue::Float(x * y as f64)
                    }
                    _ => ScalarValue::Undefined,
                }),
            BinaryOp::Div => left_stream
                .clone()
                .zip(right_stream.clone(), |a, b| match (a, b) {
                    (ScalarValue::Int(x), ScalarValue::Int(y)) => {
                        if y == 0 {
                            ScalarValue::Undefined
                        } else {
                            ScalarValue::Int(x / y)
                        }
                    }
                    (ScalarValue::Float(x), ScalarValue::Float(y)) => {
                        if y == 0.0 {
                            ScalarValue::Undefined
                        } else {
                            ScalarValue::Float(x / y)
                        }
                    }
                    (ScalarValue::Int(x), ScalarValue::Float(y)) => {
                        if y == 0.0 {
                            ScalarValue::Undefined
                        } else {
                            ScalarValue::Float(x as f64 / y)
                        }
                    }
                    (ScalarValue::Float(x), ScalarValue::Int(y)) => {
                        if y == 0 {
                            ScalarValue::Undefined
                        } else {
                            ScalarValue::Float(x / y as f64)
                        }
                    }
                    _ => ScalarValue::Undefined,
                }),
            BinaryOp::Mod => left_stream
                .clone()
                .zip(right_stream.clone(), |a, b| match (a, b) {
                    (ScalarValue::Int(x), ScalarValue::Int(y)) => {
                        if y == 0 {
                            ScalarValue::Undefined
                        } else {
                            ScalarValue::Int(x % y)
                        }
                    }
                    _ => ScalarValue::Undefined,
                }),
            BinaryOp::Eq => left_stream
                .clone()
                .zip(right_stream.clone(), |a, b| ScalarValue::Bool(a == b)),
            BinaryOp::Neq => left_stream
                .clone()
                .zip(right_stream.clone(), |a, b| ScalarValue::Bool(a != b)),
            BinaryOp::Lt => left_stream.clone().zip(right_stream.clone(), |a, b| {
                ScalarValue::Bool(compare(&a, &b, |x, y| x < y))
            }),
            BinaryOp::Lte => left_stream.clone().zip(right_stream.clone(), |a, b| {
                ScalarValue::Bool(compare(&a, &b, |x, y| x <= y))
            }),
            BinaryOp::Gt => left_stream.clone().zip(right_stream.clone(), |a, b| {
                ScalarValue::Bool(compare(&a, &b, |x, y| x > y))
            }),
            BinaryOp::Gte => left_stream.clone().zip(right_stream.clone(), |a, b| {
                ScalarValue::Bool(compare(&a, &b, |x, y| x >= y))
            }),
            BinaryOp::And => left_stream.clone().zip(right_stream.clone(), |a, b| {
                let left = a.as_bool().unwrap_or(false);
                let right = b.as_bool().unwrap_or(false);
                ScalarValue::Bool(left && right)
            }),
            BinaryOp::Or => left_stream.clone().zip(right_stream.clone(), |a, b| {
                let left = a.as_bool().unwrap_or(false);
                let right = b.as_bool().unwrap_or(false);
                ScalarValue::Bool(left || right)
            }),
            BinaryOp::Fby => {
                let left = left_stream.clone();
                let right = right_stream.clone();
                Stream::new(move |tick| {
                    if tick == 0 {
                        left.value_at(0)
                    } else {
                        right.value_at(tick - 1)
                    }
                })
            }
        }))
    }

    fn apply_callable(
        &self,
        callee: RuntimeValue,
        args: Vec<CallArg>,
    ) -> Result<RuntimeValue, Vec<Diagnostic>> {
        if let Some(function) = callee.as_function() {
            if function.params.len() != args.len() {
                return Err(vec![Diagnostic::new(
                    DiagnosticKind::Eval,
                    format!(
                        "function '{}' expected {} arguments, found {}",
                        function.name,
                        function.params.len(),
                        args.len()
                    ),
                )]);
            }
            if args.iter().any(|arg| arg.label.is_some()) {
                return Err(vec![Diagnostic::new(
                    DiagnosticKind::Eval,
                    format!(
                        "function '{}' does not accept labeled arguments",
                        function.name
                    ),
                )]);
            }
            let mut env_map = function.env.clone();
            for (param, arg) in function.params.iter().zip(args.into_iter()) {
                env_map = env_map.extend(param.clone(), arg.value);
            }
            self.evaluate_expr(&function.body, &env_map)
        } else if let Some(builtin) = callee.as_builtin() {
            if args.len() < builtin.min_arity
                || builtin
                    .max_arity
                    .map(|max| args.len() > max)
                    .unwrap_or(false)
            {
                return Err(vec![Diagnostic::new(
                    DiagnosticKind::Eval,
                    format!(
                        "builtin '{}' expects {} arguments",
                        builtin.name,
                        match builtin.max_arity {
                            Some(max) if max != builtin.min_arity =>
                                format!("{} to {}", builtin.min_arity, max),
                            _ => builtin.min_arity.to_string(),
                        }
                    ),
                )]);
            }
            (builtin.handler)(&args).map_err(|diag| vec![diag])
        } else if let Some(method) = callee.as_value_type_method() {
            self.invoke_value_type_method(method, args)
        } else {
            Err(vec![Diagnostic::new(
                DiagnosticKind::Eval,
                "attempted to call non-callable value",
            )])
        }
    }

    fn invoke_value_type_method(
        &self,
        method: ValueTypeMethodRuntime,
        args: Vec<CallArg>,
    ) -> Result<RuntimeValue, Vec<Diagnostic>> {
        if args.len() != 1 {
            return Err(vec![Diagnostic::new(
                DiagnosticKind::Eval,
                format!(
                    "value '{}' method expects exactly one stream argument",
                    method.value_type.decl.name.name
                ),
            )]);
        }

        if let Some(label) = &args[0].label {
            return Err(vec![Diagnostic::new(
                DiagnosticKind::Eval,
                format!(
                    "value '{}' method does not accept labeled argument '{}'",
                    method.value_type.decl.name.name, label
                ),
            )]);
        }

        let input_stream = expect_stream_vec(&args[0].value, "value method argument")?;
        let eval = self.evaluate_value_type(&method.value_type, input_stream.clone())?;
        let ValueTypeEvaluation {
            normalized,
            guard,
            errors,
            invalid_policy,
        } = eval;

        match method.method {
            ValueTypeMethod::Apply => {
                let result = mask_stream_with_guard(normalized.clone(), guard.clone());
                Ok(RuntimeValue::Stream(result))
            }
            ValueTypeMethod::Guard => Ok(RuntimeValue::Stream(guard)),
            ValueTypeMethod::Use => {
                let stream = apply_invalid_policy(normalized, guard, invalid_policy);
                Ok(RuntimeValue::Stream(stream))
            }
            ValueTypeMethod::Errors => Ok(RuntimeValue::Stream(errors)),
        }
    }

    fn evaluate_value_type(
        &self,
        value_type: &ValueTypeRuntime,
        input_stream: Stream,
    ) -> Result<ValueTypeEvaluation, Vec<Diagnostic>> {
        let mut env = value_type.env.clone();
        env = env.extend("it", RuntimeValue::Stream(input_stream.clone()));

        for rule in &value_type.decl.normalize {
            let value = self.evaluate_expr(&rule.expr, &env)?;
            let stream = expect_stream_vec(&value, "normalize expression")?;
            let ident = match &rule.target {
                Pattern::Identifier(ident) => ident.name.clone(),
                _ => {
                    return Err(vec![
                        Diagnostic::new(
                            DiagnosticKind::Eval,
                            "only identifier patterns supported in normalize rules",
                        )
                        .with_span(rule.span),
                    ]);
                }
            };
            env = env.extend(ident, RuntimeValue::Stream(stream.clone()));
        }

        let normalized_value = env
            .get("it")
            .unwrap_or(RuntimeValue::Stream(input_stream.clone()));
        let normalized = expect_stream_vec(&normalized_value, "normalized stream")?;

        let mut guard_expressions = Vec::new();
        for require_expr in &value_type.decl.requires {
            let value = self.evaluate_expr(require_expr, &env)?;
            guard_expressions.push(expect_stream_vec(&value, "require expression")?);
        }
        for always_expr in &value_type.decl.always {
            let value = self.evaluate_expr(always_expr, &env)?;
            guard_expressions.push(expect_stream_vec(&value, "always expression")?);
        }

        let guard = combine_guards(guard_expressions);

        let mut error_streams = Vec::new();
        for policy in &value_type.decl.policies {
            if matches!(policy.kind, PolicyKind::Error) {
                match &policy.expr {
                    PolicyExpr::Expr(expr) => {
                        let value = self.evaluate_expr(expr, &env)?;
                        let stream = expect_stream_vec(&value, "error policy expression")?;
                        error_streams.push(stream);
                    }
                    PolicyExpr::Replace(expr) => {
                        let value = self.evaluate_expr(expr, &env)?;
                        let stream = expect_stream_vec(&value, "error policy expression")?;
                        error_streams.push(stream);
                    }
                    PolicyExpr::Builtin(builtin) => {
                        return Err(vec![
                            Diagnostic::new(
                                DiagnosticKind::Eval,
                                format!(
                                    "policy error does not support builtin '{}'",
                                    builtin_name(builtin)
                                ),
                            )
                            .with_span(policy.span),
                        ]);
                    }
                }
            }
        }

        let default_error_code = format!("{}::invalid", value_type.decl.name.name);
        let errors = make_error_stream(guard.clone(), error_streams, default_error_code);

        let invalid_policy = self.resolve_invalid_policy(value_type, &env)?;

        Ok(ValueTypeEvaluation {
            normalized,
            guard,
            errors,
            invalid_policy,
        })
    }

    fn resolve_invalid_policy(
        &self,
        value_type: &ValueTypeRuntime,
        env: &Environment,
    ) -> Result<InvalidPolicyRuntime, Vec<Diagnostic>> {
        let policy_rule = value_type
            .decl
            .policies
            .iter()
            .find(|rule| matches!(rule.kind, PolicyKind::Invalid));

        match policy_rule {
            None => Ok(InvalidPolicyRuntime::Drop),
            Some(rule) => match &rule.expr {
                PolicyExpr::Builtin(PolicyBuiltin::Drop) => Ok(InvalidPolicyRuntime::Drop),
                PolicyExpr::Builtin(PolicyBuiltin::Hold) => Ok(InvalidPolicyRuntime::Hold),
                PolicyExpr::Builtin(PolicyBuiltin::Replace) => Err(vec![
                    Diagnostic::new(
                        DiagnosticKind::Eval,
                        "policy invalid = replace requires a replacement expression",
                    )
                    .with_span(rule.span),
                ]),
                PolicyExpr::Replace(expr) => {
                    let value = self.evaluate_expr(expr, env)?;
                    let stream = expect_stream_vec(&value, "invalid policy expression")?;
                    Ok(InvalidPolicyRuntime::Replace(stream))
                }
                PolicyExpr::Expr(expr) => {
                    let value = self.evaluate_expr(expr, env)?;
                    let stream = expect_stream_vec(&value, "invalid policy expression")?;
                    Ok(InvalidPolicyRuntime::Replace(stream))
                }
            },
        }
    }
}

fn expect_stream(value: &RuntimeValue, context: &str) -> Result<Stream, Diagnostic> {
    value.as_stream().ok_or_else(|| {
        Diagnostic::new(
            DiagnosticKind::Eval,
            format!("{} expects stream argument", context),
        )
    })
}

fn expect_stream_vec(value: &RuntimeValue, context: &str) -> Result<Stream, Vec<Diagnostic>> {
    expect_stream(value, context).map_err(|diag| vec![diag])
}

fn builtin_name(builtin: &PolicyBuiltin) -> &'static str {
    match builtin {
        PolicyBuiltin::Drop => "drop",
        PolicyBuiltin::Hold => "hold",
        PolicyBuiltin::Replace => "replace",
    }
}

fn combine_guards(streams: Vec<Stream>) -> Stream {
    let mut acc = Stream::constant(ScalarValue::Bool(true));
    for guard in streams {
        let left = acc.clone();
        let right = guard.clone();
        acc = Stream::new(move |tick| {
            let lhs = left.value_at(tick).as_bool().unwrap_or(false);
            let rhs = right.value_at(tick).as_bool().unwrap_or(false);
            ScalarValue::Bool(lhs && rhs)
        });
    }
    acc
}

fn apply_invalid_policy(normalized: Stream, guard: Stream, policy: InvalidPolicyRuntime) -> Stream {
    match policy {
        InvalidPolicyRuntime::Drop => mask_stream_with_guard(normalized, guard),
        InvalidPolicyRuntime::Hold => make_upon_stream(normalized, guard),
        InvalidPolicyRuntime::Replace(replacement) => {
            let guard_stream = guard;
            let normalized_stream = normalized;
            Stream::new(move |tick| {
                if guard_stream.value_at(tick).as_bool().unwrap_or(false) {
                    normalized_stream.value_at(tick)
                } else {
                    replacement.value_at(tick)
                }
            })
        }
    }
}

fn make_error_stream(guard: Stream, error_sources: Vec<Stream>, default_code: String) -> Stream {
    Stream::new(move |tick| {
        let is_valid = guard.value_at(tick).as_bool().unwrap_or(false);
        if is_valid {
            ScalarValue::List(Vec::new())
        } else {
            let mut errors = Vec::new();
            if error_sources.is_empty() {
                errors.push(default_error_record(tick, default_code.as_str()));
            } else {
                for source in &error_sources {
                    let raw = source.value_at(tick);
                    errors.push(coerce_error_value(raw, tick, default_code.as_str()));
                }
            }
            ScalarValue::List(errors)
        }
    })
}

fn coerce_error_value(value: ScalarValue, tick: usize, default_code: &str) -> ScalarValue {
    match value {
        ScalarValue::Record(mut record) => {
            record
                .entry("t".into())
                .or_insert_with(|| ScalarValue::Int(tick as i64));
            record
                .entry("code".into())
                .or_insert_with(|| ScalarValue::String(default_code.to_string()));
            ScalarValue::Record(record)
        }
        other => {
            let mut record = BTreeMap::new();
            record.insert("t".into(), ScalarValue::Int(tick as i64));
            record.insert("code".into(), ScalarValue::String(default_code.to_string()));
            record.insert("detail".into(), other);
            ScalarValue::Record(record)
        }
    }
}

fn default_error_record(tick: usize, default_code: &str) -> ScalarValue {
    let mut record = BTreeMap::new();
    record.insert("t".into(), ScalarValue::Int(tick as i64));
    record.insert("code".into(), ScalarValue::String(default_code.to_string()));
    record.insert(
        "detail".into(),
        ScalarValue::String("value guard failed".to_string()),
    );
    ScalarValue::Record(record)
}

fn mask_stream_with_guard(value: Stream, guard: Stream) -> Stream {
    Stream::new(move |tick| {
        if guard.value_at(tick).as_bool().unwrap_or(false) {
            value.value_at(tick)
        } else {
            ScalarValue::Undefined
        }
    })
}

#[derive(Clone)]
enum InvalidPolicyRuntime {
    Drop,
    Hold,
    Replace(Stream),
}

#[derive(Clone)]
struct ValueTypeEvaluation {
    normalized: Stream,
    guard: Stream,
    errors: Stream,
    invalid_policy: InvalidPolicyRuntime,
}

fn compare<F>(left: &ScalarValue, right: &ScalarValue, cmp: F) -> bool
where
    F: Fn(f64, f64) -> bool,
{
    match (left.as_float(), right.as_float()) {
        (Some(x), Some(y)) => cmp(x, y),
        _ => false,
    }
}

fn make_upon_stream(value_stream: Stream, guard_stream: Stream) -> Stream {
    use std::cell::RefCell;

    let state = RefCell::new(Vec::<ScalarValue>::new());
    Stream::new(move |tick| {
        {
            let mut cached = state.borrow_mut();
            while cached.len() <= tick {
                let idx = cached.len();
                let guard = guard_stream.value_at(idx).as_bool().unwrap_or(false);
                let next_value = if guard {
                    value_stream.value_at(idx)
                } else if idx == 0 {
                    ScalarValue::Undefined
                } else {
                    cached
                        .get(idx - 1)
                        .cloned()
                        .unwrap_or(ScalarValue::Undefined)
                };
                cached.push(next_value);
            }
        }
        state
            .borrow()
            .get(tick)
            .cloned()
            .unwrap_or(ScalarValue::Undefined)
    })
}

fn make_hold_stream(source_stream: Stream) -> Stream {
    use std::cell::RefCell;

    let state = RefCell::new(Vec::<ScalarValue>::new());
    Stream::new(move |tick| {
        {
            let mut cached = state.borrow_mut();
            while cached.len() <= tick {
                let idx = cached.len();
                let current = source_stream.value_at(idx);
                if current.is_defined() {
                    cached.push(current);
                } else if idx == 0 {
                    cached.push(ScalarValue::Undefined);
                } else {
                    let previous = cached
                        .get(idx - 1)
                        .cloned()
                        .unwrap_or(ScalarValue::Undefined);
                    cached.push(previous);
                }
            }
        }
        state
            .borrow()
            .get(tick)
            .cloned()
            .unwrap_or(ScalarValue::Undefined)
    })
}

fn make_unique_guard(value_stream: Stream, key_stream: Option<Stream>) -> Stream {
    use std::cell::RefCell;

    match key_stream {
        Some(keys) => {
            let seen = RefCell::new(HashMap::<String, HashSet<String>>::new());
            Stream::new(move |tick| {
                let value = value_stream.value_at(tick);
                if !value.is_defined() {
                    ScalarValue::Bool(true)
                } else {
                    let key_value = keys.value_at(tick);
                    if !key_value.is_defined() {
                        return ScalarValue::Bool(true);
                    }
                    let key = key_value.to_string_lossy();
                    let item = value.to_string_lossy();
                    let mut seen = seen.borrow_mut();
                    let entry = seen.entry(key).or_insert_with(HashSet::new);
                    if entry.insert(item) {
                        ScalarValue::Bool(true)
                    } else {
                        ScalarValue::Bool(false)
                    }
                }
            })
        }
        None => {
            let seen = RefCell::new(HashSet::<String>::new());
            Stream::new(move |tick| {
                let value = value_stream.value_at(tick);
                if !value.is_defined() {
                    ScalarValue::Bool(true)
                } else {
                    let key = value.to_string_lossy();
                    let mut seen = seen.borrow_mut();
                    if seen.insert(key) {
                        ScalarValue::Bool(true)
                    } else {
                        ScalarValue::Bool(false)
                    }
                }
            })
        }
    }
}

fn parse_monotone_direction(value: &ScalarValue) -> Result<MonotoneDirection, Diagnostic> {
    let directive = match value {
        ScalarValue::String(text) => text.trim().to_ascii_lowercase(),
        other => {
            return Err(Diagnostic::new(
                DiagnosticKind::Eval,
                format!(
                    "monotone direction expects string descriptor, found {}",
                    other.to_string_lossy()
                ),
            ));
        }
    };

    match directive.as_str() {
        "\u{2264}" | "<=" | "le" | "ascending" | "increasing" | "nondecreasing" | "asc" => {
            Ok(MonotoneDirection::NonDecreasing)
        }
        "\u{2265}" | ">=" | "ge" | "descending" | "decreasing" | "nonincreasing" | "desc" => {
            Ok(MonotoneDirection::NonIncreasing)
        }
        _ => Err(Diagnostic::new(
            DiagnosticKind::Eval,
            format!(
                "unsupported monotone direction '{}'; expected <= or >= variants",
                directive
            ),
        )),
    }
}

#[derive(Clone, Copy)]
enum MonotoneDirection {
    NonDecreasing,
    NonIncreasing,
}

fn make_monotone_guard(source_stream: Stream, direction: MonotoneDirection) -> Stream {
    use std::cell::RefCell;

    let last = RefCell::new(None::<ScalarValue>);
    Stream::new(move |tick| {
        let value = source_stream.value_at(tick);
        if !value.is_defined() {
            ScalarValue::Bool(true)
        } else {
            let mut last_ref = last.borrow_mut();
            let result = match last_ref.as_ref() {
                None => true,
                Some(prev) => match direction {
                    MonotoneDirection::NonDecreasing => compare(prev, &value, |a, b| a <= b),
                    MonotoneDirection::NonIncreasing => compare(prev, &value, |a, b| a >= b),
                },
            };
            *last_ref = Some(value.clone());
            ScalarValue::Bool(result)
        }
    })
}

fn parse_stabilizes_window(value: &ScalarValue) -> Result<usize, Diagnostic> {
    match value.as_int() {
        Some(n) if n >= 0 => Ok(n as usize),
        Some(_) => Err(Diagnostic::new(
            DiagnosticKind::Eval,
            "stabilizes window must be a non-negative integer",
        )),
        None => Err(Diagnostic::new(
            DiagnosticKind::Eval,
            format!(
                "stabilizes window expects integer length, found {}",
                value.to_string_lossy()
            ),
        )),
    }
}

fn make_stabilizes_guard(source_stream: Stream, window: usize) -> Stream {
    use std::cell::RefCell;

    #[derive(Default)]
    struct State {
        last: Option<ScalarValue>,
        run: usize,
    }

    let state = RefCell::new(State::default());
    Stream::new(move |tick| {
        let value = source_stream.value_at(tick);
        if !value.is_defined() {
            ScalarValue::Bool(true)
        } else {
            let mut state = state.borrow_mut();
            match state.last.as_ref() {
                Some(previous) if *previous == value => {
                    state.run += 1;
                }
                _ => {
                    state.last = Some(value.clone());
                    state.run = 1;
                }
            }
            ScalarValue::Bool(state.run > window)
        }
    })
}

fn parse_rate_limit_interval(value: &ScalarValue) -> Result<usize, Diagnostic> {
    match value.as_int() {
        Some(n) if n >= 0 => Ok(n as usize),
        Some(_) => Err(Diagnostic::new(
            DiagnosticKind::Eval,
            "rateLimit interval must be a non-negative integer",
        )),
        None => Err(Diagnostic::new(
            DiagnosticKind::Eval,
            format!(
                "rateLimit interval expects integer length, found {}",
                value.to_string_lossy()
            ),
        )),
    }
}

fn make_rate_limit_guard(event_stream: Stream, interval: usize) -> Stream {
    use std::cell::RefCell;

    let last_tick = RefCell::new(None::<usize>);
    Stream::new(move |tick| {
        let event_value = event_stream.value_at(tick);
        let is_event = event_value.as_bool().unwrap_or(false);
        if !is_event {
            ScalarValue::Bool(true)
        } else {
            let mut last = last_tick.borrow_mut();
            match *last {
                None => {
                    *last = Some(tick);
                    ScalarValue::Bool(true)
                }
                Some(prev) => {
                    if tick.saturating_sub(prev) >= interval {
                        *last = Some(tick);
                        ScalarValue::Bool(true)
                    } else {
                        ScalarValue::Bool(false)
                    }
                }
            }
        }
    })
}

fn make_guard_and(left: Stream, right: Stream) -> Stream {
    Stream::new(move |tick| {
        let lhs = left.value_at(tick).as_bool().unwrap_or(false);
        let rhs = right.value_at(tick).as_bool().unwrap_or(false);
        ScalarValue::Bool(lhs && rhs)
    })
}

fn make_guard_or(left: Stream, right: Stream) -> Stream {
    Stream::new(move |tick| {
        let lhs = left.value_at(tick).as_bool().unwrap_or(false);
        let rhs = right.value_at(tick).as_bool().unwrap_or(false);
        ScalarValue::Bool(lhs || rhs)
    })
}

fn make_no_gaps_guard(stream: Stream) -> Stream {
    Stream::new(move |tick| ScalarValue::Bool(stream.value_at(tick).is_defined()))
}

fn ensure_no_labels(name: &str, args: &[CallArg]) -> Result<(), Diagnostic> {
    for arg in args {
        if let Some(label) = &arg.label {
            return Err(Diagnostic::new(
                DiagnosticKind::Eval,
                format!(
                    "builtin '{}' does not accept labeled argument '{}'",
                    name, label
                ),
            ));
        }
    }
    Ok(())
}

fn parse_window_size(value: &ScalarValue) -> Result<usize, Diagnostic> {
    match value.as_int() {
        Some(n) if n >= 0 => Ok(n as usize),
        Some(_) => Err(Diagnostic::new(
            DiagnosticKind::Eval,
            "window size must be a non-negative integer",
        )),
        None => Err(Diagnostic::new(
            DiagnosticKind::Eval,
            format!(
                "window size expects integer length, found {}",
                value.to_string_lossy()
            ),
        )),
    }
}

fn make_window_stream(value_stream: Stream, size: usize) -> Stream {
    use std::cell::RefCell;

    let buffer = RefCell::new(VecDeque::<ScalarValue>::new());
    Stream::new(move |tick| {
        let value = value_stream.value_at(tick);
        {
            let mut deque = buffer.borrow_mut();
            if size == 0 {
                deque.clear();
            } else {
                if deque.len() == size {
                    deque.pop_front();
                }
                deque.push_back(value.clone());
            }
        }
        let snapshot = buffer.borrow().iter().cloned().collect::<Vec<_>>();
        ScalarValue::List(snapshot)
    })
}
