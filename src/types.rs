use std::collections::HashMap;

use crate::ast::{
    self, Argument, BinaryOp, Expr, Item, Literal, Module, Pattern, TypeExpr, TypeExprKind, UnaryOp,
};
use crate::diagnostics::{Diagnostic, DiagnosticKind};
use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Any,
    Unit,
    Bool,
    Int,
    Float,
    String,
    Stream(Box<Type>),
    List(Box<Type>),
    Tuple(Vec<Type>),
    Struct(String, Vec<(String, Type)>),
    Function(Vec<Type>, Box<Type>),
    Value(String),
    Unknown,
}

impl Type {
    fn stream(inner: Type) -> Type {
        Type::Stream(Box::new(inner))
    }

    fn describe(&self) -> String {
        match self {
            Type::Any => "any".to_string(),
            Type::Unit => "unit".to_string(),
            Type::Bool => "Bool".to_string(),
            Type::Int => "Int".to_string(),
            Type::Float => "Float".to_string(),
            Type::String => "String".to_string(),
            Type::Stream(inner) => format!("S<{}>", inner.describe()),
            Type::List(inner) => format!("List<{}>", inner.describe()),
            Type::Tuple(items) => {
                let parts: Vec<String> = items.iter().map(|ty| ty.describe()).collect();
                format!("({})", parts.join(", "))
            }
            Type::Struct(name, _) => format!("Struct({})", name),
            Type::Function(params, ret) => {
                let parts: Vec<String> = params.iter().map(|ty| ty.describe()).collect();
                format!("({}) -> {}", parts.join(", "), ret.describe())
            }
            Type::Value(name) => format!("Value({})", name),
            Type::Unknown => "unknown".to_string(),
        }
    }

    fn is_numeric(&self) -> bool {
        matches!(self, Type::Int | Type::Float | Type::Any)
    }

    fn is_bool(&self) -> bool {
        matches!(self, Type::Bool | Type::Any)
    }
}

#[derive(Debug, Default, Clone)]
pub struct TypeEnv {
    values: HashMap<String, Type>,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub fn insert(&mut self, name: impl Into<String>, ty: Type) {
        self.values.insert(name.into(), ty);
    }

    pub fn get(&self, name: &str) -> Option<&Type> {
        self.values.get(name)
    }
}

#[derive(Debug)]
pub struct TypeChecker {
    env: TypeEnv,
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut checker = Self {
            env: TypeEnv::new(),
        };
        checker.install_prelude();
        checker
    }

    pub fn infer_module(&mut self, module: &Module) -> Result<(), Vec<Diagnostic>> {
        let mut diagnostics = Vec::new();

        for item in &module.items {
            if let Err(mut diags) = self.check_item(item) {
                diagnostics.append(&mut diags);
            }
        }

        if diagnostics.is_empty() {
            Ok(())
        } else {
            Err(diagnostics)
        }
    }

    fn check_item(&mut self, item: &Item) -> Result<(), Vec<Diagnostic>> {
        match item {
            Item::Function(func) => {
                let mut param_types = Vec::new();
                let mut diagnostics = Vec::new();
                for pattern in &func.params {
                    match pattern {
                        Pattern::Identifier(_) => {
                            param_types.push(Type::stream(Type::Any));
                        }
                        _ => diagnostics.push(
                            Diagnostic::new(
                                DiagnosticKind::Type,
                                "function parameters must be simple identifiers",
                            )
                            .with_span(pattern.span()),
                        ),
                    }
                }
                let declared_return = func
                    .return_type
                    .as_ref()
                    .and_then(|ty| self.type_from_type_expr(ty))
                    .map(Type::stream)
                    .unwrap_or(Type::stream(Type::Any));
                self.env.insert(
                    func.name.name.clone(),
                    Type::Function(param_types.clone(), Box::new(declared_return.clone())),
                );
                if !diagnostics.is_empty() {
                    return Err(diagnostics);
                }

                let mut scope = self.env.clone();
                for (pattern, ty) in func.params.iter().zip(param_types.iter()) {
                    if let Pattern::Identifier(ident) = pattern {
                        scope.insert(ident.name.clone(), ty.clone());
                    }
                }

                match self.check_expr(&func.body, &mut scope) {
                    Ok(body_ty) => self
                        .unify_streams(
                            body_ty,
                            declared_return,
                            func.span,
                            "function body must match declared return type",
                        )
                        .map(|_| ())
                        .map_err(|diag| vec![diag]),
                    Err(diag) => Err(vec![diag]),
                }
            }
            Item::Value(value) => {
                let mut diagnostics = Vec::new();
                let rep_inner = self
                    .type_from_type_expr(&value.representation)
                    .unwrap_or(Type::Any);
                self.env.insert(
                    value.name.name.clone(),
                    Type::Value(value.name.name.clone()),
                );

                let mut scope = self.env.clone();
                scope.insert("it".to_string(), Type::stream(rep_inner.clone()));

                for normalize in &value.normalize {
                    match self.check_expr(&normalize.expr, &mut scope) {
                        Ok(expr_ty) => {
                            if let Pattern::Identifier(ident) = &normalize.target {
                                scope.insert(ident.name.clone(), expr_ty);
                            } else {
                                diagnostics.push(
                                    Diagnostic::new(
                                        DiagnosticKind::Type,
                                        "only identifier targets supported in normalize rules",
                                    )
                                    .with_span(normalize.span),
                                );
                            }
                        }
                        Err(diag) => diagnostics.push(diag),
                    }
                }

                for require in &value.requires {
                    match self.check_expr(require, &mut scope) {
                        Ok(expr_ty) => {
                            if let Err(diag) = self.ensure_bool_stream(
                                expr_ty,
                                require.span(),
                                "require expression",
                            ) {
                                diagnostics.push(diag);
                            }
                        }
                        Err(diag) => diagnostics.push(diag),
                    }
                }

                for always in &value.always {
                    match self.check_expr(always, &mut scope) {
                        Ok(expr_ty) => {
                            if let Err(diag) =
                                self.ensure_bool_stream(expr_ty, always.span(), "always expression")
                            {
                                diagnostics.push(diag);
                            }
                        }
                        Err(diag) => diagnostics.push(diag),
                    }
                }

                for policy in &value.policies {
                    match self.check_policy(policy, &mut scope, &rep_inner) {
                        Ok(()) => {}
                        Err(diag) => diagnostics.push(diag),
                    }
                }

                if diagnostics.is_empty() {
                    Ok(())
                } else {
                    Err(diagnostics)
                }
            }
            Item::Struct(struct_decl) => {
                self.env.insert(
                    struct_decl.name.name.clone(),
                    Type::Struct(struct_decl.name.name.clone(), Vec::new()),
                );
                Ok(())
            }
            Item::Source(source) => {
                let declared_ty = self
                    .type_from_type_expr(&source.ty)
                    .unwrap_or(Type::stream(Type::Any));
                self.env
                    .insert(source.name.name.clone(), declared_ty.clone());
                let mut scope = self.env.clone();
                match self.check_expr(&source.provider, &mut scope) {
                    Ok(provider_ty) => self
                        .unify_streams(
                            provider_ty,
                            declared_ty,
                            source.span,
                            "source provider must match declared stream type",
                        )
                        .map(|_| ())
                        .map_err(|diag| vec![diag]),
                    Err(diag) => Err(vec![diag]),
                }
            }
            Item::Sink(sink) => {
                let mut scope = self.env.clone();
                match self.check_expr(&sink.target, &mut scope) {
                    Ok(target_ty) => self
                        .expect_stream(target_ty, sink.span, "sink target")
                        .map(|_| ())
                        .map_err(|diag| vec![diag]),
                    Err(diag) => Err(vec![diag]),
                }
            }
            Item::Let(let_decl) => {
                let mut scope = self.env.clone();
                let expr_ty = match self.check_expr(&let_decl.expr, &mut scope) {
                    Ok(ty) => ty,
                    Err(diag) => return Err(vec![diag]),
                };
                match &let_decl.pattern {
                    Pattern::Identifier(ident) => {
                        self.env.insert(ident.name.clone(), expr_ty);
                        Ok(())
                    }
                    _ => Err(vec![
                        Diagnostic::new(
                            DiagnosticKind::Type,
                            "only identifier patterns supported in top-level let bindings",
                        )
                        .with_span(let_decl.pattern.span()),
                    ]),
                }
            }
        }
    }

    fn check_expr(&self, expr: &Expr, env: &mut TypeEnv) -> Result<Type, Diagnostic> {
        match expr {
            Expr::Literal(lit, _) => Ok(Type::stream(self.literal_inner_type(lit))),
            Expr::Identifier(ident) => env
                .get(&ident.name)
                .cloned()
                .or_else(|| self.env.get(&ident.name).cloned())
                .ok_or_else(|| {
                    Diagnostic::new(
                        DiagnosticKind::Type,
                        format!("unknown identifier '{}'", ident.name),
                    )
                    .with_span(ident.span)
                }),
            Expr::Unary { op, expr, span } => {
                let expr_ty = self.check_expr(expr, env)?;
                let elem = self.expect_stream(expr_ty, *span, "unary operand")?;
                match op {
                    UnaryOp::Neg => {
                        if !elem.is_numeric() {
                            return Err(type_error(
                                *span,
                                "negation expects numeric stream",
                                elem.describe(),
                            ));
                        }
                        Ok(Type::stream(elem))
                    }
                    UnaryOp::Not => {
                        if !elem.is_bool() {
                            return Err(type_error(
                                *span,
                                "logical not expects Bool stream",
                                elem.describe(),
                            ));
                        }
                        Ok(Type::stream(Type::Bool))
                    }
                }
            }
            Expr::Binary {
                op,
                left,
                right,
                span,
            } => self.infer_binary(*op, left, right, *span, env),
            Expr::Call {
                callee,
                arguments,
                span,
            } => self.infer_call(callee, arguments, *span, env),
            Expr::If {
                condition,
                then_branch,
                else_branch,
                span,
            } => {
                let cond_ty = self.check_expr(condition, env)?;
                let cond_elem = self.expect_stream(cond_ty, *span, "if condition")?;
                if !cond_elem.is_bool() {
                    return Err(type_error(
                        *span,
                        "if condition expects Bool stream",
                        cond_elem.describe(),
                    ));
                }
                let then_ty = self.check_expr(then_branch, env)?;
                let else_ty = if let Some(else_branch) = else_branch {
                    self.check_expr(else_branch, env)?
                } else {
                    Type::stream(Type::Any)
                };
                self.unify_streams(then_ty, else_ty, *span, "if branches")
            }
            Expr::Let {
                bindings,
                body,
                span: _,
            } => {
                let mut scope = env.clone();
                for binding in bindings {
                    let value_ty = self.check_expr(&binding.expr, &mut scope)?;
                    if let Pattern::Identifier(ident) = &binding.pattern {
                        scope.insert(ident.name.clone(), value_ty);
                    } else {
                        return Err(Diagnostic::new(
                            DiagnosticKind::Type,
                            "only identifier patterns supported in let bindings",
                        )
                        .with_span(binding.pattern.span()));
                    }
                }
                self.check_expr(body, &mut scope)
            }
            Expr::Block {
                statements, tail, ..
            } => {
                let mut scope = env.clone();
                let mut last_type = Type::stream(Type::Any);
                for stmt in statements {
                    match stmt {
                        ast::Stmt::Let(binding) => {
                            let value_ty = self.check_expr(&binding.expr, &mut scope)?;
                            if let Pattern::Identifier(ident) = &binding.pattern {
                                scope.insert(ident.name.clone(), value_ty);
                            } else {
                                return Err(Diagnostic::new(
                                    DiagnosticKind::Type,
                                    "only identifier patterns supported in let bindings",
                                )
                                .with_span(binding.pattern.span()));
                            }
                        }
                        ast::Stmt::Expr(stmt_expr) => {
                            last_type = self.check_expr(stmt_expr, &mut scope)?;
                        }
                    }
                }
                if let Some(tail_expr) = tail {
                    self.check_expr(tail_expr, &mut scope)
                } else {
                    Ok(last_type)
                }
            }
            Expr::Tuple(elements, _) => {
                let mut elem_types = Vec::new();
                for element in elements {
                    let ty = self.check_expr(element, env)?;
                    let inner = self.expect_stream(ty, element.span(), "tuple element")?;
                    elem_types.push(inner);
                }
                Ok(Type::stream(Type::Tuple(elem_types)))
            }
            Expr::List(elements, _) => {
                let mut element_type = Type::Any;
                for element in elements {
                    let ty = self.check_expr(element, env)?;
                    let inner = self.expect_stream(ty, element.span(), "list element")?;
                    element_type = match self.unify_inner_types(element_type, inner, element.span())
                    {
                        Ok(ty) => ty,
                        Err(diag) => return Err(diag),
                    };
                }
                Ok(Type::stream(Type::List(Box::new(element_type))))
            }
            Expr::Record(fields, _) => {
                let mut field_types = Vec::new();
                for (ident, expr) in fields {
                    let ty = self.check_expr(expr, env)?;
                    let inner = self.expect_stream(ty, expr.span(), "record field")?;
                    field_types.push((ident.name.clone(), inner));
                }
                Ok(Type::stream(Type::Struct(
                    "<anon>".to_string(),
                    field_types,
                )))
            }
            Expr::Access {
                target,
                field,
                span,
            } => {
                let target_ty = self.check_expr(target, env)?;
                if let Type::Value(_) = target_ty {
                    let function_ty = match field.name.as_str() {
                        "apply" => Type::Function(
                            vec![Type::stream(Type::Any)],
                            Box::new(Type::stream(Type::Any)),
                        ),
                        "guard" => Type::Function(
                            vec![Type::stream(Type::Any)],
                            Box::new(Type::stream(Type::Bool)),
                        ),
                        "use" | "use_fn" => Type::Function(
                            vec![Type::stream(Type::Any)],
                            Box::new(Type::stream(Type::Any)),
                        ),
                        "errors" => Type::Function(
                            vec![Type::stream(Type::Any)],
                            Box::new(Type::stream(Type::List(Box::new(Type::Any)))),
                        ),
                        _ => {
                            return Err(Diagnostic::new(
                                DiagnosticKind::Type,
                                format!("value type has no member '{}'", field.name),
                            )
                            .with_span(field.span));
                        }
                    };
                    return Ok(function_ty);
                }
                let inner = self.expect_stream(target_ty, *span, "record access")?;
                match inner {
                    Type::Struct(_, ref fields) => {
                        if let Some((_, field_ty)) =
                            fields.iter().find(|(name, _)| name == &field.name)
                        {
                            Ok(Type::stream(field_ty.clone()))
                        } else {
                            Err(Diagnostic::new(
                                DiagnosticKind::Type,
                                format!("record has no field '{}'", field.name),
                            )
                            .with_span(field.span))
                        }
                    }
                    Type::Any => Ok(Type::stream(Type::Any)),
                    Type::Value(_) => Ok(Type::stream(Type::Any)),
                    other => Err(type_error(
                        *span,
                        "field access expects stream of records/value types",
                        other.describe(),
                    )),
                }
            }
            Expr::Index {
                target,
                index,
                span,
            } => {
                let target_ty = self.check_expr(target, env)?;
                let list_elem = self.expect_stream(target_ty, *span, "index target")?;
                let index_ty = self.check_expr(index, env)?;
                let index_elem = self.expect_stream(index_ty, *span, "index expression")?;
                if !matches!(index_elem, Type::Int | Type::Any) {
                    return Err(type_error(
                        *span,
                        "index expects Int stream",
                        index_elem.describe(),
                    ));
                }
                match list_elem {
                    Type::List(inner) => Ok(Type::stream(*inner.clone())),
                    Type::Tuple(items) => {
                        Ok(Type::stream(items.first().cloned().unwrap_or(Type::Any)))
                    }
                    Type::Any => Ok(Type::stream(Type::Any)),
                    other => Err(type_error(
                        *span,
                        "index expects stream of lists",
                        other.describe(),
                    )),
                }
            }
            Expr::Lambda { .. } => Ok(Type::Function(
                Vec::new(),
                Box::new(Type::stream(Type::Any)),
            )),
        }
    }

    fn infer_binary(
        &self,
        op: BinaryOp,
        left: &Expr,
        right: &Expr,
        span: Span,
        env: &mut TypeEnv,
    ) -> Result<Type, Diagnostic> {
        let left_ty = self.check_expr(left, env)?;
        let right_ty = self.check_expr(right, env)?;
        let left_elem = self.expect_stream(left_ty, left.span(), "binary left")?;
        let right_elem = self.expect_stream(right_ty, right.span(), "binary right")?;
        match op {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                let result_inner =
                    self.unify_numeric(left_elem.clone(), right_elem.clone(), span, op_symbol(op))?;
                Ok(Type::stream(result_inner))
            }
            BinaryOp::Eq | BinaryOp::Neq => {
                if !self.types_compatible(left_elem.clone(), right_elem.clone()) {
                    return Err(type_error(
                        span,
                        "equality operands must share a type",
                        format!("{} vs {}", left_elem.describe(), right_elem.describe()),
                    ));
                }
                Ok(Type::stream(Type::Bool))
            }
            BinaryOp::Lt | BinaryOp::Lte | BinaryOp::Gt | BinaryOp::Gte => {
                self.unify_numeric(left_elem.clone(), right_elem.clone(), span, op_symbol(op))?;
                Ok(Type::stream(Type::Bool))
            }
            BinaryOp::And | BinaryOp::Or => {
                if !left_elem.is_bool() || !right_elem.is_bool() {
                    return Err(type_error(
                        span,
                        "logical operations expect Bool streams",
                        format!("{} vs {}", left_elem.describe(), right_elem.describe()),
                    ));
                }
                Ok(Type::stream(Type::Bool))
            }
            BinaryOp::Fby => {
                match self.unify_inner_types(left_elem.clone(), right_elem.clone(), span) {
                    Ok(inner) => Ok(Type::stream(inner)),
                    Err(diag) => Err(diag),
                }
            }
        }
    }

    fn infer_call(
        &self,
        callee: &Expr,
        arguments: &[Argument],
        span: Span,
        env: &mut TypeEnv,
    ) -> Result<Type, Diagnostic> {
        if let Expr::Identifier(ident) = callee {
            match ident.name.as_str() {
                "when" => return self.check_when(arguments, env, span),
                "upon" => return self.check_upon(arguments, env, span),
                "hold" => return self.check_hold(arguments, env, span),
                "defined" => return self.check_defined(arguments, env, span),
                _ => {}
            }
            if self.is_builtin(&ident.name) {
                for argument in arguments {
                    self.check_expr(&argument.expr, env)?;
                }
                return Ok(Type::stream(Type::Any));
            }
        }

        let callee_ty = self.check_expr(callee, env)?;
        if let Type::Function(params, ret_ty) = callee_ty {
            if params.len() != arguments.len() {
                return Err(Diagnostic::new(
                    DiagnosticKind::Type,
                    format!(
                        "function expects {} arguments, found {}",
                        params.len(),
                        arguments.len()
                    ),
                )
                .with_span(span));
            }
            for (argument, param_ty) in arguments.iter().zip(params.iter()) {
                let arg_ty = self.check_expr(&argument.expr, env)?;
                self.unify_streams(arg_ty, param_ty.clone(), argument.span, "function argument")?;
            }
            Ok(*ret_ty)
        } else {
            for argument in arguments {
                self.check_expr(&argument.expr, env)?;
            }
            Ok(Type::stream(Type::Any))
        }
    }

    fn check_when(
        &self,
        arguments: &[Argument],
        env: &mut TypeEnv,
        span: Span,
    ) -> Result<Type, Diagnostic> {
        if arguments.len() != 2 {
            return Err(Diagnostic::new(
                DiagnosticKind::Type,
                "when expects exactly two arguments",
            )
            .with_span(span));
        }
        let value_ty = self.check_expr(&arguments[0].expr, env)?;
        let guard_ty = self.check_expr(&arguments[1].expr, env)?;
        let value_inner = self.expect_stream(value_ty, arguments[0].span, "when value")?;
        let guard_inner = self.expect_stream(guard_ty, arguments[1].span, "when guard")?;
        if !guard_inner.is_bool() {
            return Err(type_error(
                arguments[1].span,
                "when guard expects Bool stream",
                guard_inner.describe(),
            ));
        }
        Ok(Type::stream(value_inner))
    }

    fn check_upon(
        &self,
        arguments: &[Argument],
        env: &mut TypeEnv,
        span: Span,
    ) -> Result<Type, Diagnostic> {
        if arguments.len() != 2 {
            return Err(Diagnostic::new(
                DiagnosticKind::Type,
                "upon expects exactly two arguments",
            )
            .with_span(span));
        }
        let value_ty = self.check_expr(&arguments[0].expr, env)?;
        let guard_ty = self.check_expr(&arguments[1].expr, env)?;
        let value_inner = self.expect_stream(value_ty, arguments[0].span, "upon value")?;
        let guard_inner = self.expect_stream(guard_ty, arguments[1].span, "upon guard")?;
        if !guard_inner.is_bool() {
            return Err(type_error(
                arguments[1].span,
                "upon guard expects Bool stream",
                guard_inner.describe(),
            ));
        }
        Ok(Type::stream(value_inner))
    }

    fn check_hold(
        &self,
        arguments: &[Argument],
        env: &mut TypeEnv,
        span: Span,
    ) -> Result<Type, Diagnostic> {
        if arguments.len() != 1 {
            return Err(
                Diagnostic::new(DiagnosticKind::Type, "hold expects exactly one argument")
                    .with_span(span),
            );
        }
        let value_ty = self.check_expr(&arguments[0].expr, env)?;
        let value_inner = self.expect_stream(value_ty, arguments[0].span, "hold operand")?;
        Ok(Type::stream(value_inner))
    }

    fn check_defined(
        &self,
        arguments: &[Argument],
        env: &mut TypeEnv,
        span: Span,
    ) -> Result<Type, Diagnostic> {
        if arguments.len() != 1 {
            return Err(Diagnostic::new(
                DiagnosticKind::Type,
                "defined expects exactly one argument",
            )
            .with_span(span));
        }
        let value_ty = self.check_expr(&arguments[0].expr, env)?;
        self.expect_stream(value_ty, arguments[0].span, "defined operand")?;
        Ok(Type::stream(Type::Bool))
    }

    fn expect_stream(&self, ty: Type, span: Span, context: &str) -> Result<Type, Diagnostic> {
        match ty {
            Type::Stream(inner) => Ok(*inner),
            Type::Any => Ok(Type::Any),
            other => Err(type_error(
                span,
                &format!("{} expects a stream value", context),
                other.describe(),
            )),
        }
    }

    fn unify_numeric(
        &self,
        left: Type,
        right: Type,
        span: Span,
        context: &str,
    ) -> Result<Type, Diagnostic> {
        match (&left, &right) {
            (Type::Int, Type::Int) => Ok(Type::Int),
            (Type::Float, Type::Float) => Ok(Type::Float),
            (Type::Int, Type::Float) | (Type::Float, Type::Int) => Ok(Type::Float),
            (Type::Any, other) | (other, Type::Any) => {
                if other.is_numeric() {
                    Ok(other.clone())
                } else {
                    Err(type_error(
                        span,
                        &format!("{} expects numeric streams", context),
                        other.describe(),
                    ))
                }
            }
            _ => Err(type_error(
                span,
                &format!("{} expects numeric streams", context),
                format!("{} vs {}", left.describe(), right.describe()),
            )),
        }
    }

    fn types_compatible(&self, left: Type, right: Type) -> bool {
        left == right || matches!(left, Type::Any) || matches!(right, Type::Any)
    }

    fn unify_streams(
        &self,
        left: Type,
        right: Type,
        span: Span,
        context: &str,
    ) -> Result<Type, Diagnostic> {
        match (left, right) {
            (Type::Stream(inner_left), Type::Stream(inner_right)) => {
                match self.unify_inner_types(*inner_left, *inner_right, span) {
                    Ok(inner) => Ok(Type::stream(inner)),
                    Err(diag) => Err(diag),
                }
            }
            (Type::Any, other) | (other, Type::Any) => Ok(other),
            (left, right) => Err(type_error(
                span,
                &format!("{} expect compatible stream types", context),
                format!("{} vs {}", left.describe(), right.describe()),
            )),
        }
    }

    fn ensure_bool_stream(&self, ty: Type, span: Span, context: &str) -> Result<(), Diagnostic> {
        match self.expect_stream(ty, span, context) {
            Ok(inner) => {
                if inner.is_bool() {
                    Ok(())
                } else {
                    Err(type_error(
                        span,
                        &format!("{} must yield Bool stream", context),
                        inner.describe(),
                    ))
                }
            }
            Err(diag) => Err(diag),
        }
    }

    fn unify_inner_types(&self, left: Type, right: Type, span: Span) -> Result<Type, Diagnostic> {
        if left == right {
            Ok(left)
        } else if matches!(left, Type::Any) {
            Ok(right)
        } else if matches!(right, Type::Any) {
            Ok(left)
        } else {
            Err(type_error(
                span,
                "operands must share the same element type",
                format!("{} vs {}", left.describe(), right.describe()),
            ))
        }
    }

    fn literal_inner_type(&self, literal: &Literal) -> Type {
        match literal {
            Literal::Int(_) => Type::Int,
            Literal::Float(_) => Type::Float,
            Literal::Bool(_) => Type::Bool,
            Literal::String(_) => Type::String,
            Literal::Null => Type::Any,
        }
    }

    fn type_from_type_expr(&self, expr: &TypeExpr) -> Option<Type> {
        match &expr.kind {
            TypeExprKind::Named(name) => match name.as_str() {
                "Int" => Some(Type::Int),
                "Float" => Some(Type::Float),
                "Bool" => Some(Type::Bool),
                "String" => Some(Type::String),
                _ => Some(Type::Value(name.clone())),
            },
            TypeExprKind::Applied { base, .. } => Some(Type::Value(base.clone())),
            TypeExprKind::Stream(inner) => self.type_from_type_expr(inner).map(Type::stream),
            TypeExprKind::Tuple(items) => {
                let mut elems = Vec::new();
                for item in items {
                    if let Some(ty) = self.type_from_type_expr(item) {
                        elems.push(ty);
                    }
                }
                Some(Type::Tuple(elems))
            }
        }
    }

    fn install_prelude(&mut self) {
        let stream_any = Type::stream(Type::Any);
        let bool_stream = Type::stream(Type::Bool);
        let entries = [
            (
                "next",
                Type::Function(vec![stream_any.clone()], Box::new(stream_any.clone())),
            ),
            (
                "first",
                Type::Function(vec![stream_any.clone()], Box::new(stream_any.clone())),
            ),
            (
                "rest",
                Type::Function(vec![stream_any.clone()], Box::new(stream_any.clone())),
            ),
            (
                "defined",
                Type::Function(vec![stream_any.clone()], Box::new(bool_stream.clone())),
            ),
            (
                "when",
                Type::Function(
                    vec![stream_any.clone(), bool_stream.clone()],
                    Box::new(stream_any.clone()),
                ),
            ),
            (
                "upon",
                Type::Function(
                    vec![stream_any.clone(), bool_stream.clone()],
                    Box::new(stream_any.clone()),
                ),
            ),
            (
                "hold",
                Type::Function(vec![stream_any.clone()], Box::new(stream_any.clone())),
            ),
            (
                "unique",
                Type::Function(vec![stream_any.clone()], Box::new(bool_stream.clone())),
            ),
            (
                "monotone",
                Type::Function(vec![stream_any.clone()], Box::new(bool_stream.clone())),
            ),
            (
                "stabilizes",
                Type::Function(vec![stream_any.clone()], Box::new(bool_stream.clone())),
            ),
            (
                "rateLimit",
                Type::Function(vec![stream_any.clone()], Box::new(bool_stream.clone())),
            ),
            (
                "window",
                Type::Function(vec![stream_any.clone()], Box::new(stream_any.clone())),
            ),
            (
                "sum",
                Type::Function(vec![stream_any.clone()], Box::new(stream_any.clone())),
            ),
            (
                "avg",
                Type::Function(vec![stream_any.clone()], Box::new(stream_any.clone())),
            ),
            (
                "countDistinct",
                Type::Function(vec![stream_any.clone()], Box::new(stream_any.clone())),
            ),
            (
                "foldWindow",
                Type::Function(vec![stream_any.clone()], Box::new(stream_any.clone())),
            ),
            (
                "tickEvery",
                Type::Function(vec![stream_any.clone()], Box::new(bool_stream.clone())),
            ),
            (
                "guard_and",
                Type::Function(
                    vec![bool_stream.clone(), bool_stream.clone()],
                    Box::new(bool_stream.clone()),
                ),
            ),
            (
                "guard_or",
                Type::Function(
                    vec![bool_stream.clone(), bool_stream.clone()],
                    Box::new(bool_stream.clone()),
                ),
            ),
            (
                "noGaps",
                Type::Function(vec![stream_any.clone()], Box::new(bool_stream.clone())),
            ),
        ];

        for (name, ty) in entries {
            self.env.insert(name.to_string(), ty);
        }
    }

    fn is_builtin(&self, name: &str) -> bool {
        matches!(
            name,
            "unique"
                | "monotone"
                | "stabilizes"
                | "rateLimit"
                | "window"
                | "sum"
                | "avg"
                | "countDistinct"
                | "foldWindow"
                | "tickEvery"
                | "guard_and"
                | "guard_or"
                | "noGaps"
        )
    }

    fn check_policy(
        &self,
        policy: &ast::PolicyRule,
        env: &mut TypeEnv,
        rep_inner: &Type,
    ) -> Result<(), Diagnostic> {
        match policy.kind {
            ast::PolicyKind::Invalid => match &policy.expr {
                ast::PolicyExpr::Builtin(_) => Ok(()),
                ast::PolicyExpr::Replace(expr) => {
                    let ty = self.check_expr(expr.as_ref(), env)?;
                    self.unify_streams(
                        ty,
                        Type::stream(rep_inner.clone()),
                        policy.span,
                        "invalid policy expression",
                    )
                    .map(|_| ())
                }
                ast::PolicyExpr::Expr(expr) => {
                    let ty = self.check_expr(expr, env)?;
                    self.unify_streams(
                        ty,
                        Type::stream(rep_inner.clone()),
                        policy.span,
                        "invalid policy expression",
                    )
                    .map(|_| ())
                }
            },
            ast::PolicyKind::Error => match &policy.expr {
                ast::PolicyExpr::Builtin(_) => Ok(()),
                ast::PolicyExpr::Replace(expr) => {
                    let ty = self.check_expr(expr.as_ref(), env)?;
                    self.expect_stream(ty, policy.span, "error policy expression")
                        .map(|_| ())
                }
                ast::PolicyExpr::Expr(expr) => {
                    let ty = self.check_expr(expr, env)?;
                    self.expect_stream(ty, policy.span, "error policy expression")
                        .map(|_| ())
                }
            },
            ast::PolicyKind::Custom(_) => match &policy.expr {
                ast::PolicyExpr::Builtin(_) => Ok(()),
                ast::PolicyExpr::Replace(expr) => {
                    let ty = self.check_expr(expr.as_ref(), env)?;
                    self.ensure_bool_stream(ty, policy.span, "policy expression")
                }
                ast::PolicyExpr::Expr(expr) => {
                    let ty = self.check_expr(expr, env)?;
                    self.ensure_bool_stream(ty, policy.span, "policy expression")
                }
            },
        }
    }
}

fn op_symbol(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::Sub => "-",
        BinaryOp::Mul => "*",
        BinaryOp::Div => "/",
        BinaryOp::Mod => "%",
        BinaryOp::Eq => "==",
        BinaryOp::Neq => "!=",
        BinaryOp::Lt => "<",
        BinaryOp::Lte => "<=",
        BinaryOp::Gt => ">",
        BinaryOp::Gte => ">=",
        BinaryOp::And => "&&",
        BinaryOp::Or => "||",
        BinaryOp::Fby => "fby",
    }
}

fn type_error(span: Span, message: &str, found: String) -> Diagnostic {
    Diagnostic::new(
        DiagnosticKind::Type,
        format!("{}; found {}", message, found),
    )
    .with_span(span)
}
