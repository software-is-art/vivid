use std::collections::HashMap;

use crate::ast::{Expr, Module};
use crate::diagnostics::{Diagnostic, DiagnosticKind};

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

#[derive(Debug, Default)]
pub struct TypeChecker {
    env: TypeEnv,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            env: TypeEnv::new(),
        }
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

    fn check_item(&mut self, item: &crate::ast::Item) -> Result<(), Vec<Diagnostic>> {
        match item {
            crate::ast::Item::Function(func) => {
                let name = func.name.name.clone();
                self.env
                    .insert(name, Type::Function(Vec::new(), Box::new(Type::Any)));
                Ok(())
            }
            crate::ast::Item::Value(value) => {
                self.env.insert(
                    value.name.name.clone(),
                    Type::Value(value.name.name.clone()),
                );
                Ok(())
            }
            crate::ast::Item::Struct(struct_decl) => {
                self.env.insert(
                    struct_decl.name.name.clone(),
                    Type::Struct(struct_decl.name.name.clone(), Vec::new()),
                );
                Ok(())
            }
            crate::ast::Item::Source(source) => {
                self.env
                    .insert(source.name.name.clone(), Type::Stream(Box::new(Type::Any)));
                Ok(())
            }
            crate::ast::Item::Sink(sink) => {
                self.env
                    .insert(sink.name.name.clone(), Type::Stream(Box::new(Type::Any)));
                Ok(())
            }
            crate::ast::Item::Let(let_decl) => {
                self.env
                    .insert(self.pattern_name(&let_decl.pattern), Type::Unknown);
                Ok(())
            }
        }
    }

    fn pattern_name(&self, pattern: &crate::ast::Pattern) -> String {
        match pattern {
            crate::ast::Pattern::Identifier(ident) => ident.name.clone(),
            crate::ast::Pattern::Tuple(_, _) => "<tuple>".to_string(),
            crate::ast::Pattern::Wildcard(_) => "_".to_string(),
        }
    }

    #[allow(dead_code)]
    fn infer_expr(&mut self, _expr: &Expr) -> Result<Type, Diagnostic> {
        Err(Diagnostic::new(
            DiagnosticKind::Type,
            "expression type inference not yet implemented",
        )
        .with_note("this will be implemented in a later milestone"))
    }
}
