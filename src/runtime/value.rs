use std::collections::BTreeMap;
use std::rc::Rc;

use crate::ast::{Expr, ValueDecl};
use crate::diagnostics::Diagnostic;

use super::env::Environment;
use super::evaluator::Interpreter;
use super::stream::Stream;

#[derive(Debug, Clone, PartialEq)]
pub enum ScalarValue {
    Undefined,
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    List(Vec<ScalarValue>),
    Record(BTreeMap<String, ScalarValue>),
}

impl ScalarValue {
    pub fn is_defined(&self) -> bool {
        !matches!(self, ScalarValue::Undefined)
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            ScalarValue::Bool(value) => Some(*value),
            _ => None,
        }
    }

    pub fn as_int(&self) -> Option<i64> {
        match self {
            ScalarValue::Int(value) => Some(*value),
            _ => None,
        }
    }

    pub fn as_float(&self) -> Option<f64> {
        match self {
            ScalarValue::Float(value) => Some(*value),
            ScalarValue::Int(value) => Some(*value as f64),
            _ => None,
        }
    }

    pub fn to_string_lossy(&self) -> String {
        match self {
            ScalarValue::Undefined => "⊥".to_string(),
            ScalarValue::Int(v) => v.to_string(),
            ScalarValue::Float(v) => v.to_string(),
            ScalarValue::Bool(v) => v.to_string(),
            ScalarValue::String(v) => v.clone(),
            ScalarValue::List(items) => {
                let parts: Vec<String> = items.iter().map(|item| item.to_string_lossy()).collect();
                format!("[{}]", parts.join(", "))
            }
            ScalarValue::Record(fields) => {
                let parts: Vec<String> = fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v.to_string_lossy()))
                    .collect();
                format!("{{{}}}", parts.join(", "))
            }
        }
    }
}

#[derive(Clone)]
pub struct FunctionValue {
    pub name: String,
    pub params: Vec<String>,
    pub body: Expr,
    pub env: Environment,
}

#[derive(Clone)]
pub struct CallArg {
    pub label: Option<String>,
    pub value: RuntimeValue,
}

#[derive(Clone)]
pub struct BuiltinFunction {
    pub name: &'static str,
    pub handler: Rc<dyn Fn(&Interpreter, &[CallArg]) -> Result<RuntimeValue, Diagnostic>>, // expects already-evaluated args
    pub min_arity: usize,
    pub max_arity: Option<usize>,
}

impl BuiltinFunction {
    pub fn new<F>(
        name: &'static str,
        min_arity: usize,
        max_arity: Option<usize>,
        handler: F,
    ) -> Self
    where
        F: Fn(&Interpreter, &[CallArg]) -> Result<RuntimeValue, Diagnostic> + 'static,
    {
        Self {
            name,
            handler: Rc::new(handler),
            min_arity,
            max_arity,
        }
    }
}

#[derive(Clone)]
pub struct ValueTypeRuntime {
    pub decl: Rc<ValueDecl>,
    pub env: Environment,
}

#[derive(Clone)]
pub enum ValueTypeMethod {
    Apply,
    Guard,
    Use,
    Errors,
}

#[derive(Clone)]
pub struct ValueTypeMethodRuntime {
    pub value_type: Rc<ValueTypeRuntime>,
    pub method: ValueTypeMethod,
}

#[derive(Clone)]
pub enum RuntimeValue {
    Stream(Stream),
    Function(FunctionValue),
    Builtin(BuiltinFunction),
    ValueType(Rc<ValueTypeRuntime>),
    ValueTypeMethod(ValueTypeMethodRuntime),
    Scalar(ScalarValue),
}

impl RuntimeValue {
    pub fn as_stream(&self) -> Option<Stream> {
        match self {
            RuntimeValue::Stream(stream) => Some(stream.clone()),
            RuntimeValue::Scalar(value) => Some(Stream::constant(value.clone())),
            _ => None,
        }
    }

    pub fn into_stream(self) -> Option<Stream> {
        match self {
            RuntimeValue::Stream(stream) => Some(stream),
            RuntimeValue::Scalar(value) => Some(Stream::constant(value)),
            _ => None,
        }
    }

    pub fn as_function(&self) -> Option<FunctionValue> {
        match self {
            RuntimeValue::Function(func) => Some(func.clone()),
            _ => None,
        }
    }

    pub fn as_builtin(&self) -> Option<BuiltinFunction> {
        match self {
            RuntimeValue::Builtin(builtin) => Some(builtin.clone()),
            _ => None,
        }
    }

    pub fn as_value_type(&self) -> Option<Rc<ValueTypeRuntime>> {
        match self {
            RuntimeValue::ValueType(value_type) => Some(value_type.clone()),
            _ => None,
        }
    }

    pub fn as_value_type_method(&self) -> Option<ValueTypeMethodRuntime> {
        match self {
            RuntimeValue::ValueTypeMethod(method) => Some(method.clone()),
            _ => None,
        }
    }
}
