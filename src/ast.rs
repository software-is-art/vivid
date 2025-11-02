use std::fmt;

use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

impl Ident {
    pub fn new(name: impl Into<String>, span: Span) -> Self {
        Self {
            name: name.into(),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Value(ValueDecl),
    Function(FunctionDecl),
    Struct(StructDecl),
    Source(SourceDecl),
    Sink(SinkDecl),
    Let(LetDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueDecl {
    pub name: Ident,
    pub generics: Vec<Ident>,
    pub representation: TypeExpr,
    pub normalize: Vec<NormalizeRule>,
    pub requires: Vec<Expr>,
    pub always: Vec<Expr>,
    pub policies: Vec<PolicyRule>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NormalizeRule {
    pub target: Pattern,
    pub expr: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PolicyRule {
    pub kind: PolicyKind,
    pub expr: PolicyExpr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PolicyKind {
    Invalid,
    Error,
    Custom(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PolicyExpr {
    Builtin(PolicyBuiltin),
    Replace(Box<Expr>),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PolicyBuiltin {
    Drop,
    Hold,
    Replace,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDecl {
    pub name: Ident,
    pub params: Vec<Pattern>,
    pub return_type: Option<TypeExpr>,
    pub body: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDecl {
    pub name: Ident,
    pub fields: Vec<StructField>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: Ident,
    pub ty: TypeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SourceDecl {
    pub name: Ident,
    pub ty: TypeExpr,
    pub provider: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SinkDecl {
    pub name: Ident,
    pub ty: TypeExpr,
    pub target: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetDecl {
    pub pattern: Pattern,
    pub expr: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Identifier(Ident),
    Tuple(Vec<Pattern>, Span),
    Wildcard(Span),
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Identifier(id) => id.span,
            Pattern::Tuple(_, span) => *span,
            Pattern::Wildcard(span) => *span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal, Span),
    Identifier(Ident),
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
        span: Span,
    },
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    Call {
        callee: Box<Expr>,
        arguments: Vec<Argument>,
        span: Span,
    },
    Lambda {
        params: Vec<Pattern>,
        body: Box<Expr>,
        span: Span,
    },
    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>,
        span: Span,
    },
    Let {
        bindings: Vec<LetBinding>,
        body: Box<Expr>,
        span: Span,
    },
    Block {
        statements: Vec<Stmt>,
        tail: Option<Box<Expr>>,
        span: Span,
    },
    Tuple(Vec<Expr>, Span),
    List(Vec<Expr>, Span),
    Record(Vec<(Ident, Expr)>, Span),
    Access {
        target: Box<Expr>,
        field: Ident,
        span: Span,
    },
    Index {
        target: Box<Expr>,
        index: Box<Expr>,
        span: Span,
    },
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Literal(_, span) => *span,
            Expr::Identifier(id) => id.span,
            Expr::Unary { span, .. }
            | Expr::Binary { span, .. }
            | Expr::Call { span, .. }
            | Expr::Lambda { span, .. }
            | Expr::If { span, .. }
            | Expr::Let { span, .. }
            | Expr::Block { span, .. }
            | Expr::Tuple(_, span)
            | Expr::List(_, span)
            | Expr::Record(_, span)
            | Expr::Access { span, .. }
            | Expr::Index { span, .. } => *span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetBinding {
    pub pattern: Pattern,
    pub expr: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let(LetBinding),
    Expr(Expr),
}

impl Stmt {
    pub fn span(&self) -> Span {
        match self {
            Stmt::Let(binding) => binding.span,
            Stmt::Expr(expr) => expr.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
    pub label: Option<Ident>,
    pub expr: Expr,
    pub span: Span,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Int(value) => write!(f, "{}", value),
            Literal::Float(value) => write!(f, "{}", value),
            Literal::Bool(value) => write!(f, "{}", value),
            Literal::String(value) => write!(f, "\"{}\"", value.escape_debug()),
            Literal::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Or,
    Fby,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeExpr {
    pub kind: TypeExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeExprKind {
    Named(String),
    Applied { base: String, args: Vec<TypeExpr> },
    Stream(Box<TypeExpr>),
    Tuple(Vec<TypeExpr>),
}
