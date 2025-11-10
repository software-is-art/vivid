use crate::runtime::value::ScalarValue;

#[derive(Debug, Clone)]
pub struct CoreModule {
    pub bindings: Vec<CoreBinding>,
    pub state_slots: Vec<CoreStateSlot>,
    pub unique_specs: Vec<UniqueSpec>,
}

#[derive(Debug, Clone)]
pub struct CoreBinding {
    pub name: String,
    pub expr: CoreExpr,
}

#[derive(Debug, Clone)]
pub struct CoreStateSlot {
    pub name: String,
    pub init: CoreExpr,
    pub next: CoreExpr,
}

#[derive(Debug, Clone)]
pub struct UniqueSpec {
    pub name: String,
    pub keyed: bool,
}

#[derive(Debug, Clone)]
pub enum CoreExpr {
    Literal(ScalarValue),
    Ref(String),
    StateRef(String),
    Unary {
        op: CoreUnaryOp,
        expr: Box<CoreExpr>,
    },
    Binary {
        op: CoreBinaryOp,
        left: Box<CoreExpr>,
        right: Box<CoreExpr>,
    },
    Next(Box<CoreExpr>),
    First(Box<CoreExpr>),
    Fby {
        head: Box<CoreExpr>,
        tail: Box<CoreExpr>,
    },
    When {
        value: Box<CoreExpr>,
        guard: Box<CoreExpr>,
    },
    WindowSum {
        window_len: usize,
        input: Box<CoreExpr>,
    },
    Monotone {
        direction: MonotoneDirection,
        input: Box<CoreExpr>,
    },
    UniqueWithin {
        window_len: usize,
        value: Box<CoreExpr>,
        key: Option<Box<CoreExpr>>,
    },
    UniqueUnbounded {
        store: String,
        value: Box<CoreExpr>,
        key: Option<Box<CoreExpr>>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CoreUnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CoreBinaryOp {
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MonotoneDirection {
    NonDecreasing,
    NonIncreasing,
}
