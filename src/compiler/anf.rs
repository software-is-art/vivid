use std::collections::HashMap;

use super::core::{
    CoreBinaryOp, CoreBinding, CoreExpr, CoreModule, CoreUnaryOp, MonotoneDirection,
};
use crate::runtime::value::ScalarValue;

#[derive(Debug, Clone)]
pub struct AnfModule {
    pub bindings: Vec<AnfBinding>,
}

#[derive(Debug, Clone)]
pub struct AnfBinding {
    pub name: String,
    pub block: AnfBlock,
}

#[derive(Debug, Clone)]
pub struct AnfBlock {
    pub lets: Vec<AnfLet>,
    pub result: AnfAtom,
}

#[derive(Debug, Clone)]
pub struct AnfLet {
    pub name: String,
    pub expr: AnfExpr,
}

#[derive(Debug, Clone)]
pub enum AnfExpr {
    Unary {
        op: CoreUnaryOp,
        expr: AnfAtom,
    },
    Binary {
        op: CoreBinaryOp,
        left: AnfAtom,
        right: AnfAtom,
    },
    Next {
        stream: AnfAtom,
    },
    First {
        stream: AnfAtom,
    },
    Fby {
        head: AnfAtom,
        tail: AnfAtom,
    },
    When {
        value: AnfAtom,
        guard: AnfAtom,
    },
    WindowSum {
        window_len: usize,
        input: AnfAtom,
    },
    UniqueWithin {
        window_len: usize,
        value: AnfAtom,
        key: Option<AnfAtom>,
    },
    Monotone {
        direction: MonotoneDirection,
        input: AnfAtom,
    },
}

#[derive(Debug, Clone)]
pub enum AnfAtom {
    Literal(ScalarValue),
    Ref(String),
}

pub fn normalize(surface: &CoreModule) -> AnfModule {
    let mut gensym = Gensym::default();
    let bindings = surface
        .bindings
        .iter()
        .map(|binding| normalize_binding(binding, &mut gensym))
        .collect();
    AnfModule { bindings }
}

fn normalize_binding(binding: &CoreBinding, gensym: &mut Gensym) -> AnfBinding {
    let (result, lets) = normalize_expr(&binding.expr, gensym);
    AnfBinding {
        name: binding.name.clone(),
        block: AnfBlock { lets, result },
    }
}

fn normalize_expr(expr: &CoreExpr, gensym: &mut Gensym) -> (AnfAtom, Vec<AnfLet>) {
    match expr {
        CoreExpr::Literal(value) => (AnfAtom::Literal(value.clone()), Vec::new()),
        CoreExpr::Ref(name) => (AnfAtom::Ref(name.clone()), Vec::new()),
        CoreExpr::StateRef(_) => unreachable!("state refs should not appear before extraction"),
        CoreExpr::Unary { op, expr } => {
            let (atom, mut lets) = normalize_expr(expr, gensym);
            let temp = gensym.fresh();
            lets.push(AnfLet {
                name: temp.clone(),
                expr: AnfExpr::Unary {
                    op: *op,
                    expr: atom,
                },
            });
            (AnfAtom::Ref(temp), lets)
        }
        CoreExpr::Binary { op, left, right } => {
            let (left_atom, mut left_lets) = normalize_expr(left, gensym);
            let (right_atom, mut right_lets) = normalize_expr(right, gensym);
            let mut lets = Vec::new();
            lets.append(&mut left_lets);
            lets.append(&mut right_lets);
            let temp = gensym.fresh();
            lets.push(AnfLet {
                name: temp.clone(),
                expr: AnfExpr::Binary {
                    op: *op,
                    left: left_atom,
                    right: right_atom,
                },
            });
            (AnfAtom::Ref(temp), lets)
        }
        CoreExpr::Next(stream) => normalize_unary_like(stream, gensym, |temp, atom| AnfLet {
            name: temp,
            expr: AnfExpr::Next { stream: atom },
        }),
        CoreExpr::First(stream) => normalize_unary_like(stream, gensym, |temp, atom| AnfLet {
            name: temp,
            expr: AnfExpr::First { stream: atom },
        }),
        CoreExpr::Fby { head, tail } => {
            let (head_atom, mut head_lets) = normalize_expr(head, gensym);
            let (tail_atom, mut tail_lets) = normalize_expr(tail, gensym);
            let mut lets = Vec::new();
            lets.append(&mut head_lets);
            lets.append(&mut tail_lets);
            let temp = gensym.fresh();
            lets.push(AnfLet {
                name: temp.clone(),
                expr: AnfExpr::Fby {
                    head: head_atom,
                    tail: tail_atom,
                },
            });
            (AnfAtom::Ref(temp), lets)
        }
        CoreExpr::When { value, guard } => {
            let (value_atom, mut value_lets) = normalize_expr(value, gensym);
            let (guard_atom, mut guard_lets) = normalize_expr(guard, gensym);
            let mut lets = Vec::new();
            lets.append(&mut value_lets);
            lets.append(&mut guard_lets);
            let temp = gensym.fresh();
            lets.push(AnfLet {
                name: temp.clone(),
                expr: AnfExpr::When {
                    value: value_atom,
                    guard: guard_atom,
                },
            });
            (AnfAtom::Ref(temp), lets)
        }
        CoreExpr::WindowSum { window_len, input } => {
            let (input_atom, mut lets) = normalize_expr(input, gensym);
            let temp = gensym.fresh();
            lets.push(AnfLet {
                name: temp.clone(),
                expr: AnfExpr::WindowSum {
                    window_len: *window_len,
                    input: input_atom,
                },
            });
            (AnfAtom::Ref(temp), lets)
        }
        CoreExpr::Monotone { direction, input } => {
            let (input_atom, mut lets) = normalize_expr(input, gensym);
            let temp = gensym.fresh();
            lets.push(AnfLet {
                name: temp.clone(),
                expr: AnfExpr::Monotone {
                    direction: *direction,
                    input: input_atom,
                },
            });
            (AnfAtom::Ref(temp), lets)
        }
        CoreExpr::UniqueWithin {
            window_len,
            value,
            key,
        } => {
            let (value_atom, mut lets) = normalize_expr(value, gensym);
            let key_atom = if let Some(key_expr) = key {
                let (atom, mut extra) = normalize_expr(key_expr, gensym);
                lets.append(&mut extra);
                Some(atom)
            } else {
                None
            };
            let temp = gensym.fresh();
            lets.push(AnfLet {
                name: temp.clone(),
                expr: AnfExpr::UniqueWithin {
                    window_len: *window_len,
                    value: value_atom,
                    key: key_atom,
                },
            });
            (AnfAtom::Ref(temp), lets)
        }
        CoreExpr::UniqueUnbounded { .. } => {
            unreachable!("UniqueUnbounded should not appear before state extraction")
        }
    }
}

fn normalize_unary_like<F>(expr: &CoreExpr, gensym: &mut Gensym, ctor: F) -> (AnfAtom, Vec<AnfLet>)
where
    F: Fn(String, AnfAtom) -> AnfLet,
{
    let (atom, mut lets) = normalize_expr(expr, gensym);
    let temp = gensym.fresh();
    lets.push(ctor(temp.clone(), atom));
    (AnfAtom::Ref(temp), lets)
}

#[derive(Default)]
struct Gensym {
    counter: usize,
}

impl Gensym {
    fn fresh(&mut self) -> String {
        let name = format!("__anf_tmp_{}", self.counter);
        self.counter += 1;
        name
    }
}

impl AnfModule {
    pub fn rebuild_core(&self) -> Vec<CoreBinding> {
        self.bindings
            .iter()
            .map(|binding| CoreBinding {
                name: binding.name.clone(),
                expr: block_to_core(&binding.block),
            })
            .collect()
    }
}

fn block_to_core(block: &AnfBlock) -> CoreExpr {
    let mut env: HashMap<String, CoreExpr> = HashMap::new();
    for let_binding in &block.lets {
        let expr = anf_expr_to_core(&let_binding.expr, &env);
        env.insert(let_binding.name.clone(), expr);
    }
    atom_to_core(&block.result, &env)
}

fn anf_expr_to_core(expr: &AnfExpr, env: &HashMap<String, CoreExpr>) -> CoreExpr {
    match expr {
        AnfExpr::Unary { op, expr } => CoreExpr::Unary {
            op: *op,
            expr: Box::new(atom_to_core(expr, env)),
        },
        AnfExpr::Binary { op, left, right } => CoreExpr::Binary {
            op: *op,
            left: Box::new(atom_to_core(left, env)),
            right: Box::new(atom_to_core(right, env)),
        },
        AnfExpr::Next { stream } => CoreExpr::Next(Box::new(atom_to_core(stream, env))),
        AnfExpr::First { stream } => CoreExpr::First(Box::new(atom_to_core(stream, env))),
        AnfExpr::Fby { head, tail } => CoreExpr::Fby {
            head: Box::new(atom_to_core(head, env)),
            tail: Box::new(atom_to_core(tail, env)),
        },
        AnfExpr::When { value, guard } => CoreExpr::When {
            value: Box::new(atom_to_core(value, env)),
            guard: Box::new(atom_to_core(guard, env)),
        },
        AnfExpr::WindowSum { window_len, input } => CoreExpr::WindowSum {
            window_len: *window_len,
            input: Box::new(atom_to_core(input, env)),
        },
        AnfExpr::UniqueWithin {
            window_len,
            value,
            key,
        } => CoreExpr::UniqueWithin {
            window_len: *window_len,
            value: Box::new(atom_to_core(value, env)),
            key: key.as_ref().map(|atom| Box::new(atom_to_core(atom, env))),
        },
        AnfExpr::Monotone { direction, input } => CoreExpr::Monotone {
            direction: *direction,
            input: Box::new(atom_to_core(input, env)),
        },
    }
}

fn atom_to_core(atom: &AnfAtom, env: &HashMap<String, CoreExpr>) -> CoreExpr {
    match atom {
        AnfAtom::Literal(value) => CoreExpr::Literal(value.clone()),
        AnfAtom::Ref(name) => env
            .get(name)
            .cloned()
            .unwrap_or_else(|| CoreExpr::Ref(name.clone())),
    }
}
