use super::anf::AnfModule;
use super::core::{
    CoreBinaryOp, CoreBinding, CoreExpr, CoreModule, CoreStateSlot, CoreUnaryOp, MonotoneDirection,
    UniqueSpec,
};
use crate::runtime::value::ScalarValue;

pub fn extract_state(module: &AnfModule) -> CoreModule {
    let mut state_slots = Vec::new();
    let mut unique_specs = Vec::new();
    let mut gensym = 0usize;
    let bindings = module
        .rebuild_core()
        .into_iter()
        .map(|binding| rewrite_binding(binding, &mut state_slots, &mut unique_specs, &mut gensym))
        .collect();

    CoreModule {
        bindings,
        state_slots,
        unique_specs,
    }
}

fn rewrite_binding(
    binding: CoreBinding,
    state_slots: &mut Vec<CoreStateSlot>,
    unique_specs: &mut Vec<UniqueSpec>,
    gensym: &mut usize,
) -> CoreBinding {
    let expr = rewrite_expr(&binding.expr, state_slots, unique_specs, gensym);
    CoreBinding {
        name: binding.name,
        expr,
    }
}

fn rewrite_expr(
    expr: &CoreExpr,
    slots: &mut Vec<CoreStateSlot>,
    uniques: &mut Vec<UniqueSpec>,
    gensym: &mut usize,
) -> CoreExpr {
    match expr {
        CoreExpr::Literal(_) | CoreExpr::Ref(_) | CoreExpr::StateRef(_) => expr.clone(),
        CoreExpr::Unary { op, expr } => CoreExpr::Unary {
            op: *op,
            expr: Box::new(rewrite_expr(expr, slots, uniques, gensym)),
        },
        CoreExpr::Binary { op, left, right } => CoreExpr::Binary {
            op: *op,
            left: Box::new(rewrite_expr(left, slots, uniques, gensym)),
            right: Box::new(rewrite_expr(right, slots, uniques, gensym)),
        },
        CoreExpr::Next(stream) => {
            CoreExpr::Next(Box::new(rewrite_expr(stream, slots, uniques, gensym)))
        }
        CoreExpr::First(stream) => {
            CoreExpr::First(Box::new(rewrite_expr(stream, slots, uniques, gensym)))
        }
        CoreExpr::When { value, guard } => CoreExpr::When {
            value: Box::new(rewrite_expr(value, slots, uniques, gensym)),
            guard: Box::new(rewrite_expr(guard, slots, uniques, gensym)),
        },
        CoreExpr::WindowSum { window_len, input } => {
            let lowered_input = rewrite_expr(input, slots, uniques, gensym);
            if *window_len <= 1 {
                lowered_input
            } else {
                let history_len = window_len - 1;
                let mut slot_names = Vec::new();
                for _ in 0..history_len {
                    let slot_name = format!("state_{}", *gensym);
                    *gensym += 1;
                    slot_names.push(slot_name);
                }
                for (idx, name) in slot_names.iter().enumerate() {
                    let next_expr = if idx == 0 {
                        lowered_input.clone()
                    } else {
                        CoreExpr::StateRef(slot_names[idx - 1].clone())
                    };
                    slots.push(CoreStateSlot {
                        name: name.clone(),
                        init: CoreExpr::Literal(ScalarValue::Int(0)),
                        next: next_expr,
                    });
                }
                let mut sum_expr = lowered_input;
                for name in slot_names.iter() {
                    sum_expr = CoreExpr::Binary {
                        op: CoreBinaryOp::Add,
                        left: Box::new(sum_expr),
                        right: Box::new(CoreExpr::StateRef(name.clone())),
                    };
                }
                sum_expr
            }
        }
        CoreExpr::Monotone { direction, input } => {
            let lowered_input = rewrite_expr(input, slots, uniques, gensym);
            let last_slot = format!("state_{}", *gensym);
            *gensym += 1;
            let seen_slot = format!("state_{}", *gensym);
            *gensym += 1;

            slots.push(CoreStateSlot {
                name: last_slot.clone(),
                init: CoreExpr::Literal(ScalarValue::Int(0)),
                next: lowered_input.clone(),
            });
            slots.push(CoreStateSlot {
                name: seen_slot.clone(),
                init: CoreExpr::Literal(ScalarValue::Int(0)),
                next: CoreExpr::Literal(ScalarValue::Int(1)),
            });

            let not_seen = CoreExpr::Binary {
                op: CoreBinaryOp::Eq,
                left: Box::new(CoreExpr::StateRef(seen_slot)),
                right: Box::new(CoreExpr::Literal(ScalarValue::Int(0))),
            };
            let cmp_op = match direction {
                MonotoneDirection::NonDecreasing => CoreBinaryOp::Gte,
                MonotoneDirection::NonIncreasing => CoreBinaryOp::Lte,
            };
            let comparison = CoreExpr::Binary {
                op: cmp_op,
                left: Box::new(lowered_input.clone()),
                right: Box::new(CoreExpr::StateRef(last_slot)),
            };
            CoreExpr::Binary {
                op: CoreBinaryOp::Or,
                left: Box::new(not_seen),
                right: Box::new(comparison),
            }
        }
        CoreExpr::UniqueWithin {
            window_len,
            value,
            key,
        } => {
            let lowered_value = rewrite_expr(value, slots, uniques, gensym);
            let lowered_key = key
                .as_ref()
                .map(|expr| rewrite_expr(expr, slots, uniques, gensym));

            if *window_len == 0 {
                let store_name = format!("unique_store_{}", uniques.len());
                uniques.push(UniqueSpec {
                    name: store_name.clone(),
                    keyed: lowered_key.is_some(),
                });
                return CoreExpr::UniqueUnbounded {
                    store: store_name,
                    value: Box::new(lowered_value),
                    key: lowered_key.map(Box::new),
                };
            }

            let capacity = *window_len;
            let mut value_names = Vec::new();
            for _ in 0..capacity {
                value_names.push(format!("state_{}", *gensym));
                *gensym += 1;
            }
            let mut key_names = Vec::new();
            if lowered_key.is_some() {
                for _ in 0..capacity {
                    key_names.push(format!("state_{}", *gensym));
                    *gensym += 1;
                }
            }
            let mut rem_names = Vec::new();
            for _ in 0..capacity {
                rem_names.push(format!("state_{}", *gensym));
                *gensym += 1;
            }

            let zero = CoreExpr::Literal(ScalarValue::Int(0));
            let window_literal = CoreExpr::Literal(ScalarValue::Int(*window_len as i64));

            let mut dec_rems = Vec::new();
            let mut alive_exprs = Vec::new();
            for rem_name in &rem_names {
                let rem_ref = CoreExpr::StateRef(rem_name.clone());
                let still_has_time = CoreExpr::Binary {
                    op: CoreBinaryOp::Gt,
                    left: Box::new(rem_ref.clone()),
                    right: Box::new(zero.clone()),
                };
                let dec = CoreExpr::Binary {
                    op: CoreBinaryOp::Sub,
                    left: Box::new(rem_ref),
                    right: Box::new(still_has_time.clone()),
                };
                dec_rems.push(dec.clone());
                alive_exprs.push(CoreExpr::Binary {
                    op: CoreBinaryOp::Gt,
                    left: Box::new(dec),
                    right: Box::new(zero.clone()),
                });
            }

            let mut is_unique = CoreExpr::Literal(ScalarValue::Bool(true));
            for idx in 0..capacity {
                let alive = alive_exprs[idx].clone();
                let value_eq = CoreExpr::Binary {
                    op: CoreBinaryOp::Eq,
                    left: Box::new(lowered_value.clone()),
                    right: Box::new(CoreExpr::StateRef(value_names[idx].clone())),
                };
                let key_eq = if let Some(ref key_expr) = lowered_key {
                    CoreExpr::Binary {
                        op: CoreBinaryOp::Eq,
                        left: Box::new(key_expr.clone()),
                        right: Box::new(CoreExpr::StateRef(key_names[idx].clone())),
                    }
                } else {
                    CoreExpr::Literal(ScalarValue::Bool(true))
                };
                let match_slot = CoreExpr::Binary {
                    op: CoreBinaryOp::And,
                    left: Box::new(alive),
                    right: Box::new(CoreExpr::Binary {
                        op: CoreBinaryOp::And,
                        left: Box::new(value_eq),
                        right: Box::new(key_eq),
                    }),
                };
                let not_match = CoreExpr::Unary {
                    op: CoreUnaryOp::Not,
                    expr: Box::new(match_slot),
                };
                is_unique = CoreExpr::Binary {
                    op: CoreBinaryOp::And,
                    left: Box::new(is_unique),
                    right: Box::new(not_match),
                };
            }

            let inv_unique = CoreExpr::Unary {
                op: CoreUnaryOp::Not,
                expr: Box::new(is_unique.clone()),
            };

            let mut value_nexts = Vec::new();
            for idx in 0..capacity {
                let current_value = CoreExpr::StateRef(value_names[idx].clone());
                let source_value = if idx == 0 {
                    lowered_value.clone()
                } else {
                    CoreExpr::StateRef(value_names[idx - 1].clone())
                };
                value_nexts.push(select_expr(
                    &is_unique,
                    &inv_unique,
                    source_value,
                    current_value,
                ));
            }

            let mut key_nexts = Vec::new();
            if lowered_key.is_some() {
                for idx in 0..capacity {
                    let current_key = CoreExpr::StateRef(key_names[idx].clone());
                    let source_key = if idx == 0 {
                        lowered_key.as_ref().unwrap().clone()
                    } else {
                        CoreExpr::StateRef(key_names[idx - 1].clone())
                    };
                    key_nexts.push(select_expr(
                        &is_unique,
                        &inv_unique,
                        source_key,
                        current_key,
                    ));
                }
            }

            let mut rem_nexts = Vec::new();
            for idx in 0..capacity {
                let current_rem = dec_rems[idx].clone();
                let source_rem = if idx == 0 {
                    window_literal.clone()
                } else {
                    dec_rems[idx - 1].clone()
                };
                rem_nexts.push(select_expr(
                    &is_unique,
                    &inv_unique,
                    source_rem,
                    current_rem,
                ));
            }

            for (name, next_expr) in value_names.into_iter().zip(value_nexts.into_iter()) {
                slots.push(CoreStateSlot {
                    name,
                    init: CoreExpr::Literal(ScalarValue::Int(0)),
                    next: next_expr,
                });
            }
            if lowered_key.is_some() {
                for (name, next_expr) in key_names.into_iter().zip(key_nexts.into_iter()) {
                    slots.push(CoreStateSlot {
                        name,
                        init: CoreExpr::Literal(ScalarValue::Int(0)),
                        next: next_expr,
                    });
                }
            }
            for (name, next_expr) in rem_names.into_iter().zip(rem_nexts.into_iter()) {
                slots.push(CoreStateSlot {
                    name,
                    init: CoreExpr::Literal(ScalarValue::Int(0)),
                    next: next_expr,
                });
            }

            is_unique
        }
        CoreExpr::UniqueUnbounded { .. } => expr.clone(),
        CoreExpr::Fby { head, tail } => {
            let init_expr = rewrite_expr(head, slots, uniques, gensym);
            let next_expr = rewrite_expr(tail, slots, uniques, gensym);
            let slot_name = format!("state_{}", *gensym);
            *gensym += 1;
            slots.push(CoreStateSlot {
                name: slot_name.clone(),
                init: init_expr,
                next: next_expr,
            });
            CoreExpr::StateRef(slot_name)
        }
    }
}

fn select_expr(
    cond: &CoreExpr,
    inv_cond: &CoreExpr,
    when_true: CoreExpr,
    when_false: CoreExpr,
) -> CoreExpr {
    let true_term = CoreExpr::Binary {
        op: CoreBinaryOp::Mul,
        left: Box::new(cond.clone()),
        right: Box::new(when_true),
    };
    let false_term = CoreExpr::Binary {
        op: CoreBinaryOp::Mul,
        left: Box::new(inv_cond.clone()),
        right: Box::new(when_false),
    };
    CoreExpr::Binary {
        op: CoreBinaryOp::Add,
        left: Box::new(true_term),
        right: Box::new(false_term),
    }
}
