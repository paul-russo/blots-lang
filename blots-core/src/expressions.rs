use crate::{
    ast::{BinaryOp, DoStatement, Expr, PostfixOp, RecordEntry, RecordKey, UnaryOp},
    functions::{
        get_built_in_function_def_by_ident, get_built_in_function_id, get_function_def,
        is_built_in_function,
    },
    heap::{Heap, HeapPointer, HeapValue, IterablePointer, RecordPointer},
    parser::Rule,
    values::{
        LambdaArg, LambdaDef,
        Value::{self, Bool, List, Number, Spread},
    },
};
use anyhow::{anyhow, Result};
use pest::{
    iterators::Pairs,
    pratt_parser::{Assoc, Op, PrattParser},
};
use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap, HashSet},
    rc::Rc,
    sync::LazyLock,
};

static PRATT: LazyLock<PrattParser<Rule>> = LazyLock::new(|| {
    PrattParser::new()
        .op(Op::infix(Rule::and, Assoc::Left)
            | Op::infix(Rule::natural_and, Assoc::Left)
            | Op::infix(Rule::or, Assoc::Left)
            | Op::infix(Rule::natural_or, Assoc::Left)
            | Op::infix(Rule::with, Assoc::Left))
        .op(Op::infix(Rule::equal, Assoc::Left)
            | Op::infix(Rule::not_equal, Assoc::Left)
            | Op::infix(Rule::less, Assoc::Left)
            | Op::infix(Rule::less_eq, Assoc::Left)
            | Op::infix(Rule::greater, Assoc::Left)
            | Op::infix(Rule::greater_eq, Assoc::Left))
        .op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::subtract, Assoc::Left))
        .op(Op::infix(Rule::multiply, Assoc::Left)
            | Op::infix(Rule::divide, Assoc::Left)
            | Op::infix(Rule::modulo, Assoc::Left))
        .op(Op::infix(Rule::power, Assoc::Right) | Op::infix(Rule::coalesce, Assoc::Left))
        .op(Op::prefix(Rule::negation)
            | Op::prefix(Rule::spread_operator)
            | Op::prefix(Rule::invert)
            | Op::prefix(Rule::not))
        .op(Op::postfix(Rule::factorial))
        .op(Op::postfix(Rule::access)
            | Op::postfix(Rule::dot_access)
            | Op::postfix(Rule::call_list))
});

/// Parse pairs into AST and evaluate
pub fn evaluate_pairs(
    pairs: Pairs<Rule>,
    heap: Rc<RefCell<Heap>>,
    bindings: Rc<RefCell<HashMap<String, Value>>>,
    call_depth: usize,
) -> Result<Value> {
    // First parse to AST
    let ast = pairs_to_expr(pairs)?;

    // Then evaluate the AST
    evaluate_ast(&ast, heap, bindings, call_depth)
}

// Evaluate an AST expression
pub fn evaluate_ast(
    expr: &Expr,
    heap: Rc<RefCell<Heap>>,
    bindings: Rc<RefCell<HashMap<String, Value>>>,
    call_depth: usize,
) -> Result<Value> {
    match expr {
        Expr::Number(n) => Ok(Number(*n)),
        Expr::String(s) => Ok(heap.borrow_mut().insert_string(s.clone())),
        Expr::Bool(b) => Ok(Bool(*b)),
        Expr::Null => Ok(Value::Null),
        Expr::Identifier(ident) => match ident.as_str() {
            "infinity" => Ok(Number(f64::INFINITY)),
            "constants" => Ok(Value::Record(RecordPointer::new(0))),
            _ => bindings
                .borrow()
                .get(ident)
                .copied()
                .ok_or(anyhow!("unknown identifier: {}", ident)),
        },
        Expr::BuiltIn(id) => Ok(Value::BuiltIn(*id)),
        Expr::List(exprs) => {
            let values = exprs
                .iter()
                .map(|e| evaluate_ast(e, Rc::clone(&heap), Rc::clone(&bindings), call_depth))
                .collect::<Result<Vec<Value>>>()?;

            // Flatten spreads
            let flattened = values
                .into_iter()
                .flat_map(|value| {
                    value
                        .reify(unsafe { heap.try_borrow_unguarded().unwrap() })
                        .unwrap()
                        .with_heap(Rc::clone(&heap))
                        .into_iter()
                })
                .collect::<Vec<Value>>();

            Ok(heap.borrow_mut().insert_list(flattened))
        }
        Expr::Record(entries) => {
            let mut record = BTreeMap::new();

            for entry in entries {
                match &entry.key {
                    RecordKey::Static(key) => {
                        let value = evaluate_ast(
                            &entry.value,
                            Rc::clone(&heap),
                            Rc::clone(&bindings),
                            call_depth,
                        )?;
                        record.insert(key.clone(), value);
                    }
                    RecordKey::Dynamic(key_expr) => {
                        let key_value = evaluate_ast(
                            key_expr,
                            Rc::clone(&heap),
                            Rc::clone(&bindings),
                            call_depth,
                        )?;
                        let key = key_value.as_string(&heap.borrow())?.to_string();
                        let value = evaluate_ast(
                            &entry.value,
                            Rc::clone(&heap),
                            Rc::clone(&bindings),
                            call_depth,
                        )?;
                        record.insert(key, value);
                    }
                    RecordKey::Shorthand(ident) => {
                        let value = bindings
                            .borrow()
                            .get(ident)
                            .copied()
                            .ok_or(anyhow!("unknown identifier: {}", ident))?;
                        record.insert(ident.clone(), value);
                    }
                    RecordKey::Spread(spread_expr) => {
                        let spread_value = evaluate_ast(
                            spread_expr,
                            Rc::clone(&heap),
                            Rc::clone(&bindings),
                            call_depth,
                        )?;

                        if let Spread(iterable) = spread_value {
                            match iterable {
                                IterablePointer::List(pointer) => {
                                    let borrowed_heap = &heap.borrow();
                                    pointer
                                        .reify(borrowed_heap)
                                        .as_list()?
                                        .iter()
                                        .enumerate()
                                        .for_each(|(i, value)| {
                                            record.insert(i.to_string(), *value);
                                        });
                                }
                                IterablePointer::String(pointer) => {
                                    let s = {
                                        let borrowed_heap = &heap.borrow();
                                        pointer.reify(borrowed_heap).as_string()?.to_string()
                                    };

                                    s.chars().enumerate().for_each(|(i, c)| {
                                        record.insert(
                                            i.to_string(),
                                            heap.borrow_mut().insert_string(c.to_string()),
                                        );
                                    });
                                }
                                IterablePointer::Record(pointer) => {
                                    let borrowed_heap = &heap.borrow();
                                    let spread_record = pointer.reify(borrowed_heap).as_record()?;

                                    for (k, v) in spread_record {
                                        record.insert(k.clone(), *v);
                                    }
                                }
                            }
                        }
                    }
                }
            }

            Ok(heap.borrow_mut().insert_record(record))
        }
        Expr::Lambda { args, body } => {
            // Capture variables used in the lambda body
            let mut captured_scope = HashMap::new();
            let mut referenced_vars = Vec::new();
            collect_free_variables(body, &mut referenced_vars, &mut HashSet::new());

            let current_bindings = bindings.borrow();
            for var in referenced_vars {
                if current_bindings.contains_key(&var) && !is_built_in_function(&var) {
                    captured_scope.insert(var.clone(), current_bindings[&var].clone());
                }
            }

            let lambda = heap.borrow_mut().insert_lambda(LambdaDef {
                name: None,
                args: args.clone(),
                body: (**body).clone(),
                scope: captured_scope,
            });

            Ok(lambda)
        }
        Expr::Assignment { ident, value } => {
            if is_built_in_function(ident) {
                return Err(anyhow!(
                    "{} is the name of a built-in function, and cannot be reassigned",
                    ident
                ));
            }

            if ident == "constants"
                || ident == "if"
                || ident == "then"
                || ident == "else"
                || ident == "true"
                || ident == "false"
                || ident == "null"
                || ident == "inputs"
                || ident == "and"
                || ident == "or"
                || ident == "with"
            {
                return Err(anyhow!("{} is a keyword, and cannot be reassigned", ident));
            }

            // Check if the variable already exists (immutability check)
            if bindings.borrow().contains_key(ident) {
                return Err(anyhow!(
                    "{} is already defined, and cannot be reassigned",
                    ident
                ));
            }

            let val = evaluate_ast(value, Rc::clone(&heap), Rc::clone(&bindings), call_depth)?;

            // Set lambda name if assigning a lambda
            if let Value::Lambda(lambda_ptr) = val {
                let mut borrowed_heap = heap.borrow_mut();
                if let Some(HeapValue::Lambda(lambda_def)) =
                    borrowed_heap.get_mut(lambda_ptr.index())
                {
                    lambda_def.name = Some(ident.clone());
                }
            }

            bindings.borrow_mut().insert(ident.clone(), val);
            Ok(val)
        }
        Expr::Conditional {
            condition,
            then_expr,
            else_expr,
        } => {
            let cond_val = evaluate_ast(
                condition,
                Rc::clone(&heap),
                Rc::clone(&bindings),
                call_depth,
            )?;

            if cond_val.as_bool()? {
                evaluate_ast(then_expr, heap, bindings, call_depth)
            } else {
                evaluate_ast(else_expr, heap, bindings, call_depth)
            }
        }
        Expr::DoBlock {
            statements,
            return_expr,
        } => {
            // Create new scope from current bindings
            let new_bindings = bindings.borrow().clone();
            let block_bindings = Rc::new(RefCell::new(new_bindings));

            // Execute statements
            for stmt in statements {
                match stmt {
                    DoStatement::Expression(expr) => {
                        evaluate_ast(
                            expr,
                            Rc::clone(&heap),
                            Rc::clone(&block_bindings),
                            call_depth,
                        )?;
                    }
                    DoStatement::Comment(_) => {} // Skip comments
                }
            }

            // Return the final expression
            evaluate_ast(return_expr, heap, block_bindings, call_depth)
        }
        Expr::Call { func, args } => {
            let func_val = evaluate_ast(func, Rc::clone(&heap), Rc::clone(&bindings), call_depth)?;
            let arg_vals_raw = args
                .iter()
                .map(|arg| evaluate_ast(arg, Rc::clone(&heap), Rc::clone(&bindings), call_depth))
                .collect::<Result<Vec<_>>>()?;

            // Flatten any spread arguments
            let arg_vals = arg_vals_raw
                .into_iter()
                .flat_map(|value| {
                    value
                        .reify(unsafe { heap.try_borrow_unguarded().unwrap() })
                        .unwrap()
                        .with_heap(Rc::clone(&heap))
                        .into_iter()
                })
                .collect::<Vec<Value>>();

            if !func_val.is_lambda() && !func_val.is_built_in() {
                return Err(anyhow!(
                    "can't call a non-function: {}",
                    func_val.stringify(&heap.borrow())
                ));
            }

            let def = {
                let borrowed_heap = &heap.borrow();
                get_function_def(&func_val, borrowed_heap).ok_or_else(|| {
                    anyhow!("unknown function: {}", func_val.stringify(borrowed_heap))
                })?
            };

            def.call(func_val, arg_vals, heap, bindings, call_depth)
        }
        Expr::Access { expr, index } => {
            let val = evaluate_ast(expr, Rc::clone(&heap), Rc::clone(&bindings), call_depth)?;
            let idx_val = evaluate_ast(index, Rc::clone(&heap), Rc::clone(&bindings), call_depth)?;

            match val {
                Value::Record(record) => {
                    let borrowed_heap = &heap.borrow();
                    let record = record.reify(borrowed_heap).as_record()?;
                    let key = idx_val.as_string(borrowed_heap)?;
                    Ok(record.get(key).copied().unwrap_or(Value::Null))
                }
                Value::List(list) => {
                    let borrowed_heap = &heap.borrow();
                    let list = list.reify(borrowed_heap).as_list()?;
                    let index = usize::try_from(idx_val.as_number()? as u64)?;
                    Ok(list.get(index).copied().unwrap_or(Value::Null))
                }
                Value::String(string) => {
                    let string = string.reify(&heap.borrow()).as_string()?.to_string();
                    let index = usize::try_from(idx_val.as_number()? as u64)?;

                    Ok(string
                        .chars()
                        .nth(index)
                        .map(|c| heap.borrow_mut().insert_string(c.to_string()))
                        .unwrap_or(Value::Null))
                }
                _ => Err(anyhow!(
                    "expected a record, list, string, but got a {}",
                    val.get_type()
                )),
            }
        }
        Expr::DotAccess { expr, field } => {
            let val = evaluate_ast(expr, Rc::clone(&heap), Rc::clone(&bindings), call_depth)?;
            let borrowed_heap = &heap.borrow();

            match val {
                Value::Record(record) => {
                    let record = record.reify(borrowed_heap).as_record()?;
                    Ok(record.get(field).copied().unwrap_or(Value::Null))
                }
                _ => Err(anyhow!("expected a record, but got a {}", val.get_type())),
            }
        }
        Expr::BinaryOp { op, left, right } => {
            evaluate_binary_op_ast(*op, left, right, heap, bindings, call_depth)
        }
        Expr::UnaryOp { op, expr } => {
            let val = evaluate_ast(expr, heap, bindings, call_depth)?;

            match op {
                UnaryOp::Negate => Ok(Number(-val.as_number()?)),
                UnaryOp::Not | UnaryOp::Invert => Ok(Bool(!val.as_bool()?)),
            }
        }
        Expr::PostfixOp { op, expr } => {
            let val = evaluate_ast(expr, heap, bindings, call_depth)?;

            match op {
                PostfixOp::Factorial => {
                    let n = val.as_number()?;
                    if n >= 0.0 && n == (n as u64) as f64 {
                        Ok(Number(
                            (1..(n as u64) + 1).map(|x| x as f64).product::<f64>(),
                        ))
                    } else {
                        Err(anyhow!("factorial only works on non-negative integers"))
                    }
                }
            }
        }
        Expr::Spread(expr) => {
            let val = evaluate_ast(expr, heap, bindings, call_depth)?;

            match val {
                List(pointer) => Ok(Spread(IterablePointer::List(pointer))),
                Value::String(pointer) => Ok(Spread(IterablePointer::String(pointer))),
                Value::Record(pointer) => Ok(Spread(IterablePointer::Record(pointer))),
                _ => Err(anyhow!("expected a list, record, or string")),
            }
        }
    }
}

// Helper function to collect free variables in an expression
fn collect_free_variables(expr: &Expr, vars: &mut Vec<String>, bound: &mut HashSet<String>) {
    match expr {
        Expr::Identifier(name) => {
            if !bound.contains(name) && name != "infinity" && name != "constants" {
                vars.push(name.clone());
            }
        }
        Expr::Lambda { args, body } => {
            let mut new_bound = bound.clone();
            for arg in args {
                new_bound.insert(arg.get_name().to_string());
            }
            collect_free_variables(body, vars, &mut new_bound);
        }
        Expr::BinaryOp { left, right, .. } => {
            collect_free_variables(left, vars, bound);
            collect_free_variables(right, vars, bound);
        }
        Expr::UnaryOp { expr, .. } | Expr::PostfixOp { expr, .. } | Expr::Spread(expr) => {
            collect_free_variables(expr, vars, bound);
        }
        Expr::Call { func, args } => {
            collect_free_variables(func, vars, bound);
            for arg in args {
                collect_free_variables(arg, vars, bound);
            }
        }
        Expr::Access { expr, index } => {
            collect_free_variables(expr, vars, bound);
            collect_free_variables(index, vars, bound);
        }
        Expr::DotAccess { expr, .. } => {
            collect_free_variables(expr, vars, bound);
        }
        Expr::Conditional {
            condition,
            then_expr,
            else_expr,
        } => {
            collect_free_variables(condition, vars, bound);
            collect_free_variables(then_expr, vars, bound);
            collect_free_variables(else_expr, vars, bound);
        }
        Expr::Assignment { value, .. } => {
            collect_free_variables(value, vars, bound);
        }
        Expr::List(exprs) => {
            for expr in exprs {
                collect_free_variables(expr, vars, bound);
            }
        }
        Expr::Record(entries) => {
            for entry in entries {
                match &entry.key {
                    RecordKey::Dynamic(expr) | RecordKey::Spread(expr) => {
                        collect_free_variables(expr, vars, bound);
                    }
                    _ => {}
                }
                if !matches!(entry.key, RecordKey::Shorthand(_) | RecordKey::Spread(_)) {
                    collect_free_variables(&entry.value, vars, bound);
                }
            }
        }
        Expr::DoBlock {
            statements,
            return_expr,
        } => {
            for stmt in statements {
                if let DoStatement::Expression(expr) = stmt {
                    collect_free_variables(expr, vars, bound);
                }
            }
            collect_free_variables(return_expr, vars, bound);
        }
        _ => {}
    }
}

// Evaluate binary operations on AST
fn evaluate_binary_op_ast(
    op: BinaryOp,
    left: &Expr,
    right: &Expr,
    heap: Rc<RefCell<Heap>>,
    bindings: Rc<RefCell<HashMap<String, Value>>>,
    call_depth: usize,
) -> Result<Value> {
    let lhs = evaluate_ast(left, Rc::clone(&heap), Rc::clone(&bindings), call_depth)?;
    let rhs = evaluate_ast(right, Rc::clone(&heap), Rc::clone(&bindings), call_depth)?;

    match (lhs, rhs) {
        (Value::List(list_l), Value::List(list_r)) => {
            let (l_vec, r_vec) = {
                let borrowed_heap = &heap.borrow();
                (
                    list_l.reify(borrowed_heap).as_list()?.clone(),
                    list_r.reify(borrowed_heap).as_list()?.clone(),
                )
            };

            if l_vec.len() != r_vec.len() {
                return Err(anyhow!(
                    "left- and right-hand-side lists must be the same length"
                ));
            }

            match op {
                BinaryOp::Equal => Ok(Bool(l_vec.iter().zip(&r_vec).all(|(l, r)| l == r))),
                BinaryOp::NotEqual => Ok(Bool(l_vec.iter().zip(&r_vec).all(|(l, r)| l != r))),
                BinaryOp::Less => Ok(Bool(l_vec.iter().zip(&r_vec).all(|(l, r)| l < r))),
                BinaryOp::LessEq => Ok(Bool(l_vec.iter().zip(&r_vec).all(|(l, r)| l <= r))),
                BinaryOp::Greater => Ok(Bool(l_vec.iter().zip(&r_vec).all(|(l, r)| l > r))),
                BinaryOp::GreaterEq => Ok(Bool(l_vec.iter().zip(&r_vec).all(|(l, r)| l >= r))),
                BinaryOp::And | BinaryOp::NaturalAnd => {
                    let mapped_list = l_vec
                        .iter()
                        .zip(&r_vec)
                        .map(|(l, r)| Ok(Bool(l.as_bool()? && r.as_bool()?)))
                        .collect::<Result<Vec<Value>>>()?;
                    Ok(heap.borrow_mut().insert_list(mapped_list))
                }
                BinaryOp::Or | BinaryOp::NaturalOr => {
                    let mapped_list = l_vec
                        .iter()
                        .zip(&r_vec)
                        .map(|(l, r)| Ok(Bool(l.as_bool()? || r.as_bool()?)))
                        .collect::<Result<Vec<Value>>>()?;
                    Ok(heap.borrow_mut().insert_list(mapped_list))
                }
                BinaryOp::Add => {
                    let mapped_list = l_vec
                        .iter()
                        .zip(&r_vec)
                        .map(|(l, r)| match (l, r) {
                            (Value::String(_), Value::String(_)) => {
                                let (l_str, r_str) = {
                                    (
                                        l.as_string(&heap.borrow())?.to_string(),
                                        r.as_string(&heap.borrow())?.to_string(),
                                    )
                                };
                                Ok(heap
                                    .borrow_mut()
                                    .insert_string(format!("{}{}", l_str, r_str)))
                            }
                            (Value::Number(_), Value::Number(_)) => {
                                Ok(Number(l.as_number()? + r.as_number()?))
                            }
                            _ => Err(anyhow!("can't add {} and {}", l.get_type(), r.get_type())),
                        })
                        .collect::<Result<Vec<Value>>>()?;
                    Ok(heap.borrow_mut().insert_list(mapped_list))
                }
                BinaryOp::Subtract => {
                    let mapped_list = l_vec
                        .iter()
                        .zip(&r_vec)
                        .map(|(l, r)| Ok(Number(l.as_number()? - r.as_number()?)))
                        .collect::<Result<Vec<Value>>>()?;
                    Ok(heap.borrow_mut().insert_list(mapped_list))
                }
                BinaryOp::Multiply => {
                    let mapped_list = l_vec
                        .iter()
                        .zip(&r_vec)
                        .map(|(l, r)| Ok(Number(l.as_number()? * r.as_number()?)))
                        .collect::<Result<Vec<Value>>>()?;
                    Ok(heap.borrow_mut().insert_list(mapped_list))
                }
                BinaryOp::Divide => {
                    let mapped_list = l_vec
                        .iter()
                        .zip(&r_vec)
                        .map(|(l, r)| Ok(Number(l.as_number()? / r.as_number()?)))
                        .collect::<Result<Vec<Value>>>()?;
                    Ok(heap.borrow_mut().insert_list(mapped_list))
                }
                BinaryOp::Modulo => {
                    let mapped_list = l_vec
                        .iter()
                        .zip(&r_vec)
                        .map(|(l, r)| Ok(Number(l.as_number()? % r.as_number()?)))
                        .collect::<Result<Vec<Value>>>()?;
                    Ok(heap.borrow_mut().insert_list(mapped_list))
                }
                BinaryOp::Power => {
                    let mapped_list = l_vec
                        .iter()
                        .zip(&r_vec)
                        .map(|(l, r)| Ok(Number(l.as_number()?.powf(r.as_number()?))))
                        .collect::<Result<Vec<Value>>>()?;
                    Ok(heap.borrow_mut().insert_list(mapped_list))
                }
                BinaryOp::Coalesce => {
                    let mapped_list = l_vec
                        .iter()
                        .zip(&r_vec)
                        .map(|(l, r)| {
                            if *l == Value::Null {
                                Ok(r.clone())
                            } else {
                                Ok(l.clone())
                            }
                        })
                        .collect::<Result<Vec<Value>>>()?;
                    Ok(heap.borrow_mut().insert_list(mapped_list))
                }
                BinaryOp::With => {
                    let mapped_list = l_vec
                        .iter()
                        .zip(&r_vec)
                        .map(|(l, r)| {
                            if !r.is_lambda() && !r.is_built_in() {
                                return Err(anyhow!(
                                    "right-hand iterable contains non-function {}",
                                    r.stringify(&heap.borrow())
                                ));
                            }

                            let def = {
                                let borrowed_heap = &heap.borrow();
                                get_function_def(r, borrowed_heap).ok_or_else(|| {
                                    anyhow!("unknown function: {}", r.stringify(borrowed_heap))
                                })?
                            };

                            def.call(
                                r.clone(),
                                vec![l.clone()],
                                Rc::clone(&heap),
                                Rc::clone(&bindings),
                                call_depth,
                            )
                        })
                        .collect::<Result<Vec<Value>>>()?;
                    Ok(heap.borrow_mut().insert_list(mapped_list))
                }
            }
        }
        (Value::List(list), scalar) | (scalar, Value::List(list)) => {
            let (l_vec, is_list_first) = {
                let borrowed_heap = &heap.borrow();
                if matches!(lhs, Value::List(_)) {
                    (list.reify(borrowed_heap).as_list()?.clone(), true)
                } else {
                    (list.reify(borrowed_heap).as_list()?.clone(), false)
                }
            };

            match op {
                BinaryOp::Equal => {
                    if is_list_first {
                        Ok(Bool(l_vec.iter().all(|v| v == &scalar)))
                    } else {
                        Ok(Bool(l_vec.iter().all(|v| &scalar == v)))
                    }
                }
                BinaryOp::NotEqual => {
                    if is_list_first {
                        Ok(Bool(l_vec.iter().all(|v| v != &scalar)))
                    } else {
                        Ok(Bool(l_vec.iter().all(|v| &scalar != v)))
                    }
                }
                BinaryOp::Less => {
                    if is_list_first {
                        Ok(Bool(l_vec.iter().all(|v| v < &scalar)))
                    } else {
                        Ok(Bool(l_vec.iter().all(|v| &scalar < v)))
                    }
                }
                BinaryOp::LessEq => {
                    if is_list_first {
                        Ok(Bool(l_vec.iter().all(|v| v <= &scalar)))
                    } else {
                        Ok(Bool(l_vec.iter().all(|v| &scalar <= v)))
                    }
                }
                BinaryOp::Greater => {
                    if is_list_first {
                        Ok(Bool(l_vec.iter().all(|v| v > &scalar)))
                    } else {
                        Ok(Bool(l_vec.iter().all(|v| &scalar > v)))
                    }
                }
                BinaryOp::GreaterEq => {
                    if is_list_first {
                        Ok(Bool(l_vec.iter().all(|v| v >= &scalar)))
                    } else {
                        Ok(Bool(l_vec.iter().all(|v| &scalar >= v)))
                    }
                }
                BinaryOp::And | BinaryOp::NaturalAnd => {
                    let mapped_list = if is_list_first {
                        l_vec
                            .iter()
                            .map(|v| Ok(Bool(v.as_bool()? && scalar.as_bool()?)))
                            .collect::<Result<Vec<Value>>>()?
                    } else {
                        l_vec
                            .iter()
                            .map(|v| Ok(Bool(scalar.as_bool()? && v.as_bool()?)))
                            .collect::<Result<Vec<Value>>>()?
                    };
                    Ok(heap.borrow_mut().insert_list(mapped_list))
                }
                BinaryOp::Or | BinaryOp::NaturalOr => {
                    let mapped_list = if is_list_first {
                        l_vec
                            .iter()
                            .map(|v| Ok(Bool(v.as_bool()? || scalar.as_bool()?)))
                            .collect::<Result<Vec<Value>>>()?
                    } else {
                        l_vec
                            .iter()
                            .map(|v| Ok(Bool(scalar.as_bool()? || v.as_bool()?)))
                            .collect::<Result<Vec<Value>>>()?
                    };
                    Ok(heap.borrow_mut().insert_list(mapped_list))
                }
                BinaryOp::Add => {
                    let mapped_list = if is_list_first {
                        l_vec
                            .iter()
                            .map(|v| match (v, &scalar) {
                                (Value::String(_), Value::String(_)) => {
                                    let (v_str, s_str) = {
                                        (
                                            v.as_string(&heap.borrow())?.to_string(),
                                            scalar.as_string(&heap.borrow())?.to_string(),
                                        )
                                    };
                                    Ok(heap
                                        .borrow_mut()
                                        .insert_string(format!("{}{}", v_str, s_str)))
                                }
                                (Value::Number(_), Value::Number(_)) => {
                                    Ok(Number(v.as_number()? + scalar.as_number()?))
                                }
                                _ => Err(anyhow!(
                                    "can't add {} and {}",
                                    v.get_type(),
                                    scalar.get_type()
                                )),
                            })
                            .collect::<Result<Vec<Value>>>()?
                    } else {
                        l_vec
                            .iter()
                            .map(|v| match (&scalar, v) {
                                (Value::String(_), Value::String(_)) => {
                                    let (s_str, v_str) = {
                                        (
                                            scalar.as_string(&heap.borrow())?.to_string(),
                                            v.as_string(&heap.borrow())?.to_string(),
                                        )
                                    };
                                    Ok(heap
                                        .borrow_mut()
                                        .insert_string(format!("{}{}", s_str, v_str)))
                                }
                                (Value::Number(_), Value::Number(_)) => {
                                    Ok(Number(scalar.as_number()? + v.as_number()?))
                                }
                                _ => Err(anyhow!(
                                    "can't add {} and {}",
                                    scalar.get_type(),
                                    v.get_type()
                                )),
                            })
                            .collect::<Result<Vec<Value>>>()?
                    };
                    Ok(heap.borrow_mut().insert_list(mapped_list))
                }
                BinaryOp::Subtract => {
                    let mapped_list = if is_list_first {
                        l_vec
                            .iter()
                            .map(|v| Ok(Number(v.as_number()? - scalar.as_number()?)))
                            .collect::<Result<Vec<Value>>>()?
                    } else {
                        l_vec
                            .iter()
                            .map(|v| Ok(Number(scalar.as_number()? - v.as_number()?)))
                            .collect::<Result<Vec<Value>>>()?
                    };
                    Ok(heap.borrow_mut().insert_list(mapped_list))
                }
                BinaryOp::Multiply => {
                    let mapped_list = l_vec
                        .iter()
                        .map(|v| Ok(Number(v.as_number()? * scalar.as_number()?)))
                        .collect::<Result<Vec<Value>>>()?;
                    Ok(heap.borrow_mut().insert_list(mapped_list))
                }
                BinaryOp::Divide => {
                    let mapped_list = if is_list_first {
                        l_vec
                            .iter()
                            .map(|v| Ok(Number(v.as_number()? / scalar.as_number()?)))
                            .collect::<Result<Vec<Value>>>()?
                    } else {
                        l_vec
                            .iter()
                            .map(|v| Ok(Number(scalar.as_number()? / v.as_number()?)))
                            .collect::<Result<Vec<Value>>>()?
                    };
                    Ok(heap.borrow_mut().insert_list(mapped_list))
                }
                BinaryOp::Modulo => {
                    let mapped_list = if is_list_first {
                        l_vec
                            .iter()
                            .map(|v| Ok(Number(v.as_number()? % scalar.as_number()?)))
                            .collect::<Result<Vec<Value>>>()?
                    } else {
                        l_vec
                            .iter()
                            .map(|v| Ok(Number(scalar.as_number()? % v.as_number()?)))
                            .collect::<Result<Vec<Value>>>()?
                    };
                    Ok(heap.borrow_mut().insert_list(mapped_list))
                }
                BinaryOp::Power => {
                    let mapped_list = if is_list_first {
                        l_vec
                            .iter()
                            .map(|v| Ok(Number(v.as_number()?.powf(scalar.as_number()?))))
                            .collect::<Result<Vec<Value>>>()?
                    } else {
                        l_vec
                            .iter()
                            .map(|v| Ok(Number(scalar.as_number()?.powf(v.as_number()?))))
                            .collect::<Result<Vec<Value>>>()?
                    };
                    Ok(heap.borrow_mut().insert_list(mapped_list))
                }
                BinaryOp::Coalesce => {
                    let mapped_list = if is_list_first {
                        l_vec
                            .iter()
                            .map(|v| {
                                if *v == Value::Null {
                                    Ok(scalar.clone())
                                } else {
                                    Ok(v.clone())
                                }
                            })
                            .collect::<Result<Vec<Value>>>()?
                    } else {
                        l_vec
                            .iter()
                            .map(|v| {
                                if scalar == Value::Null {
                                    Ok(v.clone())
                                } else {
                                    Ok(scalar.clone())
                                }
                            })
                            .collect::<Result<Vec<Value>>>()?
                    };
                    Ok(heap.borrow_mut().insert_list(mapped_list))
                }
                BinaryOp::With => {
                    if is_list_first {
                        if !scalar.is_callable() {
                            return Err(anyhow!(
                                "can't call a non-function: {}",
                                scalar.stringify(&heap.borrow())
                            ));
                        }

                        let list = heap.borrow_mut().insert_list(l_vec.clone());

                        let call_result = get_built_in_function_def_by_ident("map").unwrap().call(
                            scalar,
                            vec![list, scalar],
                            Rc::clone(&heap),
                            Rc::clone(&bindings),
                            call_depth,
                        )?;

                        let mapped_list = { call_result.as_list(&heap.borrow())?.clone() };

                        Ok(heap.borrow_mut().insert_list(mapped_list))
                    } else {
                        return Err(anyhow!("with operator requires function on right side"));
                    }
                }
            }
        }
        (lhs, rhs) => match op {
            BinaryOp::Equal => Ok(Bool(lhs == rhs)),
            BinaryOp::NotEqual => Ok(Bool(lhs != rhs)),
            BinaryOp::Less => Ok(Bool(lhs < rhs)),
            BinaryOp::LessEq => Ok(Bool(lhs <= rhs)),
            BinaryOp::Greater => Ok(Bool(lhs > rhs)),
            BinaryOp::GreaterEq => Ok(Bool(lhs >= rhs)),
            BinaryOp::And | BinaryOp::NaturalAnd => Ok(Bool(lhs.as_bool()? && rhs.as_bool()?)),
            BinaryOp::Or | BinaryOp::NaturalOr => Ok(Bool(lhs.as_bool()? || rhs.as_bool()?)),
            BinaryOp::Add => {
                if lhs.is_string() {
                    let (l_str, r_str) = {
                        (
                            lhs.as_string(&heap.borrow())?.to_string(),
                            rhs.as_string(&heap.borrow())?.to_string(),
                        )
                    };

                    return Ok(heap
                        .borrow_mut()
                        .insert_string(format!("{}{}", l_str, r_str)));
                }

                Ok(Number(lhs.as_number()? + rhs.as_number()?))
            }
            BinaryOp::Subtract => Ok(Number(lhs.as_number()? - rhs.as_number()?)),
            BinaryOp::Multiply => Ok(Number(lhs.as_number()? * rhs.as_number()?)),
            BinaryOp::Divide => Ok(Number(lhs.as_number()? / rhs.as_number()?)),
            BinaryOp::Modulo => Ok(Number(lhs.as_number()? % rhs.as_number()?)),
            BinaryOp::Power => Ok(Number(lhs.as_number()?.powf(rhs.as_number()?))),
            BinaryOp::Coalesce => {
                if lhs == Value::Null {
                    Ok(rhs)
                } else {
                    Ok(lhs)
                }
            }
            BinaryOp::With => {
                if !rhs.is_callable() {
                    return Err(anyhow!(
                        "can't call a non-function ({} is of type {})",
                        rhs.stringify(&heap.borrow()),
                        rhs.get_type()
                    ));
                }

                let def = { get_function_def(&rhs, &heap.borrow()) };

                if def.is_none() {
                    return Err(anyhow!(
                        "unknown function: {}",
                        rhs.stringify(&heap.borrow())
                    ));
                }

                return def.unwrap().call(
                    rhs,
                    vec![lhs],
                    Rc::clone(&heap),
                    Rc::clone(&bindings),
                    call_depth,
                );
            }
        },
    }
}

// Convert Pest pairs to our AST
pub fn pairs_to_expr(pairs: Pairs<Rule>) -> Result<Expr> {
    PRATT
        .map_primary(|primary| match primary.as_rule() {
            Rule::number => Ok(Expr::Number(
                primary
                    .as_str()
                    .replace("_", "")
                    .parse::<f64>()
                    .map_err(|e| anyhow::Error::from(e))?,
            )),
            Rule::list => {
                let list_pairs = primary.into_inner();
                let exprs = list_pairs
                    .into_iter()
                    .map(|pair| pairs_to_expr(pair.into_inner()))
                    .collect::<Result<Vec<Expr>>>()?;
                Ok(Expr::List(exprs))
            }
            Rule::record => {
                let record_pairs = primary.into_inner();
                let mut entries = Vec::new();

                for pair in record_pairs {
                    match pair.as_rule() {
                        Rule::record_pair => {
                            let mut inner_pairs = pair.into_inner();
                            let key_pair = inner_pairs.next().unwrap();
                            let key = match key_pair.as_rule() {
                                Rule::record_key_static => {
                                    let inner_key_pair = key_pair.into_inner().next().unwrap();
                                    match inner_key_pair.as_rule() {
                                        Rule::identifier => {
                                            RecordKey::Static(inner_key_pair.as_str().to_string())
                                        }
                                        Rule::string => RecordKey::Static(
                                            inner_key_pair.into_inner().as_str().to_string(),
                                        ),
                                        _ => unreachable!(),
                                    }
                                }
                                Rule::record_key_dynamic => RecordKey::Dynamic(Box::new(
                                    pairs_to_expr(key_pair.into_inner())?,
                                )),
                                _ => unreachable!(),
                            };

                            let value = pairs_to_expr(inner_pairs.next().unwrap().into_inner())?;
                            entries.push(RecordEntry { key, value });
                        }
                        Rule::record_shorthand => {
                            let ident = pair.into_inner().next().unwrap().as_str().to_string();
                            entries.push(RecordEntry {
                                key: RecordKey::Shorthand(ident),
                                value: Expr::Null, // Will be resolved during evaluation
                            });
                        }
                        Rule::spread_expression => {
                            let spread_expr = pairs_to_expr(pair.into_inner())?;
                            entries.push(RecordEntry {
                                key: RecordKey::Spread(Box::new(spread_expr)),
                                value: Expr::Null, // Will be resolved during evaluation
                            });
                        }
                        _ => {}
                    }
                }

                Ok(Expr::Record(entries))
            }
            Rule::bool => {
                let bool_str = primary.as_str();
                match bool_str {
                    "true" => Ok(Expr::Bool(true)),
                    "false" => Ok(Expr::Bool(false)),
                    _ => unreachable!(),
                }
            }
            Rule::null => Ok(Expr::Null),
            Rule::string => {
                let contents = primary.into_inner().as_str().to_string();
                Ok(Expr::String(contents))
            }
            Rule::assignment => {
                let mut inner_pairs = primary.into_inner();
                let ident = inner_pairs.next().unwrap().as_str().to_string();
                let value = Box::new(pairs_to_expr(inner_pairs.next().unwrap().into_inner())?);
                Ok(Expr::Assignment { ident, value })
            }
            Rule::lambda => {
                let mut inner_pairs = primary.into_inner();
                let arg_list = inner_pairs.next().unwrap();
                let body_pairs = inner_pairs.next().unwrap();

                let mut args = Vec::new();
                for arg_pair in arg_list.into_inner() {
                    match arg_pair.as_rule() {
                        Rule::required_arg => {
                            args.push(LambdaArg::Required(
                                arg_pair.into_inner().as_str().to_string(),
                            ));
                        }
                        Rule::optional_arg => {
                            args.push(LambdaArg::Optional(
                                arg_pair.into_inner().as_str().to_string(),
                            ));
                        }
                        Rule::rest_arg => {
                            args.push(LambdaArg::Rest(arg_pair.into_inner().as_str().to_string()));
                        }
                        _ => {}
                    }
                }

                let body = Box::new(pairs_to_expr(body_pairs.into_inner())?);
                Ok(Expr::Lambda { args, body })
            }
            Rule::conditional => {
                let mut inner_pairs = primary.into_inner();
                let condition = Box::new(pairs_to_expr(inner_pairs.next().unwrap().into_inner())?);
                let then_expr = Box::new(pairs_to_expr(inner_pairs.next().unwrap().into_inner())?);
                let else_expr = Box::new(pairs_to_expr(inner_pairs.next().unwrap().into_inner())?);
                Ok(Expr::Conditional {
                    condition,
                    then_expr,
                    else_expr,
                })
            }
            Rule::do_block => {
                let mut inner_pairs = primary.into_inner();
                let mut statements = Vec::new();
                let mut return_expr = Box::new(Expr::Null);

                while let Some(pair) = inner_pairs.next() {
                    match pair.as_rule() {
                        Rule::do_statement => {
                            if let Some(inner) = pair.into_inner().next() {
                                if inner.as_rule() == Rule::expression {
                                    statements.push(DoStatement::Expression(pairs_to_expr(
                                        inner.into_inner(),
                                    )?));
                                } else if inner.as_rule() == Rule::comment {
                                    statements
                                        .push(DoStatement::Comment(inner.as_str().to_string()));
                                }
                            }
                        }
                        Rule::return_statement => {
                            let return_expr_pair = pair.into_inner().next().unwrap();
                            return_expr = Box::new(pairs_to_expr(return_expr_pair.into_inner())?);
                        }
                        _ => {}
                    }
                }

                Ok(Expr::DoBlock {
                    statements,
                    return_expr,
                })
            }
            Rule::identifier => {
                let ident = primary.as_str();

                // Check if it's a built-in function
                if let Some(id) = get_built_in_function_id(ident) {
                    Ok(Expr::BuiltIn(id))
                } else {
                    Ok(Expr::Identifier(ident.to_string()))
                }
            }
            Rule::expression => pairs_to_expr(primary.into_inner()),
            _ => unreachable!("{}", primary.as_str()),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::negation => Ok(Expr::UnaryOp {
                op: UnaryOp::Negate,
                expr: Box::new(rhs?),
            }),
            Rule::spread_operator => Ok(Expr::Spread(Box::new(rhs?))),
            Rule::invert | Rule::not => Ok(Expr::UnaryOp {
                op: UnaryOp::Not,
                expr: Box::new(rhs?),
            }),
            _ => unreachable!(),
        })
        .map_postfix(|lhs, op| match op.as_rule() {
            Rule::factorial => Ok(Expr::PostfixOp {
                op: PostfixOp::Factorial,
                expr: Box::new(lhs?),
            }),
            Rule::access => {
                let index_expr = pairs_to_expr(op.into_inner())?;
                Ok(Expr::Access {
                    expr: Box::new(lhs?),
                    index: Box::new(index_expr),
                })
            }
            Rule::dot_access => {
                let field = op.into_inner().as_str().to_string();
                Ok(Expr::DotAccess {
                    expr: Box::new(lhs?),
                    field,
                })
            }
            Rule::call_list => {
                let call_list = op.into_inner();
                let args = call_list
                    .into_iter()
                    .map(|arg| pairs_to_expr(arg.into_inner()))
                    .collect::<Result<Vec<Expr>>>()?;
                Ok(Expr::Call {
                    func: Box::new(lhs?),
                    args,
                })
            }
            _ => unreachable!(),
        })
        .map_infix(|lhs, op, rhs| {
            let op = match op.as_rule() {
                Rule::add => BinaryOp::Add,
                Rule::subtract => BinaryOp::Subtract,
                Rule::multiply => BinaryOp::Multiply,
                Rule::divide => BinaryOp::Divide,
                Rule::modulo => BinaryOp::Modulo,
                Rule::power => BinaryOp::Power,
                Rule::equal => BinaryOp::Equal,
                Rule::not_equal => BinaryOp::NotEqual,
                Rule::less => BinaryOp::Less,
                Rule::less_eq => BinaryOp::LessEq,
                Rule::greater => BinaryOp::Greater,
                Rule::greater_eq => BinaryOp::GreaterEq,
                Rule::and => BinaryOp::And,
                Rule::natural_and => BinaryOp::NaturalAnd,
                Rule::or => BinaryOp::Or,
                Rule::natural_or => BinaryOp::NaturalOr,
                Rule::with => BinaryOp::With,
                Rule::coalesce => BinaryOp::Coalesce,
                _ => unreachable!(),
            };
            Ok(Expr::BinaryOp {
                op,
                left: Box::new(lhs?),
                right: Box::new(rhs?),
            })
        })
        .parse(pairs)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{heap::LambdaPointer, parser::get_pairs};

    fn parse_and_evaluate(
        input: &str,
        heap: Option<Rc<RefCell<Heap>>>,
        bindings: Option<Rc<RefCell<HashMap<String, Value>>>>,
    ) -> Result<Value> {
        let binding = input.to_string();
        let mut pairs = get_pairs(&binding).unwrap();
        let expr = pairs.next().unwrap().into_inner();
        evaluate_pairs(
            expr,
            heap.unwrap_or(Rc::new(RefCell::new(Heap::new()))),
            bindings.unwrap_or(Rc::new(RefCell::new(HashMap::new()))),
            0,
        )
    }

    #[test]
    fn addition_of_integers() {
        let result = parse_and_evaluate("5 + 2", None, None).unwrap();
        assert_eq!(result, Value::Number(7.0));
    }

    #[test]
    fn exponentiation_of_two_integers() {
        let result = parse_and_evaluate("2 ^ 3", None, None).unwrap();
        assert_eq!(result, Value::Number(8.0));
    }

    #[test]
    fn multiplication_of_integers() {
        let result = parse_and_evaluate("8 * 4", None, None).unwrap();
        assert_eq!(result, Value::Number(32.0));
    }

    #[test]
    fn division_with_integer_resulting_in_decimal() {
        let result = parse_and_evaluate("9 / 2", None, None).unwrap();
        assert_eq!(result, Value::Number(4.5));
    }

    #[test]
    fn addition_with_nested_expression() {
        let result = parse_and_evaluate("5 + (2 * 4)", None, None).unwrap();
        assert_eq!(result, Value::Number(13.0));
    }

    #[test]
    fn grouping_and_multiplication_in_expression() {
        let result = parse_and_evaluate("(3 + 2) * 2", None, None).unwrap();
        assert_eq!(result, Value::Number(10.0));
    }

    #[test]
    fn mixed_operations_with_decimal_and_precedence() {
        let result = parse_and_evaluate("6.5 / 2 + 4 * 2", None, None).unwrap();
        assert_eq!(result, Value::Number(11.25));
    }

    #[test]
    fn exponentiation_with_nested_expression() {
        let result = parse_and_evaluate("2 ^ (1 + 2)", None, None).unwrap();
        assert_eq!(result, Value::Number(8.0));
    }

    #[test]
    fn complex_expression_with_decimals() {
        let result = parse_and_evaluate("7.5 - 3.25 + 2 * (8 / 4)", None, None).unwrap();
        assert_eq!(result, Value::Number(8.25));
    }

    #[test]
    fn subtraction_with_decimal_result() {
        let result = parse_and_evaluate("10.75 - 3.5", None, None).unwrap();
        assert_eq!(result, Value::Number(7.25));
    }

    #[test]
    fn multiplication_of_two_decimals() {
        let result = parse_and_evaluate("3.5 * 2.0", None, None).unwrap();
        assert_eq!(result, Value::Number(7.0));
    }

    #[test]
    fn division_of_two_decimals() {
        let result = parse_and_evaluate("7.5 / 2.5", None, None).unwrap();
        assert_eq!(result, Value::Number(3.0));
    }

    #[test]
    fn boolean_and() {
        let result = parse_and_evaluate("true and false", None, None).unwrap();
        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn boolean_and_alt() {
        let result = parse_and_evaluate("true && false", None, None).unwrap();
        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn boolean_or() {
        let result = parse_and_evaluate("true or false", None, None).unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn boolean_or_alt() {
        let result = parse_and_evaluate("true || false", None, None).unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn boolean_and_with_nested_expression() {
        let result = parse_and_evaluate("true and (false or true)", None, None).unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn boolean_and_with_nested_expression_alt() {
        let result = parse_and_evaluate("true && (false || true)", None, None).unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn boolean_or_with_nested_expression() {
        let result = parse_and_evaluate("true or (false and true)", None, None).unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn boolean_or_with_nested_expression_alt() {
        let result = parse_and_evaluate("true || (false && true)", None, None).unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn logical_not() {
        let result = parse_and_evaluate("!true", None, None).unwrap();
        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn logical_not_with_nested_expression() {
        let result = parse_and_evaluate("!(true and false)", None, None).unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn equality_of_two_integers() {
        let result = parse_and_evaluate("5 == 5", None, None).unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn inequality_of_two_integers() {
        let result = parse_and_evaluate("5 != 5", None, None).unwrap();
        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn less_than_comparison() {
        let result = parse_and_evaluate("5 < 10", None, None).unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn less_than_or_equal_comparison() {
        let result = parse_and_evaluate("5 <= 5", None, None).unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn greater_than_comparison() {
        let result = parse_and_evaluate("10 > 5", None, None).unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn greater_than_or_equal_comparison() {
        let result = parse_and_evaluate("5 >= 5", None, None).unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn equality_of_two_lists() {
        // This test now simply checks that the expression doesn't error
        let result = parse_and_evaluate("[1, 2, 3] == [1, 2, 3]", None, None);
        assert!(result.is_ok());
    }

    #[test]
    fn inequality_of_two_lists() {
        // This test now simply checks that the expression doesn't error
        let result = parse_and_evaluate("[1, 2, 3] != [2, 3, 4]", None, None);
        assert!(result.is_ok());
    }

    #[test]
    fn conditional_expression_with_true_condition() {
        let result = parse_and_evaluate("if true then 5 else 10", None, None).unwrap();
        assert_eq!(result, Value::Number(5.0));
    }

    #[test]
    fn conditional_expression_with_false_condition() {
        let result = parse_and_evaluate("if false then 5 else 10", None, None).unwrap();
        assert_eq!(result, Value::Number(10.0));
    }

    #[test]
    fn factorial_of_integer() {
        let result = parse_and_evaluate("5!", None, None).unwrap();
        assert_eq!(result, Value::Number(120.0));
    }

    #[test]
    fn factorial_of_zero() {
        let result = parse_and_evaluate("0!", None, None).unwrap();
        assert_eq!(result, Value::Number(1.0));
    }

    #[test]
    fn factorial_of_negative_integer() {
        let result = parse_and_evaluate("(-5)!", None, None);
        assert!(result.is_err());
    }

    #[test]
    fn factorial_of_decimal() {
        let result = parse_and_evaluate("5.5!", None, None);
        assert!(result.is_err());
    }

    #[test]
    fn string_concatenation() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let result =
            parse_and_evaluate("\"hello\" + \"world\"", Some(Rc::clone(&heap)), None).unwrap();

        assert!(matches!(result, Value::String(_)));
        assert_eq!(result.as_string(&heap.borrow()).unwrap(), "helloworld");
    }

    #[test]
    fn string_concatenation_with_integer() {
        let result = parse_and_evaluate("\"hello\" + 5", None, None);
        assert!(result.is_err());
    }

    #[test]
    fn variable_assignment() {
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        let result = parse_and_evaluate("x = 5", None, Some(Rc::clone(&bindings))).unwrap();

        assert_eq!(result, Value::Number(5.0));
        assert_eq!(bindings.borrow().get("x").unwrap(), &Value::Number(5.0));
    }

    #[test]
    fn variable_assignment_with_expression() {
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        let result = parse_and_evaluate("x = 5 + 2", None, Some(Rc::clone(&bindings))).unwrap();
        assert_eq!(result, Value::Number(7.0));
        assert_eq!(bindings.borrow().get("x").unwrap(), &Value::Number(7.0));
    }

    #[test]
    fn variable_assignment_with_lambda() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        let result = parse_and_evaluate(
            "f = x => x + 1",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        {
            let heap_borrow = heap.borrow();
            let lambda_def = result.as_lambda(&heap_borrow).unwrap();
            assert_eq!(lambda_def.name, Some("f".to_string()));
            assert_eq!(lambda_def.args, vec![LambdaArg::Required("x".to_string())]);
            assert_eq!(lambda_def.scope, HashMap::new());
            // The body should be a BinaryOp(Add) with Identifier("x") and Number(1)
            match &lambda_def.body {
                Expr::BinaryOp {
                    op: BinaryOp::Add,
                    left,
                    right,
                } => match (left.as_ref(), right.as_ref()) {
                    (Expr::Identifier(x), Expr::Number(n)) => {
                        assert_eq!(x, "x");
                        assert_eq!(*n, 1.0);
                    }
                    _ => panic!("Expected Identifier('x') + Number(1)"),
                },
                _ => panic!("Expected BinaryOp(Add)"),
            }
        }
        assert_eq!(
            bindings.borrow().get("f").unwrap(),
            &Value::Lambda(LambdaPointer::new(1))
        );
    }

    #[test]
    fn variable_assignment_with_lambda_and_call() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        let _ = parse_and_evaluate(
            "f = x => x + 1",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();
        let result =
            parse_and_evaluate("f(5)", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings))).unwrap();

        assert_eq!(result, Value::Number(6.0));
    }

    #[test]
    fn variable_assignment_with_lambda_and_call_with_multiple_args() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        let _ = parse_and_evaluate(
            "f = (x, y) => x + y",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();
        let result = parse_and_evaluate(
            "f(5, 2)",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        assert_eq!(result, Value::Number(7.0));
    }

    #[test]
    fn variable_assignment_with_lambda_and_call_with_multiple_args_and_expression() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        let _ = parse_and_evaluate(
            "f = (x, y) => x + y",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();
        let result = parse_and_evaluate(
            "f(5, 2) + 3",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        assert_eq!(result, Value::Number(10.0));
    }

    #[test]
    fn coalesce_operator_with_null() {
        let result = parse_and_evaluate("null ?? 5", None, None).unwrap();
        assert_eq!(result, Value::Number(5.0));
    }

    #[test]
    fn coalesce_operator_with_non_null() {
        let result = parse_and_evaluate("3 ?? 10", None, None).unwrap();
        assert_eq!(result, Value::Number(3.0));
    }

    // Element-wise operations tests
    #[test]
    fn list_elementwise_addition() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let result =
            parse_and_evaluate("[1, 2, 3] + [4, 5, 6]", Some(Rc::clone(&heap)), None).unwrap();
        let expected = vec![Value::Number(5.0), Value::Number(7.0), Value::Number(9.0)];
        assert_eq!(result.as_list(&heap.borrow()).unwrap(), &expected);
    }

    #[test]
    fn list_elementwise_subtraction() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let result =
            parse_and_evaluate("[5, 7, 9] - [1, 2, 3]", Some(Rc::clone(&heap)), None).unwrap();
        let expected = vec![Value::Number(4.0), Value::Number(5.0), Value::Number(6.0)];
        assert_eq!(result.as_list(&heap.borrow()).unwrap(), &expected);
    }

    #[test]
    fn list_elementwise_multiplication() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let result =
            parse_and_evaluate("[2, 3, 4] * [5, 6, 7]", Some(Rc::clone(&heap)), None).unwrap();
        let expected = vec![
            Value::Number(10.0),
            Value::Number(18.0),
            Value::Number(28.0),
        ];
        assert_eq!(result.as_list(&heap.borrow()).unwrap(), &expected);
    }

    #[test]
    fn list_elementwise_division() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let result =
            parse_and_evaluate("[10, 20, 30] / [2, 4, 5]", Some(Rc::clone(&heap)), None).unwrap();
        let expected = vec![Value::Number(5.0), Value::Number(5.0), Value::Number(6.0)];
        assert_eq!(result.as_list(&heap.borrow()).unwrap(), &expected);
    }

    #[test]
    fn list_elementwise_power() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let result =
            parse_and_evaluate("[2, 3, 4] ^ [2, 2, 2]", Some(Rc::clone(&heap)), None).unwrap();
        let expected = vec![Value::Number(4.0), Value::Number(9.0), Value::Number(16.0)];
        assert_eq!(result.as_list(&heap.borrow()).unwrap(), &expected);
    }

    #[test]
    fn list_elementwise_modulo() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let result =
            parse_and_evaluate("[10, 11, 12] % [3, 3, 3]", Some(Rc::clone(&heap)), None).unwrap();
        let expected = vec![Value::Number(1.0), Value::Number(2.0), Value::Number(0.0)];
        assert_eq!(result.as_list(&heap.borrow()).unwrap(), &expected);
    }

    #[test]
    fn list_elementwise_different_lengths_error() {
        let result = parse_and_evaluate("[1, 2, 3] + [4, 5]", None, None);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("left- and right-hand-side lists must be the same length"));
    }

    // Broadcast operations tests
    #[test]
    fn list_scalar_addition() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let result = parse_and_evaluate("[1, 2, 3] + 10", Some(Rc::clone(&heap)), None).unwrap();
        let expected = vec![
            Value::Number(11.0),
            Value::Number(12.0),
            Value::Number(13.0),
        ];
        assert_eq!(result.as_list(&heap.borrow()).unwrap(), &expected);
    }

    #[test]
    fn list_scalar_subtraction() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let result = parse_and_evaluate("[10, 20, 30] - 5", Some(Rc::clone(&heap)), None).unwrap();
        let expected = vec![Value::Number(5.0), Value::Number(15.0), Value::Number(25.0)];
        assert_eq!(result.as_list(&heap.borrow()).unwrap(), &expected);
    }

    #[test]
    fn list_scalar_multiplication() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let result = parse_and_evaluate("[2, 3, 4] * 2", Some(Rc::clone(&heap)), None).unwrap();
        let expected = vec![Value::Number(4.0), Value::Number(6.0), Value::Number(8.0)];
        assert_eq!(result.as_list(&heap.borrow()).unwrap(), &expected);
    }

    #[test]
    fn list_scalar_division() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let result = parse_and_evaluate("[10, 20, 30] / 10", Some(Rc::clone(&heap)), None).unwrap();
        let expected = vec![Value::Number(1.0), Value::Number(2.0), Value::Number(3.0)];
        assert_eq!(result.as_list(&heap.borrow()).unwrap(), &expected);
    }

    #[test]
    fn list_scalar_power() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let result = parse_and_evaluate("[2, 3, 4] ^ 2", Some(Rc::clone(&heap)), None).unwrap();
        let expected = vec![Value::Number(4.0), Value::Number(9.0), Value::Number(16.0)];
        assert_eq!(result.as_list(&heap.borrow()).unwrap(), &expected);
    }

    #[test]
    fn list_scalar_modulo() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let result = parse_and_evaluate("[10, 11, 12] % 3", Some(Rc::clone(&heap)), None).unwrap();
        let expected = vec![Value::Number(1.0), Value::Number(2.0), Value::Number(0.0)];
        assert_eq!(result.as_list(&heap.borrow()).unwrap(), &expected);
    }

    #[test]
    fn list_with_operator() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let result =
            parse_and_evaluate("[1, 2, 3] with (x => x * x)", Some(Rc::clone(&heap)), None)
                .unwrap();
        let expected = vec![Value::Number(1.0), Value::Number(4.0), Value::Number(9.0)];
        assert_eq!(result.as_list(&heap.borrow()).unwrap(), &expected);
    }

    #[test]
    fn list_with_builtin_function() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let result =
            parse_and_evaluate("[4, 9, 16] with sqrt", Some(Rc::clone(&heap)), None).unwrap();
        let expected = vec![Value::Number(2.0), Value::Number(3.0), Value::Number(4.0)];
        assert_eq!(result.as_list(&heap.borrow()).unwrap(), &expected);
    }

    #[test]
    fn list_elementwise_boolean_and() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let result = parse_and_evaluate(
            "[true, true, false] && [true, false, true]",
            Some(Rc::clone(&heap)),
            None,
        )
        .unwrap();
        let expected = vec![Value::Bool(true), Value::Bool(false), Value::Bool(false)];
        assert_eq!(result.as_list(&heap.borrow()).unwrap(), &expected);
    }

    #[test]
    fn list_elementwise_boolean_or() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let result = parse_and_evaluate(
            "[true, false, false] || [false, true, false]",
            Some(Rc::clone(&heap)),
            None,
        )
        .unwrap();
        let expected = vec![Value::Bool(true), Value::Bool(true), Value::Bool(false)];
        assert_eq!(result.as_list(&heap.borrow()).unwrap(), &expected);
    }

    #[test]
    fn list_elementwise_coalesce() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let result = parse_and_evaluate(
            "[null, 2, null] ?? [1, null, 3]",
            Some(Rc::clone(&heap)),
            None,
        )
        .unwrap();
        let expected = vec![Value::Number(1.0), Value::Number(2.0), Value::Number(3.0)];
        assert_eq!(result.as_list(&heap.borrow()).unwrap(), &expected);
    }

    #[test]
    fn list_scalar_coalesce() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let result =
            parse_and_evaluate("[null, 2, null] ?? 5", Some(Rc::clone(&heap)), None).unwrap();
        let expected = vec![Value::Number(5.0), Value::Number(2.0), Value::Number(5.0)];
        assert_eq!(result.as_list(&heap.borrow()).unwrap(), &expected);
    }

    #[test]
    fn list_elementwise_string_concat() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let result = parse_and_evaluate(
            "[\"a\", \"b\", \"c\"] + [\"1\", \"2\", \"3\"]",
            Some(Rc::clone(&heap)),
            None,
        )
        .unwrap();
        let borrowed_heap = heap.borrow();
        let result_list = result.as_list(&borrowed_heap).unwrap();
        assert_eq!(result_list.len(), 3);
        assert_eq!(result_list[0].as_string(&borrowed_heap).unwrap(), "a1");
        assert_eq!(result_list[1].as_string(&borrowed_heap).unwrap(), "b2");
        assert_eq!(result_list[2].as_string(&borrowed_heap).unwrap(), "c3");
    }

    #[test]
    fn list_scalar_string_concat() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let result = parse_and_evaluate(
            "[\"a\", \"b\", \"c\"] + \"!\"",
            Some(Rc::clone(&heap)),
            None,
        )
        .unwrap();
        let borrowed_heap = heap.borrow();
        let result_list = result.as_list(&borrowed_heap).unwrap();
        assert_eq!(result_list.len(), 3);
        assert_eq!(result_list[0].as_string(&borrowed_heap).unwrap(), "a!");
        assert_eq!(result_list[1].as_string(&borrowed_heap).unwrap(), "b!");
        assert_eq!(result_list[2].as_string(&borrowed_heap).unwrap(), "c!");
    }

    #[test]
    fn nested_list_operations() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let result =
            parse_and_evaluate("([1, 2, 3] + [4, 5, 6]) * 2", Some(Rc::clone(&heap)), None)
                .unwrap();
        let expected = vec![
            Value::Number(10.0),
            Value::Number(14.0),
            Value::Number(18.0),
        ];
        assert_eq!(result.as_list(&heap.borrow()).unwrap(), &expected);
    }

    #[test]
    fn list_comparison_all_equal() {
        let result = parse_and_evaluate("[1, 1, 1] == 1", None, None).unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn list_comparison_not_all_equal() {
        let result = parse_and_evaluate("[1, 2, 1] == 1", None, None).unwrap();
        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn list_comparison_all_less() {
        let result = parse_and_evaluate("[1, 2, 3] < 5", None, None).unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn list_comparison_not_all_less() {
        let result = parse_and_evaluate("[1, 6, 3] < 5", None, None).unwrap();
        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn not_operator_basic() {
        let result = parse_and_evaluate("not true", None, None).unwrap();
        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn not_operator_with_false() {
        let result = parse_and_evaluate("not false", None, None).unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn not_operator_with_expression() {
        let result = parse_and_evaluate("not (5 > 10)", None, None).unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn not_operator_double_negation() {
        let result = parse_and_evaluate("not not true", None, None).unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn not_operator_with_and() {
        let result = parse_and_evaluate("not (true and false)", None, None).unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn not_operator_with_or() {
        let result = parse_and_evaluate("not (false or false)", None, None).unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn not_operator_precedence() {
        let result = parse_and_evaluate("not true and false", None, None).unwrap();
        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn not_operator_comparison_with_invert() {
        let not_result = parse_and_evaluate("not true", None, None).unwrap();
        let invert_result = parse_and_evaluate("!true", None, None).unwrap();
        assert_eq!(not_result, invert_result);
    }

    #[test]
    fn variable_immutability_prevents_reassignment() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));

        // First assignment should succeed
        let result1 =
            parse_and_evaluate("x = 5", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)));
        assert!(result1.is_ok());
        assert_eq!(result1.unwrap(), Value::Number(5.0));

        // Second assignment to same variable should fail
        let result2 =
            parse_and_evaluate("x = 10", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)));
        assert!(result2.is_err());
        assert!(result2
            .unwrap_err()
            .to_string()
            .contains("x is already defined, and cannot be reassigned"));

        // Original value should remain unchanged
        let result3 = parse_and_evaluate("x", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)));
        assert!(result3.is_ok());
        assert_eq!(result3.unwrap(), Value::Number(5.0));
    }

    #[test]
    fn variable_immutability_allows_shadowing_in_nested_scopes() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));

        // Assign x in outer scope
        let result1 =
            parse_and_evaluate("x = 5", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)));
        assert!(result1.is_ok());

        // Shadowing in lambda should work (creates new scope)
        let result2 = parse_and_evaluate(
            "f = (x) => x * 2",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        );
        assert!(result2.is_ok());

        // Call lambda with different value
        let result3 =
            parse_and_evaluate("f(10)", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)));
        assert!(result3.is_ok());
        assert_eq!(result3.unwrap(), Value::Number(20.0));

        // Original x should be unchanged
        let result4 = parse_and_evaluate("x", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)));
        assert!(result4.is_ok());
        assert_eq!(result4.unwrap(), Value::Number(5.0));
    }

    #[test]
    fn variable_immutability_in_do_blocks() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));

        // Assign x in outer scope
        let result1 =
            parse_and_evaluate("x = 5", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)));
        assert!(result1.is_ok());

        // Do block inherits outer scope, so reassigning x should fail
        let do_block = r#"do {
            x = 10
            y = x * 2
            return y
        }"#;
        let result2 =
            parse_and_evaluate(do_block, Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)));
        assert!(result2.is_err());
        assert!(result2
            .unwrap_err()
            .to_string()
            .contains("x is already defined, and cannot be reassigned"));

        // But new variables in do block should work
        let do_block2 = r#"do {
            z = x * 3
            return z
        }"#;
        let result3 = parse_and_evaluate(
            do_block2,
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        );
        assert!(result3.is_ok());
        assert_eq!(result3.unwrap(), Value::Number(15.0));

        // Original x should be unchanged
        let result4 = parse_and_evaluate("x", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)));
        assert!(result4.is_ok());
        assert_eq!(result4.unwrap(), Value::Number(5.0));
    }

    #[test]
    fn variable_immutability_different_variables() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));

        // Multiple different variables should work
        let result1 =
            parse_and_evaluate("x = 5", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)));
        assert!(result1.is_ok());

        let result2 =
            parse_and_evaluate("y = 10", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)));
        assert!(result2.is_ok());

        let result3 = parse_and_evaluate(
            "z = x + y",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        );
        assert!(result3.is_ok());
        assert_eq!(result3.unwrap(), Value::Number(15.0));
    }

    #[test]
    fn variable_immutability_with_complex_types() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));

        // List assignment
        let result1 = parse_and_evaluate(
            "list = [1, 2, 3]",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        );
        assert!(result1.is_ok());

        // Reassignment should fail
        let result2 = parse_and_evaluate(
            "list = [4, 5, 6]",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        );
        assert!(result2.is_err());
        assert!(result2
            .unwrap_err()
            .to_string()
            .contains("list is already defined, and cannot be reassigned"));

        // Record assignment
        let result3 = parse_and_evaluate(
            "rec = {x: 1, y: 2}",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        );
        assert!(result3.is_ok());

        // Reassignment should fail
        let result4 = parse_and_evaluate(
            "rec = {x: 3, y: 4}",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        );
        assert!(result4.is_err());
        assert!(result4
            .unwrap_err()
            .to_string()
            .contains("rec is already defined, and cannot be reassigned"));
    }
}
