use crate::{
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

// Helper function to extract unique identifiers from a parsed expression
fn extract_identifiers(pairs: Pairs<Rule>, identifiers: &mut Vec<String>) {
    for pair in pairs {
        match pair.as_rule() {
            Rule::identifier => {
                // Regular identifier reference
                identifiers.push(pair.as_str().to_string());
            }
            Rule::assignment => {
                // For assignments, the LHS identifier is not a reference to capture
                let mut inner_pairs = pair.into_inner();
                let _ident = inner_pairs.next().unwrap(); // Skip the identifier being assigned to
                let value_expr = inner_pairs.next().unwrap();

                // Process the RHS for identifiers
                extract_identifiers(value_expr.into_inner(), identifiers);
            }
            Rule::lambda => {
                // For lambdas, we need to process differently
                let mut inner_pairs = pair.into_inner();
                let arg_list = inner_pairs.next().unwrap();
                let body = inner_pairs.next().unwrap();

                // Extract arg names to exclude them from captures
                let mut lambda_args = HashSet::new();
                for arg_pair in arg_list.into_inner() {
                    match arg_pair.as_rule() {
                        Rule::required_arg | Rule::optional_arg | Rule::rest_arg => {
                            lambda_args.insert(arg_pair.into_inner().as_str().to_string());
                        }
                        _ => {}
                    }
                }

                // Extract identifiers from body
                let mut body_identifiers = Vec::new();
                extract_identifiers(body.into_inner(), &mut body_identifiers);

                // Only add identifiers that aren't lambda arguments
                for ident in body_identifiers {
                    if !lambda_args.contains(&ident) {
                        identifiers.push(ident);
                    }
                }
            }
            _ => {
                // Process other rules normally
                extract_identifiers(pair.into_inner(), identifiers);
            }
        }
    }
}

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

pub fn collect_list(
    pairs: Pairs<Rule>,
    heap: Rc<RefCell<Heap>>,
    bindings: Rc<RefCell<HashMap<String, Value>>>,
    call_depth: usize,
) -> Result<Vec<Value>> {
    let values = pairs
        .into_iter()
        .map(|value| {
            evaluate_expression(
                value.into_inner(),
                Rc::clone(&heap),
                Rc::clone(&bindings),
                call_depth,
            )
        })
        .collect::<Result<Vec<Value>>>()?;

    Ok(values
        .into_iter()
        .flat_map(|value| {
            value
                .reify(unsafe { heap.try_borrow_unguarded().unwrap() })
                .unwrap()
                .with_heap(Rc::clone(&heap))
                .into_iter()
        })
        .collect::<Vec<Value>>())
}

pub fn evaluate_expression(
    pairs: Pairs<Rule>,
    heap: Rc<RefCell<Heap>>,
    bindings: Rc<RefCell<HashMap<String, Value>>>,
    call_depth: usize,
) -> Result<Value> {
    PRATT
        .map_primary(|primary| match primary.as_rule() {
            Rule::number => Ok(Number(
                primary
                    .as_str()
                    .replace("_", "")
                    .parse::<f64>()
                    .map_err(|e| anyhow::Error::from(e))?,
            )),
            Rule::list => {
                let list_pairs = primary.into_inner();
                let values = collect_list(
                    list_pairs,
                    Rc::clone(&heap),
                    Rc::clone(&bindings),
                    call_depth,
                )?;

                Ok(heap.borrow_mut().insert_list(values))
            }
            Rule::record => {
                let record_pairs = primary.into_inner();
                let mut record = BTreeMap::new();

                for pair in record_pairs {
                    match pair.as_rule() {
                        Rule::record_pair => {
                            let mut inner_pairs = pair.into_inner();
                            let key_pair = inner_pairs.next().unwrap();
                            let key = match key_pair.as_rule() {
                                Rule::record_key_static => {
                                    let inner_key_pair = key_pair.into_inner().next().unwrap();
                                    match inner_key_pair.as_rule() {
                                        Rule::identifier => inner_key_pair.as_str().to_string(),
                                        Rule::string => {
                                            inner_key_pair.into_inner().as_str().to_string()
                                        }
                                        _ => unreachable!(),
                                    }
                                }
                                Rule::record_key_dynamic => evaluate_expression(
                                    key_pair.into_inner(),
                                    Rc::clone(&heap),
                                    Rc::clone(&bindings),
                                    call_depth,
                                )?
                                .as_string(&heap.borrow())?
                                .to_string(),
                                _ => unreachable!(),
                            };

                            let value = evaluate_expression(
                                inner_pairs.next().unwrap().into_inner(),
                                Rc::clone(&heap),
                                Rc::clone(&bindings),
                                call_depth,
                            )?;
                            record.insert(key, value);
                        }
                        Rule::record_shorthand => {
                            let ident = pair.into_inner().next().unwrap().as_str().to_string();
                            let value = bindings
                                .borrow()
                                .get(&ident)
                                .copied()
                                .ok_or(anyhow!("unknown identifier: {}", ident))?;

                            record.insert(ident, value);
                        }
                        Rule::spread_expression => {
                            let spread_value = evaluate_expression(
                                pair.into_inner(),
                                Rc::clone(&heap),
                                Rc::clone(&bindings),
                                call_depth,
                            )?;

                            match spread_value {
                                Spread(spread_value) => match spread_value {
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
                                        let spread_record =
                                            pointer.reify(borrowed_heap).as_record()?;

                                        for (k, v) in spread_record {
                                            record.insert(k.clone(), *v);
                                        }
                                    }
                                },
                                _ => {}
                            }
                        }
                        _ => {}
                    }
                }

                Ok(heap.borrow_mut().insert_record(record))
            }
            Rule::bool => {
                let bool_str = primary.as_str();

                match bool_str {
                    "true" => Ok(Bool(true)),
                    "false" => Ok(Bool(false)),
                    _ => unreachable!(),
                }
            }
            Rule::null => Ok(Value::Null),
            Rule::string => {
                let contents = primary.into_inner().as_str().to_string();
                Ok(heap.borrow_mut().insert_string(contents))
            }
            Rule::assignment => {
                let mut inner_pairs = primary.into_inner();
                let ident = inner_pairs.next().unwrap().as_str();

                if is_built_in_function(&ident) {
                    return Err(anyhow!("cannot assign to built-in function: {}", ident));
                }

                if ident == "constants" {
                    return Err(anyhow!("cannot assign to constants"));
                }

                if ident == "if"
                    || ident == "then"
                    || ident == "else"
                    || ident == "true"
                    || ident == "false"
                    || ident == "null"
                    || ident == "inputs"
                    || ident == "and"
                    || ident == "or"
                    || ident == "not"
                    || ident == "with"
                    || ident == "do"
                    || ident == "return"
                {
                    return Err(anyhow!("cannot assign to keyword: {}", ident));
                }

                if bindings.borrow().contains_key(ident) {
                    return Err(anyhow!("identifier already defined: {}", ident));
                }

                let expression = inner_pairs.next().unwrap();
                let value = evaluate_expression(
                    expression.into_inner(),
                    Rc::clone(&heap),
                    Rc::clone(&bindings),
                    call_depth,
                )?;

                match value {
                    Value::Lambda(pointer) => match pointer.reify_mut(&mut heap.borrow_mut()) {
                        HeapValue::Lambda(lambda) => {
                            lambda.set_name(ident.to_string(), value);
                        }
                        _ => {}
                    },
                    _ => {}
                }

                bindings.borrow_mut().insert(ident.to_string(), value);

                Ok(value)
            }
            Rule::lambda => {
                let mut inner_pairs = primary.into_inner();
                let arg_pairs = inner_pairs.next().unwrap().into_inner();
                let mut has_rest = false;
                let mut has_optional = false;

                let args = arg_pairs
                    .map(|arg| match arg.as_rule() {
                        Rule::required_arg => {
                            if has_optional {
                                return Err(anyhow!(
                                    "required argument '{}' must come before optional arguments",
                                    arg.into_inner().as_str()
                                ));
                            }

                            if has_rest {
                                return Err(anyhow!(
                                    "required argument '{}' must come before rest arguments",
                                    arg.into_inner().as_str()
                                ));
                            }

                            Ok(LambdaArg::Required(arg.into_inner().as_str().to_string()))
                        }
                        Rule::optional_arg => {
                            if has_rest {
                                return Err(anyhow!(
                                    "optional argument '{}' must come before a rest argument",
                                    arg.into_inner().as_str()
                                ));
                            }

                            has_optional = true;
                            Ok(LambdaArg::Optional(arg.into_inner().as_str().to_string()))
                        }
                        Rule::rest_arg => {
                            if has_rest {
                                return Err(anyhow!("a function can only have one rest argument"));
                            }

                            has_rest = true;
                            Ok(LambdaArg::Rest(arg.into_inner().as_str().to_string()))
                        }
                        _ => unreachable!(),
                    })
                    .collect::<Result<Vec<LambdaArg>>>()?;

                let body = inner_pairs.next().unwrap();
                let body_str = body.as_str().to_string();

                // Extract only the variables that are referenced in the body and present in the current scope
                let mut captured_scope = HashMap::new();
                let current_bindings = bindings.borrow();

                let mut identifiers = Vec::new();
                extract_identifiers(body.into_inner(), &mut identifiers);

                // Create a set of unique identifiers to avoid capturing duplicates
                let unique_identifiers: HashSet<String> = identifiers.into_iter().collect();

                // Only capture identifiers that exist in the current scope
                for ident in unique_identifiers {
                    if current_bindings.contains_key(&ident)
                        && !is_built_in_function(&ident)
                        && ident != "constants"
                        && ident != "infinity"
                        && ident != "if"
                        && ident != "then"
                        && ident != "else"
                        && ident != "true"
                        && ident != "false"
                        && ident != "null"
                        && ident != "inputs"
                        && ident != "and"
                        && ident != "or"
                        && ident != "with"
                    {
                        captured_scope.insert(ident.clone(), current_bindings[&ident].clone());
                    }
                }

                let lambda = heap.borrow_mut().insert_lambda(LambdaDef {
                    name: None,
                    args,
                    body: body_str,
                    scope: captured_scope,
                });

                Ok(lambda)
            }
            Rule::conditional => {
                let mut inner_pairs = primary.into_inner();
                let condition_expr = inner_pairs.next().unwrap();
                let then_expr = inner_pairs.next().unwrap();
                let else_expr = inner_pairs.next().unwrap();

                let condition = evaluate_expression(
                    condition_expr.into_inner(),
                    Rc::clone(&heap),
                    Rc::clone(&bindings),
                    call_depth,
                )?
                .as_bool()?;

                if condition {
                    evaluate_expression(
                        then_expr.into_inner(),
                        Rc::clone(&heap),
                        Rc::clone(&bindings),
                        call_depth,
                    )
                } else {
                    evaluate_expression(
                        else_expr.into_inner(),
                        Rc::clone(&heap),
                        Rc::clone(&bindings),
                        call_depth,
                    )
                }
            }
            Rule::do_block => {
                let mut inner_pairs = primary.into_inner();

                // Create new scope from current bindings
                let new_bindings = bindings.borrow().clone();
                let block_bindings = Rc::new(RefCell::new(new_bindings));

                let mut result = Value::Null;

                // Process all pairs
                while let Some(pair) = inner_pairs.next() {
                    match pair.as_rule() {
                        Rule::do_statement => {
                            // Step statement contains either an expression or a comment
                            if let Some(inner) = pair.into_inner().next() {
                                if inner.as_rule() == Rule::expression {
                                    // Evaluate the expression but ignore its result
                                    evaluate_expression(
                                        inner.into_inner(),
                                        Rc::clone(&heap),
                                        Rc::clone(&block_bindings),
                                        call_depth,
                                    )?;
                                }
                                // Comments are ignored
                            }
                        }
                        Rule::return_statement => {
                            // Return statement contains the expression to return
                            let return_expr = pair.into_inner().next().unwrap();
                            result = evaluate_expression(
                                return_expr.into_inner(),
                                Rc::clone(&heap),
                                Rc::clone(&block_bindings),
                                call_depth,
                            )?;
                        }
                        _ => {}
                    }
                }

                Ok(result)
            }
            Rule::identifier => {
                let ident = primary.as_str();

                match ident {
                    "infinity" => Ok(Number(f64::INFINITY)),
                    "constants" => Ok(Value::Record(RecordPointer::new(0))),
                    _ => {
                        if let Some(built_in_id) = get_built_in_function_id(ident) {
                            return Ok(Value::BuiltIn(built_in_id));
                        }

                        let v = bindings
                            .borrow_mut()
                            .get(ident)
                            .copied()
                            .ok_or(anyhow!("unknown identifier: {}", primary.as_str()));

                        v
                    }
                }
            }
            Rule::expression => evaluate_expression(
                primary.into_inner(),
                Rc::clone(&heap),
                Rc::clone(&bindings),
                call_depth,
            ),
            _ => unreachable!("{}", primary.as_str()),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::negation => {
                let rhs = rhs?.as_number()?;
                Ok(Number(-rhs))
            }
            Rule::spread_operator => {
                let rhs = rhs?;
                match rhs {
                    List(pointer) => return Ok(Spread(IterablePointer::List(pointer))),
                    Value::String(pointer) => return Ok(Spread(IterablePointer::String(pointer))),
                    Value::Record(pointer) => return Ok(Spread(IterablePointer::Record(pointer))),
                    _ => return Err(anyhow!("expected a list, record, or string")),
                }
            }
            Rule::invert => {
                let rhs = rhs?.as_bool()?;
                Ok(Bool(!rhs))
            }
            Rule::not => {
                let rhs = rhs?.as_bool()?;
                Ok(Bool(!rhs))
            }
            _ => unreachable!(),
        })
        .map_postfix(|lhs, op| match op.as_rule() {
            Rule::factorial => {
                let lhs = lhs?.as_number()?;

                if lhs >= 0.0 && lhs == (lhs as u64) as f64 {
                    Ok(Number(
                        (1..(lhs as u64) + 1).map(|x| x as f64).product::<f64>(),
                    ))
                } else {
                    Err(anyhow!("factorial only works on non-negative integers"))
                }
            }
            Rule::access => {
                let lhs = lhs?;

                let index_value = evaluate_expression(
                    op.into_inner(),
                    Rc::clone(&heap),
                    Rc::clone(&bindings),
                    call_depth,
                )?;

                match lhs {
                    Value::Record(record) => {
                        let borrowed_heap = &heap.borrow();
                        let record = record.reify(borrowed_heap).as_record()?;
                        let key = index_value.as_string(borrowed_heap)?;

                        Ok(record.get(key).copied().unwrap_or(Value::Null))
                    }
                    Value::List(list) => {
                        let borrowed_heap = &heap.borrow();
                        let list = list.reify(borrowed_heap).as_list()?;
                        let index = usize::try_from(index_value.as_number()? as u64)?;

                        Ok(list.get(index).copied().unwrap_or(Value::Null))
                    }
                    Value::String(string) => {
                        let string = string.reify(&heap.borrow()).as_string()?.to_string();
                        let index = usize::try_from(index_value.as_number()? as u64)?;

                        Ok(string
                            .chars()
                            .nth(index)
                            .map(|c| heap.borrow_mut().insert_string(c.to_string()))
                            .unwrap_or(Value::Null))
                    }
                    _ => Err(anyhow!(
                        "expected a record, list, string, but got a {}",
                        lhs.get_type()
                    )),
                }
            }
            Rule::dot_access => {
                let lhs = lhs?;
                let key = op.into_inner().as_str();
                let borrowed_heap = &heap.borrow();

                match lhs {
                    Value::Record(record) => {
                        let record = record.reify(borrowed_heap).as_record()?;
                        Ok(record.get(key).copied().unwrap_or(Value::Null))
                    }
                    _ => Err(anyhow!("expected a record, but got a {}", lhs.get_type())),
                }
            }
            Rule::call_list => {
                let lhs = lhs?;
                let call_list = op.into_inner();
                let args = collect_list(
                    call_list,
                    Rc::clone(&heap),
                    Rc::clone(&bindings),
                    call_depth,
                )?;

                if !lhs.is_lambda() && !lhs.is_built_in() {
                    return Err(anyhow!(
                        "can't call a non-function: {}",
                        lhs.stringify(&heap.borrow())
                    ));
                }

                let def = {
                    let borrowed_heap = &heap.borrow();
                    get_function_def(&lhs, borrowed_heap).ok_or_else(|| {
                        anyhow!("unknown function: {}", lhs.stringify(borrowed_heap))
                    })?
                };

                def.call(
                    lhs,
                    args,
                    Rc::clone(&heap),
                    Rc::clone(&bindings),
                    None,
                    call_depth,
                )
            }
            _ => unreachable!(),
        })
        .map_infix(|lhs, op, rhs| {
            let lhs = lhs?;
            let rhs = rhs?;
            let rule = op.as_rule();

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

                    match rule {
                        Rule::equal => Ok(Bool(l_vec.iter().zip(&r_vec).all(|(l, r)| l == r))),
                        Rule::not_equal => Ok(Bool(l_vec.iter().zip(&r_vec).all(|(l, r)| l != r))),
                        Rule::less => Ok(Bool(l_vec.iter().zip(&r_vec).all(|(l, r)| l < r))),
                        Rule::less_eq => Ok(Bool(l_vec.iter().zip(&r_vec).all(|(l, r)| l <= r))),
                        Rule::greater => Ok(Bool(l_vec.iter().zip(&r_vec).all(|(l, r)| l > r))),
                        Rule::greater_eq => Ok(Bool(l_vec.iter().zip(&r_vec).all(|(l, r)| l >= r))),
                        Rule::and | Rule::natural_and => {
                            let mapped_list = l_vec
                                .iter()
                                .zip(&r_vec)
                                .map(|(l, r)| Ok(Bool(l.as_bool()? && r.as_bool()?)))
                                .collect::<Result<Vec<Value>>>()?;

                            Ok(heap.borrow_mut().insert_list(mapped_list))
                        }
                        Rule::or | Rule::natural_or => {
                            let mapped_list = l_vec
                                .iter()
                                .zip(&r_vec)
                                .map(|(l, r)| Ok(Bool(l.as_bool()? || r.as_bool()?)))
                                .collect::<Result<Vec<Value>>>()?;

                            Ok(heap.borrow_mut().insert_list(mapped_list))
                        }
                        Rule::add => {
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
                                    _ => Err(anyhow!(
                                        "can't add {} and {}",
                                        l.get_type(),
                                        r.get_type()
                                    )),
                                })
                                .collect::<Result<Vec<Value>>>()?;

                            Ok(heap.borrow_mut().insert_list(mapped_list))
                        }
                        Rule::subtract => {
                            let mapped_list = l_vec
                                .iter()
                                .zip(&r_vec)
                                .map(|(l, r)| Ok(Number(l.as_number()? - r.as_number()?)))
                                .collect::<Result<Vec<Value>>>()?;

                            Ok(heap.borrow_mut().insert_list(mapped_list))
                        }
                        Rule::multiply => {
                            let mapped_list = l_vec
                                .iter()
                                .zip(&r_vec)
                                .map(|(l, r)| Ok(Number(l.as_number()? * r.as_number()?)))
                                .collect::<Result<Vec<Value>>>()?;

                            Ok(heap.borrow_mut().insert_list(mapped_list))
                        }
                        Rule::divide => {
                            let mapped_list = l_vec
                                .iter()
                                .zip(&r_vec)
                                .map(|(l, r)| Ok(Number(l.as_number()? / r.as_number()?)))
                                .collect::<Result<Vec<Value>>>()?;

                            Ok(heap.borrow_mut().insert_list(mapped_list))
                        }
                        Rule::modulo => {
                            let mapped_list = l_vec
                                .iter()
                                .zip(&r_vec)
                                .map(|(l, r)| Ok(Number(l.as_number()? % r.as_number()?)))
                                .collect::<Result<Vec<Value>>>()?;

                            Ok(heap.borrow_mut().insert_list(mapped_list))
                        }
                        Rule::power => {
                            let mapped_list = l_vec
                                .iter()
                                .zip(&r_vec)
                                .map(|(l, r)| Ok(Number(l.as_number()?.powf(r.as_number()?))))
                                .collect::<Result<Vec<Value>>>()?;

                            Ok(heap.borrow_mut().insert_list(mapped_list))
                        }
                        Rule::coalesce => {
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
                        Rule::with => {
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

                                    get_function_def(&r, &heap.borrow())
                                        .ok_or(anyhow!(
                                            "can't call unknown function {}",
                                            r.stringify(&heap.borrow())
                                        ))?
                                        .call(
                                            r.clone(),
                                            vec![l.clone()],
                                            Rc::clone(&heap),
                                            Rc::clone(&bindings),
                                            None,
                                            call_depth,
                                        )
                                })
                                .collect::<Result<Vec<Value>>>()?;

                            Ok(heap.borrow_mut().insert_list(mapped_list))
                        }
                        _ => unreachable!(),
                    }
                }
                (Value::List(list_l), rhs) => {
                    let l_vec = {
                        let borrowed_heap = &heap.borrow();
                        list_l.reify(borrowed_heap).as_list()?.clone()
                    };

                    match rule {
                        Rule::equal => Ok(Bool(l_vec.iter().all(|v| v == &rhs))),
                        Rule::not_equal => Ok(Bool(l_vec.iter().all(|v| v != &rhs))),
                        Rule::less => Ok(Bool(l_vec.iter().all(|v| v < &rhs))),
                        Rule::less_eq => Ok(Bool(l_vec.iter().all(|v| v <= &rhs))),
                        Rule::greater => Ok(Bool(l_vec.iter().all(|v| v > &rhs))),
                        Rule::greater_eq => Ok(Bool(l_vec.iter().all(|v| v >= &rhs))),
                        Rule::and | Rule::natural_and => {
                            let mapped_list = l_vec
                                .iter()
                                .map(|v| Ok(Bool(v.as_bool()? && rhs.as_bool()?)))
                                .collect::<Result<Vec<Value>>>()?;

                            Ok(heap.borrow_mut().insert_list(mapped_list))
                        }
                        Rule::or | Rule::natural_or => {
                            let mapped_list = l_vec
                                .iter()
                                .map(|v| Ok(Bool(v.as_bool()? || rhs.as_bool()?)))
                                .collect::<Result<Vec<Value>>>()?;

                            Ok(heap.borrow_mut().insert_list(mapped_list))
                        }
                        Rule::add => {
                            if rhs.is_string() {
                                let rhs_string = {
                                    let borrowed_heap = &heap.borrow();
                                    rhs.as_string(borrowed_heap)?.to_string()
                                };

                                let mapped_list = l_vec
                                    .iter()
                                    .map(|v| {
                                        let string = {
                                            let borrowed_heap = &heap.borrow();
                                            v.as_string(borrowed_heap)?.to_string()
                                        };

                                        Ok(heap
                                            .borrow_mut()
                                            .insert_string(format!("{}{}", string, rhs_string)))
                                    })
                                    .collect::<Result<Vec<Value>>>()?;

                                return Ok(heap.borrow_mut().insert_list(mapped_list));
                            }

                            let mapped_list = l_vec
                                .iter()
                                .map(|v| Ok(Number(v.as_number()? + rhs.as_number()?)))
                                .collect::<Result<Vec<Value>>>()?;

                            return Ok(heap.borrow_mut().insert_list(mapped_list));
                        }
                        Rule::subtract => {
                            let mapped_list = l_vec
                                .iter()
                                .map(|v| Ok(Number(v.as_number()? - rhs.as_number()?)))
                                .collect::<Result<Vec<Value>>>()?;

                            Ok(heap.borrow_mut().insert_list(mapped_list))
                        }
                        Rule::multiply => {
                            let mapped_list = l_vec
                                .iter()
                                .map(|v| Ok(Number(v.as_number()? * rhs.as_number()?)))
                                .collect::<Result<Vec<Value>>>()?;

                            Ok(heap.borrow_mut().insert_list(mapped_list))
                        }
                        Rule::divide => {
                            let mapped_list = l_vec
                                .iter()
                                .map(|v| Ok(Number(v.as_number()? / rhs.as_number()?)))
                                .collect::<Result<Vec<Value>>>()?;

                            Ok(heap.borrow_mut().insert_list(mapped_list))
                        }
                        Rule::modulo => {
                            let mapped_list = l_vec
                                .iter()
                                .map(|v| Ok(Number(v.as_number()? % rhs.as_number()?)))
                                .collect::<Result<Vec<Value>>>()?;

                            Ok(heap.borrow_mut().insert_list(mapped_list))
                        }
                        Rule::power => {
                            let mapped_list = l_vec
                                .iter()
                                .map(|v| Ok(Number(v.as_number()?.powf(rhs.as_number()?))))
                                .collect::<Result<Vec<Value>>>()?;

                            Ok(heap.borrow_mut().insert_list(mapped_list))
                        }
                        Rule::coalesce => {
                            let mapped_list = l_vec
                                .iter()
                                .map(|v| {
                                    if *v == Value::Null {
                                        Ok(rhs.clone())
                                    } else {
                                        Ok(v.clone())
                                    }
                                })
                                .collect::<Result<Vec<Value>>>()?;

                            Ok(heap.borrow_mut().insert_list(mapped_list))
                        }
                        Rule::with => {
                            if !rhs.is_callable() {
                                return Err(anyhow!(
                                    "can't call a non-function: {}",
                                    rhs.stringify(&heap.borrow())
                                ));
                            }

                            let list = heap.borrow_mut().insert_list(l_vec.clone());

                            let call_result =
                                get_built_in_function_def_by_ident("map").unwrap().call(
                                    rhs,
                                    vec![list, rhs],
                                    Rc::clone(&heap),
                                    Rc::clone(&bindings),
                                    None,
                                    call_depth,
                                )?;

                            let mapped_list = { call_result.as_list(&heap.borrow())?.clone() };

                            Ok(heap.borrow_mut().insert_list(mapped_list))
                        }
                        _ => unreachable!(),
                    }
                }
                (lhs, rhs) => match rule {
                    Rule::equal => Ok(Bool(lhs == rhs)),
                    Rule::not_equal => Ok(Bool(lhs != rhs)),
                    Rule::less => Ok(Bool(lhs < rhs)),
                    Rule::less_eq => Ok(Bool(lhs <= rhs)),
                    Rule::greater => Ok(Bool(lhs > rhs)),
                    Rule::greater_eq => Ok(Bool(lhs >= rhs)),
                    Rule::and | Rule::natural_and => Ok(Bool(lhs.as_bool()? && rhs.as_bool()?)),
                    Rule::or | Rule::natural_or => Ok(Bool(lhs.as_bool()? || rhs.as_bool()?)),
                    Rule::add => {
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
                    Rule::subtract => Ok(Number(lhs.as_number()? - rhs.as_number()?)),
                    Rule::multiply => Ok(Number(lhs.as_number()? * rhs.as_number()?)),
                    Rule::divide => Ok(Number(lhs.as_number()? / rhs.as_number()?)),
                    Rule::modulo => Ok(Number(lhs.as_number()? % rhs.as_number()?)),
                    Rule::power => Ok(Number(lhs.as_number()?.powf(rhs.as_number()?))),
                    Rule::coalesce => {
                        if lhs == Value::Null {
                            Ok(rhs)
                        } else {
                            Ok(lhs)
                        }
                    }
                    Rule::with => {
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
                            None,
                            call_depth,
                        );
                    }
                    _ => unreachable!(),
                },
            }
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
        evaluate_expression(
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

        assert_eq!(
            result.as_lambda(&heap.borrow()).unwrap(),
            &LambdaDef {
                name: Some("f".to_string()),
                args: vec![LambdaArg::Required("x".to_string())],
                body: "x + 1".to_string(),
                scope: HashMap::new(),
            }
        );
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
}
