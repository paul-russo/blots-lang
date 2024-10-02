use crate::{
    functions::{get_function_def, BUILT_IN_FUNCTION_IDENTS},
    parser::Rule,
    values::{
        LambdaDef, SpreadValue,
        Value::{self, Bool, List, Number, Spread},
    },
};
use anyhow::{anyhow, Ok, Result};
use pest::{
    iterators::Pairs,
    pratt_parser::{Assoc, Op, PrattParser},
};
use std::{collections::HashMap, sync::LazyLock};

static PRATT: LazyLock<PrattParser<Rule>> = LazyLock::new(|| {
    PrattParser::new()
        .op(Op::infix(Rule::and, Assoc::Left) | Op::infix(Rule::or, Assoc::Left))
        .op(Op::infix(Rule::equal, Assoc::Left)
            | Op::infix(Rule::not_equal, Assoc::Left)
            | Op::infix(Rule::less, Assoc::Left)
            | Op::infix(Rule::less_eq, Assoc::Left)
            | Op::infix(Rule::greater, Assoc::Left)
            | Op::infix(Rule::greater_eq, Assoc::Left)
            | Op::infix(Rule::each_equal, Assoc::Left)
            | Op::infix(Rule::each_not_equal, Assoc::Left)
            | Op::infix(Rule::each_less_eq, Assoc::Left)
            | Op::infix(Rule::each_less, Assoc::Left)
            | Op::infix(Rule::each_greater_eq, Assoc::Left)
            | Op::infix(Rule::each_greater, Assoc::Left))
        .op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::subtract, Assoc::Left))
        .op(Op::infix(Rule::multiply, Assoc::Left)
            | Op::infix(Rule::divide, Assoc::Left)
            | Op::infix(Rule::modulo, Assoc::Left))
        .op(Op::infix(Rule::power, Assoc::Right))
        .op(Op::prefix(Rule::negation)
            | Op::prefix(Rule::spread_operator)
            | Op::prefix(Rule::invert))
        .op(Op::postfix(Rule::factorial))
});

pub fn collect_list(
    pairs: Pairs<Rule>,
    variables: &mut HashMap<String, Value>,
) -> Result<Vec<Value>> {
    Ok(pairs
        .into_iter()
        // This can't just be flat_map (at least I think) because we don't want to unwrap
        // each Result as we iterate, which would result in failed values being skipped.
        .map(|value| evaluate_expression(value.into_inner(), variables))
        .collect::<Result<Vec<Value>>>()?
        .into_iter()
        .flatten()
        .collect())
}

pub fn evaluate_expression(
    pairs: Pairs<Rule>,
    variables: &mut HashMap<String, Value>,
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
                let values = collect_list(list_pairs, variables)?;

                Ok(List(values))
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
            Rule::string => Ok(Value::String(primary.into_inner().as_str().to_string())),
            Rule::assignment => {
                let mut inner_pairs = primary.into_inner();
                let ident = inner_pairs.next().unwrap().as_str();

                if BUILT_IN_FUNCTION_IDENTS.contains(&ident) {
                    return Err(anyhow!("cannot assign to built-in function: {}", ident));
                }

                if ident == "pi" || ident == "e" || ident == "infinity" {
                    return Err(anyhow!("cannot assign to constant: {}", ident));
                }

                if ident == "if"
                    || ident == "then"
                    || ident == "else"
                    || ident == "true"
                    || ident == "false"
                    || ident == "null"
                {
                    return Err(anyhow!("cannot assign to keyword: {}", ident));
                }

                let expression = inner_pairs.next().unwrap();
                let mut value = evaluate_expression(expression.into_inner(), variables)?;

                match value {
                    Value::Lambda(ref mut lambda) => {
                        lambda.set_name(ident.to_string());
                    }
                    _ => {}
                }

                variables.insert(ident.to_string(), value.clone());
                Ok(value)
            }
            Rule::lambda => {
                let mut inner_pairs = primary.into_inner();
                let args = inner_pairs.next().unwrap().into_inner();
                let body = inner_pairs.next().unwrap();

                Ok(Value::Lambda(LambdaDef {
                    name: None,
                    args: args.map(|arg| arg.as_str().to_string()).collect(),
                    body: body.as_str().to_string(),
                    scope: variables.clone(),
                }))
            }
            Rule::conditional => {
                let mut inner_pairs = primary.into_inner();
                let condition_expr = inner_pairs.next().unwrap();
                let then_expr = inner_pairs.next().unwrap();
                let else_expr = inner_pairs.next().unwrap();

                let condition =
                    evaluate_expression(condition_expr.into_inner(), variables)?.as_bool()?;

                if condition {
                    evaluate_expression(then_expr.into_inner(), variables)
                } else {
                    evaluate_expression(else_expr.into_inner(), variables)
                }
            }
            Rule::list_access => {
                let mut inner_pairs = primary.into_inner();

                let ident = inner_pairs.next().unwrap().as_str();
                let index = evaluate_expression(inner_pairs, variables)?;

                match variables.get(ident) {
                    Some(List(list)) => {
                        list.get(index.as_number()? as usize)
                            .cloned()
                            .ok_or_else(|| {
                                anyhow!(
                                    "index out of bounds: {} is out of range for list {}",
                                    index,
                                    ident
                                )
                            })
                    }
                    Some(_) => Err(anyhow!("expected a list, but got a number")),
                    None => Err(anyhow!("unknown variable: {}", ident)),
                }
            }
            Rule::identifier => {
                let ident = primary.as_str();

                match ident {
                    "pi" => return Ok(Number(core::f64::consts::PI)),
                    "e" => return Ok(Number(core::f64::consts::E)),
                    "infinity" => return Ok(Number(f64::INFINITY)),
                    _ => variables
                        .get(ident)
                        .cloned()
                        .ok_or_else(|| anyhow!("unknown variable: {}", primary.as_str())),
                }
            }
            Rule::expression => evaluate_expression(primary.into_inner(), variables),
            Rule::function_call => {
                let mut inner_pairs = primary.into_inner();
                let ident = inner_pairs.next().unwrap().as_str();
                let call_list = inner_pairs.next().unwrap();
                let call_list_entries = call_list.into_inner();
                let args = collect_list(call_list_entries, variables)?;

                if let Some(def) = get_function_def(ident, variables) {
                    return def.call(args, variables, None);
                }

                Err(anyhow!("unknown function: {}", ident))
            }
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
                    List(list) => return Ok(Spread(SpreadValue::List(list.clone()))),
                    Value::String(s) => return Ok(Spread(SpreadValue::String(s))),
                    _ => return Err(anyhow!("expected a list or string")),
                }
            }
            Rule::invert => {
                let rhs = rhs?.as_bool()?;
                Ok(Bool(!rhs))
            }
            _ => unreachable!(),
        })
        .map_postfix(|lhs, op| {
            let lhs = lhs?.as_number()?;

            match op.as_rule() {
                Rule::factorial => {
                    if lhs >= 0.0 && lhs == (lhs as u64) as f64 {
                        Ok(Number(
                            (1..(lhs as u64) + 1).map(|x| x as f64).product::<f64>(),
                        ))
                    } else {
                        Err(anyhow!("factorial only works on non-negative integers"))
                    }
                }
                _ => unreachable!(),
            }
        })
        .map_infix(|lhs, op, rhs| {
            let lhs = lhs?;
            let rhs = rhs?;

            match op.as_rule() {
                Rule::equal => Ok(Bool(lhs == rhs)),
                Rule::not_equal => Ok(Bool(lhs != rhs)),
                Rule::less => Ok(Bool(lhs < rhs)),
                Rule::less_eq => Ok(Bool(lhs <= rhs)),
                Rule::greater => Ok(Bool(lhs > rhs)),
                Rule::greater_eq => Ok(Bool(lhs >= rhs)),
                Rule::each_equal => match (lhs, rhs) {
                    (List(lhs), List(rhs)) => {
                        if lhs.len() != rhs.len() {
                            return Err(anyhow!("lists must be the same length"));
                        }
                        Ok(Bool(lhs.iter().zip(rhs.iter()).all(|(l, r)| l == r)))
                    }
                    (List(lhs), rhs_value) => Ok(Bool(lhs.iter().all(|v| v == &rhs_value))),
                    _ => Err(anyhow!(
                        "expected two lists or a list and a number, bool, function, or string"
                    )),
                },
                Rule::each_not_equal => match (lhs, rhs) {
                    (List(lhs), List(rhs)) => {
                        if lhs.len() != rhs.len() {
                            return Err(anyhow!("lists must be the same length"));
                        }
                        Ok(Bool(lhs.iter().zip(rhs.iter()).all(|(l, r)| l != r)))
                    }
                    (List(lhs), rhs_value) => Ok(Bool(lhs.iter().all(|v| v != &rhs_value))),
                    _ => Err(anyhow!(
                        "expected two lists or a list and a number, bool, function, or string"
                    )),
                },
                Rule::each_less => match (lhs, rhs) {
                    (List(lhs), List(rhs)) => {
                        if lhs.len() != rhs.len() {
                            return Err(anyhow!("lists must be the same length"));
                        }
                        Ok(Bool(lhs.iter().zip(rhs.iter()).all(|(l, r)| l < r)))
                    }
                    (List(lhs), rhs_value) => Ok(Bool(lhs.iter().all(|l| l < &rhs_value))),
                    _ => Err(anyhow!(
                        "expected two lists or a list and a number, bool, function, or string"
                    )),
                },
                Rule::each_less_eq => match (lhs, rhs) {
                    (List(lhs), List(rhs)) => {
                        if lhs.len() != rhs.len() {
                            return Err(anyhow!("lists must be the same length"));
                        }
                        Ok(Bool(lhs.iter().zip(rhs.iter()).all(|(l, r)| l <= r)))
                    }
                    (List(lhs), rhs_value) => Ok(Bool(lhs.iter().all(|l| l <= &rhs_value))),
                    _ => Err(anyhow!(
                        "expected two lists or a list and a number, bool, function, or string"
                    )),
                },
                Rule::each_greater => match (lhs, rhs) {
                    (List(lhs), List(rhs)) => {
                        if lhs.len() != rhs.len() {
                            return Err(anyhow!("lists must be the same length"));
                        }
                        Ok(Bool(lhs.iter().zip(rhs.iter()).all(|(l, r)| l > r)))
                    }
                    (List(lhs), rhs_value) => Ok(Bool(lhs.iter().all(|l| l > &rhs_value))),
                    _ => Err(anyhow!(
                        "expected two lists or a list and a number, bool, function, or string"
                    )),
                },
                Rule::each_greater_eq => match (lhs, rhs) {
                    (List(lhs), List(rhs)) => {
                        if lhs.len() != rhs.len() {
                            return Err(anyhow!("lists must be the same length"));
                        }
                        Ok(Bool(lhs.iter().zip(rhs.iter()).all(|(l, r)| l >= r)))
                    }
                    (List(lhs), rhs_value) => Ok(Bool(lhs.iter().all(|l| l >= &rhs_value))),
                    _ => Err(anyhow!(
                        "expected two lists or a list and a number, bool, function, or string"
                    )),
                },
                Rule::and => Ok(Bool(lhs.as_bool()? && rhs.as_bool()?)),
                Rule::or => Ok(Bool(lhs.as_bool()? || rhs.as_bool()?)),
                Rule::add => Ok(Number(lhs.as_number()? + rhs.as_number()?)),
                Rule::subtract => Ok(Number(lhs.as_number()? - rhs.as_number()?)),
                Rule::multiply => Ok(Number(lhs.as_number()? * rhs.as_number()?)),
                Rule::divide => Ok(Number(lhs.as_number()? / rhs.as_number()?)),
                Rule::modulo => Ok(Number(lhs.as_number()? % rhs.as_number()?)),
                Rule::power => Ok(Number(lhs.as_number()?.powf(rhs.as_number()?))),
                _ => unreachable!(),
            }
        })
        .parse(pairs)
}
