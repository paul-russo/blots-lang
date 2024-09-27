use crate::{
    functions::{get_function_def, UserDefinedFunctionDef},
    parser::Rule,
    values::Value::{self, List, Number, Spread},
};
use anyhow::{anyhow, Ok, Result};
use pest::{
    iterators::Pairs,
    pratt_parser::{Assoc, Op, PrattParser},
};
use std::{collections::HashMap, sync::LazyLock};

static PRATT: LazyLock<PrattParser<Rule>> = LazyLock::new(|| {
    PrattParser::new()
        .op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::subtract, Assoc::Left))
        .op(Op::infix(Rule::multiply, Assoc::Left)
            | Op::infix(Rule::divide, Assoc::Left)
            | Op::infix(Rule::modulo, Assoc::Left))
        .op(Op::infix(Rule::power, Assoc::Right))
        .op(Op::prefix(Rule::negation) | Op::prefix(Rule::spread_operator))
        .op(Op::postfix(Rule::factorial))
});

pub fn collect_list(
    pairs: Pairs<Rule>,
    variables: &HashMap<String, Value>,
    function_defs: &HashMap<String, UserDefinedFunctionDef>,
) -> Result<Vec<Value>> {
    Ok(pairs
        .into_iter()
        // This can't just be flat_map (at least I think) because we don't want to unwrap
        // each Result as we iterate, which would result in failed values being skipped.
        .map(|value| evaluate_expression(value.into_inner(), variables, function_defs))
        .collect::<Result<Vec<Value>>>()?
        .into_iter()
        .flatten()
        .collect())
}

pub fn evaluate_expression(
    pairs: Pairs<Rule>,
    variables: &HashMap<String, Value>,
    function_defs: &HashMap<String, UserDefinedFunctionDef>,
) -> Result<Value> {
    PRATT
        .map_primary(|primary| match primary.as_rule() {
            Rule::number => Ok(Number(
                primary
                    .as_str()
                    .parse::<f64>()
                    .map_err(|e| anyhow::Error::from(e))?,
            )),
            Rule::list => {
                let list_pairs = primary.into_inner();
                let values = collect_list(list_pairs, variables, function_defs)?;

                Ok(List(values))
            }
            Rule::list_access => {
                let mut inner_pairs = primary.into_inner();

                let ident = inner_pairs.next().unwrap().as_str();
                let index = evaluate_expression(inner_pairs, variables, function_defs)?;

                match variables.get(ident) {
                    Some(List(list)) => {
                        list.get(index.to_number()? as usize)
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
            Rule::expression => evaluate_expression(primary.into_inner(), variables, function_defs),
            Rule::function_call => {
                let mut inner_pairs = primary.into_inner();
                let ident = inner_pairs.next().unwrap().as_str();
                let call_list = inner_pairs.next().unwrap();
                let call_list_entries = call_list.into_inner();
                let args = collect_list(call_list_entries, variables, function_defs)?;

                if let Some(def) = get_function_def(ident, function_defs) {
                    return def.call(args, variables, function_defs);
                }

                Err(anyhow!("unknown function: {}", ident))
            }
            _ => unreachable!("{}", primary.as_str()),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::negation => {
                let rhs = rhs?.to_number()?;
                Ok(Number(-rhs))
            }
            Rule::spread_operator => {
                let rhs = rhs?;
                let list = rhs.to_list()?;
                Ok(Spread(list.clone()))
            }
            _ => unreachable!(),
        })
        .map_postfix(|lhs, op| {
            let lhs = lhs?.to_number()?;

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
            let lhs = lhs?.to_number()?;
            let rhs = rhs?.to_number()?;

            match op.as_rule() {
                Rule::add => Ok(Number(lhs + rhs)),
                Rule::subtract => Ok(Number(lhs - rhs)),
                Rule::multiply => Ok(Number(lhs * rhs)),
                Rule::divide => Ok(Number(lhs / rhs)),
                Rule::modulo => Ok(Number(lhs % rhs)),
                Rule::power => Ok(Number(lhs.powf(rhs))),
                _ => unreachable!(),
            }
        })
        .parse(pairs)
}
