use crate::{
    functions::{get_function_def, UserDefinedFunctionDef},
    parser::Rule,
};
use anyhow::{anyhow, Result};
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
        .op(Op::prefix(Rule::negation))
        .op(Op::postfix(Rule::factorial))
});

pub fn evaluate_expression(
    pairs: Pairs<Rule>,
    variables: &HashMap<String, f64>,
    function_defs: &HashMap<String, UserDefinedFunctionDef>,
) -> Result<f64> {
    PRATT
        .map_primary(|primary| match primary.as_rule() {
            Rule::number => primary.as_str().parse::<f64>().map_err(|e| e.into()),
            Rule::identifier => {
                let ident = primary.as_str();

                match ident {
                    "pi" => return Ok(core::f64::consts::PI),
                    "e" => return Ok(core::f64::consts::E),
                    "infinity" => return Ok(f64::INFINITY),
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
                let args: Vec<f64> = call_list_entries
                    .into_iter()
                    .map(|arg| evaluate_expression(arg.into_inner(), variables, function_defs))
                    .collect::<Result<Vec<_>, _>>()?;

                if let Some(def) = get_function_def(ident, function_defs) {
                    return def.call(args, variables, function_defs);
                }

                Err(anyhow!("unknown function: {}", ident))
            }
            _ => unreachable!(),
        })
        .map_prefix(|op, rhs| {
            let rhs = rhs?;

            match op.as_rule() {
                Rule::negation => Ok(-rhs),
                _ => unreachable!(),
            }
        })
        .map_postfix(|lhs, op| {
            let lhs = lhs?;

            match op.as_rule() {
                Rule::factorial => {
                    if lhs >= 0.0 && lhs == (lhs as u64) as f64 {
                        Ok((1..(lhs as u64) + 1).map(|x| x as f64).product::<f64>())
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
                Rule::add => Ok(lhs + rhs),
                Rule::subtract => Ok(lhs - rhs),
                Rule::multiply => Ok(lhs * rhs),
                Rule::divide => Ok(lhs / rhs),
                Rule::modulo => Ok(lhs % rhs),
                Rule::power => Ok(lhs.powf(rhs)),
                _ => unreachable!(),
            }
        })
        .parse(pairs)
}
