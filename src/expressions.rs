use anyhow::{anyhow, Result};
use pest::{
    iterators::Pairs,
    pratt_parser::{Assoc, Op, PrattParser},
};

use crate::parser::Rule;
use std::sync::LazyLock;

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

pub fn evaluate_expression(pairs: Pairs<Rule>) -> Result<f64> {
    PRATT
        .map_primary(|primary| match primary.as_rule() {
            Rule::number => primary.as_str().parse::<f64>().map_err(|e| e.into()),
            Rule::identifier => todo!("variables not implemented yet"),
            Rule::expression => evaluate_expression(primary.into_inner()),
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
                        Ok((1..(lhs as u64) + 1).product::<u64>() as f64)
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
