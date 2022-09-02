use super::parser::Rule;
use pest::{
    iterators::{Pair, Pairs},
    prec_climber::*,
};

lazy_static! {
    static ref CLIMBER: PrecClimber<Rule> = {
        PrecClimber::new(vec![
            Operator::new(Rule::add, Assoc::Left) | Operator::new(Rule::subtract, Assoc::Left),
            Operator::new(Rule::multiply, Assoc::Left) | Operator::new(Rule::divide, Assoc::Left),
            Operator::new(Rule::power, Assoc::Right),
        ])
    };
}

pub fn evaluate(pairs: Pairs<Rule>) -> f64 {
    CLIMBER.climb(
        pairs,
        |pair: Pair<Rule>| match pair.as_rule() {
            Rule::number => pair.as_str().parse::<f64>().unwrap(),
            Rule::expression => evaluate(pair.into_inner()),
            _ => unreachable!(),
        },
        |lhs: f64, op: Pair<Rule>, rhs: f64| match op.as_rule() {
            Rule::power => lhs.powf(rhs),
            Rule::multiply => lhs * rhs,
            Rule::divide => lhs / rhs,
            Rule::add => lhs + rhs,
            Rule::subtract => lhs - rhs,
            _ => unreachable!(),
        },
    )
}
