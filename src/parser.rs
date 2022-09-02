use pest::{iterators::Pairs, Parser};

#[derive(Parser)]
#[grammar = "math.pest"]
pub struct MathParser;

pub fn parse_expression(input: &str) -> Result<Pairs<Rule>, pest::error::Error<Rule>> {
    MathParser::parse(Rule::calculation, input)
}
