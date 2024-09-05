use pest::{iterators::Pairs, Parser};

#[derive(Parser)]
#[grammar = "math.pest"]
pub struct MathParser;

pub fn get_pairs(input: &String) -> Result<Pairs<Rule>, pest::error::Error<Rule>> {
    MathParser::parse(Rule::calculation, input)
}
