pub use pest::Token;
use pest::{
    iterators::{Pairs, Tokens},
    Parser,
};

#[derive(Parser)]
#[grammar = "math.pest"]
pub struct MathParser;

pub fn get_pairs(input: &String) -> Result<Pairs<Rule>, pest::error::Error<Rule>> {
    MathParser::parse(Rule::calculation, input)
}

pub fn get_tokens(input: &String) -> Result<Tokens<Rule>, pest::error::Error<Rule>> {
    let mut pairs = MathParser::parse(Rule::calculation, input)?;
    let inner_pair = pairs.next().unwrap();
    Ok(inner_pair.tokens())
}
