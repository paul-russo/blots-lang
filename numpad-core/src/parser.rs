pub use pest::Token;
use pest::{iterators::Pairs, Parser};

#[derive(Parser)]
#[grammar = "math.pest"]
struct MathParser;

pub fn get_pairs(input: &String) -> Result<Pairs<Rule>, pest::error::Error<Rule>> {
    MathParser::parse(Rule::input, input)
}

pub fn get_tokens(input: &String) -> Result<Vec<Token<Rule>>, pest::error::Error<Rule>> {
    let pairs = MathParser::parse(Rule::input, input)?;
    let tokens: Vec<Token<Rule>> = pairs.flat_map(|pair| pair.tokens()).collect();
    Ok(tokens)
}
