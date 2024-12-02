pub use pest::Token;
use pest::{iterators::Pairs, Parser};

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct MathParser;

pub static CALL_COUNT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
pub static TOTAL_PARSE_TIME: std::sync::atomic::AtomicUsize =
    std::sync::atomic::AtomicUsize::new(0);

pub fn get_pairs(input: &String) -> Result<Pairs<Rule>, pest::error::Error<Rule>> {
    let start = std::time::Instant::now();
    CALL_COUNT.fetch_add(1, std::sync::atomic::Ordering::AcqRel);
    let pairs = MathParser::parse(Rule::input, input);
    TOTAL_PARSE_TIME.fetch_add(
        start.elapsed().as_micros() as usize,
        std::sync::atomic::Ordering::AcqRel,
    );
    pairs
}

pub fn get_tokens(input: &String) -> Result<Vec<Token<Rule>>, pest::error::Error<Rule>> {
    let pairs = MathParser::parse(Rule::input, input)?;
    let tokens: Vec<Token<Rule>> = pairs.flat_map(|pair| pair.tokens()).collect();
    Ok(tokens)
}
