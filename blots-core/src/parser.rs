pub use pest::Token;
use pest::{iterators::Pairs, Parser};

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct MathParser;

#[cfg(not(target_arch = "wasm32"))]
pub static CALL_COUNT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

#[cfg(not(target_arch = "wasm32"))]
pub static TOTAL_PARSE_TIME: std::sync::atomic::AtomicUsize =
    std::sync::atomic::AtomicUsize::new(0);

pub fn get_pairs(input: &String) -> Result<Pairs<'_, Rule>, pest::error::Error<Rule>> {
    #[cfg(not(target_arch = "wasm32"))]
    let start = std::time::Instant::now();

    let pairs = MathParser::parse(Rule::input, input);

    #[cfg(not(target_arch = "wasm32"))]
    CALL_COUNT.fetch_add(1, std::sync::atomic::Ordering::AcqRel);

    #[cfg(not(target_arch = "wasm32"))]
    TOTAL_PARSE_TIME.fetch_add(
        start.elapsed().as_micros() as usize,
        std::sync::atomic::Ordering::AcqRel,
    );

    pairs
}

pub fn get_tokens(input: &String) -> Result<Vec<Token<'_, Rule>>, pest::error::Error<Rule>> {
    let pairs = MathParser::parse(Rule::input, input)?;
    let tokens: Vec<Token<Rule>> = pairs.flat_map(|pair| pair.tokens()).collect();
    Ok(tokens)
}
