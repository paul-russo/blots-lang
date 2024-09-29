pub use pest::Token;
use pest::{iterators::Pairs, Parser};
use std::collections::HashMap;
use string_interner::{DefaultBackend, DefaultSymbol, StringInterner};

#[derive(Parser)]
#[grammar = "math.pest"]
pub struct MathParser;

pub struct CachedParser<'a> {
    interner: StringInterner<DefaultBackend>,
    cache: HashMap<DefaultSymbol, Result<Pairs<'a, Rule>, pest::error::Error<Rule>>>,
}

impl<'a> CachedParser<'a> {
    pub fn new() -> CachedParser<'a> {
        CachedParser {
            interner: StringInterner::default(),
            cache: HashMap::new(),
        }
    }

    pub fn get_pairs(
        &'a mut self,
        input: String,
    ) -> Result<Pairs<'a, Rule>, pest::error::Error<Rule>> {
        let symbol = self.interner.get_or_intern(input);

        match self.cache.get(&symbol) {
            Some(pairs) => pairs.clone(),
            None => {
                let interned_input = self.interner.resolve(symbol).unwrap();
                let result = MathParser::parse(Rule::input, &interned_input);
                self.cache.insert(symbol, result.clone());
                result
            }
        }
    }
}

pub fn get_pairs(input: &String) -> Result<Pairs<Rule>, pest::error::Error<Rule>> {
    MathParser::parse(Rule::input, input)
}

pub fn get_tokens(input: &String) -> Result<Vec<Token<Rule>>, pest::error::Error<Rule>> {
    let pairs = MathParser::parse(Rule::input, input)?;
    let tokens: Vec<Token<Rule>> = pairs.flat_map(|pair| pair.tokens()).collect();
    Ok(tokens)
}
