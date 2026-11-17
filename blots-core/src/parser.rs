pub use pest::Token;
use pest::{
    Parser, Position,
    error::{Error, ErrorVariant},
    iterators::Pairs,
};

pub const MAX_NESTING_DEPTH: usize = 160;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct BlotsParser;

#[cfg(not(target_arch = "wasm32"))]
pub static CALL_COUNT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

#[cfg(not(target_arch = "wasm32"))]
pub static TOTAL_PARSE_TIME: std::sync::atomic::AtomicUsize =
    std::sync::atomic::AtomicUsize::new(0);

fn nesting_error(input: &str, index: usize) -> Error<Rule> {
    let pos = Position::new(input, index).unwrap_or_else(|| Position::from_start(input));

    Error::new_from_pos(
        ErrorVariant::CustomError {
            message: format!(
                "expression exceeds maximum nesting depth of {}",
                MAX_NESTING_DEPTH
            ),
        },
        pos,
    )
}

/// Whether the token starting at `index` continues an expression from a previous line.
fn continues_after_newline(bytes: &[u8], index: usize) -> bool {
    let Some(&first) = bytes.get(index) else {
        return false;
    };

    if first.is_ascii_lowercase() {
        let mut end = index + 1;
        while bytes
            .get(end)
            .is_some_and(|b| b.is_ascii_alphanumeric() || *b == b'_')
        {
            end += 1;
        }

        return matches!(
            &bytes[index..end],
            b"then" | b"else" | b"and" | b"or" | b"via" | b"into" | b"where"
        );
    }

    match first {
        b'+' | b'-' | b'*' | b'%' | b'^' | b'<' | b'>' => true,
        b'/' => bytes.get(index + 1) != Some(&b'/'),
        b'=' => bytes.get(index + 1) == Some(&b'='),
        b'!' => bytes.get(index + 1) == Some(&b'='),
        b'&' => bytes.get(index + 1) == Some(&b'&'),
        b'|' => bytes.get(index + 1) == Some(&b'|'),
        b'?' => bytes.get(index + 1) == Some(&b'?'),
        b'.' => matches!(bytes.get(index + 1), Some(b'=' | b'!' | b'<' | b'>')),
        _ => false,
    }
}

#[allow(clippy::result_large_err)]
fn check_nesting_depth(input: &str) -> Result<(), Error<Rule>> {
    let bytes = input.as_bytes();
    let mut frames: Vec<usize> = Vec::new();
    let mut chain_depth: usize = 0;
    let mut total_chain: usize = 0;
    let mut prefix_run: usize = 0;
    let mut prev_continues = false;
    let mut index = 0;

    while index < bytes.len() {
        match bytes[index] {
            b' ' | b'\t' | b'\r' => {
                index += 1;
            }
            b'/' if bytes.get(index + 1) == Some(&b'/') => {
                let text = index + 2;
                match bytes[text..].iter().position(|&b| b == b'\n') {
                    Some(offset) => index = text + offset,
                    None => break,
                }
            }
            b'\n' => {
                index += 1;
                while index < bytes.len() {
                    match bytes[index] {
                        b' ' | b'\t' | b'\r' | b'\n' => index += 1,
                        b'/' if bytes.get(index + 1) == Some(&b'/') => {
                            let text = index + 2;
                            match bytes[text..].iter().position(|&b| b == b'\n') {
                                Some(offset) => index = text + offset + 1,
                                None => {
                                    index = bytes.len();
                                    break;
                                }
                            }
                        }
                        _ => break,
                    }
                }

                prefix_run = 0;
                if !prev_continues && !continues_after_newline(bytes, index) {
                    total_chain -= chain_depth;
                    chain_depth = 0;
                }
                prev_continues = false;
            }
            b'"' | b'\'' => {
                let quote = bytes[index];
                prefix_run = 0;
                prev_continues = false;
                index += 1;

                match bytes[index..].iter().position(|&b| b == quote) {
                    Some(offset) => index += offset + 1,
                    None => break,
                }
            }
            b'(' | b'[' | b'{' => {
                frames.push(chain_depth);
                chain_depth = 0;
                prefix_run = 0;
                prev_continues = true;

                if frames.len() + total_chain > MAX_NESTING_DEPTH {
                    return Err(nesting_error(input, index));
                }
                index += 1;
            }
            b')' | b']' | b'}' => {
                total_chain -= chain_depth;
                chain_depth = frames.pop().unwrap_or(0);
                prefix_run = 0;
                prev_continues = false;
                index += 1;
            }
            b',' | b';' => {
                total_chain -= chain_depth;
                chain_depth = 0;
                prefix_run = 0;
                prev_continues = true;
                index += 1;
            }
            b'=' if bytes.get(index + 1) == Some(&b'>') => {
                chain_depth += 1;
                total_chain += 1;
                prefix_run = 0;
                prev_continues = true;

                if frames.len() + total_chain > MAX_NESTING_DEPTH {
                    return Err(nesting_error(input, index));
                }
                index += 2;
            }
            b'=' if bytes.get(index + 1) == Some(&b'=') => {
                prefix_run = 0;
                prev_continues = true;
                index += 2;
            }
            b'=' => {
                chain_depth += 1;
                total_chain += 1;
                prefix_run = 0;
                prev_continues = true;

                if frames.len() + total_chain > MAX_NESTING_DEPTH {
                    return Err(nesting_error(input, index));
                }
                index += 1;
            }
            b'!' if bytes.get(index + 1) == Some(&b'=') => {
                prefix_run = 0;
                prev_continues = true;
                index += 2;
            }
            b'!' => {
                prefix_run += 1;
                prev_continues = false;

                if prefix_run > MAX_NESTING_DEPTH {
                    return Err(nesting_error(input, index));
                }
                index += 1;
            }
            b'-' => {
                prefix_run += 1;
                prev_continues = true;

                if prefix_run > MAX_NESTING_DEPTH {
                    return Err(nesting_error(input, index));
                }
                index += 1;
            }
            b'^' => {
                chain_depth += 1;
                total_chain += 1;
                prefix_run = 0;
                prev_continues = true;

                if frames.len() + total_chain > MAX_NESTING_DEPTH {
                    return Err(nesting_error(input, index));
                }
                index += 1;
            }
            b'.' => {
                if bytes.get(index..index + 3) == Some(b"...") {
                    prefix_run += 1;
                    prev_continues = true;

                    if prefix_run > MAX_NESTING_DEPTH {
                        return Err(nesting_error(input, index));
                    }
                    index += 3;
                } else if matches!(
                    bytes.get(index..index + 3),
                    Some(b".==" | b".!=" | b".<=" | b".>=")
                ) {
                    prefix_run = 0;
                    prev_continues = true;
                    index += 3;
                } else if matches!(bytes.get(index..index + 2), Some(b".<" | b".>")) {
                    prefix_run = 0;
                    prev_continues = true;
                    index += 2;
                } else {
                    prefix_run = 0;
                    prev_continues = false;
                    index += 1;
                }
            }
            b'<' | b'>' if bytes.get(index + 1) == Some(&b'=') => {
                prefix_run = 0;
                prev_continues = true;
                index += 2;
            }
            b'&' if bytes.get(index + 1) == Some(&b'&') => {
                prefix_run = 0;
                prev_continues = true;
                index += 2;
            }
            b'|' if bytes.get(index + 1) == Some(&b'|') => {
                prefix_run = 0;
                prev_continues = true;
                index += 2;
            }
            b'?' if bytes.get(index + 1) == Some(&b'?') => {
                prefix_run = 0;
                prev_continues = true;
                index += 2;
            }
            b'+' | b'*' | b'/' | b'%' | b'<' | b'>' | b':' => {
                prefix_run = 0;
                prev_continues = true;
                index += 1;
            }
            b'#' => {
                prefix_run = 0;
                prev_continues = false;
                index += 1;
                while bytes
                    .get(index)
                    .is_some_and(|b| b.is_ascii_alphanumeric() || *b == b'_')
                {
                    index += 1;
                }
            }
            b'0'..=b'9' => {
                let is_hex =
                    bytes[index] == b'0' && matches!(bytes.get(index + 1), Some(b'x' | b'X'));
                prefix_run = 0;
                prev_continues = false;
                index += 1;
                while bytes
                    .get(index)
                    .is_some_and(|b| b.is_ascii_alphanumeric() || *b == b'_')
                {
                    let is_exp = !is_hex
                        && matches!(bytes[index], b'e' | b'E')
                        && matches!(bytes.get(index + 1), Some(b'+' | b'-'));
                    index += if is_exp { 2 } else { 1 };
                }

                if !is_hex
                    && bytes.get(index) == Some(&b'.')
                    && bytes.get(index + 1).is_some_and(u8::is_ascii_digit)
                {
                    index += 2;
                    while bytes
                        .get(index)
                        .is_some_and(|b| b.is_ascii_alphanumeric() || *b == b'_')
                    {
                        let is_exp = matches!(bytes[index], b'e' | b'E')
                            && matches!(bytes.get(index + 1), Some(b'+' | b'-'));
                        index += if is_exp { 2 } else { 1 };
                    }
                }
            }
            b if b.is_ascii_alphabetic() || b == b'_' => {
                let start = index;
                index += 1;
                while bytes
                    .get(index)
                    .is_some_and(|c| c.is_ascii_alphanumeric() || *c == b'_')
                {
                    index += 1;
                }

                match &bytes[start..index] {
                    b"not" if matches!(bytes.get(index), Some(b' ' | b'\t')) => {
                        prefix_run += 1;
                        prev_continues = false;

                        if prefix_run > MAX_NESTING_DEPTH {
                            return Err(nesting_error(input, start));
                        }
                    }
                    b"if" => {
                        chain_depth += 1;
                        total_chain += 1;
                        prefix_run = 0;
                        prev_continues = true;

                        if frames.len() + total_chain > MAX_NESTING_DEPTH {
                            return Err(nesting_error(input, start));
                        }
                    }
                    b"then" | b"else" | b"and" | b"or" | b"via" | b"into" | b"where" => {
                        prefix_run = 0;
                        prev_continues = true;
                    }
                    _ => {
                        prefix_run = 0;
                        prev_continues = false;
                    }
                }
            }
            _ => {
                prefix_run = 0;
                prev_continues = false;
                index += 1;
            }
        }
    }

    Ok(())
}

#[allow(clippy::result_large_err)]
pub fn get_pairs(input: &str) -> Result<Pairs<'_, Rule>, pest::error::Error<Rule>> {
    check_nesting_depth(input)?;

    #[cfg(not(target_arch = "wasm32"))]
    let start = std::time::Instant::now();

    let pairs = BlotsParser::parse(Rule::input, input);

    #[cfg(not(target_arch = "wasm32"))]
    CALL_COUNT.fetch_add(1, std::sync::atomic::Ordering::AcqRel);

    #[cfg(not(target_arch = "wasm32"))]
    TOTAL_PARSE_TIME.fetch_add(
        start.elapsed().as_micros() as usize,
        std::sync::atomic::Ordering::AcqRel,
    );

    pairs
}

#[allow(clippy::result_large_err)]
pub fn get_tokens(input: &str) -> Result<Vec<Token<'_, Rule>>, pest::error::Error<Rule>> {
    check_nesting_depth(input)?;
    let pairs = BlotsParser::parse(Rule::input, input)?;
    let tokens: Vec<Token<Rule>> = pairs.flat_map(|pair| pair.tokens()).collect();
    Ok(tokens)
}
