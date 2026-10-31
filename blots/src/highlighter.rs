use std::rc::Rc;

use blots_core::{environment::Environment, functions::BUILTIN_FUNCTION_NAMES};
use rustyline::{
    Context, Helper, completion::Completer, highlight::Highlighter, hint::Hinter,
    validate::Validator,
};
use yansi::{Color, Paint};

pub struct BlotsHighlighter {
    bindings: Rc<Environment>,
}

impl BlotsHighlighter {
    pub fn new(bindings: Rc<Environment>) -> Self {
        Self { bindings }
    }

    fn highlight_line(&self, line: &str) -> String {
        let mut highlighted = String::new();
        let mut last_end = 0;

        // Apply highlighting
        for (start, end, highlight_type) in tokens(line) {
            // Add unhighlighted text before this match
            if start > last_end {
                highlighted.push_str(&line[last_end..start]);
            }

            // Add highlighted text
            let text = &line[start..end];
            let colored_text = match highlight_type {
                HighlightType::Comment => text.fg(Color::Rgb(128, 128, 128)).to_string(), // Gray
                HighlightType::Keyword => text.fg(Color::Magenta).bold().to_string(), // Purple/Magenta
                HighlightType::Operator => text.fg(Color::Cyan).to_string(),          // Cyan
                HighlightType::String => text.fg(Color::Green).to_string(),           // Green
                HighlightType::Number => text.fg(Color::Yellow).to_string(),          // Yellow
                HighlightType::InputReference => text.fg(Color::Blue).to_string(),    // Blue
            };
            highlighted.push_str(&colored_text);
            last_end = end;
        }

        // Add remaining unhighlighted text
        if last_end < line.len() {
            highlighted.push_str(&line[last_end..]);
        }

        highlighted
    }

    /// Format a result string with syntax highlighting for display
    pub fn highlight_result(&self, result: &str) -> String {
        format!("= {}", self.highlight_line(result))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum HighlightType {
    Comment,
    Keyword,
    Operator,
    String,
    Number,
    InputReference,
}

/// A highlighted stretch of a line: the byte offsets it starts and ends at, and its kind.
type Token = (usize, usize, HighlightType);

/// Looks for one kind of token starting at a byte offset of a line (a character boundary),
/// and gives the offset it ends at.
type TokenAt = fn(&str, usize) -> Option<usize>;

/// The stretches of `line` to highlight, ordered by where they start.
///
/// Each kind of token is looked for on its own, over the whole line, and a token is dropped
/// when it lies entirely within one of a kind looked for before it: an operator inside a
/// string is part of the string.
fn tokens(line: &str) -> Vec<Token> {
    let mut found: Vec<Token> = Vec::new();

    // Comments first (they have highest priority), then strings (which can contain what
    // would otherwise be numbers, keywords or operators).
    let kinds: [(HighlightType, TokenAt); 6] = [
        (HighlightType::Comment, comment_at),
        (HighlightType::String, string_at),
        (HighlightType::Number, number_at),
        (HighlightType::InputReference, input_reference_at),
        (HighlightType::Keyword, keyword_at),
        (HighlightType::Operator, operator_at),
    ];

    for (kind, token_at) in kinds {
        let known = found.len();
        let mut at = 0;

        while at < line.len() {
            let Some(end) = token_at(line, at) else {
                at += 1;
                continue;
            };

            let inside = found[..known]
                .iter()
                .any(|&(start, known_end, _)| at >= start && end <= known_end);
            if !inside {
                found.push((at, end, kind));
            }
            at = end;
        }
    }

    found.sort_by_key(|&(start, _, _)| start);

    found
}

/// Whether a character can be part of a word. A keyword, a number or an operator that is
/// spelled as a word only counts when no such character is directly next to it.
fn is_word(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

/// Whether the character before byte offset `at` (a character boundary) is part of a word.
fn word_before(line: &str, at: usize) -> bool {
    line[..at].chars().next_back().is_some_and(is_word)
}

/// Whether the character at byte offset `at` (a character boundary) is part of a word.
fn word_after(line: &str, at: usize) -> bool {
    line[at..].chars().next().is_some_and(is_word)
}

/// The end of the run of ASCII digits starting at `from`.
fn digits_end(bytes: &[u8], from: usize) -> usize {
    let digits = bytes.get(from..).unwrap_or_default();

    from + digits
        .iter()
        .take_while(|byte| byte.is_ascii_digit())
        .count()
}

/// The end of the comment starting at `at`: `//` up to the end of the text, on its last line.
fn comment_at(line: &str, at: usize) -> Option<usize> {
    let rest = line.as_bytes().get(at..)?;

    (rest.starts_with(b"//") && !rest.contains(&b'\n')).then_some(line.len())
}

/// The end of the string starting at `at`: everything up to the next quote of the same kind.
fn string_at(line: &str, at: usize) -> Option<usize> {
    let bytes = line.as_bytes();
    let quote = bytes[at];
    if quote != b'"' && quote != b'\'' {
        return None;
    }

    let length = bytes[at + 1..].iter().position(|byte| *byte == quote)?;

    Some(at + 1 + length + 1)
}

/// The end of the number starting at `at`: digits, optionally followed by a fraction and by
/// an exponent, standing on their own. Where the longest such form runs into a word, a
/// shorter one that does not is taken instead (the `1` of `1.5x`).
fn number_at(line: &str, at: usize) -> Option<usize> {
    let bytes = line.as_bytes();
    if !bytes[at].is_ascii_digit() || word_before(line, at) {
        return None;
    }

    let exponent_end = |from: usize| {
        if !matches!(bytes.get(from), Some(b'e' | b'E')) {
            return None;
        }

        let digits = from + 1 + usize::from(matches!(bytes.get(from + 1), Some(b'+' | b'-')));
        let end = digits_end(bytes, digits);

        (end > digits).then_some(end)
    };

    let whole = digits_end(bytes, at);
    let fraction = (bytes.get(whole) == Some(&b'.'))
        .then(|| digits_end(bytes, whole + 1))
        .filter(|end| *end > whole + 1);

    [
        fraction.and_then(exponent_end),
        fraction,
        exponent_end(whole),
        Some(whole),
    ]
    .into_iter()
    .flatten()
    .find(|end| !word_after(line, *end))
}

/// The end of the input reference starting at `at`: `#` and an identifier.
fn input_reference_at(line: &str, at: usize) -> Option<usize> {
    let bytes = line.as_bytes();
    let starts_name = |byte: &u8| byte.is_ascii_alphabetic() || *byte == b'_';
    if bytes[at] != b'#' || !bytes.get(at + 1).is_some_and(starts_name) {
        return None;
    }

    let name = &bytes[at + 2..];
    let length = name
        .iter()
        .take_while(|byte| byte.is_ascii_alphanumeric() || **byte == b'_')
        .count();

    Some(at + 2 + length)
}

/// The end of the one of `words` that starts at `at` and stands on its own.
fn word_at(line: &str, at: usize, words: &[&str]) -> Option<usize> {
    if !line.as_bytes()[at].is_ascii_lowercase() || word_before(line, at) {
        return None;
    }

    words
        .iter()
        .filter(|word| line[at..].starts_with(**word))
        .map(|word| at + word.len())
        .find(|end| !word_after(line, *end))
}

/// The end of the keyword starting at `at`.
fn keyword_at(line: &str, at: usize) -> Option<usize> {
    word_at(line, at, &["if", "then", "else", "output"])
}

/// The end of the operator (or piece of punctuation) starting at `at`. Every dot of a spread
/// is an operator of its own.
fn operator_at(line: &str, at: usize) -> Option<usize> {
    let bytes = line.as_bytes();
    let next = bytes.get(at + 1).copied();

    let length = match bytes[at] {
        b'=' if matches!(next, Some(b'>' | b'=')) => 2,
        b'!' | b'<' | b'>' if next == Some(b'=') => 2,
        b'&' | b'|' | b'?' if next == Some(bytes[at]) => 2,
        b'+' | b'-' | b'*' | b'/' | b'%' | b'^' | b'=' | b'!' | b'<' | b'>' | b'.' | b'{'
        | b'}' | b'[' | b']' | b'(' | b')' | b',' | b'?' => 1,
        _ => return word_at(line, at, &["and", "or", "via", "into", "where"]),
    };

    Some(at + length)
}

impl Highlighter for BlotsHighlighter {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> std::borrow::Cow<'l, str> {
        self.highlight_line(line).into()
    }

    fn highlight_char(
        &self,
        _line: &str,
        _pos: usize,
        _forced: rustyline::highlight::CmdKind,
    ) -> bool {
        true // Always highlight
    }
}

impl Hinter for BlotsHighlighter {
    type Hint = String;

    fn hint(&self, _line: &str, _pos: usize, _ctx: &Context<'_>) -> Option<String> {
        None
    }
}

impl Completer for BlotsHighlighter {
    type Candidate = String;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &Context<'_>,
    ) -> rustyline::Result<(usize, Vec<String>)> {
        let maybe_last_word = line.split_whitespace().last();

        // If the cursor is at the last position, try matching for completions
        if let Some(last_word) = maybe_last_word
            && let Some(last_word_pos) = line.match_indices(last_word).last()
            && pos == line.len()
        {
            let rel_pos = pos - last_word_pos.0;

            // First, collect completions from bindings (higher priority)
            let mut completions: Vec<String> = self
                .bindings
                .keys()
                .map(|key| key.as_str())
                .filter(|key| key.starts_with(last_word))
                .map(|key| key[rel_pos..].to_string())
                .collect();

            // Then, add built-in function completions (lower priority)
            let builtin_completions: Vec<String> = BUILTIN_FUNCTION_NAMES
                .iter()
                .filter(|&name| name.starts_with(last_word))
                .map(|&name| name[rel_pos..].to_string())
                .collect();

            completions.extend(builtin_completions);

            return Ok((pos, completions));
        }

        Ok((pos, vec![]))
    }
}

impl Validator for BlotsHighlighter {
    fn validate(
        &self,
        _ctx: &mut rustyline::validate::ValidationContext,
    ) -> rustyline::Result<rustyline::validate::ValidationResult> {
        Ok(rustyline::validate::ValidationResult::Valid(None))
    }
}

impl Helper for BlotsHighlighter {}

#[cfg(test)]
mod tests {
    use super::HighlightType::{Comment, InputReference, Keyword, Number, Operator, String};
    use super::*;

    /// The highlighted stretches of `line`: the text of each, and its kind.
    fn highlights(line: &str) -> Vec<(&str, HighlightType)> {
        tokens(line)
            .into_iter()
            .map(|(start, end, kind)| (&line[start..end], kind))
            .collect()
    }

    #[test]
    fn finds_each_kind_of_token() {
        assert_eq!(
            highlights("output total = #price * 1.5e3 // tax"),
            [
                ("output", Keyword),
                ("=", Operator),
                ("#price", InputReference),
                ("*", Operator),
                ("1.5e3", Number),
                ("// tax", Comment),
            ]
        );
        assert_eq!(
            highlights("xs via (x, i) => x ?? 'none' into sum where true"),
            [
                ("via", Operator),
                ("(", Operator),
                (",", Operator),
                (")", Operator),
                ("=>", Operator),
                ("??", Operator),
                ("'none'", String),
                ("into", Operator),
                ("where", Operator),
            ]
        );
        assert_eq!(
            highlights("a && b || !c == d => e <= f < g % h ^ i"),
            [
                ("&&", Operator),
                ("||", Operator),
                ("!", Operator),
                ("==", Operator),
                ("=>", Operator),
                ("<=", Operator),
                ("<", Operator),
                ("%", Operator),
                ("^", Operator),
            ]
        );
    }

    #[test]
    fn leaves_the_inside_of_strings_and_comments_alone() {
        assert_eq!(
            highlights("if x >= 10 and y != \"a or b\" then x else y"),
            [
                ("if", Keyword),
                (">=", Operator),
                ("10", Number),
                ("and", Operator),
                ("!=", Operator),
                ("\"a or b\"", String),
                ("then", Keyword),
                ("else", Keyword),
            ]
        );
        assert_eq!(
            highlights("1 // \"a\" + 2"),
            [("1", Number), ("// \"a\" + 2", Comment)]
        );
        assert_eq!(
            highlights("'it''s' \"unterminated"),
            [("'it'", String), ("'s'", String)]
        );
    }

    #[test]
    fn only_takes_words_and_numbers_that_stand_alone() {
        assert_eq!(highlights("iffy android x1 1x output_ _if"), []);
        assert_eq!(
            highlights("é1 + 日if - 2"),
            [("+", Operator), ("-", Operator), ("2", Number)]
        );
        assert_eq!(
            highlights("#a #_b1 #1 # x"),
            [
                ("#a", InputReference),
                ("#_b1", InputReference),
                ("1", Number)
            ]
        );
    }

    #[test]
    fn falls_back_to_a_shorter_number_before_a_word() {
        assert_eq!(
            highlights("1.5x 3. 2e+x 4e-2 5E6.7"),
            [
                ("1", Number),
                (".", Operator),
                ("3", Number),
                (".", Operator),
                ("+", Operator),
                ("4e-2", Number),
                ("5E6", Number),
                (".", Operator),
                ("7", Number),
            ]
        );
    }

    #[test]
    fn treats_each_dot_of_a_spread_as_an_operator() {
        assert_eq!(
            highlights("[...xs, {a: 1}]"),
            [
                ("[", Operator),
                (".", Operator),
                (".", Operator),
                (".", Operator),
                (",", Operator),
                ("{", Operator),
                ("1", Number),
                ("}", Operator),
                ("]", Operator),
            ]
        );
    }

    #[test]
    fn only_finds_a_comment_on_the_last_line() {
        assert_eq!(
            highlights("1 // a\n2 // b"),
            [
                ("1", Number),
                ("/", Operator),
                ("/", Operator),
                ("2", Number),
                ("// b", Comment),
            ]
        );
    }
}
