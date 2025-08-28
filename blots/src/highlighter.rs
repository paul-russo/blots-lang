use regex::Regex;
use rustyline::{
    completion::Completer, highlight::Highlighter, hint::Hinter, validate::Validator, Context,
    Helper,
};
use yansi::{Color, Paint};

pub struct BlotsHighlighter {
    // Regex patterns for different syntax elements
    comment_regex: Regex,
    keyword_regex: Regex,
    operator_regex: Regex,
    string_regex: Regex,
    number_regex: Regex,
}

impl BlotsHighlighter {
    pub fn new() -> Self {
        Self {
            // Comments: // to end of line
            comment_regex: Regex::new(r"//.*$").unwrap(),

            // Keywords: if, then, else, output, with
            keyword_regex: Regex::new(r"\b(if|then|else|output|with)\b").unwrap(),

            // Operators: all the operators from the grammar
            operator_regex: Regex::new(r"(=>|\+|-|\*|/|%|\^|==|!=|<=|<|>=|>|&&|\|\||\?\?|!|\.|=|\{|\}|\[|\]|\(|\)|,|\?|\.\.\.|\band\b|\bor\b)").unwrap(),

            // Strings: both single and double quoted (simplified for now)
            string_regex: Regex::new(r#""[^"]*"|'[^']*'"#).unwrap(),

            // Numbers: including decimals and scientific notation
            number_regex: Regex::new(r"\b\d+(\.\d+)?([eE][+-]?\d+)?\b").unwrap(),
        }
    }

    fn highlight_line(&self, line: &str) -> String {
        let mut highlighted = String::new();
        let mut last_end = 0;

        // Collect all matches with their positions and types
        let mut matches: Vec<(usize, usize, HighlightType)> = Vec::new();

        // Find comments first (they have highest priority)
        for mat in self.comment_regex.find_iter(line) {
            matches.push((mat.start(), mat.end(), HighlightType::Comment));
        }

        // Find strings (high priority, can contain operators/keywords)
        for mat in self.string_regex.find_iter(line) {
            // Only add if not inside a comment
            if !self.is_inside_ranges(&matches, mat.start(), mat.end()) {
                matches.push((mat.start(), mat.end(), HighlightType::String));
            }
        }

        // Find numbers
        for mat in self.number_regex.find_iter(line) {
            if !self.is_inside_ranges(&matches, mat.start(), mat.end()) {
                matches.push((mat.start(), mat.end(), HighlightType::Number));
            }
        }

        // Find keywords
        for mat in self.keyword_regex.find_iter(line) {
            if !self.is_inside_ranges(&matches, mat.start(), mat.end()) {
                matches.push((mat.start(), mat.end(), HighlightType::Keyword));
            }
        }

        // Find operators
        for mat in self.operator_regex.find_iter(line) {
            if !self.is_inside_ranges(&matches, mat.start(), mat.end()) {
                matches.push((mat.start(), mat.end(), HighlightType::Operator));
            }
        }

        // Sort matches by start position
        matches.sort_by_key(|&(start, _, _)| start);

        // Apply highlighting
        for (start, end, highlight_type) in matches {
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

    fn is_inside_ranges(
        &self,
        ranges: &[(usize, usize, HighlightType)],
        start: usize,
        end: usize,
    ) -> bool {
        ranges
            .iter()
            .any(|&(range_start, range_end, _)| start >= range_start && end <= range_end)
    }

    /// Format a result string with syntax highlighting for display
    pub fn highlight_result(&self, result: &str) -> String {
        format!("= {}", self.highlight_line(result))
    }
}

#[derive(Debug, Clone, Copy)]
enum HighlightType {
    Comment,
    Keyword,
    Operator,
    String,
    Number,
}

impl Highlighter for BlotsHighlighter {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> std::borrow::Cow<'l, str> {
        self.highlight_line(line).into()
    }

    fn highlight_char(&self, _line: &str, _pos: usize, _forced: bool) -> bool {
        true // Always highlight
    }
}

impl Hinter for BlotsHighlighter {
    type Hint = String;

    fn hint(&self, _line: &str, _pos: usize, _ctx: &Context<'_>) -> Option<String> {
        None // No hints for now
    }
}

impl Completer for BlotsHighlighter {
    type Candidate = String;

    fn complete(
        &self,
        _line: &str,
        _pos: usize,
        _ctx: &Context<'_>,
    ) -> rustyline::Result<(usize, Vec<String>)> {
        Ok((0, vec![])) // No completion for now
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
