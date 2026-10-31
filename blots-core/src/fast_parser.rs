//! A hand-written parser for the programs people actually write.
//!
//! `grammar.pest` remains the definition of the language. This module reads the same programs
//! many times faster by building the AST in one pass over the bytes, and it only answers for
//! input it is certain about: [`parse_program`] returns `None` when the input is malformed or
//! leans on a corner of the grammar that is not reproduced here, and the caller then parses
//! with pest, which also produces the error messages. When a program is returned, its
//! statements, nodes and spans are exactly the ones pest followed by `pairs_to_expr` produce.
//!
//! The grammar is a PEG, so it is already a recursive-descent parser on paper: ordered choice,
//! greedy repetition, and backtracking only where a choice or a repetition gives up. Each
//! function below follows one rule and notes the rule it follows. Most of what looks like
//! arbitrary strictness (no space inside `a[1]`, a trailing comma in a call only before a line
//! break) is the grammar's own behavior, kept so that both parsers accept the same programs.

use crate::ast::{
    BinaryOp, Commented, Expr, PostfixOp, RecordEntry, RecordKey, Span, Spanned, SpannedExpr,
    UnaryOp,
};
use crate::expressions::lambda_expr;
use crate::functions::BuiltInFunction;
use crate::intern::Symbol;
use crate::values::LambdaArg;

/// One top-level statement of a program.
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    /// An expression, evaluated for its effect on the bindings.
    Expression(SpannedExpr),

    /// `output name` or `output name = value`. `expr` is the bare name or the assignment.
    Output {
        name: Symbol,
        expr: SpannedExpr,
        assigns: bool,
    },

    /// A line holding nothing but a comment.
    Comment,
}

/// Parse a whole program, or return `None` to have the caller parse it with pest instead.
pub fn parse_program(source: &str) -> Option<Vec<Statement>> {
    Parser::new(source).program()
}

/// Nesting beyond this is left to pest, so that this parser never decides how deep is too deep.
const MAX_DEPTH: usize = 128;

/// Why an attempt to parse something produced no node.
#[derive(Debug, Clone, Copy, PartialEq)]
enum Stop {
    /// Nothing that could begin an operand is here. The grammar fails the same way, cleanly,
    /// so an enclosing repetition may end and parsing carry on from before the attempt.
    NoMatch,

    /// The input needs the grammar itself to decide; give up on the whole program.
    Defer,
}

type Parse<T> = Result<T, Stop>;

/// A binary operator found after an operand, together with where its right operand begins.
#[derive(Clone, Copy)]
struct Infix {
    op: BinaryOp,
    precedence: u8,
    right_associative: bool,
    span: Span,
    operand_at: usize,
}

/// What an opening parenthesis at the start of a term turned out to begin.
enum Parenthesized {
    /// A lambda's parameter list; `body_at` is just past the `=>`.
    Parameters { body_at: usize },

    /// Not a parameter list, so a parenthesized expression if anything.
    Expression,

    /// A parameter list only the grammar should judge.
    Unclear,
}

struct Parser<'s> {
    source: &'s str,
    bytes: &'s [u8],
    pos: usize,

    /// Whether columns can be counted in bytes.
    ascii: bool,

    /// Byte offsets at which lines start, for every line break scanned so far.
    line_starts: Vec<usize>,

    depth: usize,

    /// The operator search is a pure function of the position it starts from, and enclosing
    /// expressions repeat it at the position where an inner one stopped, so remember the last.
    peeked_at: usize,
    peeked: Option<Infix>,

    /// A position after which an operator was found but no operand followed it.
    no_infix_at: usize,
}

fn is_space(byte: u8) -> bool {
    byte == b' ' || byte == b'\t'
}

fn is_digit(byte: u8) -> bool {
    byte.is_ascii_digit()
}

fn is_word_start(byte: u8) -> bool {
    byte.is_ascii_alphabetic() || byte == b'_'
}

fn is_word(byte: u8) -> bool {
    byte.is_ascii_alphanumeric() || byte == b'_'
}

/// `reserved_word`: the words `identifier` refuses.
fn is_reserved(word: &[u8]) -> bool {
    matches!(
        word,
        b"if"
            | b"then"
            | b"else"
            | b"true"
            | b"false"
            | b"null"
            | b"and"
            | b"or"
            | b"not"
            | b"do"
            | b"return"
            | b"output"
    )
}

impl<'s> Parser<'s> {
    fn new(source: &'s str) -> Self {
        Self {
            source,
            bytes: source.as_bytes(),
            pos: 0,
            ascii: source.is_ascii(),
            line_starts: vec![0],
            depth: 0,
            peeked_at: usize::MAX,
            peeked: None,
            no_infix_at: usize::MAX,
        }
    }

    /// The byte at `index`, or NUL past the end. NUL begins nothing in the grammar, so the two
    /// cases never need telling apart except where the end of input is itself acceptable.
    #[inline(always)]
    fn at(&self, index: usize) -> u8 {
        if index < self.bytes.len() {
            self.bytes[index]
        } else {
            0
        }
    }

    #[inline(always)]
    fn starts_with(&self, index: usize, text: &[u8]) -> bool {
        self.bytes
            .get(index..)
            .is_some_and(|rest| rest.starts_with(text))
    }

    fn at_comment(&self, index: usize) -> bool {
        self.at(index) == b'/' && self.at(index + 1) == b'/'
    }

    // ---- Positions -------------------------------------------------------------------------

    fn note_line(&mut self, start: usize) {
        if start > self.line_starts[self.line_starts.len() - 1] {
            self.line_starts.push(start);
        }
    }

    /// The 1-based line and column of `index`, with columns counted in characters as pest
    /// counts them. Every line break before `index` has been scanned by the time this is asked.
    fn line_col(&self, index: usize) -> (usize, usize) {
        let mut line = self.line_starts.len() - 1;
        while self.line_starts[line] > index {
            line -= 1;
        }

        let line_start = self.line_starts[line];
        let column = if self.ascii {
            index - line_start
        } else {
            self.source[line_start..index].chars().count()
        };

        (line + 1, column + 1)
    }

    fn span(&self, start: usize, end: usize) -> Span {
        let (line, column) = self.line_col(start);

        Span::new(start, end, line, column)
    }

    // ---- Scanning --------------------------------------------------------------------------

    /// `WHITESPACE*`
    #[inline(always)]
    fn spaces_end(&self, mut index: usize) -> usize {
        while is_space(self.at(index)) {
            index += 1;
        }

        index
    }

    fn word_end(&self, mut index: usize) -> usize {
        while is_word(self.at(index)) {
            index += 1;
        }

        index
    }

    fn digits_end(&self, mut index: usize) -> usize {
        while is_digit(self.at(index)) {
            index += 1;
        }

        index
    }

    /// `identifier`: where the identifier starting at `index` ends, if one starts there.
    fn identifier_end(&self, index: usize) -> Option<usize> {
        if !is_word_start(self.at(index)) {
            return None;
        }

        let end = self.word_end(index);
        if is_reserved(&self.bytes[index..end]) {
            return None;
        }

        Some(end)
    }

    /// `plain_newline`: where the line break starting at `index` ends, if one starts there.
    fn newline_end(&self, index: usize) -> Option<usize> {
        match self.at(index) {
            b'\n' => Some(index + 1),
            b'\r' if self.at(index + 1) == b'\n' => Some(index + 2),
            _ => None,
        }
    }

    /// `comment`: where the comment starting at `index` ends, which is before the line break
    /// that follows it or at the end of the input.
    fn comment_end(&self, index: usize) -> usize {
        let text = index + 2;
        let Some(offset) = self.bytes[text..].iter().position(|&byte| byte == b'\n') else {
            return self.bytes.len();
        };

        let newline = text + offset;
        if newline > text && self.bytes[newline - 1] == b'\r' {
            newline - 1
        } else {
            newline
        }
    }

    /// `(WHITESPACE | NEWLINE)*`: spaces, line breaks, and comments that a line break follows.
    /// Returns where that ends and whether it crossed a line break.
    fn trivia_end(&mut self, mut index: usize) -> (usize, bool) {
        let mut crossed_line = false;

        loop {
            match self.at(index) {
                b' ' | b'\t' => index += 1,
                b'\n' | b'\r' | b'/' => {
                    let line_end = if self.at_comment(index) {
                        self.comment_end(index)
                    } else {
                        index
                    };
                    let Some(next) = self.newline_end(line_end) else {
                        break;
                    };

                    index = next;
                    crossed_line = true;
                    self.note_line(next);
                }
                _ => break,
            }
        }

        (index, crossed_line)
    }

    /// `(WHITESPACE | plain_newline)*`: spaces and line breaks, but no comments.
    fn blank_end(&mut self, mut index: usize) -> usize {
        loop {
            if is_space(self.at(index)) {
                index += 1;
            } else if let Some(next) = self.newline_end(index) {
                index = next;
                self.note_line(next);
            } else {
                return index;
            }
        }
    }

    /// `NEWLINE*` where the grammar skips no spaces: line breaks, each optionally preceded by a
    /// comment, and nothing else.
    fn line_breaks_end(&mut self, mut index: usize) -> usize {
        loop {
            let line_end = if self.at_comment(index) {
                self.comment_end(index)
            } else {
                index
            };
            let Some(next) = self.newline_end(line_end) else {
                return index;
            };

            index = next;
            self.note_line(next);
        }
    }

    // ---- Statements ------------------------------------------------------------------------

    /// `input = SOI ~ statement? ~ (NEWLINE ~ statement?)* ~ EOI`
    fn program(mut self) -> Option<Vec<Statement>> {
        let mut statements = Vec::new();

        loop {
            self.pos = self.spaces_end(self.pos);
            if self.pos == self.bytes.len() {
                break;
            }

            if let Some(next) = self.newline_end(self.pos) {
                self.pos = next;
                self.note_line(next);
                continue;
            }

            if self.at_comment(self.pos) {
                self.pos = self.comment_end(self.pos);
                statements.push(Statement::Comment);
            } else {
                statements.push(self.statement()?);

                let comment = self.spaces_end(self.pos);
                if self.at_comment(comment) {
                    self.pos = self.comment_end(comment);
                }
            }

            let line_end = self.spaces_end(self.pos);
            if line_end == self.bytes.len() {
                break;
            }

            let next = self.newline_end(line_end)?;
            self.pos = next;
            self.note_line(next);
        }

        Some(statements)
    }

    /// `output_declaration | expression`, the alternatives of `statement` that are not comments.
    fn statement(&mut self) -> Option<Statement> {
        let start = self.pos;
        if !(self.starts_with(start, b"output") && is_space(self.at(start + 6))) {
            return Some(Statement::Expression(self.expression(false).ok()?));
        }

        // output_declaration = "output" ~ WHITESPACE+ ~ (assignment | identifier)
        let name_start = self.spaces_end(start + 6);
        let name_end = self.identifier_end(name_start)?;
        let name = Symbol::intern(&self.source[name_start..name_end]);
        let equals = self.spaces_end(name_end);

        if self.at(equals) != b'=' {
            self.pos = name_end;

            return Some(Statement::Output {
                name,
                expr: self.name(name_start, name_end),
                assigns: false,
            });
        }

        let (line, column) = self.line_col(name_start);
        self.pos = self.spaces_end(equals + 1);
        let value = self.expression(false).ok()?;

        Some(Statement::Output {
            name,
            expr: Spanned::new(
                Expr::Assignment {
                    ident: name,
                    value: Box::new(value),
                },
                Span::new(name_start, self.pos, line, column),
            ),
            assigns: true,
        })
    }

    // ---- Expressions -----------------------------------------------------------------------

    /// `expression`, or `lambda_expression` when `in_lambda`: operands joined by binary
    /// operators. The grammar matches them as a flat sequence and a Pratt parser then groups
    /// them by precedence; climbing precedence while reading gives the same tree.
    fn expression(&mut self, in_lambda: bool) -> Parse<SpannedExpr> {
        self.binary(0, in_lambda)
    }

    /// An expression that has to be here: with nothing else the grammar could do at this
    /// point, a missing operand is not something to recover from.
    fn required_expression(&mut self) -> Parse<SpannedExpr> {
        self.expression(false).map_err(|_| Stop::Defer)
    }

    /// An operand and the operators after it that bind tighter than `min_precedence`.
    fn binary(&mut self, min_precedence: u8, in_lambda: bool) -> Parse<SpannedExpr> {
        if self.depth == MAX_DEPTH {
            return Err(Stop::Defer);
        }

        self.depth += 1;
        let result = self
            .operand()
            .and_then(|left| self.infix_chain(left, min_precedence, in_lambda));
        self.depth -= 1;

        result
    }

    fn infix_chain(
        &mut self,
        mut left: SpannedExpr,
        min_precedence: u8,
        in_lambda: bool,
    ) -> Parse<SpannedExpr> {
        while let Some(infix) = self.peek_infix(in_lambda) {
            if infix.precedence <= min_precedence {
                break;
            }

            let before = self.pos;
            self.pos = infix.operand_at;
            let right_min = if infix.right_associative {
                infix.precedence - 1
            } else {
                infix.precedence
            };

            match self.binary(right_min, in_lambda) {
                Ok(right) => {
                    left = Spanned::new(
                        Expr::BinaryOp {
                            op: infix.op,
                            left: Box::new(left),
                            right: Box::new(right),
                        },
                        infix.span,
                    );
                }
                // An operator with no operand after it ends the expression before the
                // operator: `where = 3` on the line after an expression is a new statement.
                Err(Stop::NoMatch) => {
                    self.pos = before;
                    self.no_infix_at = before;
                    break;
                }
                Err(Stop::Defer) => return Err(Stop::Defer),
            }
        }

        Ok(left)
    }

    /// The binary operator after the current position, if the expression continues with one.
    /// Does not move.
    fn peek_infix(&mut self, in_lambda: bool) -> Option<Infix> {
        let from = self.pos;
        if from == self.no_infix_at {
            return None;
        }

        if self.peeked_at != from {
            self.peeked = self.find_infix(from);
            self.peeked_at = from;
        }

        // lambda_natural_infix_op: a lambda's body does not continue with `via`, `into` or
        // `where`, which belong to the expression the lambda is an operand of.
        self.peeked.filter(|infix| {
            !(in_lambda && matches!(infix.op, BinaryOp::Via | BinaryOp::Into | BinaryOp::Where))
        })
    }

    /// `infix_usage = (WHITESPACE | NEWLINE)+ ~ natural_infix_op ~ WHITESPACE+
    ///              | (WHITESPACE | NEWLINE)* ~ infix_op ~ (WHITESPACE | NEWLINE)*`
    fn find_infix(&mut self, from: usize) -> Option<Infix> {
        let (start, _) = self.trivia_end(from);
        let first = self.at(start);

        if first.is_ascii_lowercase() {
            let (op, length) = match first {
                b'a' if self.starts_with(start, b"and") => (BinaryOp::NaturalAnd, 3),
                b'o' if self.starts_with(start, b"or") => (BinaryOp::NaturalOr, 2),
                b'v' if self.starts_with(start, b"via") => (BinaryOp::Via, 3),
                b'i' if self.starts_with(start, b"into") => (BinaryOp::Into, 4),
                b'w' if self.starts_with(start, b"where") => (BinaryOp::Where, 5),
                _ => return None,
            };

            let end = start + length;
            if start == from || !is_space(self.at(end)) {
                return None;
            }

            return Some(Infix {
                op,
                precedence: 1,
                right_associative: false,
                span: self.span(start, end),
                operand_at: self.spaces_end(end),
            });
        }

        let second = self.at(start + 1);
        let (op, length, precedence) = match first {
            b'+' => (BinaryOp::Add, 1, 3),
            b'-' => (BinaryOp::Subtract, 1, 3),
            b'*' => (BinaryOp::Multiply, 1, 4),
            // Two slashes here are a comment that runs to the end of the input. The grammar
            // reads the first as a division, finds no operand, and gives the operator up.
            b'/' if second != b'/' => (BinaryOp::Divide, 1, 4),
            b'%' => (BinaryOp::Modulo, 1, 4),
            b'^' => (BinaryOp::Power, 1, 5),
            b'.' => match (second, self.at(start + 2)) {
                (b'=', b'=') => (BinaryOp::DotEqual, 3, 2),
                (b'!', b'=') => (BinaryOp::DotNotEqual, 3, 2),
                (b'<', b'=') => (BinaryOp::DotLessEq, 3, 2),
                (b'<', _) => (BinaryOp::DotLess, 2, 2),
                (b'>', b'=') => (BinaryOp::DotGreaterEq, 3, 2),
                (b'>', _) => (BinaryOp::DotGreater, 2, 2),
                _ => return None,
            },
            b'=' if second == b'=' => (BinaryOp::Equal, 2, 2),
            b'!' if second == b'=' => (BinaryOp::NotEqual, 2, 2),
            b'<' if second == b'=' => (BinaryOp::LessEq, 2, 2),
            b'<' => (BinaryOp::Less, 1, 2),
            b'>' if second == b'=' => (BinaryOp::GreaterEq, 2, 2),
            b'>' => (BinaryOp::Greater, 1, 2),
            b'&' if second == b'&' => (BinaryOp::And, 2, 1),
            b'|' if second == b'|' => (BinaryOp::Or, 2, 1),
            b'?' if second == b'?' => (BinaryOp::Coalesce, 2, 6),
            _ => return None,
        };

        let end = start + length;
        let span = self.span(start, end);
        let (operand_at, _) = self.trivia_end(end);

        Some(Infix {
            op,
            precedence,
            right_associative: op == BinaryOp::Power,
            span,
            operand_at,
        })
    }

    /// `prefix_usage* ~ term ~ postfix_op*`. Postfix operators bind tighter than prefix ones.
    fn operand(&mut self) -> Parse<SpannedExpr> {
        let start = self.pos;

        // prefix_usage = natural_prefix_op ~ WHITESPACE+ | prefix_op. Spaces must follow `not`
        // and must not follow `-` or `!`.
        let (op, length, operand_at) = match self.at(start) {
            b'-' => (UnaryOp::Negate, 1, start + 1),
            b'!' => (UnaryOp::Not, 1, start + 1),
            b'n' if self.starts_with(start, b"not") && is_space(self.at(start + 3)) => {
                (UnaryOp::Not, 3, self.spaces_end(start + 3))
            }
            _ => {
                let term = self.term()?;

                return self.postfix(term);
            }
        };

        if self.depth == MAX_DEPTH {
            return Err(Stop::Defer);
        }

        let span = self.span(start, start + length);
        self.pos = operand_at;
        self.depth += 1;
        let operand = self.operand();
        self.depth -= 1;

        Ok(Spanned::new(
            Expr::UnaryOp {
                op,
                expr: Box::new(operand?),
            },
            span,
        ))
    }

    /// `postfix_op* = (factorial | access | call_list | dot_access)*`
    fn postfix(&mut self, mut expr: SpannedExpr) -> Parse<SpannedExpr> {
        loop {
            let start = self.pos;

            expr = match self.at(start) {
                b'(' => self.call(expr)?,
                b'[' => self.access(expr)?,
                b'.' => {
                    // dot_access = "." ~ identifier
                    let Some(end) = self.identifier_end(start + 1) else {
                        return Ok(expr);
                    };

                    self.pos = end;
                    Spanned::new(
                        Expr::DotAccess {
                            expr: Box::new(expr),
                            field: self.source[start + 1..end].to_string(),
                        },
                        self.span(start, end),
                    )
                }
                b'!' => {
                    self.pos = start + 1;
                    Spanned::new(
                        Expr::PostfixOp {
                            op: PostfixOp::Factorial,
                            expr: Box::new(expr),
                        },
                        self.span(start, start + 1),
                    )
                }
                _ => return Ok(expr),
            };
        }
    }

    /// `call_list = "(" ~ NEWLINE* ~ (spreadable_expression ~ ("," ~ NEWLINE* ~
    /// spreadable_expression)*)? ~ ("," ~ NEWLINE)? ~ NEWLINE* ~ ")"`, with spaces skipped
    /// between its parts.
    fn call(&mut self, func: SpannedExpr) -> Parse<SpannedExpr> {
        let start = self.pos;
        let (line, column) = self.line_col(start);
        let mut args = Vec::new();
        let (mut index, _) = self.trivia_end(start + 1);

        while self.at(index) != b')' {
            self.pos = index;
            args.push(self.spreadable()?);
            index = self.spaces_end(self.pos);

            if self.at(index) != b',' {
                (index, _) = self.trivia_end(index);
                if self.at(index) != b')' {
                    return Err(Stop::Defer);
                }

                break;
            }

            // A trailing comma is only one when a line break follows it.
            let (next, crossed_line) = self.trivia_end(index + 1);
            index = next;
            if self.at(index) == b')' && !crossed_line {
                return Err(Stop::Defer);
            }
        }

        self.pos = index + 1;

        Ok(Spanned::new(
            Expr::Call {
                func: Box::new(func),
                args,
            },
            Span::new(start, self.pos, line, column),
        ))
    }

    /// `access = "[" ~ NEWLINE* ~ expression ~ NEWLINE* ~ "]"`, where no spaces are skipped.
    fn access(&mut self, expr: SpannedExpr) -> Parse<SpannedExpr> {
        let start = self.pos;
        let (line, column) = self.line_col(start);

        self.pos = self.line_breaks_end(start + 1);
        let index = self.required_expression()?;
        let close = self.line_breaks_end(self.pos);
        if self.at(close) != b']' {
            return Err(Stop::Defer);
        }

        self.pos = close + 1;

        Ok(Spanned::new(
            Expr::Access {
                expr: Box::new(expr),
                index: Box::new(index),
            },
            Span::new(start, self.pos, line, column),
        ))
    }

    /// `spreadable_expression = spread_expression | expression`
    fn spreadable(&mut self) -> Parse<SpannedExpr> {
        let start = self.pos;
        if !self.starts_with(start, b"...") {
            return self.required_expression();
        }

        let span = self.span(start, start + 3);
        self.pos = start + 3;
        let spread = self.required_expression()?;

        Ok(Spanned::new(Expr::Spread(Box::new(spread)), span))
    }

    // ---- Terms -----------------------------------------------------------------------------

    /// `term = conditional | do_block | lambda | assignment | list | record | bool | string |
    /// null | input_reference | identifier | number | nested_expression`, told apart by how
    /// each can begin.
    fn term(&mut self) -> Parse<SpannedExpr> {
        let start = self.pos;

        match self.at(start) {
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.word(start),
            b'0'..=b'9' => self.number(start),
            b'"' | b'\'' => self.string(start),
            b'(' => self.parenthesized(start),
            b'[' => self.list(start),
            b'{' => self.record(start),
            b'#' => self.input_reference(start),
            b'+' | b'.' if is_digit(self.at(start + 1)) => self.number(start),
            b'.' if self.starts_with(start, b"...") => self.rest_lambda(start),
            _ => Err(Stop::NoMatch),
        }
    }

    /// A term that begins with a word: a keyword's construct, a literal, a lambda with a single
    /// bare parameter, an assignment, or a name.
    fn word(&mut self, start: usize) -> Parse<SpannedExpr> {
        let end = self.word_end(start);
        let bytes = self.bytes;
        let word = &bytes[start..end];

        match word {
            b"if" => return self.conditional(start),
            b"do" => return self.do_block(start),
            b"true" => return Ok(self.literal(Expr::Bool(true), start, end)),
            b"false" => return Ok(self.literal(Expr::Bool(false), start, end)),
            b"null" => return Ok(self.literal(Expr::Null, start, end)),
            _ if is_reserved(word) => return Err(Stop::Defer),
            _ => {}
        }

        let next = self.spaces_end(end);
        match (self.at(next), self.at(next + 1)) {
            // lambda = argument_list ~ WHITESPACE* ~ "=>" ~ ..., where the list is one argument
            (b'=', b'>') => {
                let name = Symbol::intern(&self.source[start..end]);

                return self.lambda(start, vec![LambdaArg::Required(name)], next + 2);
            }
            (b'?', _) => {
                let arrow = self.spaces_end(next + 1);
                if self.starts_with(arrow, b"=>") {
                    let name = Symbol::intern(&self.source[start..end]);

                    return self.lambda(start, vec![LambdaArg::Optional(name)], arrow + 2);
                }
            }
            // `==` after a name is a comparison: an assignment's value cannot begin with `=`.
            (b'=', b'=') => {}
            // assignment = identifier ~ "=" ~ expression
            (b'=', _) => {
                let (line, column) = self.line_col(start);
                self.pos = self.spaces_end(next + 1);
                let value = self.required_expression()?;

                return Ok(Spanned::new(
                    Expr::Assignment {
                        ident: Symbol::intern(&self.source[start..end]),
                        value: Box::new(value),
                    },
                    Span::new(start, self.pos, line, column),
                ));
            }
            _ => {}
        }

        // `bool` and `null` come before `identifier` in `term` and match without looking at
        // what follows, so a name that merely begins with one of them is read as the literal
        // followed by leftovers. Leave that to the grammar.
        if word.starts_with(b"true") || word.starts_with(b"false") || word.starts_with(b"null") {
            return Err(Stop::Defer);
        }

        self.pos = end;

        Ok(self.name(start, end))
    }

    fn literal(&mut self, node: Expr, start: usize, end: usize) -> SpannedExpr {
        self.pos = end;

        Spanned::new(node, self.span(start, end))
    }

    /// A reference to a name: a built-in function if it is one, a variable otherwise.
    fn name(&self, start: usize, end: usize) -> SpannedExpr {
        let name = &self.source[start..end];
        let node = match BuiltInFunction::from_ident(name) {
            Some(built_in) => Expr::BuiltIn(built_in),
            None => Expr::Identifier(Symbol::intern(name)),
        };

        Spanned::new(node, self.span(start, end))
    }

    /// The rest of a lambda once its parameters are known:
    /// `"=>" ~ (WHITESPACE | NEWLINE)* ~ lambda_expression`, from just past the arrow.
    fn lambda(&mut self, start: usize, args: Vec<LambdaArg>, body_at: usize) -> Parse<SpannedExpr> {
        let (line, column) = self.line_col(start);
        (self.pos, _) = self.trivia_end(body_at);
        let body = self.expression(true).map_err(|_| Stop::Defer)?;

        Ok(lambda_expr(
            args,
            body,
            Span::new(start, self.pos, line, column),
        ))
    }

    /// A lambda whose whole parameter list is one bare `...rest`.
    fn rest_lambda(&mut self, start: usize) -> Parse<SpannedExpr> {
        let name_start = self.spaces_end(start + 3);
        let name_end = self.identifier_end(name_start).ok_or(Stop::Defer)?;
        let arrow = self.spaces_end(name_end);
        if !self.starts_with(arrow, b"=>") {
            return Err(Stop::Defer);
        }

        let name = Symbol::intern(&self.source[name_start..name_end]);

        self.lambda(start, vec![LambdaArg::Rest(name)], arrow + 2)
    }

    /// `lambda` with a parenthesized parameter list, or else
    /// `nested_expression = "(" ~ (WHITESPACE | NEWLINE)* ~ expression ~ (WHITESPACE | NEWLINE)* ~ ")"`.
    fn parenthesized(&mut self, start: usize) -> Parse<SpannedExpr> {
        match self.parameters(start, None) {
            Parenthesized::Parameters { body_at } => {
                let mut args = Vec::new();
                self.parameters(start, Some(&mut args));

                self.lambda(start, args, body_at)
            }
            Parenthesized::Expression => {
                (self.pos, _) = self.trivia_end(start + 1);
                let expr = self.required_expression()?;
                let (close, _) = self.trivia_end(self.pos);
                if self.at(close) != b')' {
                    return Err(Stop::Defer);
                }

                self.pos = close + 1;

                Ok(expr)
            }
            Parenthesized::Unclear => Err(Stop::Defer),
        }
    }

    /// Decide whether the parenthesis at `start` opens a lambda's parameter list, which it does
    /// when the list is well formed and `=>` follows it, collecting the parameters into `args`
    /// when asked to. Does not move.
    ///
    /// `argument_list = "(" ~ NEWLINE* ~ (argument ~ ("," ~ NEWLINE* ~ argument)*)? ~
    /// ("," ~ NEWLINE)? ~ NEWLINE* ~ ")"`, with spaces skipped between its parts.
    fn parameters(&mut self, start: usize, mut args: Option<&mut Vec<LambdaArg>>) -> Parenthesized {
        let (mut index, _) = self.trivia_end(start + 1);

        // A comma may come before any parameter, and that is for the grammar to make sense of.
        if self.at(index) == b',' {
            return Parenthesized::Unclear;
        }

        while self.at(index) != b')' {
            // argument = optional_arg | required_arg | rest_arg
            let rest = self.starts_with(index, b"...");
            let name_start = if rest {
                self.spaces_end(index + 3)
            } else {
                index
            };
            let Some(name_end) = self.identifier_end(name_start) else {
                return Parenthesized::Expression;
            };

            let mark = self.spaces_end(name_end);
            let optional = !rest && self.at(mark) == b'?';
            index = if optional { mark + 1 } else { name_end };

            if let Some(args) = args.as_deref_mut() {
                let name = Symbol::intern(&self.source[name_start..name_end]);
                args.push(if rest {
                    LambdaArg::Rest(name)
                } else if optional {
                    LambdaArg::Optional(name)
                } else {
                    LambdaArg::Required(name)
                });
            }

            index = self.spaces_end(index);
            if self.at(index) != b',' {
                (index, _) = self.trivia_end(index);
                if self.at(index) != b')' {
                    return Parenthesized::Expression;
                }

                break;
            }

            // A trailing comma is only one when a line break follows it.
            let (next, crossed_line) = self.trivia_end(index + 1);
            index = next;
            if self.at(index) == b')' && !crossed_line {
                return Parenthesized::Expression;
            }
        }

        let arrow = self.spaces_end(index + 1);
        if self.starts_with(arrow, b"=>") {
            Parenthesized::Parameters { body_at: arrow + 2 }
        } else {
            Parenthesized::Expression
        }
    }

    /// `conditional = "if" ~ WHITESPACE+ ~ expression ~ (WHITESPACE | NEWLINE)+ ~ "then" ~
    /// (WHITESPACE | NEWLINE)+ ~ expression ~ (WHITESPACE | NEWLINE)+ ~ "else" ~
    /// (WHITESPACE | NEWLINE)+ ~ expression`
    fn conditional(&mut self, start: usize) -> Parse<SpannedExpr> {
        if !is_space(self.at(start + 2)) {
            return Err(Stop::Defer);
        }

        let (line, column) = self.line_col(start);
        self.pos = self.spaces_end(start + 2);
        let condition = self.required_expression()?;
        self.keyword(b"then")?;
        let then_expr = self.required_expression()?;
        self.keyword(b"else")?;
        let else_expr = self.required_expression()?;

        Ok(Spanned::new(
            Expr::Conditional {
                condition: Box::new(condition),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(else_expr),
            },
            Span::new(start, self.pos, line, column),
        ))
    }

    /// `(WHITESPACE | NEWLINE)+ ~ word ~ (WHITESPACE | NEWLINE)+`
    fn keyword(&mut self, word: &[u8]) -> Parse<()> {
        let (start, _) = self.trivia_end(self.pos);
        let end = start + word.len();
        if start == self.pos || !self.starts_with(start, word) {
            return Err(Stop::Defer);
        }

        let (next, _) = self.trivia_end(end);
        if next == end {
            return Err(Stop::Defer);
        }

        self.pos = next;

        Ok(())
    }

    /// `do_block = "do" ~ (WHITESPACE | plain_newline)+ ~ "{" ~ trivia ~
    /// (WHITESPACE* ~ do_statement ~ WHITESPACE* ~ (plain_newline+ | ";") ~ trivia)* ~
    /// trivia ~ return_statement ~ (WHITESPACE | plain_newline)* ~ "}"`
    fn do_block(&mut self, start: usize) -> Parse<SpannedExpr> {
        let brace = self.blank_end(start + 2);
        if brace == start + 2 || self.at(brace) != b'{' {
            return Err(Stop::Defer);
        }

        let (line, column) = self.line_col(start);
        let mut statements = Vec::new();
        (self.pos, _) = self.trivia_end(brace + 1);

        // `return` is reserved, so an expression cannot begin with it.
        while !self.starts_with(self.pos, b"return") || is_word(self.at(self.pos + 6)) {
            statements.push(Commented::new(self.required_expression()?));

            // do_statement = (expression | comment) ~ comment?, where no spaces are skipped: a
            // comment may follow a statement only when nothing separates the two.
            let comment_end = if self.at_comment(self.pos) {
                self.comment_end(self.pos)
            } else {
                self.pos
            };
            let separator = self.spaces_end(comment_end);
            let rest = match self.at(separator) {
                b';' => separator + 1,
                _ if self.newline_end(separator).is_some() => separator,
                _ => return Err(Stop::Defer),
            };

            (self.pos, _) = self.trivia_end(rest);
        }

        // return_statement = WHITESPACE* ~ "return" ~ WHITESPACE+ ~ expression
        let value = self.pos + 6;
        if !is_space(self.at(value)) {
            return Err(Stop::Defer);
        }

        self.pos = self.spaces_end(value);
        let return_expr = self.required_expression()?;
        let close = self.blank_end(self.pos);
        if self.at(close) != b'}' {
            return Err(Stop::Defer);
        }

        self.pos = close + 1;

        Ok(Spanned::new(
            Expr::DoBlock {
                statements,
                return_expr: Box::new(Commented::new(return_expr)),
            },
            Span::new(start, self.pos, line, column),
        ))
    }

    /// `list = "[]" | "[" ~ trivia ~ (list_item ~ ("," ~ trivia ~ list_item)*)? ~
    /// ("," ~ trivia)? ~ trivia ~ "]"`
    fn list(&mut self, start: usize) -> Parse<SpannedExpr> {
        let (line, column) = self.line_col(start);
        let mut items = Vec::new();
        let mut more = self.open(start, b']');

        while more {
            items.push(Commented::new(self.spreadable()?));
            more = self.next_item(b']')?;
        }

        Ok(Spanned::new(
            Expr::List(items),
            Span::new(start, self.pos, line, column),
        ))
    }

    /// `record = "{}" | "{" ~ trivia ~ (record_item ~ ("," ~ trivia ~ record_item)*)? ~
    /// ("," ~ trivia)? ~ trivia ~ "}"`
    fn record(&mut self, start: usize) -> Parse<SpannedExpr> {
        let (line, column) = self.line_col(start);
        let mut entries = Vec::new();
        let mut more = self.open(start, b'}');

        while more {
            entries.push(Commented::new(self.record_entry()?));
            more = self.next_item(b'}')?;
        }

        Ok(Spanned::new(
            Expr::Record(entries),
            Span::new(start, self.pos, line, column),
        ))
    }

    /// Step inside the bracket at `start`. Returns whether there is an item to read, having
    /// moved to it, or else having moved past the closing bracket.
    fn open(&mut self, start: usize, close: u8) -> bool {
        let (first, _) = self.trivia_end(start + 1);
        let empty = self.at(first) == close;
        self.pos = if empty { first + 1 } else { first };

        !empty
    }

    /// What follows an item of a list or a record: an optional comment on the item's line, then
    /// a comma or the closing bracket. Returns whether another item follows, having moved to
    /// it, or else having moved past the closing bracket.
    fn next_item(&mut self, close: u8) -> Parse<bool> {
        let mut index = self.spaces_end(self.pos);
        if self.at_comment(index) {
            index = self.comment_end(index);
        }

        let comma = self.at(index) == b',';
        (index, _) = self.trivia_end(if comma { index + 1 } else { index });

        let closed = self.at(index) == close;
        if !closed && !comma {
            return Err(Stop::Defer);
        }

        self.pos = if closed { index + 1 } else { index };

        Ok(!closed)
    }

    /// `record_pair | record_shorthand | spread_expression`, the kinds of `record_item`.
    fn record_entry(&mut self) -> Parse<RecordEntry> {
        let start = self.pos;

        let key = match self.at(start) {
            b'"' | b'\'' => {
                let end = self.string_end(start)?;
                self.pos = end + 1;

                RecordKey::Static(self.source[start + 1..end].to_string())
            }
            // record_key_dynamic = "[" ~ expression ~ "]", with spaces skipped between its parts
            b'[' => {
                self.pos = self.spaces_end(start + 1);
                let key = self.required_expression()?;
                let close = self.spaces_end(self.pos);
                if self.at(close) != b']' {
                    return Err(Stop::Defer);
                }

                self.pos = close + 1;

                RecordKey::Dynamic(Box::new(key))
            }
            b'.' if self.starts_with(start, b"...") => {
                return Ok(RecordEntry {
                    key: RecordKey::Spread(Box::new(self.spreadable()?)),
                    value: Spanned::dummy(Expr::Null),
                });
            }
            _ => {
                let end = self.identifier_end(start).ok_or(Stop::Defer)?;
                let name = self.source[start..end].to_string();
                self.pos = end;

                if self.at(self.spaces_end(end)) != b':' {
                    return Ok(RecordEntry {
                        key: RecordKey::Shorthand(name),
                        value: Spanned::dummy(Expr::Null),
                    });
                }

                RecordKey::Static(name)
            }
        };

        // record_pair = record_key ~ ":" ~ NEWLINE* ~ expression, with spaces skipped between
        // its parts
        let colon = self.spaces_end(self.pos);
        if self.at(colon) != b':' {
            return Err(Stop::Defer);
        }

        (self.pos, _) = self.trivia_end(colon + 1);
        let value = self.required_expression()?;

        Ok(RecordEntry { key, value })
    }

    // ---- Tokens ----------------------------------------------------------------------------

    /// `string = PUSH("\"" | "'") ~ string_value ~ POP`: where the closing quote of the string
    /// starting at `start` is. There are no escapes, and a string may span lines.
    fn string_end(&mut self, start: usize) -> Parse<usize> {
        let quote = self.bytes[start];
        let mut index = start + 1;

        loop {
            let byte = *self.bytes.get(index).ok_or(Stop::Defer)?;
            if byte == quote {
                return Ok(index);
            }

            index += 1;
            if byte == b'\n' {
                self.note_line(index);
            }
        }
    }

    fn string(&mut self, start: usize) -> Parse<SpannedExpr> {
        let span_start = self.line_col(start);
        let end = self.string_end(start)?;
        self.pos = end + 1;

        Ok(Spanned::new(
            Expr::String(self.source[start + 1..end].to_string()),
            Span::new(start, self.pos, span_start.0, span_start.1),
        ))
    }

    /// `input_reference = "#" ~ (ASCII_ALPHA | "_")+ ~ identifier_rest*`
    fn input_reference(&mut self, start: usize) -> Parse<SpannedExpr> {
        if !is_word_start(self.at(start + 1)) {
            return Err(Stop::NoMatch);
        }

        let end = self.word_end(start + 1);
        let field = self.source[start + 1..end].to_string();

        Ok(self.literal(Expr::InputReference(field), start, end))
    }

    /// `number = binary_number | hex_number | decimal_number`, without a leading minus, which
    /// `prefix_usage` always takes first.
    fn number(&mut self, start: usize) -> Parse<SpannedExpr> {
        let digits = if self.bytes[start] == b'+' {
            start + 1
        } else {
            start
        };

        let (value, end) = match (self.at(digits), self.at(digits + 1)) {
            (b'0', b'b') => self.radix_number(digits + 2, 2, 62)?,
            (b'0', b'x') => self.radix_number(digits + 2, 16, 15)?,
            _ => self.decimal_number(start, digits)?,
        };

        // A number runs straight into a word only in input the grammar rejects, or in
        // literals it reads in surprising ways, such as `1_` or `0b2`.
        if is_word(self.at(end)) {
            return Err(Stop::Defer);
        }

        Ok(self.literal(Expr::Number(value), start, end))
    }

    /// The digits of `binary_number` or `hex_number`, from just past the `0b` or `0x`: groups
    /// of digits separated by underscores. At most `max_digits` are read here, few enough for
    /// the value to be exact in the integer type `pairs_to_expr` computes it in.
    fn radix_number(&self, start: usize, radix: u32, max_digits: usize) -> Parse<(f64, usize)> {
        let digit = |index: usize| char::from(self.at(index)).to_digit(radix);
        let mut value: u64 = 0;
        let mut count = 0;
        let mut index = start;

        loop {
            while let Some(digit) = digit(index) {
                if count == max_digits {
                    return Err(Stop::Defer);
                }

                value = value * u64::from(radix) + u64::from(digit);
                count += 1;
                index += 1;
            }

            let mut next = index;
            while self.at(next) == b'_' {
                next += 1;
            }

            if next == index || count == 0 || digit(next).is_none() {
                break;
            }

            index = next;
        }

        if count == 0 {
            return Err(Stop::Defer);
        }

        Ok((value as f64, index))
    }

    /// `decimal_number = (integer ~ ("_"+ ~ integer)* ~ ("." ~ ASCII_DIGIT+)? |
    /// !integer ~ "." ~ ASCII_DIGIT+) ~ (^"e" ~ integer)?`, where `start` may be a plus sign
    /// and `digits` is just past it.
    fn decimal_number(&self, start: usize, digits: usize) -> Parse<(f64, usize)> {
        let mut index = self.digits_end(digits);
        let mut underscores = false;

        // Each group after an underscore may carry its own sign, which makes a literal no
        // number parser accepts; the grammar turns that into an error of its own.
        while self.at(index) == b'_' {
            let mut next = index;
            while self.at(next) == b'_' {
                next += 1;
            }

            if !is_digit(self.at(next)) {
                return Err(Stop::Defer);
            }

            underscores = true;
            index = self.digits_end(next);
        }

        let integer_end = index;
        if self.at(index) == b'.' && is_digit(self.at(index + 1)) {
            index = self.digits_end(index + 1);
        }

        if self.at(index) | 0x20 == b'e' {
            let sign = matches!(self.at(index + 1), b'+' | b'-');
            let exponent = index + 1 + usize::from(sign);
            if is_digit(self.at(exponent)) {
                index = self.digits_end(exponent);
            }
        }

        // Integers of up to fifteen digits are exact as doubles, so summing the digits gives
        // the very value the standard parser would.
        let text = &self.source[start..index];
        let value = if index == integer_end && !underscores && start == digits && text.len() <= 15 {
            let integer = text
                .bytes()
                .fold(0u64, |value, digit| value * 10 + u64::from(digit - b'0'));

            integer as f64
        } else if underscores {
            text.replace('_', "").parse().map_err(|_| Stop::Defer)?
        } else {
            text.parse().map_err(|_| Stop::Defer)?
        };

        Ok((value, index))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expressions::pairs_to_expr;
    use crate::parser::{Rule, get_pairs};

    /// What pest and `pairs_to_expr` make of `source`, statement by statement.
    fn reference(source: &str) -> Option<Vec<Statement>> {
        let mut statements = Vec::new();

        for pair in get_pairs(source).ok()? {
            if pair.as_rule() == Rule::EOI {
                continue;
            }

            let inner = pair.into_inner().next().unwrap();
            statements.push(match inner.as_rule() {
                Rule::expression => Statement::Expression(pairs_to_expr(inner.into_inner()).ok()?),
                Rule::output_declaration => {
                    let first = inner.clone().into_inner().next().unwrap();
                    let (name, assigns) = match first.as_rule() {
                        Rule::identifier => (first.as_str(), false),
                        Rule::assignment => (first.into_inner().next().unwrap().as_str(), true),
                        rule => unreachable!("unexpected rule: {:?}", rule),
                    };

                    Statement::Output {
                        name: Symbol::intern(name),
                        expr: pairs_to_expr(inner.into_inner()).ok()?,
                        assigns,
                    }
                }
                Rule::comment => Statement::Comment,
                rule => unreachable!("unexpected rule: {:?}", rule),
            });
        }

        Some(statements)
    }

    /// Check that this parser either leaves `source` to pest or reads exactly what pest reads,
    /// spans included (which `PartialEq` on the AST ignores). Returns whether it read it.
    fn check(source: &str) -> bool {
        let Some(statements) = parse_program(source) else {
            return false;
        };

        let expected = reference(source).map(|statements| format!("{:#?}", statements));
        assert_eq!(
            expected.as_deref(),
            Some(format!("{:#?}", statements).as_str()),
            "the parsers disagree on {:?}",
            source
        );

        true
    }

    const READ: &[&str] = &[
        "",
        "\n",
        "  \n\n  ",
        "x = 1",
        "x = 1\n",
        "x = 1\r\ny = 2\r\n",
        "// only a comment",
        "x = 1 // trailing\n// own line\ny = 2",
        "1 + 2 * 3 ^ 2 ^ 2 - 4 / 2 % 3",
        "a - b - c",
        "a ^ b ^ c",
        "-2 ^ 2",
        "2 ^ -1",
        "--x",
        "!!x",
        "not x",
        "not not x",
        "-x!",
        "5!",
        "5!!",
        "-(3!)",
        "a == b",
        "a != b",
        "a<b",
        "a <= b && c >= d || e > f",
        "a .== b .!= c .< d .<= e .> f .>= g",
        "a and b or c",
        "a ?? b ?? c",
        "xs via f",
        "xs where f into g",
        "xs via x => x + 1 where y => y > 1",
        "1 +\n2",
        "1\n+ 2",
        "1 // one\n+ 2",
        "a\n  and b",
        "f(x)",
        "f()",
        "f(a, b)(c)[0].d!",
        "f(\n  a,\n  b,\n)",
        "f(...xs, 1)",
        "a[0]",
        "a[i + 1][j]",
        "a.b.c",
        "a.b(1).c[2]",
        "[1, 2, 3]",
        "[]",
        "[ ]",
        "[1, 2, 3,]",
        "[\n  1, // one\n  2,\n]",
        "[...xs, ...ys]",
        "{}",
        "{a: 1, b: 2}",
        "{a: 1, b,}",
        "{\"k\": 1, 'j': 2, [key]: 3, ...rest, short}",
        "{\n  a: 1, // first\n  b:\n    2,\n}",
        "x => x",
        "x => x + 1",
        "x? => x",
        "...rest => rest",
        "() => 1",
        "(a) => a",
        "(a, b) => a + b",
        "(a, b?, ...c) => a",
        "(\n  a,\n  b,\n) => a",
        "f = (a, b) => a * b - if a > b then a else b",
        "a => b => c => a + b + c",
        "x => y = x",
        "(x)",
        "( x )",
        "(\n x \n)",
        "(a + b) * c",
        "((a))",
        "if a then b else c",
        "if a then b\nelse c",
        "if a\nthen b\nelse if c then d else e",
        "do { return 1 }",
        "do {\n  x = 1\n  y = 2; z = 3\n  return x + y + z\n}",
        "do {\n  // note\n  x = 1// tail\n  return x\n}",
        "do\n{\n  return 1\n}",
        "f = n => do {\n  a = n + 1\n  return a * 2\n}",
        "true",
        "false",
        "null",
        "trusty",
        "iffy",
        "done",
        "notable",
        "android",
        "orbit",
        "viable",
        "#field",
        "#field_2 + 1",
        "\"double\"",
        "'single'",
        "\"it's\"",
        "'say \"hi\"'",
        "\"two\nlines\"",
        "\"// not a comment\"",
        "\"é\" + x",
        "x = \"é\"\ny = z + 1",
        "0",
        "42",
        "1.5",
        ".5",
        "+3",
        "1e3",
        "1E-3",
        "1.5e+3",
        "1_000_000",
        "1_000.5",
        "0x1F",
        "0b1010",
        "0b1010_1010",
        "123456789012345",
        "1234567890123456789",
        "1e400",
        "sum(1, 2)",
        "map(xs, x => x * 2)",
        "output x",
        "output x = 1 + 2",
        "output   y  =  [1, 2]",
        "outputs = 1",
        "x = 1\n\n\ny = x // done\n",
        "where = 3",
        "x = y\nwhere = 3",
        "a\n\n\n+ b",
    ];

    const LEFT_TO_PEST: &[&str] = &[
        "x =",
        "1 +",
        "(1",
        "[1, 2",
        "{a: 1",
        "f(a,)",
        "if a then b",
        "do { x = 1 }",
        "\"unterminated",
        "x = 1 y = 2",
        "1 2",
        "a[ 0 ]",
        "truex",
        "nullable",
        "falsey",
        "+.5",
        "do {\n  x = 1 // tail\n  return x\n}",
        "1_",
        "0b2",
        "0x",
        "1__2e",
        "output 1",
        "@",
        "x = 1 /* nope */",
        "then",
        "(,) => 1",
        "[,]",
        "{,}",
    ];

    #[test]
    fn reads_what_pest_reads() {
        let rejected: Vec<&&str> = READ
            .iter()
            .filter(|source| reference(source).is_none())
            .collect();
        assert!(rejected.is_empty(), "pest rejects: {:?}", rejected);

        let left: Vec<&&str> = READ.iter().filter(|source| !check(source)).collect();
        assert!(left.is_empty(), "left to pest: {:?}", left);
    }

    #[test]
    fn leaves_the_rest_to_pest() {
        for source in LEFT_TO_PEST {
            assert!(!check(source), "read {:?} without pest", source);
        }
    }

    #[test]
    fn never_reads_what_pest_rejects() {
        for source in LEFT_TO_PEST {
            if reference(source).is_none() {
                assert_eq!(parse_program(source), None, "accepted {:?}", source);
            }
        }
    }

    #[test]
    fn nesting_beyond_the_limit_is_left_to_pest() {
        let shallow = format!("{}1{}", "(".repeat(40), ")".repeat(40));
        assert!(check(&shallow));

        let deep = format!(
            "{}1{}",
            "(".repeat(MAX_DEPTH + 1),
            ")".repeat(MAX_DEPTH + 1)
        );
        assert_eq!(parse_program(&deep), None);

        let negated = format!("{}1", "-".repeat(MAX_DEPTH + 1));
        assert_eq!(parse_program(&negated), None);
    }

    #[test]
    fn lines_and_columns_count_characters() {
        let source = "s = \"é\"\n  u = \"üü\" + s";
        let statements = parse_program(source).unwrap();
        let Statement::Expression(second) = &statements[1] else {
            panic!("expected an expression");
        };
        let Expr::Assignment { value, .. } = &second.node else {
            panic!("expected an assignment");
        };
        let Expr::BinaryOp { right, .. } = &value.node else {
            panic!("expected a binary operation");
        };

        assert_eq!((second.span.start_line, second.span.start_col), (2, 3));
        assert_eq!((value.span.start_line, value.span.start_col), (2, 12));
        assert_eq!((right.span.start_line, right.span.start_col), (2, 14));
        assert!(check(source));
    }

    /// A small deterministic generator, so the test needs no dependency and no seed.
    struct Random(u64);

    impl Random {
        fn next(&mut self) -> u64 {
            self.0 ^= self.0 << 13;
            self.0 ^= self.0 >> 7;
            self.0 ^= self.0 << 17;

            self.0
        }

        fn below(&mut self, bound: usize) -> usize {
            (self.next() % bound as u64) as usize
        }

        fn pick<'a>(&mut self, items: &[&'a str]) -> &'a str {
            items[self.below(items.len())]
        }
    }

    const FRAGMENTS: &[&str] = &[
        " ",
        " ",
        "\n",
        "\t",
        "\r\n",
        "//c",
        "// c\n",
        ",",
        ",\n",
        ";",
        "(",
        ")",
        "[",
        "]",
        "{",
        "}",
        "=>",
        "=",
        "==",
        "!=",
        "!",
        "-",
        "+",
        "*",
        "/",
        "%",
        "^",
        "??",
        "&&",
        "||",
        ".",
        "...",
        ":",
        "?",
        "#",
        "_",
        "<",
        "<=",
        ">",
        ">=",
        ".==",
        ".<",
        " and ",
        " or ",
        "not ",
        " via ",
        " into ",
        " where ",
        "if ",
        " then ",
        " else ",
        "do {",
        "return ",
        "output ",
        "true",
        "false",
        "null",
        "x",
        "f",
        "xs",
        "sum",
        "truex",
        "iffy",
        "1",
        "2.5",
        ".5",
        "1e3",
        "0x1F",
        "0b101",
        "1_000",
        "\"s\"",
        "'t'",
        "\"",
        "'",
        "é",
        "f(x)",
        "a[0]",
        "a.b",
        "x => x",
        "[1, 2]",
        "{a: 1}",
        "do { return 1 }",
        "if a then b else c",
    ];

    /// Programs one edit away from valid ones are where the two parsers could part ways:
    /// whatever this parser still reads, pest has to read the same way.
    #[test]
    fn agrees_with_pest_on_mutated_programs() {
        let mut random = Random(0x9E37_79B9_7F4A_7C15);
        let mut read = 0;

        for round in 0..6000 {
            let mut bytes = READ[round % READ.len()].as_bytes().to_vec();

            for _ in 0..1 + random.below(3) {
                let at = random.below(bytes.len() + 1);
                let fragment = random.pick(FRAGMENTS).as_bytes();

                match random.below(3) {
                    0 if at < bytes.len() => {
                        bytes.remove(at);
                    }
                    1 if at < bytes.len() => {
                        bytes.splice(at..at + 1, fragment.iter().copied());
                    }
                    _ => {
                        bytes.splice(at..at, fragment.iter().copied());
                    }
                }
            }

            if check(&String::from_utf8_lossy(&bytes)) {
                read += 1;
            }
        }

        assert!(read > 500, "only {} mutated programs were read", read);
    }
}
