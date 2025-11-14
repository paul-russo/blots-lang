use crate::functions::BuiltInFunction;
use crate::values::LambdaArg;
use serde::{Deserialize, Serialize};

/// Represents a source code location for error reporting
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Span {
    pub start_byte: usize,
    pub end_byte: usize,
    pub start_line: usize,
    pub start_col: usize,
}

impl Span {
    pub fn new(start_byte: usize, end_byte: usize, start_line: usize, start_col: usize) -> Self {
        Self {
            start_byte,
            end_byte,
            start_line,
            start_col,
        }
    }

    /// Create a dummy span for cases where location info is unavailable
    pub fn dummy() -> Self {
        Self {
            start_byte: 0,
            end_byte: 0,
            start_line: 1,
            start_col: 1,
        }
    }
}

/// Wrapper type that attaches source location information to any AST node
///
/// Note: PartialEq is manually implemented to compare only the node content,
/// ignoring spans. This ensures structural equality for AST nodes regardless
/// of their source location.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        // Only compare the node content, ignore the span
        self.node == other.node
    }
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }

    pub fn dummy(node: T) -> Self {
        Self {
            node,
            span: Span::dummy(),
        }
    }
}

/// Expression with source location information
pub type SpannedExpr = Spanned<Expr>;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expr {
    // Literals
    Number(f64),
    String(String),
    Bool(bool),
    Null,

    // Variables and functions
    Identifier(String),
    InputReference(String),   // Shorthand for inputs.field (e.g., #field)
    BuiltIn(BuiltInFunction), // Built-in function

    // Collections
    List(Vec<SpannedExpr>),
    Record(Vec<RecordEntry>),

    // Lambda
    Lambda {
        args: Vec<LambdaArg>,
        body: Box<SpannedExpr>,
    },

    // Control flow
    Conditional {
        condition: Box<SpannedExpr>,
        then_expr: Box<SpannedExpr>,
        else_expr: Box<SpannedExpr>,
    },

    DoBlock {
        statements: Vec<DoStatement>,
        return_expr: Box<SpannedExpr>,
    },

    // Operations
    Assignment {
        ident: String,
        value: Box<SpannedExpr>,
    },

    Call {
        func: Box<SpannedExpr>,
        args: Vec<SpannedExpr>,
    },

    Access {
        expr: Box<SpannedExpr>,
        index: Box<SpannedExpr>,
    },

    DotAccess {
        expr: Box<SpannedExpr>,
        field: String,
    },

    BinaryOp {
        op: BinaryOp,
        left: Box<SpannedExpr>,
        right: Box<SpannedExpr>,
    },

    UnaryOp {
        op: UnaryOp,
        expr: Box<SpannedExpr>,
    },

    PostfixOp {
        op: PostfixOp,
        expr: Box<SpannedExpr>,
    },

    // Special
    Spread(Box<SpannedExpr>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RecordEntry {
    pub key: RecordKey,
    pub value: SpannedExpr,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum RecordKey {
    Static(String),
    Dynamic(Box<SpannedExpr>),
    Shorthand(String),
    Spread(Box<SpannedExpr>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DoStatement {
    Expression(SpannedExpr),
    Comment(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Power,

    // Comparison (with broadcasting)
    Equal,
    NotEqual,
    Less,
    LessEq,
    Greater,
    GreaterEq,

    // Comparison (without broadcasting)
    DotEqual,
    DotNotEqual,
    DotLess,
    DotLessEq,
    DotGreater,
    DotGreaterEq,

    // Logical
    And,
    NaturalAnd,
    Or,
    NaturalOr,

    // Special
    Via,
    Into,
    Where,
    Coalesce,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnaryOp {
    Negate,
    Not,
    Invert,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PostfixOp {
    Factorial,
}
