use crate::functions::BuiltInFunction;
use crate::values::LambdaArg;
use serde::{Deserialize, Serialize};

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
    List(Vec<Expr>),
    Record(Vec<RecordEntry>),

    // Lambda
    Lambda {
        args: Vec<LambdaArg>,
        body: Box<Expr>,
    },

    // Control flow
    Conditional {
        condition: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },

    DoBlock {
        statements: Vec<DoStatement>,
        return_expr: Box<Expr>,
    },

    // Operations
    Assignment {
        ident: String,
        value: Box<Expr>,
    },

    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },

    Access {
        expr: Box<Expr>,
        index: Box<Expr>,
    },

    DotAccess {
        expr: Box<Expr>,
        field: String,
    },

    BinaryOp {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },

    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr>,
    },

    PostfixOp {
        op: PostfixOp,
        expr: Box<Expr>,
    },

    // Special
    Spread(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RecordEntry {
    pub key: RecordKey,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum RecordKey {
    Static(String),
    Dynamic(Box<Expr>),
    Shorthand(String),
    Spread(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DoStatement {
    Expression(Expr),
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
