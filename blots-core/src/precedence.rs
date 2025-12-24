use crate::ast::BinaryOp;
use crate::parser::Rule;
use pest::pratt_parser::{Op, PrattParser};
use std::sync::LazyLock;

// Re-export Assoc for convenience
pub use pest::pratt_parser::Assoc;

/// Macro to define operator precedence in a declarative way
/// This ensures BinaryOp variants and grammar Rules stay in sync
macro_rules! define_precedence {
    (
        $(
            precedence $prec:expr, $assoc:ident => {
                $( $binop:ident : $rule:ident ),* $(,)?
            }
        )*
    ) => {
        /// Operator precedence table - single source of truth
        /// Maps BinaryOp to (precedence, associativity, grammar rule)
        /// Lower precedence numbers = bind less tightly
        static PRECEDENCE_TABLE: &[(u8, Assoc, BinaryOp, Rule)] = &[
            $(
                $(
                    ($prec, Assoc::$assoc, BinaryOp::$binop, Rule::$rule),
                )*
            )*
        ];
    };
}

// Define all operator precedence here - single source of truth
define_precedence! {
    // Precedence 1 (lowest): via, into, where, and, or
    precedence 1, Left => {
        And: and,
        NaturalAnd: natural_and,
        Or: or,
        NaturalOr: natural_or,
        Via: via,
        Into: into,
        Where: where_,
    }

    // Precedence 2: comparisons
    precedence 2, Left => {
        Equal: equal,
        NotEqual: not_equal,
        Less: less,
        LessEq: less_eq,
        Greater: greater,
        GreaterEq: greater_eq,
        DotEqual: dot_equal,
        DotNotEqual: dot_not_equal,
        DotLess: dot_less,
        DotLessEq: dot_less_eq,
        DotGreater: dot_greater,
        DotGreaterEq: dot_greater_eq,
    }

    // Precedence 3: add, subtract
    precedence 3, Left => {
        Add: add,
        Subtract: subtract,
    }

    // Precedence 4: multiply, divide, modulo
    precedence 4, Left => {
        Multiply: multiply,
        Divide: divide,
        Modulo: modulo,
    }

    // Precedence 5 (highest)
    precedence 5, Right => {
        Power: power,
    }
    precedence 5, Left => {
        Coalesce: coalesce,
    }
}

/// Build a Pratt parser from the precedence table
pub fn build_pratt_parser() -> PrattParser<Rule> {
    let mut parser = PrattParser::new();

    // Group operators by (precedence, associativity)
    let mut precedence_groups: Vec<(u8, Assoc, Vec<Rule>)> = Vec::new();

    for &(prec, assoc, _binop, rule) in PRECEDENCE_TABLE {
        // Find or create the group for this precedence/associativity
        if let Some(group) = precedence_groups
            .iter_mut()
            .find(|(p, a, _)| *p == prec && *a == assoc)
        {
            group.2.push(rule);
        } else {
            precedence_groups.push((prec, assoc, vec![rule]));
        }
    }

    // Sort by precedence (higher precedence = added later in Pratt parser)
    precedence_groups.sort_by_key(|(prec, _, _)| *prec);

    // Build the Pratt parser
    for (_prec, assoc, rules) in precedence_groups {
        let mut op_chain = Op::infix(rules[0], assoc);
        for &rule in &rules[1..] {
            op_chain = op_chain | Op::infix(rule, assoc);
        }
        parser = parser.op(op_chain);
    }

    // Prefix operators
    parser = parser.op(Op::prefix(Rule::negation)
        | Op::prefix(Rule::spread_operator)
        | Op::prefix(Rule::invert)
        | Op::prefix(Rule::natural_not));

    // Postfix operators
    parser = parser.op(Op::postfix(Rule::factorial));
    parser = parser.op(Op::postfix(Rule::access)
        | Op::postfix(Rule::dot_access)
        | Op::postfix(Rule::call_list));

    parser
}

/// Shared Pratt parser instance
pub static PRATT: LazyLock<PrattParser<Rule>> = LazyLock::new(build_pratt_parser);

/// Get the precedence level and associativity of a binary operator
/// Looks up the operator in the precedence table
pub fn operator_info(op: &BinaryOp) -> (u8, Assoc) {
    PRECEDENCE_TABLE
        .iter()
        .find(|(_, _, binop, _)| binop == op)
        .map(|(prec, assoc, _, _)| (*prec, *assoc))
        .expect("All BinaryOp variants must be in PRECEDENCE_TABLE")
}
