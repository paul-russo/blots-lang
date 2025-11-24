use crate::ast::{BinaryOp, DoStatement, Expr, PostfixOp, RecordEntry, RecordKey, SpannedExpr, UnaryOp};
use crate::precedence::{operator_info, Assoc};
use crate::values::{LambdaArg, SerializableValue};
use indexmap::IndexMap;

pub fn expr_to_source(spanned_expr: &SpannedExpr) -> String {
    match &spanned_expr.node {
        Expr::Number(n) => {
            if n.fract() == 0.0 && n.abs() < 1e15 {
                format!("{:.0}", n)
            } else {
                n.to_string()
            }
        }
        Expr::String(s) => format!("\"{}\"", s.replace("\\", "\\\\").replace("\"", "\\\"")),
        Expr::Bool(b) => b.to_string(),
        Expr::Null => "null".to_string(),
        Expr::Identifier(name) => name.clone(),
        Expr::InputReference(field) => format!("#{}", field),
        Expr::BuiltIn(built_in) => built_in.name().to_string(),
        Expr::List(items) => {
            let items_str: Vec<String> = items.iter().map(expr_to_source).collect();
            format!("[{}]", items_str.join(", "))
        }
        Expr::Record(entries) => {
            let entries_str: Vec<String> = entries.iter().map(record_entry_to_source).collect();
            format!("{{{}}}", entries_str.join(", "))
        }
        Expr::Lambda { args, body } => {
            let args_str: Vec<String> = args.iter().map(lambda_arg_to_source).collect();
            format!("({}) => {}", args_str.join(", "), expr_to_source(body))
        }
        Expr::Conditional {
            condition,
            then_expr,
            else_expr,
        } => format!(
            "if {} then {} else {}",
            expr_to_source(condition),
            expr_to_source(then_expr),
            expr_to_source(else_expr)
        ),
        Expr::DoBlock {
            statements,
            return_expr,
        } => {
            let mut result = "do {".to_string();
            for stmt in statements {
                match stmt {
                    DoStatement::Expression(e) => {
                        result.push_str(&format!("\n  {}", expr_to_source(e)));
                    }
                    DoStatement::Comment(c) => {
                        result.push_str(&format!("\n  {}", c));
                    }
                }
            }
            result.push_str(&format!("\n  return {}\n}}", expr_to_source(return_expr)));
            result
        }
        Expr::Assignment { ident, value } => format!("{} = {}", ident, expr_to_source(value)),
        Expr::Output { expr } => format!("output {}", expr_to_source(expr)),
        Expr::Call { func, args } => {
            let args_str: Vec<String> = args.iter().map(expr_to_source).collect();
            let func_str = match &func.node {
                // Wrap lambdas in parentheses when used in call position
                Expr::Lambda { .. } => format!("({})", expr_to_source(func)),
                _ => expr_to_source(func),
            };
            format!("{}({})", func_str, args_str.join(", "))
        }
        Expr::Access { expr, index } => {
            format!("{}[{}]", expr_to_source(expr), expr_to_source(index))
        }
        Expr::DotAccess { expr, field } => format!("{}.{}", expr_to_source(expr), field),
        Expr::BinaryOp { op, left, right } => {
            let op_str = binary_op_to_source(op);
            let left_str = if needs_parens_in_binop(op, left, true) {
                format!("({})", expr_to_source(left))
            } else {
                expr_to_source(left)
            };
            let right_str = if needs_parens_in_binop(op, right, false) {
                format!("({})", expr_to_source(right))
            } else {
                expr_to_source(right)
            };
            format!("{} {} {}", left_str, op_str, right_str)
        }
        Expr::UnaryOp { op, expr } => {
            let op_str = unary_op_to_source(op);
            format!("{}{}", op_str, expr_to_source(expr))
        }
        Expr::PostfixOp { op, expr } => {
            let op_str = postfix_op_to_source(op);
            format!("{}{}", expr_to_source(expr), op_str)
        }
        Expr::Spread(expr) => format!("...{}", expr_to_source(expr)),
    }
}

fn lambda_arg_to_source(arg: &LambdaArg) -> String {
    match arg {
        LambdaArg::Required(name) => name.clone(),
        LambdaArg::Optional(name) => format!("{}?", name),
        LambdaArg::Rest(name) => format!("...{}", name),
    }
}

fn record_entry_to_source(entry: &RecordEntry) -> String {
    match &entry.key {
        RecordKey::Static(key) => format!("{}: {}", key, expr_to_source(&entry.value)),
        RecordKey::Dynamic(key_expr) => {
            format!(
                "[{}]: {}",
                expr_to_source(key_expr),
                expr_to_source(&entry.value)
            )
        }
        RecordKey::Shorthand(name) => name.clone(),
        RecordKey::Spread(expr) => expr_to_source(expr),
    }
}

fn binary_op_to_source(op: &BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::Subtract => "-",
        BinaryOp::Multiply => "*",
        BinaryOp::Divide => "/",
        BinaryOp::Modulo => "%",
        BinaryOp::Power => "^",
        BinaryOp::Equal => "==",
        BinaryOp::NotEqual => "!=",
        BinaryOp::Less => "<",
        BinaryOp::LessEq => "<=",
        BinaryOp::Greater => ">",
        BinaryOp::GreaterEq => ">=",
        BinaryOp::DotEqual => ".==",
        BinaryOp::DotNotEqual => ".!=",
        BinaryOp::DotLess => ".<",
        BinaryOp::DotLessEq => ".<=",
        BinaryOp::DotGreater => ".>",
        BinaryOp::DotGreaterEq => ".>=",
        BinaryOp::And => "&&",
        BinaryOp::NaturalAnd => "and",
        BinaryOp::Or => "||",
        BinaryOp::NaturalOr => "or",
        BinaryOp::Via => "via",
        BinaryOp::Into => "into",
        BinaryOp::Where => "where",
        BinaryOp::Coalesce => "??",
    }
}

/// Check if a child expression needs parentheses when used in a binary operation
fn needs_parens_in_binop(parent_op: &BinaryOp, child_expr: &SpannedExpr, is_left: bool) -> bool {
    match &child_expr.node {
        Expr::BinaryOp { op: child_op, .. } => {
            let (parent_prec, parent_assoc) = operator_info(parent_op);
            let (child_prec, _child_assoc) = operator_info(child_op);

            // Need parentheses if child has lower precedence
            if child_prec < parent_prec {
                return true;
            }

            // For same precedence, need parentheses on right side for:
            // - Right-associative operators (e.g., power)
            // - Non-associative operators (subtraction, division)
            if child_prec == parent_prec && !is_left {
                match parent_assoc {
                    Assoc::Right => return true,
                    Assoc::Left => {
                        // For left-associative operators, right side needs parens for non-associative ones
                        if matches!(parent_op, BinaryOp::Subtract | BinaryOp::Divide | BinaryOp::Modulo) {
                            return true;
                        }
                    }
                }
            }

            false
        }
        _ => false,
    }
}

fn unary_op_to_source(op: &UnaryOp) -> &'static str {
    match op {
        UnaryOp::Negate => "-",
        UnaryOp::Not => "!",
        UnaryOp::Invert => "~",
    }
}

fn postfix_op_to_source(op: &PostfixOp) -> &'static str {
    match op {
        PostfixOp::Factorial => "!",
    }
}

/// Convert an expression to source code with inlined scope values
pub fn expr_to_source_with_scope(
    spanned_expr: &SpannedExpr,
    scope: &IndexMap<String, SerializableValue>,
) -> String {
    match &spanned_expr.node {
        Expr::Identifier(name) => {
            // If the identifier is in the scope, inline its value
            if let Some(value) = scope.get(name) {
                serializable_value_to_source(value)
            } else {
                name.clone()
            }
        }
        Expr::InputReference(field) => format!("#{}", field),
        // For all other expression types, recursively process with scope
        Expr::Number(n) => {
            if n.fract() == 0.0 && n.abs() < 1e15 {
                format!("{:.0}", n)
            } else {
                n.to_string()
            }
        }
        Expr::String(s) => format!("\"{}\"", s.replace("\\", "\\\\").replace("\"", "\\\"")),
        Expr::Bool(b) => b.to_string(),
        Expr::Null => "null".to_string(),
        Expr::BuiltIn(built_in) => built_in.name().to_string(),
        Expr::List(items) => {
            let items_str: Vec<String> = items
                .iter()
                .map(|e| expr_to_source_with_scope(e, scope))
                .collect();
            format!("[{}]", items_str.join(", "))
        }
        Expr::Record(entries) => {
            let entries_str: Vec<String> = entries
                .iter()
                .map(|e| record_entry_to_source_with_scope(e, scope))
                .collect();
            format!("{{{}}}", entries_str.join(", "))
        }
        Expr::Lambda { args, body } => {
            let args_str: Vec<String> = args.iter().map(lambda_arg_to_source).collect();
            // Lambda bodies should not inline their own parameters
            // Filter out parameter names from the scope before processing the body
            let mut filtered_scope = scope.clone();
            for arg in args {
                filtered_scope.shift_remove(arg.get_name());
            }
            format!(
                "({}) => {}",
                args_str.join(", "),
                expr_to_source_with_scope(body, &filtered_scope)
            )
        }
        Expr::Conditional {
            condition,
            then_expr,
            else_expr,
        } => format!(
            "if {} then {} else {}",
            expr_to_source_with_scope(condition, scope),
            expr_to_source_with_scope(then_expr, scope),
            expr_to_source_with_scope(else_expr, scope)
        ),
        Expr::DoBlock {
            statements,
            return_expr,
        } => {
            let mut result = "do {".to_string();
            for stmt in statements {
                match stmt {
                    DoStatement::Expression(e) => {
                        result.push_str(&format!("\n  {}", expr_to_source_with_scope(e, scope)));
                    }
                    DoStatement::Comment(c) => {
                        result.push_str(&format!("\n  // {}", c));
                    }
                }
            }
            result.push_str(&format!(
                "\n  return {}",
                expr_to_source_with_scope(return_expr, scope)
            ));
            result.push_str("\n}");
            result
        }
        Expr::BinaryOp { op, left, right } => {
            let op_str = binary_op_to_source(op);
            let left_str = if needs_parens_in_binop(op, left, true) {
                format!("({})", expr_to_source_with_scope(left, scope))
            } else {
                expr_to_source_with_scope(left, scope)
            };
            let right_str = if needs_parens_in_binop(op, right, false) {
                format!("({})", expr_to_source_with_scope(right, scope))
            } else {
                expr_to_source_with_scope(right, scope)
            };
            format!("{} {} {}", left_str, op_str, right_str)
        }
        Expr::UnaryOp { op, expr } => {
            let op_str = match op {
                UnaryOp::Negate => "-",
                UnaryOp::Not => "!",
                UnaryOp::Invert => "~",
            };
            format!("{}{}", op_str, expr_to_source_with_scope(expr, scope))
        }
        Expr::PostfixOp { op, expr } => {
            let op_str = postfix_op_to_source(op);
            format!("{}{}", expr_to_source_with_scope(expr, scope), op_str)
        }
        Expr::Spread(expr) => format!("...{}", expr_to_source_with_scope(expr, scope)),
        Expr::Assignment { ident, value } => {
            format!("{} = {}", ident, expr_to_source_with_scope(value, scope))
        }
        Expr::Output { expr } => {
            format!("output {}", expr_to_source_with_scope(expr, scope))
        }
        Expr::Call { func, args } => {
            let args_str: Vec<String> = args
                .iter()
                .map(|e| expr_to_source_with_scope(e, scope))
                .collect();
            let func_str = match &func.node {
                // Wrap lambdas in parentheses when used in call position
                Expr::Lambda { .. } => {
                    format!("({})", expr_to_source_with_scope(func, scope))
                }
                _ => expr_to_source_with_scope(func, scope),
            };
            format!("{}({})", func_str, args_str.join(", "))
        }
        Expr::Access { expr, index } => {
            format!(
                "{}[{}]",
                expr_to_source_with_scope(expr, scope),
                expr_to_source_with_scope(index, scope)
            )
        }
        Expr::DotAccess { expr, field } => {
            format!("{}.{}", expr_to_source_with_scope(expr, scope), field)
        }
    }
}

fn record_entry_to_source_with_scope(
    entry: &RecordEntry,
    scope: &IndexMap<String, SerializableValue>,
) -> String {
    match &entry.key {
        RecordKey::Static(key) => {
            format!(
                "{}: {}",
                key,
                expr_to_source_with_scope(&entry.value, scope)
            )
        }
        RecordKey::Dynamic(key_expr) => {
            format!(
                "[{}]: {}",
                expr_to_source_with_scope(key_expr, scope),
                expr_to_source_with_scope(&entry.value, scope)
            )
        }
        RecordKey::Shorthand(name) => {
            // For shorthand, check if the value is in scope
            if let Some(value) = scope.get(name) {
                format!("{}: {}", name, serializable_value_to_source(value))
            } else {
                name.clone()
            }
        }
        RecordKey::Spread(expr) => expr_to_source_with_scope(expr, scope),
    }
}

/// Convert a SerializableValue to its source representation
fn serializable_value_to_source(value: &SerializableValue) -> String {
    match value {
        SerializableValue::Number(n) => {
            if n.fract() == 0.0 && n.abs() < 1e15 {
                format!("{:.0}", n)
            } else {
                n.to_string()
            }
        }
        SerializableValue::Bool(b) => b.to_string(),
        SerializableValue::Null => "null".to_string(),
        SerializableValue::String(s) => {
            format!("\"{}\"", s.replace("\\", "\\\\").replace("\"", "\\\""))
        }
        SerializableValue::List(items) => {
            let items_str: Vec<String> = items.iter().map(serializable_value_to_source).collect();
            format!("[{}]", items_str.join(", "))
        }
        SerializableValue::Record(fields) => {
            let entries_str: Vec<String> = fields
                .iter()
                .map(|(k, v)| format!("{}: {}", k, serializable_value_to_source(v)))
                .collect();
            format!("{{{}}}", entries_str.join(", "))
        }
        SerializableValue::Lambda(lambda_def) => {
            let args_str: Vec<String> = lambda_def.args.iter().map(lambda_arg_to_source).collect();
            // Wrap in parentheses so it can be used in call expressions
            format!("(({}) => {})", args_str.join(", "), lambda_def.body)
        }
        SerializableValue::BuiltIn(name) => name.clone(),
    }
}
