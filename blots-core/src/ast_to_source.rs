use crate::ast::{BinaryOp, DoStatement, Expr, PostfixOp, RecordEntry, RecordKey, UnaryOp};
use crate::values::LambdaArg;

pub fn expr_to_source(expr: &Expr) -> String {
    match expr {
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
        Expr::Call { func, args } => {
            let args_str: Vec<String> = args.iter().map(expr_to_source).collect();
            format!("{}({})", expr_to_source(func), args_str.join(", "))
        }
        Expr::Access { expr, index } => {
            format!("{}[{}]", expr_to_source(expr), expr_to_source(index))
        }
        Expr::DotAccess { expr, field } => format!("{}.{}", expr_to_source(expr), field),
        Expr::BinaryOp { op, left, right } => {
            let op_str = binary_op_to_source(op);
            format!(
                "{} {} {}",
                expr_to_source(left),
                op_str,
                expr_to_source(right)
            )
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
        BinaryOp::And => "&&",
        BinaryOp::NaturalAnd => "and",
        BinaryOp::Or => "||",
        BinaryOp::NaturalOr => "or",
        BinaryOp::Pipe => "|>",
        BinaryOp::Coalesce => "??",
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
