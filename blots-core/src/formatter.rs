use crate::ast::{BinaryOp, DoStatement, Expr, RecordEntry, RecordKey, SpannedExpr};
use crate::ast_to_source::{expr_to_source};
use crate::values::LambdaArg;

const DEFAULT_MAX_COLUMNS: usize = 80;
const INDENT_SIZE: usize = 2;

/// Format a Blots expression with intelligent line breaking
pub fn format_expr(expr: &SpannedExpr, max_columns: Option<usize>) -> String {
    let max_cols = max_columns.unwrap_or(DEFAULT_MAX_COLUMNS);
    format_expr_impl(expr, max_cols, 0)
}

/// Internal formatting implementation with indentation tracking
fn format_expr_impl(expr: &SpannedExpr, max_cols: usize, indent: usize) -> String {
    // Special handling for lambdas to ensure correct argument formatting
    if let Expr::Lambda { args, body } = &expr.node {
        return format_lambda(args, body, max_cols, indent);
    }

    // First, try single-line formatting using our custom formatter
    let single_line = format_single_line(expr);
    let current_line_length = indent + single_line.len();

    // If it fits on one line, use it
    if current_line_length <= max_cols {
        return single_line;
    }

    // Otherwise, apply smart multi-line formatting based on expression type
    format_multiline(expr, max_cols, indent)
}

/// Format an expression on a single line (respecting our formatting rules)
fn format_single_line(expr: &SpannedExpr) -> String {
    match &expr.node {
        Expr::Lambda { args, body } => {
            let args_str: Vec<String> = args.iter().map(lambda_arg_to_str).collect();
            let args_part = if args.len() == 1 && matches!(args[0], LambdaArg::Required(_)) {
                args_str[0].clone()
            } else {
                format!("({})", args_str.join(", "))
            };
            format!("{} => {}", args_part, format_single_line(body))
        }
        Expr::Call { func, args } => {
            let func_str = match &func.node {
                Expr::Lambda { .. } => format!("({})", format_single_line(func)),
                _ => format_single_line(func),
            };
            let args_str: Vec<String> = args.iter().map(format_single_line).collect();
            format!("{}({})", func_str, args_str.join(", "))
        }
        Expr::List(items) => {
            let items_str: Vec<String> = items.iter().map(format_single_line).collect();
            format!("[{}]", items_str.join(", "))
        }
        Expr::Record(entries) => {
            let entries_str: Vec<String> = entries.iter().map(|e| format_record_entry_single_line(e)).collect();
            format!("{{{}}}", entries_str.join(", "))
        }
        // For everything else, use the existing expr_to_source
        _ => expr_to_source(expr),
    }
}

/// Format a record entry on a single line
fn format_record_entry_single_line(entry: &RecordEntry) -> String {
    match &entry.key {
        RecordKey::Static(key) => format!("{}: {}", key, format_single_line(&entry.value)),
        RecordKey::Dynamic(key_expr) => {
            format!("[{}]: {}", format_single_line(key_expr), format_single_line(&entry.value))
        }
        RecordKey::Shorthand(name) => name.clone(),
        RecordKey::Spread(expr) => format_single_line(expr),
    }
}

/// Format an expression across multiple lines
fn format_multiline(expr: &SpannedExpr, max_cols: usize, indent: usize) -> String {
    match &expr.node {
        Expr::List(items) => format_list_multiline(items, max_cols, indent),
        Expr::Record(entries) => format_record_multiline(entries, max_cols, indent),
        Expr::Conditional { condition, then_expr, else_expr } => {
            format_conditional_multiline(condition, then_expr, else_expr, max_cols, indent)
        }
        Expr::Call { func, args } => format_call_multiline(func, args, max_cols, indent),
        Expr::BinaryOp { op, left, right } => {
            format_binary_op_multiline(op, left, right, max_cols, indent)
        }
        Expr::DoBlock { statements, return_expr } => {
            format_do_block_multiline(statements, return_expr, indent)
        }
        // For other expression types, fall back to single-line
        _ => expr_to_source(expr),
    }
}

/// Format a list with line breaks
fn format_list_multiline(items: &[SpannedExpr], max_cols: usize, indent: usize) -> String {
    if items.is_empty() {
        return "[]".to_string();
    }

    let inner_indent = indent + INDENT_SIZE;
    let indent_str = make_indent(inner_indent);

    let mut result = "[".to_string();

    for item in items.iter() {
        result.push_str("\n");
        result.push_str(&indent_str);
        result.push_str(&format_expr_impl(item, max_cols, inner_indent));

        // Add comma after each item (including last for multi-line)
        result.push(',');
    }

    result.push_str("\n");
    result.push_str(&make_indent(indent));
    result.push(']');

    result
}

/// Format a record with line breaks
fn format_record_multiline(entries: &[RecordEntry], max_cols: usize, indent: usize) -> String {
    if entries.is_empty() {
        return "{}".to_string();
    }

    let inner_indent = indent + INDENT_SIZE;
    let indent_str = make_indent(inner_indent);

    let mut result = "{".to_string();

    for entry in entries {
        result.push_str("\n");
        result.push_str(&indent_str);
        result.push_str(&format_record_entry(entry, max_cols, inner_indent));

        // Add comma after each entry (including last for multi-line)
        result.push(',');
    }

    result.push_str("\n");
    result.push_str(&make_indent(indent));
    result.push('}');

    result
}

/// Format a single record entry
fn format_record_entry(entry: &RecordEntry, max_cols: usize, indent: usize) -> String {
    match &entry.key {
        RecordKey::Static(key) => {
            format!("{}: {}", key, format_expr_impl(&entry.value, max_cols, indent))
        }
        RecordKey::Dynamic(key_expr) => {
            format!(
                "[{}]: {}",
                format_expr_impl(key_expr, max_cols, indent),
                format_expr_impl(&entry.value, max_cols, indent)
            )
        }
        RecordKey::Shorthand(name) => name.clone(),
        RecordKey::Spread(expr) => format_expr_impl(expr, max_cols, indent),
    }
}

/// Format a lambda (handles both single-line and multi-line)
fn format_lambda(args: &[LambdaArg], body: &SpannedExpr, max_cols: usize, indent: usize) -> String {
    let args_str: Vec<String> = args.iter().map(lambda_arg_to_str).collect();

    // For single required arguments, omit parentheses
    let args_part = if args.len() == 1 && matches!(args[0], LambdaArg::Required(_)) {
        format!("{} =>", args_str[0])
    } else {
        format!("({}) =>", args_str.join(", "))
    };

    // Try single-line first
    let single_line_body = format_expr_impl(body, max_cols, indent);
    let single_line = format!("{} {}", args_part, single_line_body);

    if indent + single_line.len() <= max_cols {
        return single_line;
    }

    // Otherwise, put body on next line with increased indentation
    let body_indent = indent + INDENT_SIZE;
    format!(
        "{}\n{}{}",
        args_part,
        make_indent(body_indent),
        format_expr_impl(body, max_cols, body_indent)
    )
}

/// Format a conditional with line breaks
fn format_conditional_multiline(
    condition: &SpannedExpr,
    then_expr: &SpannedExpr,
    else_expr: &SpannedExpr,
    max_cols: usize,
    indent: usize,
) -> String {
    let cond_str = format_expr_impl(condition, max_cols, indent);

    // Try to fit "if <condition> then" on one line
    let if_then_prefix = format!("if {} then", cond_str);

    if indent + if_then_prefix.len() <= max_cols {
        // Put then/else clauses on new lines
        let inner_indent = indent + INDENT_SIZE;
        format!(
            "{}\n{}{}\nelse\n{}{}",
            if_then_prefix,
            make_indent(inner_indent),
            format_expr_impl(then_expr, max_cols, inner_indent),
            make_indent(inner_indent),
            format_expr_impl(else_expr, max_cols, inner_indent)
        )
    } else {
        // Everything on separate lines
        let inner_indent = indent + INDENT_SIZE;
        format!(
            "if\n{}{}\nthen\n{}{}\nelse\n{}{}",
            make_indent(inner_indent),
            format_expr_impl(condition, max_cols, inner_indent),
            make_indent(inner_indent),
            format_expr_impl(then_expr, max_cols, inner_indent),
            make_indent(inner_indent),
            format_expr_impl(else_expr, max_cols, inner_indent)
        )
    }
}

/// Format a function call with line breaks
fn format_call_multiline(func: &SpannedExpr, args: &[SpannedExpr], max_cols: usize, indent: usize) -> String {
    let func_str = match &func.node {
        Expr::Lambda { .. } => format!("({})", format_expr_impl(func, max_cols, indent)),
        _ => format_expr_impl(func, max_cols, indent),
    };

    if args.is_empty() {
        return format!("{}()", func_str);
    }

    // Try formatting args on separate lines
    let inner_indent = indent + INDENT_SIZE;
    let indent_str = make_indent(inner_indent);

    let mut result = format!("{}(", func_str);

    for (i, arg) in args.iter().enumerate() {
        result.push_str("\n");
        result.push_str(&indent_str);
        result.push_str(&format_expr_impl(arg, max_cols, inner_indent));

        if i < args.len() - 1 {
            result.push(',');
        } else {
            // Trailing comma on last arg for multi-line
            result.push(',');
        }
    }

    result.push_str("\n");
    result.push_str(&make_indent(indent));
    result.push(')');

    result
}

/// Format a binary operation with line breaks
fn format_binary_op_multiline(
    op: &BinaryOp,
    left: &SpannedExpr,
    right: &SpannedExpr,
    max_cols: usize,
    indent: usize,
) -> String {
    let op_str = binary_op_str(op);
    let left_str = format_expr_impl(left, max_cols, indent);

    // Break before the operator
    let right_indent = indent + INDENT_SIZE;
    format!(
        "{}\n{}{} {}",
        left_str,
        make_indent(right_indent),
        op_str,
        format_expr_impl(right, max_cols, right_indent)
    )
}

/// Format a do block (always multi-line)
fn format_do_block_multiline(statements: &[DoStatement], return_expr: &SpannedExpr, indent: usize) -> String {
    let inner_indent = indent + INDENT_SIZE;
    let indent_str = make_indent(inner_indent);

    let mut result = "do {".to_string();

    for stmt in statements {
        match stmt {
            DoStatement::Expression(e) => {
                result.push_str("\n");
                result.push_str(&indent_str);
                // Do blocks use unlimited line length for now
                result.push_str(&format_expr_impl(e, usize::MAX, inner_indent));
            }
            DoStatement::Comment(c) => {
                result.push_str("\n");
                result.push_str(&indent_str);
                result.push_str(c);
            }
        }
    }

    result.push_str("\n");
    result.push_str(&indent_str);
    result.push_str("return ");
    result.push_str(&format_expr_impl(return_expr, usize::MAX, inner_indent));
    result.push_str("\n");
    result.push_str(&make_indent(indent));
    result.push('}');

    result
}

/// Convert lambda argument to string
fn lambda_arg_to_str(arg: &LambdaArg) -> String {
    match arg {
        LambdaArg::Required(name) => name.clone(),
        LambdaArg::Optional(name) => format!("{}?", name),
        LambdaArg::Rest(name) => format!("...{}", name),
    }
}

/// Convert binary operator to string
fn binary_op_str(op: &BinaryOp) -> &'static str {
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

/// Generate indentation string
fn make_indent(indent: usize) -> String {
    " ".repeat(indent)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::get_pairs;
    use crate::expressions::pairs_to_expr;

    fn parse_test_expr(source: &str) -> SpannedExpr {
        use crate::parser::Rule;

        let pairs = get_pairs(source).unwrap();

        // Extract the actual expression from the statement wrapper
        for pair in pairs {
            if pair.as_rule() == Rule::statement {
                if let Some(inner_pair) = pair.into_inner().next() {
                    return pairs_to_expr(inner_pair.into_inner()).unwrap();
                }
            }
        }

        panic!("No statement found in parsed input");
    }

    #[test]
    fn test_format_short_list() {
        let expr = parse_test_expr("[1, 2, 3]");
        let formatted = format_expr(&expr, Some(80));
        assert_eq!(formatted, "[1, 2, 3]");
    }

    #[test]
    fn test_format_long_list() {
        let expr = parse_test_expr("[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]");
        let formatted = format_expr(&expr, Some(40));
        assert!(formatted.contains("\n"));
        assert!(formatted.contains("[\n"));
    }

    #[test]
    fn test_format_short_record() {
        let expr = parse_test_expr("{x: 1, y: 2}");
        let formatted = format_expr(&expr, Some(80));
        assert_eq!(formatted, "{x: 1, y: 2}");
    }

    #[test]
    fn test_format_long_record() {
        let expr = parse_test_expr("{name: \"Alice\", age: 30, email: \"alice@example.com\", address: \"123 Main St\"}");
        let formatted = format_expr(&expr, Some(40));
        assert!(formatted.contains("\n"));
        assert!(formatted.contains("{\n"));
    }

    #[test]
    fn test_format_conditional() {
        let expr = parse_test_expr("if very_long_condition_variable > 100 then \"yes\" else \"no\"");
        let formatted = format_expr(&expr, Some(30));
        assert!(formatted.contains("\n"));
    }

    #[test]
    fn test_format_binary_op() {
        let expr = parse_test_expr("very_long_variable_name + another_very_long_variable_name");
        let formatted = format_expr(&expr, Some(30));
        assert!(formatted.contains("\n"));
    }

    #[test]
    fn test_format_lambda() {
        let expr = parse_test_expr("(x, y) => x + y");
        let formatted = format_expr(&expr, Some(80));
        assert_eq!(formatted, "(x, y) => x + y");
    }

    #[test]
    fn test_format_nested_list() {
        let expr = parse_test_expr("[[1, 2, 3], [4, 5, 6], [7, 8, 9]]");
        let formatted = format_expr(&expr, Some(20));
        assert!(formatted.contains("\n"));
    }

    #[test]
    fn test_format_function_call() {
        let expr = parse_test_expr("map([1, 2, 3], x => x * 2)");
        let formatted = format_expr(&expr, Some(80));
        assert_eq!(formatted, "map([1, 2, 3], x => x * 2)");
    }

    #[test]
    fn test_format_do_block() {
        let expr = parse_test_expr("do { x = 1\n  return x }");
        let formatted = format_expr(&expr, Some(80));
        assert!(formatted.contains("do {"));
        assert!(formatted.contains("return"));
    }
}
