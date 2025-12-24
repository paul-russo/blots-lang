use crate::ast::{BinaryOp, DoStatement, Expr, RecordEntry, RecordKey, SpannedExpr};
use crate::ast_to_source::{expr_to_source, format_record_key, needs_parens_in_binop};
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

    // Special handling for do blocks - they're inherently multi-line
    // but we want to check if the opening fits on the current line
    if let Expr::DoBlock { .. } = &expr.node {
        return format_multiline(expr, max_cols, indent);
    }

    // First, try single-line formatting using our custom formatter
    let single_line = format_single_line(expr);

    // For expressions with do blocks inside, we can't use single_line.len()
    // because it already contains newlines. Instead, check the first line only.
    let first_line = single_line.lines().next().unwrap_or(&single_line);
    let current_line_length = indent + first_line.len();

    // If the first line fits and there are no newlines, use single-line format
    if !single_line.contains('\n') && current_line_length <= max_cols {
        return single_line;
    }

    // Otherwise, apply smart multi-line formatting based on expression type
    format_multiline(expr, max_cols, indent)
}

/// Format an expression on a single line (respecting our formatting rules)
fn format_single_line(expr: &SpannedExpr) -> String {
    match &expr.node {
        Expr::Output { expr: inner_expr } => {
            format!("output {}", format_single_line(inner_expr))
        }
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
            let entries_str: Vec<String> = entries
                .iter()
                .map(format_record_entry_single_line)
                .collect();
            format!("{{{}}}", entries_str.join(", "))
        }
        // For everything else, use the existing expr_to_source
        _ => expr_to_source(expr),
    }
}

/// Format a record entry on a single line
fn format_record_entry_single_line(entry: &RecordEntry) -> String {
    match &entry.key {
        RecordKey::Static(key) => format!(
            "{}: {}",
            format_record_key(key),
            format_single_line(&entry.value)
        ),
        RecordKey::Dynamic(key_expr) => {
            format!(
                "[{}]: {}",
                format_single_line(key_expr),
                format_single_line(&entry.value)
            )
        }
        RecordKey::Shorthand(name) => name.clone(),
        RecordKey::Spread(expr) => format_single_line(expr),
    }
}

/// Format an expression across multiple lines
fn format_multiline(expr: &SpannedExpr, max_cols: usize, indent: usize) -> String {
    match &expr.node {
        Expr::Output { expr: inner_expr } => {
            // Format as "output " + formatted inner expression
            let formatted_inner = format_expr_impl(inner_expr, max_cols, indent);
            format!("output {}", formatted_inner)
        }
        Expr::Assignment { ident, value } => {
            format_assignment_multiline(ident, value, max_cols, indent)
        }
        Expr::List(items) => format_list_multiline(items, max_cols, indent),
        Expr::Record(entries) => format_record_multiline(entries, max_cols, indent),
        Expr::Conditional {
            condition,
            then_expr,
            else_expr,
        } => format_conditional_multiline(condition, then_expr, else_expr, max_cols, indent),
        Expr::Call { func, args } => format_call_multiline(func, args, max_cols, indent),
        Expr::BinaryOp { op, left, right } => {
            format_binary_op_multiline(op, left, right, max_cols, indent)
        }
        Expr::DoBlock {
            statements,
            return_expr,
        } => format_do_block_multiline(statements, return_expr, max_cols, indent),
        // For other expression types, fall back to single-line
        _ => expr_to_source(expr),
    }
}

/// Format an assignment with line breaks
fn format_assignment_multiline(
    ident: &str,
    value: &SpannedExpr,
    max_cols: usize,
    indent: usize,
) -> String {
    // The assignment itself doesn't add indentation, but the value might need it
    // Format as: ident = <formatted_value>
    // The value should be formatted at the same indentation level, not pushed over

    let prefix = format!("{} = ", ident);

    // Format the value at the current indentation level
    // (not at prefix_len which would cause excessive indentation)
    let formatted_value = format_expr_impl(value, max_cols, indent);

    format!("{}{}", prefix, formatted_value)
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
        result.push('\n');
        result.push_str(&indent_str);
        result.push_str(&format_expr_impl(item, max_cols, inner_indent));

        // Add comma after each item (including last for multi-line)
        result.push(',');
    }

    result.push('\n');
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
        result.push('\n');
        result.push_str(&indent_str);
        result.push_str(&format_record_entry(entry, max_cols, inner_indent));

        // Add comma after each entry (including last for multi-line)
        result.push(',');
    }

    result.push('\n');
    result.push_str(&make_indent(indent));
    result.push('}');

    result
}

/// Format a single record entry
fn format_record_entry(entry: &RecordEntry, max_cols: usize, indent: usize) -> String {
    match &entry.key {
        RecordKey::Static(key) => {
            format!(
                "{}: {}",
                format_record_key(key),
                format_expr_impl(&entry.value, max_cols, indent)
            )
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

    // Special handling for do blocks - keep "=> do {" together
    if let Expr::DoBlock { .. } = &body.node {
        let body_formatted = format_expr_impl(body, max_cols, indent);
        return format!("{} {}", args_part, body_formatted);
    }

    // Try single-line first for other body types
    let single_line_body = format_expr_impl(body, max_cols, indent);
    let single_line = format!("{} {}", args_part, single_line_body);

    // Check only if it's actually single-line and fits
    if !single_line.contains('\n') && indent + single_line.len() <= max_cols {
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

    let inner_indent = indent + INDENT_SIZE;

    if indent + if_then_prefix.len() <= max_cols {
        // Put then/else clauses on new lines
        // Check if else_expr is another conditional (else-if chain)
        if let Expr::Conditional {
            condition: else_cond,
            then_expr: else_then,
            else_expr: else_else,
        } = &else_expr.node
        {
            // Format as "else if" at the same indentation level (not nested)
            let else_if_part =
                format_conditional_multiline(else_cond, else_then, else_else, max_cols, indent);
            format!(
                "{}\n{}{}\n{}else {}",
                if_then_prefix,
                make_indent(inner_indent),
                format_expr_impl(then_expr, max_cols, inner_indent),
                make_indent(indent),
                else_if_part
            )
        } else {
            format!(
                "{}\n{}{}\n{}else\n{}{}",
                if_then_prefix,
                make_indent(inner_indent),
                format_expr_impl(then_expr, max_cols, inner_indent),
                make_indent(indent),
                make_indent(inner_indent),
                format_expr_impl(else_expr, max_cols, inner_indent)
            )
        }
    } else {
        // Everything on separate lines
        // Check if else_expr is another conditional (else-if chain)
        if let Expr::Conditional {
            condition: else_cond,
            then_expr: else_then,
            else_expr: else_else,
        } = &else_expr.node
        {
            let else_if_part =
                format_conditional_multiline(else_cond, else_then, else_else, max_cols, indent);
            format!(
                "if\n{}{}\n{}then\n{}{}\n{}else {}",
                make_indent(inner_indent),
                format_expr_impl(condition, max_cols, inner_indent),
                make_indent(indent),
                make_indent(inner_indent),
                format_expr_impl(then_expr, max_cols, inner_indent),
                make_indent(indent),
                else_if_part
            )
        } else {
            format!(
                "if\n{}{}\n{}then\n{}{}\n{}else\n{}{}",
                make_indent(inner_indent),
                format_expr_impl(condition, max_cols, inner_indent),
                make_indent(indent),
                make_indent(inner_indent),
                format_expr_impl(then_expr, max_cols, inner_indent),
                make_indent(indent),
                make_indent(inner_indent),
                format_expr_impl(else_expr, max_cols, inner_indent)
            )
        }
    }
}

/// Format a function call with line breaks
fn format_call_multiline(
    func: &SpannedExpr,
    args: &[SpannedExpr],
    max_cols: usize,
    indent: usize,
) -> String {
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
        result.push('\n');
        result.push_str(&indent_str);
        result.push_str(&format_expr_impl(arg, max_cols, inner_indent));

        if i < args.len() - 1 {
            result.push(',');
        } else {
            // Trailing comma on last arg for multi-line
            result.push(',');
        }
    }

    result.push('\n');
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

    // Check if operands need parentheses based on precedence
    let left_needs_parens = needs_parens_in_binop(op, left, true);
    let right_needs_parens = needs_parens_in_binop(op, right, false);

    let left_str = format_expr_impl(left, max_cols, indent);
    let left_str = if left_needs_parens {
        format!("({})", left_str)
    } else {
        left_str
    };

    // Special handling for via/into/where with lambda on the right
    // Try to keep "via lambda" together on the same line
    if matches!(op, BinaryOp::Via | BinaryOp::Into | BinaryOp::Where)
        && let Expr::Lambda { .. } = &right.node
    {
        // Format the right side (lambda with possible do block)
        let right_str = format_expr_impl(right, max_cols, indent);
        let right_str = if right_needs_parens {
            format!("({})", right_str)
        } else {
            right_str
        };

        // Check if the first line of the whole expression fits
        // (for lambdas with do blocks, this would be "left via i => do {")
        let first_line_of_right = right_str.lines().next().unwrap_or(&right_str);
        let first_line_combined = format!("{} {} {}", left_str, op_str, first_line_of_right);

        if indent + first_line_combined.len() <= max_cols {
            // The opening line fits! Return the full formatted expression
            // If right_str is multi-line, this will preserve that structure
            if right_str.contains('\n') {
                // Multi-line lambda (like with do block)
                let remaining_lines = right_str.lines().skip(1).collect::<Vec<_>>().join("\n");
                return format!(
                    "{} {} {}\n{}",
                    left_str, op_str, first_line_of_right, remaining_lines
                );
            } else {
                // Single-line lambda
                return format!("{} {} {}", left_str, op_str, right_str);
            }
        }

        // If it doesn't fit, break before the operator (keep operator with right operand)
        let continued_indent = indent;
        let right_formatted = format_expr_impl(right, max_cols, continued_indent);
        let right_formatted = if right_needs_parens {
            format!("({})", right_formatted)
        } else {
            right_formatted
        };
        return format!(
            "{}\n{}{} {}",
            left_str,
            make_indent(continued_indent),
            op_str,
            right_formatted
        );
    }

    // Default: break before the operator with indentation
    let right_indent = indent + INDENT_SIZE;
    let right_str = format_expr_impl(right, max_cols, right_indent);
    let right_str = if right_needs_parens {
        format!("({})", right_str)
    } else {
        right_str
    };
    format!(
        "{}\n{}{} {}",
        left_str,
        make_indent(right_indent),
        op_str,
        right_str
    )
}

/// Format a do block (always multi-line)
fn format_do_block_multiline(
    statements: &[DoStatement],
    return_expr: &SpannedExpr,
    max_cols: usize,
    indent: usize,
) -> String {
    let inner_indent = indent + INDENT_SIZE;
    let indent_str = make_indent(inner_indent);

    let mut result = "do {".to_string();

    for stmt in statements {
        match stmt {
            DoStatement::Expression(e) => {
                result.push('\n');
                result.push_str(&indent_str);
                result.push_str(&format_expr_impl(e, max_cols, inner_indent));
            }
            DoStatement::Comment(c) => {
                result.push('\n');
                result.push_str(&indent_str);
                result.push_str(c);
            }
        }
    }

    result.push('\n');
    result.push_str(&indent_str);
    result.push_str("return ");
    result.push_str(&format_expr_impl(return_expr, max_cols, inner_indent));
    result.push('\n');
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

/// Join formatted statements with appropriate spacing based on their original positions
/// Preserves up to 2 empty lines between statements
pub fn join_statements_with_spacing(
    statements: &[(String, usize, usize)], // (formatted_statement, start_line, end_line)
) -> String {
    if statements.is_empty() {
        return String::new();
    }

    let mut result = String::new();

    for (i, (stmt, _start_line, end_line)) in statements.iter().enumerate() {
        result.push_str(stmt);

        // Add newlines between statements
        if i < statements.len() - 1 {
            let next_start_line = statements[i + 1].1;

            // Calculate how many lines apart they are
            // If they're on consecutive lines (end_line = 1, next_start = 2), gap = 0
            // If there's one empty line (end_line = 1, next_start = 3), gap = 1
            // If there's two empty lines (end_line = 1, next_start = 4), gap = 2
            let line_gap = next_start_line.saturating_sub(*end_line).saturating_sub(1);

            // Preserve up to 2 empty lines (which means up to 3 newlines total)
            // 0 empty lines = 1 newline
            // 1 empty line = 2 newlines
            // 2 empty lines = 3 newlines
            // 3+ empty lines = 3 newlines (capped at 2)
            let newlines = std::cmp::min(line_gap + 1, 3);

            for _ in 0..newlines {
                result.push('\n');
            }
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expressions::pairs_to_expr;
    use crate::parser::get_pairs;

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
        let expr = parse_test_expr(
            "{name: \"Alice\", age: 30, email: \"alice@example.com\", address: \"123 Main St\"}",
        );
        let formatted = format_expr(&expr, Some(40));
        assert!(formatted.contains("\n"));
        assert!(formatted.contains("{\n"));
    }

    #[test]
    fn test_format_conditional() {
        let expr =
            parse_test_expr("if very_long_condition_variable > 100 then \"yes\" else \"no\"");
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

    #[test]
    fn test_multiple_statements() {
        use crate::parser::Rule;

        let source = "x = [1, 2, 3, 4, 5]\ny = {name: \"Alice\", age: 30}\nz = x + y";
        let pairs = get_pairs(source).unwrap();

        let mut formatted_statements = Vec::new();

        for pair in pairs {
            if pair.as_rule() == Rule::statement {
                if let Some(inner_pair) = pair.into_inner().next() {
                    if let Ok(expr) = pairs_to_expr(inner_pair.into_inner()) {
                        let formatted = format_expr(&expr, Some(80));
                        formatted_statements.push(formatted);
                    }
                }
            }
        }

        // Should have formatted all 3 statements
        assert_eq!(formatted_statements.len(), 3);
        assert_eq!(formatted_statements[0], "x = [1, 2, 3, 4, 5]");
        assert_eq!(formatted_statements[1], "y = {name: \"Alice\", age: 30}");
        assert_eq!(formatted_statements[2], "z = x + y");
    }

    #[test]
    fn test_comments_are_preserved() {
        use crate::parser::Rule;

        let source = "// Comment 1\nx = [1, 2, 3]\n// Comment 2\ny = x + 1";
        let pairs = get_pairs(source).unwrap();

        let mut formatted_statements = Vec::new();

        for pair in pairs {
            if pair.as_rule() == Rule::statement {
                if let Some(inner_pair) = pair.into_inner().next() {
                    match inner_pair.as_rule() {
                        Rule::comment => {
                            // Preserve comment as-is
                            formatted_statements.push(inner_pair.as_str().to_string());
                        }
                        _ => {
                            // Format as expression
                            if let Ok(expr) = pairs_to_expr(inner_pair.into_inner()) {
                                let formatted = format_expr(&expr, Some(80));
                                formatted_statements.push(formatted);
                            }
                        }
                    }
                }
            }
        }

        // All 4 items should be preserved
        assert_eq!(formatted_statements.len(), 4);
        assert_eq!(formatted_statements[0], "// Comment 1");
        assert_eq!(formatted_statements[1], "x = [1, 2, 3]");
        assert_eq!(formatted_statements[2], "// Comment 2");
        assert_eq!(formatted_statements[3], "y = x + 1");
    }

    #[test]
    fn test_comments_with_formatted_code() {
        use crate::parser::Rule;

        let source = "// Configuration\nconfig = {name: \"test\", debug: true}\n// Process data\nresult = [1, 2, 3]";
        let pairs = get_pairs(source).unwrap();

        let mut formatted_statements = Vec::new();

        for pair in pairs {
            if pair.as_rule() == Rule::statement {
                if let Some(inner_pair) = pair.into_inner().next() {
                    match inner_pair.as_rule() {
                        Rule::comment => {
                            formatted_statements.push(inner_pair.as_str().to_string());
                        }
                        _ => {
                            if let Ok(expr) = pairs_to_expr(inner_pair.into_inner()) {
                                let formatted = format_expr(&expr, Some(80));
                                formatted_statements.push(formatted);
                            }
                        }
                    }
                }
            }
        }

        let result = formatted_statements.join("\n");

        // Should have 4 statements: 2 comments + 2 expressions
        assert_eq!(formatted_statements.len(), 4);
        assert_eq!(formatted_statements[0], "// Configuration");
        assert!(formatted_statements[1].starts_with("config = "));
        assert_eq!(formatted_statements[2], "// Process data");
        assert_eq!(formatted_statements[3], "result = [1, 2, 3]");

        // Verify the full result preserves comments
        assert!(result.contains("// Configuration"));
        assert!(result.contains("// Process data"));
    }

    #[test]
    fn test_output_declaration_with_assignment() {
        use crate::parser::Rule;

        let source = "output result = 42";
        let pairs = get_pairs(source).unwrap();

        let mut formatted_statements = Vec::new();

        for pair in pairs {
            if pair.as_rule() == Rule::statement {
                if let Some(inner_pair) = pair.into_inner().next() {
                    match inner_pair.as_rule() {
                        Rule::output_declaration => {
                            // Should preserve the output keyword
                            let mut inner = inner_pair.into_inner();
                            let assignment_or_ident = inner.next().unwrap();

                            let formatted = if assignment_or_ident.as_rule() == Rule::assignment {
                                // Extract identifier and value from assignment
                                let mut assignment_inner = assignment_or_ident.into_inner();
                                let ident = assignment_inner.next().unwrap().as_str();
                                let value_expr =
                                    pairs_to_expr(assignment_inner.next().unwrap().into_inner())
                                        .unwrap();
                                let value_formatted = format_expr(&value_expr, Some(80));
                                format!("{} = {}", ident, value_formatted)
                            } else {
                                assignment_or_ident.as_str().to_string()
                            };

                            formatted_statements.push(format!("output {}", formatted));
                        }
                        _ => {
                            if let Ok(expr) = pairs_to_expr(inner_pair.into_inner()) {
                                let formatted = format_expr(&expr, Some(80));
                                formatted_statements.push(formatted);
                            }
                        }
                    }
                }
            }
        }

        assert_eq!(formatted_statements.len(), 1);
        assert_eq!(formatted_statements[0], "output result = 42");
    }

    #[test]
    fn test_output_statement_separate() {
        use crate::parser::Rule;

        let source = "result = 42\noutput result";
        let pairs = get_pairs(source).unwrap();

        let mut formatted_statements = Vec::new();

        for pair in pairs {
            if pair.as_rule() == Rule::statement {
                if let Some(inner_pair) = pair.into_inner().next() {
                    match inner_pair.as_rule() {
                        Rule::output_declaration => {
                            // Should preserve the output keyword
                            let mut inner = inner_pair.into_inner();
                            let assignment_or_ident = inner.next().unwrap();

                            let formatted = if assignment_or_ident.as_rule() == Rule::assignment {
                                // Extract identifier and value from assignment
                                let mut assignment_inner = assignment_or_ident.into_inner();
                                let ident = assignment_inner.next().unwrap().as_str();
                                let value_expr =
                                    pairs_to_expr(assignment_inner.next().unwrap().into_inner())
                                        .unwrap();
                                let value_formatted = format_expr(&value_expr, Some(80));
                                format!("{} = {}", ident, value_formatted)
                            } else {
                                assignment_or_ident.as_str().to_string()
                            };

                            formatted_statements.push(format!("output {}", formatted));
                        }
                        _ => {
                            if let Ok(expr) = pairs_to_expr(inner_pair.into_inner()) {
                                let formatted = format_expr(&expr, Some(80));
                                formatted_statements.push(formatted);
                            }
                        }
                    }
                }
            }
        }

        assert_eq!(formatted_statements.len(), 2);
        assert_eq!(formatted_statements[0], "result = 42");
        assert_eq!(formatted_statements[1], "output result");
    }

    #[test]
    fn test_output_with_complex_expression() {
        use crate::parser::Rule;

        let source = "output total = [1, 2, 3] into sum";
        let pairs = get_pairs(source).unwrap();

        let mut formatted_statements = Vec::new();

        for pair in pairs {
            if pair.as_rule() == Rule::statement {
                if let Some(inner_pair) = pair.into_inner().next() {
                    match inner_pair.as_rule() {
                        Rule::output_declaration => {
                            // Should preserve the output keyword
                            let mut inner = inner_pair.into_inner();
                            let assignment_or_ident = inner.next().unwrap();

                            let formatted = if assignment_or_ident.as_rule() == Rule::assignment {
                                // Extract identifier and value from assignment
                                let mut assignment_inner = assignment_or_ident.into_inner();
                                let ident = assignment_inner.next().unwrap().as_str();
                                let value_expr =
                                    pairs_to_expr(assignment_inner.next().unwrap().into_inner())
                                        .unwrap();
                                let value_formatted = format_expr(&value_expr, Some(80));
                                format!("{} = {}", ident, value_formatted)
                            } else {
                                assignment_or_ident.as_str().to_string()
                            };

                            formatted_statements.push(format!("output {}", formatted));
                        }
                        _ => {
                            if let Ok(expr) = pairs_to_expr(inner_pair.into_inner()) {
                                let formatted = format_expr(&expr, Some(80));
                                formatted_statements.push(formatted);
                            }
                        }
                    }
                }
            }
        }

        assert_eq!(formatted_statements.len(), 1);
        assert_eq!(formatted_statements[0], "output total = [1, 2, 3] into sum");
    }

    #[test]
    fn test_end_of_line_comments() {
        use crate::parser::Rule;

        let source = "x = 5  // this is an end-of-line comment\ny = 10";
        let pairs = get_pairs(source).unwrap();

        let mut formatted_statements = Vec::new();

        for pair in pairs {
            if pair.as_rule() == Rule::statement {
                let mut inner_pairs = pair.into_inner();

                if let Some(first_pair) = inner_pairs.next() {
                    let formatted = match first_pair.as_rule() {
                        Rule::comment => first_pair.as_str().to_string(),
                        _ => {
                            if let Ok(expr) = pairs_to_expr(first_pair.into_inner()) {
                                format_expr(&expr, Some(80))
                            } else {
                                continue;
                            }
                        }
                    };

                    // Check for end-of-line comment (second element in statement)
                    if let Some(eol_comment) = inner_pairs.next() {
                        if eol_comment.as_rule() == Rule::comment {
                            formatted_statements.push(format!(
                                "{}  {}",
                                formatted,
                                eol_comment.as_str()
                            ));
                        } else {
                            formatted_statements.push(formatted);
                        }
                    } else {
                        formatted_statements.push(formatted);
                    }
                }
            }
        }

        assert_eq!(formatted_statements.len(), 2);
        assert_eq!(
            formatted_statements[0],
            "x = 5  // this is an end-of-line comment"
        );
        assert_eq!(formatted_statements[1], "y = 10");
    }

    #[test]
    fn test_multiple_end_of_line_comments() {
        use crate::parser::Rule;

        let source = "a = 1  // comment 1\nb = 2  // comment 2\nc = 3";
        let pairs = get_pairs(source).unwrap();

        let mut formatted_statements = Vec::new();

        for pair in pairs {
            if pair.as_rule() == Rule::statement {
                let mut inner_pairs = pair.into_inner();

                if let Some(first_pair) = inner_pairs.next() {
                    let formatted = match first_pair.as_rule() {
                        Rule::comment => first_pair.as_str().to_string(),
                        _ => {
                            if let Ok(expr) = pairs_to_expr(first_pair.into_inner()) {
                                format_expr(&expr, Some(80))
                            } else {
                                continue;
                            }
                        }
                    };

                    // Check for end-of-line comment
                    if let Some(eol_comment) = inner_pairs.next() {
                        if eol_comment.as_rule() == Rule::comment {
                            formatted_statements.push(format!(
                                "{}  {}",
                                formatted,
                                eol_comment.as_str()
                            ));
                        } else {
                            formatted_statements.push(formatted);
                        }
                    } else {
                        formatted_statements.push(formatted);
                    }
                }
            }
        }

        assert_eq!(formatted_statements.len(), 3);
        assert_eq!(formatted_statements[0], "a = 1  // comment 1");
        assert_eq!(formatted_statements[1], "b = 2  // comment 2");
        assert_eq!(formatted_statements[2], "c = 3");
    }

    #[test]
    fn test_eol_comments_not_joined_with_next_line() {
        use crate::parser::Rule;

        // Ensure statements with EOL comments remain on their own line
        let source = "x = 1  // first value\ny = 2  // second value\nz = x + y";
        let pairs = get_pairs(source).unwrap();

        let mut formatted_statements = Vec::new();

        for pair in pairs {
            if pair.as_rule() == Rule::statement {
                let mut inner_pairs = pair.into_inner();

                if let Some(first_pair) = inner_pairs.next() {
                    let formatted = match first_pair.as_rule() {
                        Rule::comment => first_pair.as_str().to_string(),
                        _ => {
                            if let Ok(expr) = pairs_to_expr(first_pair.into_inner()) {
                                format_expr(&expr, Some(80))
                            } else {
                                continue;
                            }
                        }
                    };

                    // Check for end-of-line comment
                    if let Some(eol_comment) = inner_pairs.next() {
                        if eol_comment.as_rule() == Rule::comment {
                            formatted_statements.push(format!(
                                "{}  {}",
                                formatted,
                                eol_comment.as_str()
                            ));
                        } else {
                            formatted_statements.push(formatted);
                        }
                    } else {
                        formatted_statements.push(formatted);
                    }
                }
            }
        }

        // Join with newlines - each statement should be on its own line
        let result = formatted_statements.join("\n");

        assert_eq!(formatted_statements.len(), 3);
        // Verify no statement got joined into one line
        assert_eq!(result.lines().count(), 3);
        assert_eq!(formatted_statements[0], "x = 1  // first value");
        assert_eq!(formatted_statements[1], "y = 2  // second value");
        assert_eq!(formatted_statements[2], "z = x + y");

        // Ensure the result doesn't contain any line with multiple statements
        for line in result.lines() {
            // Count equals signs - should only be 1 per line
            assert_eq!(
                line.matches('=').count(),
                1,
                "Line should not contain multiple statements: {}",
                line
            );
        }
    }

    #[test]
    fn test_eol_comments_with_line_breaking() {
        use crate::parser::Rule;

        // Test that expressions can still be broken across lines when they have EOL comments
        let source = "longRecord = {name: \"Alice\", age: 30, email: \"alice@example.com\", address: \"123 Main St\"}  // user data";
        let pairs = get_pairs(source).unwrap();

        let mut formatted_statements = Vec::new();

        for pair in pairs {
            if pair.as_rule() == Rule::statement {
                let mut inner_pairs = pair.into_inner();

                if let Some(first_pair) = inner_pairs.next() {
                    let formatted = match first_pair.as_rule() {
                        Rule::comment => first_pair.as_str().to_string(),
                        _ => {
                            if let Ok(expr) = pairs_to_expr(first_pair.into_inner()) {
                                // Use a shorter line limit to force breaking
                                format_expr(&expr, Some(40))
                            } else {
                                continue;
                            }
                        }
                    };

                    // Check for end-of-line comment
                    if let Some(eol_comment) = inner_pairs.next() {
                        if eol_comment.as_rule() == Rule::comment {
                            formatted_statements.push(format!(
                                "{}  {}",
                                formatted,
                                eol_comment.as_str()
                            ));
                        } else {
                            formatted_statements.push(formatted);
                        }
                    } else {
                        formatted_statements.push(formatted);
                    }
                }
            }
        }

        assert_eq!(formatted_statements.len(), 1);
        let result = &formatted_statements[0];

        // The comment should be at the end
        assert!(result.ends_with("// user data"));
        // The expression should be formatted (likely multi-line)
        assert!(result.contains("longRecord = "));
    }

    #[test]
    fn test_actual_line_breaking_behavior() {
        use crate::parser::Rule;

        // Test what actually happens with long lines
        let source = "x = {name: \"Alice\", age: 30, email: \"alice@example.com\", address: \"123 Main St\", city: \"Springfield\"}";
        let pairs = get_pairs(source).unwrap();

        for pair in pairs {
            if pair.as_rule() == Rule::statement {
                if let Some(inner_pair) = pair.into_inner().next() {
                    if let Ok(expr) = pairs_to_expr(inner_pair.into_inner()) {
                        let formatted = format_expr(&expr, Some(40));
                        println!("Formatted output:\n{}", formatted);
                        println!("Line count: {}", formatted.lines().count());

                        // Check if it's actually breaking lines
                        if formatted.lines().count() > 1 {
                            println!("✓ Lines were broken");
                        } else {
                            println!(
                                "✗ No line breaking occurred - output is {} chars",
                                formatted.len()
                            );
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_multiline_input_gets_collapsed() {
        use crate::parser::Rule;

        // Test if multiline input gets collapsed to a single line
        let source = "x = [\n  1,\n  2,\n  3\n]";
        let pairs = get_pairs(source).unwrap();

        for pair in pairs {
            if pair.as_rule() == Rule::statement {
                if let Some(inner_pair) = pair.into_inner().next() {
                    if let Ok(expr) = pairs_to_expr(inner_pair.into_inner()) {
                        let formatted = format_expr(&expr, Some(80));
                        println!("Input:\n{}", source);
                        println!("Output:\n{}", formatted);

                        // Currently it collapses to single line
                        assert_eq!(formatted, "x = [1, 2, 3]");
                    }
                }
            }
        }
    }

    #[test]
    fn test_empty_lines_between_statements() {
        use crate::parser::Rule;

        // Test that empty lines between statements are preserved (up to 2)
        let source = "x = 1\n\ny = 2\n\n\n\nz = 3";
        let pairs = get_pairs(source).unwrap();

        let mut statements_with_positions = Vec::new();

        for pair in pairs {
            if pair.as_rule() == Rule::statement {
                let start_line = pair.as_span().start_pos().line_col().0;
                let end_line = pair.as_span().end_pos().line_col().0;

                if let Some(inner_pair) = pair.into_inner().next() {
                    if let Ok(expr) = pairs_to_expr(inner_pair.into_inner()) {
                        let formatted = format_expr(&expr, Some(80));
                        statements_with_positions.push((formatted, start_line, end_line));
                    }
                }
            }
        }

        println!("Statement positions: {:?}", statements_with_positions);

        // Use the new helper function to join with spacing
        let result = join_statements_with_spacing(&statements_with_positions);
        println!("Output with preserved spacing:\n{}", result);

        // We want to preserve 1 empty line between x and y, and cap at 2 between y and z
        // So it should be:
        // x = 1
        // <blank>
        // y = 2
        // <blank>
        // <blank>
        // z = 3

        assert_eq!(result.lines().count(), 6); // 3 statements + 1 blank + 2 blanks
        assert_eq!(result, "x = 1\n\ny = 2\n\n\nz = 3");
    }

    #[test]
    fn test_assignment_with_long_list_indentation() {
        use crate::parser::Rule;

        // Test that assignments with lists don't have excessive indentation
        let source = "ingredients = [{name: \"sugar\", amount: 1}, {name: \"flour\", amount: 2}]";
        let pairs = get_pairs(source).unwrap();

        for pair in pairs {
            if pair.as_rule() == Rule::statement {
                if let Some(inner_pair) = pair.into_inner().next() {
                    if let Ok(expr) = pairs_to_expr(inner_pair.into_inner()) {
                        let formatted = format_expr(&expr, Some(40));
                        println!("Formatted:\n{}", formatted);

                        // The list items should be indented by 2 spaces, not pushed over by the "ingredients = " prefix
                        assert!(formatted.contains("[\n  {"));
                        assert!(!formatted.contains("            ")); // Should not have excessive indentation
                    }
                }
            }
        }
    }

    #[test]
    fn test_else_if_chain_stays_flat() {
        // Test that else-if chains don't create staircase indentation
        let source = "x = if a then 1 else if b then 2 else if c then 3 else 4";
        let expr = parse_test_expr(source);
        let formatted = format_expr(&expr, Some(40));
        println!("Formatted:\n{}", formatted);

        // All "else if" should be at the same indentation level (no staircase)
        // Count how many times we see "else if" - should be 2
        let else_if_count = formatted.matches("else if").count();
        assert_eq!(else_if_count, 2, "Should have 2 'else if' clauses");

        // Check that there's no increasing indentation (staircase pattern)
        // The then branches should be indented by 2 spaces
        for line in formatted.lines() {
            let leading_spaces = line.len() - line.trim_start().len();
            // No line should be indented more than 2 spaces
            assert!(
                leading_spaces <= 2,
                "Line has too much indentation: '{}'",
                line
            );
        }
    }

    #[test]
    fn test_long_else_if_chain() {
        // Test with a longer else-if chain similar to the user's tax bracket example
        let source = "tax = if income <= 10000 then income * 0.1 else if income <= 50000 then 1000 + (income - 10000) * 0.2 else if income <= 100000 then 9000 + (income - 50000) * 0.3 else 24000 + (income - 100000) * 0.4";
        let expr = parse_test_expr(source);
        let formatted = format_expr(&expr, Some(60));
        println!("Formatted:\n{}", formatted);

        // Should have 3 'else if' or 'else' clauses and all at the same level
        let else_count = formatted.matches("\nelse").count();
        assert!(
            else_count >= 3,
            "Should have at least 3 else/else-if clauses, found {}",
            else_count
        );

        // Verify no staircase - each "else if" should start at column 0
        for line in formatted.lines() {
            if line.starts_with("else") {
                // "else" and "else if" should start at the beginning of the line
                assert!(
                    line.starts_with("else"),
                    "else clause should start at column 0: '{}'",
                    line
                );
            }
        }
    }

    #[test]
    fn test_multiline_preserves_precedence_parentheses() {
        // Test that multiline formatting preserves necessary parentheses for precedence
        // This is the loan amortization formula: loan * numerator / denominator
        // The denominator needs parens because subtraction has lower precedence than division
        let source = "result = amount * (rate * (1 + rate) ^ n) / ((1 + rate) ^ n - 1)";
        let expr = parse_test_expr(source);
        let formatted = format_expr(&expr, Some(40));
        println!("Formatted:\n{}", formatted);

        // The denominator ((1 + rate) ^ n - 1) must be wrapped in parentheses
        // because subtraction has lower precedence than division
        assert!(
            formatted.contains("/ ((1 + rate) ^ n - 1)"),
            "Denominator should be wrapped in parentheses to preserve precedence. Got:\n{}",
            formatted
        );
    }

    #[test]
    fn test_multiline_division_with_subtraction() {
        // Simpler test: a / (b - c) should preserve the parens
        let source = "x = a / (b - c)";
        let expr = parse_test_expr(source);
        let formatted = format_expr(&expr, Some(80));

        assert_eq!(formatted, "x = a / (b - c)");
    }

    #[test]
    fn test_multiline_multiplication_with_addition() {
        // a * (b + c) should preserve the parens
        let source = "x = a * (b + c)";
        let expr = parse_test_expr(source);
        let formatted = format_expr(&expr, Some(80));

        assert_eq!(formatted, "x = a * (b + c)");
    }

    #[test]
    fn test_else_indented_in_lambda_body() {
        // When a conditional is inside a lambda body, the else keyword should be
        // indented to match the if, not placed at column 0
        let source = "is_positive = n => if n > 0 then true else false";
        let expr = parse_test_expr(source);
        let formatted = format_expr(&expr, Some(30)); // Force multiline

        // The else should be indented, not at column 0
        for line in formatted.lines() {
            if line.trim().starts_with("else") {
                let leading_spaces = line.len() - line.trim_start().len();
                assert!(
                    leading_spaces > 0,
                    "else should be indented in lambda body, got: '{}'",
                    line
                );
            }
        }

        // Verify the formatted output is still valid syntax by re-parsing
        let reparsed = parse_test_expr(&formatted);
        assert!(matches!(reparsed.node, Expr::Assignment { .. }));
    }

    #[test]
    fn test_via_breaks_before_operator_not_after() {
        // When a via expression is too long, it should break BEFORE the operator,
        // not after (breaking after would leave 'via' at end of line, which is a parse error)
        let source = "result = [1, 2, 3, 4, 5] via (x) => x * 2";
        let expr = parse_test_expr(source);
        let formatted = format_expr(&expr, Some(30)); // Force multiline

        // If it breaks, 'via' should be at the start of a line, not at the end
        for line in formatted.lines() {
            assert!(
                !line.trim_end().ends_with("via"),
                "via should not be at end of line (would be parse error), got: '{}'",
                line
            );
        }

        // Verify the formatted output is still valid syntax by re-parsing
        let reparsed = parse_test_expr(&formatted);
        assert!(matches!(reparsed.node, Expr::Assignment { .. }));
    }

    #[test]
    fn test_record_keys_with_spaces_preserve_quotes() {
        // Record keys with spaces need to be quoted to be valid syntax
        let source = r#"x = {"my key": 1, "another-key": 2}"#;
        let expr = parse_test_expr(source);
        let formatted = format_expr(&expr, Some(80));

        // The formatted output should preserve the quotes around keys with spaces/dashes
        assert!(
            formatted.contains(r#""my key""#),
            "Key with spaces should remain quoted, got: {}",
            formatted
        );
        assert!(
            formatted.contains(r#""another-key""#),
            "Key with dashes should remain quoted, got: {}",
            formatted
        );

        // Verify the formatted output is still valid syntax by re-parsing
        let reparsed = parse_test_expr(&formatted);
        assert!(matches!(reparsed.node, Expr::Assignment { .. }));
    }

    #[test]
    fn test_record_keys_valid_identifiers_no_quotes() {
        // Record keys that are valid identifiers should NOT have quotes
        let source = r#"x = {name: "Alice", age: 30}"#;
        let expr = parse_test_expr(source);
        let formatted = format_expr(&expr, Some(80));

        // Valid identifier keys should not be quoted
        assert_eq!(formatted, r#"x = {name: "Alice", age: 30}"#);
    }

    #[test]
    fn test_record_keys_starting_with_number_need_quotes() {
        // Record keys starting with a number are not valid identifiers
        let source = r#"x = {"123abc": 1}"#;
        let expr = parse_test_expr(source);
        let formatted = format_expr(&expr, Some(80));

        assert!(
            formatted.contains(r#""123abc""#),
            "Key starting with number should remain quoted, got: {}",
            formatted
        );

        // Verify the formatted output is still valid syntax by re-parsing
        let reparsed = parse_test_expr(&formatted);
        assert!(matches!(reparsed.node, Expr::Assignment { .. }));
    }
}
