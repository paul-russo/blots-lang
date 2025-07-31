use crate::parser::{get_pairs, Rule};
use anyhow::Result;
use pest::iterators::Pair;
use regex::Regex;

#[derive(Debug, Clone)]
pub struct FormatterConfig {
    pub indent_width: usize,
    pub max_line_length: usize,
    pub use_tabs: bool,
    pub trailing_comma: bool,
    pub spaces_around_operators: bool,
}

impl Default for FormatterConfig {
    fn default() -> Self {
        Self {
            indent_width: 2,
            max_line_length: 80,
            use_tabs: false,
            trailing_comma: true,
            spaces_around_operators: true,
        }
    }
}

#[derive(Clone)]
pub struct Formatter {
    config: FormatterConfig,
    output: String,
    indent_level: usize,
    current_line_length: usize,
}

impl Formatter {
    pub fn new(config: FormatterConfig) -> Self {
        Self {
            config,
            output: String::new(),
            indent_level: 0,
            current_line_length: 0,
        }
    }

    pub fn format(input: &str) -> Result<String> {
        let config = FormatterConfig::default();
        Self::format_with_config(input, config)
    }
    
    /// Format code while preserving comments
    pub fn format_preserving_comments(input: &str) -> Result<String> {
        let config = FormatterConfig::default();
        Self::format_with_config_preserving_comments(input, config)
    }
    
    pub fn format_with_config_preserving_comments(input: &str, config: FormatterConfig) -> Result<String> {
        // Track brace depth to avoid splitting inside blocks
        let lines: Vec<&str> = input.lines().collect();
        let mut result = Vec::new();
        let mut current_section = Vec::new();
        let mut in_code_section = false;
        let mut brace_depth: i32 = 0;
        let mut paren_depth: i32 = 0;
        let mut bracket_depth: i32 = 0;
        
        for line in lines {
            // Count braces/brackets/parens (simple approach - doesn't handle strings/comments)
            for ch in line.chars() {
                match ch {
                    '{' => brace_depth += 1,
                    '}' => brace_depth = brace_depth.saturating_sub(1),
                    '(' => paren_depth += 1,
                    ')' => paren_depth = paren_depth.saturating_sub(1),
                    '[' => bracket_depth += 1,
                    ']' => bracket_depth = bracket_depth.saturating_sub(1),
                    _ => {}
                }
            }
            
            let in_block = brace_depth > 0 || paren_depth > 0 || bracket_depth > 0;
            
            if line.trim().is_empty() {
                // Blank line
                if in_code_section && !current_section.is_empty() && !in_block {
                    // Only break sections at top level (not inside blocks)
                    let section_text = current_section.join("\n");
                    let formatted = Self::format_section(&section_text, &config)?;
                    result.push(formatted);
                    current_section.clear();
                    in_code_section = false;
                } else if in_block {
                    // Keep blank lines that are inside blocks
                    current_section.push(line);
                }
                if !in_block {
                    result.push(String::new());
                }
            } else if line.trim().starts_with("//") && !in_block {
                // Comment-only line at top level
                if in_code_section && !current_section.is_empty() {
                    // Format and add the current section
                    let section_text = current_section.join("\n");
                    let formatted = Self::format_section(&section_text, &config)?;
                    result.push(formatted);
                    current_section.clear();
                    in_code_section = false;
                }
                result.push(line.to_string());
            } else {
                // Code line (possibly with inline comment)
                in_code_section = true;
                current_section.push(line);
            }
        }
        
        // Don't forget the last section
        if !current_section.is_empty() {
            let section_text = current_section.join("\n");
            let formatted = Self::format_section(&section_text, &config)?;
            result.push(formatted);
        }
        
        Ok(result.join("\n"))
    }
    
    fn format_section(section: &str, config: &FormatterConfig) -> Result<String> {
        // Simple approach: preserve inline comments
        let comment_regex = Regex::new(r"(//.*)").unwrap();
        let mut code_lines = Vec::new();
        let mut inline_comments = Vec::new();
        
        for line in section.lines() {
            if let Some(captures) = comment_regex.captures(line) {
                let comment = captures.get(1).unwrap().as_str();
                let code_part = line[..captures.get(1).unwrap().start()].trim_end();
                code_lines.push(code_part.to_string());
                inline_comments.push(Some(comment.to_string()));
            } else {
                code_lines.push(line.to_string());
                inline_comments.push(None);
            }
        }
        
        // Format just the code
        let code_only = code_lines.join("\n");
        let formatted = Self::format_with_config(&code_only, config.clone())?;
        let formatted_lines: Vec<&str> = formatted.lines().collect();
        
        // If the number of lines changed, we can't reliably map comments
        if formatted_lines.len() != code_lines.len() {
            // Just return the formatted code without inline comments
            return Ok(formatted);
        }
        
        // Re-add inline comments
        let mut result = Vec::new();
        for (i, formatted_line) in formatted_lines.iter().enumerate() {
            if let Some(Some(comment)) = inline_comments.get(i) {
                result.push(format!("{}  {}", formatted_line, comment));
            } else {
                result.push(formatted_line.to_string());
            }
        }
        
        Ok(result.join("\n"))
    }

    pub fn format_with_config(input: &str, config: FormatterConfig) -> Result<String> {
        let mut formatter = Self::new(config);
        let input_string = input.to_string();
        let pairs = get_pairs(&input_string)?;

        for pair in pairs {
            formatter.format_statement(pair)?;
        }

        Ok(formatter.output.trim_end().to_string())
    }

    fn write(&mut self, s: &str) {
        self.output.push_str(s);
        self.current_line_length += s.len();
    }

    fn newline(&mut self) {
        self.output.push('\n');
        self.current_line_length = 0;
    }

    fn indent(&mut self) {
        let indent = self.get_indent();
        self.write(&indent);
    }

    fn get_indent(&self) -> String {
        if self.config.use_tabs {
            "\t".repeat(self.indent_level)
        } else {
            " ".repeat(self.indent_level * self.config.indent_width)
        }
    }

    fn increase_indent(&mut self) {
        self.indent_level += 1;
    }

    fn decrease_indent(&mut self) {
        if self.indent_level > 0 {
            self.indent_level -= 1;
        }
    }

    fn format_statement(&mut self, pair: Pair<Rule>) -> Result<()> {
        match pair.as_rule() {
            Rule::statement => {
                let has_content = pair.clone().into_inner().any(|inner| {
                    matches!(
                        inner.as_rule(),
                        Rule::output_declaration | Rule::expression | Rule::comment
                    )
                });

                if has_content {
                    for inner in pair.into_inner() {
                        match inner.as_rule() {
                            Rule::output_declaration => self.format_output_declaration(inner)?,
                            Rule::expression => self.format_expression(inner)?,
                            Rule::comment => self.format_comment(inner)?,
                            _ => {}
                        }
                    }
                    self.newline();
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn format_output_declaration(&mut self, pair: Pair<Rule>) -> Result<()> {
        self.write("output ");
        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::assignment => self.format_assignment(inner)?,
                Rule::identifier => self.write(inner.as_str()),
                _ => {}
            }
        }
        Ok(())
    }

    fn format_assignment(&mut self, pair: Pair<Rule>) -> Result<()> {
        let mut inner = pair.into_inner();
        let ident = inner.next().unwrap();
        let expr = inner.next().unwrap();

        self.write(ident.as_str());
        self.write(" = ");
        self.format_expression(expr)?;
        Ok(())
    }

    fn format_expression(&mut self, pair: Pair<Rule>) -> Result<()> {
        let mut inner = pair.into_inner().peekable();
        let mut first = true;

        while let Some(part) = inner.next() {
            if !first && self.config.spaces_around_operators {
                match part.as_rule() {
                    Rule::add
                    | Rule::subtract
                    | Rule::multiply
                    | Rule::divide
                    | Rule::modulo
                    | Rule::power
                    | Rule::equal
                    | Rule::not_equal
                    | Rule::less_eq
                    | Rule::less
                    | Rule::greater_eq
                    | Rule::greater
                    | Rule::and
                    | Rule::or
                    | Rule::coalesce
                    | Rule::natural_and
                    | Rule::natural_or
                    | Rule::with => {
                        self.write(" ");
                        self.write(part.as_str());
                        self.write(" ");
                        continue;
                    }
                    _ => {}
                }
            }
            first = false;

            self.format_term(part)?;
        }
        Ok(())
    }

    fn format_term(&mut self, pair: Pair<Rule>) -> Result<()> {
        match pair.as_rule() {
            Rule::conditional => self.format_conditional(pair)?,
            Rule::do_block => self.format_do_block(pair)?,
            Rule::lambda => self.format_lambda(pair)?,
            Rule::assignment => self.format_assignment(pair)?,
            Rule::list => self.format_list(pair)?,
            Rule::record => self.format_record(pair)?,
            Rule::bool => self.write(pair.as_str()),
            Rule::string => self.write(pair.as_str()),
            Rule::null => self.write(pair.as_str()),
            Rule::identifier => self.write(pair.as_str()),
            Rule::number => self.write(pair.as_str()),
            Rule::expression => {
                self.write("(");
                self.format_expression(pair)?;
                self.write(")");
            }
            Rule::negation => self.write("-"),
            Rule::invert => self.write("!"),
            Rule::not => self.write("not "),
            Rule::factorial => self.write("!"),
            Rule::access => {
                self.write("[");
                for inner in pair.into_inner() {
                    self.format_expression(inner)?;
                }
                self.write("]");
            }
            Rule::dot_access => {
                self.write(".");
                for inner in pair.into_inner() {
                    self.write(inner.as_str());
                }
            }
            Rule::call_list => self.format_call_list(pair)?,
            _ => {
                for inner in pair.into_inner() {
                    self.format_term(inner)?;
                }
            }
        }
        Ok(())
    }

    fn format_conditional(&mut self, pair: Pair<Rule>) -> Result<()> {
        let mut inner = pair.into_inner();

        self.write("if ");
        self.format_expression(inner.next().unwrap())?;
        self.write(" then ");
        self.format_expression(inner.next().unwrap())?;
        self.write(" else ");
        self.format_expression(inner.next().unwrap())?;

        Ok(())
    }

    fn format_do_block(&mut self, pair: Pair<Rule>) -> Result<()> {
        self.write("do {");
        self.newline();
        self.increase_indent();

        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::do_statement => {
                    self.indent();
                    for stmt in inner.into_inner() {
                        match stmt.as_rule() {
                            Rule::expression => self.format_expression(stmt)?,
                            Rule::comment => self.format_comment(stmt)?,
                            _ => {}
                        }
                    }
                    self.newline();
                }
                Rule::return_statement => {
                    self.indent();
                    self.write("return ");
                    for expr in inner.into_inner() {
                        if expr.as_rule() == Rule::expression {
                            self.format_expression(expr)?;
                        }
                    }
                    self.newline();
                }
                _ => {}
            }
        }

        self.decrease_indent();
        self.indent();
        self.write("}");
        Ok(())
    }

    fn format_lambda(&mut self, pair: Pair<Rule>) -> Result<()> {
        let mut inner = pair.into_inner();
        let args = inner.next().unwrap();
        let body = inner.next().unwrap();

        self.format_argument_list(args)?;
        self.write(" => ");
        self.format_expression(body)?;

        Ok(())
    }

    fn format_argument_list(&mut self, pair: Pair<Rule>) -> Result<()> {
        let args: Vec<_> = pair.into_inner().collect();

        if args.len() == 1 && matches!(args[0].as_rule(), Rule::required_arg) {
            self.write(args[0].as_str());
        } else {
            self.write("(");
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.format_argument(arg.clone())?;
            }
            self.write(")");
        }
        Ok(())
    }

    fn format_argument(&mut self, pair: Pair<Rule>) -> Result<()> {
        match pair.as_rule() {
            Rule::required_arg => self.write(pair.as_str()),
            Rule::optional_arg => {
                for inner in pair.into_inner() {
                    self.write(inner.as_str());
                }
                self.write("?");
            }
            Rule::rest_arg => {
                self.write("...");
                for inner in pair.into_inner() {
                    self.write(inner.as_str());
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn format_list(&mut self, pair: Pair<Rule>) -> Result<()> {
        let items: Vec<_> = pair.into_inner().collect();

        if items.is_empty() {
            self.write("[]");
            return Ok(());
        }
        
        // Check if we should format as multiline
        let mut test_formatter = self.clone();
        test_formatter.write("[");
        for (i, item) in items.iter().enumerate() {
            if i > 0 {
                test_formatter.write(", ");
            }
            test_formatter.format_spreadable_expression(item.clone())?;
        }
        if test_formatter.config.trailing_comma && items.len() > 1 {
            test_formatter.write(",");
        }
        test_formatter.write("]");
        
        let would_be_length = test_formatter.current_line_length;
        let should_break = would_be_length > self.config.max_line_length;
        
        if should_break && items.len() > 1 {
            // Format as multiline
            self.write("[");
            self.newline();
            self.increase_indent();
            
            for (i, item) in items.iter().enumerate() {
                self.indent();
                self.format_spreadable_expression(item.clone())?;
                if i < items.len() - 1 || self.config.trailing_comma {
                    self.write(",");
                }
                self.newline();
            }
            
            self.decrease_indent();
            self.indent();
            self.write("]");
        } else {
            // Format as single line
            self.write("[");
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.format_spreadable_expression(item.clone())?;
            }
            
            if self.config.trailing_comma && items.len() > 1 {
                self.write(",");
            }
            self.write("]");
        }

        Ok(())
    }

    fn format_record(&mut self, pair: Pair<Rule>) -> Result<()> {
        let items: Vec<_> = pair.into_inner().collect();

        if items.is_empty() {
            self.write("{}");
            return Ok(());
        }
        
        // Check if we should format as multiline
        let mut test_formatter = self.clone();
        test_formatter.write("{");
        for (i, item) in items.iter().enumerate() {
            if i > 0 {
                test_formatter.write(", ");
            }
            match item.as_rule() {
                Rule::record_pair => test_formatter.format_record_pair(item.clone())?,
                Rule::record_shorthand => test_formatter.write(item.as_str()),
                Rule::spread_expression => test_formatter.format_spread_expression(item.clone())?,
                _ => {}
            }
        }
        if test_formatter.config.trailing_comma && items.len() > 1 {
            test_formatter.write(",");
        }
        test_formatter.write("}");
        
        let would_be_length = test_formatter.current_line_length;
        let should_break = would_be_length > self.config.max_line_length;
        
        if should_break && items.len() > 1 {
            // Format as multiline
            self.write("{");
            self.newline();
            self.increase_indent();
            
            for (i, item) in items.iter().enumerate() {
                self.indent();
                match item.as_rule() {
                    Rule::record_pair => self.format_record_pair(item.clone())?,
                    Rule::record_shorthand => self.write(item.as_str()),
                    Rule::spread_expression => self.format_spread_expression(item.clone())?,
                    _ => {}
                }
                if i < items.len() - 1 || self.config.trailing_comma {
                    self.write(",");
                }
                self.newline();
            }
            
            self.decrease_indent();
            self.indent();
            self.write("}");
        } else {
            // Format as single line
            self.write("{");
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }

                match item.as_rule() {
                    Rule::record_pair => self.format_record_pair(item.clone())?,
                    Rule::record_shorthand => self.write(item.as_str()),
                    Rule::spread_expression => self.format_spread_expression(item.clone())?,
                    _ => {}
                }
            }

            if self.config.trailing_comma && items.len() > 1 {
                self.write(",");
            }
            self.write("}");
        }

        Ok(())
    }

    fn format_record_pair(&mut self, pair: Pair<Rule>) -> Result<()> {
        let mut inner = pair.into_inner();
        let key = inner.next().unwrap();
        let value = inner.next().unwrap();

        match key.as_rule() {
            Rule::record_key_static => {
                for inner_key in key.into_inner() {
                    match inner_key.as_rule() {
                        Rule::identifier => self.write(inner_key.as_str()),
                        Rule::string => self.write(inner_key.as_str()),
                        _ => {}
                    }
                }
            }
            Rule::record_key_dynamic => {
                self.write("[");
                for expr in key.into_inner() {
                    self.format_expression(expr)?;
                }
                self.write("]");
            }
            _ => {}
        }

        self.write(": ");
        self.format_expression(value)?;

        Ok(())
    }

    fn format_call_list(&mut self, pair: Pair<Rule>) -> Result<()> {
        let args: Vec<_> = pair.into_inner().collect();
        
        // Check if we should format as multiline
        let mut test_formatter = self.clone();
        test_formatter.write("(");
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                test_formatter.write(", ");
            }
            test_formatter.format_spreadable_expression(arg.clone())?;
        }
        if test_formatter.config.trailing_comma && args.len() > 1 {
            test_formatter.write(",");
        }
        test_formatter.write(")");
        
        let would_be_length = test_formatter.current_line_length;
        let should_break = would_be_length > self.config.max_line_length;
        
        if should_break && args.len() > 1 {
            // Format as multiline
            self.write("(");
            self.newline();
            self.increase_indent();
            
            for (i, arg) in args.iter().enumerate() {
                self.indent();
                self.format_spreadable_expression(arg.clone())?;
                if i < args.len() - 1 || self.config.trailing_comma {
                    self.write(",");
                }
                self.newline();
            }
            
            self.decrease_indent();
            self.indent();
            self.write(")");
        } else {
            // Format as single line
            self.write("(");
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.format_spreadable_expression(arg.clone())?;
            }

            // Don't add trailing commas to function calls on single line
            // Only add them when multiline
            // if self.config.trailing_comma && args.len() > 1 {
            //     self.write(",");
            // }
            self.write(")");
        }

        Ok(())
    }

    fn format_spreadable_expression(&mut self, pair: Pair<Rule>) -> Result<()> {
        match pair.as_rule() {
            Rule::spread_expression => self.format_spread_expression(pair)?,
            _ => self.format_expression(pair)?,
        }
        Ok(())
    }

    fn format_spread_expression(&mut self, pair: Pair<Rule>) -> Result<()> {
        self.write("...");
        for inner in pair.into_inner() {
            if inner.as_rule() == Rule::spread_operator {
                continue;
            }
            self.format_expression(inner)?;
        }
        Ok(())
    }

    fn format_comment(&mut self, _pair: Pair<Rule>) -> Result<()> {
        // Comments are handled at the statement level, not within expressions
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_simple_expression() {
        let input = "1+2*3";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "1 + 2 * 3");
    }

    #[test]
    fn test_format_list() {
        let input = "[1,2,3]";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "[1, 2, 3,]");
    }

    #[test]
    fn test_format_empty_list() {
        let input = "[]";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "[]");
    }

    #[test]
    fn test_format_record() {
        let input = "{a:1,b:2}";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "{a: 1, b: 2,}");
    }

    #[test]
    fn test_format_empty_record() {
        let input = "{}";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "{}");
    }

    #[test]
    fn test_format_lambda() {
        let input = "x=>x+1";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "x => x + 1");
    }

    #[test]
    fn test_format_lambda_with_multiple_args() {
        let input = "(a,b)=>a+b";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "(a, b) => a + b");
    }

    #[test]
    fn test_format_conditional() {
        let input = "if x then y else z";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "if x then y else z");
    }

    #[test]
    fn test_format_assignment() {
        let input = "x=42";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "x = 42");
    }

    #[test]
    fn test_format_spread_in_list() {
        let input = "[...a,1,2]";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "[...a, 1, 2,]");
    }

    #[test]
    fn test_format_spread_in_record() {
        let input = "{...a,b:1}";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "{...a, b: 1,}");
    }

    #[test]
    fn test_format_do_block() {
        let input = "do { x=1\ny=2\nreturn x+y }";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "do {\n  x = 1\n  y = 2\n  return x + y\n}");
    }

    #[test]
    fn test_format_natural_operators() {
        let input = "a and b or not c";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "a and b or not c");
    }

    #[test]
    fn test_format_postfix_operators() {
        let input = "5!";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "5!");
    }

    #[test]
    fn test_format_access_operators() {
        let input = "arr[0]";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "arr[0]");
    }

    #[test]
    fn test_format_dot_access() {
        let input = "obj.prop";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "obj.prop");
    }

    #[test]
    fn test_format_function_call() {
        let input = "func(a,b,c)";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "func(a, b, c)");
    }

    #[test]
    fn test_format_chained_calls() {
        let input = "a.b().c()";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "a.b().c()");
    }

    #[test]
    fn test_format_output_declaration() {
        let input = "output x";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "output x");
    }

    #[test]
    fn test_format_output_with_assignment() {
        let input = "output y=42";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "output y = 42");
    }

    #[test]
    fn test_format_coalesce_operator() {
        let input = "a??b";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "a ?? b");
    }

    #[test]
    fn test_format_power_operator() {
        let input = "x^2";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "x ^ 2");
    }

    #[test]
    fn test_format_comparison_operators() {
        let input = "a<b&&c>=d";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "a < b && c >= d");
    }

    #[test]
    fn test_format_nested_expressions() {
        let input = "((a+b)*c)/d";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "((a + b) * c) / d");
    }

    #[test]
    fn test_format_optional_args() {
        let input = "(a,b?)=>a+b??0";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "(a, b?) => a + b ?? 0");
    }

    #[test]
    fn test_format_rest_args() {
        let input = "(a,...rest)=>rest";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "(a, ...rest) => rest");
    }

    #[test]
    fn test_format_no_trailing_comma_single_item() {
        let mut config = FormatterConfig::default();
        config.trailing_comma = false;
        let input = "[1,2,3]";
        let output = Formatter::format_with_config(input, config).unwrap();
        assert_eq!(output, "[1, 2, 3]");
    }

    #[test]
    fn test_format_long_list() {
        let input = "[100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000]";
        let output = Formatter::format(input).unwrap();
        assert!(output.contains("\n"));
        assert!(output.starts_with("[\n"));
        assert!(output.trim_end().ends_with("\n]"));
    }

    #[test]
    fn test_format_long_record() {
        let input = "{firstName:\"John\",lastName:\"Doe\",age:30,occupation:\"Engineer\",location:\"San Francisco\",country:\"USA\"}";
        let output = Formatter::format(input).unwrap();
        assert!(output.contains("\n"));
        assert!(output.starts_with("{\n"));
        assert!(output.trim_end().ends_with("\n}"));
    }

    #[test]
    fn test_format_long_function_call() {
        let input = "someVeryLongFunctionName(argument1,argument2,argument3,argument4,argument5,argument6,argument7,argument8)";
        let output = Formatter::format(input).unwrap();
        assert!(output.contains("\n"));
        assert!(output.contains("(\n"));
        assert!(output.contains("\n)"));
    }

    #[test]
    fn test_format_short_list_single_line() {
        let input = "[1,2,3]";
        let output = Formatter::format(input).unwrap();
        assert!(!output.contains("\n"));
        assert_eq!(output, "[1, 2, 3,]");
    }

    #[test]
    fn test_format_custom_line_length() {
        let mut config = FormatterConfig::default();
        config.max_line_length = 20;
        let input = "[1,2,3,4,5,6,7,8,9,10]";
        let output = Formatter::format_with_config(input, config).unwrap();
        assert!(output.contains("\n"));
    }

    #[test]
    fn test_format_function_call_no_trailing_comma() {
        let input = "func(a, b, c)";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "func(a, b, c)");
    }

    #[test]
    fn test_format_lambda_with_record_body() {
        let input = "map(list, x => {a: x, b: x * 2})";
        let output = Formatter::format(input).unwrap();
        assert_eq!(output, "map(list, x => {a: x, b: x * 2,})");
    }
    
    #[test]
    fn test_format_preserves_comments() {
        let input = r#"// Start comment
x = 1 + 2
// Middle comment
y = x * 3"#;
        let output = Formatter::format_preserving_comments(input).unwrap();
        assert!(output.contains("// Start comment"));
        assert!(output.contains("// Middle comment"));
        assert!(output.contains("x = 1 + 2"));
        assert!(output.contains("y = x * 3"));
    }
    
    #[test]
    fn test_format_preserves_inline_comments() {
        let input = "x = 1 + 2  // inline comment";
        let output = Formatter::format_preserving_comments(input).unwrap();
        assert!(output.contains("x = 1 + 2"));
        assert!(output.contains("// inline comment"));
    }
    
    #[test]
    fn test_format_preserves_blank_lines() {
        let input = r#"x = 1

y = 2

z = 3"#;
        let output = Formatter::format_preserving_comments(input).unwrap();
        let lines: Vec<&str> = output.lines().collect();
        assert_eq!(lines.len(), 5);
        assert_eq!(lines[1], "");
        assert_eq!(lines[3], "");
    }
    
    #[test]
    fn test_format_preserves_comment_indentation() {
        let input = r#"// Top level comment
  // This comment is indented
    // This comment is more indented
x = 1"#;
        let output = Formatter::format_preserving_comments(input).unwrap();
        assert!(output.contains("// Top level comment"));
        assert!(output.contains("  // This comment is indented"));
        assert!(output.contains("    // This comment is more indented"));
    }
}
