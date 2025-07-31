use crate::parser::{get_pairs, Rule};
use anyhow::Result;
use pest::iterators::Pair;

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

        Ok(())
    }

    fn format_record(&mut self, pair: Pair<Rule>) -> Result<()> {
        let items: Vec<_> = pair.into_inner().collect();

        if items.is_empty() {
            self.write("{}");
            return Ok(());
        }

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

        self.write("(");
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                self.write(", ");
            }
            self.format_spreadable_expression(arg.clone())?;
        }

        if self.config.trailing_comma && args.len() > 1 {
            self.write(",");
        }
        self.write(")");

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

    fn format_comment(&mut self, pair: Pair<Rule>) -> Result<()> {
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
        assert_eq!(output, "func(a, b, c,)");
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
}
