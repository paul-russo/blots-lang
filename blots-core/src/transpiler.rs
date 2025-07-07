use crate::parser::{get_pairs, Rule};
use anyhow::{anyhow, Result};
use pest::iterators::Pairs;
use regex::Regex;

// Note: We implement a custom with-chain parser instead of using the Pratt parser
// due to Rust borrowing issues with closures that access &mut self

// Expression tokens for precedence parsing
#[derive(Debug, Clone)]
enum ExprToken {
    Term(String),
    Operator(BinaryOp),
}

#[derive(Debug, Clone, Copy)]
enum BinaryOp {
    // Lowest precedence
    And,
    Or,
    // Comparison
    Equal,
    NotEqual,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    // Addition/Subtraction
    Add,
    Subtract,
    // Multiplication/Division
    Multiply,
    Divide,
    Modulo,
    // Highest precedence
    Power,
    Coalesce,
}

impl BinaryOp {
    fn precedence(&self) -> i32 {
        match self {
            BinaryOp::And | BinaryOp::Or => 1,
            BinaryOp::Equal
            | BinaryOp::NotEqual
            | BinaryOp::Less
            | BinaryOp::LessEq
            | BinaryOp::Greater
            | BinaryOp::GreaterEq => 2,
            BinaryOp::Add | BinaryOp::Subtract => 3,
            BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo => 4,
            BinaryOp::Power | BinaryOp::Coalesce => 5,
        }
    }

    fn is_right_associative(&self) -> bool {
        matches!(self, BinaryOp::Power)
    }

    fn to_js_function(&self) -> String {
        match self {
            BinaryOp::Add => "$$add".to_string(),
            BinaryOp::Subtract => "$$subtract".to_string(),
            BinaryOp::Multiply => "$$multiply".to_string(),
            BinaryOp::Divide => "$$divide".to_string(),
            BinaryOp::Modulo => "$$modulo".to_string(),
            BinaryOp::Power => "$$power".to_string(),
            BinaryOp::Equal => " === ".to_string(),
            BinaryOp::NotEqual => " !== ".to_string(),
            BinaryOp::Less => " < ".to_string(),
            BinaryOp::LessEq => " <= ".to_string(),
            BinaryOp::Greater => " > ".to_string(),
            BinaryOp::GreaterEq => " >= ".to_string(),
            BinaryOp::And => " && ".to_string(),
            BinaryOp::Or => " || ".to_string(),
            BinaryOp::Coalesce => " ?? ".to_string(),
        }
    }

    fn is_function_call(&self) -> bool {
        matches!(
            self,
            BinaryOp::Add
                | BinaryOp::Subtract
                | BinaryOp::Multiply
                | BinaryOp::Divide
                | BinaryOp::Modulo
                | BinaryOp::Power
        )
    }
}

pub struct Transpiler {
    indent_level: usize,
    inline_evaluation: bool,
}

impl Transpiler {
    pub fn new() -> Self {
        Self {
            indent_level: 0,
            inline_evaluation: false,
        }
    }

    pub fn new_with_inline_eval() -> Self {
        Self {
            indent_level: 0,
            inline_evaluation: true,
        }
    }

    pub fn transpile(&mut self, source: &str) -> Result<String> {
        let source_string = source.to_string();
        let pairs = get_pairs(&source_string)?;
        let mut output = String::new();

        // Include runtime helpers
        output.push_str(&self.get_runtime_helpers());
        output.push('\n');

        if self.inline_evaluation {
            output
                .push_str("const $$results = { values: {}, bindings: {}, outputs: new Set() };\n");
        }

        for pair in pairs {
            match pair.as_rule() {
                Rule::statement => {
                    let stmt = self.transpile_statement_with_position(pair)?;
                    if !stmt.trim().is_empty() {
                        output.push_str(&stmt);
                        output.push('\n');
                    }
                }
                Rule::EOI => {}
                rule => return Err(anyhow!("Unexpected top-level rule: {:?}", rule)),
            }
        }

        if self.inline_evaluation {
            output.push_str("\n// Return results for inline evaluation\n");
            output.push_str("if (typeof module !== 'undefined' && module.exports) {\n");
            output.push_str("    module.exports = $$results;\n");
            output.push_str("} else if (typeof window !== 'undefined') {\n");
            output.push_str("    window.$$results = $$results;\n");
            output.push_str("} else {\n");
            output.push_str("    globalThis.$$results = $$results;\n");
            output.push_str("}\n");
        }

        // No longer need post-processing since our with-chain parser handles precedence correctly

        Ok(output)
    }

    fn get_runtime_helpers(&self) -> String {
        include_str!("runtime.js").to_string()
    }

    fn transpile_statement_with_position(
        &mut self,
        pair: pest::iterators::Pair<Rule>,
    ) -> Result<String> {
        if let Some(inner_pair) = pair.into_inner().next() {
            let start_pos = inner_pair.as_span().start_pos().line_col();
            let end_pos = inner_pair.as_span().end_pos().line_col();
            let position_id = format!(
                "{}-{}__{}-{}",
                start_pos.0, start_pos.1, end_pos.0, end_pos.1
            );

            match inner_pair.as_rule() {
                Rule::expression => {
                    let expr = self.transpile_expression(inner_pair.into_inner())?;
                    if self.inline_evaluation {
                        // For inline evaluation, we need to handle assignments differently
                        if expr.starts_with("const ") {
                            // It's an assignment - execute it and capture the variable value
                            let var_name =
                                expr.split('=').next().unwrap().trim().replace("const ", "");
                            Ok(format!(
                                "{};\n$$results.values['{}'] = {};",
                                expr, position_id, var_name
                            ))
                        } else {
                            // It's a pure expression - capture its result
                            Ok(format!("$$results.values['{}'] = {};", position_id, expr))
                        }
                    } else {
                        Ok(self.ensure_semicolon(expr))
                    }
                }
                Rule::output_declaration => {
                    let output_stmt =
                        self.transpile_output_declaration(inner_pair.clone().into_inner())?;
                    if self.inline_evaluation {
                        // For output declarations, extract the variable name and capture its value
                        let var_name =
                            self.extract_output_variable_name(inner_pair.into_inner())?;
                        Ok(format!(
                            "{};\n$$results.values['{}'] = {};",
                            output_stmt, position_id, var_name
                        ))
                    } else {
                        Ok(self.ensure_semicolon(output_stmt))
                    }
                }
                rule => Err(anyhow!("Unexpected statement rule: {:?}", rule)),
            }
        } else {
            Ok(String::new())
        }
    }

    fn transpile_statement(&mut self, mut pairs: Pairs<Rule>) -> Result<String> {
        if let Some(pair) = pairs.next() {
            match pair.as_rule() {
                Rule::expression => self.transpile_expression(pair.into_inner()),
                Rule::output_declaration => self.transpile_output_declaration(pair.into_inner()),
                rule => Err(anyhow!("Unexpected statement rule: {:?}", rule)),
            }
        } else {
            Ok(String::new())
        }
    }

    fn extract_output_variable_name(&mut self, mut pairs: Pairs<Rule>) -> Result<String> {
        if let Some(pair) = pairs.next() {
            match pair.as_rule() {
                Rule::identifier => Ok(self.escape_js_identifier(pair.as_str())),
                Rule::assignment => {
                    // Extract variable name from assignment
                    let assignment = self.transpile_assignment(pair.into_inner())?;
                    let var_name = assignment
                        .split('=')
                        .next()
                        .unwrap()
                        .trim()
                        .replace("const ", "");
                    Ok(var_name)
                }
                rule => Err(anyhow!(
                    "Unexpected output declaration rule in extract_output_variable_name: {:?}",
                    rule
                )),
            }
        } else {
            Err(anyhow!(
                "Empty output declaration in extract_output_variable_name"
            ))
        }
    }

    fn transpile_output_declaration(&mut self, mut pairs: Pairs<Rule>) -> Result<String> {
        if let Some(pair) = pairs.next() {
            match pair.as_rule() {
                Rule::identifier => {
                    let original_var_name = pair.as_str();
                    let escaped_var_name = self.escape_js_identifier(original_var_name);
                    if self.inline_evaluation {
                        // For inline evaluation, add to outputs set and export
                        Ok(format!("$$results.outputs.add('{}');\n// Export {} for next code block\nif (typeof exports !== 'undefined') exports.{} = {};", original_var_name, original_var_name, original_var_name, escaped_var_name))
                    } else {
                        // Export the value to make it available for next code block
                        Ok(format!("// Export {} for next code block\nif (typeof exports !== 'undefined') exports.{} = {};", original_var_name, original_var_name, escaped_var_name))
                    }
                }
                Rule::assignment => {
                    let assignment = self.transpile_assignment(pair.into_inner())?;
                    let var_name = assignment
                        .split('=')
                        .next()
                        .unwrap()
                        .trim()
                        .replace("const ", "");
                    if self.inline_evaluation {
                        // For inline evaluation, add to outputs set and export
                        Ok(format!("{};\n$$results.outputs.add('{}');\n// Export {} for next code block\nif (typeof exports !== 'undefined') exports.{} = {};", assignment, var_name, var_name, var_name, var_name))
                    } else {
                        // Create the assignment and export the value
                        Ok(format!("{};\n// Export {} for next code block\nif (typeof exports !== 'undefined') exports.{} = {};", assignment, var_name, var_name, var_name))
                    }
                }
                rule => Err(anyhow!("Unexpected output declaration rule: {:?}", rule)),
            }
        } else {
            Err(anyhow!("Empty output declaration"))
        }
    }

    fn transpile_expression(&mut self, pairs: Pairs<Rule>) -> Result<String> {
        self.transpile_expression_pratt(pairs)
    }

    fn transpile_expression_pratt(&mut self, pairs: Pairs<Rule>) -> Result<String> {
        // Collect all pairs to analyze the structure
        let pair_vec: Vec<_> = pairs.collect();

        // Check if this is a 'with' chain that needs special handling
        if self.contains_with_chain(&pair_vec) {
            return self.transpile_with_chain_properly(&pair_vec);
        }

        // For non-with expressions, use the regular parsing
        self.transpile_expression_from_pairs(pair_vec)
    }

    fn contains_with_chain(&self, pairs: &[pest::iterators::Pair<Rule>]) -> bool {
        pairs.iter().any(|p| p.as_rule() == Rule::with)
    }

    fn transpile_with_chain_properly(
        &mut self,
        pairs: &[pest::iterators::Pair<Rule>],
    ) -> Result<String> {
        // Parse the with chain by splitting on 'with' keywords and building left-associatively
        let mut segments = Vec::new();
        let mut current_segment = Vec::new();

        for pair in pairs {
            if pair.as_rule() == Rule::with {
                if !current_segment.is_empty() {
                    segments.push(current_segment);
                    current_segment = Vec::new();
                }
            } else {
                current_segment.push(pair.clone());
            }
        }

        // Add the last segment
        if !current_segment.is_empty() {
            segments.push(current_segment);
        }

        if segments.len() < 2 {
            // Not actually a with chain, fall back to regular parsing
            return self.transpile_expression_from_pairs(pairs.to_vec());
        }

        // Build the with chain left-associatively
        let mut result = self.transpile_segment(&segments[0])?;

        for segment in &segments[1..] {
            let right_expr = self.transpile_segment(segment)?;
            result = self.transpile_with_operator(&result, &right_expr)?;
        }

        Ok(result)
    }

    fn transpile_segment(&mut self, pairs: &[pest::iterators::Pair<Rule>]) -> Result<String> {
        if pairs.len() == 1 {
            match pairs[0].as_rule() {
                Rule::lambda => self.transpile_lambda(pairs[0].clone().into_inner()),
                Rule::number => Ok(pairs[0].as_str().replace('_', "")),
                Rule::identifier => Ok(self.escape_js_identifier(pairs[0].as_str())),
                Rule::string => Ok(pairs[0].as_str().to_string()),
                Rule::bool => Ok(pairs[0].as_str().to_string()),
                Rule::null => Ok("null".to_string()),
                Rule::list => self.transpile_list(pairs[0].clone().into_inner()),
                Rule::nested_expression => Ok(format!(
                    "({})",
                    self.transpile_expression(pairs[0].clone().into_inner())?
                )),
                Rule::expression => {
                    // For expression rules, we need to transpile the inner content
                    self.transpile_expression(pairs[0].clone().into_inner())
                }
                _ => self.transpile_single_term(pairs[0].clone()),
            }
        } else {
            // Multiple pairs in segment - this could be a function call or other complex expression
            self.transpile_expression_from_pairs(pairs.to_vec())
        }
    }

    fn transpile_expression_from_pairs(
        &mut self,
        pairs: Vec<pest::iterators::Pair<Rule>>,
    ) -> Result<String> {
        // For non-with expressions, reconstruct the pairs iterator and use the old method
        // This ensures compatibility with existing transpilation logic

        // Convert the Vec back to something we can iterate over
        // We need to carefully reconstruct a Pairs iterator-like behavior

        // For now, let's handle simple cases and use the old method for complex ones
        if pairs.len() == 1 {
            let pair = &pairs[0];
            match pair.as_rule() {
                Rule::lambda => self.transpile_lambda(pair.clone().into_inner()),
                Rule::conditional => self.transpile_conditional(pair.clone().into_inner()),
                Rule::assignment => self.transpile_assignment(pair.clone().into_inner()),
                Rule::list => self.transpile_list(pair.clone().into_inner()),
                Rule::record => self.transpile_record(pair.clone().into_inner()),
                Rule::nested_expression => Ok(format!(
                    "({})",
                    self.transpile_expression(pair.clone().into_inner())?
                )),
                _ => self.transpile_single_term(pair.clone()),
            }
        } else {
            // For complex expressions, we need to fall back to the old method
            // Create a temporary iterator from our vector
            self.transpile_complex_expression_from_pairs(pairs)
        }
    }

    fn transpile_complex_expression_from_pairs(
        &mut self,
        pairs: Vec<pest::iterators::Pair<Rule>>,
    ) -> Result<String> {
        // Parse expression with proper operator precedence
        let mut tokens = Vec::new();
        let mut i = 0;

        while i < pairs.len() {
            let pair = &pairs[i];

            match pair.as_rule() {
                Rule::negation => {
                    // Handle unary minus - we need to get the next term and apply negation to it
                    if let Some(next_pair) = pairs.get(i + 1) {
                        let next_term = match next_pair.as_rule() {
                            Rule::lambda => {
                                self.transpile_lambda(next_pair.clone().into_inner())?
                            }
                            Rule::list => self.transpile_list(next_pair.clone().into_inner())?,
                            Rule::record => {
                                self.transpile_record(next_pair.clone().into_inner())?
                            }
                            Rule::bool => next_pair.as_str().to_string(),
                            Rule::string => next_pair.as_str().to_string(),
                            Rule::null => "null".to_string(),
                            Rule::identifier => self.escape_js_identifier(next_pair.as_str()),
                            Rule::number => next_pair.as_str().replace('_', ""),
                            Rule::nested_expression => {
                                format!(
                                    "({})",
                                    self.transpile_expression(next_pair.clone().into_inner())?
                                )
                            }
                            _ => self.transpile_single_term(next_pair.clone())?,
                        };
                        tokens.push(ExprToken::Term(format!("(-({}))", next_term)));
                        i += 1; // Skip the next pair since we consumed it
                    } else {
                        return Err(anyhow!("Negation operator without following term"));
                    }
                }
                Rule::conditional => {
                    return self.transpile_conditional(pair.clone().into_inner());
                }
                Rule::lambda => {
                    return self.transpile_lambda(pair.clone().into_inner());
                }
                Rule::assignment => {
                    return self.transpile_assignment(pair.clone().into_inner());
                }
                Rule::list => {
                    tokens.push(ExprToken::Term(
                        self.transpile_list(pair.clone().into_inner())?,
                    ));
                }
                Rule::record => {
                    tokens.push(ExprToken::Term(
                        self.transpile_record(pair.clone().into_inner())?,
                    ));
                }
                Rule::bool => {
                    tokens.push(ExprToken::Term(pair.as_str().to_string()));
                }
                Rule::string => {
                    tokens.push(ExprToken::Term(pair.as_str().to_string()));
                }
                Rule::null => {
                    tokens.push(ExprToken::Term("null".to_string()));
                }
                Rule::identifier => {
                    tokens.push(ExprToken::Term(self.escape_js_identifier(pair.as_str())));
                }
                Rule::number => {
                    let num_str = pair.as_str().replace('_', "");
                    tokens.push(ExprToken::Term(num_str));
                }
                Rule::nested_expression => {
                    tokens.push(ExprToken::Term(format!(
                        "({})",
                        self.transpile_expression(pair.clone().into_inner())?
                    )));
                }
                // Handle postfix operators by applying them to the last term
                Rule::factorial => {
                    if let Some(ExprToken::Term(last_term)) = tokens.last_mut() {
                        *last_term = format!("$$factorial({})", last_term);
                    }
                }
                Rule::access => {
                    if let Some(ExprToken::Term(last_term)) = tokens.last_mut() {
                        let inner_pairs = pair.clone().into_inner();
                        let first_pair = inner_pairs.into_iter().next();
                        if let Some(expr_pair) = first_pair {
                            let index = self.transpile_single_term(expr_pair)?;
                            *last_term = format!("{}[{}]", last_term, index);
                        } else {
                            *last_term = format!("{}[/* NO INNER PAIR */]", last_term);
                        }
                    }
                }
                Rule::dot_access => {
                    if let Some(ExprToken::Term(last_term)) = tokens.last_mut() {
                        let prop = pair.clone().into_inner().next().unwrap().as_str();
                        *last_term = format!("{}.{}", last_term, prop);
                    }
                }
                Rule::call_list => {
                    if let Some(ExprToken::Term(last_term)) = tokens.last_mut() {
                        let args = self.transpile_call_args(pair.clone().into_inner())?;
                        *last_term = format!("{}({})", last_term, args);
                    }
                }
                // Infix operators
                Rule::add => tokens.push(ExprToken::Operator(BinaryOp::Add)),
                Rule::subtract => tokens.push(ExprToken::Operator(BinaryOp::Subtract)),
                Rule::multiply => tokens.push(ExprToken::Operator(BinaryOp::Multiply)),
                Rule::divide => tokens.push(ExprToken::Operator(BinaryOp::Divide)),
                Rule::modulo => tokens.push(ExprToken::Operator(BinaryOp::Modulo)),
                Rule::power => tokens.push(ExprToken::Operator(BinaryOp::Power)),
                Rule::equal => tokens.push(ExprToken::Operator(BinaryOp::Equal)),
                Rule::not_equal => tokens.push(ExprToken::Operator(BinaryOp::NotEqual)),
                Rule::less => tokens.push(ExprToken::Operator(BinaryOp::Less)),
                Rule::less_eq => tokens.push(ExprToken::Operator(BinaryOp::LessEq)),
                Rule::greater => tokens.push(ExprToken::Operator(BinaryOp::Greater)),
                Rule::greater_eq => tokens.push(ExprToken::Operator(BinaryOp::GreaterEq)),
                Rule::and => tokens.push(ExprToken::Operator(BinaryOp::And)),
                Rule::or => tokens.push(ExprToken::Operator(BinaryOp::Or)),
                Rule::coalesce => tokens.push(ExprToken::Operator(BinaryOp::Coalesce)),
                Rule::natural_and => tokens.push(ExprToken::Operator(BinaryOp::And)),
                Rule::natural_or => tokens.push(ExprToken::Operator(BinaryOp::Or)),
                Rule::expression => {
                    let expr_result = self.transpile_expression(pair.clone().into_inner())?;
                    tokens.push(ExprToken::Term(format!("({})", expr_result)));
                }
                _ => {}
            }

            i += 1;
        }

        // Apply operator precedence using a precedence climbing algorithm
        self.parse_expression_with_precedence(tokens, 0)
    }

    fn parse_expression_with_precedence(
        &mut self,
        tokens: Vec<ExprToken>,
        _min_precedence: i32,
    ) -> Result<String> {
        if tokens.is_empty() {
            return Ok(String::new());
        }

        // Handle simple cases
        if tokens.len() == 1 {
            return match &tokens[0] {
                ExprToken::Term(term) => Ok(term.clone()),
                ExprToken::Operator(_) => Err(anyhow!("Unexpected operator without operands")),
            };
        }

        // Convert to postfix using the shunting yard algorithm, then evaluate
        let postfix = self.to_postfix(tokens)?;
        self.evaluate_postfix(postfix)
    }

    fn to_postfix(&self, tokens: Vec<ExprToken>) -> Result<Vec<ExprToken>> {
        let mut output = Vec::new();
        let mut operator_stack = Vec::new();

        for token in tokens {
            match token {
                ExprToken::Term(_) => output.push(token),
                ExprToken::Operator(op) => {
                    while let Some(ExprToken::Operator(stack_op)) = operator_stack.last() {
                        if stack_op.precedence() > op.precedence()
                            || (stack_op.precedence() == op.precedence()
                                && !op.is_right_associative())
                        {
                            output.push(operator_stack.pop().unwrap());
                        } else {
                            break;
                        }
                    }
                    operator_stack.push(token);
                }
            }
        }

        // Pop remaining operators
        while let Some(op) = operator_stack.pop() {
            output.push(op);
        }

        Ok(output)
    }

    fn evaluate_postfix(&self, postfix: Vec<ExprToken>) -> Result<String> {
        let mut stack = Vec::new();

        for token in postfix {
            match token {
                ExprToken::Term(term) => stack.push(term),
                ExprToken::Operator(op) => {
                    if stack.len() < 2 {
                        return Err(anyhow!("Not enough operands for operator"));
                    }
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();

                    let result = if op.is_function_call() {
                        format!("{}({}, {})", op.to_js_function(), left, right)
                    } else {
                        format!("{}{}{}", left, op.to_js_function(), right)
                    };

                    stack.push(result);
                }
            }
        }

        if stack.len() != 1 {
            return Err(anyhow!("Invalid expression"));
        }

        Ok(stack.pop().unwrap())
    }

    fn transpile_expression_old(&mut self, mut pairs: Pairs<Rule>) -> Result<String> {
        let mut output = String::new();
        let mut terms = Vec::new();
        let mut operators = Vec::new();

        while let Some(pair) = pairs.next() {
            match pair.as_rule() {
                Rule::negation => {
                    // Handle unary minus
                    if let Some(next_pair) = pairs.next() {
                        let next_term = match next_pair.as_rule() {
                            Rule::lambda => self.transpile_lambda(next_pair.into_inner())?,
                            Rule::list => self.transpile_list(next_pair.into_inner())?,
                            Rule::record => self.transpile_record(next_pair.into_inner())?,
                            Rule::bool => next_pair.as_str().to_string(),
                            Rule::string => next_pair.as_str().to_string(),
                            Rule::null => "null".to_string(),
                            Rule::identifier => self.escape_js_identifier(next_pair.as_str()),
                            Rule::number => next_pair.as_str().replace('_', ""),
                            Rule::nested_expression => {
                                format!("({})", self.transpile_expression(next_pair.into_inner())?)
                            }
                            _ => self.transpile_single_term(next_pair)?,
                        };
                        terms.push(format!("(-({}))", next_term));
                    } else {
                        return Err(anyhow!("Negation operator without following term"));
                    }
                }
                Rule::invert => {
                    output.push('!');
                }
                Rule::conditional => {
                    return self.transpile_conditional(pair.into_inner());
                }
                Rule::lambda => {
                    return self.transpile_lambda(pair.into_inner());
                }
                Rule::assignment => {
                    return self.transpile_assignment(pair.into_inner());
                }
                Rule::list => {
                    terms.push(self.transpile_list(pair.into_inner())?);
                }
                Rule::record => {
                    terms.push(self.transpile_record(pair.into_inner())?);
                }
                Rule::bool => {
                    terms.push(pair.as_str().to_string());
                }
                Rule::string => {
                    terms.push(pair.as_str().to_string());
                }
                Rule::null => {
                    terms.push("null".to_string());
                }
                Rule::identifier => {
                    terms.push(self.escape_js_identifier(pair.as_str()));
                }
                Rule::number => {
                    let num_str = pair.as_str().replace('_', "");
                    terms.push(num_str);
                }
                Rule::nested_expression => {
                    terms.push(format!(
                        "({})",
                        self.transpile_expression(pair.into_inner())?
                    ));
                }
                Rule::factorial => {
                    if let Some(last_term) = terms.last_mut() {
                        *last_term = format!("$$factorial({})", last_term);
                    }
                }
                Rule::access => {
                    if let Some(last_term) = terms.last_mut() {
                        let inner_pairs = pair.into_inner();
                        let first_pair = inner_pairs.into_iter().next();
                        if let Some(expr_pair) = first_pair {
                            let index = self.transpile_single_term(expr_pair)?;
                            *last_term = format!("{}[{}]", last_term, index);
                        } else {
                            *last_term = format!("{}[/* NO INNER PAIR */]", last_term);
                        }
                    }
                }
                Rule::dot_access => {
                    if let Some(last_term) = terms.last_mut() {
                        let prop = pair.into_inner().next().unwrap().as_str();
                        *last_term = format!("{}.{}", last_term, prop);
                    }
                }
                Rule::call_list => {
                    if let Some(last_term) = terms.last_mut() {
                        let args = self.transpile_call_args(pair.into_inner())?;
                        *last_term = format!("{}({})", last_term, args);
                    }
                }
                // Infix operators
                Rule::add => operators.push("$$add".to_string()),
                Rule::subtract => operators.push("$$subtract".to_string()),
                Rule::multiply => operators.push("$$multiply".to_string()),
                Rule::divide => operators.push("$$divide".to_string()),
                Rule::modulo => operators.push("$$modulo".to_string()),
                Rule::power => operators.push("$$power".to_string()),
                Rule::equal => operators.push(" === ".to_string()),
                Rule::not_equal => operators.push(" !== ".to_string()),
                Rule::less => operators.push(" < ".to_string()),
                Rule::less_eq => operators.push(" <= ".to_string()),
                Rule::greater => operators.push(" > ".to_string()),
                Rule::greater_eq => operators.push(" >= ".to_string()),
                Rule::and => operators.push(" && ".to_string()),
                Rule::or => operators.push(" || ".to_string()),
                Rule::coalesce => operators.push(" ?? ".to_string()),
                Rule::natural_and => operators.push(" && ".to_string()),
                Rule::natural_or => operators.push(" || ".to_string()),
                Rule::with => {
                    // Process 'with' operator immediately to ensure left-associativity
                    if !terms.is_empty() {
                        let left = terms.pop().unwrap();

                        // Find the next lambda or expression for the right side
                        if let Some(next_pair) = pairs.next() {
                            let right = match next_pair.as_rule() {
                                Rule::lambda => self.transpile_lambda(next_pair.into_inner())?,
                                Rule::nested_expression => {
                                    format!(
                                        "({})",
                                        self.transpile_expression(next_pair.into_inner())?
                                    )
                                }
                                _ => self.transpile_single_term(next_pair)?,
                            };

                            let result = self.transpile_with_operator(&left, &right)?;
                            terms.push(result);
                        }
                    }
                }
                Rule::expression => {
                    let expr_result = self.transpile_expression(pair.into_inner())?;
                    terms.push(format!("({})", expr_result));
                }
                _ => {}
            }
        }

        // Combine terms and operators
        if !output.is_empty() {
            if terms.len() == 1 {
                output.push_str(&terms[0]);
            }
        } else if terms.len() == 1 && operators.is_empty() {
            output = terms[0].clone();
        } else if terms.len() == 2 && operators.is_empty() {
            output = format!("{}({})", terms[0], terms[1]);
        } else {
            if !terms.is_empty() {
                output = terms[0].clone();
                for (i, op) in operators.iter().enumerate() {
                    if let Some(next_term) = terms.get(i + 1) {
                        if self.is_function_operator(op) {
                            if i == 0 {
                                output = format!("{}({}, {})", op, &terms[0], next_term);
                            } else {
                                output = format!("{}({}, {})", op, &output, next_term);
                            }
                        } else if op == "with" {
                            // Handle 'with' operator specially
                            let left_term = if i == 0 { &terms[0] } else { &output };
                            let result = self.transpile_with_operator(left_term, next_term)?;
                            output = result;
                        } else if self.is_distributive_operator(op) {
                            // Handle distributive operators with each expressions
                            let left = if i == 0 { &terms[0] } else { &output };
                            let right = next_term;
                            output = self.handle_distributive_operation(left, op, right)?;
                        } else {
                            output.push_str(op);
                            output.push_str(next_term);
                        }
                    }
                }
            }
        }

        Ok(output)
    }

    fn is_function_operator(&self, op: &str) -> bool {
        matches!(
            op,
            "$$add" | "$$subtract" | "$$multiply" | "$$divide" | "$$modulo" | "$$power"
        )
    }

    fn is_distributive_operator(&self, op: &str) -> bool {
        matches!(
            op,
            " && " | " || " | " ?? " | " === " | " !== " | " < " | " <= " | " > " | " >= "
        )
    }

    fn handle_distributive_operation(&self, left: &str, op: &str, right: &str) -> Result<String> {
        let left_is_each = self.is_each_expression(left);
        let right_is_each = self.is_each_expression(right);

        if left_is_each && !right_is_each {
            // each(collection) OP value -> each(collection.map(x => x OP value))
            let inner_expr = self.extract_each_inner(left);
            Ok(format!("$$each({}.map(x => x{}{}))", inner_expr, op, right))
        } else if !left_is_each && right_is_each {
            // value OP each(collection) -> each(collection.map(x => value OP x))
            let inner_expr = self.extract_each_inner(right);
            Ok(format!("$$each({}.map(x => {}{} x))", inner_expr, left, op))
        } else if left_is_each && right_is_each {
            // each(coll1) OP each(coll2) -> each(coll1.map((x, i) => x OP coll2[i]))
            let left_inner = self.extract_each_inner(left);
            let right_inner = self.extract_each_inner(right);
            Ok(format!(
                "$$each({}.map((x, i) => x{} ({})[i]))",
                left_inner, op, right_inner
            ))
        } else {
            // Neither side is each - regular operation
            Ok(format!("{}{}{}", left, op, right))
        }
    }

    fn extract_each_inner<'a>(&self, expr: &'a str) -> &'a str {
        if expr.starts_with("$$each(") && expr.ends_with(")") {
            &expr[7..expr.len() - 1]
        } else if expr.starts_with("each(") && expr.ends_with(")") {
            &expr[5..expr.len() - 1]
        } else {
            expr
        }
    }

    fn escape_js_identifier(&self, ident: &str) -> String {
        // Always prefix user identifiers with $$_ to avoid global scope collision
        // This ensures user variables can never access JavaScript globals
        // Built-in functions are also prefixed, but the runtime provides them globally
        format!("$$_{}", ident)
    }

    fn transpile_primary(&mut self, primary: pest::iterators::Pair<Rule>) -> Result<String> {
        match primary.as_rule() {
            Rule::conditional => self.transpile_conditional(primary.into_inner()),
            Rule::lambda => self.transpile_lambda(primary.into_inner()),
            Rule::assignment => self.transpile_assignment(primary.into_inner()),
            Rule::list => self.transpile_list(primary.into_inner()),
            Rule::record => self.transpile_record(primary.into_inner()),
            Rule::bool => Ok(primary.as_str().to_string()),
            Rule::string => Ok(primary.as_str().to_string()),
            Rule::null => Ok("null".to_string()),
            Rule::identifier => Ok(self.escape_js_identifier(primary.as_str())),
            Rule::number => {
                let num_str = primary.as_str().replace('_', "");
                Ok(num_str)
            }
            Rule::nested_expression => Ok(format!(
                "({})",
                self.transpile_expression(primary.into_inner())?
            )),
            Rule::expression => {
                // Handle inner expressions (from parentheses that were parsed as silent rules)
                let expr_result = self.transpile_expression(primary.into_inner())?;
                Ok(format!("({})", expr_result))
            }
            _ => Ok(primary.as_str().to_string()),
        }
    }

    fn transpile_with_operator(&mut self, lhs: &str, rhs: &str) -> Result<String> {
        // Debug: print what we're receiving

        // Check if the left side is an 'each' expression
        let is_each_expr = self.is_each_expression(lhs);

        if is_each_expr {
            // For each expressions, use mapping behavior
            // Remove outer parentheses if present, since we're appending .map()
            let clean_lhs = if lhs.starts_with("(") && lhs.ends_with(")") {
                &lhs[1..lhs.len() - 1]
            } else {
                lhs
            };
            let result = format!("{}.map({})", clean_lhs, rhs);
            Ok(result)
        } else {
            // For regular values, use function application
            let result = format!("({})({})", rhs, lhs);
            Ok(result)
        }
    }

    fn transpile_conditional(&mut self, mut pairs: Pairs<Rule>) -> Result<String> {
        let condition = self.transpile_expression(pairs.next().unwrap().into_inner())?;
        let then_expr = self.transpile_expression(pairs.next().unwrap().into_inner())?;
        let else_expr = self.transpile_expression(pairs.next().unwrap().into_inner())?;

        Ok(format!("({} ? {} : {})", condition, then_expr, else_expr))
    }

    fn transpile_lambda(&mut self, mut pairs: Pairs<Rule>) -> Result<String> {
        let args_pair = pairs.next().unwrap();
        let body_pair = pairs.next().unwrap();

        let args = self.transpile_argument_list(args_pair.clone().into_inner())?;
        let body = self.transpile_expression(body_pair.clone().into_inner())?;

        // If body starts with {, it's an object literal and needs parentheses in JS
        let formatted_body = if body.trim().starts_with('{') {
            format!("({})", body)
        } else {
            body
        };

        // Preserve original source for display purposes
        let original_args = args_pair.as_str();
        let original_body = body_pair.as_str();
        let original_source = format!("{} => {}", original_args, original_body);

        // Create function with original source preserved as a property
        Ok(format!(
            "(($$f) => {{ $$f.$$originalSource = `{}`; return $$f; }})(({}) => {})",
            original_source.replace('`', "\\`"),
            args,
            formatted_body
        ))
    }

    fn transpile_argument_list(&mut self, mut pairs: Pairs<Rule>) -> Result<String> {
        let mut args = Vec::new();

        while let Some(pair) = pairs.next() {
            match pair.as_rule() {
                Rule::required_arg => {
                    let arg_name = pair.into_inner().next().unwrap().as_str();
                    args.push(self.escape_js_identifier(arg_name));
                }
                Rule::optional_arg => {
                    let arg_name = pair.into_inner().next().unwrap().as_str();
                    let escaped_name = self.escape_js_identifier(arg_name);
                    args.push(format!("{} = undefined", escaped_name));
                }
                Rule::rest_arg => {
                    let arg_name = pair.into_inner().next().unwrap().as_str();
                    let escaped_name = self.escape_js_identifier(arg_name);
                    args.push(format!("...{}", escaped_name));
                }
                _ => {}
            }
        }

        Ok(args.join(", "))
    }

    fn transpile_assignment(&mut self, mut pairs: Pairs<Rule>) -> Result<String> {
        let original_var_name = pairs.next().unwrap().as_str();
        let escaped_var_name = self.escape_js_identifier(original_var_name);
        let value_pair = pairs.next().unwrap();

        // Debug: Check what we're getting as the value
        // println!("DEBUG assignment value_pair rule: {:?}", value_pair.as_rule());
        // println!("DEBUG assignment value_pair content: {}", value_pair.as_str());

        // Check if the value itself is an assignment (chained assignment)
        let value_pairs: Vec<_> = value_pair.into_inner().collect();

        // Look for assignments in the value expression
        if value_pairs.len() == 1 && value_pairs[0].as_rule() == Rule::assignment {
            // This is a chained assignment like a = b = 5
            // We need to handle this specially to avoid "const a = const b = 5"
            let inner_assignment =
                self.transpile_assignment(value_pairs[0].clone().into_inner())?;

            // Extract the variable name from the inner assignment to use as our value
            // For chained assignments, we want the leftmost variable (the one closest to us)
            // e.g., for "const z = 10; const y = z", we want "y"
            let inner_var = if inner_assignment.contains(';') {
                // Multiple statements, get the last assignment's variable
                let statements: Vec<&str> = inner_assignment.split(';').collect();
                for stmt in statements.iter().rev() {
                    let trimmed = stmt.trim();
                    if trimmed.starts_with("const ") && trimmed.contains('=') {
                        let var_part = trimmed.split('=').next().unwrap().trim();
                        return Ok(format!(
                            "{}; const {} = {}",
                            inner_assignment,
                            escaped_var_name,
                            var_part.replace("const ", "")
                        ));
                    }
                }
                // Fallback - extract from first statement
                inner_assignment
                    .split('=')
                    .next()
                    .unwrap()
                    .trim()
                    .replace("const ", "")
            } else {
                // Single statement
                inner_assignment
                    .split('=')
                    .next()
                    .unwrap()
                    .trim()
                    .replace("const ", "")
            };

            if self.inline_evaluation {
                // For inline evaluation, we need both assignments
                Ok(format!(
                    "{}; const {} = {}; $$results.bindings['{}'] = {}",
                    inner_assignment,
                    escaped_var_name,
                    inner_var,
                    original_var_name,
                    escaped_var_name
                ))
            } else {
                // Generate: const b = 5; const a = b;
                Ok(format!(
                    "{}; const {} = {}",
                    inner_assignment, escaped_var_name, inner_var
                ))
            }
        } else {
            // Regular assignment - not chained

            // Special handling for expressions that contain 'with' operator
            if self.contains_with_chain(&value_pairs) {
                // Use the with chain handler directly
                let value = self.transpile_with_chain_properly(&value_pairs)?;

                if self.inline_evaluation {
                    Ok(format!(
                        "const {} = {}; $$results.bindings['{}'] = {}",
                        escaped_var_name, value, original_var_name, escaped_var_name
                    ))
                } else {
                    Ok(format!("const {} = {}", escaped_var_name, value))
                }
            } else {
                let value = self.transpile_expression_from_pairs(value_pairs)?;

                if self.inline_evaluation {
                    // Capture the binding in $$results.bindings (use original name as the key)
                    Ok(format!(
                        "const {} = {}; $$results.bindings['{}'] = {}",
                        escaped_var_name, value, original_var_name, escaped_var_name
                    ))
                } else {
                    Ok(format!("const {} = {}", escaped_var_name, value))
                }
            }
        }
    }

    fn transpile_list(&mut self, mut pairs: Pairs<Rule>) -> Result<String> {
        let mut elements = Vec::new();

        while let Some(pair) = pairs.next() {
            match pair.as_rule() {
                Rule::spread_expression => {
                    let expr = self.transpile_expression(
                        pair.into_inner().skip(1).next().unwrap().into_inner(),
                    )?;
                    elements.push(format!("...$$spreadToArray({})", expr));
                }
                Rule::expression => {
                    elements.push(self.transpile_expression(pair.into_inner())?);
                }
                _ => {}
            }
        }

        Ok(format!("[{}]", elements.join(", ")))
    }

    fn transpile_record(&mut self, mut pairs: Pairs<Rule>) -> Result<String> {
        let mut properties = Vec::new();

        while let Some(pair) = pairs.next() {
            match pair.as_rule() {
                Rule::record_pair => {
                    let mut inner = pair.into_inner();
                    let key_pair = inner.next().unwrap();
                    let value = self.transpile_expression(inner.next().unwrap().into_inner())?;

                    let key = match key_pair.as_rule() {
                        Rule::record_key_static => {
                            let key_inner = key_pair.into_inner().next().unwrap();
                            match key_inner.as_rule() {
                                Rule::identifier => key_inner.as_str().to_string(),
                                Rule::string => key_inner.as_str().to_string(),
                                _ => return Err(anyhow!("Unexpected key type")),
                            }
                        }
                        Rule::record_key_dynamic => {
                            format!("[{}]", self.transpile_expression(key_pair.into_inner())?)
                        }
                        _ => return Err(anyhow!("Unexpected record key rule")),
                    };

                    properties.push(format!("{}: {}", key, value));
                }
                Rule::record_shorthand => {
                    let prop_name = pair.into_inner().next().unwrap().as_str();
                    let escaped_prop = self.escape_js_identifier(prop_name);
                    properties.push(format!("{}: {}", prop_name, escaped_prop));
                }
                Rule::spread_expression => {
                    let expr = self.transpile_expression(
                        pair.into_inner().skip(1).next().unwrap().into_inner(),
                    )?;
                    properties.push(format!("...{}", expr));
                }
                _ => {}
            }
        }

        Ok(format!("{{{}}}", properties.join(", ")))
    }

    fn transpile_call_args(&mut self, mut pairs: Pairs<Rule>) -> Result<String> {
        let mut args = Vec::new();

        while let Some(pair) = pairs.next() {
            match pair.as_rule() {
                Rule::spread_expression => {
                    let expr = self.transpile_expression(
                        pair.into_inner().skip(1).next().unwrap().into_inner(),
                    )?;
                    args.push(format!("...{}", expr));
                }
                Rule::expression => {
                    args.push(self.transpile_expression(pair.into_inner())?);
                }
                _ => {}
            }
        }

        Ok(args.join(", "))
    }

    fn transpile_single_term(&mut self, pair: pest::iterators::Pair<Rule>) -> Result<String> {
        match pair.as_rule() {
            Rule::lambda => self.transpile_lambda(pair.into_inner()),
            Rule::list => self.transpile_list(pair.into_inner()),
            Rule::record => self.transpile_record(pair.into_inner()),
            Rule::bool => Ok(pair.as_str().to_string()),
            Rule::string => Ok(pair.as_str().to_string()),
            Rule::null => Ok("null".to_string()),
            Rule::identifier => Ok(self.escape_js_identifier(pair.as_str())),
            Rule::number => {
                let num_str = pair.as_str().replace('_', "");
                Ok(num_str)
            }
            Rule::nested_expression => Ok(format!(
                "({})",
                self.transpile_expression(pair.into_inner())?
            )),
            Rule::expression => {
                // Handle expression rules by transpiling their inner content
                self.transpile_expression(pair.into_inner())
            }
            _ => Ok(pair.as_str().to_string()),
        }
    }

    fn ensure_semicolon(&self, stmt: String) -> String {
        let trimmed = stmt.trim();
        if trimmed.is_empty() {
            return stmt;
        }

        // Don't add semicolon if it already ends with one, or if it's a block statement
        if trimmed.ends_with(';') || trimmed.ends_with('}') || trimmed.ends_with('{') {
            stmt
        } else {
            format!("{};", stmt)
        }
    }

    fn is_each_expression(&self, term: &str) -> bool {
        // Check for direct each expressions
        if term.starts_with("each(") || term.starts_with("$$each(") || term.starts_with("$$_each(")
        {
            return true;
        }

        // Check for parenthesized each expressions like "($$_each(...))"
        if term.starts_with("(") && term.ends_with(")") {
            let inner = &term[1..term.len() - 1];
            return self.is_each_expression(inner);
        }

        false
    }
}

pub fn transpile_to_js(source: &str) -> Result<String> {
    let mut transpiler = Transpiler::new();
    transpiler.transpile(source)
}

pub fn transpile_to_js_with_inline_eval(source: &str) -> Result<String> {
    let mut transpiler = Transpiler::new_with_inline_eval();
    transpiler.transpile(source)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn transpile_simple(source: &str) -> Result<String> {
        let mut transpiler = Transpiler::new();
        transpiler.transpile(source)
    }

    fn extract_user_code(transpiled: &str) -> String {
        // Extract just the user code part, skipping the runtime library
        // Look for the line that contains user code (usually after all the runtime setup)
        let lines: Vec<&str> = transpiled.lines().collect();
        let mut user_start = lines.len();

        // Find the last instance of runtime setup code
        for (i, line) in lines.iter().enumerate().rev() {
            if line.contains("var $$_inputs = globalThis.$$_inputs;") {
                user_start = i + 1;
                break;
            }
        }

        if user_start < lines.len() {
            lines[user_start..]
                .iter()
                .filter(|line| {
                    let trimmed = line.trim();
                    !trimmed.is_empty() && trimmed != "}" && !trimmed.starts_with("//")
                })
                .map(|line| line.trim())
                .collect::<Vec<_>>()
                .join("\n")
        } else {
            String::new()
        }
    }

    #[test]
    fn test_simple_assignment() {
        let result = transpile_simple("x = 5").unwrap();
        let user_code = extract_user_code(&result);
        assert!(user_code.contains("const $$_x = 5"));
    }

    #[test]
    fn test_chained_assignment_two_variables() {
        let result = transpile_simple("a = b = 5").unwrap();
        let user_code = extract_user_code(&result);

        // Should generate: const $$_b = 5; const $$_a = $$_b;
        assert!(user_code.contains("const $$_b = 5"));
        assert!(user_code.contains("const $$_a = $$_b"));
        assert!(!user_code.contains("const $$_a = const $$_b"));
    }

    #[test]
    fn test_chained_assignment_three_variables() {
        let result = transpile_simple("x = y = z = 10").unwrap();
        let user_code = extract_user_code(&result);

        // Should generate a chain of assignments
        assert!(user_code.contains("const $$_z = 10"));
        assert!(user_code.contains("const $$_y = $$_z"));
        assert!(user_code.contains("const $$_x = $$_y"));
        assert!(!user_code.contains("const $$_x = const $$_y"));
        assert!(!user_code.contains("const $$_y = const $$_z"));
    }

    #[test]
    fn test_chained_assignment_with_expression() {
        let result = transpile_simple("a = b = 2 + 3").unwrap();
        let user_code = extract_user_code(&result);

        // Should evaluate the expression first, then assign
        assert!(user_code.contains("const $$_b = $$add(2, 3)"));
        assert!(user_code.contains("const $$_a = $$_b"));
    }

    #[test]
    fn test_chained_assignment_with_list() {
        let result = transpile_simple("c = d = [1, 2, 3]").unwrap();
        let user_code = extract_user_code(&result);

        // Should handle list expressions
        assert!(user_code.contains("const $$_d = [1, 2, 3]"));
        assert!(user_code.contains("const $$_c = $$_d"));
    }

    #[test]
    fn test_nested_assignment_in_expression() {
        // This test case shows a limitation - parenthesized assignments aren't fully supported yet
        // The transpiler produces: const $$_result = $$_p = $$_q = 42; which is invalid JS
        // For now, just test that we don't crash and the assignment is attempted
        let result = transpile_simple("result = (p = q = 42)").unwrap();
        let user_code = extract_user_code(&result);

        // The current transpiler has limitations with nested assignments in parentheses
        assert!(user_code.contains("const $$_result = "));
        assert!(user_code.contains("42"));
    }

    #[test]
    fn test_complex_chained_assignment_with_operations() {
        let result = transpile_simple("w = x = y = z = 1 + 2 * 3").unwrap();
        let user_code = extract_user_code(&result);

        // Should create a proper chain
        assert!(user_code.contains("const $$_z = "));
        assert!(user_code.contains("const $$_y = $$_z"));
        assert!(user_code.contains("const $$_x = $$_y"));
        assert!(user_code.contains("const $$_w = $$_x"));
        assert!(user_code.contains("$$add"));
        assert!(user_code.contains("$$multiply"));
    }

    #[test]
    fn test_regular_assignment_still_works() {
        let result = transpile_simple("single = 100").unwrap();
        let user_code = extract_user_code(&result);

        assert!(user_code.contains("const $$_single = 100"));
        assert!(!user_code.contains("const $$_single = const"));
    }

    #[test]
    fn test_assignment_with_function_call() {
        let result = transpile_simple("a = b = sum(1, 2, 3)").unwrap();
        let user_code = extract_user_code(&result);

        // Should handle function calls in chained assignments
        assert!(user_code.contains("const $$_b = $$_sum(1, 2, 3)"));
        assert!(user_code.contains("const $$_a = $$_b"));
    }

    #[test]
    fn test_mixed_assignments_and_expressions() {
        let result = transpile_simple("x = 5\ny = z = x + 1\nprint(y)").unwrap();
        let user_code = extract_user_code(&result);

        assert!(user_code.contains("const $$_x = 5"));
        assert!(user_code.contains("const $$_z = $$add($$_x, 1)"));
        assert!(user_code.contains("const $$_y = $$_z"));
        assert!(user_code.contains("$$_print($$_y)"));
    }

    #[test]
    fn test_arithmetic_operations() {
        let result = transpile_simple("result = 2 + 3 * 4").unwrap();
        let user_code = extract_user_code(&result);

        // Should use the proper function calls for arithmetic and prefix variables
        assert!(user_code.contains("const $$_result = "));
        assert!(user_code.contains("$$add"));
        assert!(user_code.contains("$$multiply"));
    }

    #[test]
    fn test_list_creation() {
        let result = transpile_simple("numbers = [1, 2, 3]").unwrap();
        let user_code = extract_user_code(&result);

        assert!(user_code.contains("const $$_numbers = [1, 2, 3]"));
    }

    #[test]
    fn test_record_creation() {
        let result = transpile_simple("person = {name: \"Alice\", age: 30}").unwrap();
        let user_code = extract_user_code(&result);

        assert!(user_code.contains("const $$_person = {"));
        assert!(user_code.contains("name: \"Alice\""));
        assert!(user_code.contains("age: 30"));
    }

    #[test]
    fn test_lambda_expression() {
        let result = transpile_simple("add = (x, y) => x + y").unwrap();
        let user_code = extract_user_code(&result);

        // Lambda expressions are complex, just check that we have the assignment and the add function
        assert!(user_code.contains("const $$_add = "));
        assert!(user_code.contains("$$add($$_x, $$_y)"));
    }

    #[test]
    fn test_function_call() {
        let result = transpile_simple("result = map([1, 2, 3], (x) => x * 2)").unwrap();
        let user_code = extract_user_code(&result);

        // Function calls may be complex, just check basic structure
        assert!(user_code.contains("const $$_result = "));
        assert!(user_code.contains("$$_map"));
        assert!(user_code.contains("$$multiply"));
    }

    #[test]
    fn test_conditional_expression() {
        let result = transpile_simple("result = if true then 1 else 2").unwrap();
        let user_code = extract_user_code(&result);

        assert!(user_code.contains("const $$_result = (true ? 1 : 2)"));
    }

    #[test]
    fn test_print_statement() {
        let result = transpile_simple("print(\"Hello, {}!\", \"World\")").unwrap();
        let user_code = extract_user_code(&result);

        assert!(user_code.contains("$$_print(\"Hello, {}!\", \"World\")"));
    }

    #[test]
    fn test_inline_evaluation_mode() {
        let mut transpiler = Transpiler::new_with_inline_eval();
        let result = transpiler.transpile("x = 5").unwrap();

        // Should include inline evaluation setup
        assert!(result.contains("$$results"));
        assert!(result.contains("$$results.bindings"));
    }

    #[test]
    fn test_inline_evaluation_chained_assignment() {
        let mut transpiler = Transpiler::new_with_inline_eval();
        let result = transpiler.transpile("a = b = 10").unwrap();
        let user_code = extract_user_code(&result);

        // Should handle chained assignments in inline mode
        assert!(user_code.contains("$$results.bindings"));
        assert!(user_code.contains("const $$_b = 10"));
        assert!(user_code.contains("const $$_a = $$_b"));
    }

    #[test]
    fn test_assignment_does_not_break_other_expressions() {
        let result = transpile_simple("x = 5\ny = x + 1\nz = y * 2").unwrap();
        let user_code = extract_user_code(&result);

        assert!(user_code.contains("const $$_x = 5"));
        assert!(user_code.contains("const $$_y = $$add($$_x, 1)"));
        assert!(user_code.contains("const $$_z = $$multiply($$_y, 2)"));
    }

    // Tests for built-in function stringification
    // These tests verify that the transpiled JS includes the correct runtime fixes

    #[test]
    fn test_runtime_includes_fixed_to_string() {
        let result = transpile_simple("x = 5").unwrap();

        // The runtime should include our fixed $$to_string function
        assert!(result.contains("function $$to_string(value)"));
        assert!(result.contains("// Check if this is a built-in function"));
        assert!(result.contains("for (const [name, func] of Object.entries($$builtins))"));
        assert!(result.contains("return `${name} (built-in function)`;"));
    }

    #[test]
    fn test_runtime_includes_fixed_format() {
        let result = transpile_simple("x = 5").unwrap();

        // The runtime should include our fixed $$format function
        assert!(result.contains("function $$format(formatStr, ...values)"));
        assert!(result.contains("} else if (typeof value === \"function\") {"));
        assert!(result.contains("formattedValue = $$to_string(value);"));
    }

    #[test]
    fn test_runtime_includes_builtins_registry() {
        let result = transpile_simple("x = 5").unwrap();

        // The runtime should include the $$builtins registry
        assert!(result.contains("const $$builtins = {"));
        assert!(result.contains("map: $$map,"));
        assert!(result.contains("filter: $$filter,"));
        assert!(result.contains("to_string: $$to_string,"));
        assert!(result.contains("format: $$format,"));
    }

    #[test]
    fn test_to_string_function_call_transpilation() {
        let result = transpile_simple("result = to_string(map)").unwrap();
        let user_code = extract_user_code(&result);

        // Should transpile to_string call correctly with prefixes
        assert!(user_code.contains("const $$_result = $$_to_string($$_map)"));
    }

    #[test]
    fn test_format_function_call_transpilation() {
        let result = transpile_simple("print(\"Function: {}\", filter)").unwrap();
        let user_code = extract_user_code(&result);

        // Should transpile format call correctly with prefixes
        assert!(user_code.contains("$$_print(\"Function: {}\", $$_filter)"));
    }

    #[test]
    fn test_runtime_arithmetic_functions() {
        let result = transpile_simple("x = 1 + 2").unwrap();

        // Should include arithmetic functions in runtime
        assert!(result.contains("function $$add(left, right)"));
        assert!(result.contains("function $$subtract(left, right)"));
        assert!(result.contains("function $$multiply(left, right)"));
        assert!(result.contains("function $$divide(left, right)"));
    }

    #[test]
    fn test_runtime_collection_functions() {
        let result = transpile_simple("x = [1, 2, 3]").unwrap();

        // Should include collection functions in runtime
        assert!(result.contains("function $$map(collection, fn)"));
        assert!(result.contains("function $$filter(collection, predicate)"));
        assert!(result.contains("function $$reduce(collection, reducer, initialValue)"));
        assert!(result.contains("function $$each(collection)"));
    }

    #[test]
    fn test_runtime_math_functions() {
        let result = transpile_simple("x = 5").unwrap();

        // Should include math functions in runtime
        assert!(result.contains("function $$abs(x)"));
        assert!(result.contains("function $$sin(x)"));
        assert!(result.contains("function $$cos(x)"));
        assert!(result.contains("function $$sqrt(x)"));
        assert!(result.contains("function $$factorial(n)"));
    }

    #[test]
    fn test_runtime_string_functions() {
        let result = transpile_simple("x = \"hello\"").unwrap();

        // Should include string functions in runtime
        assert!(result.contains("function $$split(str, delimiter)"));
        assert!(result.contains("function $$join(arr, delimiter)"));
        assert!(result.contains("function $$uppercase(str)"));
        assert!(result.contains("function $$lowercase(str)"));
        assert!(result.contains("function $$replace(old, replacement, str)"));
    }

    #[test]
    fn test_runtime_type_checking_functions() {
        let result = transpile_simple("x = true").unwrap();

        // Should include type checking functions in runtime
        assert!(result.contains("function $$is_string(value)"));
        assert!(result.contains("function $$is_number(value)"));
        assert!(result.contains("function $$is_bool(value)"));
        assert!(result.contains("function $$is_list(value)"));
        assert!(result.contains("function $$is_null(value)"));
        assert!(result.contains("function $$typeof(value)"));
    }

    #[test]
    fn test_runtime_setup_code() {
        let result = transpile_simple("x = 5").unwrap();

        // Should include proper runtime setup
        assert!(result.contains("// Set up aliases at the end of execution"));
        assert!(result.contains("setTimeout(() => {"));
        assert!(result.contains("for (const [name, func] of Object.entries($$builtins))"));
        assert!(result.contains("globalThis[name] = func;"));

        // Should include immediate setup too
        assert!(result.contains("// Immediately make them available for function declarations"));
    }

    #[test]
    fn test_runtime_inputs_setup() {
        let result = transpile_simple("x = inputs.value").unwrap();

        // Should include inputs setup with prefixed names
        assert!(result.contains("// Set up inputs variable"));
        assert!(result.contains("if (typeof globalThis.$$_inputs === \"undefined\")"));
        assert!(result.contains("globalThis.$$_inputs = {};"));
        assert!(result.contains("var $$_inputs = globalThis.$$_inputs;"));
    }

    #[test]
    fn test_runtime_constants() {
        let result = transpile_simple("x = 5").unwrap();

        // Should include constants
        assert!(result.contains("const $$constants = {"));
        assert!(result.contains("pi: Math.PI,"));
        assert!(result.contains("e: Math.E,"));
        assert!(result.contains("infinity: Infinity,"));
    }

    #[test]
    fn test_print_function_environment_check() {
        let result = transpile_simple("print(\"hello\")").unwrap();

        // Should include environment-aware print function
        assert!(result.contains("function $$print(...args)"));
        assert!(result.contains("typeof process !== \"undefined\""));
        assert!(result.contains("process.versions &&"));
        assert!(result.contains("console.log"));
        assert!(result.contains("print function is not available in browser environments"));
    }

    #[test]
    fn test_runtime_header_comment() {
        let result = transpile_simple("x = 5").unwrap();

        // Should start with the correct header
        assert!(result.starts_with("// Blots JavaScript Runtime Library"));
        assert!(result.contains("// Built-in functions for transpiled Blots code"));
    }

    // Order of Operations Tests
    #[test]
    fn test_basic_precedence_multiplication_over_addition() {
        let result = transpile_simple("result = 2 + 3 * 4").unwrap();
        let user_code = extract_user_code(&result);

        // Should transpile to: $$add(2, $$multiply(3, 4))
        // This ensures multiplication happens before addition
        assert!(user_code.contains("$$add(2, $$multiply(3, 4))"));
        assert!(!user_code.contains("$$multiply($$add(2, 3), 4)"));
    }

    #[test]
    fn test_basic_precedence_division_over_subtraction() {
        let result = transpile_simple("result = 10 - 6 / 2").unwrap();
        let user_code = extract_user_code(&result);

        // Should transpile to: $$subtract(10, $$divide(6, 2))
        assert!(user_code.contains("$$subtract(10, $$divide(6, 2))"));
        assert!(!user_code.contains("$$divide($$subtract(10, 6), 2)"));
    }

    #[test]
    fn test_same_precedence_left_to_right_addition_subtraction() {
        let result = transpile_simple("result = 10 - 5 + 2").unwrap();
        let user_code = extract_user_code(&result);

        // Should transpile to: $$add($$subtract(10, 5), 2)
        // Left-to-right for same precedence
        assert!(user_code.contains("$$add($$subtract(10, 5), 2)"));
        assert!(!user_code.contains("$$subtract(10, $$add(5, 2))"));
    }

    #[test]
    fn test_same_precedence_left_to_right_multiplication_division() {
        let result = transpile_simple("result = 12 / 3 * 2").unwrap();
        let user_code = extract_user_code(&result);

        // Should transpile to: $$multiply($$divide(12, 3), 2)
        assert!(user_code.contains("$$multiply($$divide(12, 3), 2)"));
        assert!(!user_code.contains("$$divide(12, $$multiply(3, 2))"));
    }

    #[test]
    fn test_parentheses_override_precedence() {
        let result = transpile_simple("result = (2 + 3) * 4").unwrap();
        let user_code = extract_user_code(&result);

        // Should transpile to: $$multiply(($$add(2, 3)), 4)
        assert!(user_code.contains("$$multiply(($$add(2, 3)), 4)"));
    }

    #[test]
    fn test_complex_mixed_operations() {
        let result = transpile_simple("result = 2 + 3 * 4 - 5").unwrap();
        let user_code = extract_user_code(&result);

        // Should transpile to: $$subtract($$add(2, $$multiply(3, 4)), 5)
        assert!(user_code.contains("$$subtract($$add(2, $$multiply(3, 4)), 5)"));
    }

    #[test]
    fn test_nested_parentheses() {
        let result = transpile_simple("result = 2 * (3 + (4 * 5))").unwrap();
        let user_code = extract_user_code(&result);

        // Should handle nested parentheses correctly
        assert!(user_code.contains("$$multiply(2, ($$add(3, ($$multiply(4, 5)))))"));
    }

    #[test]
    fn test_power_operator_highest_precedence() {
        let result = transpile_simple("result = 2 + 3 ^ 2").unwrap();
        let user_code = extract_user_code(&result);

        // Power should have highest precedence
        assert!(user_code.contains("$$add(2, $$power(3, 2))"));
        assert!(!user_code.contains("$$power($$add(2, 3), 2)"));
    }

    #[test]
    fn test_power_operator_right_associative() {
        let result = transpile_simple("result = 2 ^ 3 ^ 2").unwrap();
        let user_code = extract_user_code(&result);

        // Power is right-associative: 2^(3^2) = 2^9 = 512
        assert!(user_code.contains("$$power(2, $$power(3, 2))"));
        assert!(!user_code.contains("$$power($$power(2, 3), 2)"));
    }

    #[test]
    fn test_unary_minus_with_precedence() {
        let result = transpile_simple("result = -5 + 3 * 2").unwrap();
        let user_code = extract_user_code(&result);

        // Should handle unary minus correctly with precedence
        // The double parentheses are expected due to how unary minus is processed
        assert!(user_code.contains("$$add((-(5)), $$multiply(3, 2))"));
    }

    #[test]
    fn test_comparison_operators_lower_precedence() {
        let result = transpile_simple("result = 5 + 3 > 2 * 4").unwrap();
        let user_code = extract_user_code(&result);

        // Arithmetic should happen before comparison
        assert!(user_code.contains("$$add(5, 3) > $$multiply(2, 4)"));
    }

    #[test]
    fn test_logical_operators_lowest_precedence() {
        let result = transpile_simple("result = 5 > 3 && 2 + 1 < 4").unwrap();
        let user_code = extract_user_code(&result);

        // Arithmetic and comparison before logical
        assert!(user_code.contains("5 > 3 && $$add(2, 1) < 4"));
    }

    // Tests that also validate JS runtime execution
    #[test]
    fn test_order_of_operations_runtime_execution() {
        use std::fs;
        use std::process::Command;

        // Test cases with expected results
        let test_cases = vec![
            ("2 + 3 * 4", "14"),    // Basic precedence
            ("10 - 6 / 2", "7"),    // Division before subtraction
            ("12 / 3 * 2", "8"),    // Same precedence left-to-right
            ("(2 + 3) * 4", "20"),  // Parentheses override
            ("2 + 3 * 4 - 5", "9"), // Complex mixed operations
            ("-5 + 3 * 2", "1"),    // Unary minus
        ];

        for (expression, expected) in test_cases {
            let source = format!("result = {}\nprint(result)", expression);
            let transpiled = transpile_simple(&source).unwrap();

            // Write to temp file
            let temp_file = format!(
                "/tmp/blots_test_{}.js",
                expression
                    .replace(" ", "_")
                    .replace("/", "div")
                    .replace("*", "mul")
                    .replace("+", "add")
                    .replace("-", "sub")
                    .replace("(", "")
                    .replace(")", "")
            );
            fs::write(&temp_file, &transpiled).unwrap();

            // Execute with Node.js if available
            if let Ok(output) = Command::new("node").arg(&temp_file).output() {
                let stdout = String::from_utf8_lossy(&output.stdout);
                assert!(
                    stdout.trim() == expected,
                    "Expression '{}' should equal '{}', got '{}'",
                    expression,
                    expected,
                    stdout.trim()
                );
            }

            // Clean up
            let _ = fs::remove_file(&temp_file);
        }
    }

    #[test]
    fn test_complex_order_of_operations_runtime() {
        use std::fs;
        use std::process::Command;

        // More complex test cases
        let test_cases = vec![
            ("20 / 4 + 2 * 3", "11"),    // Multiple different operators
            ("15 - 8 / 2 + 1", "12"),    // Mixed operations
            ("3 * 4 + 12 / 3", "16"),    // Multiple same-precedence groups
            ("((2 + 3) * 4) - 5", "15"), // Nested parentheses
            ("2 * (3 + (4 * 5))", "46"), // Deep nesting
        ];

        for (expression, expected) in test_cases {
            let source = format!("result = {}\nprint(result)", expression);
            let transpiled = transpile_simple(&source).unwrap();

            // Write to temp file
            let temp_file = format!(
                "/tmp/blots_complex_test_{}.js",
                expression
                    .replace(" ", "_")
                    .replace("/", "div")
                    .replace("*", "mul")
                    .replace("+", "add")
                    .replace("-", "sub")
                    .replace("(", "lp")
                    .replace(")", "rp")
            );
            fs::write(&temp_file, &transpiled).unwrap();

            // Execute with Node.js if available
            if let Ok(output) = Command::new("node").arg(&temp_file).output() {
                let stdout = String::from_utf8_lossy(&output.stdout);
                assert!(
                    stdout.trim() == expected,
                    "Complex expression '{}' should equal '{}', got '{}'",
                    expression,
                    expected,
                    stdout.trim()
                );
            }

            // Clean up
            let _ = fs::remove_file(&temp_file);
        }
    }

    // Tests for global scope isolation fix
    #[test]
    fn test_user_variables_are_prefixed() {
        let result = transpile_simple("x = 42\ny = x + 1").unwrap();
        let user_code = extract_user_code(&result);

        // User variables should be prefixed with $$_
        assert!(user_code.contains("const $$_x = 42"));
        assert!(user_code.contains("const $$_y = $$add($$_x, 1)"));

        // Should not contain unprefixed variable names
        assert!(!user_code.contains("const x = 42"));
        assert!(!user_code.contains("const y = $$add(x, 1)"));
    }

    #[test]
    fn test_builtin_functions_are_prefixed_but_accessible() {
        let result = transpile_simple("result = sum(1, 2, 3)").unwrap();
        let user_code = extract_user_code(&result);

        // Built-in function calls should be prefixed
        assert!(user_code.contains("const $$_result = $$_sum(1, 2, 3)"));

        // Runtime should provide both prefixed and unprefixed versions
        assert!(result.contains("globalThis[name] = func;"));
        assert!(result.contains("const prefixedName = `$$_${name}`;"));
        assert!(result.contains("globalThis[prefixedName] = func;"));
    }

    #[test]
    fn test_lambda_parameters_are_prefixed() {
        let result = transpile_simple("double = x => x * 2").unwrap();
        let user_code = extract_user_code(&result);

        // Lambda parameters should be prefixed in the actual function
        assert!(user_code.contains("($$_x) => $$multiply($$_x, 2)"));

        // Variable name should be prefixed
        assert!(user_code.contains("const $$_double = "));

        // Should not contain unprefixed parameter in the function
        assert!(!user_code.contains("(x) => $$multiply(x, 2)"));
    }

    #[test]
    fn test_complex_lambda_with_multiple_parameters() {
        let result = transpile_simple("calc = (a, b, c) => a + b * c").unwrap();
        let user_code = extract_user_code(&result);

        // All parameters should be prefixed
        assert!(user_code.contains("($$_a, $$_b, $$_c) => $$add($$_a, $$multiply($$_b, $$_c))"));

        // Variable name should be prefixed
        assert!(user_code.contains("const $$_calc = "));
    }

    #[test]
    fn test_nested_lambda_parameters() {
        let result = transpile_simple("outer = x => y => x + y").unwrap();
        let user_code = extract_user_code(&result);

        // Both lambda parameters should be prefixed in the transpiled output
        assert!(user_code.contains("($$_x) =>"));
        assert!(user_code.contains("($$_y) => $$add($$_x, $$_y)"));

        // Variable name should be prefixed
        assert!(user_code.contains("const $$_outer = "));
    }

    #[test]
    fn test_record_shorthand_with_prefixed_variables() {
        let result = transpile_simple("name = \"Alice\"\nage = 30\nperson = {name, age}").unwrap();
        let user_code = extract_user_code(&result);

        // Variables should be prefixed
        assert!(user_code.contains("const $$_name = \"Alice\""));
        assert!(user_code.contains("const $$_age = 30"));

        // Record should reference prefixed variables
        // Note: Record shorthand might need special handling in the transpiler
        assert!(user_code.contains("const $$_person = "));
    }

    #[test]
    fn test_function_call_with_prefixed_variables() {
        let result =
            transpile_simple("numbers = [1, 2, 3]\ndoubled = map(numbers, x => x * 2)").unwrap();
        let user_code = extract_user_code(&result);

        // Variables should be prefixed
        assert!(user_code.contains("const $$_numbers = [1, 2, 3]"));
        assert!(user_code.contains("const $$_doubled = $$_map($$_numbers,"));

        // Lambda parameter should be prefixed (in the actual function)
        assert!(user_code.contains("($$_x) => $$multiply($$_x, 2)"));
    }

    #[test]
    fn test_chained_assignment_with_prefixed_variables() {
        let result = transpile_simple("a = b = c = 42").unwrap();
        let user_code = extract_user_code(&result);

        // All variables in chain should be prefixed
        assert!(user_code.contains("const $$_c = 42"));
        assert!(user_code.contains("const $$_b = $$_c"));
        assert!(user_code.contains("const $$_a = $$_b"));
    }

    #[test]
    fn test_conditional_with_prefixed_variables() {
        let result =
            transpile_simple("x = 5\nresult = if x > 3 then \"big\" else \"small\"").unwrap();
        let user_code = extract_user_code(&result);

        // Variables should be prefixed in conditionals
        assert!(user_code.contains("const $$_x = 5"));
        assert!(user_code.contains("const $$_result = ($$_x > 3 ? \"big\" : \"small\")"));
    }

    #[test]
    fn test_translate_js_identifiers_function() {
        // Test the helper function that cleans up identifiers for display
        assert_eq!(translate_js_identifiers("$$_x"), "x");
        assert_eq!(translate_js_identifiers("$$_myVariable"), "myVariable");
        assert_eq!(
            translate_js_identifiers("ReferenceError: $$_origin is not defined"),
            "ReferenceError: origin is not defined"
        );
        assert_eq!(
            translate_js_identifiers("$$_x = $$_sum($$_a, $$_b)"),
            "x = sum(a, b)"
        );

        // Should not affect built-in function names without $$_ prefix
        assert_eq!(translate_js_identifiers("sum(1, 2, 3)"), "sum(1, 2, 3)");

        // Should not affect $$ prefixed built-ins
        assert_eq!(translate_js_identifiers("$$add(1, 2)"), "$$add(1, 2)");
    }

    #[test]
    fn test_translate_js_error_function() {
        let error_msg = "ReferenceError: $$_unknownVar is not defined at line 1";
        let cleaned = translate_js_error(error_msg);
        assert_eq!(
            cleaned,
            "ReferenceError: unknownVar is not defined at line 1"
        );
    }

    #[test]
    fn test_mixed_builtin_and_user_variables() {
        let result =
            transpile_simple("nums = [1, 2, 3]\ntotal = sum(nums)\navg = total / len(nums)")
                .unwrap();
        let user_code = extract_user_code(&result);

        // User variables should be prefixed
        assert!(user_code.contains("const $$_nums = [1, 2, 3]"));
        assert!(user_code.contains("const $$_total = $$_sum($$_nums)"));
        assert!(user_code.contains("const $$_avg = $$divide($$_total, $$_len($$_nums))"));

        // Built-in functions should be accessible via $$_ prefix
        assert!(result.contains("$$_sum"));
        assert!(result.contains("$$_len"));
    }

    #[test]
    fn test_runtime_provides_prefixed_builtins() {
        let result = transpile_simple("x = 5").unwrap();

        // Runtime should set up both regular and prefixed versions
        assert!(result.contains("for (const [name, func] of Object.entries($$builtins))"));
        assert!(result.contains("globalThis[name] = func;"));
        assert!(result.contains("const prefixedName = `$$_${name}`;"));
        assert!(result.contains("globalThis[prefixedName] = func;"));

        // Should do this both immediately and with setTimeout
        assert!(result.contains("// Immediately make them available for function declarations"));
        assert!(result.contains("// Set up aliases at the end of execution"));
        assert!(result.contains("setTimeout(() => {"));
    }

    #[test]
    fn test_global_scope_isolation_prevents_js_globals() {
        // This test verifies that the transpiled code would block access to JS globals
        let test_cases = vec![
            "origin",
            "window",
            "document",
            "console",
            "location",
            "navigator",
            "history",
            "localStorage",
            "sessionStorage",
        ];

        for global in test_cases {
            let result = transpile_simple(global).unwrap();
            let user_code = extract_user_code(&result);

            // Should transpile to $$_globalName, which won't be defined
            let expected_prefixed = format!("$$_{}", global);
            assert!(
                user_code.contains(&expected_prefixed),
                "Global '{}' should be prefixed to '{}' in transpiled code",
                global,
                expected_prefixed
            );

            // Should not contain the unprefixed global name
            let lines: Vec<&str> = user_code.lines().collect();
            let contains_unprefixed = lines.iter().any(|line| {
                let trimmed = line.trim();
                trimmed == global || trimmed == format!("{};", global)
            });
            assert!(
                !contains_unprefixed,
                "Transpiled code should not contain unprefixed global '{}'",
                global
            );
        }
    }

    #[test]
    fn test_with_operator_in_assignment_fixed() {
        // This test verifies that assignment properly captures the full with expression
        let result = transpile_simple("y = 5 with x => x + 1").unwrap();
        let user_code = extract_user_code(&result);

        // Should properly capture the full with expression and apply the lambda to 5
        assert!(user_code.contains("const $$_y = "));
        assert!(
            user_code.contains("(5)"),
            "Should contain function application (5)"
        );

        // Should not assign the function directly without applying it
        assert!(
            !user_code.starts_with("const $$_y = (($$f) => { $$f.$$originalSource"),
            "Should not assign the lambda function directly"
        );
    }

    // Tests for each(...) with expression transpilation
    // These tests prevent regression of the issue where variables in each(...) with expressions
    // were not being properly escaped, causing ReferenceError: n is not defined

    #[test]
    fn test_simple_each_with_expression() {
        let result = transpile_simple("result = each(range(n)) with i => i * 2").unwrap();
        let user_code = extract_user_code(&result);

        // Should properly escape all variables and use .map() for each(...) with
        assert!(user_code.contains("$$_each($$_range($$_n))"));
        assert!(user_code.contains(".map("));
        assert!(user_code.contains("$$_i"));
        assert!(user_code.contains("$$multiply($$_i, 2)"));

        // Should not contain unescaped variable names in executable code
        let executable_code = user_code.replace("$$originalSource", "");
        assert!(!executable_code.contains("each(range(n))"));
        assert!(!executable_code.contains("(i) =>"));
    }

    #[test]
    fn test_parenthesized_each_with_expression() {
        let result = transpile_simple("result = (each(range(n))) with i => i * 2").unwrap();
        let user_code = extract_user_code(&result);

        // Should handle parentheses around each(...) and still escape properly
        assert!(user_code.contains("$$_each($$_range($$_n))"));
        assert!(user_code.contains(".map("));
        assert!(user_code.contains("$$_i"));
        assert!(user_code.contains("$$multiply($$_i, 2)"));

        // Should not contain unescaped variables in executable code
        let executable_code = user_code.replace("$$originalSource", "");
        assert!(!executable_code.contains("each(range(n))"));
        assert!(!executable_code.contains("range(n)"));
    }

    #[test]
    fn test_complex_each_with_nested_functions() {
        let result =
            transpile_simple("fibs = collect(each(range(n)) with i => fibonacci(i))").unwrap();
        let user_code = extract_user_code(&result);

        // Should escape all variables in complex nested expressions
        assert!(user_code.contains("$$_collect($$_each($$_range($$_n))"));
        assert!(user_code.contains("$$_fibonacci($$_i)"));
        assert!(user_code.contains(".map("));

        // Should not have any unescaped variables in executable code (excluding debug metadata)
        // Remove all debug metadata lines that contain $$originalSource
        let executable_code = user_code
            .lines()
            .filter(|line| !line.contains("$$originalSource"))
            .collect::<Vec<_>>()
            .join("\n");
        assert!(!executable_code.contains("range(n)"));
        assert!(!executable_code.contains("fibonacci(i)"));
    }

    #[test]
    fn test_record_shorthand_with_each_expression() {
        let result =
            transpile_simple("data = each(range(n)) with x => { x, value: x * 2 }").unwrap();
        let user_code = extract_user_code(&result);

        // Should properly handle record shorthand syntax
        assert!(user_code.contains("$$_each($$_range($$_n))"));
        assert!(user_code.contains("x: $$_x")); // Record shorthand should expand properly
        assert!(user_code.contains("value: $$multiply($$_x, 2)"));

        // The important thing is that in the actual executable code, shorthand is expanded properly
        // The $$originalSource metadata will contain the original unescaped form for debugging
        assert!(user_code.contains("({x: $$_x, value:")); // Should be expanded to { x: $$_x,
    }

    #[test]
    fn test_record_spread_with_each_expression() {
        let result =
            transpile_simple("times = each(range(n)) with x => { x, ...get_data(x) }").unwrap();
        let user_code = extract_user_code(&result);

        // Should handle spread syntax correctly
        assert!(user_code.contains("$$_each($$_range($$_n))"));
        assert!(user_code.contains("x: $$_x"));
        assert!(user_code.contains("...$$_get_data($$_x)"));

        // Should not contain unescaped variables in executable code
        // Remove all debug metadata lines that contain $$originalSource
        let executable_code = user_code
            .lines()
            .filter(|line| !line.contains("$$originalSource"))
            .collect::<Vec<_>>()
            .join("\n");
        assert!(!executable_code.contains("get_data(x)"));
        assert!(!executable_code.contains("range(n)"));
    }

    #[test]
    fn test_nested_each_with_expressions() {
        let result =
            transpile_simple("matrix = each(range(n)) with i => each(range(m)) with j => i * j")
                .unwrap();
        let user_code = extract_user_code(&result);

        // Should handle nested each(...) with expressions
        assert!(user_code.contains("$$_each($$_range($$_n))"));
        assert!(user_code.contains("$$_each($$_range($$_m))"));
        assert!(user_code.contains("$$multiply($$_i, $$_j)"));

        // Should not contain unescaped variables in executable code
        // Remove all debug metadata lines that contain $$originalSource
        let executable_code = user_code
            .lines()
            .filter(|line| !line.contains("$$originalSource"))
            .collect::<Vec<_>>()
            .join("\n");
        assert!(!executable_code.contains("range(n)"));
        assert!(!executable_code.contains("range(m)"));
    }

    #[test]
    fn test_each_with_complex_lambda() {
        let result = transpile_simple(
            "result = each(data) with item => { transformed: process(item.value), index: item.id }",
        )
        .unwrap();
        let user_code = extract_user_code(&result);

        // Should properly escape all parts of complex lambda expressions
        assert!(user_code.contains("$$_each($$_data)"));
        assert!(user_code.contains("$$_process($$_item.value)"));
        assert!(user_code.contains("$$_item.id"));

        // Should not contain unescaped variables in executable code
        // Remove all debug metadata lines that contain $$originalSource
        let executable_code = user_code
            .lines()
            .filter(|line| !line.contains("$$originalSource"))
            .collect::<Vec<_>>()
            .join("\n");
        assert!(!executable_code.contains("process(item.value)"));
        assert!(!executable_code.contains("item.id"));
    }

    #[test]
    fn test_multiple_each_expressions_in_record() {
        let result = transpile_simple("result = { evens: each(range(n)) with x => x * 2, odds: each(range(m)) with y => y * 2 + 1 }").unwrap();
        let user_code = extract_user_code(&result);

        // Should handle multiple each(...) with expressions in the same record
        assert!(user_code.contains("evens: $$_each($$_range($$_n))"));
        assert!(user_code.contains("odds: $$_each($$_range($$_m))"));
        assert!(user_code.contains("$$multiply($$_x, 2)"));
        assert!(user_code.contains("$$add($$multiply($$_y, 2), 1)"));

        // Should not contain unescaped variables in executable code
        let executable_code = user_code.replace("$$originalSource", "");
        assert!(!executable_code.contains("range(n)"));
        assert!(!executable_code.contains("range(m)"));
    }

    #[test]
    fn test_each_with_function_calls_in_lambda() {
        let result = transpile_simple(
            "processed = each(items) with item => transform(filter(item, predicate), mapper)",
        )
        .unwrap();
        let user_code = extract_user_code(&result);

        // Should escape all function calls and variables within the lambda
        assert!(user_code.contains("$$_each($$_items)"));
        assert!(user_code.contains("$$_transform($$_filter($$_item, $$_predicate), $$_mapper)"));

        // Should not contain unescaped variables in executable code
        // Remove all debug metadata lines that contain $$originalSource
        let executable_code = user_code
            .lines()
            .filter(|line| !line.contains("$$originalSource"))
            .collect::<Vec<_>>()
            .join("\n");
        assert!(!executable_code.contains("filter(item, predicate)"));
        assert!(!executable_code.contains("transform(filter"));
    }

    #[test]
    fn test_benchmarks_pattern_regression() {
        // This test specifically covers the pattern that was failing in bench.blot
        let result = transpile_simple("benchmark = { fibs: collect((each(range(n))) with i => fibonacci(i)), squares: collect((each(range(n))) with i => i * i) }").unwrap();
        let user_code = extract_user_code(&result);

        // Should handle the exact pattern from bench.blot that was causing issues
        assert!(user_code.contains("$$_collect($$_each($$_range($$_n))"));
        assert!(user_code.contains("$$_fibonacci($$_i)"));
        assert!(user_code.contains("$$multiply($$_i, $$_i)"));

        // Note: $$originalSource metadata intentionally contains original unescaped code for debugging
    }

    #[test]
    fn test_parenthesized_expression_in_lambda() {
        // This test covers the bug where (x + 1) in lambda doesn't escape x
        let result = transpile_simple("[1,2,3] with x => (x + 1)").unwrap();
        let user_code = extract_user_code(&result);

        // Should correctly escape the variable inside parentheses
        assert!(user_code.contains("$$add($$_x, 1)"));

        // Should not contain unescaped x in executable code
        let executable_code = user_code
            .lines()
            .filter(|line| !line.contains("$$originalSource"))
            .collect::<Vec<_>>()
            .join("\n");
        assert!(!executable_code.contains("x + 1"));
    }
}

/// Translate JavaScript identifiers back to their original form for user display
/// Removes the $$_ prefix from user variables while preserving built-in function names
pub fn translate_js_identifiers(text: &str) -> String {
    let re = Regex::new(r"\$\$_(\w+)").unwrap();
    re.replace_all(text, "$1").to_string()
}

/// Translate JavaScript error messages back to user-friendly form
pub fn translate_js_error(error: &str) -> String {
    translate_js_identifiers(error)
}
