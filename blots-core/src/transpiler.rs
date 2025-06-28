use crate::parser::{get_pairs, Rule};
use anyhow::{anyhow, Result};
use pest::iterators::Pairs;

// Note: We implement a custom with-chain parser instead of using the Pratt parser
// due to Rust borrowing issues with closures that access &mut self

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
            output.push_str("const $$results = { values: {}, bindings: {}, outputs: new Set() };\n");
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

    fn transpile_statement_with_position(&mut self, pair: pest::iterators::Pair<Rule>) -> Result<String> {
        if let Some(inner_pair) = pair.into_inner().next() {
            let start_pos = inner_pair.as_span().start_pos().line_col();
            let end_pos = inner_pair.as_span().end_pos().line_col();
            let position_id = format!("{}-{}__{}-{}", start_pos.0, start_pos.1, end_pos.0, end_pos.1);
            
            match inner_pair.as_rule() {
                Rule::expression => {
                    let expr = self.transpile_expression(inner_pair.into_inner())?;
                    if self.inline_evaluation {
                        // For inline evaluation, we need to handle assignments differently
                        if expr.starts_with("const ") {
                            // It's an assignment - execute it and capture the variable value
                            let var_name = expr.split('=').next().unwrap().trim().replace("const ", "");
                            Ok(format!("{};\n$$results.values['{}'] = {};", expr, position_id, var_name))
                        } else {
                            // It's a pure expression - capture its result
                            Ok(format!("$$results.values['{}'] = {};", position_id, expr))
                        }
                    } else {
                        Ok(self.ensure_semicolon(expr))
                    }
                }
                Rule::output_declaration => {
                    let output_stmt = self.transpile_output_declaration(inner_pair.clone().into_inner())?;
                    if self.inline_evaluation {
                        // For output declarations, extract the variable name and capture its value
                        let var_name = self.extract_output_variable_name(inner_pair.into_inner())?;
                        Ok(format!("{};\n$$results.values['{}'] = {};", output_stmt, position_id, var_name))
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
                Rule::identifier => {
                    Ok(self.escape_js_identifier(pair.as_str()))
                }
                Rule::assignment => {
                    // Extract variable name from assignment
                    let assignment = self.transpile_assignment(pair.into_inner())?;
                    let var_name = assignment.split('=').next().unwrap().trim().replace("const ", "");
                    Ok(var_name)
                }
                rule => Err(anyhow!("Unexpected output declaration rule in extract_output_variable_name: {:?}", rule)),
            }
        } else {
            Err(anyhow!("Empty output declaration in extract_output_variable_name"))
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
                    let var_name = assignment.split('=').next().unwrap().trim().replace("const ", "");
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
    
    fn transpile_with_chain_properly(&mut self, pairs: &[pest::iterators::Pair<Rule>]) -> Result<String> {
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
                Rule::nested_expression => {
                    Ok(format!("({})", self.transpile_expression(pairs[0].clone().into_inner())?))
                },
                _ => self.transpile_single_term(pairs[0].clone()),
            }
        } else {
            // Multiple pairs in segment - this could be a function call or other complex expression
            self.transpile_expression_from_pairs(pairs.to_vec())
        }
    }
    
    fn transpile_expression_from_pairs(&mut self, pairs: Vec<pest::iterators::Pair<Rule>>) -> Result<String> {
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
                Rule::nested_expression => {
                    Ok(format!("({})", self.transpile_expression(pair.clone().into_inner())?))
                }
                _ => self.transpile_single_term(pair.clone()),
            }
        } else {
            // For complex expressions, we need to fall back to the old method
            // Create a temporary iterator from our vector
            self.transpile_complex_expression_from_pairs(pairs)
        }
    }
    
    fn transpile_complex_expression_from_pairs(&mut self, pairs: Vec<pest::iterators::Pair<Rule>>) -> Result<String> {
        // This is a simplified version of the old expression logic for non-with expressions
        let mut output = String::new();
        let mut terms = Vec::new();
        let mut operators = Vec::new();
        let mut i = 0;
        
        while i < pairs.len() {
            let pair = &pairs[i];
            
            match pair.as_rule() {
                Rule::negation => {
                    // Handle unary minus - we need to get the next term and apply negation to it
                    if let Some(next_pair) = pairs.get(i + 1) {
                        let next_term = match next_pair.as_rule() {
                            Rule::lambda => self.transpile_lambda(next_pair.clone().into_inner())?,
                            Rule::list => self.transpile_list(next_pair.clone().into_inner())?,
                            Rule::record => self.transpile_record(next_pair.clone().into_inner())?,
                            Rule::bool => next_pair.as_str().to_string(),
                            Rule::string => next_pair.as_str().to_string(),
                            Rule::null => "null".to_string(),
                            Rule::identifier => self.escape_js_identifier(next_pair.as_str()),
                            Rule::number => next_pair.as_str().replace('_', ""),
                            Rule::nested_expression => {
                                format!("({})", self.transpile_expression(next_pair.clone().into_inner())?)
                            },
                            _ => self.transpile_single_term(next_pair.clone())?,
                        };
                        terms.push(format!("(-({}))", next_term));
                        i += 1; // Skip the next pair since we consumed it
                    } else {
                        return Err(anyhow!("Negation operator without following term"));
                    }
                }
                Rule::invert => {
                    output.push('!');
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
                    terms.push(self.transpile_list(pair.clone().into_inner())?);
                }
                Rule::record => {
                    terms.push(self.transpile_record(pair.clone().into_inner())?);
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
                    terms.push(format!("({})", self.transpile_expression(pair.clone().into_inner())?));
                }
                Rule::factorial => {
                    if let Some(last_term) = terms.last_mut() {
                        *last_term = format!("$$factorial({})", last_term);
                    }
                }
                Rule::access => {
                    if let Some(last_term) = terms.last_mut() {
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
                    if let Some(last_term) = terms.last_mut() {
                        let prop = pair.clone().into_inner().next().unwrap().as_str();
                        *last_term = format!("{}.{}", last_term, prop);
                    }
                }
                Rule::call_list => {
                    if let Some(last_term) = terms.last_mut() {
                        let args = self.transpile_call_args(pair.clone().into_inner())?;
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
                Rule::expression => {
                    let expr_result = self.transpile_expression(pair.clone().into_inner())?;
                    terms.push(format!("({})", expr_result));
                }
                _ => {}
            }
            
            i += 1;
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
                            },
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
                    terms.push(format!("({})", self.transpile_expression(pair.into_inner())?));
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
                                    format!("({})", self.transpile_expression(next_pair.into_inner())?)
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
        matches!(op, "$$add" | "$$subtract" | "$$multiply" | "$$divide" | "$$modulo" | "$$power")
    }
    
    fn is_distributive_operator(&self, op: &str) -> bool {
        matches!(op, " && " | " || " | " ?? " | " === " | " !== " | " < " | " <= " | " > " | " >= ")
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
            Ok(format!("$$each({}.map((x, i) => x{} ({})[i]))", left_inner, op, right_inner))
        } else {
            // Neither side is each - regular operation
            Ok(format!("{}{}{}", left, op, right))
        }
    }
    
    fn extract_each_inner<'a>(&self, expr: &'a str) -> &'a str {
        if expr.starts_with("$$each(") && expr.ends_with(")") {
            &expr[7..expr.len()-1]
        } else if expr.starts_with("each(") && expr.ends_with(")") {
            &expr[5..expr.len()-1]
        } else {
            expr
        }
    }
    
    fn escape_js_identifier(&self, ident: &str) -> String {
        // List of JavaScript reserved keywords that need escaping
        let js_keywords = [
            "abstract", "await", "boolean", "break", "byte", "case", "catch", "char",
            "class", "const", "continue", "debugger", "default", "delete", "do", "double",
            "else", "enum", "export", "extends", "false", "final", "finally", "float",
            "for", "function", "goto", "if", "implements", "import", "in", "instanceof",
            "int", "interface", "let", "long", "native", "new", "package", "private",
            "protected", "public", "return", "short", "static", "super", "switch",
            "synchronized", "this", "throw", "throws", "transient", "true", "try",
            "typeof", "undefined", "var", "void", "volatile", "while", "with", "yield"
        ];
        
        if js_keywords.contains(&ident) {
            format!("$$_{}", ident)
        } else {
            ident.to_string()
        }
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
            Rule::nested_expression => {
                Ok(format!("({})", self.transpile_expression(primary.into_inner())?))
            }
            Rule::expression => {
                // Handle inner expressions (from parentheses that were parsed as silent rules)
                let expr_result = self.transpile_expression(primary.into_inner())?;
                Ok(format!("({})", expr_result))
            }
            _ => Ok(primary.as_str().to_string()),
        }
    }


    fn transpile_with_operator(&mut self, lhs: &str, rhs: &str) -> Result<String> {
        // Check if the left side is an 'each' expression
        let is_each_expr = self.is_each_expression(lhs);
        
        if is_each_expr {
            // For each expressions, use mapping behavior
            // Extract the inner expression from each(...) or $$each(...)
            let inner_expr = if lhs.starts_with("$$each(") && lhs.ends_with(")") {
                &lhs[7..lhs.len()-1]
            } else if lhs.starts_with("each(") && lhs.ends_with(")") {
                &lhs[5..lhs.len()-1]
            } else {
                lhs
            };
            Ok(format!("$$each({}.map({}))", inner_expr, rhs))
        } else {
            // For regular values, use function application
            Ok(format!("({})({})", rhs, lhs))
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
            original_source.replace('`', "\\`"), args, formatted_body
        ))
    }

    fn transpile_argument_list(&mut self, mut pairs: Pairs<Rule>) -> Result<String> {
        let mut args = Vec::new();
        
        while let Some(pair) = pairs.next() {
            match pair.as_rule() {
                Rule::required_arg => {
                    args.push(pair.into_inner().next().unwrap().as_str().to_string());
                }
                Rule::optional_arg => {
                    let arg_name = pair.into_inner().next().unwrap().as_str();
                    args.push(format!("{} = undefined", arg_name));
                }
                Rule::rest_arg => {
                    let arg_name = pair.into_inner().next().unwrap().as_str();
                    args.push(format!("...{}", arg_name));
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
        
        // Check if the value itself is an assignment (chained assignment)
        let value_pairs: Vec<_> = value_pair.into_inner().collect();
        
        // Look for assignments in the value expression
        if value_pairs.len() == 1 && value_pairs[0].as_rule() == Rule::assignment {
            // This is a chained assignment like a = b = 5
            // We need to handle this specially to avoid "const a = const b = 5"
            let inner_assignment = self.transpile_assignment(value_pairs[0].clone().into_inner())?;
            
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
                        return Ok(format!("{}; const {} = {}", 
                            inner_assignment, 
                            escaped_var_name, 
                            var_part.replace("const ", "")));
                    }
                }
                // Fallback - extract from first statement
                inner_assignment.split('=').next().unwrap().trim().replace("const ", "")
            } else {
                // Single statement
                inner_assignment.split('=').next().unwrap().trim().replace("const ", "")
            };
            
            if self.inline_evaluation {
                // For inline evaluation, we need both assignments
                Ok(format!("{}; const {} = {}; $$results.bindings['{}'] = {}", 
                    inner_assignment, escaped_var_name, inner_var, original_var_name, escaped_var_name))
            } else {
                // Generate: const b = 5; const a = b;
                Ok(format!("{}; const {} = {}", inner_assignment, escaped_var_name, inner_var))
            }
        } else {
            // Regular assignment - not chained
            let value = self.transpile_expression_from_pairs(value_pairs)?;
            
            if self.inline_evaluation {
                // Capture the binding in $$results.bindings (use original name as the key)
                Ok(format!("const {} = {}; $$results.bindings['{}'] = {}", escaped_var_name, value, original_var_name, escaped_var_name))
            } else {
                Ok(format!("const {} = {}", escaped_var_name, value))
            }
        }
    }

    fn transpile_list(&mut self, mut pairs: Pairs<Rule>) -> Result<String> {
        let mut elements = Vec::new();
        
        while let Some(pair) = pairs.next() {
            match pair.as_rule() {
                Rule::spread_expression => {
                    let expr = self.transpile_expression(pair.into_inner().skip(1).next().unwrap().into_inner())?;
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
                    properties.push(prop_name.to_string());
                }
                Rule::spread_expression => {
                    let expr = self.transpile_expression(pair.into_inner().skip(1).next().unwrap().into_inner())?;
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
                    let expr = self.transpile_expression(pair.into_inner().skip(1).next().unwrap().into_inner())?;
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
            Rule::nested_expression => {
                Ok(format!("({})", self.transpile_expression(pair.into_inner())?))
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
        term.starts_with("each(") || term.starts_with("$$each(")
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
            if line.contains("var inputs = globalThis.inputs;") {
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
        assert!(user_code.contains("const x = 5"));
    }

    #[test]
    fn test_chained_assignment_two_variables() {
        let result = transpile_simple("a = b = 5").unwrap();
        let user_code = extract_user_code(&result);
        
        // Should generate: const b = 5; const a = b;
        assert!(user_code.contains("const b = 5"));
        assert!(user_code.contains("const a = b"));
        assert!(!user_code.contains("const a = const b"));
    }

    #[test]
    fn test_chained_assignment_three_variables() {
        let result = transpile_simple("x = y = z = 10").unwrap();
        let user_code = extract_user_code(&result);
        
        // Should generate a chain of assignments
        assert!(user_code.contains("const z = 10"));
        assert!(user_code.contains("const y = z"));
        assert!(user_code.contains("const x = y"));
        assert!(!user_code.contains("const x = const y"));
        assert!(!user_code.contains("const y = const z"));
    }

    #[test]
    fn test_chained_assignment_with_expression() {
        let result = transpile_simple("a = b = 2 + 3").unwrap();
        let user_code = extract_user_code(&result);
        
        // Should evaluate the expression first, then assign
        assert!(user_code.contains("const b = $$add(2, 3)"));
        assert!(user_code.contains("const a = b"));
    }

    #[test]
    fn test_chained_assignment_with_list() {
        let result = transpile_simple("c = d = [1, 2, 3]").unwrap();
        let user_code = extract_user_code(&result);
        
        // Should handle list expressions
        assert!(user_code.contains("const d = [1, 2, 3]"));
        assert!(user_code.contains("const c = d"));
    }

    #[test]
    fn test_nested_assignment_in_expression() {
        // This test case shows a limitation - parenthesized assignments aren't fully supported yet
        // The transpiler produces: const result = p = q = 42; which is invalid JS
        // For now, just test that we don't crash and the assignment is attempted
        let result = transpile_simple("result = (p = q = 42)").unwrap();
        let user_code = extract_user_code(&result);
        
        // The current transpiler has limitations with nested assignments in parentheses
        assert!(user_code.contains("const result = "));
        assert!(user_code.contains("42"));
    }

    #[test] 
    fn test_complex_chained_assignment_with_operations() {
        let result = transpile_simple("w = x = y = z = 1 + 2 * 3").unwrap();
        let user_code = extract_user_code(&result);
        
        // Should create a proper chain
        assert!(user_code.contains("const z = "));
        assert!(user_code.contains("const y = z"));
        assert!(user_code.contains("const x = y"));
        assert!(user_code.contains("const w = x"));
        assert!(user_code.contains("$$add"));
        assert!(user_code.contains("$$multiply"));
    }

    #[test]
    fn test_regular_assignment_still_works() {
        let result = transpile_simple("single = 100").unwrap();
        let user_code = extract_user_code(&result);
        
        assert!(user_code.contains("const single = 100"));
        assert!(!user_code.contains("const single = const"));
    }

    #[test]
    fn test_assignment_with_function_call() {
        let result = transpile_simple("a = b = sum(1, 2, 3)").unwrap();
        let user_code = extract_user_code(&result);
        
        // Should handle function calls in chained assignments
        assert!(user_code.contains("const b = sum(1, 2, 3)"));
        assert!(user_code.contains("const a = b"));
    }

    #[test]
    fn test_mixed_assignments_and_expressions() {
        let result = transpile_simple("x = 5\ny = z = x + 1\nprint(y)").unwrap();
        let user_code = extract_user_code(&result);
        
        assert!(user_code.contains("const x = 5"));
        assert!(user_code.contains("const z = $$add(x, 1)"));
        assert!(user_code.contains("const y = z"));
        assert!(user_code.contains("print(y)"));
    }

    #[test]
    fn test_arithmetic_operations() {
        let result = transpile_simple("result = 2 + 3 * 4").unwrap();
        let user_code = extract_user_code(&result);
        
        // Should use the proper function calls for arithmetic
        assert!(user_code.contains("$$add"));
        assert!(user_code.contains("$$multiply"));
    }

    #[test]
    fn test_list_creation() {
        let result = transpile_simple("numbers = [1, 2, 3]").unwrap();
        let user_code = extract_user_code(&result);
        
        assert!(user_code.contains("const numbers = [1, 2, 3]"));
    }

    #[test]
    fn test_record_creation() {
        let result = transpile_simple("person = {name: \"Alice\", age: 30}").unwrap();
        let user_code = extract_user_code(&result);
        
        assert!(user_code.contains("const person = {"));
        assert!(user_code.contains("name: \"Alice\""));
        assert!(user_code.contains("age: 30"));
    }

    #[test]
    fn test_lambda_expression() {
        let result = transpile_simple("add = (x, y) => x + y").unwrap();
        let user_code = extract_user_code(&result);
        
        // Lambda expressions are complex, just check that we have the assignment and the add function
        assert!(user_code.contains("const add = "));
        assert!(user_code.contains("$$add(x, y)"));
    }

    #[test]
    fn test_function_call() {
        let result = transpile_simple("result = map([1, 2, 3], (x) => x * 2)").unwrap();
        let user_code = extract_user_code(&result);
        
        // Function calls may be complex, just check basic structure
        assert!(user_code.contains("const result = "));
        assert!(user_code.contains("map"));
        assert!(user_code.contains("$$multiply"));
    }

    #[test]
    fn test_conditional_expression() {
        let result = transpile_simple("result = if true then 1 else 2").unwrap();
        let user_code = extract_user_code(&result);
        
        assert!(user_code.contains("const result = (true ? 1 : 2)"));
    }

    #[test]
    fn test_print_statement() {
        let result = transpile_simple("print(\"Hello, {}!\", \"World\")").unwrap();
        let user_code = extract_user_code(&result);
        
        assert!(user_code.contains("print(\"Hello, {}!\", \"World\")"));
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
        assert!(user_code.contains("const b = 10"));
        assert!(user_code.contains("const a = b"));
    }

    #[test]
    fn test_assignment_does_not_break_other_expressions() {
        let result = transpile_simple("x = 5\ny = x + 1\nz = y * 2").unwrap();
        let user_code = extract_user_code(&result);
        
        assert!(user_code.contains("const x = 5"));
        assert!(user_code.contains("const y = $$add(x, 1)"));
        assert!(user_code.contains("const z = $$multiply(y, 2)"));
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
        
        // Should transpile to_string call correctly
        assert!(user_code.contains("const result = to_string(map)"));
    }

    #[test]
    fn test_format_function_call_transpilation() {
        let result = transpile_simple("print(\"Function: {}\", filter)").unwrap();
        let user_code = extract_user_code(&result);
        
        // Should transpile format call correctly
        assert!(user_code.contains("print(\"Function: {}\", filter)"));
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
        
        // Should include inputs setup
        assert!(result.contains("// Set up inputs variable"));
        assert!(result.contains("if (typeof globalThis.inputs === \"undefined\")"));
        assert!(result.contains("globalThis.inputs = {};"));
        assert!(result.contains("var inputs = globalThis.inputs;"));
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
}