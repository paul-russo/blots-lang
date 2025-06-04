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
                    output.push('-');
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
                    output.push('-');
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
        let value = self.transpile_expression(pairs.next().unwrap().into_inner())?;
        
        if self.inline_evaluation {
            // Capture the binding in $$results.bindings (use original name as the key)
            Ok(format!("const {} = {}; $$results.bindings['{}'] = {}", escaped_var_name, value, original_var_name, escaped_var_name))
        } else {
            Ok(format!("const {} = {}", escaped_var_name, value))
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