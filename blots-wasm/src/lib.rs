use anyhow::Result;
use blots_core::{
    ast::{Expr, Spanned},
    environment::Environment,
    expressions::{evaluate_pairs, pairs_to_expr},
    formatter::{format_expr, join_statements_with_spacing},
    functions::get_built_in_function_idents,
    heap::{CONSTANTS, Heap},
    parser::{Rule, Token, get_pairs, get_tokens},
    values::SerializableValue,
};
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};
use wasm_bindgen::prelude::*;

#[derive(Debug, Serialize, Deserialize)]
struct EvaluationResult {
    values: HashMap<String, SerializableValue>,
    bindings: HashMap<String, SerializableValue>,
    outputs: HashSet<String>,
}

#[wasm_bindgen]
pub fn evaluate(expr: &str, inputs_js: JsValue) -> Result<JsValue, JsError> {
    let heap = Rc::new(RefCell::new(Heap::new()));
    let inputs_given: IndexMap<String, SerializableValue> =
        serde_wasm_bindgen::from_value(inputs_js)?;

    let inputs = inputs_given
        .into_iter()
        .map(|(key, value)| (key, value.to_value(&mut heap.borrow_mut()).unwrap()))
        .collect();

    let bindings = Rc::new(Environment::new());
    bindings.insert(
        String::from("inputs"),
        heap.borrow_mut().insert_record(inputs),
    );

    let expr_owned = String::from(expr);
    let pairs =
        get_pairs(&expr_owned).map_err(|e| JsError::new(&format!("Parsing error: {}", e)))?;

    let mut outputs = HashSet::new();
    let mut values = HashMap::new();

    for pair in pairs {
        match pair.as_rule() {
            Rule::statement => {
                if let Some(inner_pair) = pair.into_inner().next() {
                    let rule = inner_pair.as_rule();

                    // Skip comments - they don't need to be evaluated
                    if rule == Rule::comment {
                        continue;
                    }

                    let start_line_col = inner_pair.as_span().start_pos().line_col();
                    let end_line_col = inner_pair.as_span().end_pos().line_col();

                    let inner_pairs = inner_pair.into_inner();

                    if rule == Rule::output_declaration {
                        let mut inner_pairs_clone = inner_pairs.clone();

                        let next_token = inner_pairs_clone.next().unwrap(); // Skip the output keyword.
                        let output_name = match next_token.as_rule() {
                            Rule::identifier => next_token.as_str().to_string(),
                            Rule::assignment => {
                                let next_inner_token = next_token.into_inner().next().unwrap();
                                match next_inner_token.as_rule() {
                                    Rule::identifier => next_inner_token.as_str().to_string(),
                                    _ => {
                                        unreachable!(
                                            "unexpected rule: {:?}",
                                            next_inner_token.as_rule()
                                        )
                                    }
                                }
                            }
                            _ => unreachable!("unexpected rule: {:?}", next_token.as_rule()),
                        };

                        outputs.insert(output_name);
                    }

                    let col_id = format!(
                        "{}-{}__{}-{}",
                        start_line_col.0, start_line_col.1, end_line_col.0, end_line_col.1
                    );

                    let value =
                        evaluate_pairs(inner_pairs, Rc::clone(&heap), Rc::clone(&bindings), 0, expr)
                            .map_err(|error| {
                                JsError::new(&format!("Evaluation error: {}", error))
                            })?;

                    values.insert(col_id, value);
                }
            }
            Rule::EOI => {}
            rule => unreachable!("unexpected rule: {:?}", rule),
        }
    }

    let values_serializable = values
        .iter()
        .map(|(k, v)| (k.clone(), v.to_serializable_value(&heap.borrow()).unwrap()))
        .collect();

    let bindings_serializable = bindings
        .iter()
        .map(|(k, v)| (k, v.to_serializable_value(&heap.borrow()).unwrap()))
        .collect();

    // Use json_compatible serializer to ensure Records are serialized as JSON objects
    // instead of JavaScript Maps
    let serializer = serde_wasm_bindgen::Serializer::json_compatible();
    Ok(EvaluationResult {
        values: values_serializable,
        bindings: bindings_serializable,
        outputs,
    }
    .serialize(&serializer)?)
}

#[derive(Debug, Serialize, Deserialize)]
enum SerialToken {
    Start { rule: String, pos: usize },
    End { rule: String, pos: usize },
}

#[wasm_bindgen]
pub fn tokenize(input: &str) -> Result<JsValue, JsError> {
    let input = input.to_string();
    let tokens = get_tokens(&input)?;

    let tokens: Vec<SerialToken> = tokens
        .iter()
        .map(|token| match token {
            Token::Start { rule, pos } => SerialToken::Start {
                rule: format!("{:?}", rule),
                pos: pos.pos(),
            },
            Token::End { rule, pos } => SerialToken::End {
                rule: format!("{:?}", rule),
                pos: pos.pos(),
            },
        })
        .collect();

    // Use json_compatible serializer for consistency
    let serializer = serde_wasm_bindgen::Serializer::json_compatible();
    Ok(tokens.serialize(&serializer)?)
}

#[wasm_bindgen]
pub fn get_built_in_function_names() -> Result<JsValue, JsError> {
    // Use json_compatible serializer for consistency
    let serializer = serde_wasm_bindgen::Serializer::json_compatible();
    Ok(get_built_in_function_idents().serialize(&serializer)?)
}

#[wasm_bindgen]
pub fn get_constants() -> Result<JsValue, JsError> {
    let mut map = HashMap::new();
    let constants = SerializableValue::Record(
        CONSTANTS
            .clone()
            .into_iter()
            .map(|(k, v)| (k, v.into()))
            .collect(),
    );

    map.insert(String::from("constants"), constants);

    // Use json_compatible serializer to ensure Records are serialized as JSON objects
    let serializer = serde_wasm_bindgen::Serializer::json_compatible();
    Ok(map.serialize(&serializer)?)
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(untagged)]
enum ExpressionResult {
    Value { value: SerializableValue },
    Error { error: String },
}

#[wasm_bindgen]
pub fn evaluate_inline_expressions(
    expressions_js: JsValue,
    inputs_js: JsValue,
) -> Result<JsValue, JsError> {
    let expressions: Vec<String> = serde_wasm_bindgen::from_value(expressions_js)?;
    let inputs_given: IndexMap<String, SerializableValue> =
        serde_wasm_bindgen::from_value(inputs_js)?;

    let mut results = Vec::new();

    for expr in expressions {
        let result = evaluate_single_inline_expression(&expr, &inputs_given);
        results.push(result);
    }

    // Use json_compatible serializer to ensure Records are serialized as JSON objects
    let serializer = serde_wasm_bindgen::Serializer::json_compatible();
    Ok(results.serialize(&serializer)?)
}

fn evaluate_single_inline_expression(
    expr: &str,
    inputs_given: &IndexMap<String, SerializableValue>,
) -> ExpressionResult {
    let heap = Rc::new(RefCell::new(Heap::new()));

    // Convert inputs to Values
    let inputs: IndexMap<String, _> = inputs_given
        .iter()
        .map(|(key, value)| (key.clone(), value.to_value(&mut heap.borrow_mut()).unwrap()))
        .collect();

    let bindings = Rc::new(Environment::new());

    // Add inputs record
    bindings.insert(
        String::from("inputs"),
        heap.borrow_mut().insert_record(inputs.clone()),
    );

    // Inject all input values directly into bindings for inline access
    for (key, value) in inputs {
        bindings.insert(key, value);
    }

    // Parse the expression
    let expr_string = expr.to_string();
    let pairs = match get_pairs(&expr_string) {
        Ok(pairs) => pairs,
        Err(e) => {
            return ExpressionResult::Error {
                error: format!("Parsing error: {}", e),
            };
        }
    };

    // Find and evaluate the first statement/expression
    for pair in pairs {
        match pair.as_rule() {
            Rule::statement => {
                if let Some(inner_pair) = pair.into_inner().next() {
                    let inner_pairs = inner_pair.into_inner();

                    match evaluate_pairs(inner_pairs, Rc::clone(&heap), Rc::clone(&bindings), 0, expr) {
                        Ok(value) => match value.to_serializable_value(&heap.borrow()) {
                            Ok(serializable) => {
                                return ExpressionResult::Value {
                                    value: serializable,
                                };
                            }
                            Err(e) => {
                                return ExpressionResult::Error {
                                    error: format!("Serialization error: {}", e),
                                };
                            }
                        },
                        Err(e) => {
                            return ExpressionResult::Error {
                                error: format!("Evaluation error: {}", e),
                            };
                        }
                    }
                }
            }
            Rule::EOI => continue,
            _ => continue,
        }
    }

    ExpressionResult::Error {
        error: "No expression found".to_string(),
    }
}

#[wasm_bindgen]
pub fn format_blots(source: &str, max_columns: Option<usize>) -> Result<JsValue, JsError> {
    // Parse the source code to AST
    let pairs = get_pairs(source).map_err(|e| JsError::new(&format!("Parsing error: {}", e)))?;

    // Format all statements (expressions and comments)
    let mut formatted_statements = Vec::new();

    for pair in pairs {
        if pair.as_rule() == Rule::statement {
            let start_line = pair.as_span().start_pos().line_col().0;
            let end_line = pair.as_span().end_pos().line_col().0;
            let mut inner_pairs = pair.into_inner();

            if let Some(first_pair) = inner_pairs.next() {
                let formatted = match first_pair.as_rule() {
                    Rule::comment => {
                        // Standalone comment
                        first_pair.as_str().to_string()
                    }
                    Rule::output_declaration => {
                        // Output declaration - wrap in Output expression
                        let inner_expr = pairs_to_expr(first_pair.into_inner())
                            .map_err(|e| JsError::new(&format!("AST conversion error: {}", e)))?;
                        let output_expr = Spanned::dummy(Expr::Output {
                            expr: Box::new(inner_expr),
                        });
                        format_expr(&output_expr, max_columns)
                    }
                    _ => {
                        // Format as expression
                        let expr = pairs_to_expr(first_pair.into_inner())
                            .map_err(|e| JsError::new(&format!("AST conversion error: {}", e)))?;
                        format_expr(&expr, max_columns)
                    }
                };

                // Check for end-of-line comment (second element in statement)
                let final_formatted = if let Some(eol_comment) = inner_pairs.next() {
                    if eol_comment.as_rule() == Rule::comment {
                        format!("{}  {}", formatted, eol_comment.as_str())
                    } else {
                        formatted
                    }
                } else {
                    formatted
                };

                formatted_statements.push((final_formatted, start_line, end_line));
            }
        }
    }

    if formatted_statements.is_empty() {
        return Err(JsError::new("No statements found in source"));
    }

    // Join all formatted statements with appropriate spacing (preserving up to 1 empty line)
    let result = join_statements_with_spacing(&formatted_statements);

    // Return the formatted string
    let serializer = serde_wasm_bindgen::Serializer::json_compatible();
    Ok(result.serialize(&serializer)?)
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasm_bindgen_test::*;

    wasm_bindgen_test_configure!(run_in_browser);

    // ============================================================================
    // format_blots tests
    // ============================================================================

    #[wasm_bindgen_test]
    fn test_format_blots_simple() {
        let source = "x = [1, 2, 3]";
        let result = format_blots(source, Some(80)).unwrap();
        let formatted: String = serde_wasm_bindgen::from_value(result).unwrap();
        assert_eq!(formatted, "x = [1, 2, 3]");
    }

    #[wasm_bindgen_test]
    fn test_format_blots_multiline() {
        let source = "{name: \"Alice\", age: 30, email: \"alice@example.com\", address: \"123 Main Street\"}";
        let result = format_blots(source, Some(30)).unwrap();
        let formatted: String = serde_wasm_bindgen::from_value(result).unwrap();
        // Record should be formatted across multiple lines
        assert!(formatted.contains("\n"));
        assert!(formatted.contains("{"));
    }

    #[wasm_bindgen_test]
    fn test_format_blots_preserves_comments() {
        let source = "// This is a comment\nx = [1, 2, 3]\n// Another comment\ny = x + 1";
        let result = format_blots(source, Some(80)).unwrap();
        let formatted: String = serde_wasm_bindgen::from_value(result).unwrap();
        assert!(formatted.contains("// This is a comment"));
        assert!(formatted.contains("// Another comment"));
        assert!(formatted.contains("x = [1, 2, 3]"));
        assert!(formatted.contains("y = x + 1"));
    }

    #[wasm_bindgen_test]
    fn test_format_blots_default_max_columns() {
        let source = "x = 1";
        let result = format_blots(source, None).unwrap();
        let formatted: String = serde_wasm_bindgen::from_value(result).unwrap();
        assert_eq!(formatted, "x = 1");
    }

    #[wasm_bindgen_test]
    fn test_format_blots_invalid_syntax() {
        let source = "x = [1, 2,";
        let result = format_blots(source, Some(80));
        assert!(result.is_err());
    }

    #[wasm_bindgen_test]
    fn test_format_blots_multiple_statements() {
        let source = "x = 1\ny = 2\nz = x + y";
        let result = format_blots(source, Some(80)).unwrap();
        let formatted: String = serde_wasm_bindgen::from_value(result).unwrap();
        let lines: Vec<&str> = formatted.lines().collect();
        assert_eq!(lines.len(), 3);
        assert_eq!(lines[0], "x = 1");
        assert_eq!(lines[1], "y = 2");
        assert_eq!(lines[2], "z = x + y");
    }

    #[wasm_bindgen_test]
    fn test_format_blots_end_of_line_comments() {
        let source = "x = 5  // this is a comment\ny = 10";
        let result = format_blots(source, Some(80)).unwrap();
        let formatted: String = serde_wasm_bindgen::from_value(result).unwrap();
        let lines: Vec<&str> = formatted.lines().collect();
        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0], "x = 5  // this is a comment");
        assert_eq!(lines[1], "y = 10");
    }

    #[wasm_bindgen_test]
    fn test_format_blots_mixed_comments() {
        let source = "// standalone comment\nx = 1  // end-of-line comment\ny = 2";
        let result = format_blots(source, Some(80)).unwrap();
        let formatted: String = serde_wasm_bindgen::from_value(result).unwrap();
        let lines: Vec<&str> = formatted.lines().collect();
        assert_eq!(lines.len(), 3);
        assert_eq!(lines[0], "// standalone comment");
        assert_eq!(lines[1], "x = 1  // end-of-line comment");
        assert_eq!(lines[2], "y = 2");
    }

    #[wasm_bindgen_test]
    fn test_format_blots_preserves_empty_lines() {
        let source = "x = 1\n\ny = 2\n\n\n\nz = 3";
        let result = format_blots(source, Some(80)).unwrap();
        let formatted: String = serde_wasm_bindgen::from_value(result).unwrap();

        // Should preserve up to 2 empty lines between statements
        // 1 empty line between x and y (preserved)
        // 4 empty lines between y and z (capped at 2)
        assert_eq!(formatted, "x = 1\n\ny = 2\n\n\nz = 3");

        let lines: Vec<&str> = formatted.lines().collect();
        assert_eq!(lines.len(), 6); // 3 statements + 1 blank + 2 blanks
    }

    #[wasm_bindgen_test]
    fn test_format_blots_no_empty_lines() {
        let source = "x = 1\ny = 2\nz = 3";
        let result = format_blots(source, Some(80)).unwrap();
        let formatted: String = serde_wasm_bindgen::from_value(result).unwrap();

        // Should NOT add empty lines when there were none
        assert_eq!(formatted, "x = 1\ny = 2\nz = 3");

        let lines: Vec<&str> = formatted.lines().collect();
        assert_eq!(lines.len(), 3);
    }

    #[wasm_bindgen_test]
    fn test_format_blots_preserves_output_keyword() {
        let source = "output x = 42\noutput y = [1, 2, 3]\nz = 10";
        let result = format_blots(source, Some(80)).unwrap();
        let formatted: String = serde_wasm_bindgen::from_value(result).unwrap();

        // Output keywords should be preserved
        assert!(formatted.contains("output x = 42"));
        assert!(formatted.contains("output y = [1, 2, 3]"));
        assert!(formatted.contains("z = 10"));

        let lines: Vec<&str> = formatted.lines().collect();
        assert_eq!(lines.len(), 3);
        assert_eq!(lines[0], "output x = 42");
        assert_eq!(lines[1], "output y = [1, 2, 3]");
        assert_eq!(lines[2], "z = 10");
    }

    #[wasm_bindgen_test]
    fn test_format_blots_output_with_via() {
        let source = "output result = items via i => i * 2";
        let result = format_blots(source, Some(80)).unwrap();
        let formatted: String = serde_wasm_bindgen::from_value(result).unwrap();

        // Output keyword should be preserved with via expression
        assert!(formatted.contains("output result = items via"));
        assert!(formatted.starts_with("output result"));
    }

    // ============================================================================
    // evaluate tests
    // ============================================================================

    #[wasm_bindgen_test]
    fn test_evaluate_simple_expression() {
        let source = "output result = 1 + 2";
        let inputs = serde_wasm_bindgen::to_value(&serde_json::json!({})).unwrap();
        let result = evaluate(source, inputs).unwrap();
        let result_obj: serde_json::Value = serde_wasm_bindgen::from_value(result).unwrap();
        assert_eq!(result_obj["outputs"], serde_json::json!(["result"]));
    }

    #[wasm_bindgen_test]
    fn test_evaluate_with_inputs() {
        let source = "output doubled = inputs.x * 2";
        // Create inputs using SerializableValue directly
        use indexmap::IndexMap;
        let mut inputs_map: IndexMap<String, SerializableValue> = IndexMap::new();
        inputs_map.insert("x".to_string(), SerializableValue::Number(5.0));
        let inputs = serde_wasm_bindgen::to_value(&inputs_map).unwrap();

        let result = evaluate(source, inputs).unwrap();
        let result_obj: serde_json::Value = serde_wasm_bindgen::from_value(result).unwrap();
        assert_eq!(result_obj["outputs"], serde_json::json!(["doubled"]));

        // Check that we got a binding for doubled
        let bindings = &result_obj["bindings"];
        assert_eq!(bindings["doubled"]["Number"], serde_json::json!(10));
    }

    #[wasm_bindgen_test]
    fn test_evaluate_input_reference() {
        let source = "output result = inputs.x + inputs.y";
        // Create inputs using SerializableValue directly
        use indexmap::IndexMap;
        let mut inputs_map: IndexMap<String, SerializableValue> = IndexMap::new();
        inputs_map.insert("x".to_string(), SerializableValue::Number(3.0));
        inputs_map.insert("y".to_string(), SerializableValue::Number(7.0));
        let inputs = serde_wasm_bindgen::to_value(&inputs_map).unwrap();

        let result = evaluate(source, inputs).unwrap();
        let result_obj: serde_json::Value = serde_wasm_bindgen::from_value(result).unwrap();
        assert_eq!(result_obj["bindings"]["result"]["Number"], serde_json::json!(10));
    }

    #[wasm_bindgen_test]
    fn test_evaluate_list_operations() {
        let source = "output result = [1, 2, 3] + [4, 5, 6]";
        let inputs = serde_wasm_bindgen::to_value(&serde_json::json!({})).unwrap();
        let result = evaluate(source, inputs).unwrap();
        let result_obj: serde_json::Value = serde_wasm_bindgen::from_value(result).unwrap();
        // Check that result is a List serializable value
        assert!(result_obj["bindings"]["result"]["List"].is_array());
        let list = &result_obj["bindings"]["result"]["List"];
        assert_eq!(list[0]["Number"], serde_json::json!(5));
        assert_eq!(list[1]["Number"], serde_json::json!(7));
        assert_eq!(list[2]["Number"], serde_json::json!(9));
    }

    #[wasm_bindgen_test]
    fn test_evaluate_multiline_source() {
        let source = "x = inputs.a * 2\ny = inputs.b + 10\noutput result = x + y";
        use indexmap::IndexMap;
        let mut inputs_map: IndexMap<String, SerializableValue> = IndexMap::new();
        inputs_map.insert("a".to_string(), SerializableValue::Number(5.0));
        inputs_map.insert("b".to_string(), SerializableValue::Number(3.0));
        let inputs = serde_wasm_bindgen::to_value(&inputs_map).unwrap();

        let result = evaluate(source, inputs).unwrap();
        let result_obj: serde_json::Value = serde_wasm_bindgen::from_value(result).unwrap();

        // Check the output
        assert_eq!(result_obj["outputs"], serde_json::json!(["result"]));
        // x = 5 * 2 = 10, y = 3 + 10 = 13, result = 10 + 13 = 23
        assert_eq!(result_obj["bindings"]["result"]["Number"], serde_json::json!(23));

        // Check intermediate bindings
        assert_eq!(result_obj["bindings"]["x"]["Number"], serde_json::json!(10));
        assert_eq!(result_obj["bindings"]["y"]["Number"], serde_json::json!(13));
    }

    #[wasm_bindgen_test]
    fn test_evaluate_multiline_with_comments() {
        let source = "x = inputs.a * 2\ny = inputs.b + 10\n\n// hi\noutput result = x + y";
        use indexmap::IndexMap;
        let mut inputs_map: IndexMap<String, SerializableValue> = IndexMap::new();
        inputs_map.insert("a".to_string(), SerializableValue::Number(5.0));
        inputs_map.insert("b".to_string(), SerializableValue::Number(3.0));
        let inputs = serde_wasm_bindgen::to_value(&inputs_map).unwrap();

        let result = evaluate(source, inputs).unwrap();
        let result_obj: serde_json::Value = serde_wasm_bindgen::from_value(result).unwrap();

        // Check the output
        assert_eq!(result_obj["outputs"], serde_json::json!(["result"]));
        // x = 5 * 2 = 10, y = 3 + 10 = 13, result = 10 + 13 = 23
        assert_eq!(result_obj["bindings"]["result"]["Number"], serde_json::json!(23));
    }

    #[wasm_bindgen_test]
    fn test_evaluate_invalid_syntax() {
        let source = "output result = 1 +";
        let inputs = serde_wasm_bindgen::to_value(&serde_json::json!({})).unwrap();
        let result = evaluate(source, inputs);
        assert!(result.is_err());
    }

    // ============================================================================
    // tokenize tests
    // ============================================================================

    #[wasm_bindgen_test]
    fn test_tokenize_simple() {
        let source = "1 + 2";
        let result = tokenize(source).unwrap();
        let tokens: Vec<serde_json::Value> = serde_wasm_bindgen::from_value(result).unwrap();
        assert!(!tokens.is_empty());
    }

    #[wasm_bindgen_test]
    fn test_tokenize_with_identifiers() {
        let source = "x = 42";
        let result = tokenize(source).unwrap();
        let tokens: Vec<serde_json::Value> = serde_wasm_bindgen::from_value(result).unwrap();
        assert!(!tokens.is_empty());
    }

    #[wasm_bindgen_test]
    fn test_tokenize_complex_expression() {
        let source = "[1, 2, 3] + [4, 5, 6]";
        let result = tokenize(source).unwrap();
        let tokens: Vec<serde_json::Value> = serde_wasm_bindgen::from_value(result).unwrap();
        assert!(!tokens.is_empty());
    }

    // ============================================================================
    // get_built_in_function_names tests
    // ============================================================================

    #[wasm_bindgen_test]
    fn test_get_built_in_function_names() {
        let result = get_built_in_function_names().unwrap();
        let names: Vec<String> = serde_wasm_bindgen::from_value(result).unwrap();
        assert!(!names.is_empty());
        assert!(names.contains(&"map".to_string()));
        assert!(names.contains(&"filter".to_string()));
        assert!(names.contains(&"reduce".to_string()));
        assert!(names.contains(&"sum".to_string()));
    }

    // ============================================================================
    // get_constants tests
    // ============================================================================

    #[wasm_bindgen_test]
    fn test_get_constants() {
        let result = get_constants().unwrap();
        let constants: serde_json::Value = serde_wasm_bindgen::from_value(result).unwrap();
        assert!(constants["constants"].is_object());
        let const_obj = &constants["constants"]["Record"];
        assert!(const_obj["pi"]["Number"].is_number());
        assert!(const_obj["e"]["Number"].is_number());
    }

    // ============================================================================
    // evaluate_inline_expressions tests
    // ============================================================================

    #[wasm_bindgen_test]
    fn test_evaluate_inline_expressions_single() {
        let expressions = serde_wasm_bindgen::to_value(&vec!["1 + 2"]).unwrap();
        let inputs = serde_wasm_bindgen::to_value(&serde_json::json!({})).unwrap();
        let result = evaluate_inline_expressions(expressions, inputs).unwrap();
        let results: Vec<serde_json::Value> = serde_wasm_bindgen::from_value(result).unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0]["value"]["Number"], serde_json::json!(3));
    }

    #[wasm_bindgen_test]
    fn test_evaluate_inline_expressions_multiple() {
        let expressions = serde_wasm_bindgen::to_value(&vec!["x * 2", "x + 5", "x - 1"]).unwrap();
        // Create inputs using SerializableValue directly
        use indexmap::IndexMap;
        let mut inputs_map: IndexMap<String, SerializableValue> = IndexMap::new();
        inputs_map.insert("x".to_string(), SerializableValue::Number(10.0));
        let inputs = serde_wasm_bindgen::to_value(&inputs_map).unwrap();

        let result = evaluate_inline_expressions(expressions, inputs).unwrap();
        let results: Vec<serde_json::Value> = serde_wasm_bindgen::from_value(result).unwrap();
        assert_eq!(results.len(), 3);
        assert_eq!(results[0]["value"]["Number"], serde_json::json!(20));
        assert_eq!(results[1]["value"]["Number"], serde_json::json!(15));
        assert_eq!(results[2]["value"]["Number"], serde_json::json!(9));
    }

    #[wasm_bindgen_test]
    fn test_evaluate_inline_expressions_with_error() {
        let expressions = serde_wasm_bindgen::to_value(&vec!["1 + 2", "invalid +"]).unwrap();
        let inputs = serde_wasm_bindgen::to_value(&serde_json::json!({})).unwrap();
        let result = evaluate_inline_expressions(expressions, inputs).unwrap();
        let results: Vec<serde_json::Value> = serde_wasm_bindgen::from_value(result).unwrap();
        assert_eq!(results.len(), 2);
        assert_eq!(results[0]["value"]["Number"], serde_json::json!(3));
        assert!(results[1]["error"].is_string());
    }

    #[wasm_bindgen_test]
    fn test_evaluate_inline_expressions_with_inputs() {
        let expressions = serde_wasm_bindgen::to_value(&vec!["\"Hello, \" + name", "age >= 18"]).unwrap();
        // Create inputs using SerializableValue directly
        use indexmap::IndexMap;
        let mut inputs_map: IndexMap<String, SerializableValue> = IndexMap::new();
        inputs_map.insert("name".to_string(), SerializableValue::String("Alice".to_string()));
        inputs_map.insert("age".to_string(), SerializableValue::Number(30.0));
        let inputs = serde_wasm_bindgen::to_value(&inputs_map).unwrap();

        let result = evaluate_inline_expressions(expressions, inputs).unwrap();
        let results: Vec<serde_json::Value> = serde_wasm_bindgen::from_value(result).unwrap();
        assert_eq!(results.len(), 2);
        assert_eq!(results[0]["value"]["String"], serde_json::json!("Hello, Alice"));
        assert_eq!(results[1]["value"]["Bool"], serde_json::json!(true));
    }
}
