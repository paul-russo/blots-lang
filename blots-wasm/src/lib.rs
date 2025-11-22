use anyhow::Result;
use blots_core::{
    expressions::{evaluate_pairs, pairs_to_expr},
    formatter::format_expr,
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

    let bindings = Rc::new(RefCell::new(HashMap::new()));
    {
        bindings.borrow_mut().insert(
            String::from("inputs"),
            heap.borrow_mut().insert_record(inputs),
        );
    }

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
        .borrow()
        .iter()
        .map(|(k, v)| (k.clone(), v.to_serializable_value(&heap.borrow()).unwrap()))
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

    let bindings = Rc::new(RefCell::new(HashMap::new()));

    // Add inputs record
    {
        bindings.borrow_mut().insert(
            String::from("inputs"),
            heap.borrow_mut().insert_record(inputs.clone()),
        );
    }

    // Inject all input values directly into bindings for inline access
    for (key, value) in inputs {
        bindings.borrow_mut().insert(key, value);
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
            if let Some(inner_pair) = pair.into_inner().next() {
                match inner_pair.as_rule() {
                    Rule::comment => {
                        // Preserve comments as-is
                        formatted_statements.push(inner_pair.as_str().to_string());
                    }
                    _ => {
                        // Format as expression
                        let expr = pairs_to_expr(inner_pair.into_inner())
                            .map_err(|e| JsError::new(&format!("AST conversion error: {}", e)))?;
                        let formatted = format_expr(&expr, max_columns);
                        formatted_statements.push(formatted);
                    }
                }
            }
        }
    }

    if formatted_statements.is_empty() {
        return Err(JsError::new("No statements found in source"));
    }

    // Join all formatted statements with newlines
    let result = formatted_statements.join("\n");

    // Return the formatted string
    let serializer = serde_wasm_bindgen::Serializer::json_compatible();
    Ok(result.serialize(&serializer)?)
}
