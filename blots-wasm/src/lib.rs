use anyhow::Result;
use blots_core::{
    ast::{Expr, Spanned},
    environment::Environment,
    error::RuntimeError,
    expressions::{evaluate_pairs, pairs_to_expr_with_comments},
    formatter::{format_expr, join_statements_with_spacing},
    functions::get_built_in_function_idents,
    heap::{CONSTANTS, Heap},
    parser::{Rule, Token, get_pairs, get_tokens},
    values::SerializableValue,
};
use indexmap::{IndexMap, IndexSet};
use pest::error::InputLocation;
use serde::{Deserialize, Serialize};
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use string_offsets::StringOffsets;
use wasm_bindgen::prelude::*;

#[cfg(test)]
mod wasm_tests;

#[derive(Debug, Serialize, Deserialize)]
struct EvaluationResult {
    values: HashMap<String, SerializableValue>,
    bindings: HashMap<String, SerializableValue>,
    outputs: IndexSet<String>,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(untagged)]
enum EvaluateResponse {
    Success { result: EvaluationResult },
    Error { error: ErrorInfo },
}

#[derive(Debug, Serialize, Deserialize)]
struct ErrorInfo {
    message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    range: Option<ErrorRange>,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(untagged)]
enum ErrorRange {
    Span { start: usize, end: usize },
    Pos { pos: usize },
}

/// Extract error range from pest parsing error, converting UTF-8 byte positions to UTF-16 offsets
fn extract_pest_error_range(
    error: &pest::error::Error<Rule>,
    offsets: &StringOffsets,
) -> Option<ErrorRange> {
    match &error.location {
        InputLocation::Pos(pos) => Some(ErrorRange::Pos {
            pos: offsets.utf8_to_utf16(*pos),
        }),
        InputLocation::Span((start, end)) => Some(ErrorRange::Span {
            start: offsets.utf8_to_utf16(*start),
            end: offsets.utf8_to_utf16(*end),
        }),
    }
}

/// Extract error range from RuntimeError, converting UTF-8 byte positions to UTF-16 offsets
fn extract_runtime_error_range(
    error: &RuntimeError,
    offsets: &StringOffsets,
) -> Option<ErrorRange> {
    error.span.as_ref().map(|span| {
        // Convert UTF-8 byte positions to UTF-16 offsets
        if span.end_byte != span.start_byte {
            ErrorRange::Span {
                start: offsets.utf8_to_utf16(span.start_byte),
                end: offsets.utf8_to_utf16(span.end_byte),
            }
        } else {
            // Single position
            ErrorRange::Pos {
                pos: offsets.utf8_to_utf16(span.start_byte),
            }
        }
    })
}

#[wasm_bindgen]
pub fn evaluate(expr: &str, inputs_js: JsValue) -> Result<JsValue, JsError> {
    let expr_owned = String::from(expr);

    // Parse inputs
    let inputs_given: IndexMap<String, SerializableValue> =
        match serde_wasm_bindgen::from_value(inputs_js) {
            Ok(v) => v,
            Err(e) => {
                let serializer = serde_wasm_bindgen::Serializer::json_compatible();
                return Ok(EvaluateResponse::Error {
                    error: ErrorInfo {
                        message: format!("Invalid inputs: {}", e),
                        range: None,
                    },
                }
                .serialize(&serializer)?);
            }
        };

    let heap = Rc::new(RefCell::new(Heap::new()));
    let inputs = match inputs_given
        .into_iter()
        .map(|(key, value)| {
            value
                .to_value(&mut heap.borrow_mut())
                .map(|v| (key.clone(), v))
                .map_err(|e| RuntimeError::new(format!("Failed to convert input '{}': {}", key, e)))
        })
        .collect::<Result<IndexMap<String, _>, RuntimeError>>()
    {
        Ok(inputs) => inputs,
        Err(e) => {
            let serializer = serde_wasm_bindgen::Serializer::json_compatible();
            let offsets = StringOffsets::new(&expr_owned);
            return Ok(EvaluateResponse::Error {
                error: ErrorInfo {
                    message: e.message.clone(),
                    range: extract_runtime_error_range(&e, &offsets),
                },
            }
            .serialize(&serializer)?);
        }
    };

    let bindings = Rc::new(Environment::new());
    bindings.insert(
        String::from("inputs"),
        heap.borrow_mut().insert_record(inputs),
    );

    // Parse the expression
    let pairs = match get_pairs(&expr_owned) {
        Ok(pairs) => pairs,
        Err(e) => {
            let serializer = serde_wasm_bindgen::Serializer::json_compatible();
            let offsets = StringOffsets::new(&expr_owned);
            return Ok(EvaluateResponse::Error {
                error: ErrorInfo {
                    message: format!("Parsing error: {}", e),
                    range: extract_pest_error_range(&e, &offsets),
                },
            }
            .serialize(&serializer)?);
        }
    };

    let mut outputs = IndexSet::new();
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

                    let value = match evaluate_pairs(
                        inner_pairs,
                        Rc::clone(&heap),
                        Rc::clone(&bindings),
                        0,
                        &expr_owned,
                    ) {
                        Ok(v) => v,
                        Err(e) => {
                            let serializer = serde_wasm_bindgen::Serializer::json_compatible();
                            let offsets = StringOffsets::new(&expr_owned);
                            return Ok(EvaluateResponse::Error {
                                error: ErrorInfo {
                                    message: e.message.clone(),
                                    range: extract_runtime_error_range(&e, &offsets),
                                },
                            }
                            .serialize(&serializer)?);
                        }
                    };

                    values.insert(col_id, value);
                }
            }
            Rule::EOI => {}
            rule => unreachable!("unexpected rule: {:?}", rule),
        }
    }

    let values_serializable = values
        .iter()
        .map(|(k, v)| {
            v.to_serializable_value(&heap.borrow())
                .map(|sv| (k.clone(), sv))
                .map_err(|e| RuntimeError::new(format!("Serialization error for '{}': {}", k, e)))
        })
        .collect::<Result<HashMap<String, _>, RuntimeError>>();

    let values_serializable = match values_serializable {
        Ok(v) => v,
        Err(e) => {
            let serializer = serde_wasm_bindgen::Serializer::json_compatible();
            let offsets = StringOffsets::new(&expr_owned);
            return Ok(EvaluateResponse::Error {
                error: ErrorInfo {
                    message: e.message.clone(),
                    range: extract_runtime_error_range(&e, &offsets),
                },
            }
            .serialize(&serializer)?);
        }
    };

    let bindings_serializable = bindings
        .iter()
        .map(|(k, v)| {
            v.to_serializable_value(&heap.borrow())
                .map(|sv| (k.clone(), sv))
                .map_err(|e| {
                    RuntimeError::new(format!("Serialization error for binding '{}': {}", k, e))
                })
        })
        .collect::<Result<HashMap<String, _>, RuntimeError>>();

    let bindings_serializable = match bindings_serializable {
        Ok(v) => v,
        Err(e) => {
            let serializer = serde_wasm_bindgen::Serializer::json_compatible();
            let offsets = StringOffsets::new(&expr_owned);
            return Ok(EvaluateResponse::Error {
                error: ErrorInfo {
                    message: e.message.clone(),
                    range: extract_runtime_error_range(&e, &offsets),
                },
            }
            .serialize(&serializer)?);
        }
    };

    // Use json_compatible serializer to ensure Records are serialized as JSON objects
    // instead of JavaScript Maps
    let serializer = serde_wasm_bindgen::Serializer::json_compatible();
    Ok(EvaluateResponse::Success {
        result: EvaluationResult {
            values: values_serializable,
            bindings: bindings_serializable,
            outputs,
        },
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

                    match evaluate_pairs(
                        inner_pairs,
                        Rc::clone(&heap),
                        Rc::clone(&bindings),
                        0,
                        expr,
                    ) {
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
                        let inner_expr = pairs_to_expr_with_comments(first_pair.into_inner())
                            .map_err(|e| JsError::new(&format!("AST conversion error: {}", e)))?;
                        let output_expr = Spanned::dummy(Expr::Output {
                            expr: Box::new(inner_expr),
                        });
                        format_expr(&output_expr, max_columns)
                    }
                    _ => {
                        // Format as expression
                        let expr = pairs_to_expr_with_comments(first_pair.into_inner())
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
