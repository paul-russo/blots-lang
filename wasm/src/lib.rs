use mir_core::{
    expressions::evaluate_expression,
    functions::BUILT_IN_FUNCTION_IDENTS,
    parser::{get_pairs, get_tokens, Rule, Token},
    values::Value,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use wasm_bindgen::prelude::*;

#[derive(Debug, Serialize, Deserialize)]
struct EvaluationResult {
    value: Option<Value>,
    variables: HashMap<String, Value>,
}

#[wasm_bindgen]
pub fn evaluate(expr: &str, variables_js: JsValue) -> Result<JsValue, JsError> {
    let mut variables = serde_wasm_bindgen::from_value(variables_js)?;

    let expr_owned = String::from(expr);
    let pairs = get_pairs(&expr_owned);
    let outer_pair = pairs
        .map_err(|e| JsError::new(&format!("Parsing error: {}", e)))?
        .next()
        .unwrap();

    match outer_pair.as_rule() {
        Rule::expression => {
            let result = evaluate_expression(outer_pair.into_inner(), &mut variables);

            match result {
                Ok(value) => {
                    let result = EvaluationResult {
                        value: Some(value),
                        variables: variables.clone(),
                    };
                    Ok(serde_wasm_bindgen::to_value(&result)?)
                }
                Err(error) => Err(JsError::new(&format!("Evaluation error: {}", error))),
            }
        }
        Rule::comment => {
            let result = EvaluationResult {
                value: None,
                variables: variables.clone(),
            };
            Ok(serde_wasm_bindgen::to_value(&result)?)
        }
        _ => unreachable!(),
    }
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

    Ok(serde_wasm_bindgen::to_value(&tokens)?)
}

#[wasm_bindgen]
pub fn get_built_in_function_names() -> Result<JsValue, JsError> {
    Ok(serde_wasm_bindgen::to_value(
        &(BUILT_IN_FUNCTION_IDENTS
            .iter()
            .cloned()
            .collect::<Vec<&str>>()),
    )?)
}

#[wasm_bindgen]
pub fn get_constants() -> Result<JsValue, JsError> {
    let mut constants = HashMap::new();
    constants.insert(format!("pi"), core::f64::consts::PI);
    constants.insert(format!("e"), core::f64::consts::E);
    constants.insert(format!("infinity"), f64::INFINITY);

    Ok(serde_wasm_bindgen::to_value(&constants)?)
}
