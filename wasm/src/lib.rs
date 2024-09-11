mod log;

use core::{
    expressions::evaluate_expression,
    parser::{get_pairs, get_tokens, Rule, Token},
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    // Use `js_namespace` here to bind `console.log(..)` instead of just
    // `log(..)`
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

#[derive(Debug, Serialize, Deserialize)]
struct EvaluationResult {
    value: Option<f64>,
    variables: HashMap<String, f64>,
}

#[wasm_bindgen]
pub fn evaluate(expr: &str, variables: JsValue) -> Result<JsValue, JsError> {
    let mut variables = serde_wasm_bindgen::from_value(variables)?;
    let expr = expr.to_string();
    let pairs = get_pairs(&expr);
    let outer_pair = pairs
        .map_err(|e| JsError::new(&format!("Parsing error: {}", e)))?
        .next()
        .unwrap();

    match outer_pair.as_rule() {
        Rule::assignment => {
            let mut inner_pairs = outer_pair.into_inner();
            let ident = inner_pairs.next().unwrap().as_str();
            let expression = inner_pairs.next().unwrap();
            let result = evaluate_expression(expression.into_inner(), &variables);

            match result {
                Ok(value) => {
                    variables.insert(ident.to_string(), value);
                    let result = EvaluationResult {
                        value: Some(value),
                        variables: variables.clone(),
                    };
                    Ok(serde_wasm_bindgen::to_value(&result)?)
                }
                Err(error) => Err(JsError::new(&format!("Evaluation error: {}", error))),
            }
        }
        Rule::expression => {
            let result = evaluate_expression(outer_pair.into_inner(), &variables);

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
