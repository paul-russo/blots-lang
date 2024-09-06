use core::{
    expressions::evaluate_expression,
    parser::{get_pairs, Rule},
};
use std::collections::HashMap;

use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;

#[derive(Debug, Serialize, Deserialize)]
struct EvluationResult {
    value: f64,
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
                    let result = EvluationResult {
                        value,
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
                    let result = EvluationResult {
                        value,
                        variables: variables.clone(),
                    };
                    Ok(serde_wasm_bindgen::to_value(&result)?)
                }
                Err(error) => Err(JsError::new(&format!("Evaluation error: {}", error))),
            }
        }
        _ => unreachable!(),
    }
}
