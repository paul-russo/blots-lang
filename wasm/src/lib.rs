use mir_core::{
    expressions::evaluate_expression,
    functions::{is_built_in_function, UserDefinedFunctionDef, BUILT_IN_FUNCTION_IDENTS},
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
    function_defs: HashMap<String, UserDefinedFunctionDef>,
}

#[wasm_bindgen]
pub fn evaluate(
    expr: &str,
    variables_js: JsValue,
    function_defs_js: JsValue,
) -> Result<JsValue, JsError> {
    let mut variables = serde_wasm_bindgen::from_value(variables_js)?;
    let mut function_defs: HashMap<String, UserDefinedFunctionDef> =
        serde_wasm_bindgen::from_value(function_defs_js)?;

    let expr_owned = String::from(expr);
    let pairs = get_pairs(&expr_owned);
    let outer_pair = pairs
        .map_err(|e| JsError::new(&format!("Parsing error: {}", e)))?
        .next()
        .unwrap();

    match outer_pair.as_rule() {
        Rule::assignment => {
            let mut inner_pairs = outer_pair.into_inner();
            let ident = inner_pairs.next().unwrap().as_str();
            let expression = inner_pairs.next().unwrap();
            let result = evaluate_expression(expression.into_inner(), &variables, &function_defs);

            match result {
                Ok(value) => {
                    variables.insert(ident.to_string(), value.clone());

                    let result = EvaluationResult {
                        value: Some(value),
                        variables: variables.clone(),
                        function_defs: function_defs.clone(),
                    };
                    Ok(serde_wasm_bindgen::to_value(&result)?)
                }
                Err(error) => Err(JsError::new(&format!("Evaluation error: {}", error))),
            }
        }
        Rule::expression => {
            let result = evaluate_expression(outer_pair.into_inner(), &variables, &function_defs);

            match result {
                Ok(value) => {
                    let result = EvaluationResult {
                        value: Some(value),
                        variables: variables.clone(),
                        function_defs: function_defs.clone(),
                    };
                    Ok(serde_wasm_bindgen::to_value(&result)?)
                }
                Err(error) => Err(JsError::new(&format!("Evaluation error: {}", error))),
            }
        }
        Rule::function_definition => {
            let mut inner_pairs = outer_pair.into_inner();
            let ident = inner_pairs.next().unwrap().as_str();
            let args = inner_pairs.next().unwrap().into_inner();
            let body = inner_pairs.next().unwrap().as_str().trim();
            let args = args.map(|arg| arg.as_str().to_string()).collect();

            if is_built_in_function(ident) {
                return Err(JsError::new(&format!(
                    "Error: {} is a built-in function and cannot be redefined",
                    ident
                )));
            }

            function_defs.insert(
                ident.to_string(),
                UserDefinedFunctionDef {
                    name: ident.to_string(),
                    args,
                    body: body.to_string(),
                },
            );

            let result = EvaluationResult {
                value: None,
                variables: variables.clone(),
                function_defs: function_defs.clone(),
            };

            Ok(serde_wasm_bindgen::to_value(&result)?)
        }
        Rule::comment => {
            let result = EvaluationResult {
                value: None,
                variables: variables.clone(),
                function_defs: function_defs.clone(),
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
