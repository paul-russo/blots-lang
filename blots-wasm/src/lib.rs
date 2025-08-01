use anyhow::Result;
use blots_core::{
    expressions::evaluate_expression,
    formatter::Formatter,
    functions::get_built_in_function_idents,
    heap::{Heap, CONSTANTS},
    parser::{get_pairs, get_tokens, Rule, Token},
    values::SerializableValue,
};
use serde::{Deserialize, Serialize};
use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap, HashSet},
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
    let inputs_given: BTreeMap<String, SerializableValue> =
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
                        evaluate_expression(inner_pairs, Rc::clone(&heap), Rc::clone(&bindings), 0)
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

    Ok(serde_wasm_bindgen::to_value(&EvaluationResult {
        values: values_serializable,
        bindings: bindings_serializable,
        outputs,
    })?)
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
        &get_built_in_function_idents(),
    )?)
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

    Ok(serde_wasm_bindgen::to_value(&map)?)
}

#[wasm_bindgen]
pub fn format(code: &str) -> Result<String, JsError> {
    Formatter::format_preserving_comments(code)
        .map_err(|e| JsError::new(&format!("Formatting error: {}", e)))
}
