use anyhow::Result;
use blots_core::{
    functions::get_built_in_function_idents,
    heap::CONSTANTS,
    parser::{get_tokens, Token},
    transpiler::{transpile_to_js, transpile_to_js_with_inline_eval},
    values::PrimitiveValue,
};
use js_sys::{eval, Object, Reflect};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use wasm_bindgen::prelude::*;

#[derive(Debug)]
struct EvaluationResult {
    values: HashMap<String, JsValue>,
    bindings: HashMap<String, JsValue>,
    outputs: HashSet<String>,
}

#[wasm_bindgen]
pub fn evaluate(expr: &str, inputs_js: JsValue) -> Result<JsValue, JsError> {
    // Transpile Blots code to JavaScript with inline evaluation
    let js_code = transpile_to_js_with_inline_eval(expr)
        .map_err(|e| JsError::new(&format!("Transpilation error: {}", e)))?;

    // Set up inputs in global scope if provided
    if !inputs_js.is_undefined() && !inputs_js.is_null() {
        let global = js_sys::global();
        Reflect::set(&global, &"inputs".into(), &inputs_js)
            .map_err(|e| JsError::new(&format!("Failed to set inputs: {:?}", e)))?;
    }

    // Execute the transpiled JavaScript
    let result = eval(&js_code)
        .map_err(|e| JsError::new(&format!("JavaScript execution error: {:?}", e)))?;

    // The transpiled code with inline evaluation returns $$results object
    // Extract the results from the global scope
    let global = js_sys::global();
    let results_obj = Reflect::get(&global, &"$$results".into())
        .map_err(|e| JsError::new(&format!("Failed to get results: {:?}", e)))?;

    if results_obj.is_undefined() {
        // No inline evaluation results, return empty result
        let result_obj = Object::new();
        Reflect::set(&result_obj, &"values".into(), &js_sys::Map::new())
            .map_err(|e| JsError::new(&format!("Failed to set empty values: {:?}", e)))?;
        Reflect::set(&result_obj, &"bindings".into(), &js_sys::Map::new())
            .map_err(|e| JsError::new(&format!("Failed to set empty bindings: {:?}", e)))?;
        Reflect::set(&result_obj, &"outputs".into(), &js_sys::Array::new())
            .map_err(|e| JsError::new(&format!("Failed to set empty outputs: {:?}", e)))?;
        return Ok(result_obj.into());
    }

    // Extract values, bindings, and outputs from the results object
    let values_obj = Reflect::get(&results_obj, &"values".into())
        .map_err(|e| JsError::new(&format!("Failed to get values: {:?}", e)))?;
    let bindings_obj = Reflect::get(&results_obj, &"bindings".into())
        .map_err(|e| JsError::new(&format!("Failed to get bindings: {:?}", e)))?;
    let outputs_obj = Reflect::get(&results_obj, &"outputs".into())
        .map_err(|e| JsError::new(&format!("Failed to get outputs: {:?}", e)))?;

    // Convert to HashMap - for now, keep values as JsValue
    let mut values = HashMap::new();
    let mut bindings = HashMap::new();
    let mut outputs: HashSet<String> = HashSet::new();

    // Convert values object to HashMap
    if values_obj.is_object() {
        let obj = Object::from(values_obj);
        let keys = Object::keys(&obj);
        for i in 0..keys.length() {
            if let Some(key) = keys.get(i).as_string() {
                if let Ok(value) = Reflect::get(&obj, &keys.get(i)) {
                    values.insert(key, value);
                }
            }
        }
    }

    // Convert bindings object to HashMap  
    if bindings_obj.is_object() {
        let obj = Object::from(bindings_obj);
        let keys = Object::keys(&obj);
        for i in 0..keys.length() {
            if let Some(key) = keys.get(i).as_string() {
                if let Ok(value) = Reflect::get(&obj, &keys.get(i)) {
                    bindings.insert(key, value);
                }
            }
        }
    }

    // Convert outputs Set to HashSet
    if outputs_obj.is_object() {
        // For now, skip outputs processing - would need more complex Set handling
    }

    // Create result object manually
    let result_obj = Object::new();
    
    // Create values Map
    let values_map = js_sys::Map::new();
    for (key, value) in values {
        values_map.set(&key.into(), &value);
    }
    
    // Create bindings Map
    let bindings_map = js_sys::Map::new();
    for (key, value) in bindings {
        bindings_map.set(&key.into(), &value);
    }
    
    // Create outputs array (simplified)
    let outputs_array = js_sys::Array::new();
    for output in outputs {
        outputs_array.push(&output.into());
    }
    
    Reflect::set(&result_obj, &"values".into(), &values_map)
        .map_err(|e| JsError::new(&format!("Failed to set values map: {:?}", e)))?;
    Reflect::set(&result_obj, &"bindings".into(), &bindings_map)
        .map_err(|e| JsError::new(&format!("Failed to set bindings map: {:?}", e)))?;
    Reflect::set(&result_obj, &"outputs".into(), &outputs_array)
        .map_err(|e| JsError::new(&format!("Failed to set outputs array: {:?}", e)))?;
    
    Ok(result_obj.into())
}

#[wasm_bindgen]
pub fn transpile(expr: &str) -> Result<String, JsError> {
    transpile_to_js(expr)
        .map_err(|e| JsError::new(&format!("Transpilation error: {}", e)))
}

#[wasm_bindgen]
pub fn transpile_with_inline_eval(expr: &str) -> Result<String, JsError> {
    transpile_to_js_with_inline_eval(expr)
        .map_err(|e| JsError::new(&format!("Transpilation error: {}", e)))
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
    // Convert constants to a simple JavaScript object
    let constants_obj = js_sys::Object::new();
    
    for (key, value) in CONSTANTS.clone() {
        let js_value = match value {
            PrimitiveValue::Number(n) => JsValue::from(n),
            PrimitiveValue::Bool(b) => JsValue::from(b),
            PrimitiveValue::Null => JsValue::null(),
        };
        
        Reflect::set(&constants_obj, &key.into(), &js_value)
            .map_err(|e| JsError::new(&format!("Failed to set constant: {:?}", e)))?;
    }

    Ok(constants_obj.into())
}
