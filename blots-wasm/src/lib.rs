use anyhow::Result;
use blots_core::{
    functions::get_built_in_function_idents,
    heap::CONSTANTS,
    parser::{get_tokens, Token},
    transpiler::{transpile_to_js, transpile_to_js_with_inline_eval},
    values::{LambdaArg, PrimitiveValue, SerializableLambdaDef, SerializableValue},
};
use js_sys::{eval, Array, Object, Reflect};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, HashSet};
use wasm_bindgen::prelude::*;

#[derive(Debug)]
struct EvaluationResult {
    values: HashMap<String, JsValue>,
    bindings: HashMap<String, JsValue>,
    outputs: HashSet<String>,
}

fn parse_js_function_to_lambda(js_func: &JsValue) -> Result<SerializableLambdaDef, JsError> {
    // Get the function source code by calling toString() on the function
    let func_str = if let Some(func) = js_func.dyn_ref::<js_sys::Function>() {
        func.to_string()
            .as_string()
            .unwrap_or_else(|| format!("{:?}", js_func))
    } else {
        js_func
            .as_string()
            .unwrap_or_else(|| format!("{:?}", js_func))
    };

    // Parse arrow function: (params) => body
    if let Some(arrow_pos) = func_str.find("=>") {
        let params_part = func_str[..arrow_pos].trim();
        let body_part = func_str[arrow_pos + 2..].trim();

        // Parse parameters
        let args = parse_function_parameters(params_part)?;

        // For the body, we'll store the original JavaScript code
        // This maintains the function's behavior when reconstructed
        let body = body_part.to_string();

        Ok(SerializableLambdaDef {
            name: None, // Arrow functions are typically anonymous
            args,
            body,
            scope: None, // We don't capture scope for now
        })
    } else {
        // Fallback: treat as anonymous function with unknown parameters
        Ok(SerializableLambdaDef {
            name: None,
            args: vec![], // Unknown parameters
            body: func_str,
            scope: None,
        })
    }
}

fn parse_function_parameters(params_str: &str) -> Result<Vec<LambdaArg>, JsError> {
    let params_str = params_str.trim();

    // Handle single parameter without parentheses: x => ...
    if !params_str.starts_with('(') {
        let param_name = params_str.trim();
        if param_name.is_empty() {
            return Ok(vec![]);
        }
        return Ok(vec![LambdaArg::Required(param_name.to_string())]);
    }

    // Handle parenthesized parameters: (x, y, z) => ...
    let params_content = params_str
        .strip_prefix('(')
        .and_then(|s| s.strip_suffix(')'))
        .unwrap_or(params_str)
        .trim();

    if params_content.is_empty() {
        return Ok(vec![]);
    }

    let mut args = Vec::new();
    for param in params_content.split(',') {
        let param = param.trim();

        if param.starts_with("...") {
            // Rest parameter: ...args
            let name = param[3..].trim().to_string();
            args.push(LambdaArg::Rest(name));
        } else if param.contains('=') {
            // Optional parameter: x = undefined
            let name = param.split('=').next().unwrap().trim().to_string();
            args.push(LambdaArg::Optional(name));
        } else {
            // Required parameter: x
            args.push(LambdaArg::Required(param.to_string()));
        }
    }

    Ok(args)
}

fn js_value_to_serializable_value(value: &JsValue) -> Result<SerializableValue, JsError> {
    // Handle null and undefined
    if value.is_null() || value.is_undefined() {
        return Ok(SerializableValue::Null);
    }

    // Handle booleans
    if let Some(bool_val) = value.as_bool() {
        return Ok(SerializableValue::Bool(bool_val));
    }

    // Handle numbers
    if let Some(num_val) = value.as_f64() {
        return Ok(SerializableValue::Number(num_val));
    }

    // Handle strings
    if let Some(str_val) = value.as_string() {
        return Ok(SerializableValue::String(str_val));
    }

    // Handle arrays (lists)
    if Array::is_array(value) {
        let array = Array::from(value);
        let mut list_items = Vec::new();

        for i in 0..array.length() {
            let item = array.get(i);
            list_items.push(js_value_to_serializable_value(&item)?);
        }

        return Ok(SerializableValue::List(list_items));
    }

    // Handle functions
    if value.is_function() {
        // Convert JavaScript function to Lambda representation
        let lambda_def = parse_js_function_to_lambda(value)?;
        return Ok(SerializableValue::Lambda(lambda_def));
    }

    // Handle objects (records)
    if value.is_object() {
        let obj = Object::from(value.clone());
        let keys = Object::keys(&obj);
        let mut record_map = BTreeMap::new();

        for i in 0..keys.length() {
            if let Some(key) = keys.get(i).as_string() {
                if let Ok(prop_value) = Reflect::get(&obj, &keys.get(i)) {
                    record_map.insert(key, js_value_to_serializable_value(&prop_value)?);
                }
            }
        }

        return Ok(SerializableValue::Record(record_map));
    }

    // Fallback: convert to string representation
    Ok(SerializableValue::String(format!("{:?}", value)))
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
        // No inline evaluation results, return empty result with SerializableValue format
        let result_obj = Object::new();
        let empty_values: BTreeMap<String, SerializableValue> = BTreeMap::new();
        let empty_bindings: BTreeMap<String, SerializableValue> = BTreeMap::new();

        let values_js = serde_wasm_bindgen::to_value(&empty_values)
            .map_err(|e| JsError::new(&format!("Failed to serialize empty values: {:?}", e)))?;
        let bindings_js = serde_wasm_bindgen::to_value(&empty_bindings)
            .map_err(|e| JsError::new(&format!("Failed to serialize empty bindings: {:?}", e)))?;

        Reflect::set(&result_obj, &"values".into(), &values_js)
            .map_err(|e| JsError::new(&format!("Failed to set empty values: {:?}", e)))?;
        Reflect::set(&result_obj, &"bindings".into(), &bindings_js)
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

    // Convert JS values to SerializableValue format for compatibility
    let mut values = BTreeMap::new();
    let mut bindings = BTreeMap::new();
    let mut outputs: HashSet<String> = HashSet::new();

    // Convert values object to HashMap with SerializableValue
    if values_obj.is_object() {
        let obj = Object::from(values_obj);
        let keys = Object::keys(&obj);
        for i in 0..keys.length() {
            if let Some(key) = keys.get(i).as_string() {
                if let Ok(js_value) = Reflect::get(&obj, &keys.get(i)) {
                    let serializable_value = js_value_to_serializable_value(&js_value)?;
                    values.insert(key, serializable_value);
                }
            }
        }
    }

    // Convert bindings object to HashMap with SerializableValue
    if bindings_obj.is_object() {
        let obj = Object::from(bindings_obj);
        let keys = Object::keys(&obj);
        for i in 0..keys.length() {
            if let Some(key) = keys.get(i).as_string() {
                if let Ok(js_value) = Reflect::get(&obj, &keys.get(i)) {
                    let serializable_value = js_value_to_serializable_value(&js_value)?;
                    bindings.insert(key, serializable_value);
                }
            }
        }
    }

    // Convert outputs Set to HashSet
    if outputs_obj.is_object() {
        // For now, skip outputs processing - would need more complex Set handling
    }

    // Return the values and bindings as serialized objects in the expected format
    let result_obj = Object::new();

    // Serialize values and bindings to JsValue
    let values_js = serde_wasm_bindgen::to_value(&values)
        .map_err(|e| JsError::new(&format!("Failed to serialize values: {:?}", e)))?;
    let bindings_js = serde_wasm_bindgen::to_value(&bindings)
        .map_err(|e| JsError::new(&format!("Failed to serialize bindings: {:?}", e)))?;

    // Create outputs array (simplified)
    let outputs_array = js_sys::Array::new();
    for output in outputs {
        outputs_array.push(&output.into());
    }

    Reflect::set(&result_obj, &"values".into(), &values_js)
        .map_err(|e| JsError::new(&format!("Failed to set values: {:?}", e)))?;
    Reflect::set(&result_obj, &"bindings".into(), &bindings_js)
        .map_err(|e| JsError::new(&format!("Failed to set bindings: {:?}", e)))?;
    Reflect::set(&result_obj, &"outputs".into(), &outputs_array)
        .map_err(|e| JsError::new(&format!("Failed to set outputs array: {:?}", e)))?;

    Ok(result_obj.into())
}

#[wasm_bindgen]
pub fn transpile(expr: &str) -> Result<String, JsError> {
    transpile_to_js(expr).map_err(|e| JsError::new(&format!("Transpilation error: {}", e)))
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
    // Convert constants to SerializableValue format for consistency
    let mut constants_map = BTreeMap::new();

    for (key, value) in CONSTANTS.clone() {
        let serializable_value = match value {
            PrimitiveValue::Number(n) => SerializableValue::Number(n),
            PrimitiveValue::Bool(b) => SerializableValue::Bool(b),
            PrimitiveValue::Null => SerializableValue::Null,
        };

        constants_map.insert(key, serializable_value);
    }

    // Serialize to JsValue for return
    serde_wasm_bindgen::to_value(&constants_map)
        .map_err(|e| JsError::new(&format!("Failed to serialize constants: {:?}", e)))
}
