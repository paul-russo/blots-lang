use anyhow::Result;
use blots_core::{
    functions::get_built_in_function_idents,
    heap::CONSTANTS,
    parser::{get_tokens, Token},
    source_map::SourceMap,
    transpiler::{
        translate_js_error, translate_js_identifiers, transpile_to_js,
        transpile_to_js_with_inline_eval, transpile_to_js_with_source_map,
        transpile_to_js_with_inline_eval_and_source_map,
    },
    values::{LambdaArg, PrimitiveValue, SerializableLambdaDef, SerializableValue},
};
use js_sys::{eval, Array, Object, Reflect, Set};
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
    // Check if the function has an $$originalSource property
    if let Some(func) = js_func.dyn_ref::<js_sys::Function>() {
        if let Ok(original_source) = Reflect::get(func, &"$$originalSource".into()) {
            if let Some(original_str) = original_source.as_string() {
                // Parse the original Blots source
                if let Some(arrow_pos) = original_str.find("=>") {
                    let params_part = original_str[..arrow_pos].trim();
                    let body_part = original_str[arrow_pos + 2..].trim();

                    // Parse parameters
                    let args = parse_function_parameters(params_part)?;

                    return Ok(SerializableLambdaDef {
                        name: None,
                        args,
                        body: body_part.to_string(), // Use original Blots body
                        scope: None,
                    });
                }
            }
        }
    }

    // Fallback: use the transpiled JavaScript source
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

fn serializable_value_to_js_value(value: &SerializableValue) -> Result<JsValue, JsError> {
    match value {
        SerializableValue::Number(n) => Ok(JsValue::from(*n)),
        SerializableValue::Bool(b) => Ok(JsValue::from(*b)),
        SerializableValue::Null => Ok(JsValue::null()),
        SerializableValue::String(s) => Ok(JsValue::from(s.as_str())),
        SerializableValue::List(items) => {
            let array = js_sys::Array::new();
            for item in items {
                let js_item = serializable_value_to_js_value(item)?;
                array.push(&js_item);
            }
            Ok(array.into())
        }
        SerializableValue::Record(map) => {
            let obj = js_sys::Object::new();
            for (key, val) in map {
                let js_val = serializable_value_to_js_value(val)?;
                Reflect::set(&obj, &key.into(), &js_val)
                    .map_err(|e| JsError::new(&format!("Failed to set property: {:?}", e)))?;
            }
            Ok(obj.into())
        }
        SerializableValue::Lambda(lambda_def) => {
            // Convert lambda back to JavaScript function
            let params = lambda_def
                .args
                .iter()
                .map(|arg| match arg {
                    LambdaArg::Required(name) => name.clone(),
                    LambdaArg::Optional(name) => format!("{} = undefined", name),
                    LambdaArg::Rest(name) => format!("...{}", name),
                })
                .collect::<Vec<_>>()
                .join(", ");

            let func_str = format!("({}) => {}", params, lambda_def.body);

            // Use eval to create the function
            let func = eval(&func_str)
                .map_err(|e| JsError::new(&format!("Failed to create function: {:?}", e)))?;

            Ok(func)
        }
        SerializableValue::Each(_) => {
            // Each values are internal and shouldn't be in inputs
            Err(JsError::new("Each values are not supported in inputs"))
        }
        SerializableValue::BuiltIn(name) => {
            // Built-in functions are internal and shouldn't be in inputs
            Err(JsError::new(&format!(
                "Built-in function '{}' is not supported in inputs",
                name
            )))
        }
    }
}

fn convert_inputs_to_js_values(inputs_js: &JsValue) -> Result<JsValue, JsError> {
    // Try to deserialize the inputs as a SerializableValue structure
    if let Ok(inputs_map) =
        serde_wasm_bindgen::from_value::<BTreeMap<String, SerializableValue>>(inputs_js.clone())
    {
        // Convert the SerializableValue map to plain JS values
        let js_obj = js_sys::Object::new();
        for (key, serializable_val) in inputs_map {
            let js_val = serializable_value_to_js_value(&serializable_val)?;
            Reflect::set(&js_obj, &key.into(), &js_val)
                .map_err(|e| JsError::new(&format!("Failed to set input property: {:?}", e)))?;
        }
        Ok(js_obj.into())
    } else {
        // If it's not in SerializableValue format, use it as-is (plain JS values)
        Ok(inputs_js.clone())
    }
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
        // First check if this is a built-in function
        if let Some(builtin_name) = detect_builtin_function(value) {
            return Ok(SerializableValue::BuiltIn(builtin_name));
        }

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
pub fn evaluate(expr: &str, inputs_js: JsValue, source_mapped: Option<bool>) -> Result<JsValue, JsError> {
    let use_source_maps = source_mapped.unwrap_or(false);
    
    if use_source_maps {
        evaluate_with_source_map(expr, inputs_js)
    } else {
        evaluate_without_source_map(expr, inputs_js)
    }
}

fn evaluate_without_source_map(expr: &str, inputs_js: JsValue) -> Result<JsValue, JsError> {
    // Transpile Blots code to JavaScript with inline evaluation
    let js_code = transpile_to_js_with_inline_eval(expr)
        .map_err(|e| JsError::new(&format!("Transpilation error: {}", e)))?;

    // Set up inputs in global scope if provided
    if !inputs_js.is_undefined() && !inputs_js.is_null() {
        let global = js_sys::global();
        // Convert SerializableValue inputs to plain JS values for the runtime
        let converted_inputs = convert_inputs_to_js_values(&inputs_js)?;
        Reflect::set(&global, &"$$_inputs".into(), &converted_inputs)
            .map_err(|e| JsError::new(&format!("Failed to set inputs: {:?}", e)))?;
    }

    // Execute the transpiled JavaScript
    let result = eval(&js_code).map_err(|e| {
        JsError::new(&format!(
            "JavaScript execution error: {}",
            translate_js_error(&format!("{:?}", e))
        ))
    })?;

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
                    let cleaned_key = translate_js_identifiers(&key);
                    values.insert(cleaned_key, serializable_value);
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
                    let cleaned_key = translate_js_identifiers(&key);
                    bindings.insert(cleaned_key, serializable_value);
                }
            }
        }
    }

    // Convert outputs Set to HashSet
    if outputs_obj.is_object() {
        // Check if it's a JavaScript Set
        if let Some(js_set) = outputs_obj.dyn_ref::<js_sys::Set>() {
            // Convert Set to Array for easier iteration
            let array_from = js_sys::Array::from(js_set);
            for i in 0..array_from.length() {
                let value = array_from.get(i);
                if let Some(output_name) = value.as_string() {
                    let cleaned_output = translate_js_identifiers(&output_name);
                    outputs.insert(cleaned_output);
                }
            }
        }
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
pub fn transpile(expr: &str, source_mapped: Option<bool>) -> Result<JsValue, JsError> {
    let use_source_maps = source_mapped.unwrap_or(false);
    
    if use_source_maps {
        let (js_code, source_map) = transpile_to_js_with_source_map(expr, "input.blot".to_string())
            .map_err(|e| JsError::new(&format!("Transpilation error: {}", e)))?;
        
        let result = Object::new();
        Reflect::set(&result, &"code".into(), &js_code.into())
            .map_err(|e| JsError::new(&format!("Failed to set code: {:?}", e)))?;
        
        let source_map_js = serde_wasm_bindgen::to_value(&source_map)
            .map_err(|e| JsError::new(&format!("Failed to serialize source map: {:?}", e)))?;
        Reflect::set(&result, &"sourceMap".into(), &source_map_js)
            .map_err(|e| JsError::new(&format!("Failed to set source map: {:?}", e)))?;
        
        Ok(result.into())
    } else {
        let code = transpile_to_js(expr)
            .map_err(|e| JsError::new(&format!("Transpilation error: {}", e)))?;
        Ok(JsValue::from_str(&code))
    }
}

#[wasm_bindgen]
pub fn transpile_with_inline_eval(expr: &str, source_mapped: Option<bool>) -> Result<JsValue, JsError> {
    let use_source_maps = source_mapped.unwrap_or(false);
    
    if use_source_maps {
        let (js_code, source_map) = transpile_to_js_with_inline_eval_and_source_map(expr, "input.blot".to_string())
            .map_err(|e| JsError::new(&format!("Transpilation error: {}", e)))?;
        
        let result = Object::new();
        Reflect::set(&result, &"code".into(), &js_code.into())
            .map_err(|e| JsError::new(&format!("Failed to set code: {:?}", e)))?;
        
        let source_map_js = serde_wasm_bindgen::to_value(&source_map)
            .map_err(|e| JsError::new(&format!("Failed to serialize source map: {:?}", e)))?;
        Reflect::set(&result, &"sourceMap".into(), &source_map_js)
            .map_err(|e| JsError::new(&format!("Failed to set source map: {:?}", e)))?;
        
        Ok(result.into())
    } else {
        let code = transpile_to_js_with_inline_eval(expr)
            .map_err(|e| JsError::new(&format!("Transpilation error: {}", e)))?;
        Ok(JsValue::from_str(&code))
    }
}

#[wasm_bindgen]
pub fn transpile_with_source_map(expr: &str, source_name: String) -> Result<JsValue, JsError> {
    let (js_code, source_map) = transpile_to_js_with_source_map(expr, source_name)
        .map_err(|e| JsError::new(&format!("Transpilation error: {}", e)))?;
    
    let result = Object::new();
    Reflect::set(&result, &"code".into(), &js_code.into())
        .map_err(|e| JsError::new(&format!("Failed to set code: {:?}", e)))?;
    
    let source_map_js = serde_wasm_bindgen::to_value(&source_map)
        .map_err(|e| JsError::new(&format!("Failed to serialize source map: {:?}", e)))?;
    Reflect::set(&result, &"sourceMap".into(), &source_map_js)
        .map_err(|e| JsError::new(&format!("Failed to set source map: {:?}", e)))?;
    
    Ok(result.into())
}

#[wasm_bindgen]
pub fn transpile_with_inline_eval_and_source_map(expr: &str, source_name: String) -> Result<JsValue, JsError> {
    let (js_code, source_map) = transpile_to_js_with_inline_eval_and_source_map(expr, source_name)
        .map_err(|e| JsError::new(&format!("Transpilation error: {}", e)))?;
    
    let result = Object::new();
    Reflect::set(&result, &"code".into(), &js_code.into())
        .map_err(|e| JsError::new(&format!("Failed to set code: {:?}", e)))?;
    
    let source_map_js = serde_wasm_bindgen::to_value(&source_map)
        .map_err(|e| JsError::new(&format!("Failed to serialize source map: {:?}", e)))?;
    Reflect::set(&result, &"sourceMap".into(), &source_map_js)
        .map_err(|e| JsError::new(&format!("Failed to set source map: {:?}", e)))?;
    
    Ok(result.into())
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

fn detect_builtin_function(js_func: &JsValue) -> Option<String> {
    // Get the function as a string to check its implementation
    if let Some(func_str) = js_func.as_string() {
        // Check if it matches the pattern of a built-in function
        if func_str.starts_with("function $$") {
            // Extract the function name
            if let Some(start) = func_str.find("$$") {
                if let Some(end) = func_str[start + 2..].find("(") {
                    let builtin_name = &func_str[start + 2..start + 2 + end];
                    return Some(builtin_name.to_string());
                }
            }
        }
    }

    // Try calling toString() on the function to get its source
    if let Some(func) = js_func.dyn_ref::<js_sys::Function>() {
        let func_str_val = func.to_string();
        if let Some(func_str) = func_str_val.as_string() {
            // Check if it matches the pattern of a built-in function
            if func_str.contains("function $$") {
                // Extract the function name after $$
                if let Some(start) = func_str.find("$$") {
                    if let Some(end) = func_str[start + 2..].find("(") {
                        let builtin_name = &func_str[start + 2..start + 2 + end];
                        return Some(builtin_name.to_string());
                    }
                }
            }
        }
    }

    None
}

fn evaluate_with_source_map(expr: &str, inputs_js: JsValue) -> Result<JsValue, JsError> {
    // Transpile Blots code to JavaScript with inline evaluation and source map
    let (js_code, source_map) = transpile_to_js_with_inline_eval_and_source_map(expr, "input.blot".to_string())
        .map_err(|e| JsError::new(&format!("Transpilation error: {}", e)))?;

    // Set up inputs in global scope if provided
    if !inputs_js.is_undefined() && !inputs_js.is_null() {
        let global = js_sys::global();
        // Convert SerializableValue inputs to plain JS values for the runtime
        let converted_inputs = convert_inputs_to_js_values(&inputs_js)?;
        Reflect::set(&global, &"$$_inputs".into(), &converted_inputs)
            .map_err(|e| JsError::new(&format!("Failed to set inputs: {:?}", e)))?;
    }

    // Store the source map for error mapping
    let global = js_sys::global();
    let source_map_js = serde_wasm_bindgen::to_value(&source_map)
        .map_err(|e| JsError::new(&format!("Failed to serialize source map: {:?}", e)))?;
    Reflect::set(&global, &"$$_sourceMap".into(), &source_map_js)
        .map_err(|e| JsError::new(&format!("Failed to set source map: {:?}", e)))?;

    // Execute the transpiled JavaScript
    let _result = eval(&js_code).map_err(|e| {
        let error_str = format!("{:?}", e);
        
        // Since source map mappings aren't implemented yet, try to at least clean up the error
        let cleaned_error = enhance_error_message(&error_str, expr);
        JsError::new(&cleaned_error)
    })?;

    // The transpiled code with inline evaluation returns $$results object
    // Extract the results from the global scope
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
                    let cleaned_key = translate_js_identifiers(&key);
                    values.insert(cleaned_key, serializable_value);
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
                    let cleaned_key = translate_js_identifiers(&key);
                    bindings.insert(cleaned_key, serializable_value);
                }
            }
        }
    }

    // Convert outputs Set to HashSet
    if outputs_obj.is_object() {
        // Check if it's a JavaScript Set
        if let Some(js_set) = outputs_obj.dyn_ref::<js_sys::Set>() {
            // Convert Set to Array for easier iteration
            let array_from = js_sys::Array::from(js_set);
            for i in 0..array_from.length() {
                let value = array_from.get(i);
                if let Some(output_name) = value.as_string() {
                    let cleaned_output = translate_js_identifiers(&output_name);
                    outputs.insert(cleaned_output);
                }
            }
        }
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

fn map_js_error_to_blots(error_str: &str, source_map: &SourceMap) -> Result<String> {
    // Extract line and column from JavaScript error
    // Look for patterns like "at eval (eval at <anonymous>...:line:col)"
    let re = regex::Regex::new(r":(\d+):(\d+)").unwrap();
    
    if let Some(captures) = re.captures(error_str) {
        if let (Some(line_str), Some(col_str)) = (captures.get(1), captures.get(2)) {
            if let (Ok(js_line), Ok(js_col)) = (line_str.as_str().parse::<usize>(), col_str.as_str().parse::<usize>()) {
                // Find the corresponding Blots source location
                for mapping in &source_map.mappings {
                    if mapping.generated_line == js_line && mapping.generated_column <= js_col {
                        let source_line = mapping.source_range.start.line;
                        let source_col = mapping.source_range.start.column;
                        
                        // Extract the error message
                        let error_msg = if let Some(pos) = error_str.find("Error:") {
                            &error_str[pos..]
                        } else {
                            error_str
                        };
                        
                        // Get the source line for context
                        let source_lines: Vec<&str> = source_map.sources_content[0].lines().collect();
                        let context = if source_line > 0 && source_line <= source_lines.len() {
                            format!("\n\n{}\n{}^", 
                                source_lines[source_line - 1],
                                " ".repeat(source_col - 1))
                        } else {
                            String::new()
                        };
                        
                        return Ok(format!("{}\n  at {}:{}:{}{}", 
                            error_msg,
                            source_map.sources[0],
                            source_line,
                            source_col,
                            context
                        ));
                    }
                }
            }
        }
    }
    
    Err(anyhow::anyhow!("Could not map error"))
}

fn enhance_error_message(error_str: &str, blots_code: &str) -> String {
    // Extract the error message
    let error_msg = if let Some(pos) = error_str.find("Error:") {
        let end_pos = error_str[pos..].find('\n').unwrap_or(error_str[pos..].len());
        &error_str[pos..pos + end_pos]
    } else {
        error_str
    };
    
    // Try to identify which operation caused the error
    let enhanced_msg = if error_msg.contains("can't add") && error_msg.contains("boolean") {
        // Find the line with + operator and boolean
        let lines: Vec<&str> = blots_code.lines().collect();
        for (i, line) in lines.iter().enumerate() {
            if line.contains('+') && (blots_code.contains("true") || blots_code.contains("false")) {
                return format!("{}\n  at line {} in your Blots code:\n\n{}\n{}^",
                    error_msg,
                    i + 1,
                    line,
                    " ".repeat(line.find('+').unwrap_or(0))
                );
            }
        }
        error_msg.to_string()
    } else if error_msg.contains("can't multiply") && error_msg.contains("string") {
        // Find the line with * operator
        let lines: Vec<&str> = blots_code.lines().collect();
        for (i, line) in lines.iter().enumerate() {
            if line.contains('*') {
                return format!("{}\n  at line {} in your Blots code:\n\n{}\n{}^",
                    error_msg,
                    i + 1,
                    line,
                    " ".repeat(line.find('*').unwrap_or(0))
                );
            }
        }
        error_msg.to_string()
    } else if error_msg.contains("can't divide") {
        // Find the line with / operator
        let lines: Vec<&str> = blots_code.lines().collect();
        for (i, line) in lines.iter().enumerate() {
            if line.contains('/') {
                return format!("{}\n  at line {} in your Blots code:\n\n{}\n{}^",
                    error_msg,
                    i + 1,
                    line,
                    " ".repeat(line.find('/').unwrap_or(0))
                );
            }
        }
        error_msg.to_string()
    } else {
        translate_js_error(error_msg)
    };
    
    enhanced_msg
}

#[cfg(test)]
mod tests;
#[cfg(test)]
mod error_tests;
