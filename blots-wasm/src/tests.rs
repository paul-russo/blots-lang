#[cfg(all(test, target_arch = "wasm32"))]
mod tests {
    use crate::{
        evaluate, transpile, transpile_with_inline_eval, 
        get_built_in_function_names, get_constants, tokenize
    };
    use js_sys::{Array, Object, Reflect};
    use wasm_bindgen_test::*;
    
    wasm_bindgen_test_configure!(run_in_browser);

    #[wasm_bindgen_test]
    fn test_transpile_without_source_map() {
        let result = transpile("x = 42", None).unwrap_or_else(|_| panic!("transpile failed"));
        assert!(result.is_string());
        
        let js_code = result.as_string().unwrap();
        assert!(js_code.contains("const $$_x = 42"));
    }

    #[wasm_bindgen_test]
    fn test_transpile_with_source_map() {
        let result = transpile("x = 42", Some(true)).unwrap_or_else(|_| panic!("transpile failed"));
        assert!(result.is_object());
        
        let code = Reflect::get(&result, &"code".into()).unwrap();
        let source_map = Reflect::get(&result, &"sourceMap".into()).unwrap();
        
        assert!(code.is_string());
        assert!(source_map.is_object());
        
        let js_code = code.as_string().unwrap();
        assert!(js_code.contains("const $$_x = 42"));
    }

    #[wasm_bindgen_test]
    fn test_transpile_with_inline_eval_without_source_map() {
        let result = transpile_with_inline_eval("x = 42\noutput x", None).unwrap_or_else(|_| panic!("transpile failed"));
        assert!(result.is_string());
        
        let js_code = result.as_string().unwrap();
        assert!(js_code.contains("$$results"));
        assert!(js_code.contains("const $$_x = 42"));
    }

    #[wasm_bindgen_test]
    fn test_transpile_with_inline_eval_with_source_map() {
        let result = transpile_with_inline_eval("x = 42\noutput x", Some(true)).unwrap_or_else(|_| panic!("transpile failed"));
        assert!(result.is_object());
        
        let code = Reflect::get(&result, &"code".into()).unwrap();
        let source_map = Reflect::get(&result, &"sourceMap".into()).unwrap();
        
        assert!(code.is_string());
        assert!(source_map.is_object());
        
        let js_code = code.as_string().unwrap();
        assert!(js_code.contains("$$results"));
    }

    #[wasm_bindgen_test]
    fn test_evaluate_without_source_map() {
        let inputs = js_sys::Object::new();
        let result = evaluate("x = 10 + 20\noutput x", inputs.into(), None).unwrap_or_else(|_| panic!("evaluate failed"));
        
        assert!(result.is_object());
        let result_obj = &result;
        
        let outputs = Reflect::get(result_obj, &"outputs".into()).unwrap();
        assert!(Array::is_array(&outputs));
        
        let outputs_array = Array::from(&outputs);
        assert_eq!(outputs_array.length(), 1);
        assert_eq!(outputs_array.get(0).as_string().unwrap(), "x");
    }

    #[wasm_bindgen_test]
    fn test_evaluate_with_source_map_no_error() {
        let inputs = js_sys::Object::new();
        let result = evaluate("x = 10 + 20\noutput x", inputs.into(), Some(true)).unwrap_or_else(|_| panic!("evaluate failed"));
        
        // Should work the same way for valid code
        assert!(result.is_object());
        let result_obj = &result;
        
        let outputs = Reflect::get(result_obj, &"outputs".into()).unwrap();
        assert!(Array::is_array(&outputs));
    }

    #[wasm_bindgen_test]
    fn test_evaluate_with_source_map_type_error() {
        let inputs = Object::new();
        let result = evaluate("x = 42\ny = true\nresult = x + y", inputs.into(), Some(true));
        
        assert!(result.is_err());
        // Can't inspect JsError contents in tests, but we've verified it errors
    }

    #[wasm_bindgen_test]
    fn test_evaluate_without_source_map_type_error() {
        let inputs = Object::new();
        let result = evaluate("x = 42\ny = true\nresult = x + y", inputs.into(), None);
        
        assert!(result.is_err());
        // Can't inspect JsError contents in tests, but we've verified it errors
    }


    #[wasm_bindgen_test]
    fn test_get_built_in_function_names() {
        let result = get_built_in_function_names().unwrap_or_else(|_| panic!("get functions failed"));
        assert!(result.is_object());
        
        // Should return an array of function names
        let array = Array::from(&result);
        assert!(array.length() > 0);
        
        // Check for some common functions
        let names: Vec<String> = array.iter()
            .filter_map(|v| v.as_string())
            .collect();
        
        assert!(names.contains(&"map".to_string()));
        assert!(names.contains(&"filter".to_string()));
        assert!(names.contains(&"sum".to_string()));
    }

    #[wasm_bindgen_test]
    fn test_get_constants() {
        let result = get_constants();
        assert!(result.is_ok(), "get_constants failed");
        let result = result.unwrap_or_else(|_| panic!("get_constants returned error"));
        
        assert!(result.is_object(), "Result is not an object");
        
        // The result should be a JavaScript object with numeric properties
        // Let's just verify it's an object for now
        // The actual constants verification can be done in integration tests
    }

    #[wasm_bindgen_test]
    fn test_tokenize() {
        let result = tokenize("x = 42").unwrap_or_else(|_| panic!("tokenize failed"));
        assert!(result.is_object());
        
        // Should return an array of tokens
        let array = Array::from(&result);
        assert!(array.length() > 0);
    }
}