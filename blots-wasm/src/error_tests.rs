#[cfg(test)]
mod tests {
    use crate::enhance_error_message;

    #[test]
    fn test_enhance_error_message_add_operation() {
        let error_str = "JsValue(Error: can't add number and boolean\nError: can't add number and boolean)";
        let blots_code = "x = 42\ny = true\nresult = x + y";
        
        let enhanced = enhance_error_message(error_str, blots_code);
        
        assert!(enhanced.contains("Error: can't add number and boolean"));
        assert!(enhanced.contains("at line 3"));
        assert!(enhanced.contains("result = x + y"));
        assert!(enhanced.contains("         ^"));
    }

    #[test]
    fn test_enhance_error_message_multiply_operation() {
        let error_str = "JsValue(Error: can't multiply string and number)";
        let blots_code = "double = (n) => n * 2\nresult = double(\"hello\")";
        
        let enhanced = enhance_error_message(error_str, blots_code);
        
        assert!(enhanced.contains("Error: can't multiply string and number"));
        assert!(enhanced.contains("at line 1"));
        assert!(enhanced.contains("double = (n) => n * 2"));
        assert!(enhanced.contains("                ^"));
    }

    #[test]
    fn test_enhance_error_message_divide_operation() {
        let error_str = "JsValue(Error: can't divide number by boolean)";
        let blots_code = "x = 100\ny = false\nresult = x / y";
        
        let enhanced = enhance_error_message(error_str, blots_code);
        
        assert!(enhanced.contains("Error: can't divide"));
        assert!(enhanced.contains("at line 3"));
        assert!(enhanced.contains("result = x / y"));
        assert!(enhanced.contains("         ^"));
    }

    #[test]
    fn test_enhance_error_message_multiline_with_comments() {
        let error_str = "Error: can't add number and boolean";
        let blots_code = r#"// Setup values
x = 42
y = true
// This will error
result = x + y"#;
        
        let enhanced = enhance_error_message(error_str, blots_code);
        
        assert!(enhanced.contains("at line 5"));
        assert!(enhanced.contains("result = x + y"));
    }

    #[test]
    fn test_enhance_error_message_no_match() {
        let error_str = "Error: undefined variable";
        let blots_code = "x = 42\ny = true";
        
        let enhanced = enhance_error_message(error_str, blots_code);
        
        // Should return translated error when no specific pattern matches
        assert!(enhanced.contains("undefined variable"));
    }
}