use serde_json::Value;

fn run_blots_eval(code: &str) -> String {
    use std::io::Write;
    use std::process::{Command, Stdio};

    let mut child = Command::new("cargo")
        .args(&["run", "-p", "blots", "--", "-e"])
        .env("CARGO_TERM_COLOR", "never")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn blots");

    // Write code to stdin
    if let Some(mut stdin) = child.stdin.take() {
        stdin
            .write_all(code.as_bytes())
            .expect("Failed to write to stdin");
    }

    let output = child.wait_with_output().expect("Failed to wait for blots");

    assert!(
        output.status.success(),
        "Command failed with stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    String::from_utf8_lossy(&output.stdout).trim().to_string()
}

fn parse_function_output(json_str: &str) -> String {
    let json: Value =
        serde_json::from_str(json_str).expect(&format!("Failed to parse JSON: {}", json_str));

    // Extract the function source from the first output
    if let Some(obj) = json.as_object() {
        if let Some((_, value)) = obj.iter().next() {
            if let Some(func_obj) = value.as_object() {
                if let Some(func_src) = func_obj.get("__blots_function") {
                    if let Some(src) = func_src.as_str() {
                        return src.to_string();
                    }
                }
            }
        }
    }
    panic!("Could not extract function source from: {}", json_str);
}

#[test]
fn test_lambda_parameter_not_substituted_simple() {
    // Regression test: lambda parameter 'x' should not be replaced with value '42'
    let code = "x = 42\noutput increment = x => x + 1";
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);

    // The parameter 'x' should NOT be replaced with '42'
    assert_eq!(func_src, "(x) => x + 1", "Lambda parameter should not be substituted with outer scope value");
    assert!(!func_src.contains("42"), "Lambda body should not contain the value '42'");
}

#[test]
fn test_lambda_multiple_parameters_not_substituted() {
    // Test that multiple parameters are preserved
    let code = "a = 10\nb = 20\noutput add = (a, b) => a + b";
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);

    assert_eq!(func_src, "(a, b) => a + b", "Lambda parameters should not be substituted");
    assert!(!func_src.contains("10"), "Lambda body should not contain value '10'");
    assert!(!func_src.contains("20"), "Lambda body should not contain value '20'");
}

#[test]
fn test_curried_lambda_captures_outer_preserves_inner() {
    // Test that curried functions capture outer parameters but preserve inner parameter names
    let code = "output curry_add = x => y => x + y";
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);

    // The outer function should preserve both x and y as identifiers
    assert_eq!(func_src, "(x) => (y) => x + y");
}

#[test]
fn test_partial_application_captures_value_preserves_parameter() {
    // Test that partially applied function captures value but preserves its own parameter
    let code = "curry_add = x => y => x + y\noutput add_five = curry_add(5)";
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);

    // The inner function should have x captured as 5, but y should remain as parameter
    assert_eq!(func_src, "(y) => 5 + y");
    assert!(!func_src.contains("x"), "Captured variable 'x' should be substituted with value");
    assert!(func_src.contains("y"), "Parameter 'y' should be preserved");
}

#[test]
fn test_nested_lambdas_preserve_parameters() {
    // Test nested lambdas with same parameter names in different scopes
    let code = "x = 100\noutput outer = x => (y => x + y)";
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);

    // Both x and y should be preserved as parameters
    assert_eq!(func_src, "(x) => (y) => x + y");
    assert!(!func_src.contains("100"), "Outer scope value should not pollute lambda");
}

#[test]
fn test_lambda_in_conditional_preserves_parameters() {
    // Test that lambda parameters are preserved even in complex expressions
    let code = "x = 42\noutput check = x => if x > 0 then \"positive\" else \"negative\"";
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);

    assert_eq!(func_src, "(x) => if x > 0 then \"positive\" else \"negative\"");
    assert!(!func_src.contains("42"), "Lambda body should not contain outer scope value");
}

#[test]
fn test_higher_order_function_composition() {
    // Test that composed functions preserve parameter names
    let code = "x = 99\noutput compose = (f, g) => x => f(g(x))";
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);

    // All parameters should be preserved
    assert_eq!(func_src, "(f, g) => (x) => f(g(x))");
    assert!(!func_src.contains("99"), "Lambda should not capture outer scope value with same name as parameter");
}

#[test]
fn test_multiple_lambdas_same_parameter_name() {
    // Test that multiple lambdas with the same parameter name work correctly
    let code = "x = 1000\ndouble = x => x * 2\noutput triple = x => x * 3";
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);

    assert_eq!(func_src, "(x) => x * 3");
    assert!(!func_src.contains("1000"), "Parameter 'x' should not be replaced with outer value");
}

#[test]
fn test_lambda_with_list_operations_preserves_parameters() {
    // Test that lambda parameters in list operations are preserved
    let code = "x = 50\noutput mapper = x => x via (y => y * 2)";
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);

    // Both x and y should be preserved as parameters
    assert!(func_src.contains("x"), "Outer parameter 'x' should be preserved");
    assert!(func_src.contains("y"), "Inner parameter 'y' should be preserved");
    assert!(!func_src.contains("50"), "Should not contain outer scope value");
}
