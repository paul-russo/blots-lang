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
fn test_simple_closure() {
    let code = "x = 5\noutput f = y => x + 2";
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);
    assert_eq!(func_src, "(y) => 5 + 2");
}

#[test]
fn test_function_without_closure() {
    let code = "output f = x => x * 2";
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);
    assert_eq!(func_src, "(x) => x * 2");
}

#[test]
fn test_multiple_closures() {
    let code = "a = 10\nb = 20\noutput f = (x, y) => a * x + b * y";
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);
    assert_eq!(func_src, "(x, y) => 10 * x + 20 * y");
}

#[test]
fn test_builtin_function_output() {
    let code = "output f = max";
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);
    assert_eq!(func_src, "max");
}

#[test]
fn test_closure_with_list() {
    let code = "arr = [1, 2, 3]\noutput f = x => arr via (y => y * x)";
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);
    assert_eq!(func_src, "(x) => [1, 2, 3] via (y) => y * x");
}

#[test]
fn test_nested_function_with_closure() {
    let code = "a = 5\noutput f = x => y => a + x + y";
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);
    assert_eq!(func_src, "(x) => (y) => 5 + x + y");
}

#[test]
fn test_closure_over_record() {
    let code = "y = 42\nx = { something: y }\noutput f = z => x.something + z";
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);
    assert_eq!(func_src, "(z) => {something: 42}.something + z");
}

#[test]
fn test_closure_over_list_with_bindings() {
    let code = "a = 10\nb = 20\narr = [a, b, a + b]\noutput f = i => arr[i]";
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);
    assert_eq!(func_src, "(i) => [10, 20, 30][i]");
}

#[test]
fn test_closure_over_nested_record() {
    let code = "x = 5\ny = 10\nobj = { a: x, b: y, nested: { sum: x + y, product: x * y } }\noutput f = () => obj.nested.sum";
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);
    assert_eq!(
        func_src,
        "() => {a: 5, b: 10, nested: {sum: 15, product: 50}}.nested.sum"
    );
}

#[test]
fn test_list_of_records_with_closures() {
    let code =
        "x = 3\ny = { value: x * 2 }\narr = [y, { value: x * 3 }]\noutput f = i => arr[i].value";
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);
    assert_eq!(func_src, "(i) => [{value: 6}, {value: 9}][i].value");
}

#[test]
fn test_via_operator_with_closures() {
    let code =
        "base = 100\nmultipliers = [1, 2, 3]\noutput f = () => multipliers via (m => m * base)";
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);
    assert_eq!(func_src, "() => [1, 2, 3] via (m) => m * 100");
}

#[test]
fn test_string_closure() {
    let code = "greeting = \"Hello\"\noutput f = name => greeting + \", \" + name";
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);
    assert_eq!(func_src, "(name) => \"Hello\" + \", \" + name");
}

#[test]
fn test_boolean_closure() {
    let code = "flag = true\noutput f = x => if flag then x * 2 else x";
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);
    assert_eq!(func_src, "(x) => if true then x * 2 else x");
}

#[test]
fn test_null_closure() {
    let code = "nothing = null\noutput f = x => x ?? nothing";
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);
    assert_eq!(func_src, "(x) => x ?? null");
}

#[test]
fn test_optional_arg_function() {
    let code = "output f = (x, y?) => x + (y ?? 0)";
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);
    // Note: The AST to source conversion may not preserve unnecessary parentheses
    assert_eq!(func_src, "(x, y?) => x + y ?? 0");
}

#[test]
fn test_rest_arg_function() {
    let code = "output f = (x, ...rest) => [x, ...rest]";
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);
    assert_eq!(func_src, "(x, ...rest) => [x, ...rest]");
}

#[test]
fn test_higher_order_function_compose() {
    let code = r#"
compose = (f, g) => x => f(g(x))
double = x => x * 2
add_one = x => x + 1
output composed = compose(add_one, double)
"#;
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);
    assert_eq!(func_src, "(x) => ((x) => x + 1)(((x) => x * 2)(x))");
}

#[test]
fn test_higher_order_function_with_multiple_captures() {
    let code = r#"
make_adder = base => x => base + x
add_five = make_adder(5)
add_ten = make_adder(10)
combiner = (f, g) => x => f(x) + g(x)
output combined = combiner(add_five, add_ten)
"#;
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);
    assert_eq!(func_src, "(x) => ((x) => 5 + x)(x) + ((x) => 10 + x)(x)");
}

#[test]
fn test_curry_function() {
    let code = r#"
curry = (f) => x => y => f(x, y)
add = (a, b) => a + b
output curried_add = curry(add)
"#;
    let output = run_blots_eval(code);
    let func_src = parse_function_output(&output);
    assert_eq!(func_src, "(x) => (y) => ((a, b) => a + b)(x, y)");
}
