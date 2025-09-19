use serde_json::json;

fn run_blots_with_input(code: &str, input_json: serde_json::Value) -> String {
    use std::io::Write;
    use std::process::{Command, Stdio};

    let input_str = input_json.to_string();

    let mut child = Command::new("cargo")
        .args(&["run", "-p", "blots", "--", "-e", "-i", &input_str])
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

#[test]
fn test_simple_function_input() {
    let code = "output result = inputs.f(10)";
    let input = json!({
        "f": { "__blots_function": "x => x * 2" }
    });

    let output = run_blots_with_input(code, input);
    let result: serde_json::Value = serde_json::from_str(&output).unwrap();

    assert_eq!(result["result"], 20.0);
}

#[test]
fn test_multiple_function_inputs() {
    let code = "output result = inputs.f(5) + inputs.g(3)";
    let input = json!({
        "f": { "__blots_function": "x => x * 2" },
        "g": { "__blots_function": "x => x + 10" }
    });

    let output = run_blots_with_input(code, input);
    let result: serde_json::Value = serde_json::from_str(&output).unwrap();

    assert_eq!(result["result"], 23.0);
}

#[test]
fn test_builtin_function_input() {
    let code = "output result = inputs.fn(inputs.arr)";
    let input = json!({
        "fn": { "__blots_function": "max" },
        "arr": [3, 7, 2, 9, 1]
    });

    let output = run_blots_with_input(code, input);
    let result: serde_json::Value = serde_json::from_str(&output).unwrap();

    assert_eq!(result["result"], 9.0);
}

#[test]
fn test_multi_argument_function_input() {
    let code = "output result = inputs.add(2, 3)";
    let input = json!({
        "add": { "__blots_function": "(x, y) => x + y" }
    });

    let output = run_blots_with_input(code, input);
    let result: serde_json::Value = serde_json::from_str(&output).unwrap();

    assert_eq!(result["result"], 5.0);
}

#[test]
fn test_optional_argument_function_input() {
    let code = "output result = inputs.f(10)";
    let input = json!({
        "f": { "__blots_function": "(x, y?) => x + (y ?? 0)" }
    });

    let output = run_blots_with_input(code, input);
    let result: serde_json::Value = serde_json::from_str(&output).unwrap();

    assert_eq!(result["result"], 10.0);
}

#[test]
fn test_rest_argument_function_input() {
    let code = "output result = inputs.f(1, 2, 3)";
    let input = json!({
        "f": { "__blots_function": "(x, ...rest) => [x, ...rest]" }
    });

    let output = run_blots_with_input(code, input);
    let result: serde_json::Value = serde_json::from_str(&output).unwrap();

    assert_eq!(result["result"], json!([1.0, 2.0, 3.0]));
}

#[test]
fn test_function_with_inlined_closure() {
    let code = "output result = inputs.f(10)";
    let input = json!({
        "f": { "__blots_function": "y => 5 + y" }
    });

    let output = run_blots_with_input(code, input);
    let result: serde_json::Value = serde_json::from_str(&output).unwrap();

    assert_eq!(result["result"], 15.0);
}

#[test]
fn test_higher_order_function_input() {
    let code = "output result = [1, 2, 3] via inputs.mapper";
    let input = json!({
        "mapper": { "__blots_function": "x => x * 2" }
    });

    let output = run_blots_with_input(code, input);
    let result: serde_json::Value = serde_json::from_str(&output).unwrap();

    assert_eq!(result["result"], json!([2.0, 4.0, 6.0]));
}

#[test]
fn test_function_returning_function() {
    let code = "output result = inputs.makeAdder(5)(10)";
    let input = json!({
        "makeAdder": { "__blots_function": "x => y => x + y" }
    });

    let output = run_blots_with_input(code, input);
    let result: serde_json::Value = serde_json::from_str(&output).unwrap();

    assert_eq!(result["result"], 15.0);
}

#[test]
fn test_function_in_nested_structure() {
    let code = "output result = inputs.obj.funcs.add(3, 4)";
    let input = json!({
        "obj": {
            "funcs": {
                "add": { "__blots_function": "(a, b) => a + b" }
            }
        }
    });

    let output = run_blots_with_input(code, input);
    let result: serde_json::Value = serde_json::from_str(&output).unwrap();

    assert_eq!(result["result"], 7.0);
}

#[test]
fn test_array_of_functions() {
    let code = "output result = inputs.funcs[1](10)";
    let input = json!({
        "funcs": [
            { "__blots_function": "x => x + 1" },
            { "__blots_function": "x => x * 2" },
            { "__blots_function": "x => x - 1" }
        ]
    });

    let output = run_blots_with_input(code, input);
    let result: serde_json::Value = serde_json::from_str(&output).unwrap();

    assert_eq!(result["result"], 20.0);
}

#[test]
fn test_round_trip_function() {
    // First, output a function with closure
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

    if let Some(mut stdin) = child.stdin.take() {
        stdin
            .write_all(b"x = 5\noutput f = y => x + y")
            .expect("Failed to write to stdin");
    }

    let output = child.wait_with_output().expect("Failed to wait for blots");
    let function_output: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("Failed to parse function output");

    // Now pass it back as input
    let code = "output result = inputs.func(10)";
    let input = json!({
        "func": function_output["f"]
    });

    let result_output = run_blots_with_input(code, input);
    let result: serde_json::Value = serde_json::from_str(&result_output).unwrap();

    assert_eq!(result["result"], 15.0);
}