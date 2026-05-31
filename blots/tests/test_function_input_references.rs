//! Pins the interaction between function portability and input references.
//!
//! Functions cross document boundaries via the `__blots_function` JSON encoding: captured
//! bindings are inlined into the serialized source, `#field` references stay symbolic and
//! re-bind to the consuming document's inputs, and `inputs.field` is captured (and therefore
//! frozen) like any other binding. These tests lock that contract in end to end so call-path
//! optimizations cannot regress it silently.

use serde_json::json;

/// Run the blots CLI with `code` on stdin, optionally passing `inputs` via `-i`, and return the
/// trimmed stdout (the JSON outputs object).
fn run_blots(code: &str, inputs: Option<serde_json::Value>) -> String {
    use std::io::Write;
    use std::process::{Command, Stdio};

    let mut args = vec![
        "run".to_string(),
        "-p".to_string(),
        "blots".to_string(),
        "--".to_string(),
        "-e".to_string(),
    ];
    if let Some(inputs) = &inputs {
        args.push("-i".to_string());
        args.push(inputs.to_string());
    }

    let mut child = Command::new("cargo")
        .args(&args)
        .env("CARGO_TERM_COLOR", "never")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn blots");

    if let Some(mut stdin) = child.stdin.take() {
        stdin
            .write_all(code.as_bytes())
            .expect("Failed to write to stdin");
        stdin.flush().expect("Failed to flush stdin");
        drop(stdin); // Explicitly drop to close stdin before waiting
    }

    let output = child.wait_with_output().expect("Failed to wait for blots");

    assert!(
        output.status.success(),
        "Command failed with stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    String::from_utf8_lossy(&output.stdout).trim().to_string()
}

/// Extract the serialized `__blots_function` source for the output named `key`.
fn function_source(output_json: &str, key: &str) -> String {
    let json: serde_json::Value = serde_json::from_str(output_json)
        .unwrap_or_else(|_| panic!("Failed to parse JSON: {}", output_json));

    json[key]["__blots_function"]
        .as_str()
        .unwrap_or_else(|| panic!("No __blots_function for key {} in: {}", key, output_json))
        .to_string()
}

#[test]
fn test_output_function_keeps_input_reference_symbolic() {
    // `#x` is not a capture: it stays symbolic in the serialized source instead of baking in
    // the producing document's value.
    let output = run_blots("output f = () => #x * 2", Some(json!({ "x": 42 })));
    assert_eq!(function_source(&output, "f"), "() => #x * 2");
}

#[test]
fn test_output_function_inlines_captured_inputs_record() {
    // `inputs.x` is captured like any other binding, so the producing document's inputs record
    // is inlined into the serialized source.
    let output = run_blots("output f = () => inputs.x * 2", Some(json!({ "x": 42 })));
    assert_eq!(function_source(&output, "f"), "() => {x: 42}.x * 2");
}

#[test]
fn test_output_function_mixes_inlined_capture_and_input_reference() {
    // Ordinary captures are inlined while `#x` stays symbolic within the same function.
    let output = run_blots(
        "k = 7\noutput f = y => k * y + #x",
        Some(json!({ "x": 42 })),
    );
    assert_eq!(function_source(&output, "f"), "(y) => 7 * y + #x");
}

#[test]
fn test_round_trip_input_reference_rebinds_to_consuming_inputs() {
    // Produce a function that reads `#x` under one set of inputs...
    let producer_output = run_blots("output f = y => #x + y", Some(json!({ "x": 42 })));
    let producer_json: serde_json::Value = serde_json::from_str(&producer_output).unwrap();

    // ...and consume it in a document with different inputs: `#x` must resolve to the
    // consuming document's value, not the producer's.
    let consumer_inputs = json!({ "x": 100, "f": producer_json["f"] });
    let consumer_output = run_blots("output result = inputs.f(1)", Some(consumer_inputs));

    let result: serde_json::Value = serde_json::from_str(&consumer_output).unwrap();
    assert_eq!(result["result"], 101.0);
}

#[test]
fn test_round_trip_inputs_field_keeps_producer_snapshot() {
    // Produce a function that reads `inputs.x`: the producer's inputs record is inlined...
    let producer_output = run_blots("output f = () => inputs.x * 2", Some(json!({ "x": 42 })));
    let producer_json: serde_json::Value = serde_json::from_str(&producer_output).unwrap();

    // ...so the consuming document's different `x` does not affect the result.
    let consumer_inputs = json!({ "x": 100, "f": producer_json["f"] });
    let consumer_output = run_blots("output result = inputs.f()", Some(consumer_inputs));

    let result: serde_json::Value = serde_json::from_str(&consumer_output).unwrap();
    assert_eq!(result["result"], 84.0);
}

#[test]
fn test_input_function_with_input_reference_called_through_via() {
    // A deserialized input function that reads `#x` must see the document's inputs even when it
    // is invoked from inside the `via` broadcasting loop rather than a direct call.
    let inputs = json!({
        "x": 10,
        "scale": { "__blots_function": "(n) => n * #x" }
    });

    let output = run_blots("output result = [1, 2, 3] via inputs.scale", Some(inputs));
    let result: serde_json::Value = serde_json::from_str(&output).unwrap();
    assert_eq!(result["result"], json!([10.0, 20.0, 30.0]));
}

#[test]
fn test_input_function_with_input_reference_called_through_map() {
    // Same as the `via` case, but invoked from inside a higher-order builtin.
    let inputs = json!({
        "x": 10,
        "scale": { "__blots_function": "(n) => n * #x" }
    });

    let output = run_blots("output result = map([1, 2, 3], inputs.scale)", Some(inputs));
    let result: serde_json::Value = serde_json::from_str(&output).unwrap();
    assert_eq!(result["result"], json!([10.0, 20.0, 30.0]));
}

#[test]
fn test_input_function_with_input_reference_called_from_user_lambda() {
    // The input function is passed into a user-defined lambda and called from its body, so
    // `#x` resolves through a nested call frame rather than the top-level environment.
    let inputs = json!({
        "x": 10,
        "scale": { "__blots_function": "(n) => n * #x" }
    });

    let code = "apply = (g, v) => g(v)\noutput result = apply(inputs.scale, 4)";
    let output = run_blots(code, Some(inputs));

    let result: serde_json::Value = serde_json::from_str(&output).unwrap();
    assert_eq!(result["result"], 40.0);
}
