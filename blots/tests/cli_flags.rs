use std::fs;
use std::io::Write;
use std::process::{Command, Stdio};
use tempfile::NamedTempFile;

fn run_blots(args: &[&str], stdin: Option<&str>) -> (String, String, bool) {
    let mut cmd = Command::new("cargo");
    cmd.args(&["run", "-p", "blots", "--"]);
    cmd.args(args);
    
    if stdin.is_some() {
        cmd.stdin(Stdio::piped());
    }
    
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());
    
    let mut child = cmd.spawn().expect("Failed to spawn blots process");
    
    if let Some(input) = stdin {
        let mut stdin = child.stdin.take().expect("Failed to open stdin");
        stdin.write_all(input.as_bytes()).expect("Failed to write to stdin");
    }
    
    let output = child.wait_with_output().expect("Failed to read output");
    
    (
        String::from_utf8_lossy(&output.stdout).to_string(),
        String::from_utf8_lossy(&output.stderr).to_string(),
        output.status.success(),
    )
}

#[test]
fn test_evaluate_flag() {
    // Test basic -e flag with Blots code from stdin
    let (stdout, _, success) = run_blots(
        &["-e"],
        Some("output result = 42"),
    );
    
    assert!(success);
    assert_eq!(stdout.trim(), r#"{"result":42.0}"#);
}

#[test]
fn test_evaluate_flag_with_expression() {
    // Test -e flag with more complex expression
    let (stdout, stderr, success) = run_blots(
        &["-e"],
        Some("x = 10\ny = 20\noutput total = x + y"),
    );
    
    if !success {
        eprintln!("stderr: {}", stderr);
        eprintln!("stdout: {}", stdout);
    }
    
    assert!(success);
    assert_eq!(stdout.trim(), r#"{"total":30.0}"#);
}

#[test]
fn test_evaluate_flag_with_inputs() {
    // Test -e flag combined with -i flag
    let (stdout, _, success) = run_blots(
        &["-e", "-i", r#"{"x": 5, "y": 10}"#],
        Some("output result = inputs.x + inputs.y"),
    );
    
    assert!(success);
    assert_eq!(stdout.trim(), r#"{"result":15.0}"#);
}

#[test]
fn test_inputs_flag_with_file() {
    // Create a test file
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        "output x = inputs.x ?? 0\noutput y = inputs.y ?? 0"
    ).unwrap();
    
    // Test -i flag with file
    let (stdout, _, success) = run_blots(
        &[file.path().to_str().unwrap(), "-i", r#"{"x": 10, "y": 20}"#],
        None,
    );
    
    assert!(success);
    assert_eq!(stdout.trim(), r#"{"x":10.0,"y":20.0}"#);
}

#[test]
fn test_inputs_flag_merge() {
    // Create a test file
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        "output all = inputs"
    ).unwrap();
    
    // Test merging piped inputs with -i flag inputs
    let (stdout, _, success) = run_blots(
        &[file.path().to_str().unwrap(), "-i", r#"{"y": 100, "z": 300}"#],
        Some(r#"{"x": 1, "y": 2}"#),
    );
    
    assert!(success);
    // -i flag should override piped inputs for "y"
    assert!(stdout.contains(r#""x":1.0"#));
    assert!(stdout.contains(r#""y":100.0"#));
    assert!(stdout.contains(r#""z":300.0"#));
}

#[test]
fn test_output_flag() {
    // Create a test file
    let mut source_file = NamedTempFile::new().unwrap();
    writeln!(
        source_file,
        "output result = 42\noutput message = \"test\""
    ).unwrap();
    
    // Create temp output file path
    let output_file = NamedTempFile::new().unwrap();
    let output_path = output_file.path();
    
    // Test -o flag
    let (stdout, stderr, success) = run_blots(
        &[source_file.path().to_str().unwrap(), "-o", output_path.to_str().unwrap()],
        None,
    );
    
    assert!(success);
    assert!(stdout.is_empty()); // Should not print to stdout
    assert!(stderr.contains("Output saved to"));
    
    // Check the output file contents
    let contents = fs::read_to_string(output_path).unwrap();
    assert_eq!(contents, r#"{"result":42.0,"message":"test"}"#);
}

#[test]
fn test_output_flag_with_evaluate() {
    // Create temp output file path
    let output_file = NamedTempFile::new().unwrap();
    let output_path = output_file.path();
    
    // Test -e with -o flag
    let (stdout, stderr, success) = run_blots(
        &["-e", "-o", output_path.to_str().unwrap()],
        Some("output x = 100"),
    );
    
    assert!(success);
    assert!(stdout.is_empty()); // Should not print to stdout
    assert!(stderr.contains("Output saved to"));
    
    // Check the output file contents
    let contents = fs::read_to_string(output_path).unwrap();
    assert_eq!(contents, r#"{"x":100.0}"#);
}

#[test]
fn test_combined_flags() {
    // Create a test file
    let mut source_file = NamedTempFile::new().unwrap();
    writeln!(
        source_file,
        "output result = inputs.x * 2"
    ).unwrap();
    
    // Create temp output file path
    let output_file = NamedTempFile::new().unwrap();
    let output_path = output_file.path();
    
    // Test all flags together
    let (stdout, stderr, success) = run_blots(
        &[
            source_file.path().to_str().unwrap(),
            "-i", r#"{"x": 21}"#,
            "-o", output_path.to_str().unwrap()
        ],
        None,
    );
    
    assert!(success);
    assert!(stdout.is_empty());
    assert!(stderr.contains("Output saved to"));
    
    // Check the output file contents
    let contents = fs::read_to_string(output_path).unwrap();
    assert_eq!(contents, r#"{"result":42.0}"#);
}

#[test]
fn test_invalid_json_input() {
    // Create a test file
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, "output x = 1").unwrap();
    
    // Test with invalid JSON in -i flag
    let (_, stderr, success) = run_blots(
        &[file.path().to_str().unwrap(), "-i", "not valid json"],
        None,
    );
    
    assert!(!success);
    assert!(stderr.contains("[input error]"));
}

#[test]
fn test_invalid_json_stdin() {
    // Create a test file
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, "output x = 1").unwrap();
    
    // Test with invalid JSON from stdin
    let (_, stderr, success) = run_blots(
        &[file.path().to_str().unwrap()],
        Some("not valid json"),
    );
    
    assert!(!success);
    assert!(stderr.contains("[input error]"));
}

#[test]
fn test_evaluate_without_stdin() {
    // Test -e flag without piped input
    // When stdin is not provided but also not a terminal (as in tests),
    // it will read empty input and parse it as empty program
    let (stdout, _, success) = run_blots(
        &["-e"],
        Some(""), // Empty stdin
    );
    
    assert!(success);
    assert_eq!(stdout.trim(), "{}"); // Empty program produces empty output
}

#[test]
fn test_empty_outputs() {
    // Test file with no outputs
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, "x = 10\ny = 20").unwrap();
    
    let (stdout, _, success) = run_blots(
        &[file.path().to_str().unwrap()],
        None,
    );
    
    assert!(success);
    assert_eq!(stdout.trim(), "{}");
}

#[test]
fn test_output_with_nullish_coalescing() {
    // Test ?? operator with inputs
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        "output x = inputs.x ?? 42\noutput y = inputs.y ?? \"default\""
    ).unwrap();
    
    // Test with partial inputs
    let (stdout, _, success) = run_blots(
        &[file.path().to_str().unwrap(), "-i", r#"{"x": 10}"#],
        None,
    );
    
    assert!(success);
    assert!(stdout.contains(r#""x":10.0"#));
    assert!(stdout.contains(r#""y":"default""#));
}