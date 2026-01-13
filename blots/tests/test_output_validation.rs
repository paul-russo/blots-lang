use std::io::Write;
use std::process::Command;
use tempfile::NamedTempFile;

#[test]
fn test_output_unbound_variable_file_mode() {
    // Create a temporary file with code that has an unbound variable
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, "f = x => g(x)").unwrap();
    writeln!(file, "output f").unwrap();

    // Run blots on the file
    let output = Command::new("cargo")
        .args(["run", "-p", "blots", "--", file.path().to_str().unwrap()])
        .output()
        .expect("Failed to execute command");

    let stderr = String::from_utf8_lossy(&output.stderr);

    // Should contain an error about unbound variable
    assert!(
        stderr.contains("unbound variable"),
        "Expected error about unbound variable, got stderr: {}",
        stderr
    );
    assert!(
        stderr.contains("\"g\""),
        "Expected error to mention variable 'g', got stderr: {}",
        stderr
    );
}

#[test]
fn test_output_self_contained_function_succeeds() {
    // Create a temporary file with a self-contained function
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, "f = x => x * 2").unwrap();
    writeln!(file, "output f").unwrap();

    // Run blots on the file
    let output = Command::new("cargo")
        .args(["run", "-p", "blots", "--", file.path().to_str().unwrap()])
        .output()
        .expect("Failed to execute command");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Should succeed and output the function
    assert!(
        !stderr.contains("error"),
        "Unexpected error in stderr: {}",
        stderr
    );
    assert!(
        stdout.contains("\"f\""),
        "Expected output to contain function 'f', got stdout: {}",
        stdout
    );
}

#[test]
fn test_output_recursive_function_succeeds() {
    // Create a temporary file with a recursive function
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        "factorial = n => if n <= 1 then 1 else n * factorial(n - 1)"
    )
    .unwrap();
    writeln!(file, "output factorial").unwrap();

    // Run blots on the file
    let output = Command::new("cargo")
        .args(["run", "-p", "blots", "--", file.path().to_str().unwrap()])
        .output()
        .expect("Failed to execute command");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Should succeed - recursive functions are allowed
    assert!(
        !stderr.contains("error"),
        "Unexpected error in stderr: {}",
        stderr
    );
    assert!(
        stdout.contains("\"factorial\""),
        "Expected output to contain function 'factorial', got stdout: {}",
        stdout
    );
}

#[test]
fn test_output_function_with_closure_succeeds() {
    // Create a temporary file with a function that captures a variable
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, "y = 10").unwrap();
    writeln!(file, "f = x => x + y").unwrap();
    writeln!(file, "output f").unwrap();

    // Run blots on the file
    let output = Command::new("cargo")
        .args(["run", "-p", "blots", "--", file.path().to_str().unwrap()])
        .output()
        .expect("Failed to execute command");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Should succeed - captured variables are allowed
    assert!(
        !stderr.contains("error"),
        "Unexpected error in stderr: {}",
        stderr
    );
    assert!(
        stdout.contains("\"f\""),
        "Expected output to contain function 'f', got stdout: {}",
        stdout
    );
}

#[test]
fn test_output_reassigned_recursive_function_succeeds() {
    // Create a temporary file with a recursive function reassigned
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        "factorial = n => if n <= 1 then 1 else n * factorial(n - 1)"
    )
    .unwrap();
    writeln!(file, "output f = factorial").unwrap();

    // Run blots on the file
    let output = Command::new("cargo")
        .args(["run", "-p", "blots", "--", file.path().to_str().unwrap()])
        .output()
        .expect("Failed to execute command");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Should succeed - factorial exists in the environment so f is valid
    assert!(
        !stderr.contains("error"),
        "Unexpected error in stderr: {}",
        stderr
    );
    assert!(
        stdout.contains("\"f\""),
        "Expected output to contain function 'f', got stdout: {}",
        stdout
    );
}

#[test]
fn test_output_late_bound_function_succeeds() {
    // Create a temporary file with a function that references a later-defined function
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, "f = x => g(x)").unwrap();
    writeln!(file, "g = x => x + 1").unwrap();
    writeln!(file, "output f").unwrap();
    writeln!(file, "output g").unwrap();

    // Run blots on the file
    let output = Command::new("cargo")
        .args(["run", "-p", "blots", "--", file.path().to_str().unwrap()])
        .output()
        .expect("Failed to execute command");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Should succeed - g is defined when we output f
    assert!(
        !stderr.contains("error"),
        "Unexpected error in stderr: {}",
        stderr
    );
    assert!(
        stdout.contains("\"f\""),
        "Expected output to contain function 'f', got stdout: {}",
        stdout
    );
    assert!(
        stdout.contains("\"g\""),
        "Expected output to contain function 'g', got stdout: {}",
        stdout
    );
}

#[test]
fn test_output_truly_unbound_variable_fails() {
    // Create a temporary file with a function that references an undefined function
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, "f = x => h(x)").unwrap();
    writeln!(file, "output f").unwrap();

    // Run blots on the file
    let output = Command::new("cargo")
        .args(["run", "-p", "blots", "--", file.path().to_str().unwrap()])
        .output()
        .expect("Failed to execute command");

    let stderr = String::from_utf8_lossy(&output.stderr);

    // Should fail - h is never defined
    assert!(
        stderr.contains("unbound variable"),
        "Expected error about unbound variable, got stderr: {}",
        stderr
    );
    assert!(
        stderr.contains("\"h\""),
        "Expected error to mention variable 'h', got stderr: {}",
        stderr
    );
}
