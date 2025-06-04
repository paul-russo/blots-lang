mod cli;
mod commands;
mod highlighter;

use blots_core::expressions::evaluate_expression;
use blots_core::functions::FUNCTION_CALLS;
use blots_core::heap::Heap;
use blots_core::parser::{get_pairs, Rule};
use blots_core::transpiler::{transpile_to_js, transpile_to_js_with_inline_eval};
use clap::Parser;
use cli::Args;
use commands::{exec_command, is_command};
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::{BufWriter, Write, IsTerminal};
use std::process::Command;
use rustyline::Editor;
use highlighter::BlotsHighlighter;
use std::rc::Rc;
use std::time::Duration;

fn execute_js_with_bun(js_code: &str) -> Result<String, String> {
    let child = Command::new("bun")
        .arg("-")
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn();
        
    match child {
        Ok(mut process) => {
            // Write the JavaScript code to stdin
            if let Some(stdin) = process.stdin.take() {
                let mut writer = BufWriter::new(stdin);
                if let Err(e) = writer.write_all(js_code.as_bytes()) {
                    return Err(format!("Error writing to Bun stdin: {}", e));
                }
                if let Err(e) = writer.flush() {
                    return Err(format!("Error flushing to Bun stdin: {}", e));
                }
            }
            
            // Wait for the process to complete and get output
            match process.wait_with_output() {
                Ok(output) => {
                    let stdout = String::from_utf8_lossy(&output.stdout);
                    let stderr = String::from_utf8_lossy(&output.stderr);
                    
                    if output.status.success() {
                        Ok(stdout.to_string())
                    } else {
                        Err(stderr.to_string())
                    }
                }
                Err(e) => Err(format!("Error waiting for Bun process: {}", e))
            }
        }
        Err(e) => Err(format!("Error executing Bun: {}. Make sure Bun is installed and in your PATH.", e))
    }
}

fn main() -> ! {
    let args = Args::parse();

    // Handle transpile mode first
    if args.transpile {
        let content = if let Some(ref path) = args.path {
            std::fs::read_to_string(path).unwrap()
        } else {
            // Handle transpile mode with stdin input
            use std::io::Read;
            let mut content = String::new();
            std::io::stdin().read_to_string(&mut content).unwrap();
            content
        };
        
        let transpile_result = if args.inline_eval {
            transpile_to_js_with_inline_eval(&content)
        } else {
            transpile_to_js(&content)
        };
        
        match transpile_result {
            Ok(js_code) => {
                if let Some(output_path) = args.output {
                    std::fs::write(output_path, js_code).unwrap();
                } else {
                    println!("{}", js_code);
                }
            }
            Err(e) => {
                eprintln!("Transpilation error: {}", e);
                std::process::exit(1);
            }
        }
        std::process::exit(0);
    }

    // Handle JavaScript execution mode for files (default unless --slow is specified)
    if !args.slow && args.path.is_some() {
        let content = std::fs::read_to_string(args.path.as_ref().unwrap()).unwrap_or_else(|e| {
            eprintln!("Error reading file {}: {}", args.path.as_ref().unwrap(), e);
            std::process::exit(1);
        });
        
        let transpile_result = if args.inline_eval {
            transpile_to_js_with_inline_eval(&content)
        } else {
            transpile_to_js(&content)
        };
        
        match transpile_result {
            Ok(js_code) => {
                // Execute the JavaScript by piping it to Bun
                let child = Command::new("bun")
                    .arg("-")
                    .stdin(std::process::Stdio::piped())
                    .stdout(std::process::Stdio::piped())
                    .stderr(std::process::Stdio::piped())
                    .spawn();
                    
                match child {
                    Ok(mut process) => {
                        // Write the JavaScript code to stdin
                        if let Some(stdin) = process.stdin.take() {
                            use std::io::{BufWriter, Write};
                            let mut writer = BufWriter::new(stdin);
                            if let Err(e) = writer.write_all(js_code.as_bytes()) {
                                eprintln!("Error writing to Bun stdin: {}", e);
                                std::process::exit(1);
                            }
                            if let Err(e) = writer.flush() {
                                eprintln!("Error flushing to Bun stdin: {}", e);
                                std::process::exit(1);
                            }
                        }
                        
                        // Wait for the process to complete and get output
                        match process.wait_with_output() {
                            Ok(output) => {
                                // Print stdout
                                if !output.stdout.is_empty() {
                                    print!("{}", String::from_utf8_lossy(&output.stdout));
                                }
                                
                                // Print stderr
                                if !output.stderr.is_empty() {
                                    eprint!("{}", String::from_utf8_lossy(&output.stderr));
                                }
                                
                                std::process::exit(output.status.code().unwrap_or(1));
                            }
                            Err(e) => {
                                eprintln!("Error waiting for Bun process: {}", e);
                                std::process::exit(1);
                            }
                        }
                    }
                    Err(e) => {
                        eprintln!("Error executing Bun: {}. Make sure Bun is installed and in your PATH.", e);
                        std::process::exit(1);
                    }
                }
            }
            Err(e) => {
                eprintln!("Transpilation error: {}", e);
                std::process::exit(1);
            }
        }
    }

    if let Some(path) = args.path {
        let content = std::fs::read_to_string(&path).unwrap();
        
        println!("Reading from file: {}", path);
        let pairs = get_pairs(&content).unwrap();
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        let heap = Rc::new(RefCell::new(Heap::new()));

        pairs.for_each(|pair| match pair.as_rule() {
            Rule::statement => {
                if let Some(inner_pair) = pair.into_inner().next() {
                    match inner_pair.as_rule() {
                        Rule::expression => {
                            let result = evaluate_expression(
                                inner_pair.into_inner(),
                                Rc::clone(&heap),
                                Rc::clone(&bindings),
                                0,
                            );

                            match result {
                                Err(error) => {
                                    println!("[evaluation error] {}", error);
                                    std::process::exit(1);
                                }
                                _ => {}
                            }
                        }
                        _ => unreachable!("unexpected rule: {:?}", inner_pair.as_rule()),
                    }
                }
            }
            Rule::EOI => {}
            rule => unreachable!("unexpected rule: {:?}", rule),
        });

        println!("---------------------------------------------");
        println!(
            "PARSE_COUNT: {}",
            blots_core::parser::CALL_COUNT.load(std::sync::atomic::Ordering::Acquire)
        );
        let elapsed_ms = Duration::from_micros(
            blots_core::parser::TOTAL_PARSE_TIME.load(std::sync::atomic::Ordering::Acquire) as u64,
        )
        .as_millis();
        println!("TOTAL_PARSE_TIME: {}ms", elapsed_ms);

        println!(
            "Total function calls: {}",
            FUNCTION_CALLS.lock().unwrap().len()
        );
        println!(
            "Total function call time: {}ms",
            FUNCTION_CALLS.lock().unwrap().iter().fold(0.0, |acc, s| acc
                + ((s.end - s.start).as_secs_f64() * 1_000.0))
        );

        std::process::exit(0);
    }

    let mut lines: Vec<String> = Vec::new();
    let heap = Rc::new(RefCell::new(Heap::new()));
    let bindings = Rc::new(RefCell::new(HashMap::new()));
    
    // For JS mode (default unless --slow specified), accumulate transpiled code
    let mut js_accumulated_code = if !args.slow {
        // Get just the runtime library without any user code
        let runtime = transpile_to_js("").unwrap_or_default();
        // Extract everything up to the user code section
        if let Some(end) = runtime.rfind("// Make inputs available") {
            let lines: Vec<&str> = runtime[..end].lines().collect();
            let mut result = lines.join("\n");
            result.push_str("\n// Make inputs available as a bare identifier (not just globalThis.inputs)\nif (typeof inputs === 'undefined') {\n    var inputs = globalThis.inputs;\n}\n");
            Some(result)
        } else {
            Some(runtime)
        }
    } else {
        None
    };

    // Check if stdin is being piped (not a terminal)
    let is_piped = !IsTerminal::is_terminal(&std::io::stdin());
    
    // Initialize rustyline editor for command history (only if not piped)
    let highlighter = BlotsHighlighter::new();
    let mut rl: Option<Editor<BlotsHighlighter, rustyline::history::DefaultHistory>> = if !is_piped {
        match Editor::with_config(rustyline::Config::builder().build()) {
            Ok(mut editor) => {
                // Set up syntax highlighting
                editor.set_helper(Some(BlotsHighlighter::new()));
                Some(editor)
            },
            Err(_) => None, // Fall back to basic input if rustyline fails
        }
    } else {
        None
    };

    loop {
        let line = if let Some(ref mut editor) = rl {
            // Use rustyline for interactive input with history
            match editor.readline("> ") {
                Ok(input) => {
                    if !input.trim().is_empty() {
                        let _ = editor.add_history_entry(&input);
                    }
                    input + "\n" // Add newline to match read_line behavior
                }
                Err(_) => {
                    std::process::exit(0); // User pressed Ctrl+C or EOF
                }
            }
        } else {
            // Fall back to basic stdin for piped input
            let mut line = String::new();
            let bytes_read = std::io::stdin().read_line(&mut line).unwrap();
            
            // If piped input and we've reached EOF, exit
            if is_piped && bytes_read == 0 {
                std::process::exit(0);
            }
            
            line
        };
        
        if is_command(&line.trim()) {
            exec_command(&line.trim());
            continue;
        }

        if let Some(ref mut _js_code) = js_accumulated_code {
            // JavaScript evaluation mode with state accumulation (default)
            
            // First, try to transpile just the current line to check for syntax errors
            let current_line = line.clone();
            match transpile_to_js(&current_line) {
                Ok(_) => {
                    // Line transpiles successfully, now test it with the accumulated state
                    let mut test_lines = lines.clone();
                    test_lines.push(current_line.clone());
                    
                    // Transpile the test accumulated program with inline evaluation 
                    let test_accumulated_blots = test_lines.join("");
                    match transpile_to_js_with_inline_eval(&test_accumulated_blots) {
                        Ok(full_js) => {
                            // Add code to show the last evaluated expression with function formatting
                            let eval_js = format!("{}\n// Show the last result with proper function formatting\nif (typeof $$results !== 'undefined' && $$results.values) {{\n    const keys = Object.keys($$results.values);\n    if (keys.length > 0) {{\n        const lastKey = keys[keys.length - 1];\n        const result = $$results.values[lastKey];\n        if (result !== undefined) {{\n            if (typeof result === 'function') {{\n                // Check if this is a built-in function\n                const funcStr = result.toString();\n                if (funcStr.includes('function $$')) {{\n                    // Extract built-in function name\n                    const match = funcStr.match(/function \\$\\$(\\w+)\\s*\\(/);\n                    if (match) {{\n                        console.log('=', `BuiltIn(${{match[1]}})`);\n                    }} else {{\n                        console.log('=', 'BuiltIn');\n                    }}\n                }} else if (result.$$originalSource) {{\n                    // User-defined lambda with original source\n                    console.log('=', result.$$originalSource);\n                }} else {{\n                    // Other function types\n                    console.log('=', '[Function]');\n                }}\n            }} else {{\n                console.log('=', result);\n            }}\n        }}\n    }}\n}}", full_js);
                            
                            match execute_js_with_bun(&eval_js) {
                                Ok(output) => {
                                    // Runtime execution succeeded - now it's safe to add to accumulated lines
                                    lines.push(current_line);
                                    
                                    if !output.trim().is_empty() {
                                        // Parse and highlight the result
                                        let output_lines: Vec<&str> = output.trim().lines().collect();
                                        for line in output_lines {
                                            if line.starts_with("= ") {
                                                // This is a result line, highlight it
                                                let result_value = &line[2..]; // Remove "= " prefix
                                                if !is_piped {
                                                    println!("{}", highlighter.highlight_result(result_value));
                                                } else {
                                                    println!("= {}", result_value);
                                                }
                                            } else {
                                                // Other output (like print statements), just pass through
                                                println!("{}", line);
                                            }
                                        }
                                    }
                                }
                                Err(error) => {
                                    // Runtime error - don't add the line to accumulated state
                                    println!("[js error] {}", error.trim());
                                }
                            }
                        }
                        Err(error) => {
                            // Transpile error with accumulated state - don't add the line
                            println!("[transpile error] {}", error);
                        }
                    }
                }
                Err(error) => {
                    // Line has syntax error, don't add it to accumulated lines
                    println!("[transpile error] {}", error);
                }
            }
        } else {
            // Native Blots evaluation mode (--slow flag specified)
            let pairs = match get_pairs(&line) {
                Ok(pairs) => pairs,
                Err(error) => {
                    println!("[parse error] {}", error);
                    continue;
                }
            };

            pairs.for_each(|pair| match pair.as_rule() {
                Rule::statement => {
                    if let Some(inner_pair) = pair.into_inner().next() {
                        match inner_pair.as_rule() {
                            Rule::expression => {
                                let result = evaluate_expression(
                                    inner_pair.into_inner(),
                                    Rc::clone(&heap),
                                    Rc::clone(&bindings),
                                    0,
                                );

                                match result {
                                    Ok(value) => {
                                        let value_str = format!("{}", value);
                                        if !is_piped {
                                            println!("{}", highlighter.highlight_result(&value_str));
                                        } else {
                                            println!("= {}", value_str);
                                        }
                                    },
                                    Err(error) => println!("[evaluation error] {}", error),
                                }
                            }
                            _ => unreachable!("unexpected rule: {:?}", inner_pair.as_rule()),
                        }
                    }
                }
                Rule::EOI => {}
                rule => unreachable!("unexpected rule: {:?}", rule),
            });
        }
    }
}
