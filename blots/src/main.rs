mod cli;
mod commands;

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
use std::process::Command;
use std::rc::Rc;
use std::time::Duration;

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

    // Handle Bun execution mode
    if args.bun {
        let content = if let Some(ref path) = args.path {
            std::fs::read_to_string(path).unwrap_or_else(|e| {
                eprintln!("Error reading file {}: {}", path, e);
                std::process::exit(1);
            })
        } else {
            eprintln!("Error: --bun flag requires a file path");
            std::process::exit(1);
        };
        
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

    // Check if stdin is being piped (not a terminal)
    use std::io::IsTerminal;
    let is_piped = !std::io::IsTerminal::is_terminal(&std::io::stdin());

    loop {
        let mut line = String::new();
        let bytes_read = std::io::stdin().read_line(&mut line).unwrap();
        
        // If piped input and we've reached EOF, exit
        if is_piped && bytes_read == 0 {
            std::process::exit(0);
        }
        
        lines.push(line.clone());

        if is_command(&line.trim()) {
            exec_command(&line.trim());
            continue;
        }

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
                                Ok(value) => println!("= {}", value),
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
