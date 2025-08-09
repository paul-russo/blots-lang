mod cli;
mod commands;
mod highlighter;

use blots_core::expressions::evaluate_pairs;
use blots_core::functions::FUNCTION_CALLS;
use blots_core::heap::Heap;
use blots_core::parser::{get_pairs, Rule};
use clap::Parser;
use cli::Args;
use commands::{exec_command, is_command};
use highlighter::BlotsHighlighter;
use rustyline::Editor;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::IsTerminal;
use std::rc::Rc;
use std::time::Duration;

fn main() -> ! {
    let args = Args::parse();

    // Handle subcommands (currently none)

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
                            let result = evaluate_pairs(
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
                        Rule::output_declaration => {
                            // Output declarations are ignored in the CLI
                            // They're only meaningful in the Blots application
                        }
                        Rule::comment => {} // Ignore comments
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

    let heap = Rc::new(RefCell::new(Heap::new()));
    let bindings = Rc::new(RefCell::new(HashMap::new()));

    // Check if stdin is being piped (not a terminal)
    let is_piped = !IsTerminal::is_terminal(&std::io::stdin());

    // Initialize rustyline editor for command history (only if not piped)
    let highlighter = BlotsHighlighter::new();
    let mut rl: Option<Editor<BlotsHighlighter, rustyline::history::DefaultHistory>> = if !is_piped
    {
        match Editor::with_config(rustyline::Config::builder().build()) {
            Ok(mut editor) => {
                // Set up syntax highlighting
                editor.set_helper(Some(BlotsHighlighter::new()));
                Some(editor)
            }
            Err(_) => None, // Fall back to basic input if rustyline fails
        }
    } else {
        None
    };

    let mut accumulated_input = String::new();
    let mut continuation = false;

    loop {
        let prompt = if continuation { "... " } else { "> " };
        let line = if let Some(ref mut editor) = rl {
            // Use rustyline for interactive input with history
            match editor.readline(prompt) {
                Ok(input) => {
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

        // Check for commands only on first line
        if !continuation && is_command(&line.trim()) {
            exec_command(&line.trim());
            continue;
        }

        // Accumulate the input
        accumulated_input.push_str(&line);

        // Try to parse the accumulated input
        let pairs = match get_pairs(&accumulated_input) {
            Ok(pairs) => pairs,
            Err(error) => {
                // Check for unmatched brackets
                if accumulated_input.matches('(').count() > accumulated_input.matches(')').count()
                    || accumulated_input.matches('[').count()
                        > accumulated_input.matches(']').count()
                    || accumulated_input.matches('{').count()
                        > accumulated_input.matches('}').count()
                {
                    continuation = true;
                    continue;
                } else {
                    println!("[parse error] {}", error);
                    accumulated_input.clear();
                    continuation = false;
                    continue;
                }
            }
        };

        // Add to history if using rustyline
        if let Some(ref mut editor) = rl {
            if !accumulated_input.trim().is_empty() {
                let _ = editor.add_history_entry(accumulated_input.trim());
            }
        }

        pairs.for_each(|pair| match pair.as_rule() {
            Rule::statement => {
                if let Some(inner_pair) = pair.into_inner().next() {
                    match inner_pair.as_rule() {
                        Rule::expression => {
                            let result = evaluate_pairs(
                                inner_pair.into_inner(),
                                Rc::clone(&heap),
                                Rc::clone(&bindings),
                                0,
                            );

                            match result {
                                Ok(value) => {
                                    let value_str = value.stringify(&heap.borrow());

                                    if !is_piped {
                                        println!("{}", highlighter.highlight_result(&value_str));
                                    } else {
                                        println!("= {}", value_str);
                                    }
                                }
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

        // Reset for next input
        accumulated_input.clear();
        continuation = false;
    }
}
