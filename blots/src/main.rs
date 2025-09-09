mod cli;
mod commands;
mod highlighter;

use blots_core::expressions::evaluate_pairs;
use blots_core::heap::Heap;
use blots_core::parser::{get_pairs, Rule};
use blots_core::values::SerializableValue;
use clap::Parser;
use cli::Args;
use commands::{exec_command, is_command};
use highlighter::BlotsHighlighter;
use indexmap::IndexMap;
use rustyline::Editor;
use serde_json;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::IsTerminal;
use std::rc::Rc;

// Convert SerializableValue to clean JSON
fn to_json_value(value: &SerializableValue) -> serde_json::Value {
    match value {
        SerializableValue::Number(n) => serde_json::Value::Number(
            serde_json::Number::from_f64(*n).unwrap_or_else(|| serde_json::Number::from(0)),
        ),
        SerializableValue::Bool(b) => serde_json::Value::Bool(*b),
        SerializableValue::Null => serde_json::Value::Null,
        SerializableValue::String(s) => serde_json::Value::String(s.clone()),
        SerializableValue::List(items) => {
            serde_json::Value::Array(items.iter().map(to_json_value).collect())
        }
        SerializableValue::Record(fields) => {
            let map: serde_json::Map<String, serde_json::Value> = fields
                .iter()
                .map(|(k, v)| (k.clone(), to_json_value(v)))
                .collect();
            serde_json::Value::Object(map)
        }
        SerializableValue::Lambda(_) => serde_json::Value::String("<function>".to_string()),
        SerializableValue::BuiltIn(name) => {
            serde_json::Value::String(format!("<builtin: {}>", name))
        }
    }
}

fn main() -> ! {
    let args = Args::parse();

    // Handle subcommands (currently none)

    if let Some(path) = args.path {
        let content = std::fs::read_to_string(&path).unwrap();
        let pairs = get_pairs(&content).unwrap();
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        let heap = Rc::new(RefCell::new(Heap::new()));
        let mut outputs: IndexMap<String, SerializableValue> = IndexMap::new();

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
                            // The output_declaration contains the entire "output x" or "output x = expr"
                            // We need to evaluate it like a regular expression/assignment but track the output
                            let inner_pairs_clone = inner_pair.clone().into_inner();

                            // Evaluate the entire output declaration
                            let result = evaluate_pairs(
                                inner_pair.clone().into_inner(),
                                Rc::clone(&heap),
                                Rc::clone(&bindings),
                                0,
                            );

                            // Extract the output name from the declaration
                            for pair in inner_pairs_clone {
                                match pair.as_rule() {
                                    Rule::identifier => {
                                        // output x - reference existing binding
                                        let identifier = pair.as_str();
                                        if let Some(value) = bindings.borrow().get(identifier) {
                                            if let Ok(serializable) =
                                                value.to_serializable_value(&heap.borrow())
                                            {
                                                outputs
                                                    .insert(identifier.to_string(), serializable);
                                            }
                                        }
                                        break;
                                    }
                                    Rule::assignment => {
                                        // output x = expr - get the identifier from the assignment
                                        if let Some(ident_pair) = pair.into_inner().next() {
                                            let identifier = ident_pair.as_str();
                                            if let Ok(value) = &result {
                                                if let Ok(serializable) =
                                                    value.to_serializable_value(&heap.borrow())
                                                {
                                                    outputs.insert(
                                                        identifier.to_string(),
                                                        serializable,
                                                    );
                                                }
                                            }
                                        }
                                        break;
                                    }
                                    _ => {}
                                }
                            }

                            // Check for errors in evaluation
                            if let Err(error) = result {
                                println!("[evaluation error] {}", error);
                                std::process::exit(1);
                            }
                        }
                        Rule::comment => {} // Ignore comments
                        _ => unreachable!("unexpected rule: {:?}", inner_pair.as_rule()),
                    }
                }
            }
            Rule::EOI => {}
            rule => unreachable!("unexpected rule: {:?}", rule),
        });

        // Output the collected outputs as JSON (empty object if no outputs)
        let json_outputs: IndexMap<String, serde_json::Value> = outputs
            .iter()
            .map(|(k, v)| (k.clone(), to_json_value(v)))
            .collect();
        if let Ok(json) = serde_json::to_string(&json_outputs) {
            println!("{}", json);
        }

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
