mod cli;
mod commands;
mod highlighter;

use blots_core::expressions::evaluate_pairs;
use blots_core::heap::Heap;
use blots_core::parser::{get_pairs, Rule};
use blots_core::values::{SerializableValue, Value};
use clap::Parser;
use cli::Args;
use commands::{exec_command, is_command, CommandResult};
use highlighter::BlotsHighlighter;
use indexmap::IndexMap;
use rustyline::Editor;
use serde_json;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::io::{self, IsTerminal, Read, Write};
use std::rc::Rc;

/// Parse JSON string into an IndexMap of Values
fn parse_json_inputs(
    json_str: &str,
    heap: &RefCell<Heap>,
    source: &str,
) -> Result<IndexMap<String, Value>, String> {
    match serde_json::from_str::<serde_json::Value>(json_str) {
        Ok(json_value) => {
            let mut inputs_map: IndexMap<String, Value> = IndexMap::new();

            if let serde_json::Value::Object(obj) = json_value {
                for (k, v) in obj.iter() {
                    let serializable = SerializableValue::from_json(v);
                    if let Ok(val) = serializable.to_value(&mut heap.borrow_mut()) {
                        inputs_map.insert(k.clone(), val);
                    }
                }
            } else {
                // If not an object, wrap in a record with "value" key
                let serializable = SerializableValue::from_json(&json_value);
                if let Ok(val) = serializable.to_value(&mut heap.borrow_mut()) {
                    inputs_map.insert("value".to_string(), val);
                }
            }

            Ok(inputs_map)
        }
        Err(e) => Err(format!(
            "[input error] Failed to parse JSON from {}: {}",
            source, e
        )),
    }
}

/// Write outputs to file or stdout
fn write_outputs(outputs: &IndexMap<String, SerializableValue>, output_path: Option<&String>) {
    let json_outputs: IndexMap<String, serde_json::Value> = outputs
        .iter()
        .map(|(k, v)| (k.clone(), v.to_json()))
        .collect();

    if let Ok(json) = serde_json::to_string(&json_outputs) {
        if let Some(path) = output_path {
            // Write to file
            match fs::File::create(path) {
                Ok(mut file) => {
                    if let Err(e) = file.write_all(json.as_bytes()) {
                        eprintln!("[output error] Failed to write to file {}: {}", path, e);
                        std::process::exit(1);
                    }
                    eprintln!("Output saved to {}", path);
                }
                Err(e) => {
                    eprintln!("[output error] Failed to create file {}: {}", path, e);
                    std::process::exit(1);
                }
            }
        } else {
            // Write to stdout
            println!("{}", json);
        }
    }
}

fn main() -> ! {
    let args = Args::parse();

    // Save output path for REPL mode (before args is moved)
    let output_path = args.output.clone();

    // Handle subcommands (currently none)

    // Check if we should evaluate from stdin with -e flag
    if args.evaluate && args.path.is_none() {
        // Read stdin as Blots source code
        if io::stdin().is_terminal() {
            eprintln!("Error: --evaluate requires piped input");
            std::process::exit(1);
        }

        let mut content = String::new();
        io::stdin().read_to_string(&mut content).unwrap();

        let pairs = get_pairs(&content).unwrap();
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        let heap = Rc::new(RefCell::new(Heap::new()));
        let mut outputs: IndexMap<String, SerializableValue> = IndexMap::new();

        // Parse inputs from -i flag if provided
        let inputs_map = if let Some(inputs_str) = &args.inputs {
            match parse_json_inputs(inputs_str, &heap, "--inputs") {
                Ok(map) => map,
                Err(e) => {
                    eprintln!("{}", e);
                    std::process::exit(1);
                }
            }
        } else {
            IndexMap::new()
        };

        let inputs_record = heap.borrow_mut().insert_record(inputs_map);

        bindings
            .borrow_mut()
            .insert("inputs".to_string(), inputs_record);

        // Process the code (same as file mode)
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
                            // Same output handling as file mode
                            let inner_pairs_clone = inner_pair.clone().into_inner();

                            let result = evaluate_pairs(
                                inner_pair.clone().into_inner(),
                                Rc::clone(&heap),
                                Rc::clone(&bindings),
                                0,
                            );

                            for pair in inner_pairs_clone {
                                match pair.as_rule() {
                                    Rule::identifier => {
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

                            if let Err(error) = result {
                                println!("[evaluation error] {}", error);
                                std::process::exit(1);
                            }
                        }
                        Rule::comment => {}
                        _ => unreachable!("unexpected rule: {:?}", inner_pair.as_rule()),
                    }
                }
            }
            Rule::EOI => {}
            rule => unreachable!("unexpected rule: {:?}", rule),
        });

        // Output the collected outputs
        write_outputs(&outputs, args.output.as_ref());

        std::process::exit(0);
    }

    if let Some(path) = args.path {
        let content = std::fs::read_to_string(&path).unwrap();
        let pairs = get_pairs(&content).unwrap();
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        let heap = Rc::new(RefCell::new(Heap::new()));
        let mut outputs: IndexMap<String, SerializableValue> = IndexMap::new();

        // Parse inputs from stdin and/or -i flag
        let mut inputs_map: IndexMap<String, Value> = IndexMap::new();

        // First, read from stdin if available (piped inputs)
        let mut stdin_content = String::new();
        if !io::stdin().is_terminal() {
            let _ = io::stdin().read_to_string(&mut stdin_content);

            if !stdin_content.trim().is_empty() {
                match parse_json_inputs(&stdin_content, &heap, "stdin") {
                    Ok(map) => inputs_map = map,
                    Err(e) => {
                        eprintln!("{}", e);
                        std::process::exit(1);
                    }
                }
            }
        }

        // Then, apply -i flag inputs (which override piped inputs)
        if let Some(inputs_str) = &args.inputs {
            match parse_json_inputs(inputs_str, &heap, "--inputs") {
                Ok(map) => {
                    // Merge with existing inputs (--inputs overrides stdin)
                    for (k, v) in map {
                        inputs_map.insert(k, v);
                    }
                }
                Err(e) => {
                    eprintln!("{}", e);
                    std::process::exit(1);
                }
            }
        }

        let inputs_record = heap.borrow_mut().insert_record(inputs_map);

        // Add inputs to bindings
        bindings
            .borrow_mut()
            .insert("inputs".to_string(), inputs_record);

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

        // Output the collected outputs
        write_outputs(&outputs, args.output.as_ref());

        std::process::exit(0);
    }

    // REPL mode requires an interactive terminal
    if !io::stdin().is_terminal() {
        eprintln!("Error: REPL mode requires an interactive terminal.");
        eprintln!("To process JSON input, use: echo '{{}}' | blots file.blot");
        std::process::exit(1);
    }

    let heap = Rc::new(RefCell::new(Heap::new()));
    let bindings = Rc::new(RefCell::new(HashMap::new()));
    let mut outputs: IndexMap<String, SerializableValue> = IndexMap::new();

    // In REPL mode, inputs is always an empty record
    let inputs_record = heap.borrow_mut().insert_record(IndexMap::new());

    // Add inputs to bindings
    bindings
        .borrow_mut()
        .insert("inputs".to_string(), inputs_record);

    // At this point we know stdin is a terminal
    let is_piped = false;

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
                    // User pressed Ctrl+C or EOF - print outputs and exit
                    if outputs.is_empty() && output_path.is_none() {
                        println!("{{}}");
                    } else {
                        write_outputs(&outputs, output_path.as_ref());
                    }
                    std::process::exit(0);
                }
            }
        } else {
            // Fall back to basic stdin for piped input
            let mut line = String::new();
            let bytes_read = std::io::stdin().read_line(&mut line).unwrap();

            // If piped input and we've reached EOF, exit
            if is_piped && bytes_read == 0 {
                // Print outputs before exiting
                if !outputs.is_empty() {
                    let json_outputs: IndexMap<String, serde_json::Value> = outputs
                        .iter()
                        .map(|(k, v)| (k.clone(), v.to_json()))
                        .collect();
                    if let Ok(json) = serde_json::to_string(&json_outputs) {
                        println!("{}", json);
                    }
                } else {
                    println!("{{}}");
                }
                std::process::exit(0);
            }

            line
        };

        // Check for commands only on first line
        if !continuation && is_command(&line.trim()) {
            match exec_command(&line.trim()) {
                CommandResult::Quit => {
                    // Print outputs before exiting
                    if outputs.is_empty() && output_path.is_none() {
                        println!("{{}}");
                    } else {
                        write_outputs(&outputs, output_path.as_ref());
                    }
                    std::process::exit(0);
                }
                CommandResult::Continue => continue,
            }
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
                        Rule::output_declaration => {
                            // Handle output declarations in REPL
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
                                                // Show confirmation in REPL
                                                if !is_piped {
                                                    println!("[output '{}' recorded]", identifier);
                                                }
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
                                                    // Show value and confirmation in REPL
                                                    if !is_piped {
                                                        let value_str =
                                                            value.stringify(&heap.borrow());
                                                        println!(
                                                            "{}",
                                                            highlighter
                                                                .highlight_result(&value_str)
                                                        );
                                                        println!(
                                                            "[output '{}' recorded]",
                                                            identifier
                                                        );
                                                    }
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

        // Reset for next input
        accumulated_input.clear();
        continuation = false;
    }
}
