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
use std::io::{self, IsTerminal, Read};
use std::rc::Rc;

fn main() -> ! {
    let args = Args::parse();

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

        // With -e flag, inputs is always empty
        let inputs_record = heap.borrow_mut().insert_record(IndexMap::new());
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

        // Output the collected outputs as JSON
        let json_outputs: IndexMap<String, serde_json::Value> = outputs
            .iter()
            .map(|(k, v)| (k.clone(), v.to_json()))
            .collect();
        if let Ok(json) = serde_json::to_string(&json_outputs) {
            println!("{}", json);
        }

        std::process::exit(0);
    }

    if let Some(path) = args.path {
        let content = std::fs::read_to_string(&path).unwrap();
        let pairs = get_pairs(&content).unwrap();
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        let heap = Rc::new(RefCell::new(Heap::new()));
        let mut outputs: IndexMap<String, SerializableValue> = IndexMap::new();

        // Read stdin for inputs if available
        let mut stdin_content = String::new();
        if !io::stdin().is_terminal() {
            // Try to read from stdin as JSON inputs (non-blocking check)
            let _ = io::stdin().read_to_string(&mut stdin_content);
        }

        // Parse inputs from stdin or create empty record
        let inputs_record = if !stdin_content.trim().is_empty() {
            match serde_json::from_str::<serde_json::Value>(&stdin_content) {
                Ok(json_value) => {
                    // Convert JSON object to IndexMap of Values
                    if let serde_json::Value::Object(obj) = json_value {
                        let mut inputs_map: IndexMap<String, Value> = IndexMap::new();
                        for (k, v) in obj.iter() {
                            let serializable = SerializableValue::from_json(v);
                            if let Ok(val) = serializable.to_value(&mut heap.borrow_mut()) {
                                inputs_map.insert(k.clone(), val);
                            }
                        }
                        heap.borrow_mut().insert_record(inputs_map)
                    } else {
                        // If not an object, wrap in a record with "value" key
                        let mut map = IndexMap::new();
                        let serializable = SerializableValue::from_json(&json_value);
                        if let Ok(val) = serializable.to_value(&mut heap.borrow_mut()) {
                            map.insert("value".to_string(), val);
                        }
                        heap.borrow_mut().insert_record(map)
                    }
                }
                Err(e) => {
                    eprintln!("[input error] Failed to parse JSON from stdin: {}", e);
                    std::process::exit(1);
                }
            }
        } else {
            // No stdin input, create empty record
            heap.borrow_mut().insert_record(IndexMap::new())
        };

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

        // Output the collected outputs as JSON (empty object if no outputs)
        let json_outputs: IndexMap<String, serde_json::Value> = outputs
            .iter()
            .map(|(k, v)| (k.clone(), v.to_json()))
            .collect();
        if let Ok(json) = serde_json::to_string(&json_outputs) {
            println!("{}", json);
        }

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
