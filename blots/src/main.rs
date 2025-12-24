mod cli;
mod commands;
mod highlighter;

use blots_core::ast::{Expr, Spanned};
use blots_core::environment::Environment;
use blots_core::expressions::{evaluate_pairs, pairs_to_expr, validate_portable_value};
use blots_core::formatter::format_expr;
use blots_core::functions::{clear_function_call_stats, get_function_call_stats};
use blots_core::heap::Heap;
use blots_core::parser::{Rule, get_pairs};
use blots_core::stats::ProfilingSummary;
use blots_core::values::{SerializableValue, Value};
use clap::{CommandFactory, Parser};
use clap_complete::{Shell, generate, shells::*};
use cli::Args;
use commands::{CommandResult, exec_command, is_command};
use highlighter::BlotsHighlighter;
use indexmap::IndexMap;
use rustyline::Editor;
use std::cell::RefCell;
use std::fs;
use std::io::{self, IsTerminal, Read, Write};
use std::rc::Rc;
use std::sync::LazyLock;

static ARGS: LazyLock<Args> = LazyLock::new(Args::parse);

/// Parse JSON string into an IndexMap of Values
fn parse_json_inputs(
    json_str: &str,
    heap: &RefCell<Heap>,
    source: &str,
    unnamed_counter: &mut usize,
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
                // If not an object, wrap in a record with a unique key
                let serializable = SerializableValue::from_json(&json_value);
                if let Ok(val) = serializable.to_value(&mut heap.borrow_mut()) {
                    let key = format!("value_{}", *unnamed_counter + 1);
                    *unnamed_counter += 1;
                    inputs_map.insert(key, val);
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

/// Evaluate Blots source code and collect outputs
fn evaluate_source(
    source: &str,
    heap: &Rc<RefCell<Heap>>,
    bindings: &Rc<Environment>,
    outputs: &mut IndexMap<String, SerializableValue>,
) -> Result<(), String> {
    let pairs = get_pairs(source).map_err(|e| format!("Parse error: {}", e))?;

    pairs.for_each(|pair| match pair.as_rule() {
        Rule::statement => {
            if let Some(inner_pair) = pair.into_inner().next() {
                match inner_pair.as_rule() {
                    Rule::expression => {
                        let result = evaluate_pairs(
                            inner_pair.into_inner(),
                            Rc::clone(heap),
                            Rc::clone(bindings),
                            0,
                            source,
                        );

                        if let Err(error) = result {
                            println!("[evaluation error] {}", error);
                            std::process::exit(1);
                        }
                    }
                    Rule::output_declaration => {
                        let inner_pairs_clone = inner_pair.clone().into_inner();

                        let result = evaluate_pairs(
                            inner_pair.clone().into_inner(),
                            Rc::clone(heap),
                            Rc::clone(bindings),
                            0,
                            source,
                        );

                        for pair in inner_pairs_clone {
                            match pair.as_rule() {
                                Rule::identifier => {
                                    let identifier = pair.as_str();
                                    if let Some(value) = bindings.get(identifier) {
                                        // Validate that the value is portable
                                        if let Err(e) = validate_portable_value(
                                            &value,
                                            &heap.borrow(),
                                            bindings,
                                        ) {
                                            eprintln!("[output error] {}", e);
                                            std::process::exit(1);
                                        } else if let Ok(serializable) =
                                            value.to_serializable_value(&heap.borrow())
                                        {
                                            outputs.insert(identifier.to_string(), serializable);
                                        }
                                    }
                                    break;
                                }
                                Rule::assignment => {
                                    if let Some(ident_pair) = pair.into_inner().next() {
                                        let identifier = ident_pair.as_str();
                                        if let Ok(value) = &result {
                                            if let Err(e) = validate_portable_value(
                                                value,
                                                &heap.borrow(),
                                                bindings,
                                            ) {
                                                eprintln!("[output error] {}", e);
                                                std::process::exit(1);
                                            } else if let Ok(serializable) =
                                                value.to_serializable_value(&heap.borrow())
                                            {
                                                outputs
                                                    .insert(identifier.to_string(), serializable);
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

    Ok(())
}

fn main() -> ! {
    // Handle shell completion generation
    if let Some(shell) = &ARGS.completions {
        let mut cmd = cli::Args::command();
        let name = "blots";
        match shell {
            Shell::Bash => generate(Bash, &mut cmd, name, &mut io::stdout()),
            Shell::Zsh => generate(Zsh, &mut cmd, name, &mut io::stdout()),
            Shell::Fish => generate(Fish, &mut cmd, name, &mut io::stdout()),
            Shell::PowerShell => generate(PowerShell, &mut cmd, name, &mut io::stdout()),
            Shell::Elvish => generate(Elvish, &mut cmd, name, &mut io::stdout()),
            _ => {
                eprintln!("Unsupported shell: {}", shell);
                std::process::exit(1);
            }
        }
        std::process::exit(0);
    }

    // Handle formatting
    if !ARGS.format.is_empty() {
        if ARGS.format.len() != 2 {
            eprintln!("Error: --format requires exactly 2 arguments: INPUT and OUTPUT");
            std::process::exit(1);
        }

        let input_path = &ARGS.format[0];
        let output_path = &ARGS.format[1];

        // Read input file
        let source = match fs::read_to_string(input_path) {
            Ok(content) => content,
            Err(e) => {
                eprintln!("Error reading input file '{}': {}", input_path, e);
                std::process::exit(1);
            }
        };

        // Parse the source
        let pairs = match get_pairs(&source) {
            Ok(pairs) => pairs,
            Err(e) => {
                eprintln!("Parse error: {}", e);
                std::process::exit(1);
            }
        };

        // Format each statement
        let mut formatted_output = String::new();
        for pair in pairs {
            match pair.as_rule() {
                Rule::statement => {
                    if let Some(inner_pair) = pair.into_inner().next() {
                        match inner_pair.as_rule() {
                            Rule::expression => match pairs_to_expr(inner_pair.into_inner()) {
                                Ok(expr) => {
                                    let formatted = format_expr(&expr, None);
                                    formatted_output.push_str(&formatted);
                                    formatted_output.push('\n');
                                }
                                Err(e) => {
                                    eprintln!("Error converting to AST: {}", e);
                                    std::process::exit(1);
                                }
                            },
                            Rule::output_declaration => {
                                match pairs_to_expr(inner_pair.into_inner()) {
                                    Ok(inner_expr) => {
                                        // Wrap in Output expression
                                        let output_expr = Spanned::dummy(Expr::Output {
                                            expr: Box::new(inner_expr),
                                        });
                                        let formatted = format_expr(&output_expr, None);
                                        formatted_output.push_str(&formatted);
                                        formatted_output.push('\n');
                                    }
                                    Err(e) => {
                                        eprintln!("Error converting to AST: {}", e);
                                        std::process::exit(1);
                                    }
                                }
                            }
                            Rule::comment => {
                                // Preserve comments as-is
                                formatted_output.push_str(inner_pair.as_str());
                                formatted_output.push('\n');
                            }
                            _ => {}
                        }
                    }
                }
                Rule::EOI => {}
                _ => {}
            }
        }

        // Write to output file
        match fs::write(output_path, formatted_output) {
            Ok(_) => {
                println!("Formatted {} -> {}", input_path, output_path);
                std::process::exit(0);
            }
            Err(e) => {
                eprintln!("Error writing output file '{}': {}", output_path, e);
                std::process::exit(1);
            }
        }
    }

    // Save output path for REPL mode (before args is moved)
    let output_path = ARGS.output.clone();

    // Check if stdin is piped or interactive
    let is_piped = !io::stdin().is_terminal();
    let should_parse_piped_inputs = is_piped && !ARGS.evaluate;

    // Track if we've consumed piped input and need to reopen stdin
    let mut stdin_consumed = false;

    // Set up our memory
    let heap = Rc::new(RefCell::new(Heap::new()));
    let bindings = Rc::new(Environment::new());
    let mut outputs: IndexMap<String, SerializableValue> = IndexMap::new();

    // Clear profiling stats if profiling is enabled
    #[cfg(not(target_arch = "wasm32"))]
    if ARGS.profile {
        clear_function_call_stats();
    }

    // Collect inputs
    // Parse inputs from stdin and/or -i flag
    let mut inputs_map: IndexMap<String, Value> = IndexMap::new();
    let mut unnamed_counter: usize = 0;

    // First, read from stdin if available (piped inputs)
    let mut stdin_content = String::new();
    if should_parse_piped_inputs {
        let _ = io::stdin().read_to_string(&mut stdin_content);
        stdin_consumed = true;

        if !stdin_content.trim().is_empty() {
            match parse_json_inputs(&stdin_content, &heap, "stdin", &mut unnamed_counter) {
                Ok(map) => inputs_map = map,
                Err(e) => {
                    eprintln!("{}", e);
                    std::process::exit(1);
                }
            }
        }
    }

    // Then, apply -i flag inputs (which override piped inputs)
    // Process each -i flag in order, with later ones overriding earlier ones
    for (index, inputs_str) in ARGS.input.iter().enumerate() {
        match parse_json_inputs(
            inputs_str,
            &heap,
            &format!("--input #{}", index + 1),
            &mut unnamed_counter,
        ) {
            Ok(map) => {
                // Merge with existing inputs (later --input override earlier ones)
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

    let inputs_record = { heap.borrow_mut().insert_record(inputs_map) };

    bindings.insert("inputs".to_string(), inputs_record);

    // Check if we should evaluate from stdin with -e flag
    if ARGS.evaluate && ARGS.file_or_source.is_none() {
        // Read stdin as Blots source code
        if io::stdin().is_terminal() {
            eprintln!("Error: --evaluate requires piped input");
            std::process::exit(1);
        }

        let mut content = String::new();
        io::stdin().read_to_string(&mut content).unwrap();

        if let Err(e) = evaluate_source(&content, &heap, &bindings, &mut outputs) {
            eprintln!("{}", e);
            std::process::exit(1);
        }

        #[cfg(not(target_arch = "wasm32"))]
        if ARGS.profile {
            let stats = get_function_call_stats();
            if !stats.is_empty() {
                let summary = ProfilingSummary::from_stats(&stats);
                summary.print_summary();
            }
        }

        write_outputs(&outputs, ARGS.output.as_ref());
        std::process::exit(0);
    }

    if let Some(path_or_source) = &ARGS.file_or_source {
        let is_file = std::path::Path::new(path_or_source).exists();

        let content = if !is_file {
            // Treat the argument as inline Blots code
            path_or_source.clone()
        } else {
            // Treat as a file path
            std::fs::read_to_string(path_or_source).unwrap_or_else(|e| {
                eprintln!("Error reading file '{}': {}", path_or_source, e);
                std::process::exit(1);
            })
        };

        if let Err(e) = evaluate_source(&content, &heap, &bindings, &mut outputs) {
            eprintln!("{}", e);
            std::process::exit(1);
        }

        #[cfg(not(target_arch = "wasm32"))]
        if ARGS.profile {
            let stats = get_function_call_stats();
            if !stats.is_empty() {
                let summary = ProfilingSummary::from_stats(&stats);
                summary.print_summary();
            }
        }

        write_outputs(&outputs, ARGS.output.as_ref());
        std::process::exit(0);
    }

    // REPL mode

    // Check if stdin was consumed (piped input) - if so, we can't start an interactive REPL
    if stdin_consumed {
        eprintln!("Error: Cannot start Interactive Mode after reading piped input.");
        eprintln!();
        eprintln!(
            "When piping JSON data, you must also specify what to do with it, by specifying a .blots file to run:"
        );
        eprintln!("    echo '{{\"x\": 42}}' | blots myfile.blots");
        eprintln!();
        eprintln!(
            "If you want to provide inputs that you can use in an interactive session, you can use the --input (-i) flag:"
        );
        eprintln!("    blots --input '{{\"x\": 42, \"y\": \"hello\"}}'");
        eprintln!();
        eprintln!(
            "You can also pipe Blots code with --evaluate (-e). This can be combined with the -i flag:"
        );
        eprintln!(
            "    echo -e 'output result = inputs.x + inputs.y' | blots --evaluate -i '{{\"x\": 42, \"y\": \"hello\"}}'"
        );

        // If outputs were collected, write them before exiting
        if !outputs.is_empty() || output_path.is_some() {
            write_outputs(&outputs, output_path.as_ref());
        }

        std::process::exit(1);
    }

    // Initialize rustyline editor for command history
    let mut rl: Option<Editor<BlotsHighlighter, rustyline::history::DefaultHistory>> =
        match Editor::with_config(rustyline::Config::builder().build()) {
            Ok(mut editor) => {
                // Set up syntax highlighting
                let highlighter = BlotsHighlighter::new(Rc::clone(&bindings));
                editor.set_helper(Some(highlighter));
                Some(editor)
            }
            Err(e) => {
                eprintln!("Error: Failed to initialize interactive editor: {}", e);
                std::process::exit(1);
            }
        };

    let mut accumulated_input = String::new();
    let mut continuation = false;

    // REPL loop
    loop {
        let prompt = if continuation { "... " } else { "> " };
        let line = if let Some(ref mut editor) = rl {
            // Use rustyline for interactive input with history
            match editor.readline(prompt) {
                Ok(input) => {
                    input + "\n" // Add newline to match read_line behavior
                }
                Err(rustyline::error::ReadlineError::Eof) => {
                    // EOF - print outputs and exit gracefully
                    if outputs.is_empty() && output_path.is_none() {
                        println!("{{}}");
                    } else {
                        write_outputs(&outputs, output_path.as_ref());
                    }
                    std::process::exit(0);
                }
                Err(rustyline::error::ReadlineError::Interrupted) => {
                    // Ctrl+C - print outputs and exit
                    if outputs.is_empty() && output_path.is_none() {
                        println!("{{}}");
                    } else {
                        write_outputs(&outputs, output_path.as_ref());
                    }
                    std::process::exit(0);
                }
                Err(e) => {
                    eprintln!("Error: Failed to read line: {}", e);
                    if stdin_consumed {
                        eprintln!(
                            "This may be because the terminal is not accessible after piped input."
                        );
                        eprintln!("Try using the -i flag instead: blots -i '{{\"x\": 42}}'");
                    }
                    std::process::exit(1);
                }
            }
        } else {
            // Fall back to basic stdin just in case
            let mut line = String::new();
            let bytes_read = std::io::stdin().read_line(&mut line).unwrap();

            // If we've reached EOF, exit
            if bytes_read == 0 {
                // Print outputs before exiting
                if outputs.is_empty() && output_path.is_none() {
                    println!("{{}}");
                } else {
                    write_outputs(&outputs, output_path.as_ref());
                }
                std::process::exit(0);
            }

            line
        };

        // Check for commands only on first line
        if !continuation && is_command(line.trim()) {
            match exec_command(line.trim()) {
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
        if let Some(ref mut editor) = rl
            && !accumulated_input.trim().is_empty()
        {
            let _ = editor.add_history_entry(accumulated_input.trim());
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
                                &accumulated_input,
                            );

                            match result {
                                Ok(value) => {
                                    let value_str = value.stringify_external(&heap.borrow());

                                    println!(
                                        "{}",
                                        rl.as_ref()
                                            .unwrap()
                                            .helper()
                                            .unwrap()
                                            .highlight_result(&value_str)
                                    );
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
                                &accumulated_input,
                            );

                            // Extract the output name from the declaration
                            for pair in inner_pairs_clone {
                                match pair.as_rule() {
                                    Rule::identifier => {
                                        // output x - reference existing binding
                                        let identifier = pair.as_str();
                                        if let Some(value) = bindings.get(identifier) {
                                            // Validate that the value is portable
                                            if let Err(e) = validate_portable_value(
                                                &value,
                                                &heap.borrow(),
                                                &bindings,
                                            ) {
                                                eprintln!("[output error] {}", e);
                                            } else if let Ok(serializable) =
                                                value.to_serializable_value(&heap.borrow())
                                            {
                                                outputs
                                                    .insert(identifier.to_string(), serializable);
                                                println!("[output '{}' recorded]", identifier);
                                            }
                                        }
                                        break;
                                    }
                                    Rule::assignment => {
                                        // output x = expr - get the identifier from the assignment
                                        if let Some(ident_pair) = pair.into_inner().next() {
                                            let identifier = ident_pair.as_str();
                                            if let Ok(value) = &result {
                                                // Validate that the value is portable
                                                if let Err(e) = validate_portable_value(
                                                    value,
                                                    &heap.borrow(),
                                                    &bindings,
                                                ) {
                                                    eprintln!("[output error] {}", e);
                                                } else if let Ok(serializable) =
                                                    value.to_serializable_value(&heap.borrow())
                                                {
                                                    outputs.insert(
                                                        identifier.to_string(),
                                                        serializable,
                                                    );
                                                    // Show value and confirmation in REPL
                                                    let value_str =
                                                        value.stringify_external(&heap.borrow());
                                                    println!(
                                                        "{}",
                                                        rl.as_ref()
                                                            .unwrap()
                                                            .helper()
                                                            .unwrap()
                                                            .highlight_result(&value_str)
                                                    );
                                                    println!("[output '{}' recorded]", identifier);
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
