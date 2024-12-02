mod cli;
mod commands;

use clap::Parser;
use cli::Args;
use commands::{exec_command, is_command};
use numpad_core::expressions::evaluate_expression;
use numpad_core::functions::FUNCTION_CALLS;
use numpad_core::parser::{get_pairs, Rule};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::time::Duration;

fn main() -> ! {
    let args = Args::parse();

    if let Some(path) = args.path {
        println!("Reading from file: {}", path);
        let content = std::fs::read_to_string(path).unwrap();
        let pairs = get_pairs(&content).unwrap();
        let variables = Rc::new(RefCell::new(HashMap::new()));
        // let flat = pairs.clone().flatten();
        // flat.for_each(|p| println!("{:?}", p));

        pairs.for_each(|pair| match pair.as_rule() {
            Rule::statement => {
                if let Some(inner_pair) = pair.into_inner().next() {
                    match inner_pair.as_rule() {
                        Rule::expression => {
                            let result = evaluate_expression(
                                inner_pair.into_inner(),
                                Rc::clone(&variables),
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
            numpad_core::parser::CALL_COUNT.load(std::sync::atomic::Ordering::Acquire)
        );
        let elapsed_ms = Duration::from_micros(
            numpad_core::parser::TOTAL_PARSE_TIME.load(std::sync::atomic::Ordering::Acquire) as u64,
        )
        .as_millis();
        println!("TOTAL_PARSE_TIME: {}ms", elapsed_ms);

        // println!("FUNCTION_CALLS: [");
        // println!(
        //     "\t{}",
        //     FUNCTION_CALLS
        //         .lock()
        //         .unwrap()
        //         .iter()
        //         .map(|s| format!("{}", s))
        //         .collect::<Vec<String>>()
        //         .join(",\n\t")
        // );
        // println!("]");

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
    let variables = Rc::new(RefCell::new(HashMap::new()));

    loop {
        let mut line = String::new();
        std::io::stdin().read_line(&mut line).unwrap();
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
                                Rc::clone(&variables),
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
