mod commands;

use commands::{exec_command, is_command};
use mir_core::expressions::evaluate_expression;
use mir_core::parser::{get_pairs, Rule};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

fn main() -> ! {
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
                println!("Error: {}", error);
                continue;
            }
        };

        pairs.for_each(|pair| {
            match pair.as_rule() {
                Rule::statement => {
                    let inner_pair = pair.into_inner().next().unwrap();

                    match inner_pair.as_rule() {
                        Rule::expression => {
                            let result = evaluate_expression(
                                inner_pair.into_inner(),
                                Rc::clone(&variables),
                                0,
                            );

                            match result {
                                Ok(value) => println!("= {}", value),
                                Err(error) => println!("Evaluation error: {}", error),
                            }
                        }
                        Rule::comment => {
                            // do nothing
                        }
                        _ => unreachable!(),
                    }
                }
                Rule::EOI => {
                    // do nothing
                }
                _ => unreachable!(),
            }
        });
    }
}
