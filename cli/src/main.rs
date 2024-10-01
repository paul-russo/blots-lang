mod commands;

use commands::{exec_command, is_command};
use mir_core::expressions::evaluate_expression;
use mir_core::parser::{get_pairs, Rule};
use std::collections::HashMap;
use std::time::Instant;

fn main() -> ! {
    let mut lines: Vec<String> = Vec::new();
    let mut variables = HashMap::new();

    loop {
        let mut line = String::new();
        std::io::stdin().read_line(&mut line).unwrap();
        lines.push(line.clone());

        if is_command(&line.trim()) {
            exec_command(&line.trim());
            continue;
        }

        let pairs = get_pairs(&line);

        let outer_pair = match pairs {
            Ok(mut pairs) => {
                // println!("pairs: {}", pairs);
                pairs.next().unwrap()
            }
            Err(error) => {
                println!("Error: {}", error);
                continue;
            }
        };

        match outer_pair.as_rule() {
            Rule::expression => {
                let now = Instant::now();
                let result = evaluate_expression(outer_pair.into_inner(), &mut variables);

                match result {
                    Ok(value) => println!("= {}", value),
                    Err(error) => println!("Evaluation error: {}", error),
                }

                println!(
                    "evaluation took: {}ms",
                    (now.elapsed().as_micros() as f64) / 1000.0
                );
            }
            Rule::comment => {
                // do nothing
            }
            _ => unreachable!(),
        }
    }
}
