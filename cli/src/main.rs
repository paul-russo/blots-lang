mod commands;

use commands::exec_command;
use core::expressions::evaluate_expression;
use core::parser::{get_pairs, Rule};
use std::{collections::HashMap, time::Instant};

fn main() -> ! {
    let mut variables = HashMap::new();

    loop {
        let mut buffer = String::new();
        std::io::stdin().read_line(&mut buffer).unwrap();

        let start = Instant::now();
        let pairs = get_pairs(&buffer);
        let duration = start.elapsed();
        println!("Parsing took: {:?}", duration);

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
            Rule::command => {
                let cmd = outer_pair.into_inner().next().unwrap().as_rule();
                exec_command(cmd);
            }
            Rule::assignment => {
                let mut inner_pairs = outer_pair.into_inner();
                let ident = inner_pairs.next().unwrap().as_str();
                let expression = inner_pairs.next().unwrap();
                let result = evaluate_expression(expression.into_inner(), &variables);

                match result {
                    Ok(value) => {
                        variables.insert(ident.to_string(), value);
                        println!("{} = {}", ident, value)
                    }
                    Err(error) => println!("Evaluation error: {}", error),
                }
            }
            Rule::expression => {
                let result = evaluate_expression(outer_pair.into_inner(), &variables);

                match result {
                    Ok(value) => println!("= {}", value),
                    Err(error) => println!("Evaluation error: {}", error),
                }
            }
            _ => unreachable!(),
        }
    }
}
