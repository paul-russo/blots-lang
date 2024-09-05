#[macro_use]
extern crate pest_derive;

mod commands;
mod expressions;
mod parser;

use commands::exec_command;
use expressions::evaluate_expression;
use parser::{get_pairs, Rule};
use std::time::Instant;

fn main() -> ! {
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
                let inner_pair = outer_pair.into_inner().next().unwrap();
                exec_command(inner_pair.as_rule());
            }
            Rule::assignment => todo!(),
            Rule::expression => {
                let result = evaluate_expression(outer_pair.into_inner());

                match result {
                    Ok(value) => println!("= {}", value),
                    Err(error) => println!("Error: {}", error),
                }
            }
            _ => unreachable!(),
        }
    }
}
