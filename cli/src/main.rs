mod commands;

use commands::{exec_command, is_command};
use mir_core::expressions::evaluate_expression;
use mir_core::functions::{is_built_in_function, UserDefinedFunctionDef};
use mir_core::parser::{get_pairs, Rule};
use std::{collections::HashMap, time::Instant};

fn main() -> ! {
    let mut lines: Vec<String> = Vec::new();
    let mut variables = HashMap::new();
    let mut function_defs = HashMap::new();

    loop {
        let mut line = String::new();
        std::io::stdin().read_line(&mut line).unwrap();
        lines.push(line.clone());

        let start = Instant::now();

        if is_command(&line.trim()) {
            exec_command(&line.trim());
            continue;
        }

        let pairs = get_pairs(&line);
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
            Rule::assignment => {
                let mut inner_pairs = outer_pair.into_inner();
                let ident = inner_pairs.next().unwrap().as_str();
                let expression = inner_pairs.next().unwrap();
                let result =
                    evaluate_expression(expression.into_inner(), &variables, &function_defs);

                match result {
                    Ok(value) => {
                        variables.insert(ident.to_string(), value.clone());
                        println!("{} = {}", ident, value)
                    }
                    Err(error) => println!("Evaluation error: {}", error),
                }
            }
            Rule::expression => {
                let result =
                    evaluate_expression(outer_pair.into_inner(), &variables, &function_defs);

                match result {
                    Ok(value) => println!("= {}", value),
                    Err(error) => println!("Evaluation error: {}", error),
                }
            }
            Rule::function_definition => {
                let mut inner_pairs = outer_pair.into_inner();
                let ident = inner_pairs.next().unwrap().as_str();
                let args = inner_pairs.next().unwrap().into_inner();
                let body = inner_pairs.next().unwrap().as_str().trim();
                let args = args.map(|arg| arg.as_str().to_string()).collect();

                if is_built_in_function(ident) {
                    println!(
                        "Error: {} is a built-in function and cannot be redefined",
                        ident
                    );
                    continue;
                }

                function_defs.insert(
                    ident.to_string(),
                    UserDefinedFunctionDef {
                        name: ident.to_string(),
                        args,
                        body: body.to_string(),
                    },
                );
            }
            Rule::comment => {
                // do nothing
            }
            _ => unreachable!(),
        }
    }
}
