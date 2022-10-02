#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;

mod commands;
mod parser;

use commands::exec_command;
use parser::{get_pairs, Rule};

fn main() -> ! {
    loop {
        let mut buffer = String::new();
        std::io::stdin().read_line(&mut buffer).unwrap();

        let outer_pair = match get_pairs(&buffer) {
            Ok(mut pairs) => {
                println!("pairs: {}", pairs);
                pairs.next().unwrap()
            }
            Err(error) => {
                println!("{}", error);
                continue;
            }
        };

        match outer_pair.as_rule() {
            Rule::command => {
                let inner_pair = outer_pair.into_inner().next().unwrap();
                exec_command(inner_pair.as_rule());
            }
            Rule::assignment => todo!(),
            Rule::expression => todo!(),
            _ => unreachable!(),
        }
    }
}
