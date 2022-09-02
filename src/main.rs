#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;

mod evaluator;
mod format;
mod parser;

use evaluator::evaluate;
use format::PrettyFormat;
use parser::parse_expression;

fn main() -> ! {
    loop {
        let mut buffer = String::new();
        std::io::stdin().read_line(&mut buffer).unwrap();

        match parse_expression(&buffer) {
            Ok(pairs) => {
                println!("{}", pairs);

                let result = evaluate(pairs);
                println!("result: {}", result);
            }
            Err(error) => {
                println!("{:?}", error);
            }
        }
    }
}
