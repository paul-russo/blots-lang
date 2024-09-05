use crate::parser::Rule;

/// Executes the given command.
pub fn exec_command(cmd: Rule) {
    match cmd {
        Rule::quit => {
            println!("bye!");
            std::process::exit(0);
        }
        Rule::help => {
            println!("Nothing I can do for you pal, sorry.");
        }
        Rule::history => {
            println!("Not implemented yet! Sorry.");
        }
        _ => unreachable!(),
    }
}
