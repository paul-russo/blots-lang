pub fn is_command(input: &str) -> bool {
    match input {
        "quit" | "help" | "history" => true,
        _ => false,
    }
}

/// Executes the given command.
pub fn exec_command(cmd: &str) {
    match cmd {
        "quit" => {
            println!("bye!");
            std::process::exit(0);
        }
        "help" => {
            println!("Nothing I can do for you pal, sorry.");
        }
        "history" => {
            println!("Not implemented yet! Sorry.");
        }
        _ => unreachable!(),
    }
}
