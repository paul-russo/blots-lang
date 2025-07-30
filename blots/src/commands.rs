pub fn is_command(input: &str) -> bool {
    match input {
        "quit" | "help" => true,
        _ => false,
    }
}

/// Executes the given command.
pub fn exec_command(cmd: &str) {
    match cmd {
        "quit" | "exit" => {
            println!("bye!");
            std::process::exit(0);
        }
        "help" => {
            println!("Blots REPL Commands:");
            println!("  help        Show this help message");
            println!("  quit, exit  Exit the REPL");
            println!();
            println!("Multi-line expressions:");
            println!("  Expressions with unmatched brackets will automatically");
            println!("  continue on the next line. The prompt changes from");
            println!("  > to ... to indicate continuation mode.");
            println!();
            println!("Example:");
            println!("  > map(");
            println!("  ...   [1, 2, 3],");
            println!("  ...   x => x * 2");
            println!("  ... )");
            println!("  = [2, 4, 6]");
        }
        _ => unreachable!(),
    }
}
