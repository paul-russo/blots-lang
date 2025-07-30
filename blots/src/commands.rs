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
            println!("Blots - A Calculator Language");
            println!("=========================================");
            println!();
            println!("REPL Commands:");
            println!("  help        Show this help message");
            println!("  quit, exit  Exit the REPL");
            println!();
            println!("Basic Syntax:");
            println!("  Numbers:     42, 3.14, 1_000_000");
            println!("  Strings:     \"hello\" or 'world'");
            println!("  Booleans:    true, false");
            println!("  Lists:       [1, 2, 3]");
            println!("  Records:     {{ name: \"Alice\", age: 30 }}");
            println!("  Functions:   x => x + 1");
            println!();
            println!("Bindings (aka variables):");
            println!("  x = 10");
            println!("  greet = name => \"Hello, \" + name");
            println!();
            println!("Common Functions:");
            println!("  map(list, fn)         Transform each element");
            println!("  filter(list, fn)      Keep matching elements");
            println!("  reduce(list, fn, init) Reduce to single value");
            println!("  range(n)              Generate [0, 1, ..., n-1]");
            println!();
            println!("Operators:");
            println!("  Arithmetic: + - * / % ^");
            println!("  Comparison: == != < > <= >=");
            println!("  Logical:    and or not");
            println!("  Nullish:    ?? (coalesce null values)");
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
