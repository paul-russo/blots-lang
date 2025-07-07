use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Args {
    /// Path to a source file to run.
    pub path: Option<String>,

    /// Transpile to JavaScript instead of running
    #[arg(long)]
    pub transpile: bool,

    /// Enable inline evaluation for displaying results at each expression
    #[arg(long)]
    pub inline_eval: bool,

    /// Output file for transpiled JavaScript (stdout if not specified)
    #[arg(long, short)]
    pub output: Option<String>,

    /// Use the slower built-in interpreter instead of the fast JavaScript runtime
    #[arg(long)]
    pub slow: bool,
}
