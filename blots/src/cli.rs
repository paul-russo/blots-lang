use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Args {
    /// Path to a source file to run
    pub path: Option<String>,

    /// Evaluate stdin as Blots source code instead of JSON inputs
    #[arg(short, long)]
    pub evaluate: bool,

    /// JSON object to use as inputs (can be combined with piped inputs)
    #[arg(short, long)]
    pub inputs: Option<String>,
}
