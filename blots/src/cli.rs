use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Args {
    /// Path to a source file to run
    #[arg(group = "source")]
    pub path: Option<String>,

    /// Evaluate stdin as Blots source code instead of JSON inputs
    #[arg(short, long, group = "source")]
    pub evaluate: bool,

    /// JSON object to use as inputs (can be combined with piped inputs, can be specified multiple times)
    #[arg(short, long, action = clap::ArgAction::Append)]
    pub inputs: Vec<String>,

    /// Path to write output JSON file
    #[arg(short, long)]
    pub output: Option<String>,
}
