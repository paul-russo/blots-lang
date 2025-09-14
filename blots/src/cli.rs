use clap::Parser;
use clap_complete::Shell;

#[derive(Parser, Debug)]
#[command(version, about, about = Some("A small expression-oriented programming language."))]
pub struct Args {
    /// Path to a source file to run OR inline Blots code to evaluate. If neither is provided, Blots will run in interactive mode.
    #[arg(group = "source")]
    pub file_or_source: Option<String>,

    /// Evaluate stdin as Blots source code instead of JSON inputs
    #[arg(short, long, group = "source")]
    pub evaluate: bool,

    /// JSON values to use as inputs (can be combined with piped inputs)
    #[arg(short, long, action = clap::ArgAction::Append)]
    pub input: Vec<String>,

    /// Path to write output JSON file
    #[arg(short, long)]
    pub output: Option<String>,

    /// Generate shell completions for the specified shell.
    #[arg(long, value_name = "SHELL")]
    pub completions: Option<Shell>,
}
