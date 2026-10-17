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

    /// Format a Blots source file. Takes input and output file paths.
    #[arg(short = 'f', long, value_names = ["INPUT", "OUTPUT"], num_args = 2, group = "source")]
    pub format: Vec<String>,

    /// Enable performance profiling (shows function call statistics)
    #[arg(long)]
    pub profile: bool,
}

impl Args {
    /// The arguments this process was started with.
    ///
    /// `blots <file>` is by far the most common invocation, and building clap's description
    /// of the command line just to recognize it takes longer than running a short script
    /// does. A single argument that cannot be a flag is therefore taken as the file (or
    /// source) directly; every other shape goes through clap as usual.
    pub fn from_env() -> Self {
        let mut args = std::env::args_os().skip(1);

        if let (Some(first), None) = (args.next(), args.next())
            && let Some(first) = first.to_str()
            && !first.starts_with('-')
        {
            return Self {
                file_or_source: Some(first.to_string()),
                evaluate: false,
                input: Vec::new(),
                output: None,
                completions: None,
                format: Vec::new(),
                profile: false,
            };
        }

        Self::parse()
    }
}
