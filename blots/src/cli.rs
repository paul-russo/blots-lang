use clap::{Parser, Subcommand};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Args {
    #[command(subcommand)]
    pub command: Option<Commands>,

    /// Path to a source file to run (when no subcommand is provided).
    pub path: Option<String>,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    /// Format a Blots source file
    Format {
        /// Path to the source file to format
        path: String,

        /// Write formatted output back to the file
        #[arg(short, long)]
        write: bool,

        /// Check if the file is already formatted (exit with non-zero if not)
        #[arg(short, long)]
        check: bool,
    },
}
