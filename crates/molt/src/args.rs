use std::path::PathBuf;

use clap::{Subcommand, ValueEnum};

#[derive(clap::Parser)]
#[command(version, about)]
pub struct MoltCliArgs {
    pub filename: Option<PathBuf>,

    #[arg(short, long, value_enum)]
    pub format: Option<OutputFormat>,

    #[arg(short, long)]
    pub output: Option<String>,

    #[command(subcommand)]
    pub command: Option<Command>,
}

#[derive(Debug, Clone, ValueEnum, PartialEq)]
pub enum OutputFormat {
    Object,
    ASM,
    Bitcode,
    IR,
    LIR,
}

#[derive(Debug, Clone, Subcommand)]
pub enum Command {
    Target,
}
