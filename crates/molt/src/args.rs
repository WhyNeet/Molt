use std::path::PathBuf;

use clap::ValueEnum;

#[derive(clap::Parser)]
#[command(version, about)]
pub struct MoltCliArgs {
    pub filename: Option<PathBuf>,

    #[arg(short, long, value_enum)]
    pub format: Option<OutputFormat>,

    #[arg(short, long)]
    pub output: Option<String>,
}

#[derive(Debug, Clone, ValueEnum, PartialEq)]
pub enum OutputFormat {
    Object,
    ASM,
    Bitcode,
    IR,
    LIR,
}
