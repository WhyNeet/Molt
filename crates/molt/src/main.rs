mod args;

use std::{fs, path::Path, process};

use args::{MoltCliArgs, OutputFormat};
use artifact::BuildArtifactGenerator;
use checker::Checker;
use clap::{CommandFactory, Parser};
use irgen::IrEmitter;
use lexer::scanner::Scanner;
use lirgen::emitters::module::LirModuleEmitter;

fn main() {
    let args = MoltCliArgs::parse();
    let filename = args.filename.unwrap_or_else(|| {
        MoltCliArgs::command().print_help().unwrap();
        process::exit(0)
    });
    let contents = fs::read_to_string(&filename).unwrap();

    let tokens = Scanner::tokenize(&contents).collect();
    let tree = parser::Parser::new(tokens).parse();

    let tree = Checker::new(tree).check();

    let module = LirModuleEmitter::new().emit(tree);

    println!("parsed tree:\n{module:?}");

    let emitter = IrEmitter::new();
    let module = emitter.run(module);

    let artifact_builder = BuildArtifactGenerator::default();

    let output_format = args.format.unwrap_or(OutputFormat::Object);

    let output_path = args.output.unwrap_or_else(|| {
        format!(
            "{}.{}",
            filename.to_str().unwrap(),
            match output_format {
                OutputFormat::Object => "o",
                OutputFormat::Bitcode => "bc",
                OutputFormat::ASM => "s",
            }
        )
    });
    let output_path = Path::new(&output_path);

    match output_format {
        OutputFormat::Object => artifact_builder.produce_object_file(&module, output_path),
        OutputFormat::Bitcode => artifact_builder.produce_bitcode_file(&module, output_path),
        OutputFormat::ASM => artifact_builder.produce_asm_file(&module, output_path),
    }

    println!("\nCompiled 1 artifact.");
}
