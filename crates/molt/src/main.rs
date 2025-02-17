use std::{fs, path::Path};

use artifact::BuildArtifactGenerator;
use checker::Checker;
use irgen::IrEmitter;
use lexer::scanner::Scanner;
use lirgen::emitters::module::LirModuleEmitter;
use parser::Parser;

fn main() {
    let filename = std::env::args().nth(1).expect("expected filename.");
    let contents = fs::read_to_string(filename).unwrap();

    let tokens = Scanner::tokenize(&contents).collect();
    let tree = Parser::new(tokens).parse();

    let tree = Checker::new(tree).check();

    let module = LirModuleEmitter::new().emit(tree);

    println!("parsed tree:\n{module:?}");

    let emitter = IrEmitter::new();
    let module = emitter.run(module);

    let artifact_builder = BuildArtifactGenerator::default();

    let output_path = Path::new("output.o");
    artifact_builder.produce_object_file(&module, output_path);

    println!("\nCompiled 1 artifact.");
}
