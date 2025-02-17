use std::{fs, path::Path, ptr};

use checker::Checker;
use irgen::{emitters::module::IrModuleEmitter, FileType, IrEmitter, TargetInitializer};
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
    let ir = emitter.run(module);

    println!("\n>>> LLVM IR (debug) >>>\n\n");
    ir.print_to_stderr();

    let target_machine = TargetInitializer::native().init_target_machine();

    let output_path = Path::new("output.o");
    target_machine
        .write_to_file(&ir, FileType::Object, output_path)
        .unwrap();

    println!("\nDONE.");
}
