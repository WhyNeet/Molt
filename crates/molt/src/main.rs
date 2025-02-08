use std::{fs, ptr};

use checker::Checker;
use irgen::{emitters::module::IrModuleEmitter, IrEmitter};
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

    println!("\n>>> IR >>>\n");

    ir.print_to_stderr();
}
