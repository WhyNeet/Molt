use std::fs;

use checker::Checker;
use lexer::scanner::Scanner;
use lirgen::emitters::module::LirModuleEmitter;
use parser::Parser;

fn main() {
    let filename = std::env::args().nth(1).expect("expected filename.");
    let contents = fs::read_to_string(filename).unwrap();

    let tokens = Scanner::tokenize(&contents).collect();
    let tree = Parser::new(tokens).parse();

    let tree = Checker::new(tree).check();

    let tree = LirModuleEmitter::new().emit(tree);

    println!("parsed tree:\n{tree:?}");
}
