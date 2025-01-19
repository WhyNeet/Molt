use std::rc::Rc;

use ast::{
    expression::Expression,
    literal::{Literal, Number},
    statement::Statement,
};
use common::Operator;
use lexer::scanner::Scanner;
use parser::Parser;

#[test]
fn return_works() {
    let input = r#"return x - 1;"#;
    let tokens = Scanner::tokenize(input).collect();

    let tree = Parser::new(tokens).parse();

    assert_eq!(
        tree[0],
        Statement::Return(Rc::new(Expression::Binary {
            left: Rc::new(Expression::Identifier("x".to_string())),
            operator: Operator::Sub,
            right: Rc::new(Expression::Literal(Rc::new(Literal::Number(
                Number::Int32(1)
            ))))
        }))
    );
}
