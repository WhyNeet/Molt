use ast::{
    expression::Expression,
    literal::{Literal, Number},
    operator::Operator,
    statement::Statement,
};
use lexer::scanner::Scanner;
use parser::Parser;

#[test]
fn expression_works() {
    let input = r#"1 * 3 + 2;"#;
    let tokens = Scanner::tokenize(input).collect();

    let tree = Parser::new(tokens).parse();

    assert_eq!(
        tree[0],
        Statement::Expression(Expression::Binary {
            left: Box::new(Expression::Binary {
                left: Box::new(Expression::Literal(Literal::Number(Number::Int32(1)))),
                operator: Operator::Mul,
                right: Box::new(Expression::Literal(Literal::Number(Number::Int32(3))))
            }),
            operator: Operator::Add,
            right: Box::new(Expression::Literal(Literal::Number(Number::Int32(2))))
        })
    );
}

#[test]
fn variable_declaration_works() {
    let input = r#"let x = 1;"#;
    let tokens = Scanner::tokenize(input).collect();

    let tree = Parser::new(tokens).parse();

    assert_eq!(
        tree[0],
        Statement::VariableDeclaration {
            name: "x".to_string(),
            expr: Expression::Literal(Literal::Number(Number::Int32(1))),
            annotations: vec![]
        }
    );
}
