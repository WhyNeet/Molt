use ast::{
    expression::Expression,
    literal::{Literal, Number},
    operator::Operator,
};
use lexer::scanner::Scanner;
use parser::Parser;

#[test]
fn expression_works() {
    let input = r#"1 * 3 + 2"#;
    let tokens = Scanner::tokenize(input).collect();

    let tree = Parser::new(tokens).parse();

    assert_eq!(
        tree,
        Expression::Binary {
            left: Box::new(Expression::Binary {
                left: Box::new(Expression::Literal(Literal::Number(Number::Int32(1)))),
                operator: Operator::Mul,
                right: Box::new(Expression::Literal(Literal::Number(Number::Int32(3))))
            }),
            operator: Operator::Add,
            right: Box::new(Expression::Literal(Literal::Number(Number::Int32(2))))
        }
    )
}
