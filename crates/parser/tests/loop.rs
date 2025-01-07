use ast::{
    expression::Expression,
    literal::{Literal, Number},
    statement::Statement,
};
use lexer::scanner::Scanner;
use parser::Parser;

#[test]
fn loop_works() {
    let input = r#"loop {
      a = 1;
      break;
    }"#;
    let tokens = Scanner::tokenize(input).collect();
    let tree = Parser::new(tokens).parse();

    assert_eq!(
        tree[0],
        Statement::Expression {
            expr: Expression::Loop(vec![
                Statement::Expression {
                    expr: Expression::Assignment {
                        identifier: "a".to_string(),
                        expr: Box::new(Expression::Literal(Literal::Number(Number::Int32(1))))
                    },
                    end_semi: true
                },
                Statement::Expression {
                    expr: Expression::Break,
                    end_semi: true
                }
            ]),
            end_semi: false
        }
    );
}
