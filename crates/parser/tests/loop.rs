use std::rc::Rc;

use ast::{expression::Expression, statement::Statement};
use common::{Literal, Number};
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
            expr: Rc::new(Expression::Loop(vec![
                Rc::new(Statement::Expression {
                    expr: Rc::new(Expression::Assignment {
                        assignee: Rc::new(Expression::Identifier("a".to_string())),
                        expr: Rc::new(Expression::Literal(Rc::new(Literal::Number(
                            Number::Int32(1)
                        ))))
                    }),
                    end_semi: true
                }),
                Rc::new(Statement::Expression {
                    expr: Rc::new(Expression::Break),
                    end_semi: true
                })
            ])),
            end_semi: false
        }
    );
}
