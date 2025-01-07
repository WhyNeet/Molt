use ast::{
    expression::Expression,
    literal::{Literal, Number, Type},
    operator::Operator,
    statement::Statement,
};
use lexer::scanner::Scanner;
use parser::Parser;

#[test]
fn expression_works() {
    let input = r#"1 * 3 + 2"#;
    let tokens = Scanner::tokenize(input).collect();

    let tree = Parser::new(tokens).parse();

    assert_eq!(
        tree[0],
        Statement::Expression {
            expr: Expression::Binary {
                left: Box::new(Expression::Binary {
                    left: Box::new(Expression::Literal(Literal::Number(Number::Int32(1)))),
                    operator: Operator::Mul,
                    right: Box::new(Expression::Literal(Literal::Number(Number::Int32(3))))
                }),
                operator: Operator::Add,
                right: Box::new(Expression::Literal(Literal::Number(Number::Int32(2))))
            },
            end_semi: false
        }
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

#[test]
fn function_declaration_works() {
    let input = r#"fun square(x: i32) -> i32 = x * x"#;
    let tokens = Scanner::tokenize(input).collect();

    let tree = Parser::new(tokens).parse();

    assert_eq!(
        tree[0],
        Statement::FunctionDeclaration {
            name: "square".to_string(),
            block: Some(Expression::Binary {
                left: Box::new(Expression::Identifier("x".to_string())),
                operator: Operator::Mul,
                right: Box::new(Expression::Identifier("x".to_string()))
            }),
            return_type: Type::Int32,
            parameters: vec![("x".to_string(), Type::Int32)],
            annotations: vec![]
        }
    );
}

#[test]
fn extern_functions_work() {
    let input = r#"@extern fun printf(ptr: i8) -> i32;"#;
    let tokens = Scanner::tokenize(input).collect();

    let tree = Parser::new(tokens).parse();

    assert_eq!(
        tree[0],
        Statement::FunctionDeclaration {
            name: "printf".to_string(),
            block: None,
            return_type: Type::Int32,
            parameters: vec![("ptr".to_string(), Type::Int8)],
            annotations: vec!["@extern".to_string()]
        }
    );
}

#[test]
fn conditional_expressions_work() {
    let input = r#"1 * 3 + if a > 2 { 1 } else if a == 4 { -5 } else { 2 }"#;
    let tokens = Scanner::tokenize(input).collect();

    let tree = Parser::new(tokens).parse();

    assert_eq!(
        tree[0],
        Statement::Expression {
            expr: Expression::Binary {
                left: Box::new(Expression::Binary {
                    left: Box::new(Expression::Literal(Literal::Number(Number::Int32(1)))),
                    operator: Operator::Mul,
                    right: Box::new(Expression::Literal(Literal::Number(Number::Int32(3))))
                }),
                operator: Operator::Add,
                right: Box::new(Expression::Conditional {
                    condition: Box::new(Expression::Binary {
                        left: Box::new(Expression::Identifier("a".to_string())),
                        operator: Operator::Gt,
                        right: Box::new(Expression::Literal(Literal::Number(Number::Int32(2))))
                    }),
                    body: vec![Statement::Expression {
                        expr: Expression::Literal(Literal::Number(Number::Int32(1))),
                        end_semi: false
                    }],
                    alternative: Some(Box::new(Expression::Conditional {
                        condition: Box::new(Expression::Binary {
                            left: Box::new(Expression::Identifier("a".to_string())),
                            operator: Operator::Eq,
                            right: Box::new(Expression::Literal(Literal::Number(Number::Int32(4))))
                        }),
                        body: vec![Statement::Expression {
                            expr: Expression::Unary {
                                operator: Operator::Neg,
                                expr: Box::new(Expression::Literal(Literal::Number(
                                    Number::Int32(5)
                                )))
                            },
                            end_semi: false
                        }],
                        alternative: Some(Box::new(Expression::Block(vec![
                            Statement::Expression {
                                expr: Expression::Literal(Literal::Number(Number::Int32(2))),
                                end_semi: false
                            }
                        ])))
                    }))
                })
            },
            end_semi: false
        }
    );
}
