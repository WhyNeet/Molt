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

#[test]
fn member_access_works() {
    let input = r#"a.b.c"#;
    let tokens = Scanner::tokenize(input).collect();

    let tree = Parser::new(tokens).parse();

    assert_eq!(
        tree[0],
        Statement::Expression {
            expr: Expression::MemberAccess {
                expr: Box::new(Expression::MemberAccess {
                    expr: Box::new(Expression::Identifier("a".to_string())),
                    ident: "b".to_string()
                }),
                ident: "c".to_string()
            },
            end_semi: false
        }
    );
}

#[test]
fn function_call_works() {
    let input = r#"a.b.c(1 + 2, 3)()"#;
    let tokens = Scanner::tokenize(input).collect();

    let tree = Parser::new(tokens).parse();

    assert_eq!(
        tree[0],
        Statement::Expression {
            expr: Expression::Call {
                expr: Box::new(Expression::Call {
                    expr: Box::new(Expression::MemberAccess {
                        expr: Box::new(Expression::MemberAccess {
                            expr: Box::new(Expression::Identifier("a".to_string())),
                            ident: "b".to_string()
                        }),
                        ident: "c".to_string()
                    }),
                    arguments: vec![
                        Expression::Binary {
                            left: Box::new(Expression::Literal(Literal::Number(Number::Int32(1)))),
                            operator: Operator::Add,
                            right: Box::new(Expression::Literal(Literal::Number(Number::Int32(2))))
                        },
                        Expression::Literal(Literal::Number(Number::Int32(3)))
                    ]
                }),
                arguments: vec![]
            },
            end_semi: false
        }
    );
}

#[test]
fn cast_works() {
    let input = r#"(1 as u32 + 5u32) as u64"#;
    let tokens = Scanner::tokenize(input).collect();

    let tree = Parser::new(tokens).parse();

    assert_eq!(
        tree[0],
        Statement::Expression {
            expr: Expression::Cast {
                expr: Box::new(Expression::Grouping(Box::new(Expression::Binary {
                    left: Box::new(Expression::Cast {
                        expr: Box::new(Expression::Literal(Literal::Number(Number::Int32(1)))),
                        ty: Type::UInt32
                    }),
                    operator: Operator::Add,
                    right: Box::new(Expression::Literal(Literal::Number(Number::UInt32(5))))
                }))),
                ty: Type::UInt64
            },
            end_semi: false
        }
    );
}
