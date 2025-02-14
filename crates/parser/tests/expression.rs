use std::rc::Rc;

use ast::{expression::Expression, statement::Statement};
use common::{Literal, Number, Operator, Type};
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
            expr: Rc::new(Expression::Binary {
                left: Rc::new(Expression::Binary {
                    left: Rc::new(Expression::Literal(Rc::new(Literal::Number(
                        Number::Int32(1)
                    )))),
                    operator: Operator::Mul,
                    right: Rc::new(Expression::Literal(Rc::new(Literal::Number(
                        Number::Int32(3)
                    ))))
                }),
                operator: Operator::Add,
                right: Rc::new(Expression::Literal(Rc::new(Literal::Number(
                    Number::Int32(2)
                ))))
            }),
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
            expr: Rc::new(Expression::Binary {
                left: Rc::new(Expression::Binary {
                    left: Rc::new(Expression::Literal(Rc::new(Literal::Number(
                        Number::Int32(1)
                    )))),
                    operator: Operator::Mul,
                    right: Rc::new(Expression::Literal(Rc::new(Literal::Number(
                        Number::Int32(3)
                    ))))
                }),
                operator: Operator::Add,
                right: Rc::new(Expression::Conditional {
                    condition: Rc::new(Expression::Binary {
                        left: Rc::new(Expression::Identifier("a".to_string())),
                        operator: Operator::Gt,
                        right: Rc::new(Expression::Literal(Rc::new(Literal::Number(
                            Number::Int32(2)
                        ))))
                    }),
                    body: vec![Rc::new(Statement::Expression {
                        expr: Rc::new(Expression::Literal(Rc::new(Literal::Number(
                            Number::Int32(1)
                        )))),
                        end_semi: false
                    })],
                    alternative: Some(Rc::new(Expression::Conditional {
                        condition: Rc::new(Expression::Binary {
                            left: Rc::new(Expression::Identifier("a".to_string())),
                            operator: Operator::Eq,
                            right: Rc::new(Expression::Literal(Rc::new(Literal::Number(
                                Number::Int32(4)
                            ))))
                        }),
                        body: vec![Rc::new(Statement::Expression {
                            expr: Rc::new(Expression::Unary {
                                operator: Operator::Neg,
                                expr: Rc::new(Expression::Literal(Rc::new(Literal::Number(
                                    Number::Int32(5)
                                ))))
                            }),
                            end_semi: false
                        })],
                        alternative: Some(Rc::new(Expression::Block(vec![Rc::new(
                            Statement::Expression {
                                expr: Rc::new(Expression::Literal(Rc::new(Literal::Number(
                                    Number::Int32(2)
                                )))),
                                end_semi: false
                            }
                        )])))
                    }))
                })
            }),
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
            expr: Rc::new(Expression::MemberAccess {
                expr: Rc::new(Expression::MemberAccess {
                    expr: Rc::new(Expression::Identifier("a".to_string())),
                    ident: "b".to_string()
                }),
                ident: "c".to_string()
            }),
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
            expr: Rc::new(Expression::Call {
                expr: Rc::new(Expression::Call {
                    expr: Rc::new(Expression::MemberAccess {
                        expr: Rc::new(Expression::MemberAccess {
                            expr: Rc::new(Expression::Identifier("a".to_string())),
                            ident: "b".to_string()
                        }),
                        ident: "c".to_string()
                    }),
                    arguments: vec![
                        Rc::new(Expression::Binary {
                            left: Rc::new(Expression::Literal(Rc::new(Literal::Number(
                                Number::Int32(1)
                            )))),
                            operator: Operator::Add,
                            right: Rc::new(Expression::Literal(Rc::new(Literal::Number(
                                Number::Int32(2)
                            ))))
                        }),
                        Rc::new(Expression::Literal(Rc::new(Literal::Number(
                            Number::Int32(3)
                        ))))
                    ]
                }),
                arguments: vec![]
            }),
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
            expr: Rc::new(Expression::Cast {
                expr: Rc::new(Expression::Grouping(Rc::new(Expression::Binary {
                    left: Rc::new(Expression::Cast {
                        expr: Rc::new(Expression::Literal(Rc::new(Literal::Number(
                            Number::Int32(1)
                        )))),
                        ty: Type::UInt32
                    }),
                    operator: Operator::Add,
                    right: Rc::new(Expression::Literal(Rc::new(Literal::Number(
                        Number::UInt32(5)
                    ))))
                }))),
                ty: Type::UInt64
            }),
            end_semi: false
        }
    );
}

#[test]
fn cast_to_ptr_works() {
    let input = r#"1 as *u64"#;
    let tokens = Scanner::tokenize(input).collect();

    let tree = Parser::new(tokens).parse();

    assert_eq!(
        tree[0],
        Statement::Expression {
            expr: Rc::new(Expression::Cast {
                expr: Rc::new(Expression::Literal(Rc::new(Literal::Number(
                    Number::Int32(1)
                )))),
                ty: Type::Ptr(Box::new(Type::UInt64))
            }),
            end_semi: false
        }
    );
}

#[test]
fn ref_expression_works() {
    let input = r#"&(1 + 2)"#;
    let tokens = Scanner::tokenize(input).collect();

    let tree = Parser::new(tokens).parse();

    assert_eq!(
        tree[0],
        Statement::Expression {
            expr: Rc::new(Expression::Unary {
                expr: Rc::new(Expression::Grouping(Rc::new(Expression::Binary {
                    left: Rc::new(Expression::Literal(Rc::new(Literal::Number(
                        Number::Int32(1)
                    )))),
                    operator: Operator::Add,
                    right: Rc::new(Expression::Literal(Rc::new(Literal::Number(
                        Number::Int32(2)
                    ))))
                }))),
                operator: Operator::Ref
            }),
            end_semi: false
        }
    );
}

#[test]
fn intrinsics_work() {
    let input = r#"#i8_ptr(1)"#;
    let tokens = Scanner::tokenize(input).collect();

    let tree = Parser::new(tokens).parse();

    assert_eq!(
        tree[0],
        Statement::Expression {
            expr: Rc::new(Expression::CompilerIntrinsic {
                name: "i8_ptr".to_string(),
                arguments: vec![Rc::new(Expression::Literal(Rc::new(Literal::Number(
                    Number::Int32(1)
                ))))]
            }),
            end_semi: false
        }
    );
}
