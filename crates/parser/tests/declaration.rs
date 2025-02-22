use std::rc::Rc;

use ast::{
    annotation::Annotation,
    expression::Expression,
    statement::{MethodDeclaration, Statement},
};
use common::{Literal, Number, Operator, Type};
use lexer::scanner::Scanner;
use parser::Parser;

#[test]
fn variable_declaration_works() {
    let input = r#"let x = 1;"#;
    let tokens = Scanner::tokenize(input).collect();

    let tree = Parser::new(tokens).parse();

    assert_eq!(
        tree[0],
        Statement::VariableDeclaration {
            name: "x".to_string(),
            expr: Rc::new(Expression::Literal(Rc::new(Literal::Number(
                Number::Int32(1)
            )))),
            ty: None,
            is_mut: false,
        }
    );
}

#[test]
fn variable_explicit_type_declaration_works() {
    let input = r#"let x: u8 = 1;"#;
    let tokens = Scanner::tokenize(input).collect();

    let tree = Parser::new(tokens).parse();

    assert_eq!(
        tree[0],
        Statement::VariableDeclaration {
            name: "x".to_string(),
            expr: Rc::new(Expression::Literal(Rc::new(Literal::Number(
                Number::Int32(1)
            )))),
            ty: Some(Type::UInt8),
            is_mut: false
        }
    );
}

#[test]
fn variable_explicit_ptr_type_declaration_works() {
    let input = r#"let x: *u8 = 1;"#;
    let tokens = Scanner::tokenize(input).collect();

    let tree = Parser::new(tokens).parse();

    assert_eq!(
        tree[0],
        Statement::VariableDeclaration {
            name: "x".to_string(),
            expr: Rc::new(Expression::Literal(Rc::new(Literal::Number(
                Number::Int32(1)
            )))),
            ty: Some(Type::Ptr(Box::new(Type::UInt8))),
            is_mut: false
        }
    );
}

#[test]
fn mutable_variable_declaration_works() {
    let input = r#"let mut x: *u8 = 1;"#;
    let tokens = Scanner::tokenize(input).collect();

    let tree = Parser::new(tokens).parse();

    assert_eq!(
        tree[0],
        Statement::VariableDeclaration {
            name: "x".to_string(),
            expr: Rc::new(Expression::Literal(Rc::new(Literal::Number(
                Number::Int32(1)
            )))),
            ty: Some(Type::Ptr(Box::new(Type::UInt8))),
            is_mut: true
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
            block: Some(Rc::new(Expression::Binary {
                left: Rc::new(Expression::Identifier("x".to_string())),
                operator: Operator::Mul,
                right: Rc::new(Expression::Identifier("x".to_string()))
            })),
            return_type: Type::Int32,
            parameters: vec![("x".to_string(), Type::Int32)],
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
        Statement::Annotated {
            annotations: vec![Annotation {
                arguments: vec![],
                name: "extern".to_string()
            }],
            stmt: Rc::new(Statement::FunctionDeclaration {
                name: "printf".to_string(),
                block: None,
                return_type: Type::Int32,
                parameters: vec![("ptr".to_string(), Type::Int8)],
            })
        }
    );
}

#[test]
pub fn struct_declaration_works() {
    let input = r#"
        struct Config {
            port: u16 = 8080u16;
            host: str = "127.0.0.1";

            fun get_port(self: Config) -> u16 = {
                self.port
            }

            fun get_host(self: Config) -> str = {
                self.host
            }
        }
        "#;
    let tokens = Scanner::tokenize(input).collect();

    let tree = Parser::new(tokens).parse();

    assert_eq!(
        tree[0],
        Statement::StructDeclaration {
            name: "Config".to_string(),
            fields: vec![
                (
                    "port".to_string(),
                    Type::UInt16,
                    Some(Expression::Literal(Rc::new(Literal::Number(
                        Number::UInt16(8080)
                    ))))
                ),
                (
                    "host".to_string(),
                    Type::Str,
                    Some(Expression::Literal(Rc::new(Literal::Str(
                        "127.0.0.1".to_string()
                    ))))
                )
            ],
            methods: vec![
                (
                    "get_port".to_string(),
                    MethodDeclaration {
                        return_type: Type::UInt16,
                        parameters: vec![("self".to_string(), Type::Named("Config".to_string()))],
                        expression: Rc::new(Expression::Block(vec![Rc::new(
                            Statement::Expression {
                                expr: Rc::new(Expression::MemberAccess {
                                    expr: Rc::new(Expression::Identifier("self".to_string())),
                                    ident: "port".to_string()
                                }),
                                end_semi: false
                            }
                        )]))
                    }
                ),
                (
                    "get_host".to_string(),
                    MethodDeclaration {
                        return_type: Type::Str,
                        parameters: vec![("self".to_string(), Type::Named("Config".to_string()))],
                        expression: Rc::new(Expression::Block(vec![Rc::new(
                            Statement::Expression {
                                expr: Rc::new(Expression::MemberAccess {
                                    expr: Rc::new(Expression::Identifier("self".to_string())),
                                    ident: "host".to_string()
                                }),
                                end_semi: false
                            }
                        )]))
                    }
                )
            ]
        }
    );
}
