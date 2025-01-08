use ast::{
    annotation::Annotation,
    expression::Expression,
    literal::{Literal, Number, Type},
    operator::Operator,
    statement::Statement,
};
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
            expr: Expression::Literal(Literal::Number(Number::Int32(1))),
            ty: None
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
            expr: Expression::Literal(Literal::Number(Number::Int32(1))),
            ty: Some(Type::UInt8)
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
            stmt: Box::new(Statement::FunctionDeclaration {
                name: "printf".to_string(),
                block: None,
                return_type: Type::Int32,
                parameters: vec![("ptr".to_string(), Type::Int8)],
            })
        }
    );
}
