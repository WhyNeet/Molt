use lexer::scanner::{
    token::{Base, Literal, LiteralKind, Token, TokenKind},
    Scanner,
};

#[test]
fn numbers_work() {
    let input = r#"let x = 0o6u32;"#;
    let res = Scanner::tokenize(input).collect::<Vec<Token>>();

    assert_eq!(res[0].kind, TokenKind::Ident("let".to_string()));

    assert_eq!(res[1].kind, TokenKind::Ident("x".to_string()));

    assert_eq!(res[2].kind, TokenKind::Eq);

    assert_eq!(
        res[3].kind,
        TokenKind::Literal(Literal {
            kind: LiteralKind::Int {
                base: Base::Octal,
                empty_int: false
            },
            suffix: Some("u32".to_string()),
            symbol: "0o6u32".to_string()
        })
    );

    assert_eq!(res[4].kind, TokenKind::Semi);
}

#[test]
fn strings_work() {
    let input = r#"let x = "hello world!";"#;
    let res = Scanner::tokenize(input).collect::<Vec<Token>>();

    assert_eq!(res[0].kind, TokenKind::Ident("let".to_string()));

    assert_eq!(res[1].kind, TokenKind::Ident("x".to_string()));

    assert_eq!(res[2].kind, TokenKind::Eq);

    assert_eq!(
        res[3].kind,
        TokenKind::Literal(Literal {
            kind: LiteralKind::Str { terminated: true },
            suffix: None,
            symbol: "\"hello world!\"".to_string()
        })
    );

    assert_eq!(res[4].kind, TokenKind::Semi);
}
