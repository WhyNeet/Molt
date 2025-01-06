use lexer::scanner::{
    keywords::Keyword,
    token::{Base, Literal, LiteralKind, Token, TokenKind},
    Scanner,
};

#[test]
fn numbers_work() {
    let input = r#"let x = 0o6u32;"#;
    let tokens = Scanner::tokenize(input).collect::<Vec<Token>>();

    assert_eq!(tokens[0].kind, TokenKind::Keyword(Keyword::Let));

    assert_eq!(tokens[1].kind, TokenKind::Ident("x".to_string()));

    assert_eq!(tokens[2].kind, TokenKind::Eq);

    assert_eq!(
        tokens[3].kind,
        TokenKind::Literal(Literal {
            kind: LiteralKind::Int {
                base: Base::Octal,
                empty_int: false
            },
            suffix: Some("u32".to_string()),
            symbol: "0o6u32".to_string()
        })
    );

    assert_eq!(tokens[4].kind, TokenKind::Semi);
}

#[test]
fn strings_work() {
    let input = r#"let x = "hello world!";"#;
    let tokens = Scanner::tokenize(input).collect::<Vec<Token>>();

    assert_eq!(tokens[0].kind, TokenKind::Keyword(Keyword::Let));

    assert_eq!(tokens[1].kind, TokenKind::Ident("x".to_string()));

    assert_eq!(tokens[2].kind, TokenKind::Eq);

    assert_eq!(
        tokens[3].kind,
        TokenKind::Literal(Literal {
            kind: LiteralKind::Str { terminated: true },
            suffix: None,
            symbol: "\"hello world!\"".to_string()
        })
    );

    assert_eq!(tokens[4].kind, TokenKind::Semi);
}

#[test]
fn annotations_work() {
    let input = r#"@main fun main() {}"#;
    let tokens = Scanner::tokenize(input).collect::<Vec<Token>>();

    assert_eq!(tokens[0].kind, TokenKind::Annotation("@main".to_string()));

    assert_eq!(tokens[1].kind, TokenKind::Keyword(Keyword::Fun));

    assert_eq!(tokens[2].kind, TokenKind::Ident("main".to_string()));

    assert_eq!(tokens[3].kind, TokenKind::OpenParen);

    assert_eq!(tokens[4].kind, TokenKind::CloseParen);

    assert_eq!(tokens[5].kind, TokenKind::OpenBrace);

    assert_eq!(tokens[6].kind, TokenKind::CloseBrace);
}
