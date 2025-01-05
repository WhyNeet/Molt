use lexer::cursor::{
    token::{Base, LiteralKind, Token, TokenKind},
    Cursor,
};

#[test]
fn numbers_work() {
    let input = r#"let x = 0b101;"#;
    let res = Cursor::tokenize(input).collect::<Vec<Token>>();

    assert_eq!(res[0].kind, TokenKind::Ident);
    assert_eq!(res[0].len, 3);

    assert_eq!(res[1].kind, TokenKind::Whitespace);
    assert_eq!(res[1].len, 1);

    assert_eq!(res[2].kind, TokenKind::Ident);
    assert_eq!(res[2].len, 1);

    assert_eq!(res[3].kind, TokenKind::Whitespace);
    assert_eq!(res[3].len, 1);

    assert_eq!(res[4].kind, TokenKind::Eq);
    assert_eq!(res[4].len, 1);

    assert_eq!(res[5].kind, TokenKind::Whitespace);
    assert_eq!(res[5].len, 1);

    assert_eq!(
        res[6].kind,
        TokenKind::Literal {
            kind: LiteralKind::Int {
                base: Base::Binary,
                empty_int: false
            },
            suffix_start: 5
        }
    );
    assert_eq!(res[6].len, 5);
}
