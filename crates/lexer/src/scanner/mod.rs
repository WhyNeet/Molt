pub mod token;

use std::str::Chars;

use token::{Base, Literal, LiteralKind, Token, TokenKind};

const EOF_CHAR: char = '\0';

pub struct Scanner<'a> {
    src: &'a str,
    start: usize,
    current: usize,
    line: usize,
    col: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(input: &'a str) -> Scanner<'a> {
        Self {
            current: 0,
            src: input,
            start: 0,
            col: 0,
            line: 0,
        }
    }

    /// Creates an iterator that produces tokens from the input string.
    pub fn tokenize(input: &str) -> impl Iterator<Item = Token> + '_ {
        let mut scanner = Scanner::new(input);
        std::iter::from_fn(move || {
            let token = scanner.advance_token();
            if token.kind != TokenKind::Eof {
                Some(token)
            } else {
                None
            }
        })
    }

    pub fn advance(&mut self) -> char {
        let char = self.peek();
        if char == '\n' {
            self.line += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
        self.current += 1;
        char
    }

    pub fn advance_by(&mut self, n: usize) -> char {
        let char = self.peek();
        self.current += n;
        char
    }

    pub fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.src.as_bytes()[self.current] as char
        }
    }

    pub fn peek_nth(&self, n: usize) -> char {
        if self.current + n >= self.src.len() {
            '\0'
        } else {
            self.src.as_bytes()[self.current + n] as char
        }
    }

    pub fn matches(&mut self, c: char) -> bool {
        if self.peek() == c {
            self.advance();
            true
        } else {
            false
        }
    }

    pub fn matches_fn(&mut self, f: impl FnOnce(char) -> bool) -> bool {
        if f(self.peek()) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub fn is_at_end(&self) -> bool {
        self.current >= self.src.len()
    }
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

pub fn is_whitespace(c: char) -> bool {
    // This is Pattern_White_Space.
    //
    // Note that this set is stable (ie, it doesn't change with different
    // Unicode versions), so it's ok to just hard-code the values.

    matches!(
        c,
        // Usual ASCII suspects
        '\u{0009}'   // \t
        | '\u{000A}' // \n
        | '\u{000B}' // vertical tab
        | '\u{000C}' // form feed
        | '\u{000D}' // \r
        | '\u{0020}' // space

        // NEXT LINE from latin1
        | '\u{0085}'

        // Bidi markers
        | '\u{200E}' // LEFT-TO-RIGHT MARK
        | '\u{200F}' // RIGHT-TO-LEFT MARK

        // Dedicated whitespace characters from Unicode
        | '\u{2028}' // LINE SEPARATOR
        | '\u{2029}' // PARAGRAPH SEPARATOR
    )
}

impl<'a> Scanner<'a> {
    pub(crate) fn advance_token(&mut self) -> Token {
        let (kind, line, col) = self.scan_token();

        Token { kind, col, line }
    }

    fn scan_token(&mut self) -> (TokenKind, usize, usize) {
        loop {
            if self.is_at_end() {
                return (TokenKind::Eof, self.line, self.col);
            }

            let line = self.line;
            let col = self.col;

            let c = self.advance();

            let kind = match c {
                c @ '0'..='9' => {
                    let kind = self.number(c);
                    let start = self.start;
                    self.start = self.current;
                    let suffix = match self.identifier() {
                        TokenKind::Ident(s) => s,
                        _ => unreachable!(),
                    };
                    TokenKind::Literal(Literal {
                        symbol: self.src[start..self.current].to_string(),
                        suffix: if suffix.is_empty() {
                            None
                        } else {
                            Some(suffix)
                        },
                        kind,
                    })
                }
                '+' => TokenKind::Plus,
                '-' => TokenKind::Minus,
                '*' => TokenKind::Star,
                '/' => {
                    if self.matches('/') {
                        self.line_comment()
                    } else if self.matches('*') {
                        self.block_comment()
                    } else {
                        TokenKind::Slash
                    }
                }
                '^' => TokenKind::Caret,
                '&' => {
                    if self.matches('&') {
                        TokenKind::AndAnd
                    } else {
                        TokenKind::And
                    }
                }

                '|' => {
                    if self.matches('|') {
                        TokenKind::OrOr
                    } else {
                        TokenKind::Or
                    }
                }
                '=' => {
                    if self.matches('=') {
                        TokenKind::EqEq
                    } else {
                        TokenKind::Eq
                    }
                }
                '@' => match self.peek() {
                    c if is_ident_start(c) => self.annotation(),
                    _ => TokenKind::At,
                },
                ';' => TokenKind::Semi,
                ':' => {
                    if self.matches(':') {
                        TokenKind::PathSep
                    } else {
                        TokenKind::Colon
                    }
                }
                '.' => TokenKind::Dot,
                '(' => TokenKind::OpenParen,
                '{' => TokenKind::OpenBrace,
                ')' => TokenKind::CloseParen,
                '}' => TokenKind::CloseBrace,
                '$' => TokenKind::Dollar,
                '!' => {
                    if self.matches('=') {
                        TokenKind::Ne
                    } else {
                        TokenKind::Bang
                    }
                }
                '>' => {
                    if self.matches('=') {
                        TokenKind::Ge
                    } else {
                        TokenKind::Gt
                    }
                }
                '<' => {
                    if self.matches('=') {
                        TokenKind::Le
                    } else {
                        TokenKind::Lt
                    }
                }
                '#' => TokenKind::Pound,
                '?' => TokenKind::Question,
                '%' => TokenKind::Percent,
                '\n' => {
                    self.start = self.current;
                    continue;
                }
                '"' => {
                    let kind = self.string();
                    let symbol = self.src[self.start..self.current].to_string();
                    TokenKind::Literal(Literal {
                        symbol,
                        suffix: None,
                        kind,
                    })
                }
                '\'' => {
                    let kind = self.char();
                    let symbol = self.src[self.start..self.current].to_string();
                    TokenKind::Literal(Literal {
                        symbol,
                        suffix: None,
                        kind,
                    })
                }
                c if is_whitespace(c) => {
                    self.start = self.current;
                    continue;
                }
                c if is_ident_start(c) => self.identifier(),
                _ => TokenKind::Unknown,
            };

            self.start = self.current;
            return (kind, line, col);
        }
    }

    fn number(&mut self, first_digit: char) -> LiteralKind {
        let mut base = Base::Decimal;
        if first_digit == '0' {
            // Attempt to parse encoding base.
            match self.peek() {
                'b' => {
                    base = Base::Binary;
                    self.advance();
                    if !self.consume_decimal_digits() {
                        return LiteralKind::Int {
                            base,
                            empty_int: true,
                        };
                    }
                }
                'o' => {
                    base = Base::Octal;
                    self.advance();
                    if !self.consume_decimal_digits() {
                        return LiteralKind::Int {
                            base,
                            empty_int: true,
                        };
                    }
                }
                'x' => {
                    base = Base::Hexadecimal;
                    self.advance();
                    if !self.consume_hexadecimal_digits() {
                        return LiteralKind::Int {
                            base,
                            empty_int: true,
                        };
                    }
                }
                // Not a base prefix; consume additional digits.
                '0'..='9' | '_' => {
                    self.consume_decimal_digits();
                }

                // Also not a base prefix; nothing more to do here.
                '.' | 'e' | 'E' => {}

                // Just a 0.
                _ => {
                    return LiteralKind::Int {
                        base,
                        empty_int: false,
                    }
                }
            }
        } else {
            // No base prefix, parse number in the usual way.
            self.consume_decimal_digits();
        };

        match self.peek() {
            '.' if self.peek_nth(1) != '.' && !is_ident_start(self.peek_nth(1)) => {
                self.advance();
                let mut empty_exponent = false;
                if self.peek().is_ascii_digit() {
                    self.consume_decimal_digits();
                    match self.peek() {
                        'e' | 'E' => {
                            self.advance();
                            empty_exponent = !self.consume_float_exponent();
                        }
                        _ => (),
                    }
                }

                LiteralKind::Float {
                    base,
                    empty_exponent,
                }
            }
            'e' | 'E' => {
                self.advance();
                let empty_exponent = !self.consume_float_exponent();
                LiteralKind::Float {
                    base,
                    empty_exponent,
                }
            }
            _ => LiteralKind::Int {
                base,
                empty_int: false,
            },
        }
    }

    fn consume_float_exponent(&mut self) -> bool {
        if self.peek() == '-' || self.peek() == '+' {
            self.advance();
        }
        self.consume_decimal_digits()
    }

    fn consume_decimal_digits(&mut self) -> bool {
        let mut has_digits = false;
        while self.peek().is_ascii_digit() {
            has_digits = true;
            self.advance();
        }

        has_digits
    }

    fn consume_hexadecimal_digits(&mut self) -> bool {
        let mut has_digits = false;
        while self.peek().is_ascii_hexdigit() {
            has_digits = true;
            self.advance();
        }

        has_digits
    }

    fn string(&mut self) -> LiteralKind {
        while !self.is_at_end() && self.peek() != '"' {
            self.advance();
        }

        if self.advance() != '"' {
            LiteralKind::Str { terminated: false }
        } else {
            LiteralKind::Str { terminated: true }
        }
    }

    fn char(&mut self) -> LiteralKind {
        while !self.is_at_end() && self.peek() != '\'' {
            self.advance();
        }

        if self.advance() != '\'' {
            LiteralKind::Char { terminated: false }
        } else {
            LiteralKind::Char { terminated: true }
        }
    }

    fn annotation(&mut self) -> TokenKind {
        let name = match self.identifier() {
            TokenKind::Ident(s) => s,
            _ => unreachable!(),
        };

        TokenKind::Annotation(name)
    }

    fn identifier(&mut self) -> TokenKind {
        while !self.is_at_end() && self.peek().is_ascii_alphanumeric() {
            self.advance();
        }

        TokenKind::Ident(self.src[self.start..self.current].to_string())
    }

    fn line_comment(&mut self) -> TokenKind {
        while self.peek() != '\n' {
            self.advance();
        }

        TokenKind::LineComment
    }

    fn block_comment(&mut self) -> TokenKind {
        while !self.is_at_end() && self.peek() != '*' && self.peek_nth(1) != '/' {
            self.advance();
        }

        if self.is_at_end() {
            return TokenKind::BlockComment { terminated: false };
        }

        self.advance_by(2);

        TokenKind::BlockComment { terminated: true }
    }
}
