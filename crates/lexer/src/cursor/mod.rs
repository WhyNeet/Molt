pub mod token;

use std::str::Chars;

use token::{Base, LiteralKind, Token, TokenKind};

const EOF_CHAR: char = '\0';

pub struct Cursor<'a> {
    remaining: usize,
    chars: Chars<'a>,
    prev: Option<char>,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Cursor<'a> {
        Cursor {
            remaining: input.len(),
            chars: input.chars(),
            prev: None,
        }
    }

    /// Creates an iterator that produces tokens from the input string.
    pub fn tokenize(input: &str) -> impl Iterator<Item = Token> + '_ {
        let mut cursor = Cursor::new(input);
        std::iter::from_fn(move || {
            let token = cursor.advance_token();
            if token.kind != TokenKind::Eof {
                Some(token)
            } else {
                None
            }
        })
    }

    pub fn advance(&mut self) -> char {
        let prev = self.chars.next().unwrap_or(EOF_CHAR);
        self.prev = Some(prev);
        prev
    }

    pub fn consumed(&self) -> u32 {
        (self.remaining - self.chars.as_str().len()) as u32
    }

    pub fn peek(&self) -> char {
        self.chars.clone().next().unwrap_or(EOF_CHAR)
    }

    pub fn peek_second(&self) -> char {
        let mut chars = self.chars.clone();
        chars.next();
        chars.next().unwrap_or(EOF_CHAR)
    }

    pub fn prev(&self) -> Option<char> {
        self.prev
    }

    fn reset_remaining(&mut self) {
        self.remaining = self.chars.as_str().len()
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

impl<'a> Cursor<'a> {
    pub(crate) fn advance_token(&mut self) -> Token {
        let first_char = match self.advance() {
            EOF_CHAR => return Token::new(TokenKind::Eof, 0),
            char => char,
        };

        let token_kind = match first_char {
            '0'..='9' => {
                let literal_kind = self.number();
                let suffix_start = self.consumed();
                self.consume_literal_suffix();
                TokenKind::Literal {
                    kind: literal_kind,
                    suffix_start,
                }
            }

            '/' => match self.peek() {
                '/' => self.line_comment(),
                '*' => self.block_comment(),
                _ => TokenKind::Slash,
            },

            // One-symbol tokens.
            ';' => TokenKind::Semi,
            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            '{' => TokenKind::OpenBrace,
            '}' => TokenKind::CloseBrace,
            '[' => TokenKind::OpenBracket,
            ']' => TokenKind::CloseBracket,
            '@' => match self.peek() {
                char if is_ident_start(char) => {
                    self.consume_identifier();
                    TokenKind::Annotation
                }
                _ => TokenKind::At,
            },
            '#' => TokenKind::Pound,
            '?' => TokenKind::Question,
            ':' => TokenKind::Colon,
            '$' => TokenKind::Dollar,
            '=' => match self.peek() {
                '=' => TokenKind::EqEq,
                _ => TokenKind::Eq,
            },
            '!' => match self.peek() {
                '=' => TokenKind::Ne,
                _ => TokenKind::Bang,
            },
            '<' => match self.peek() {
                '=' => TokenKind::Le,
                _ => TokenKind::Lt,
            },
            '>' => match self.peek() {
                '=' => TokenKind::Ge,
                _ => TokenKind::Gt,
            },
            '-' => TokenKind::Minus,
            '&' => match self.peek() {
                '&' => TokenKind::AndAnd,
                _ => TokenKind::And,
            },
            '|' => match self.peek() {
                '|' => TokenKind::OrOr,
                _ => TokenKind::Or,
            },
            '+' => TokenKind::Plus,
            '*' => TokenKind::Star,
            '^' => TokenKind::Caret,
            '%' => TokenKind::Percent,

            other if is_ident_start(other) => {
                self.consume_identifier();
                TokenKind::Ident
            }

            '"' => {
                let terminated = self.double_quoted_string();
                let suffix_start = self.consumed();
                if terminated {
                    self.consume_literal_suffix();
                }

                let kind = LiteralKind::Str { terminated };
                TokenKind::Literal { kind, suffix_start }
            }

            '\'' => {
                let terminated = self.single_quoted_string();
                let suffix_start = self.consumed();
                if terminated {
                    self.consume_literal_suffix();
                }

                let kind = LiteralKind::Char { terminated };
                TokenKind::Literal { kind, suffix_start }
            }

            c if is_whitespace(c) => self.whitespace(),
            c if !c.is_ascii() => TokenKind::InvalidIdent,
            '\n' => {
                self.advance();
                TokenKind::NewLine
            }
            _ => TokenKind::Unknown,
        };

        let res = Token::new(token_kind, self.consumed());
        self.reset_remaining();
        res
    }

    fn line_comment(&mut self) -> TokenKind {
        while self.advance() != '\n' {}
        TokenKind::LineComment
    }

    fn block_comment(&mut self) -> TokenKind {
        while self.advance() != '*' && self.advance() != '/' {
            if self.peek() == EOF_CHAR {
                return TokenKind::BlockComment { terminated: false };
            }
        }

        TokenKind::BlockComment { terminated: true }
    }

    fn whitespace(&mut self) -> TokenKind {
        while is_whitespace(self.peek()) {
            self.advance();
        }

        TokenKind::Whitespace
    }

    fn double_quoted_string(&mut self) -> bool {
        while self.peek() != EOF_CHAR {
            if self.advance() == '"' {
                return true;
            }
        }

        self.advance();

        false
    }

    fn single_quoted_string(&mut self) -> bool {
        while self.peek() != EOF_CHAR {
            if self.advance() == '\'' {
                return true;
            }
        }

        self.advance();

        false
    }

    fn number(&mut self) -> LiteralKind {
        let prev = self.prev().unwrap();
        let mut base = Base::Decimal;
        if prev == '0' {
            // Attempt to parse encoding base.
            match self.peek() {
                'b' => {
                    base = Base::Binary;
                    self.advance();
                    if !self.consume_hexadecimal_digits() {
                        return LiteralKind::Int {
                            base,
                            empty_int: true,
                        };
                    }
                }
                'o' => {
                    base = Base::Octal;
                    self.advance();
                    if !self.consume_hexadecimal_digits() {
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
                    self.consume_hexadecimal_digits();
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
            self.consume_hexadecimal_digits();
        };

        match self.peek() {
            // Don't be greedy if this is actually an
            // integer literal followed by field/method access or a range pattern
            // (`0..2` and `12.foo()`)
            '.' if self.peek_second() != '.' && !is_ident_start(self.peek_second()) => {
                // might have stuff after the ., and if it does, it needs to start
                // with a number
                self.advance();
                let mut empty_exponent = false;
                if self.peek().is_ascii_digit() {
                    self.consume_decimal_digits();
                    match self.peek() {
                        'e' | 'E' => {
                            self.advance();
                            empty_exponent = !self.consume_hexadecimal_digits();
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
                let empty_exponent = !self.consume_hexadecimal_digits();
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

    fn consume_hexadecimal_digits(&mut self) -> bool {
        let mut has_digits = false;
        loop {
            match self.peek() {
                '_' => {
                    self.advance();
                }
                '0'..'9' | 'a'..='f' | 'A'..='F' => {
                    has_digits = true;
                    self.advance();
                }
                _ => break,
            }
        }

        has_digits
    }

    fn consume_decimal_digits(&mut self) -> bool {
        let mut has_digits = false;
        loop {
            match self.peek() {
                '_' => {
                    self.advance();
                }
                '0'..'9' => {
                    has_digits = true;
                    self.advance();
                }
                _ => break,
            }
        }

        has_digits
    }

    fn consume_literal_suffix(&mut self) {
        self.consume_identifier();
    }

    fn consume_identifier(&mut self) {
        if !is_ident_start(self.peek()) {
            return;
        }

        self.advance();

        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            self.advance();
        }
    }
}
