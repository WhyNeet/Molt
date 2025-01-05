use std::str::Chars;

use crate::token::{Base, LiteralKind, Token, TokenKind};

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

    pub fn advance(&mut self) -> char {
        let prev = self.chars.next().unwrap_or(EOF_CHAR);
        self.prev = Some(prev);
        prev
    }

    pub fn consumed(&self) -> u32 {
        (self.remaining - self.chars.as_str().len()) as u32
    }

    pub fn peek(&self) -> char {
        self.chars.clone().next().unwrap()
    }

    pub fn peek_second(&self) -> char {
        let mut chars = self.chars.clone();
        chars.next();
        chars.next().unwrap()
    }

    pub fn prev(&self) -> Option<char> {
        self.prev
    }

    fn reset_remaining(&mut self) {
        self.remaining = self.chars.as_str().len()
    }
}

impl<'a> Cursor<'a> {
    fn advance_token(&mut self) -> Token {
        let first_char = match self.advance() {
            EOF_CHAR => return Token::new(TokenKind::Eof, 0),
            char => char,
        };

        let token_kind = match first_char {
            '0'..'9' => {
                let literal_kind = self.number();
                let suffix_start = self.consumed();
                self.consume_literal_suffix();
                TokenKind::Literal {
                    kind: literal_kind,
                    suffix_start,
                }
            }
            _ => todo!(),
        };

        let res = Token::new(token_kind, self.consumed());
        self.reset_remaining();
        res
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
            '.' if self.peek_second() != '.' && !self.peek_second().is_ascii_alphanumeric() => {
                // might have stuff after the ., and if it does, it needs to start
                // with a number
                self.advance();
                let mut empty_exponent = false;
                if self.peek().is_ascii_digit() {
                    self.consume_hexadecimal_digits();
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

    fn consume_literal_suffix(&mut self) {
        self.consume_identifier();
    }

    fn consume_identifier(&mut self) {
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            self.advance();
        }
    }
}

impl<'a> Iterator for Cursor<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.advance_token();
        if token.kind == TokenKind::Eof {
            None
        } else {
            Some(token)
        }
    }
}
