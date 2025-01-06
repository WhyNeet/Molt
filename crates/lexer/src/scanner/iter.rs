use super::{
    token::{Token, TokenKind},
    Scanner,
};

impl<'a> Iterator for Scanner<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.advance_token();
        if token.kind != TokenKind::Eof {
            Some(token)
        } else {
            None
        }
    }
}
