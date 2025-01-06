use std::{iter::Peekable, mem};

use ast::{
    expression::Expression,
    literal::{Literal, Number, Type},
    operator::Operator,
    statement::Statement,
};
use lexer::scanner::{
    keywords::Keyword,
    token::{Base, Literal as LiteralToken, Token, TokenKind},
    Scanner,
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }
}

impl Parser {
    pub fn parse(&mut self) -> Vec<Statement> {
        self.program()
    }

    fn program(&mut self) -> Vec<Statement> {
        let mut statements = vec![];

        while !self.is_at_end() {
            statements.push(self.declaration())
        }

        statements
    }

    fn declaration(&mut self) -> Statement {
        let mut annotations = vec![];

        while let Some(annotation) = self.matches(TokenKind::Annotation(String::new())) {
            let annotation = match &annotation.kind {
                TokenKind::Annotation(annotation) => annotation.to_string(),
                _ => unreachable!(),
            };

            annotations.push(annotation);
        }

        if let Some(keyword) = self.matches(TokenKind::Keyword(Keyword::Fun)) {
            let keyword = match keyword.kind {
                TokenKind::Keyword(keyword) => keyword,
                _ => unreachable!(),
            };

            match keyword {
                Keyword::Let => self.var_decl(annotations),
                Keyword::Fun => self.fun_decl(annotations),
                _ => todo!(),
            }
        } else {
            self.expr_stmt()
        }
    }

    fn fun_decl(&mut self, annotations: Vec<String>) -> Statement {
        let identifier = self
            .matches(TokenKind::Ident(String::new()))
            .expect("expected identifier.");

        let identifier = match &identifier.kind {
            TokenKind::Ident(ident) => ident.to_string(),
            _ => panic!("expected identifier."),
        };

        self.matches(TokenKind::OpenParen).expect("expected `(`.");

        let parameters = self.parameters();

        let return_type = if self.matches(TokenKind::RArrow).is_some() {
            let return_type = self
                .matches(TokenKind::Ident(String::new()))
                .expect("expected type after `->`");
            let return_type = match &return_type.kind {
                TokenKind::Ident(ident) => ident.to_string(),
                _ => panic!("expected identifier."),
            };
            Type::try_from(return_type.as_str()).unwrap()
        } else {
            Type::Unit
        };

        let block = if self.matches(TokenKind::Semi).is_some() {
            // function without a block (possibly an external declaration)
            None
        } else {
            if self.matches(TokenKind::Eq).is_none() {
                panic!("expected `=`.");
            }

            Some(self.expression())
        };

        Statement::FunctionDeclaration {
            name: identifier,
            block,
            return_type,
            parameters,
            annotations,
        }
    }

    fn block(&mut self) -> Expression {
        let mut statements = vec![];

        while self.matches(TokenKind::CloseParen).is_none() {
            statements.push(self.declaration());
        }

        Expression::Block(statements)
    }

    fn parameters(&mut self) -> Vec<(String, Type)> {
        let mut parameters = vec![];

        if self
            .peek()
            .map(|token| match token.kind {
                TokenKind::Ident(_) => true,
                _ => false,
            })
            .unwrap_or(false)
        {
            parameters.push(self.ident_type_pair());
        }

        while self.matches(TokenKind::CloseParen).is_none() {
            self.matches(TokenKind::Comma).expect("expected `,`.");

            parameters.push(self.ident_type_pair());
        }

        parameters
    }

    fn ident_type_pair(&mut self) -> (String, Type) {
        let identifier = self
            .matches(TokenKind::Ident(String::new()))
            .expect("expected identifier.");

        let identifier = match &identifier.kind {
            TokenKind::Ident(ident) => ident.to_string(),
            _ => panic!("expected identifier."),
        };

        if self.matches(TokenKind::Colon).is_none() {
            panic!("expected `:`.");
        }

        let type_ident = self
            .matches(TokenKind::Ident(String::new()))
            .expect("expected identifier.");

        let type_ident = match &type_ident.kind {
            TokenKind::Ident(ident) => ident.to_string(),
            _ => panic!("expected identifier."),
        };

        let type_ident = Type::try_from(type_ident.as_str()).unwrap();

        (identifier, type_ident)
    }

    fn var_decl(&mut self, annotations: Vec<String>) -> Statement {
        let identifier = self
            .matches(TokenKind::Ident(String::new()))
            .expect("expected identifier.");

        let identifier = match &identifier.kind {
            TokenKind::Ident(ident) => ident.to_string(),
            _ => panic!("expected identifier."),
        };

        if self.matches(TokenKind::Eq).is_none() {
            panic!("expected `=`.");
        }

        let expression = self.expression();

        if self.matches(TokenKind::Semi).is_none() {
            panic!("expected `;`.");
        }

        Statement::VariableDeclaration {
            name: identifier,
            expr: expression,
            annotations,
        }
    }

    fn expr_stmt(&mut self) -> Statement {
        let expression = self.expression();

        Statement::Expression {
            expr: expression,
            end_semi: self.matches(TokenKind::Semi).is_some(),
        }
    }

    fn expression(&mut self) -> Expression {
        self.assignment()
    }

    fn assignment(&mut self) -> Expression {
        let expr = self.logic_or();

        if self.matches(TokenKind::Eq).is_none() {
            return expr;
        }

        let identifier = match expr {
            Expression::Identifier(ident) => ident,
            other => panic!("expected identifier, got: {other:?}"),
        };

        let expression = self.logic_or();

        Expression::Assignment {
            identifier,
            expr: Box::new(expression),
        }
    }

    fn logic_or(&mut self) -> Expression {
        let mut expr = self.logic_and();

        while self.matches_either(&[TokenKind::OrOr]) {
            let operator = self.prev().unwrap().try_into().unwrap();
            let right = self.logic_and();
            expr = Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        expr
    }

    fn logic_and(&mut self) -> Expression {
        let mut expr = self.equality();

        while self.matches_either(&[TokenKind::AndAnd]) {
            let operator = self.prev().unwrap().try_into().unwrap();
            let right = self.equality();
            expr = Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        expr
    }

    fn equality(&mut self) -> Expression {
        let mut expr = self.comparison();

        while self.matches_either(&[TokenKind::Eq, TokenKind::Ne]) {
            let operator = self.prev().unwrap().try_into().unwrap();
            let right = self.comparison();
            expr = Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        expr
    }

    fn comparison(&mut self) -> Expression {
        let mut expr = self.term();

        while self.matches_either(&[TokenKind::Gt, TokenKind::Ge, TokenKind::Lt, TokenKind::Le]) {
            let operator = self.prev().unwrap().try_into().unwrap();
            let right = self.term();
            expr = Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        expr
    }

    fn shift(&mut self) -> Expression {
        todo!()
    }

    fn term(&mut self) -> Expression {
        let mut expr = self.factor();

        while self.matches(TokenKind::Plus).is_some() || self.matches(TokenKind::Minus).is_some() {
            let operator = self.prev().unwrap().try_into().unwrap();
            let right = self.factor();
            expr = Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        expr
    }

    fn factor(&mut self) -> Expression {
        let mut expr = self.unary();
        println!("factor. current: {}", self.current);
        while self.matches(TokenKind::Star).is_some() || self.matches(TokenKind::Slash).is_some() {
            println!("factor. current: {}", self.current);
            let operator = self.prev().unwrap().try_into().unwrap();
            let right = self.unary();
            println!("left: {expr:?}; {operator:?}; right: {right:?}");
            expr = Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        expr
    }

    fn unary(&mut self) -> Expression {
        if self.matches(TokenKind::Bang).is_some() {
            Expression::Unary {
                operator: Operator::Not,
                expr: Box::new(self.unary()),
            }
        } else if self.matches(TokenKind::Minus).is_some() {
            Expression::Unary {
                operator: Operator::Neg,
                expr: Box::new(self.unary()),
            }
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Expression {
        if let Some(identifier) = self.matches(TokenKind::Ident("".to_string())) {
            let identifier = match &identifier.kind {
                TokenKind::Ident(ident) => ident,
                _ => unreachable!(),
            };
            Expression::Identifier(identifier.to_string())
        } else if let Some(literal) = self.matches(TokenKind::Literal(LiteralToken {
            kind: lexer::scanner::token::LiteralKind::Bool,
            suffix: None,
            symbol: "".to_string(),
        })) {
            let literal = match &literal.kind {
                TokenKind::Literal(literal) => literal.clone(),
                _ => unreachable!(),
            };
            Expression::Literal(self.literal(literal))
        } else if self.matches(TokenKind::OpenParen).is_some() {
            self.block()
        } else {
            panic!("expression expected, got: {:?}", self.peek())
        }
    }

    fn literal(&mut self, literal: LiteralToken) -> Literal {
        match literal.kind {
            lexer::scanner::token::LiteralKind::Int { base, empty_int } => {
                if empty_int {
                    panic!("empty int.");
                }
                let symbol = if base != Base::Decimal {
                    &literal.symbol[2..]
                } else {
                    &literal.symbol
                };
                let symbol = if let Some(ref suffix) = literal.suffix {
                    &symbol[..(symbol.len() - suffix.len())]
                } else {
                    symbol
                };

                let number = if let Some(suffix) = literal.suffix {
                    match suffix.as_str() {
                        "u8" => Number::UInt8(u8::from_str_radix(symbol, base as u32).unwrap()),
                        "i8" => Number::Int8(i8::from_str_radix(symbol, base as u32).unwrap()),
                        "u16" => Number::UInt16(u16::from_str_radix(symbol, base as u32).unwrap()),
                        "i16" => Number::Int16(i16::from_str_radix(symbol, base as u32).unwrap()),
                        "u32" => Number::UInt32(u32::from_str_radix(symbol, base as u32).unwrap()),
                        "i32" => Number::Int32(i32::from_str_radix(symbol, base as u32).unwrap()),
                        "u64" => Number::UInt64(u64::from_str_radix(symbol, base as u32).unwrap()),
                        "i64" => Number::Int64(i64::from_str_radix(symbol, base as u32).unwrap()),
                        other => panic!("unknown suffix: `{other}`."),
                    }
                } else {
                    Number::Int32(symbol.parse().unwrap())
                };

                Literal::Number(number)
            }
            lexer::scanner::token::LiteralKind::Float {
                base,
                empty_exponent,
            } => {
                if empty_exponent {
                    panic!("empty exponent.")
                }

                if base != Base::Decimal {
                    panic!("non-decimal float is not supported.")
                }

                let symbol = if let Some(ref suffix) = literal.suffix {
                    &literal.symbol[..(literal.symbol.len() - suffix.len())]
                } else {
                    &literal.symbol
                };

                let number = if let Some(suffix) = literal.suffix {
                    match suffix.as_str() {
                        "f32" => Number::Float32(symbol.parse().unwrap()),
                        "f64" => Number::Float64(symbol.parse().unwrap()),
                        other => panic!("unknown suffix after float: `{other}`"),
                    }
                } else {
                    Number::Float32(symbol.parse().unwrap())
                };

                Literal::Number(number)
            }
            lexer::scanner::token::LiteralKind::Str { terminated } => {
                if !terminated {
                    panic!("unterminated string.");
                }

                Literal::Str(literal.symbol[1..(literal.symbol.len() - 1)].to_string())
            }
            lexer::scanner::token::LiteralKind::Char { terminated } => {
                if !terminated {
                    panic!("unterminated char.");
                }

                Literal::Char(literal.symbol.as_bytes()[1] as char)
            }
            lexer::scanner::token::LiteralKind::Bool => Literal::Bool(literal.symbol == "true"),
        }
    }

    fn matches(&mut self, kind: TokenKind) -> Option<&Token> {
        if !self.is_at_end()
            && mem::discriminant(&self.tokens[self.current].kind) == mem::discriminant(&kind)
        {
            let token = self.tokens.get(self.current);
            self.current += 1;
            token
        } else {
            None
        }
    }

    fn matches_either(&mut self, kinds: &[TokenKind]) -> bool {
        for kind in kinds {
            if let Some(_) = self.matches(kind.clone()) {
                return true;
            }
        }

        false
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn prev(&self) -> Option<&Token> {
        self.tokens.get(self.current - 1)
    }

    fn is_at_end(&mut self) -> bool {
        self.current >= self.tokens.len()
    }
}
