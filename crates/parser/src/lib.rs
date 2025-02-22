use std::{mem, rc::Rc};

use ast::{annotation::Annotation, expression::Expression, statement::Statement};
use common::{
    keywords::Keyword,
    token::{Base, Literal as LiteralToken, LiteralKind, Token, TokenKind},
    Literal, Number, Operator, Type,
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
            self.matches_either(&[TokenKind::BlockComment { terminated: true }]);

            statements.push(self.statement())
        }

        statements
    }

    fn statement(&mut self) -> Statement {
        if let Some(keyword) = self.matches(TokenKind::Keyword(Keyword::Fun)) {
            let keyword = match keyword.kind {
                TokenKind::Keyword(keyword) => keyword,
                _ => unreachable!(),
            };

            match keyword {
                Keyword::Let => self.var_decl(),
                Keyword::Fun => self.fun_decl(),
                Keyword::Return => self.return_stmt(),
                _ => {
                    self.back();
                    self.expr_stmt()
                }
            }
        } else if self
            .peek()
            .map(|token| token.kind == TokenKind::At)
            .unwrap_or(false)
        {
            self.annotated()
        } else {
            self.expr_stmt()
        }
    }

    fn return_stmt(&mut self) -> Statement {
        let expr = self.expression();

        self.matches(TokenKind::Semi).expect("expected semicolon.");

        Statement::Return(Rc::new(expr))
    }

    fn annotated(&mut self) -> Statement {
        let mut annotations = vec![];

        while self.matches(TokenKind::At).is_some() {
            annotations.push(self.annotation());
        }

        let statement = self.statement();

        Statement::Annotated {
            annotations,
            stmt: Rc::new(statement),
        }
    }

    fn annotation(&mut self) -> Annotation {
        let identifier = self
            .matches(TokenKind::Ident(String::new()))
            .expect("expected identifier.");

        let identifier = match &identifier.kind {
            TokenKind::Ident(ident) => ident.to_string(),
            _ => panic!("expected identifier."),
        };

        let mut args = vec![];

        if self.matches(TokenKind::OpenParen).is_some()
            && self.matches(TokenKind::CloseParen).is_none()
        {
            let arg = self
                .matches(TokenKind::Ident(String::new()))
                .expect("expected identifier.")
                .as_ident()
                .unwrap();

            args.push(arg.to_string());

            while self.matches(TokenKind::Comma).is_some() {
                let arg = self
                    .matches(TokenKind::Ident(String::new()))
                    .expect("expected identifier.")
                    .as_ident()
                    .unwrap();

                args.push(arg.to_string());
            }

            self.matches(TokenKind::CloseParen).expect("expected `)`.");
        }

        Annotation {
            name: identifier,
            arguments: args,
        }
    }

    fn fun_decl(&mut self) -> Statement {
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
            self.molt_type().expect("expected type after `->`")
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
            block: block.map(Rc::new),
            return_type,
            parameters,
        }
    }

    fn block(&mut self) -> Expression {
        let mut statements = vec![];

        while self.matches(TokenKind::CloseBrace).is_none() {
            statements.push(Rc::new(self.statement()));
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

        let type_ident = self.molt_type().expect("expected type.");

        (identifier, type_ident)
    }

    fn var_decl(&mut self) -> Statement {
        let is_mut = self
            .matches_exact(TokenKind::Keyword(Keyword::Mut))
            .is_some();

        let identifier = self
            .matches(TokenKind::Ident(String::new()))
            .expect("expected identifier.")
            .as_ident()
            .unwrap()
            .to_string();

        let ty = if self.matches(TokenKind::Colon).is_some() {
            Some(self.molt_type().expect("expected type."))
        } else {
            None
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
            expr: Rc::new(expression),
            ty,
            is_mut,
        }
    }

    fn molt_type(&mut self) -> Option<Type> {
        let is_ptr = self.matches(TokenKind::Star).is_some();

        self.matches(TokenKind::Ident(String::new()))
            .expect("expected type.")
            .as_ident()
            .map(Type::try_from)
            .map(|ty| ty.unwrap())
            .map(|ty| if is_ptr { Type::Ptr(Box::new(ty)) } else { ty })
    }

    fn expr_stmt(&mut self) -> Statement {
        let expression = self.expression();

        Statement::Expression {
            expr: Rc::new(expression),
            end_semi: self.matches(TokenKind::Semi).is_some(),
        }
    }

    fn expression(&mut self) -> Expression {
        self.assignment()
    }

    fn assignment(&mut self) -> Expression {
        let assignee = self.logic_or();

        if self.matches(TokenKind::Eq).is_none() {
            return assignee;
        }

        let expression = self.logic_or();

        Expression::Assignment {
            assignee: Rc::new(assignee),
            expr: Rc::new(expression),
        }
    }

    fn logic_or(&mut self) -> Expression {
        let mut expr = self.logic_and();

        while self.matches_either(&[TokenKind::OrOr]) {
            let operator = self.prev().unwrap().try_into().unwrap();
            let right = self.logic_and();
            expr = Expression::Binary {
                left: Rc::new(expr),
                operator,
                right: Rc::new(right),
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
                left: Rc::new(expr),
                operator,
                right: Rc::new(right),
            };
        }

        expr
    }

    fn equality(&mut self) -> Expression {
        let mut expr = self.comparison();

        while self.matches_either(&[TokenKind::EqEq, TokenKind::Ne]) {
            let operator = self.prev().unwrap().try_into().unwrap();
            let right = self.comparison();
            expr = Expression::Binary {
                left: Rc::new(expr),
                operator,
                right: Rc::new(right),
            };
        }

        expr
    }

    fn comparison(&mut self) -> Expression {
        let mut expr = self.shift();

        while self.matches_either(&[TokenKind::Gt, TokenKind::Ge, TokenKind::Lt, TokenKind::Le]) {
            let operator = self.prev().unwrap().try_into().unwrap();
            let right = self.shift();
            expr = Expression::Binary {
                left: Rc::new(expr),
                operator,
                right: Rc::new(right),
            };
        }

        expr
    }

    fn shift(&mut self) -> Expression {
        let mut expr = self.term();

        while self.matches_either(&[TokenKind::LtLt, TokenKind::GtGt]) {
            let operator = self.prev().unwrap().try_into().unwrap();
            let right = self.term();
            expr = Expression::Binary {
                left: Rc::new(expr),
                operator,
                right: Rc::new(right),
            };
        }

        expr
    }

    fn term(&mut self) -> Expression {
        let mut expr = self.factor();

        while self.matches(TokenKind::Plus).is_some() || self.matches(TokenKind::Minus).is_some() {
            let operator = self.prev().unwrap().try_into().unwrap();
            let right = self.factor();
            expr = Expression::Binary {
                left: Rc::new(expr),
                operator,
                right: Rc::new(right),
            };
        }

        expr
    }

    fn factor(&mut self) -> Expression {
        let mut expr = self.cast();

        while self.matches(TokenKind::Star).is_some() || self.matches(TokenKind::Slash).is_some() {
            let operator = self.prev().unwrap().try_into().unwrap();
            let right = self.cast();
            expr = Expression::Binary {
                left: Rc::new(expr),
                operator,
                right: Rc::new(right),
            };
        }

        expr
    }

    fn cast(&mut self) -> Expression {
        let mut expr = self.unary();

        while self
            .matches_exact(TokenKind::Keyword(Keyword::As))
            .is_some()
        {
            let ty = self.molt_type().expect("expected type.");

            expr = Expression::Cast {
                expr: Rc::new(expr),
                ty,
            };
        }

        expr
    }

    fn unary(&mut self) -> Expression {
        if self.matches(TokenKind::Bang).is_some() {
            Expression::Unary {
                operator: Operator::Not,
                expr: Rc::new(self.unary()),
            }
        } else if self.matches(TokenKind::Minus).is_some() {
            Expression::Unary {
                operator: Operator::Neg,
                expr: Rc::new(self.unary()),
            }
        } else if self.matches(TokenKind::And).is_some() {
            Expression::Unary {
                operator: Operator::Ref,
                expr: Rc::new(self.unary()),
            }
        } else if self.matches(TokenKind::Star).is_some() {
            Expression::Unary {
                operator: Operator::Deref,
                expr: Rc::new(self.unary()),
            }
        } else {
            self.fn_call()
        }
    }

    fn fn_call(&mut self) -> Expression {
        let mut expr = self.member_access();

        while self.matches(TokenKind::OpenParen).is_some() {
            let arguments = self.arguments();

            self.matches(TokenKind::CloseParen).expect("expected `)`");

            expr = Expression::Call {
                expr: Rc::new(expr),
                arguments,
            };
        }

        expr
    }

    fn arguments(&mut self) -> Vec<Rc<Expression>> {
        let mut arguments = vec![];

        if self
            .peek()
            .map(|token| match token.kind {
                TokenKind::CloseParen => false,
                _ => true,
            })
            .unwrap_or(false)
        {
            // if not `ident()`
            arguments.push(Rc::new(self.expression()));
        }

        while self.matches(TokenKind::Comma).is_some() {
            arguments.push(Rc::new(self.expression()));
        }

        arguments
    }

    fn member_access(&mut self) -> Expression {
        let mut expr = self.primary();

        while self.matches(TokenKind::Dot).is_some() {
            let identifier = self
                .matches(TokenKind::Ident(String::new()))
                .expect("expected identifier.");
            let identifier = match identifier.kind {
                TokenKind::Ident(ref ident) => ident.to_string(),
                _ => unreachable!(),
            };

            expr = Expression::MemberAccess {
                expr: Rc::new(expr),
                ident: identifier,
            }
        }

        expr
    }

    fn primary(&mut self) -> Expression {
        if let Some(identifier) = self.matches(TokenKind::Ident("".to_string())) {
            let identifier = match &identifier.kind {
                TokenKind::Ident(ident) => ident,
                _ => unreachable!(),
            };

            Expression::Identifier(identifier.to_string())
        } else if let Some(literal) = self.matches(TokenKind::Literal(LiteralToken {
            kind: LiteralKind::Bool,
            suffix: None,
            symbol: "".to_string(),
        })) {
            let literal = match &literal.kind {
                TokenKind::Literal(literal) => literal.clone(),
                _ => unreachable!(),
            };
            Expression::Literal(Rc::new(self.literal(literal)))
        } else if self.matches(TokenKind::OpenParen).is_some() {
            self.grouping()
        } else if self.matches(TokenKind::OpenBrace).is_some() {
            self.block()
        } else if let Some(keyword) = self.matches(TokenKind::Keyword(Keyword::If)) {
            let keyword = match keyword.kind {
                TokenKind::Keyword(keyword) => keyword,
                _ => unreachable!(),
            };
            match keyword {
                Keyword::If => self.conditional(),
                Keyword::Loop => self.loop_expr(),
                Keyword::Break => Expression::Break,
                Keyword::Continue => Expression::Continue,
                other => todo!("{other:?}"),
            }
        } else {
            panic!("expression expected, got: {:?}", self.peek())
        }
    }

    fn loop_expr(&mut self) -> Expression {
        self.matches(TokenKind::OpenBrace).expect("expected `{`");

        let block = self.block();
        let statements = match block {
            Expression::Block(stmts) => stmts,
            _ => unreachable!(),
        };

        Expression::Loop(statements)
    }

    fn conditional(&mut self) -> Expression {
        let condition = self.expression();

        if self.matches(TokenKind::OpenBrace).is_none() {
            panic!("expected `{{`.");
        }

        let body = match self.block() {
            Expression::Block(stmts) => stmts,
            _ => unreachable!(),
        };

        let alternative = if self
            .matches_exact(TokenKind::Keyword(Keyword::Else))
            .is_some()
        {
            if self
                .matches_exact(TokenKind::Keyword(Keyword::If))
                .is_some()
            {
                Some(self.conditional())
            } else {
                if self.matches(TokenKind::OpenBrace).is_none() {
                    panic!("expected `{{`.");
                }

                Some(self.block())
            }
        } else {
            None
        };

        Expression::Conditional {
            condition: Rc::new(condition),
            body,
            alternative: alternative.map(Rc::new),
        }
    }

    fn grouping(&mut self) -> Expression {
        let expression = self.expression();

        if self.matches(TokenKind::CloseParen).is_none() {
            panic!("expected `)`.");
        }

        Expression::Grouping(Rc::new(expression))
    }

    fn literal(&mut self, literal: LiteralToken) -> Literal {
        match literal.kind {
            LiteralKind::Int { base, empty_int } => {
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
            LiteralKind::Float {
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
            LiteralKind::Str { terminated } => {
                if !terminated {
                    panic!("unterminated string.");
                }

                Literal::Str(literal.symbol[1..(literal.symbol.len() - 1)].to_string())
            }
            LiteralKind::Char { terminated } => {
                if !terminated {
                    panic!("unterminated char.");
                }

                Literal::Char(literal.symbol.as_bytes()[1] as char)
            }
            LiteralKind::Bool => Literal::Bool(literal.symbol == "true"),
        }
    }

    fn back(&mut self) {
        self.current -= 1;
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

    fn matches_exact(&mut self, kind: TokenKind) -> Option<&Token> {
        if !self.is_at_end() && self.tokens[self.current].kind == kind {
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

    fn peek_nth(&self, n: usize) -> Option<&Token> {
        self.tokens.get(self.current + n)
    }

    fn prev(&self) -> Option<&Token> {
        self.tokens.get(self.current - 1)
    }

    fn is_at_end(&mut self) -> bool {
        self.current >= self.tokens.len()
    }
}
