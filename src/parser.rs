use crate::ast::{Expression, IdentifierExpression, LetStatement, Program, Statement};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use std::mem::swap;
use std::result;

#[derive(Debug, Eq, PartialEq)]
pub enum ParseError {
    Err(String),
}

pub type Result<T> = result::Result<T, ParseError>;

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Parser {
        Parser {
            current_token: lexer.next_token(),
            peek_token: lexer.next_token(),
            lexer,
        }
    }

    pub fn parse_program(&mut self) -> Result<Program> {
        let mut program = Program::new();

        while !self.current_token_is(TokenType::EOF) {
            match self.parse_statement() {
                Ok(s) => program.statements.push(s),
                _ => {}
            }
            self.next_token();
        }

        Ok(program)
    }

    fn next_token(&mut self) {
        swap(&mut self.current_token, &mut self.peek_token);
        self.peek_token = self.lexer.next_token();
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        match self.current_token.token_type {
            TokenType::Let => Ok(Statement::Let(self.parse_let_statement()?)),
            _ => Err(ParseError::Err("dame".into())),
        }
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement> {
        let token = self.current_token.clone();

        if !self.expect_peek(TokenType::Ident) {
            return Err(ParseError::Err("hoge".into()));
        }

        let t = self.current_token.clone();
        let name = IdentifierExpression {
            value: t.literal.clone(),
            token: t,
        };

        if !self.expect_peek(TokenType::Assign) {
            return Err(ParseError::Err("fuga".into()));
        }

        // TODO: 式を実装したら実装する
        while !self.current_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        let cloned_name = name.clone();
        Ok(LetStatement {
            token,
            name,
            value: Expression::Identifier(cloned_name), // TODO
        })
    }

    fn current_token_is(&self, t: TokenType) -> bool {
        self.current_token.token_type == t
    }

    fn peed_token_is(&self, t: TokenType) -> bool {
        self.peek_token.token_type == t
    }

    fn expect_peek(&mut self, t: TokenType) -> bool {
        if self.peed_token_is(t) {
            self.next_token();
            true
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Node;
    use crate::ast::Statement;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_let_statements() {
        let input = r#"
            let x = 5;
            let y = 10;
            let foobar = 838383;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap();
        assert_eq!(program.statements.len(), 3);

        let tests: Vec<(&str)> = vec![("x"), ("y"), ("foobar")];

        for (i, t) in tests.iter().enumerate() {
            let stmt = &program.statements[i];
            assert_eq!(stmt.token_literal(), "let");

            match stmt {
                Statement::Let(l) => {
                    assert_eq!(l.name.value, *t);
                    assert_eq!(l.name.token.literal, *t)
                }
                s => panic!("Invalid statement: {:?}", s),
            }
        }
    }
}
