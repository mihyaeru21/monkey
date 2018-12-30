use crate::ast::{
    Expression, ExpressionStatement, Identifier, LetStatement, Program, ReturnStatement, Statement,
};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use std::mem::swap;
use std::result;

#[derive(Debug, Eq, PartialEq)]
pub enum ParseError {
    Err(String),
}

pub type Result<T> = result::Result<T, ParseError>;

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone)]
enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > <
    Sum,         // +
    Product,     // (
    Prefix,      // -x !x
    Call,        // add(x)
}

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Parser {
        Parser {
            current_token: lexer.next_token(),
            peek_token: lexer.next_token(),
            lexer,
            errors: Vec::new(),
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

    pub fn get_errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn next_token(&mut self) {
        swap(&mut self.current_token, &mut self.peek_token);
        self.peek_token = self.lexer.next_token();
    }

    fn parse_statement(&mut self) -> Result<Box<dyn Statement>> {
        match self.current_token.token_type {
            TokenType::Let => Ok(Box::new(self.parse_let_statement()?)),
            TokenType::Return => Ok(Box::new(self.parse_return_statement()?)),
            _ => Ok(Box::new(self.parse_expression_statement()?)),
        }
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement> {
        let token = self.current_token.clone();

        if !self.expect_peek(TokenType::Ident) {
            return Err(ParseError::Err("hoge".into()));
        }

        let t = self.current_token.clone();
        let name = Identifier {
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
            value: Box::new(cloned_name), // TODO
        })
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement> {
        let token = self.current_token.clone();

        self.next_token();

        let cloned_token = token.clone();
        Ok(ReturnStatement {
            token,
            return_value: Box::new(Identifier {
                token: cloned_token,
                value: String::new(),
            }),
        })
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStatement> {
        let token = self.current_token.clone();
        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Ok(ExpressionStatement {
            token,
            expression: Box::new(expression),
        })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<impl Expression> {
        let left_expression = match self.current_token.token_type {
            TokenType::Ident => self.parse_identifier(),
            _ => {
                return Err(ParseError::Err(
                    "エラーにはしなくていいかも".into(),
                ));
            }
        };

        Ok(left_expression)
    }

    fn parse_identifier(&self) -> Identifier {
        let token = self.current_token.clone();
        let value = token.literal.clone();
        Identifier { token, value }
    }

    fn current_token_is(&self, t: TokenType) -> bool {
        self.current_token.token_type == t
    }

    fn peek_token_is(&self, t: TokenType) -> bool {
        self.peek_token.token_type == t
    }

    fn expect_peek(&mut self, t: TokenType) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            self.peek_error(t);
            false
        }
    }

    fn peek_error(&mut self, t: TokenType) {
        self.errors.push(format!(
            "expected next token to be {:?}, got {:?} instead",
            t, self.peek_token.token_type
        ));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Node;
    use crate::lexer::Lexer;

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
        check_parse_errors(&parser);

        assert_eq!(program.statements.len(), 3);

        let tests: Vec<(&str)> = vec![("x"), ("y"), ("foobar")];

        for (i, t) in tests.iter().enumerate() {
            let statement = &program.statements[i];
            assert_eq!(statement.token_literal(), "let");

            let let_statement = statement.as_let_ref().unwrap();
            assert_eq!(let_statement.token_literal(), "let");
            assert_eq!(let_statement.name.value, *t);
            assert_eq!(let_statement.name.token.literal, *t)
        }
    }

    #[test]
    fn test_return_statement() {
        let input = r#"
            return 5;
            return 10;
            return 993322;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        check_parse_errors(&parser);

        assert_eq!(program.statements.len(), 3);

        for statement in &program.statements {
            let return_statement = statement.as_return_ref().unwrap();
            assert_eq!(return_statement.token_literal(), "return");
        }
    }

    fn check_parse_errors(parser: &Parser) {
        let errors = parser.get_errors();
        if errors.len() == 0 {
            return;
        }

        eprintln!("parser has {} errors", errors.len());
        for error in errors {
            eprintln!("{}", error)
        }

        panic!();
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        check_parse_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        let statement = program.statements[0].as_expression_ref().unwrap();
        let identifier = statement.expression.as_identifier_ref().unwrap();
        assert_eq!(identifier.value, "foobar");
        assert_eq!(identifier.token_literal(), "foobar");
    }
}
