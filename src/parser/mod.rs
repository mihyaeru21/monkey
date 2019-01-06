#[cfg(test)]
mod tests;

use crate::ast::BlockStatement;
use crate::ast::CallExpression;
use crate::ast::FunctionLiteral;
use crate::ast::IfExpression;
use crate::ast::{
    Boolean, Expression, ExpressionStatement, Identifier, InfixExpression, IntegerLiteral,
    LetStatement, PrefixExpression, Program, ReturnStatement, Statement,
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
    Equals,      // == !=
    LessGreater, // > <
    Sum,         // + -
    Product,     // * /
    Prefix,      // -x !x
    Call,        // add(x)
}

fn token_type_to_precedence(token_type: TokenType) -> Precedence {
    match token_type {
        TokenType::Eq => Precedence::Equals,
        TokenType::NotEq => Precedence::Equals,
        TokenType::Lt => Precedence::LessGreater,
        TokenType::Gt => Precedence::LessGreater,
        TokenType::Plus => Precedence::Sum,
        TokenType::Minus => Precedence::Sum,
        TokenType::Slash => Precedence::Product,
        TokenType::Asterisk => Precedence::Product,
        TokenType::LParen => Precedence::Call,
        _ => Precedence::Lowest,
    }
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

        if self.errors.len() == 0 {
            Ok(program)
        } else {
            Err(ParseError::Err("failed to parse.".into()))
        }
    }

    pub fn get_errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn next_token(&mut self) {
        swap(&mut self.current_token, &mut self.peek_token);
        self.peek_token = self.lexer.next_token();
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        match self.current_token.token_type {
            TokenType::Let => Ok(Statement::Let(self.parse_let_statement()?)),
            TokenType::Return => Ok(Statement::Return(self.parse_return_statement()?)),
            _ => Ok(Statement::Expression(self.parse_expression_statement()?)),
        }
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement> {
        let token = self.current_token.clone();

        if !self.expect_peek(TokenType::Ident) {
            return Err(ParseError::Err("hoge".into()));
        }

        let name = Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        };

        if !self.expect_peek(TokenType::Assign) {
            return Err(ParseError::Err("fuga".into()));
        }

        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Ok(LetStatement { token, name, value })
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement> {
        let token = self.current_token.clone();

        self.next_token();

        let return_value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Ok(ReturnStatement {
            token,
            return_value,
        })
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStatement> {
        let token = self.current_token.clone();
        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Ok(ExpressionStatement { token, expression })
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement> {
        let token = self.current_token.clone();

        self.next_token();

        let mut statements = Vec::new();
        while !self.current_token_is(TokenType::RBrace) && !self.current_token_is(TokenType::EOF) {
            statements.push(self.parse_statement()?);
            self.next_token();
        }

        Ok(BlockStatement { token, statements })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression> {
        let mut left_expression: Expression = match self.current_token.token_type {
            TokenType::Ident => Expression::Identifier(self.parse_identifier()),
            TokenType::Int => Expression::IntegerLiteral(self.parse_integer_literal()?),
            TokenType::Bang | TokenType::Minus => {
                Expression::Prefix(self.parse_prefix_expression()?)
            }
            TokenType::True | TokenType::False => Expression::BooleanLiteral(self.parse_boolean()?),
            TokenType::LParen => self.parse_grouped_expression()?,
            TokenType::If => Expression::If(self.parse_if_expression()?),
            TokenType::Function => Expression::Function(self.parse_function_literal()?),
            t => {
                let msg = format!("no prefix parse function for `{}` found", t);
                self.errors.push(msg);
                return Err(ParseError::Err(
                    "エラーにはしなくていいかも".into(),
                ));
            }
        };

        while !self.peek_token_is(TokenType::Semicolon) && precedence < self.peek_precedence() {
            let infix_func = match self.peek_token.token_type {
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Asterisk
                | TokenType::Eq
                | TokenType::NotEq
                | TokenType::Lt
                | TokenType::Gt => Parser::parse_infix_expression,
                TokenType::LParen => Parser::parse_call_expression,
                _ => return Ok(left_expression),
            };

            self.next_token();
            left_expression = infix_func(self, left_expression)?;
        }

        Ok(left_expression)
    }

    fn parse_identifier(&self) -> Identifier {
        let token = self.current_token.clone();
        let value = token.literal.clone();
        Identifier { token, value }
    }

    fn parse_integer_literal(&mut self) -> Result<IntegerLiteral> {
        let token = self.current_token.clone();
        let value: i64 = match token.literal.parse() {
            Ok(i) => i,
            Err(_) => {
                let message = format!("could not parse {} as integer", token.literal);
                self.errors.push(message.clone());
                return Err(ParseError::Err(message));
            }
        };

        Ok(IntegerLiteral { token, value })
    }

    fn parse_prefix_expression(&mut self) -> Result<PrefixExpression> {
        let token = self.current_token.clone();

        self.next_token();

        let operator = token.literal.clone();
        let right = self.parse_expression(Precedence::Prefix)?;

        Ok(PrefixExpression {
            token,
            operator,
            right: Box::new(right),
        })
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression> {
        let token = self.current_token.clone();

        let precedence = self.current_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;
        let operator = token.literal.clone();

        Ok(Expression::Infix(InfixExpression {
            token,
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }))
    }

    fn parse_boolean(&mut self) -> Result<Boolean> {
        Ok(Boolean {
            token: self.current_token.clone(),
            value: self.current_token_is(TokenType::True),
        })
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(TokenType::RParen) {
            return Err(ParseError::Err("invalid )".into()));
        }

        Ok(exp)
    }

    fn parse_if_expression(&mut self) -> Result<IfExpression> {
        if !self.expect_peek(TokenType::LParen) {
            return Err(ParseError::Err("expect `(`".into()));
        }

        let token = self.current_token.clone();
        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(TokenType::RParen) {
            return Err(ParseError::Err("expect `)`".into()));
        }

        if !self.expect_peek(TokenType::LBrace) {
            return Err(ParseError::Err("expect `{`".into()));
        }

        let consequence = self.parse_block_statement()?;
        let alternative = if self.peek_token_is(TokenType::Else) {
            self.next_token();
            if !self.expect_peek(TokenType::LBrace) {
                return Err(ParseError::Err("expect `{`".into()));
            }
            Some(Box::new(Statement::Block(self.parse_block_statement()?)))
        } else {
            None
        };

        Ok(IfExpression {
            token,
            condition: Box::new(condition),
            consequence: Box::new(Statement::Block(consequence)),
            alternative,
        })
    }

    fn parse_function_literal(&mut self) -> Result<FunctionLiteral> {
        let token = self.current_token.clone();

        if !self.expect_peek(TokenType::LParen) {
            return Err(ParseError::Err("expect `(`".into()));
        }

        let parameters = self.parse_function_parameters()?;

        if !self.expect_peek(TokenType::LBrace) {
            return Err(ParseError::Err("expect `{`".into()));
        }

        let body = self.parse_block_statement()?;

        Ok(FunctionLiteral {
            token,
            parameters,
            body,
        })
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>> {
        let mut identifiers = Vec::new();

        if self.peek_token_is(TokenType::RParen) {
            self.next_token();
            return Ok(identifiers);
        }

        self.next_token();

        identifiers.push(Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        });

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();
            identifiers.push(Identifier {
                token: self.current_token.clone(),
                value: self.current_token.literal.clone(),
            });
        }

        if !self.expect_peek(TokenType::RParen) {
            return Err(ParseError::Err("expect `)`".into()));
        }

        Ok(identifiers)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression> {
        let token = self.current_token.clone();

        Ok(Expression::Call(CallExpression {
            token,
            function: Box::new(function),
            arguments: self.parse_call_arguments()?,
        }))
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>> {
        let mut args = Vec::new();

        if self.peek_token_is(TokenType::RParen) {
            self.next_token();
            return Ok(args);
        }

        self.next_token();
        args.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Precedence::Lowest)?);
        }

        if !self.expect_peek(TokenType::RParen) {
            return Err(ParseError::Err("expect `)`".into()));
        }

        Ok(args)
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
            "expected next token to be `{}`, got `{}` instead",
            t, self.peek_token.token_type
        ));
    }

    fn peek_precedence(&self) -> Precedence {
        token_type_to_precedence(self.peek_token.token_type)
    }

    fn current_precedence(&self) -> Precedence {
        token_type_to_precedence(self.current_token.token_type)
    }
}
