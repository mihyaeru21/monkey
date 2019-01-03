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

        // TODO: 式を実装したら実装する
        while !self.current_token_is(TokenType::Semicolon) {
            self.next_token();
        }

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

        Ok(ExpressionStatement { token, expression })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Box<dyn Expression>> {
        let mut left_expression: Box<dyn Expression> = match self.current_token.token_type {
            TokenType::Ident => Box::new(self.parse_identifier()),
            TokenType::Int => Box::new(self.parse_integer_literal()?),
            TokenType::Bang | TokenType::Minus => Box::new(self.parse_prefix_expression()?),
            TokenType::True | TokenType::False => Box::new(self.parse_boolean()?),
            TokenType::LParen => self.parse_grouped_expression()?,
            t => {
                let msg = format!("no prefix parse function for {:?} found", t);
                self.errors.push(msg);
                return Err(ParseError::Err(
                    "エラーにはしなくていいかも".into(),
                ));
            }
        };

        while !self.peek_token_is(TokenType::Semicolon) && precedence < self.peek_precedence() {
            match self.peek_token.token_type {
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Asterisk
                | TokenType::Eq
                | TokenType::NotEq
                | TokenType::Lt
                | TokenType::Gt => {}
                _ => return Ok(left_expression),
            };

            self.next_token();

            left_expression = Box::new(self.parse_infix_expression(left_expression)?);
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
            right,
        })
    }

    fn parse_infix_expression(&mut self, left: Box<dyn Expression>) -> Result<InfixExpression> {
        let token = self.current_token.clone();

        let precedence = self.current_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;
        let operator = token.literal.clone();

        Ok(InfixExpression {
            token,
            left,
            operator,
            right,
        })
    }

    fn parse_boolean(&mut self) -> Result<Boolean> {
        Ok(Boolean {
            token: self.current_token.clone(),
            value: self.current_token_is(TokenType::True),
        })
    }

    fn parse_grouped_expression(&mut self) -> Result<Box<dyn Expression>> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(TokenType::RParen) {
            return Err(ParseError::Err("invalid )".into()));
        }

        Ok(exp)
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

    fn peek_precedence(&self) -> Precedence {
        token_type_to_precedence(self.peek_token.token_type)
    }

    fn current_precedence(&self) -> Precedence {
        token_type_to_precedence(self.current_token.token_type)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Node;
    use crate::lexer::Lexer;
    use std::any::Any;

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

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        check_parse_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        let statement = program.statements[0].as_expression_ref().unwrap();
        test_identifier(&statement.expression, "foobar");
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        check_parse_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        let statement = program.statements[0].as_expression_ref().unwrap();
        test_integer_literal(&statement.expression, &5);
    }

    #[test]
    fn test_prefix_expressions() {
        fn test(input: &str, op: &str, right: &Any) {
            let mut parser = Parser::new(Lexer::new(input));
            let program = parser.parse_program().unwrap();
            check_parse_errors(&parser);

            assert_eq!(program.statements.len(), 1);

            let statement = program.statements[0].as_expression_ref().unwrap();
            test_prefix_expression(&statement.expression, op, right);
        }

        let i64_tests: Vec<(&str, &str, i64)> = vec![("!5;", "!", 5), ("-15;", "-", 15)];
        for t in i64_tests {
            test(t.0, t.1, &t.2);
        }

        let bool_tests: Vec<(&str, &str, bool)> =
            vec![("!true;", "!", true), ("!false;", "!", false)];
        for t in bool_tests {
            test(t.0, t.1, &t.2);
        }
    }

    #[test]
    fn test_infix_expressions() {
        fn test(input: &str, left: &Any, op: &str, right: &Any) {
            let mut parser = Parser::new(Lexer::new(input));
            let program = parser.parse_program().unwrap();
            check_parse_errors(&parser);

            assert_eq!(program.statements.len(), 1);

            let statement = program.statements[0].as_expression_ref().unwrap();
            test_infix_expression(&statement.expression, left, op, right);
        }

        let i64_tests: Vec<(&str, i64, &str, i64)> = vec![
            ("5 + 6;", 5, "+", 6),
            ("5 - 6;", 5, "-", 6),
            ("5 * 6;", 5, "*", 6),
            ("5 / 6;", 5, "/", 6),
            ("5 > 6;", 5, ">", 6),
            ("5 < 6;", 5, "<", 6),
            ("5 == 6;", 5, "==", 6),
            ("5 != 6;", 5, "!=", 6),
        ];
        for t in i64_tests {
            test(t.0, &t.1, t.2, &t.3);
        }

        let bool_tests: Vec<(&str, bool, &str, bool)> = vec![
            ("true == true", true, "==", true),
            ("true != false", true, "!=", false),
            ("false == false", false, "==", false),
        ];
        for t in bool_tests {
            test(t.0, &t.1, t.2, &t.3);
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests: Vec<(&str, &str)> = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
        ];

        for t in tests {
            let mut parser = Parser::new(Lexer::new(t.0));
            let program = parser.parse_program().unwrap();
            check_parse_errors(&parser);
            assert_eq!(format!("{}", program), t.1);
        }
    }

    #[test]
    fn test_boolean_expression() {
        let tests: Vec<(&str, bool)> = vec![("true;", true), ("false;", false)];

        for t in tests {
            let mut parser = Parser::new(Lexer::new(t.0));
            let program = parser.parse_program().unwrap();
            check_parse_errors(&parser);

            assert_eq!(program.statements.len(), 1);

            let statement = program.statements[0].as_expression_ref().unwrap();
            test_boolean_literal(&statement.expression, &t.1);
        }
    }

    fn test_identifier(expression: &Box<Expression>, expected: &str) {
        let ident = expression.as_identifier_ref().unwrap();
        assert_eq!(ident.value, expected);
        assert_eq!(ident.token_literal(), expected);
    }

    fn test_integer_literal(expression: &Box<Expression>, expected: &i64) {
        let integer = expression.as_integer_literal_ref().unwrap();
        assert_eq!(integer.value, *expected);
        assert_eq!(integer.token_literal(), format!("{}", expected));
    }

    fn test_boolean_literal(expression: &Box<Expression>, expected: &bool) {
        let boolean = expression.as_boolean_literal_ref().unwrap();
        assert_eq!(boolean.value, *expected);
        assert_eq!(boolean.token_literal(), format!("{}", expected));
    }

    fn test_literal_expression(expression: &Box<Expression>, expected: &dyn Any) {
        if let Some(expected) = expected.downcast_ref::<i64>() {
            test_integer_literal(expression, expected);
        } else if let Some(expected) = expected.downcast_ref::<&str>() {
            test_identifier(expression, expected);
        } else if let Some(expected) = expected.downcast_ref::<bool>() {
            test_boolean_literal(expression, expected);
        } else {
            panic!("invalid type: {:?}", expected);
        }
    }

    fn test_prefix_expression(expression: &Box<Expression>, operator: &str, right: &Any) {
        let oe = expression.as_prefix_ref().unwrap();
        assert_eq!(oe.operator, operator);
        test_literal_expression(&oe.right, right);
    }

    fn test_infix_expression(
        expression: &Box<Expression>,
        left: &Any,
        operator: &str,
        right: &Any,
    ) {
        let oe = expression.as_infix_ref().unwrap();
        test_literal_expression(&oe.left, left);
        assert_eq!(oe.operator, operator);
        test_literal_expression(&oe.right, right);
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
}
