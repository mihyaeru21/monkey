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

        Ok(program)
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
            value: Expression::Identifier(cloned_name), // TODO
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
            return_value: Expression::Identifier(Identifier {
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
            Some(self.parse_block_statement()?)
        } else {
            None
        };

        Ok(IfExpression {
            token,
            condition: Box::new(condition),
            consequence,
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

#[cfg(test)]
mod tests {
    use super::*;
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
            let let_statement = match statement {
                Statement::Let(s) => s,
                _ => panic!("invalid variant: {:?}", statement),
            };
            assert_eq!(let_statement.token.literal, "let");
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
            let return_statement = match statement {
                Statement::Return(s) => s,
                _ => panic!("invalid variant: {:?}", statement),
            };
            assert_eq!(return_statement.token.literal, "return");
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

        let statement = match &program.statements[0] {
            Statement::Expression(s) => s,
            _ => panic!("invalid variant: {:?}", program.statements[0]),
        };
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

        let statement = match &program.statements[0] {
            Statement::Expression(s) => s,
            _ => panic!("invalid variant: {:?}", program.statements[0]),
        };
        test_integer_literal(&statement.expression, &5);
    }

    #[test]
    fn test_prefix_expressions() {
        fn test(input: &str, op: &str, right: &Any) {
            let mut parser = Parser::new(Lexer::new(input));
            let program = parser.parse_program().unwrap();
            check_parse_errors(&parser);

            assert_eq!(program.statements.len(), 1);

            let statement = match &program.statements[0] {
                Statement::Expression(s) => s,
                _ => panic!("invalid variant: {:?}", program.statements[0]),
            };
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

            let statement = match &program.statements[0] {
                Statement::Expression(s) => s,
                _ => panic!("invalid variant: {:?}", program.statements[0]),
            };
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

            let statement = match &program.statements[0] {
                Statement::Expression(s) => s,
                _ => panic!("invalid variant: {:?}", program.statements[0]),
            };
            test_boolean_literal(&statement.expression, &t.1);
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program().unwrap();
        check_parse_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        let statement = match &program.statements[0] {
            Statement::Expression(s) => s,
            _ => panic!("invalid variant: {:?}", program.statements[0]),
        };

        let if_exp = match &statement.expression {
            Expression::If(e) => e,
            _ => panic!("invalid variant: {:?}", &statement.expression),
        };

        test_infix_expression(if_exp.condition.as_ref(), &"x", "<", &"y");
        assert_eq!(if_exp.consequence.statements.len(), 1);

        let consequence = match &if_exp.consequence.statements[0] {
            Statement::Expression(s) => s,
            _ => panic!("invalid variant: {:?}", if_exp.consequence.statements[0]),
        };
        test_identifier(&consequence.expression, "x");

        assert!(if_exp.alternative.is_none());
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program().unwrap();
        check_parse_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        let statement = match &program.statements[0] {
            Statement::Expression(s) => s,
            _ => panic!("invalid variant: {:?}", program.statements[0]),
        };

        let if_exp = match &statement.expression {
            Expression::If(e) => e,
            _ => panic!("invalid variant: {:?}", &statement.expression),
        };

        test_infix_expression(if_exp.condition.as_ref(), &"x", "<", &"y");
        assert_eq!(if_exp.consequence.statements.len(), 1);

        let consequence = match &if_exp.consequence.statements[0] {
            Statement::Expression(s) => s,
            _ => panic!("invalid variant: {:?}", if_exp.consequence.statements[0]),
        };
        test_identifier(&consequence.expression, "x");

        let alt = if_exp.alternative.as_ref().unwrap();
        let alternative = match &alt.statements[0] {
            Statement::Expression(s) => s,
            _ => panic!("invalid variant: {:?}", alt.statements[0]),
        };
        test_identifier(&alternative.expression, "y");
    }

    #[test]
    fn test_function_literal() {
        let input = "fn(x, y) { x + y; }";

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program().unwrap();
        check_parse_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        let statement = match &program.statements[0] {
            Statement::Expression(s) => s,
            _ => panic!("invalid variant: {:?}", program.statements[0]),
        };

        let func_exp = match &statement.expression {
            Expression::Function(e) => e,
            _ => panic!("invalid variant: {:?}", &statement.expression),
        };

        assert_eq!(func_exp.parameters.len(), 2);
        test_literal_expression(
            &Expression::Identifier(func_exp.parameters[0].clone()),
            &"x",
        );
        test_literal_expression(
            &Expression::Identifier(func_exp.parameters[1].clone()),
            &"y",
        );

        assert_eq!(func_exp.body.statements.len(), 1);
        let body = match &func_exp.body.statements[0] {
            Statement::Expression(s) => s,
            _ => panic!("invalid variant: {:?}", func_exp.body.statements[0]),
        };
        test_infix_expression(&body.expression, &"x", "+", &"y");
    }

    #[test]
    fn test_function_parameter_parsing() {
        let tests: Vec<(&str, Vec<&str>)> = vec![
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x"]),
            ("fn(x, y, z) {};", vec!["x", "y", "z"]),
        ];

        for t in tests {
            let mut parser = Parser::new(Lexer::new(t.0));
            let program = parser.parse_program().unwrap();
            check_parse_errors(&parser);

            let statement = match &program.statements[0] {
                Statement::Expression(s) => s,
                _ => panic!("invalid variant: {:?}", program.statements[0]),
            };
            let func_exp = match &statement.expression {
                Expression::Function(e) => e,
                _ => panic!("invalid variant: {:?}", &statement.expression),
            };

            assert_eq!(func_exp.parameters.len(), t.1.len());
            for (i, expect) in t.1.iter().enumerate() {
                let exp = Expression::Identifier(func_exp.parameters[i].clone());
                test_literal_expression(&exp, expect);
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program().unwrap();
        check_parse_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        let statement = match &program.statements[0] {
            Statement::Expression(s) => s,
            _ => panic!("invalid variant: {:?}", program.statements[0]),
        };
        let call_exp = match &statement.expression {
            Expression::Call(e) => e,
            _ => panic!("invalid variant: {:?}", &statement.expression),
        };

        test_identifier(&call_exp.function, "add");
        assert_eq!(call_exp.arguments.len(), 3);
        test_literal_expression(&call_exp.arguments[0], &1);
        test_infix_expression(&call_exp.arguments[1], &2, "*", &3);
        test_infix_expression(&call_exp.arguments[2], &4, "+", &5);
    }

    fn test_identifier(expression: &Expression, expected: &str) {
        let ident = match expression {
            Expression::Identifier(e) => e,
            _ => panic!("invalid variant: {:?}", expression),
        };
        assert_eq!(ident.value, expected);
        assert_eq!(ident.token.literal, expected);
    }

    fn test_integer_literal(expression: &Expression, expected: &i64) {
        let integer = match expression {
            Expression::IntegerLiteral(e) => e,
            _ => panic!("invalid variant: {:?}", expression),
        };
        assert_eq!(integer.value, *expected);
        assert_eq!(integer.token.literal, format!("{}", expected));
    }

    fn test_boolean_literal(expression: &Expression, expected: &bool) {
        let boolean = match expression {
            Expression::BooleanLiteral(e) => e,
            _ => panic!("invalid variant: {:?}", expression),
        };
        assert_eq!(boolean.value, *expected);
        assert_eq!(boolean.token.literal, format!("{}", expected));
    }

    fn test_literal_expression(expression: &Expression, expected: &dyn Any) {
        if let Some(expected) = expected.downcast_ref::<i64>() {
            test_integer_literal(expression, expected);
        } else if let Some(expected) = expected.downcast_ref::<i32>() {
            test_integer_literal(expression, &(*expected as i64));
        } else if let Some(expected) = expected.downcast_ref::<&str>() {
            test_identifier(expression, expected);
        } else if let Some(expected) = expected.downcast_ref::<bool>() {
            test_boolean_literal(expression, expected);
        } else {
            panic!("invalid type: {:?}", expected);
        }
    }

    fn test_prefix_expression(expression: &Expression, operator: &str, right: &Any) {
        let oe = match expression {
            Expression::Prefix(e) => e,
            _ => panic!("invalid variant: {:?}", expression),
        };
        assert_eq!(oe.operator, operator);
        test_literal_expression(&oe.right, right);
    }

    fn test_infix_expression(expression: &Expression, left: &Any, operator: &str, right: &Any) {
        let oe = match expression {
            Expression::Infix(e) => e,
            _ => panic!("invalid variant: {:?}", expression),
        };
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
