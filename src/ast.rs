use crate::token::Token;
use std::fmt::{self, Debug, Display, Formatter};

pub trait Node: Debug + Display {
    fn token_literal(&self) -> &str;
}

// Statement と Expression を enum にしら良い気がする

pub trait Statement: Node {
    fn as_let_ref(&self) -> Option<&LetStatement> {
        None
    }

    fn as_return_ref(&self) -> Option<&ReturnStatement> {
        None
    }

    fn as_expression_ref(&self) -> Option<&ExpressionStatement> {
        None
    }
}

pub trait Expression: Node {
    fn as_identifier_ref(&self) -> Option<&Identifier> {
        None
    }

    fn as_integer_literal_ref(&self) -> Option<&IntegerLiteral> {
        None
    }

    fn as_prefix_ref(&self) -> Option<&PrefixExpression> {
        None
    }

    fn as_infix_ref(&self) -> Option<&InfixExpression> {
        None
    }

    fn as_boolean_literal_ref(&self) -> Option<&Boolean> {
        None
    }
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            statements: Vec::new(),
        }
    }
}

impl Node for Program {
    fn token_literal(&self) -> &str {
        match self.statements.first() {
            Some(s) => s.token_literal(),
            None => "".into(),
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for statement in &self.statements {
            Display::fmt(statement, f)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Box<dyn Expression>,
}

impl Node for LetStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

impl Statement for LetStatement {
    fn as_let_ref(&self) -> Option<&LetStatement> {
        Some(self)
    }
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{} {} = {};",
            self.token.literal, self.name.value, self.value
        )
    }
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Box<dyn Expression>,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

impl Statement for ReturnStatement {
    fn as_return_ref(&self) -> Option<&ReturnStatement> {
        Some(self)
    }
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} {};", self.token.literal, self.return_value)
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token, // 式の最初のtoken
    pub expression: Box<dyn Expression>,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

impl Statement for ExpressionStatement {
    fn as_expression_ref(&self) -> Option<&ExpressionStatement> {
        Some(self)
    }
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(&self.expression, f)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

impl Expression for Identifier {
    fn as_identifier_ref(&self) -> Option<&Identifier> {
        Some(self)
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(&self.value, f)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

impl Expression for IntegerLiteral {
    fn as_integer_literal_ref(&self) -> Option<&IntegerLiteral> {
        Some(self)
    }
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(&self.value, f)
    }
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<dyn Expression>,
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

impl Expression for PrefixExpression {
    fn as_prefix_ref(&self) -> Option<&PrefixExpression> {
        Some(self)
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<dyn Expression>,
    pub operator: String,
    pub right: Box<dyn Expression>,
}

impl Node for InfixExpression {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

impl Expression for InfixExpression {
    fn as_infix_ref(&self) -> Option<&InfixExpression> {
        Some(self)
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

#[derive(Debug)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl Node for Boolean {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

impl Expression for Boolean {
    fn as_boolean_literal_ref(&self) -> Option<&Boolean> {
        Some(self)
    }
}

impl Display for Boolean {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(&self.value, f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::{Token, TokenType};

    #[test]
    fn test_display() {
        let program = Program {
            statements: vec![Box::new(LetStatement {
                token: Token {
                    token_type: TokenType::Let,
                    literal: "let".into(),
                },
                name: Identifier {
                    token: Token {
                        token_type: TokenType::Ident,
                        literal: "myVar".into(),
                    },
                    value: "myVar".into(),
                },
                value: Box::new(Identifier {
                    token: Token {
                        token_type: TokenType::Ident,
                        literal: "anotherVar".into(),
                    },
                    value: "anotherVar".into(),
                }),
            })],
        };

        assert_eq!(format!("{}", program), "let myVar = anotherVar;");
    }
}
