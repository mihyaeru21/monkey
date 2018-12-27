use crate::token::Token;
use std::fmt::{self, Debug, Display, Formatter};

pub trait Node: Debug + Display {
    fn token_literal(&self) -> String; // TODO: &strでいけそうなら後で変える
}

#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::Let(l) => l.token.literal.to_owned(),
            Statement::Return(r) => r.token.literal.to_owned(),
            Statement::Expression(e) => e.token.literal.to_owned(),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Statement::Let(l) => Display::fmt(l, f),
            Statement::Return(r) => Display::fmt(r, f),
            Statement::Expression(e) => Display::fmt(e, f),
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Identifier(IdentifierExpression),
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Identifier(i) => i.token.literal.to_owned(),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(i) => Display::fmt(i, f),
        }
    }
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            statements: Vec::new(),
        }
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct IdentifierExpression {
    pub token: Token,
    pub value: String,
}

impl Display for IdentifierExpression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(&self.value, f)
    }
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: IdentifierExpression,
    pub value: Expression,
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
    pub return_value: Expression,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} {};", self.token.literal, self.return_value)
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token, // 式の最初のtoken
    pub expression: Expression,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(&self.expression, f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::{Token, TokenType};

    #[test]
    fn test_display() {
        let program = Program {
            statements: vec![Statement::Let(LetStatement {
                token: Token {
                    token_type: TokenType::Let,
                    literal: "let".into(),
                },
                name: IdentifierExpression {
                    token: Token {
                        token_type: TokenType::Ident,
                        literal: "myVar".into(),
                    },
                    value: "myVar".into(),
                },
                value: Expression::Identifier(IdentifierExpression {
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
