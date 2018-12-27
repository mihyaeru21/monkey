use crate::token::Token;
use std::fmt::Debug;

pub trait Node: Debug {
    fn token_literal(&self) -> String; // TODO: &strでいけそうなら後で変える
}

#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::Let(l) => l.token.literal.to_owned(),
            Statement::Return(r) => r.token.literal.to_owned(),
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct IdentifierExpression {
    pub token: Token,
    pub value: String,
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: IdentifierExpression,
    pub value: Expression,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
}
