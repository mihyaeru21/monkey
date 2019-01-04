use crate::token::Token;
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Block(BlockStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Statement::Let(s) => Display::fmt(s, f),
            Statement::Return(s) => Display::fmt(s, f),
            Statement::Expression(s) => Display::fmt(s, f),
            Statement::Block(s) => Display::fmt(s, f),
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    BooleanLiteral(Boolean),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    If(IfExpression),
    Function(FunctionLiteral),
    Call(CallExpression),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(e) => Display::fmt(e, f),
            Expression::IntegerLiteral(e) => Display::fmt(e, f),
            Expression::BooleanLiteral(e) => Display::fmt(e, f),
            Expression::Prefix(e) => Display::fmt(e, f),
            Expression::Infix(e) => Display::fmt(e, f),
            Expression::If(e) => Display::fmt(e, f),
            Expression::Function(e) => Display::fmt(e, f),
            Expression::Call(e) => Display::fmt(e, f),
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

#[derive(Debug)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for statement in &self.statements {
            Display::fmt(statement, f)?;
        }
        Ok(())
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
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

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(&self.value, f)
    }
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
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

impl Display for Boolean {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(&self.value, f)
    }
}

#[derive(Debug)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "if {} {}", self.condition, self.consequence)?;
        if let Some(alt) = &self.alternative {
            write!(f, "else {}", alt)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct FunctionLiteral {
    pub token: Token, // fn
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let params: Vec<String> = self.parameters.iter().map(|p| format!("{}", p)).collect();
        write!(
            f,
            "{}({}) {}",
            self.token.literal,
            params.join(", "),
            self.body
        )
    }
}

#[derive(Debug)]
pub struct CallExpression {
    pub token: Token, // (
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let args: Vec<String> = self.arguments.iter().map(|a| format!("{}", a)).collect();
        write!(f, "{}({})", self.function, args.join(", "))
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
                name: Identifier {
                    token: Token {
                        token_type: TokenType::Ident,
                        literal: "myVar".into(),
                    },
                    value: "myVar".into(),
                },
                value: Expression::Identifier(Identifier {
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
