use crate::ast::{Expression, Program, Statement};
use crate::object::Object;
use std::result;

#[derive(Debug, Eq, PartialEq)]
pub enum EvalError {
    Err(String),
}

pub type Result<T> = result::Result<T, EvalError>;

pub fn eval(program: &Program) -> Result<Object> {
    eval_statements(&program.statements)
}

fn eval_statements(statements: &Vec<Statement>) -> Result<Object> {
    let mut obj: Result<Object> = Err(EvalError::Err("todo".into())); // TODO: 最初はNULLが入りそう
    for statement in statements {
        obj = Ok(eval_statement(statement)?);
    }
    obj
}

fn eval_statement(statement: &Statement) -> Result<Object> {
    match statement {
        Statement::Expression(s) => eval_expression(&s.expression),
        _ => Err(EvalError::Err(format!(
            "unexpected statement: {:?}",
            statement
        ))),
    }
}

fn eval_expression(expression: &Expression) -> Result<Object> {
    match expression {
        Expression::IntegerLiteral(i) => Ok(Object::Integer(i.value)),
        _ => Err(EvalError::Err(format!(
            "unexpected expression: {:?}",
            expression
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_eval_integer_expression() {
        let tests: Vec<(&str, i64)> = vec![("5", 5), ("10", 10)];
        for t in tests {
            let evaluated = test_eval(t.0);
            test_integer_object(evaluated, t.1);
        }
    }

    fn test_eval(input: &str) -> Object {
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program().unwrap();
        eval(&program).unwrap()
    }

    fn test_integer_object(object: Object, expected: i64) {
        let int = match object {
            Object::Integer(i) => i,
            _ => panic!("object is not Integer. got: {:?}", object),
        };
        assert_eq!(int, expected);
    }
}
