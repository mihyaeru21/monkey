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
        Expression::BooleanLiteral(b) => Ok(Object::Boolean(b.value)),
        Expression::Prefix(e) => eval_prefix_expression(&e.operator, &eval_expression(&e.right)?),
        _ => Err(EvalError::Err(format!(
            "unexpected expression: {:?}",
            expression
        ))),
    }
}

fn eval_prefix_expression(operator: &str, right: &Object) -> Result<Object> {
    match operator {
        "!" => eval_bang_operator(right),
        _ => Err(EvalError::Err(format!(
            "invalid prefix operator: {:?}",
            operator
        ))),
    }
}

fn eval_bang_operator(right: &Object) -> Result<Object> {
    let res = match right {
        Object::Boolean(true) => false,
        Object::Boolean(false) => true,
        Object::Null => true,
        _ => false,
    };
    Ok(Object::Boolean(res))
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

    #[test]
    fn test_eval_boolean_expression() {
        let tests: Vec<(&str, bool)> = vec![("true", true), ("false", false)];
        for t in tests {
            let evaluated = test_eval(t.0);
            test_boolean_object(evaluated, t.1);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests: Vec<(&str, bool)> = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];
        for t in tests {
            let evaluated = test_eval(t.0);
            test_boolean_object(evaluated, t.1);
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

    fn test_boolean_object(object: Object, expected: bool) {
        let boolean = match object {
            Object::Boolean(b) => b,
            _ => panic!("object is not Boolean. got: {:?}", object),
        };
        assert_eq!(boolean, expected);
    }
}
