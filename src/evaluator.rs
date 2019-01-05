use crate::ast::{Expression, IfExpression, Program, Statement};
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
    let mut obj = Object::Null;
    for statement in statements {
        obj = eval_statement(statement)?;
        match obj {
            Object::ReturnValue(o) => return Ok(*o),
            _ => {}
        }
    }
    Ok(obj)
}

fn eval_statement(statement: &Statement) -> Result<Object> {
    match statement {
        Statement::Expression(s) => eval_expression(&s.expression),
        Statement::Block(s) => eval_statements(&s.statements),
        Statement::Return(s) => {
            let res = eval_expression(&s.return_value)?;
            Ok(Object::ReturnValue(Box::new(res)))
        }
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
        Expression::Infix(e) => eval_infix_expression(
            &eval_expression(&e.left)?,
            &e.operator,
            &eval_expression(&e.right)?,
        ),
        Expression::If(e) => eval_if_expression(e),
        _ => Err(EvalError::Err(format!(
            "unexpected expression: {:?}",
            expression
        ))),
    }
}

fn eval_prefix_expression(operator: &str, right: &Object) -> Result<Object> {
    match operator {
        "!" => eval_bang_operator(right),
        "-" => eval_minus_operator(right),
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

fn eval_minus_operator(right: &Object) -> Result<Object> {
    match right {
        Object::Integer(i) => Ok(Object::Integer(-(*i))),
        _ => Err(EvalError::Err(format!(
            "integer object is expected: {:?}",
            right
        ))),
    }
}

fn eval_infix_expression(left: &Object, operator: &str, right: &Object) -> Result<Object> {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => eval_integer_infix_expression(*l, operator, *r),
        (Object::Boolean(l), Object::Boolean(r)) => eval_boolean_infix_expression(*l, operator, *r),
        _ => Err(EvalError::Err(format!(
            "invalid operand. left: {:?}, right: {:?}",
            left, right
        ))),
    }
}

fn eval_integer_infix_expression(left: i64, operator: &str, right: i64) -> Result<Object> {
    match operator {
        "+" => Ok(Object::Integer(left + right)),
        "-" => Ok(Object::Integer(left - right)),
        "*" => Ok(Object::Integer(left * right)),
        "/" => Ok(Object::Integer(left / right)),
        "<" => Ok(Object::Boolean(left < right)),
        ">" => Ok(Object::Boolean(left > right)),
        "==" => Ok(Object::Boolean(left == right)),
        "!=" => Ok(Object::Boolean(left != right)),
        _ => Err(EvalError::Err(format!(
            "invalid integer infix operator: {:?}",
            operator
        ))),
    }
}

fn eval_boolean_infix_expression(left: bool, operator: &str, right: bool) -> Result<Object> {
    match operator {
        "==" => Ok(Object::Boolean(left == right)),
        "!=" => Ok(Object::Boolean(left != right)),
        _ => Err(EvalError::Err(format!(
            "invalid boolean infix operator: {:?}",
            operator
        ))),
    }
}

fn eval_if_expression(if_exp: &IfExpression) -> Result<Object> {
    let condition = eval_expression(&if_exp.condition)?;

    if is_truthy(&condition) {
        Ok(eval_statement(&if_exp.consequence)?)
    } else if let Some(alternative) = &if_exp.alternative {
        Ok(eval_statement(alternative.as_ref())?)
    } else {
        Ok(Object::Null)
    }
}

fn is_truthy(object: &Object) -> bool {
    match object {
        Object::Null | Object::Boolean(false) => false,
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_eval_integer_expression() {
        let tests: Vec<(&str, i64)> = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];
        for t in tests {
            let evaluated = test_eval(t.0);
            test_integer_object(&evaluated, t.1);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests: Vec<(&str, bool)> = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];
        for t in tests {
            let evaluated = test_eval(t.0);
            test_boolean_object(&evaluated, t.1);
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
            test_boolean_object(&evaluated, t.1);
        }
    }

    #[test]
    fn test_if_expressions() {
        let int_tests: Vec<(&str, i64)> = vec![
            ("if (true) { 10 }", 10),
            ("if (1) { 10 }", 10),
            ("if (1 < 2) { 10 }", 10),
            ("if (1 > 2) { 10 } else { 20 }", 20),
            ("if (1 < 2) { 10 } else { 20 }", 10),
        ];
        for t in int_tests {
            let evaluated = test_eval(t.0);
            test_integer_object(&evaluated, t.1);
        }

        let null_tests: Vec<&str> = vec!["if (false) { 10 }", "if (1 > 2) { 10 }"];
        for t in null_tests {
            let evaluated = test_eval(t);
            test_null_object(&evaluated);
        }
    }

    #[test]
    fn test_return_statements() {
        let tests: Vec<(&str, i64)> = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
        ];
        for t in tests {
            let evaluated = test_eval(t.0);
            test_integer_object(&evaluated, t.1);
        }
    }

    fn test_eval(input: &str) -> Object {
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program().unwrap();
        eval(&program).unwrap()
    }

    fn test_integer_object(object: &Object, expected: i64) {
        let int = match object {
            Object::Integer(i) => i,
            _ => panic!("object is not Integer. got: {:?}", object),
        };
        assert_eq!(*int, expected);
    }

    fn test_boolean_object(object: &Object, expected: bool) {
        let boolean = match object {
            Object::Boolean(b) => b,
            _ => panic!("object is not Boolean. got: {:?}", object),
        };
        assert_eq!(*boolean, expected);
    }

    fn test_null_object(object: &Object) {
        match object {
            Object::Null => {}
            _ => panic!("object is not null. got: {:?}", object),
        };
    }
}
