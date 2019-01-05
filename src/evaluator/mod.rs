mod tests;

use crate::ast::BlockStatement;
use crate::ast::{Expression, IfExpression, Program, Statement};
use crate::object::Object;
use std::fmt;
use std::result;

#[derive(Debug, Eq, PartialEq)]
pub enum EvalError {
    TypeMismatch(String),
    UnknownOperator(String),
    NotImplemented(String),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EvalError::TypeMismatch(s) => write!(f, "type mismatch: {}", s),
            EvalError::UnknownOperator(s) => write!(f, "unknown operator: {}", s),
            EvalError::NotImplemented(s) => write!(f, "not implemented: {}", s),
        }
    }
}

pub type Result<T> = result::Result<T, EvalError>;

pub fn eval(program: &Program) -> Result<Object> {
    let mut obj = Object::Null;

    for statement in &program.statements {
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
        Statement::Block(s) => eval_block_statement(&s),
        Statement::Return(s) => {
            let res = eval_expression(&s.return_value)?;
            Ok(Object::ReturnValue(Box::new(res)))
        }
        _ => Err(EvalError::NotImplemented(format!("{:?}", statement))),
    }
}

fn eval_block_statement(block: &BlockStatement) -> Result<Object> {
    let mut obj = Object::Null;

    for statement in &block.statements {
        obj = eval_statement(statement)?;

        match obj {
            Object::ReturnValue(_) => return Ok(obj),
            _ => {}
        }
    }

    Ok(obj)
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
        _ => Err(EvalError::NotImplemented(format!("{:?}", expression))),
    }
}

fn eval_prefix_expression(operator: &str, right: &Object) -> Result<Object> {
    match operator {
        "!" => eval_bang_operator(right),
        "-" => eval_minus_operator(right),
        _ => Err(EvalError::UnknownOperator(format!(
            "{}{}",
            operator,
            right.object_type()
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
        _ => Err(EvalError::UnknownOperator(format!(
            "-{}",
            right.object_type()
        ))),
    }
}

fn eval_infix_expression(left: &Object, operator: &str, right: &Object) -> Result<Object> {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => eval_integer_infix_expression(*l, operator, *r),
        (Object::Boolean(l), Object::Boolean(r)) => eval_boolean_infix_expression(*l, operator, *r),
        _ => Err(EvalError::TypeMismatch(format!(
            "{} {} {}",
            left.object_type(),
            operator,
            right.object_type()
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
        _ => Err(EvalError::UnknownOperator(format!(
            "INTEGER {} INTEGER",
            operator
        ))),
    }
}

fn eval_boolean_infix_expression(left: bool, operator: &str, right: bool) -> Result<Object> {
    match operator {
        "==" => Ok(Object::Boolean(left == right)),
        "!=" => Ok(Object::Boolean(left != right)),
        _ => Err(EvalError::UnknownOperator(format!(
            "BOOLEAN {} BOOLEAN",
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
