#[cfg(test)]
mod tests;

use crate::ast::{BlockStatement, Expression, Identifier, IfExpression, Program, Statement};
use crate::object::FuncObject;
use crate::object::{Environment, Object};
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
use std::result;

#[derive(Debug, Eq, PartialEq)]
pub enum EvalError {
    TypeMismatch(String),
    UnknownOperator(String),
    IdentifierNotFound(String),
    NotAFunction(String),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EvalError::TypeMismatch(s) => write!(f, "type mismatch: {}", s),
            EvalError::UnknownOperator(s) => write!(f, "unknown operator: {}", s),
            EvalError::IdentifierNotFound(s) => write!(f, "identifier not found: {}", s),
            EvalError::NotAFunction(s) => write!(f, "not a function: {}", s),
        }
    }
}

pub type Result<T> = result::Result<T, EvalError>;

type Env = Rc<RefCell<Environment>>;

pub fn eval(program: &Program, env: &Env) -> Result<Rc<Object>> {
    let mut obj = Rc::new(Object::Null);

    for statement in &program.statements {
        obj = eval_statement(statement, env)?;

        match obj.as_ref() {
            Object::ReturnValue(o) => return Ok(o.clone()),
            _ => {}
        }
    }

    Ok(obj)
}

fn eval_statement(statement: &Statement, env: &Env) -> Result<Rc<Object>> {
    match statement {
        Statement::Expression(s) => eval_expression(&s.expression, env),
        Statement::Block(s) => eval_block_statement(&s, env),
        Statement::Return(s) => {
            let res = eval_expression(&s.return_value, env)?;
            Ok(Rc::new(Object::ReturnValue(res)))
        }
        Statement::Let(s) => {
            let res = eval_expression(&s.value, env)?;
            Ok(env.borrow_mut().set(&s.name.value, res))
        }
    }
}

fn eval_block_statement(block: &BlockStatement, env: &Env) -> Result<Rc<Object>> {
    let mut obj = Rc::new(Object::Null);

    for statement in &block.statements {
        obj = eval_statement(statement, env)?;

        match obj.as_ref() {
            Object::ReturnValue(_) => return Ok(obj),
            _ => {}
        }
    }

    Ok(obj)
}

fn eval_expression(expression: &Expression, env: &Env) -> Result<Rc<Object>> {
    match expression {
        Expression::IntegerLiteral(i) => Ok(Rc::new(Object::Integer(i.value))),
        Expression::BooleanLiteral(b) => Ok(Rc::new(Object::Boolean(b.value))),
        Expression::Prefix(e) => {
            eval_prefix_expression(&e.operator, eval_expression(&e.right, env)?)
        }
        Expression::Infix(e) => eval_infix_expression(
            eval_expression(&e.left, env)?,
            &e.operator,
            eval_expression(&e.right, env)?,
        ),
        Expression::If(e) => eval_if_expression(e, env),
        Expression::Identifier(e) => eval_identifier(e, env),
        Expression::Function(e) => Ok(Rc::new(Object::Function(FuncObject {
            parameters: e.parameters.clone(),
            body: e.body.clone(),
            env: env.clone(),
        }))),
        Expression::Call(e) => {
            let func = eval_expression(&e.function, env)?;
            let mut args = Vec::new();
            for arg in &e.arguments {
                args.push(eval_expression(arg, env)?);
            }
            apply_function(&func, &args)
        }
    }
}

fn eval_prefix_expression(operator: &str, right: Rc<Object>) -> Result<Rc<Object>> {
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

fn eval_bang_operator(right: Rc<Object>) -> Result<Rc<Object>> {
    let res = match right.as_ref() {
        Object::Boolean(true) => false,
        Object::Boolean(false) => true,
        Object::Null => true,
        _ => false,
    };
    Ok(Rc::new(Object::Boolean(res)))
}

fn eval_minus_operator(right: Rc<Object>) -> Result<Rc<Object>> {
    match right.as_ref() {
        Object::Integer(i) => Ok(Rc::new(Object::Integer(-(*i)))),
        _ => Err(EvalError::UnknownOperator(format!(
            "-{}",
            right.object_type()
        ))),
    }
}

fn eval_infix_expression(
    left: Rc<Object>,
    operator: &str,
    right: Rc<Object>,
) -> Result<Rc<Object>> {
    match (left.as_ref(), right.as_ref()) {
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

fn eval_integer_infix_expression(left: i64, operator: &str, right: i64) -> Result<Rc<Object>> {
    let res = match operator {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => Object::Boolean(left < right),
        ">" => Object::Boolean(left > right),
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => {
            return Err(EvalError::UnknownOperator(format!(
                "INTEGER {} INTEGER",
                operator
            )));
        }
    };
    Ok(Rc::new(res))
}

fn eval_boolean_infix_expression(left: bool, operator: &str, right: bool) -> Result<Rc<Object>> {
    let res = match operator {
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => {
            return Err(EvalError::UnknownOperator(format!(
                "BOOLEAN {} BOOLEAN",
                operator
            )));
        }
    };
    Ok(Rc::new(res))
}

fn eval_if_expression(if_exp: &IfExpression, env: &Env) -> Result<Rc<Object>> {
    let condition = eval_expression(&if_exp.condition, env)?;

    if is_truthy(&condition) {
        Ok(eval_statement(&if_exp.consequence, env)?)
    } else if let Some(alternative) = &if_exp.alternative {
        Ok(eval_statement(alternative.as_ref(), env)?)
    } else {
        Ok(Rc::new(Object::Null))
    }
}

fn eval_identifier(identifier: &Identifier, env: &Env) -> Result<Rc<Object>> {
    match env.borrow().get(&identifier.value) {
        Some(o) => Ok(o),
        _ => Err(EvalError::IdentifierNotFound(identifier.value.clone())),
    }
}

fn apply_function(func: &Object, args: &Vec<Rc<Object>>) -> Result<Rc<Object>> {
    let function = match func {
        Object::Function(f) => f,
        _ => {
            return Err(EvalError::NotAFunction(func.object_type().to_owned()));
        }
    };

    let mut extended_env = Environment::new_enclosed(&function.env);
    for (param, arg) in function.parameters.iter().zip(args) {
        extended_env.set(&param.value, arg.clone());
    }

    let res = eval_block_statement(&function.body, &Rc::new(RefCell::new(extended_env)))?;

    match res.as_ref() {
        Object::ReturnValue(o) => Ok(o.clone()),
        _ => Ok(res),
    }
}

fn is_truthy(object: &Object) -> bool {
    match object {
        Object::Null | Object::Boolean(false) => false,
        _ => true,
    }
}
