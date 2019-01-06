mod environment;

pub use self::environment::Environment;
use crate::ast::{BlockStatement, Identifier};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    ReturnValue(Rc<Object>),
    Function(FuncObject),
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(o) => format!("{}", o),
            Object::Boolean(o) => format!("{}", o),
            Object::Null => "null".into(),
            Object::ReturnValue(o) => o.inspect(),
            Object::Function(f) => f.inspect(),
        }
    }

    pub fn object_type(&self) -> &str {
        match self {
            Object::Integer(_) => "INTEGER",
            Object::Boolean(_) => "BOOLEAN",
            Object::Null => "NULL",
            Object::ReturnValue(_) => "RETURN_VALUE",
            Object::Function(_) => "FUNCTION",
        }
    }
}

#[derive(Debug)]
pub struct FuncObject {
    pub parameters: Vec<Identifier>,
    pub body: Rc<BlockStatement>,
    pub env: Rc<RefCell<Environment>>,
}

impl FuncObject {
    pub fn inspect(&self) -> String {
        let params: Vec<String> = self.parameters.iter().map(|p| p.value.to_owned()).collect();
        format!("fn({}) {{\n{}\n}}", params.join(", "), self.body)
    }
}
