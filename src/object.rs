#[derive(Debug, Eq, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(o) => format!("{}", o),
            Object::Boolean(o) => format!("{}", o),
            Object::Null => "null".into(),
            Object::ReturnValue(o) => o.inspect(),
        }
    }

    pub fn object_type(&self) -> &str {
        match self {
            Object::Integer(_) => "INTEGER",
            Object::Boolean(_) => "BOOLEAN",
            Object::Null => "NULL",
            Object::ReturnValue(_) => "RETURN_VALUE",
        }
    }
}
