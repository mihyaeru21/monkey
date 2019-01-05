#[derive(Debug)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(o) => format!("{}", o),
            Object::Boolean(o) => format!("{}", o),
            Object::Null => "null".into(),
        }
    }
}
