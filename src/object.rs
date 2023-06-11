use std::fmt::Display;

#[derive(PartialEq, Clone, Eq, Debug)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
    Error(String),
    Null,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(integer) => write!(f, "{}", integer),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::Return(return_value) => write!(f, "{}", return_value),
            Object::Error(error) => write!(f, "ERROR: {}", error),
            Object::Null => write!(f, "null"),
        }
    }
}
