use std::{cell::RefCell, fmt::Display, rc::Rc};

use crate::{
    ast::{BlockStatement, Identifier},
    environment::Environment,
};

#[derive(PartialEq, Clone, Eq, Debug)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
    Error(String),
    Function {
        parameters: Vec<Identifier>,
        body: BlockStatement,
        environment: Rc<RefCell<Environment>>,
    },
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
            Object::Function {
                parameters,
                body,
                environment: _,
            } => write!(
                f,
                "fn({}) {{\n{}\n}}",
                parameters
                    .iter()
                    .map(|parameter| parameter.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                body
            ),
        }
    }
}
