use std::{cell::RefCell, collections::HashMap, fmt::Display, hash::Hash, rc::Rc};

use crate::{
    ast::{BlockStatement, Identifier},
    environment::Environment,
};

type BuiltinFunction = fn(Vec<Object>) -> Object;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(String),
    Array(Vec<Object>),
    Hash(HashMap<Object, Object>),
    Return(Box<Object>),
    Error(String),
    Function {
        parameters: Vec<Identifier>,
        body: BlockStatement,
        environment: Rc<RefCell<Environment>>,
    },
    Range {
        start: i64,
        end: i64,
        step: i64,
    },
    Builtin(BuiltinFunction),
    Null,
    Break,
    Continue,
    Void,
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
            Object::String(string) => write!(f, "{}", string),
            Object::Builtin(function) => write!(f, "<builtin function>"),
            Object::Array(array) => {
                let elements = array
                    .iter()
                    .map(|element| element.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "[{}]", elements)
            }
            Object::Hash(hash) => {
                let mut result = String::new();
                for (i, (key, value)) in hash.iter().enumerate() {
                    if i < 1 {
                        result.push_str(&format!("{}: {}", key, value));
                    } else {
                        result.push_str(&format!(", {}: {}", key, value));
                    }
                }
                write!(f, "{{{}}}", result)
            }
            Object::Void => write!(f, ""),
            Object::Break => write!(f, ""),
            Object::Continue => write!(f, ""),
            Object::Range { start, end, step } => {
                write!(f, "range({}, {}, {})", start, end, step)
            }
        }
    }
}

impl Hash for Object {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Object::Integer(integer) => integer.hash(state),
            Object::Boolean(boolean) => boolean.hash(state),
            Object::String(string) => string.hash(state),
            _ => "".hash(state),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::hash_map::DefaultHasher;

    #[test]
    fn string_hash_keys() {
        let mut hasher = DefaultHasher::new();

        let hello1 = Object::String("Hello World".to_string());
        let hello2 = Object::String("Hello World".to_string());
        let different1 = Object::String("My name is johnny".to_string());
        let different2 = Object::String("My name is johnny".to_string());

        assert_eq!(hello1.hash(&mut hasher), hello2.hash(&mut hasher));
        assert_eq!(different1.hash(&mut hasher), different2.hash(&mut hasher));
    }
}
