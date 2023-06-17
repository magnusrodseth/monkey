use std::collections::HashMap;

use crate::object::Object;

pub fn new_builtins() -> HashMap<String, Object> {
    let mut builtins = HashMap::new();

    builtins.insert("len".to_string(), Object::Builtin(len));
    builtins.insert("first".to_string(), Object::Builtin(first));
    builtins.insert("last".to_string(), Object::Builtin(last));
    builtins.insert("rest".to_string(), Object::Builtin(rest));
    builtins.insert("push".to_string(), Object::Builtin(push));
    builtins.insert("print".to_string(), Object::Builtin(print));
    builtins.insert("str".to_string(), Object::Builtin(str));

    builtins
}

fn len(args: Vec<Object>) -> Object {
    match args.len() {
        1 => match &args[0] {
            Object::String(string) => Object::Integer(string.len() as i64),
            Object::Array(array) => Object::Integer(array.len() as i64),
            Object::Hash(hash) => Object::Integer(hash.len() as i64),
            _ => Object::Error(format!("argument to `len` not supported, got {}", args[0])),
        },
        _ => Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )),
    }
}

fn first(args: Vec<Object>) -> Object {
    match args.len() {
        1 => match &args[0] {
            Object::Array(array) => match array.len() {
                0 => Object::Null,
                _ => array[0].clone(),
            },
            _ => Object::Error(format!(
                "argument to `first` must be ARRAY, got {}",
                args[0]
            )),
        },
        _ => Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )),
    }
}

fn last(args: Vec<Object>) -> Object {
    match args.len() {
        1 => match &args[0] {
            Object::Array(array) => match array.len() {
                0 => Object::Null,
                _ => array[array.len() - 1].clone(),
            },
            _ => Object::Error(format!("argument to `last` must be ARRAY, got {}", args[0])),
        },
        _ => Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )),
    }
}

fn rest(args: Vec<Object>) -> Object {
    match args.len() {
        1 => match &args[0] {
            Object::Array(array) => match array.len() {
                0 => Object::Null,
                _ => {
                    let mut new_array = Vec::new();
                    for i in 1..array.len() {
                        new_array.push(array[i].clone());
                    }
                    Object::Array(new_array)
                }
            },
            _ => Object::Error(format!("argument to `rest` must be ARRAY, got {}", args[0])),
        },
        _ => Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )),
    }
}

fn push(args: Vec<Object>) -> Object {
    match args.len() {
        2 => match &args[0] {
            Object::Array(array) => {
                let mut new_array = Vec::new();
                for i in 0..array.len() {
                    new_array.push(array[i].clone());
                }
                new_array.push(args[1].clone());
                Object::Array(new_array)
            }
            _ => Object::Error(format!("argument to `push` must be ARRAY, got {}", args[0])),
        },
        _ => Object::Error(format!(
            "wrong number of arguments. got={}, want=2",
            args.len()
        )),
    }
}

fn print(args: Vec<Object>) -> Object {
    for arg in args {
        print!("{}", arg);
    }
    println!();
    Object::Void
}

fn str(args: Vec<Object>) -> Object {
    match args.len() {
        1 => match &args[0] {
            Object::Integer(integer) => Object::String(integer.to_string()),
            Object::String(string) => Object::String(string.to_string()),
            Object::Boolean(boolean) => Object::String(boolean.to_string()),
            Object::Array(array) => {
                let mut string = String::from("[");
                for i in 0..array.len() {
                    string.push_str(&array[i].to_string());
                    if i != array.len() - 1 {
                        string.push_str(", ");
                    }
                }
                string.push_str("]");
                Object::String(string)
            }
            Object::Hash(hash) => {
                let mut string = String::from("{");
                let mut pairs = Vec::new();
                for (key, value) in hash {
                    pairs.push(format!("{}: {}", key, value));
                }
                string.push_str(&pairs.join(", "));
                string.push_str("}");
                Object::String(string)
            }
            Object::Null => Object::String("null".to_string()),
            Object::Void => Object::String("void".to_string()),
            _ => Object::Error(format!("argument to `str` not supported, got {}", args[0])),
        },
        _ => Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )),
    }
}
