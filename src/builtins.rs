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

    builtins
}

fn len(args: Vec<Object>) -> Object {
    match args.len() {
        1 => match &args[0] {
            Object::String(string) => Object::Integer(string.len() as i64),
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
        println!("{}", arg);
    }
    Object::Void
}
