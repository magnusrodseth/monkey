use std::collections::HashMap;

use crate::object::Object;

pub fn new_builtins() -> HashMap<String, Object> {
    let mut builtins = HashMap::new();

    builtins.insert("len".to_string(), Object::Builtin(builtin_len));

    builtins
}

fn builtin_len(args: Vec<Object>) -> Object {
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
