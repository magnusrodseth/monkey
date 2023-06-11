use std::collections::HashMap;

use crate::object::Object;

pub struct Environment {
    values: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
        }
    }

    pub fn get(&self, name: &String) -> Option<&Object> {
        self.values.get(name)
    }

    pub fn set(&mut self, name: String, value: Object) {
        self.values.insert(name.to_string(), value);
    }
}
