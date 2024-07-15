use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
    Error(String),
    Null,
}

impl Object {
    pub fn to_string(&self) -> String {
        match self {
            Object::Integer(integer) => integer.to_string(),
            Object::Boolean(boolean) => boolean.to_string(),
            Object::Return(return_object) => return_object.to_string(),
            Object::Error(error_message) => error_message.to_string(),
            Object::Null => "null".to_string(),
        }
    }

    pub fn object_type(&self) -> String {
        match self {
            Object::Integer(_) => "INTEGER".to_string(),
            Object::Boolean(_) => "BOOLEAN".to_string(),
            Object::Return(_) => "RETURN".to_string(),
            Object::Error(_) => "ERROR".to_string(),
            Object::Null => "NULL".to_string(),
        }
    }
}

#[derive(Debug)]
pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, key: &str) -> Option<&Object> {
        self.store.get(key)
    }

    pub fn set(&mut self, key: &str, object: Object) {
        self.store.insert(key.to_string(), object);
    }
}
