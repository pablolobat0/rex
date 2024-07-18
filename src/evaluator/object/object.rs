use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::ast::ast::{BlockStatement, Identifier};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(String),
    Return(Box<Object>),
    Function(Function),
    Error(String),
    Null,
}

impl Object {
    pub fn to_string(&self) -> String {
        match self {
            Object::Integer(integer) => integer.to_string(),
            Object::Boolean(boolean) => boolean.to_string(),
            Object::String(string) => string.to_string(),
            Object::Return(return_object) => return_object.to_string(),
            Object::Function(function) => function.to_string(),
            Object::Error(error_message) => error_message.to_string(),
            Object::Null => "null".to_string(),
        }
    }

    pub fn object_type(&self) -> String {
        match self {
            Object::Integer(_) => "INTEGER".to_string(),
            Object::Boolean(_) => "BOOLEAN".to_string(),
            Object::String(_) => "STRING".to_string(),
            Object::Return(_) => "RETURN".to_string(),
            Object::Function(_) => "FUNCTION".to_string(),
            Object::Error(_) => "ERROR".to_string(),
            Object::Null => "NULL".to_string(),
        }
    }
}

// Store for identifier values
#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>, // Outer environment of the function
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Environment {
        Environment {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn get(&self, key: &str) -> Option<Object> {
        let object = self.store.get(key);

        // If the object doesn't exist in the store we search in the outer
        if !object.is_some() {
            match &self.outer {
                Some(outer) => return outer.as_ref().borrow().get(key),
                None => return object.cloned(),
            };
        }

        return object.cloned();
    }

    pub fn set(&mut self, key: &str, object: Object) {
        self.store.insert(key.to_string(), object);
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub environment: Environment,
}

impl Function {
    pub fn new(
        parameters: Vec<Identifier>,
        body: BlockStatement,
        environment: Environment,
    ) -> Function {
        Function {
            parameters,
            body,
            environment,
        }
    }

    pub fn to_string(&self) -> String {
        let parameters_string = self
            .parameters
            .iter()
            .map(|parameter| parameter.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        format!("fn ({}) {{\n {}", parameters_string, self.body.to_string())
    }
}
