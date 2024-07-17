use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::ast::ast::{BlockStatement, Identifier};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
    Error(String),
    Function(Function),
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
            Object::Function(function) => function.to_string(),
        }
    }

    pub fn object_type(&self) -> String {
        match self {
            Object::Integer(_) => "INTEGER".to_string(),
            Object::Boolean(_) => "BOOLEAN".to_string(),
            Object::Return(_) => "RETURN".to_string(),
            Object::Error(_) => "ERROR".to_string(),
            Object::Null => "NULL".to_string(),
            Object::Function(_) => "FUNCTION".to_string(),
        }
    }
}
#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
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
        self.store
            .get(key)
            .cloned()
            .or_else(|| self.outer.as_ref()?.borrow().get(key))
    }

    pub fn set(&mut self, key: &str, object: Object) {
        self.store.insert(key.to_string(), object);
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    environment: Environment,
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
