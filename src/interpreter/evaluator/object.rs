use std::{collections::HashMap, fmt::Display};

use crate::interpreter::parser::ast::{BlockStatement, Identifier};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Return(Box<Object>),
    Function(Function),
    Error(String),
    Null,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let object_str = match self {
            Object::Integer(integer) => integer.to_string(),
            Object::Float(float) => float.to_string(),
            Object::Boolean(boolean) => boolean.to_string(),
            Object::String(string) => string.to_string(),
            Object::Return(return_object) => return_object.to_string(),
            Object::Function(function) => function.to_string(),
            Object::Error(error_message) => error_message.to_string(),
            Object::Null => "null".to_string(),
        };

        write!(f, "{}", object_str)
    }
}

impl Object {
    pub fn object_type(&self) -> String {
        match self {
            Object::Integer(_) => "INTEGER".to_string(),
            Object::Float(_) => "FLOAT".to_string(),
            Object::Boolean(_) => "BOOLEAN".to_string(),
            Object::String(_) => "STRING".to_string(),
            Object::Return(_) => "RETURN".to_string(),
            Object::Function(_) => "FUNCTION".to_string(),
            Object::Error(_) => "ERROR".to_string(),
            Object::Null => "NULL".to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub environment: Environment,
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let parameters_string = self
            .parameters
            .iter()
            .map(|parameter| parameter.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        write!(f, "fn ({}) {{\n {}", parameters_string, self.body)
    }
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
}

// Store for identifier values
#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    inner: HashMap<String, Object>,
    outer: Option<Box<Environment>>, // Outer environment of the function
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            inner: HashMap::new(),
            outer: None,
        }
    }

    // Creates a new environment with an outer
    pub fn new_enclosed(outer: Environment) -> Environment {
        Environment {
            inner: HashMap::new(),
            outer: Some(Box::new(outer)),
        }
    }

    pub fn get(&self, key: &str) -> Option<Object> {
        let object = self.inner.get(key);

        // If the object doesn't exist in the inner we search in the outer
        if object.is_none() {
            match &self.outer {
                Some(outer) => return outer.get(key),
                None => return None,
            };
        }

        object.cloned()
    }

    pub fn set(&mut self, key: &str, object: Object) {
        self.inner.insert(key.to_string(), object);
    }
}
