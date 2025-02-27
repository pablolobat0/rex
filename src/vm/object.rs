use super::chunk::{Chunk, Value};

#[derive(Debug, PartialEq, Clone)]
pub struct Closure {
    pub function: Function,
    pub upvalues: Vec<Value>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Upvalue {
    pub index: usize,
    pub is_local: bool,
}

#[derive(Debug, Clone)]
pub enum FunctionType {
    Script,
    Function(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: String,
    pub upvalues: Vec<Upvalue>,
}

impl Function {
    pub fn new() -> Self {
        Function {
            arity: 0,
            chunk: Chunk::new(),
            name: String::new(),
            upvalues: vec![],
        }
    }
}

impl Default for Function {
    fn default() -> Self {
        Function {
            arity: 0,
            chunk: Chunk::new(),
            name: String::new(),
            upvalues: vec![],
        }
    }
}
