use super::chunk::Chunk;

#[derive(Debug)]
pub enum FunctionType {
    Script,
    Function(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: String,
}

impl Function {
    pub fn new() -> Self {
        Function {
            arity: 0,
            chunk: Chunk::new(),
            name: String::new(),
        }
    }
}

impl Default for Function {
    fn default() -> Self {
        Function {
            arity: 0,
            chunk: Chunk::new(),
            name: String::new(),
        }
    }
}
