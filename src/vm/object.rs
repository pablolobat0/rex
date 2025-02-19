use super::chunk::Chunk;

#[derive(Debug)]
pub enum FunctionType {
    Script,
    Function,
}

#[derive(Debug)]
pub struct Function {
    arity: u32,
    pub chunk: Chunk,
    name: String,
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
