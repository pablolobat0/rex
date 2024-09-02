use std::fmt;

#[derive(Debug, PartialEq)]
pub enum OpCode {
    Constant(usize),
    Null,
    True,
    False,
    Not,
    Equal,
    Greater,
    Less,
    LessEqual,
    GreaterEqual,
    NotEqual,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Return,
    Pop,
    DefineGlobal(usize),
    GetGlobal(usize),
    SetGlobal(usize),
    GetLocal(usize),
    SetLocal(usize),
    JumpIfFalse(usize),
    Jump(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(String),
    Null,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
            Value::Null => write!(f, "null"),
        }
    }
}

impl Value {
    pub fn is_number(&self) -> bool {
        match self {
            Value::Number(_) => true,
            _ => false,
        }
    }

    pub fn is_boolean(&self) -> bool {
        match self {
            Value::Boolean(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct Chunk {
    pub code: Vec<OpCode>,
    pub constants: Vec<Value>,
    lines: Vec<(usize, usize)>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: vec![],
            constants: vec![],
            lines: vec![],
        }
    }

    pub fn write(&mut self, byte: OpCode, line: usize) {
        self.code.push(byte);
        if let Some((last_line, last_count)) = self.lines.last_mut() {
            if *last_line == line {
                *last_count += 1; // Incrementar el run length
            } else {
                self.lines.push((line, 1)); // Nueva línea
            }
        } else {
            self.lines.push((line, 1)); // Primera entrada
        }
    }

    pub fn get(&self, index: usize) -> Option<&OpCode> {
        self.code.get(index)
    }

    pub fn get_line(&self, index: usize) -> Option<usize> {
        let mut acumulated = 0;

        for (line, count) in &self.lines {
            acumulated += count;

            if index < acumulated {
                return Some(*line);
            }
        }

        None
    }

    pub fn add_constant(&mut self, constant: Value) -> usize {
        self.constants.push(constant);
        self.constants.len() - 1
    }
    pub fn get_constant(&mut self, index: usize) -> Option<&Value> {
        self.constants.get(index)
    }
}

pub fn value_equal(a: Value, b: Value) -> bool {
    match (a, b) {
        (Value::Boolean(a_bool), Value::Boolean(b_bool)) => a_bool == b_bool,
        (Value::Number(a_number), Value::Number(b_number)) => a_number == b_number,
        (Value::String(a_string), Value::String(b_string)) => a_string == b_string,
        (Value::Null, Value::Null) => true,
        (_, _) => false,
    }
}
