#[derive(Debug, PartialEq)]
pub enum OpCode {
    Constant(usize),
    Null,
    True,
    False,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Return,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Null,
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
