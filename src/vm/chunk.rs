use super::value::{Value, ValueArray};

#[derive(Debug, PartialEq)]
pub enum OpCode {
    Constant(u8),
}

#[derive(Debug)]
pub struct Chunk {
    code: Vec<OpCode>,
    constants: ValueArray,
    lines: Vec<(usize, usize)>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: vec![],
            constants: ValueArray::new(),
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
}
