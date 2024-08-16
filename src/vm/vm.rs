use super::chunk::{Chunk, OpCode, Value};

#[derive(Debug)]
pub struct VirtualMachine<'a> {
    chunk: &'a Chunk,
    pc: usize,
    pub stack: Vec<Value>,
}

#[derive(Debug, PartialEq)]
pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

impl<'a> VirtualMachine<'a> {
    pub fn new(chunk: &Chunk) -> VirtualMachine {
        VirtualMachine {
            chunk,
            pc: 0,
            stack: vec![],
        }
    }

    pub fn interpret(&mut self) -> InterpretResult {
        loop {
            // Gets next OpCode using current PC
            let instruction = self.chunk.get(self.pc);
            self.pc += 1; // Increases current PC for next instruction
            match instruction {
                Some(instruction) => match instruction {
                    OpCode::Constant(index) => {
                        if let Some(constant) = self.read_constant(*index).cloned() {
                            println!("Executing Constant with value: {}", constant);
                            self.stack.push(constant);
                        } else {
                            println!("Error: Constant not found.");
                            return InterpretResult::RuntimeError;
                        }
                    }
                    OpCode::Negate => {
                        if let Some(value) = self.stack.last_mut() {
                            *value = -*value;
                        } else {
                            return InterpretResult::RuntimeError;
                        }
                    }
                    OpCode::Add => match (self.stack.pop(), self.stack.pop()) {
                        (Some(first_value), Some(second_value)) => {
                            self.stack.push(first_value + second_value);
                        }
                        _ => return InterpretResult::RuntimeError,
                    },
                    OpCode::Subtract => match (self.stack.pop(), self.stack.pop()) {
                        (Some(first_value), Some(second_value)) => {
                            self.stack.push(first_value - second_value);
                        }
                        _ => return InterpretResult::RuntimeError,
                    },
                    OpCode::Multiply => match (self.stack.pop(), self.stack.pop()) {
                        (Some(first_value), Some(second_value)) => {
                            self.stack.push(first_value * second_value);
                        }
                        _ => return InterpretResult::RuntimeError,
                    },
                    OpCode::Divide => match (self.stack.pop(), self.stack.pop()) {
                        (Some(first_value), Some(second_value)) => {
                            if second_value == 0.0 {
                                return InterpretResult::RuntimeError;
                            }
                            self.stack.push(first_value / second_value);
                        }
                        _ => return InterpretResult::RuntimeError,
                    },
                    OpCode::Return => return InterpretResult::Ok,
                },
                None => break,
            }
        }

        InterpretResult::Ok
    }

    fn read_constant(&mut self, index: usize) -> Option<&Value> {
        let constant = self.chunk.constants.get(index);

        constant
    }
}
