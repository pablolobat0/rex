use super::{
    chunk::{OpCode, Value},
    compiler::Parser,
};

#[derive(Debug)]
pub struct VirtualMachine<'a> {
    pc: usize,
    pub stack: Vec<Value>,
    parser: &'a mut Parser<'a>,
}

#[derive(Debug, PartialEq)]
pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

impl<'a> VirtualMachine<'a> {
    pub fn new(parser: &'a mut Parser<'a>) -> VirtualMachine {
        VirtualMachine {
            pc: 0,
            stack: vec![],
            parser,
        }
    }
    pub fn compile_and_run(&mut self) -> InterpretResult {
        if !self.parser.compile() {
            return InterpretResult::CompileError;
        }

        self.interpret()
    }

    pub fn interpret(&mut self) -> InterpretResult {
        loop {
            // Gets next OpCode using current PC
            let instruction = self.parser.current_chunk.get(self.pc);
            self.pc += 1; // Increases current PC for next instruction
            match instruction {
                Some(instruction) => match instruction {
                    OpCode::Constant(index) => {
                        if let Some(constant) =
                            self.parser.current_chunk.get_constant(*index).cloned()
                        {
                            self.stack.push(constant);
                        } else {
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
                            self.stack.push(second_value - first_value);
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
                            if first_value == 0.0 {
                                return InterpretResult::RuntimeError;
                            }
                            self.stack.push(second_value / first_value);
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
}
