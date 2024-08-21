use super::{
    chunk::{value_equal, OpCode, Value},
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
                    OpCode::True => self.stack.push(Value::Boolean(true)),
                    OpCode::False => self.stack.push(Value::Boolean(false)),
                    OpCode::Null => self.stack.push(Value::Null),
                    OpCode::Not => {
                        if let Some(value) = self.stack.last_mut() {
                            *value = Value::Boolean(is_falsey(value.clone()));
                        } else {
                            return InterpretResult::RuntimeError;
                        }
                    }
                    OpCode::Equal => match (self.stack.pop(), self.stack.pop()) {
                        (Some(a), Some(b)) => self.stack.push(Value::Boolean(value_equal(a, b))),
                        (_, _) => return InterpretResult::RuntimeError,
                    },
                    OpCode::NotEqual => match (self.stack.pop(), self.stack.pop()) {
                        (Some(a), Some(b)) => self.stack.push(Value::Boolean(!value_equal(a, b))),
                        (_, _) => return InterpretResult::RuntimeError,
                    },
                    OpCode::Greater => match (self.stack.pop(), self.stack.pop()) {
                        (Some(Value::Number(first_value)), Some(Value::Number(second_value))) => {
                            self.stack.push(Value::Boolean(second_value > first_value));
                        }
                        _ => return InterpretResult::RuntimeError,
                    },
                    OpCode::GreaterEqual => match (self.stack.pop(), self.stack.pop()) {
                        (Some(Value::Number(first_value)), Some(Value::Number(second_value))) => {
                            self.stack.push(Value::Boolean(second_value >= first_value));
                        }
                        _ => return InterpretResult::RuntimeError,
                    },
                    OpCode::Less => match (self.stack.pop(), self.stack.pop()) {
                        (Some(Value::Number(first_value)), Some(Value::Number(second_value))) => {
                            self.stack.push(Value::Boolean(second_value < first_value));
                        }
                        _ => return InterpretResult::RuntimeError,
                    },
                    OpCode::LessEqual => match (self.stack.pop(), self.stack.pop()) {
                        (Some(Value::Number(first_value)), Some(Value::Number(second_value))) => {
                            self.stack.push(Value::Boolean(second_value <= first_value));
                        }
                        _ => return InterpretResult::RuntimeError,
                    },
                    OpCode::Negate => {
                        if let Some(value) = self.stack.last_mut() {
                            match value {
                                Value::Number(n) => *value = Value::Number(-*n),
                                _ => return InterpretResult::RuntimeError,
                            }
                        } else {
                            return InterpretResult::RuntimeError;
                        }
                    }
                    OpCode::Add => match (self.stack.pop(), self.stack.pop()) {
                        (Some(Value::Number(first_value)), Some(Value::Number(second_value))) => {
                            self.stack.push(Value::Number(first_value + second_value));
                        }
                        (Some(Value::String(first_value)), Some(Value::String(second_value))) => {
                            self.stack
                                .push(Value::String(format!("{}{}", second_value, first_value)));
                        }
                        _ => return InterpretResult::RuntimeError,
                    },
                    OpCode::Subtract => match (self.stack.pop(), self.stack.pop()) {
                        (Some(Value::Number(first_value)), Some(Value::Number(second_value))) => {
                            self.stack.push(Value::Number(second_value - first_value));
                        }
                        _ => return InterpretResult::RuntimeError,
                    },
                    OpCode::Multiply => match (self.stack.pop(), self.stack.pop()) {
                        (Some(Value::Number(first_value)), Some(Value::Number(second_value))) => {
                            self.stack.push(Value::Number(first_value * second_value));
                        }
                        _ => return InterpretResult::RuntimeError,
                    },
                    OpCode::Divide => match (self.stack.pop(), self.stack.pop()) {
                        (Some(Value::Number(first_value)), Some(Value::Number(second_value))) => {
                            if first_value == 0.0 {
                                return InterpretResult::RuntimeError;
                            }
                            self.stack.push(Value::Number(second_value / first_value));
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

fn is_falsey(value: Value) -> bool {
    match value {
        Value::Boolean(bool) => !bool,
        Value::Null => true,
        _ => false,
    }
}
