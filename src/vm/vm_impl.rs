use std::cell::RefCell;
use std::mem::take;
use std::{collections::HashMap, rc::Rc};

use crate::common::lexer::lexer_impl::Lexer;

use super::object::Closure;
use super::{
    chunk::{value_equal, OpCode, Value},
    compiler::Compiler,
    object::FunctionType,
};

#[derive(Debug)]
struct CallFrame {
    closure: Closure,
    pc: usize,
    slots_start: usize,
}

#[derive(Debug)]
pub struct VirtualMachine {
    frames: Vec<CallFrame>,
    pub stack: Vec<Value>,
    pub globals: HashMap<String, Value>,
}

#[derive(Debug, PartialEq)]
pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

impl VirtualMachine {
    pub fn new(closure: Closure) -> VirtualMachine {
        let stack = vec![Value::Closure(closure.clone())];

        let call_frame = CallFrame {
            closure,
            pc: 0,
            slots_start: 0,
        };
        let frames = vec![call_frame];

        VirtualMachine {
            frames,
            stack,
            globals: HashMap::new(),
        }
    }

    pub fn new_with_globals(closure: Closure, globals: HashMap<String, Value>) -> VirtualMachine {
        let stack = vec![Value::Closure(closure.clone())];

        let call_frame = CallFrame {
            closure,
            pc: 0,
            slots_start: 0,
        };
        let frames = vec![call_frame];

        VirtualMachine {
            frames,
            stack,
            globals,
        }
    }

    pub fn interpret(&mut self) -> InterpretResult {
        loop {
            // Get the last frame
            let Some(frame) = self.frames.last_mut() else {
                return InterpretResult::Ok;
            };

            // Get next OpCode using current PC
            let chunk = &mut frame.closure.function.chunk;
            let Some(instruction) = chunk.get(frame.pc) else {
                self.frames.pop();
                if self.frames.is_empty() {
                    return InterpretResult::Ok;
                }
                continue;
            };
            // Increases current PC for next instruction
            frame.pc += 1;
            match instruction {
                OpCode::Constant(index) => {
                    let Some(constant) = chunk.get_constant(*index).cloned() else {
                        return InterpretResult::RuntimeError;
                    };

                    self.stack.push(constant);
                }
                OpCode::True => self.stack.push(Value::Boolean(true)),
                OpCode::False => self.stack.push(Value::Boolean(false)),
                OpCode::Null => self.stack.push(Value::Null),
                OpCode::Not => {
                    let Some(value) = self.stack.last_mut() else {
                        return InterpretResult::RuntimeError;
                    };

                    *value = Value::Boolean(is_falsey(value));
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
                    let Some(Value::Number(value)) = self.stack.last_mut() else {
                        return InterpretResult::RuntimeError;
                    };
                    *value *= -1.0;
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
                    (Some(Value::Number(divider)), Some(Value::Number(dividend))) => {
                        if divider == 0.0 {
                            return InterpretResult::RuntimeError;
                        }
                        self.stack.push(Value::Number(dividend / divider));
                    }
                    _ => return InterpretResult::RuntimeError,
                },
                OpCode::Pop => {
                    self.stack.pop();
                }
                OpCode::JumpIfFalse(target) => {
                    let Some(last) = self.stack.last() else {
                        return InterpretResult::RuntimeError;
                    };

                    if is_falsey(last) {
                        frame.pc += target;
                    }
                }
                OpCode::Jump(target) => {
                    frame.pc += target;
                }
                OpCode::Loop(target) => {
                    frame.pc -= target;
                }
                OpCode::GetUpvalue(index) => {
                    let Some(upvalue) = frame.closure.upvalues.get(*index) else {
                        return InterpretResult::RuntimeError;
                    };

                    self.stack.push(upvalue.clone());
                }
                OpCode::DefineGlobal(index) => {
                    match (chunk.get_constant(*index), self.stack.last()) {
                        (Some(Value::String(name)), Some(last)) => {
                            self.globals.insert(name.clone(), last.clone());
                        }
                        (_, _) => return InterpretResult::RuntimeError,
                    }
                }
                OpCode::GetGlobal(index) => {
                    let Some(Value::String(name)) = chunk.get_constant(*index) else {
                        return InterpretResult::RuntimeError;
                    };

                    let Some(global) = self.globals.get(name) else {
                        return InterpretResult::RuntimeError;
                    };

                    self.stack.push(global.clone());
                }
                OpCode::SetGlobal(index) => {
                    let Some(Value::String(name)) = chunk.get_constant(*index) else {
                        return InterpretResult::RuntimeError;
                    };

                    let Some(value) = self.stack.pop() else {
                        return InterpretResult::RuntimeError;
                    };

                    self.globals.insert(name.to_string(), value);
                }
                OpCode::GetLocal(index) => {
                    let Some(value) = self.stack.get(*index + frame.slots_start - 1) else {
                        return InterpretResult::RuntimeError;
                    };

                    self.stack.push(value.clone());
                }
                OpCode::SetLocal(index) => {
                    let Some(last) = self.stack.last() else {
                        return InterpretResult::RuntimeError;
                    };

                    let last_value = last.clone();

                    let Some(slot) = self.stack.get_mut(*index + frame.slots_start - 1) else {
                        return InterpretResult::RuntimeError;
                    };

                    *slot = last_value;
                }
                OpCode::SetUpvalue(index) => {
                    let Some(last) = self.stack.last() else {
                        return InterpretResult::RuntimeError;
                    };

                    let last_value = last.clone();

                    let Some(slot) = frame.closure.upvalues.get_mut(*index) else {
                        return InterpretResult::RuntimeError;
                    };

                    *slot = last_value;
                }
                OpCode::Upvalue(_, _) => return InterpretResult::RuntimeError,
                OpCode::Closure(index) => {
                    let Some(Value::Function(function)) = chunk.get_constant(*index).cloned()
                    else {
                        return InterpretResult::RuntimeError;
                    };

                    let mut closure = Closure {
                        function,
                        upvalues: vec![],
                    };

                    // Iterate through all the upvalues of the function and add them to the current closure
                    for _ in closure.function.upvalues.iter() {
                        let Some(OpCode::Upvalue(index, is_local)) = chunk.get(frame.pc) else {
                            return InterpretResult::RuntimeError;
                        };

                        if *is_local {
                            let Some(local_upvalue) = self.stack.get(index + frame.slots_start - 1)
                            else {
                                return InterpretResult::RuntimeError;
                            };
                            closure.upvalues.push(local_upvalue.clone());
                        } else {
                            let Some(non_local_upvalue) = frame.closure.upvalues.get(*index) else {
                                return InterpretResult::RuntimeError;
                            };

                            closure.upvalues.push(non_local_upvalue.clone());
                        }
                        // Increment PC for the next instruction
                        frame.pc += 1;
                    }

                    self.stack.push(Value::Closure(closure));
                }
                OpCode::Call(arguments_count) => {
                    let Some(Value::Closure(closure)) =
                        self.stack.get(self.stack.len() - 1 - arguments_count)
                    else {
                        return InterpretResult::RuntimeError;
                    };

                    if *arguments_count != closure.function.arity {
                        return InterpretResult::RuntimeError;
                    }

                    let new_frame = CallFrame {
                        closure: closure.clone(),
                        pc: 0,
                        // Move the pointer to where function arguments start
                        slots_start: self.stack.len() - arguments_count,
                    };

                    self.frames.push(new_frame);
                }
                OpCode::Return => {
                    let result = self.stack.pop().unwrap_or(Value::Null);
                    let slots_start = frame.slots_start;

                    self.frames.pop();

                    if self.frames.is_empty() {
                        return InterpretResult::Ok;
                    }

                    // Remove slots used for the frame
                    self.stack.truncate(slots_start);
                    self.stack.push(result);
                }
            }
        }
    }
}

fn is_falsey(value: &Value) -> bool {
    match value {
        Value::Boolean(bool) => !bool,
        Value::Null => true,
        _ => false,
    }
}

pub fn compile_and_run(input: String) {
    let lexer = Lexer::new(&input);
    let mut compiler = Compiler::new(Rc::new(RefCell::new(lexer)), FunctionType::Script);

    if matches!(compiler.compile(), InterpretResult::CompileError) {
        println!("compiler has {} errors", compiler.errors.len());
        for error in compiler.errors {
            println!("compiler error: {}", error);
        }
        return;
    }

    let closure = Closure {
        function: take(&mut compiler.function),
        upvalues: vec![],
    };

    let mut vm = VirtualMachine::new(closure);

    vm.interpret();
}
