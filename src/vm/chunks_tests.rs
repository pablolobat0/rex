#[cfg(test)]
mod test {
    use std::{cell::RefCell, rc::Rc};

    use crate::{
        common::lexer::lexer::Lexer,
        vm::{
            chunk::{Chunk, OpCode, Value},
            compiler::Parser,
            vm::{InterpretResult, VirtualMachine},
        },
    };

    #[test]
    fn test_constant() {
        let mut lexer = Lexer::new("1");
        let mut parser = Parser::new(&mut lexer);

        assert!(parser.compile(), "Parser should compile without errors");

        let mut vm = VirtualMachine::new(&mut parser);

        assert_eq!(
            vm.interpret(),
            InterpretResult::Ok,
            "VM should run without errors"
        );
        assert_eq!(vm.stack.get(0), Some(&Value::Number(1.0)));
    }

    #[test]
    fn test_negate() {
        let mut lexer = Lexer::new("-1.2");
        let mut parser = Parser::new(&mut lexer);

        assert!(parser.compile(), "Parser should compile without errors");

        let mut vm = VirtualMachine::new(&mut parser);

        assert_eq!(
            vm.interpret(),
            InterpretResult::Ok,
            "VM should run without errors"
        );
        assert_eq!(vm.stack.get(0), Some(&Value::Number(-1.2)));
    }

    #[test]
    fn test_add() {
        let mut lexer = Lexer::new("10+5");
        let mut parser = Parser::new(&mut lexer);

        assert!(parser.compile(), "Parser should compile without errors");

        let mut vm = VirtualMachine::new(&mut parser);

        assert_eq!(
            vm.interpret(),
            InterpretResult::Ok,
            "VM should run without errors"
        );
        assert_eq!(vm.stack.get(0), Some(&Value::Number(15.0)));
    }
    #[test]
    fn test_subtract() {
        let mut lexer = Lexer::new("10-5");
        let mut parser = Parser::new(&mut lexer);

        assert!(parser.compile(), "Parser should compile without errors");

        let mut vm = VirtualMachine::new(&mut parser);

        assert_eq!(
            vm.interpret(),
            InterpretResult::Ok,
            "VM should run without errors"
        );
        assert_eq!(vm.stack.get(0), Some(&Value::Number(5.0)));
    }
    #[test]
    fn test_multiply() {
        let mut lexer = Lexer::new("10*5");
        let mut parser = Parser::new(&mut lexer);

        assert!(parser.compile(), "Parser should compile without errors");

        let mut vm = VirtualMachine::new(&mut parser);

        assert_eq!(
            vm.interpret(),
            InterpretResult::Ok,
            "VM should run without errors"
        );
        assert_eq!(vm.stack.get(0), Some(&Value::Number(50.0)));
    }
    #[test]
    fn test_divide() {
        let input = "10 / 5";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);

        assert!(parser.compile(), "Parser should compile without errors");

        let mut vm = VirtualMachine::new(&mut parser);

        assert_eq!(
            vm.interpret(),
            InterpretResult::Ok,
            "VM should run without errors"
        );
        assert_eq!(vm.stack.get(0), Some(&Value::Number(2.0)));
    }
    #[test]
    fn test_division_by_zero() {
        let input = "10 / 0";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);

        assert!(parser.compile(), "Parser should compile without errors");

        let mut vm = VirtualMachine::new(&mut parser);
        assert_eq!(
            vm.interpret(),
            InterpretResult::RuntimeError,
            "VM should return a runtime error for division by zero"
        );
    }
}
