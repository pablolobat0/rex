#[cfg(test)]
mod test {
    use std::{cell::RefCell, rc::Rc};

    use crate::{
        common::lexer::lexer::Lexer,
        vm::{
            chunk::{Chunk, OpCode},
            compiler::Parser,
            vm::{InterpretResult, VirtualMachine},
        },
    };

    #[test]
    fn test_constant() {
        let mut chunk = Chunk::new();
        let constant = chunk.add_constant(1.2);
        chunk.write(OpCode::Constant(constant), 1);

        let code = vec![OpCode::Constant(constant)];

        assert_eq!(chunk.get(constant), code.get(constant));
        assert_eq!(chunk.get_line(constant), Option::Some(1));
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
        assert_eq!(vm.stack.get(0), Some(-1.2).as_ref());
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
        assert_eq!(vm.stack.get(0), Some(15.0).as_ref());
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
        assert_eq!(vm.stack.get(0), Some(5.0).as_ref());
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
        assert_eq!(vm.stack.get(0), Some(50.0).as_ref());
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
        assert_eq!(vm.stack.get(0), Some(2.0).as_ref());
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
