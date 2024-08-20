#[cfg(test)]
mod test {

    use crate::{
        common::lexer::lexer::Lexer,
        vm::{
            chunk::{Chunk, OpCode, Value},
            compiler::Parser,
            vm::{InterpretResult, VirtualMachine},
        },
    };

    fn test_number(input: &str, result: f64) {
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);

        assert!(parser.compile(), "Parser should compile without errors");

        let mut vm = VirtualMachine::new(&mut parser);

        assert_eq!(
            vm.interpret(),
            InterpretResult::Ok,
            "VM should run without errors"
        );

        assert_eq!(vm.stack.get(0), Some(&Value::Number(result)));
    }

    fn test_bool(input: &str, result: bool) {
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);

        assert!(parser.compile(), "Parser should compile without errors");

        let mut vm = VirtualMachine::new(&mut parser);

        assert_eq!(
            vm.interpret(),
            InterpretResult::Ok,
            "VM should run without errors"
        );

        assert_eq!(vm.stack.get(0), Some(&Value::Boolean(result)));
    }

    #[test]
    fn test_constant() {
        test_number("1", 1.0);
    }

    #[test]
    fn test_boolean() {
        let tests = [("true", true), ("false", false)];

        for (input, result) in tests {
            test_bool(input, result);
        }
    }

    #[test]
    fn test_null() {
        let mut lexer = Lexer::new("null");
        let mut parser = Parser::new(&mut lexer);

        assert!(parser.compile(), "Parser should compile without errors");

        let mut vm = VirtualMachine::new(&mut parser);

        assert_eq!(
            vm.interpret(),
            InterpretResult::Ok,
            "VM should run without errors"
        );

        assert_eq!(vm.stack.get(0), Some(&Value::Null));
    }

    #[test]
    fn test_negate() {
        test_number("-1.2", -1.2);
    }

    #[test]
    fn test_add() {
        test_number("10+5", 15.0);
    }

    #[test]
    fn test_subtract() {
        test_number("10-5", 5.0);
    }

    #[test]
    fn test_multiply() {
        test_number("10*5", 50.0);
    }

    #[test]
    fn test_divide() {
        test_number("10/5", 2.0);
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
