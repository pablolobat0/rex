#[cfg(test)]
mod test {
    use crate::{
        common::lexer::lexer_impl::Lexer,
        vm::{
            chunk::{OpCode, Value},
            compiler::Compiler,
            vm_impl::{InterpretResult, VirtualMachine},
        },
    };

    fn check_compiler_errors(compiler: &Compiler) {
        let errors = &compiler.errors;
        if errors.is_empty() {
            return;
        }

        println!("compiler has {} errors", errors.len());
        for error in errors {
            println!("compiler error: {}", error);
        }
        panic!("compiler errors encountered");
    }

    fn test_number(input: &str, result: f64) {
        let mut lexer = Lexer::new(input);
        let mut compiler = Compiler::new(&mut lexer);
        compiler.compile_one_statement();

        check_compiler_errors(&compiler);

        let mut vm = VirtualMachine::new(&mut compiler);

        assert_eq!(
            vm.interpret(),
            InterpretResult::Ok,
            "VM should run without errors"
        );

        assert_eq!(vm.stack.first(), Some(&Value::Number(result)));
    }

    fn test_bool(input: &str, result: bool) {
        let mut lexer = Lexer::new(input);
        let mut compiler = Compiler::new(&mut lexer);

        compiler.compile_one_statement();

        check_compiler_errors(&compiler);

        let mut vm = VirtualMachine::new(&mut compiler);

        assert_eq!(
            vm.interpret(),
            InterpretResult::Ok,
            "VM should run without errors"
        );

        assert_eq!(vm.stack.first(), Some(&Value::Boolean(result)));
    }

    fn test_string(input: &str, result: String) {
        let mut lexer = Lexer::new(input);
        let mut compiler = Compiler::new(&mut lexer);

        compiler.compile_one_statement();

        check_compiler_errors(&compiler);

        let mut vm = VirtualMachine::new(&mut compiler);

        assert_eq!(
            vm.interpret(),
            InterpretResult::Ok,
            "VM should run without errors"
        );
        println!("{}", input);

        assert_eq!(vm.stack.first(), Some(&Value::String(result)));
    }

    #[test]
    fn constant() {
        test_number("1", 1.0);
    }

    #[test]
    fn boolean() {
        let tests = [("true", true), ("false", false)];

        for (input, result) in tests {
            test_bool(input, result);
        }
    }

    #[test]
    fn null() {
        let mut lexer = Lexer::new("null");
        let mut compiler = Compiler::new(&mut lexer);

        assert!(
            compiler.compile_one_statement(),
            "Compiler should compile without errors"
        );

        let mut vm = VirtualMachine::new(&mut compiler);

        assert_eq!(
            vm.interpret(),
            InterpretResult::Ok,
            "VM should run without errors"
        );

        assert_eq!(vm.stack.first(), Some(&Value::Null));
    }

    #[test]
    fn negate() {
        test_number("-1.2", -1.2);
    }

    #[test]
    fn add() {
        test_number("10+5", 15.0);
    }

    #[test]
    fn subtract() {
        test_number("10-5", 5.0);
    }

    #[test]
    fn multiply() {
        test_number("10*5", 50.0);
    }

    #[test]
    fn divide() {
        test_number("10/5", 2.0);
    }

    #[test]
    fn division_by_zero() {
        let input = "10 / 0";
        let mut lexer = Lexer::new(input);
        let mut compiler = Compiler::new(&mut lexer);

        assert!(
            compiler.compile_one_statement(),
            "Compiler should compile without errors"
        );

        let mut vm = VirtualMachine::new(&mut compiler);
        assert_eq!(
            vm.interpret(),
            InterpretResult::RuntimeError,
            "VM should return a runtime error for division by zero"
        );
    }

    #[test]
    fn not() {
        let tests = [
            ("!true", false),
            ("!false", true),
            ("!null", true),
            ("!1", false),
        ];

        for (input, result) in tests {
            test_bool(input, result);
        }
    }

    #[test]
    fn boolean_infix() {
        let tests = [
            ("true == true", true),
            ("false == true", false),
            ("\"hola\" == \"hola\"", true),
            ("\"hola\" == \"mundo\"", false),
            ("1 == 1", true),
            ("1 == 2", false),
            ("1 != 1", false),
            ("1 != 2", true),
            ("\"hola\" != \"hola\"", false),
            ("\"hola\" != \"mundo\"", true),
            ("true != true", false),
            ("true != false", true),
            ("1 > 0", true),
            ("1 > 1", false),
            ("1 >= 1", true),
            ("1 >= 2", false),
            ("1 < 1", false),
            ("1 < 2", true),
            ("1 <= 1", true),
            ("1 <= 0", false),
        ];

        for (input, result) in tests {
            test_bool(input, result);
        }
    }

    #[test]
    fn string_literal() {
        test_string("\"hola\"", "hola".to_string());
    }

    #[test]
    fn string_concatenation() {
        test_string("\"hola\" + \" mundo\"", "hola mundo".to_string());
    }

    #[test]
    fn define_global() {
        let input = "let a = 1";

        let mut lexer = Lexer::new(input);
        let mut compiler = Compiler::new(&mut lexer);

        compiler.compile();

        check_compiler_errors(&compiler);

        let mut vm = VirtualMachine::new(&mut compiler);

        assert_eq!(
            vm.interpret(),
            InterpretResult::Ok,
            "VM should run without errors"
        );

        assert_eq!(vm.globals.get("a"), Some(&Value::Number(1.0)));
    }

    #[test]
    fn get_global() {
        let input = "let a = 1\nlet b = a + 3";

        let mut lexer = Lexer::new(input);
        let mut compiler = Compiler::new(&mut lexer);

        compiler.compile();

        check_compiler_errors(&compiler);

        let mut vm = VirtualMachine::new(&mut compiler);

        assert_eq!(
            vm.interpret(),
            InterpretResult::Ok,
            "VM should run without errors"
        );

        assert_eq!(vm.globals.get("b"), Some(&Value::Number(4.0)));
    }

    #[test]
    fn set_global() {
        let input = "let a = 1\na = 3";

        let mut lexer = Lexer::new(input);
        let mut compiler = Compiler::new(&mut lexer);

        compiler.compile();

        check_compiler_errors(&compiler);

        let mut vm = VirtualMachine::new(&mut compiler);

        assert_eq!(
            vm.interpret(),
            InterpretResult::Ok,
            "VM should run without errors"
        );

        assert_eq!(vm.globals.get("a"), Some(&Value::Number(3.0)));
    }

    #[test]
    fn define_local() {
        let input = "{
                    let a = 14
                   }";

        let mut lexer = Lexer::new(input);
        let mut compiler = Compiler::new(&mut lexer);

        compiler.compile_one_statement();

        check_compiler_errors(&compiler);
        assert_eq!(
            compiler.current_chunk.code,
            vec![OpCode::Constant(0), OpCode::Pop]
        );

        assert_eq!(
            compiler.current_chunk.constants.first(),
            Some(&Value::Number(14.0))
        );

        let mut vm = VirtualMachine::new(&mut compiler);

        assert_eq!(
            vm.interpret(),
            InterpretResult::Ok,
            "VM should run without errors"
        );

        assert_eq!(vm.globals.get("a"), None);
    }

    #[test]
    fn get_local() {
        let input = "{ 
                    let a = 14
                    let b = a
                    b
                    }";

        let mut lexer = Lexer::new(input);
        let mut compiler = Compiler::new(&mut lexer);

        compiler.compile();

        assert_eq!(
            compiler.current_chunk.code,
            vec![
                OpCode::Constant(0),
                OpCode::GetLocal(0),
                OpCode::GetLocal(1),
                OpCode::Pop,
                OpCode::Pop,
                OpCode::Pop
            ]
        );

        check_compiler_errors(&compiler);

        let mut vm = VirtualMachine::new(&mut compiler);

        assert_eq!(
            vm.interpret(),
            InterpretResult::Ok,
            "VM should run without errors"
        );
    }
    #[test]
    fn set_local() {
        let input = "{ 
                    let a = 14
                    let b = a
                    b = 15
                    }";

        let mut lexer = Lexer::new(input);
        let mut compiler = Compiler::new(&mut lexer);

        compiler.compile();

        check_compiler_errors(&compiler);

        assert_eq!(
            compiler.current_chunk.code,
            vec![
                OpCode::Constant(0),
                OpCode::GetLocal(0),
                OpCode::Constant(1),
                OpCode::SetLocal(1),
                OpCode::Pop,
                OpCode::Pop,
                OpCode::Pop
            ]
        );

        let mut vm = VirtualMachine::new(&mut compiler);

        assert_eq!(
            vm.interpret(),
            InterpretResult::Ok,
            "VM should run without errors"
        );
    }

    #[test]
    fn if_true() {
        let tests = vec![
            // Caso 1: if true
            (
                "
            let a = 1
            if true {
                a = 2
            }",
                &Value::Number(2.0),
            ),
            // Caso 2: if false
            (
                "
            let a = 1
            if false {
                a = 2
            }",
                &Value::Number(1.0),
            ),
            // Caso 3: if true else
            (
                "
            let a = 1
            if true {
                a = 2
            } else {
                a = 3
            }",
                &Value::Number(2.0),
            ),
            // Caso 4: if false else
            (
                "
            let a = 1
            if false {
                a = 2
            } else {
                a = 3
            }",
                &Value::Number(3.0),
            ),
        ];

        for (input, result) in tests {
            let mut lexer = Lexer::new(input);
            let mut compiler = Compiler::new(&mut lexer);

            compiler.compile();

            check_compiler_errors(&compiler);

            let mut vm = VirtualMachine::new(&mut compiler);

            assert_eq!(
                vm.interpret(),
                InterpretResult::Ok,
                "VM should run without errors"
            );

            assert_eq!(vm.globals.get("a"), Some(result));
        }
    }

    #[test]
    fn while_statement() {
        let input = " 
                    let a = 1
                    while a != 10 {
                        a = a + 1
                    }
                    ";

        let mut lexer = Lexer::new(input);
        let mut compiler = Compiler::new(&mut lexer);

        compiler.compile();

        check_compiler_errors(&compiler);

        let mut vm = VirtualMachine::new(&mut compiler);

        assert_eq!(
            vm.interpret(),
            InterpretResult::Ok,
            "VM should run without errors"
        );

        assert_eq!(vm.globals.get("a"), Some(&Value::Number(10.0)));
    }
}
