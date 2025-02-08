#[cfg(test)]
mod test {
    use crate::{
        common::lexer::lexer::Lexer,
        interpreter::evaluator::{
            evaluator::eval,
            object::{Environment, Object},
        },
        interpreter::parser::{ast::Node, parser::Parser},
    };

    #[cfg(test)]
    fn test_eval(input: &str) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let mut environment = Environment::new();

        return eval(Node::Program(program), &mut environment);
    }

    #[test]
    fn test_eval_integer_literal() {
        let input = "5\n";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(5));
    }

    #[test]
    fn test_eval_float_literal() {
        let test = vec![("0.55", 0.55), (".565", 0.565)];
        for (input, expected) in test {
            let result = test_eval(input);
            assert_eq!(result, Object::Float(expected));
        }
    }

    #[test]
    fn test_eval_boolean_literal() {
        let test = vec![("true\n", true), ("false\n", false)];
        for (input, expected) in test {
            let result = test_eval(input);
            assert_eq!(result, Object::Boolean(expected));
        }
    }

    #[test]
    fn test_eval_string_literal() {
        let test = vec![("\"hola\"\n", "hola"), ("\"mundo\"\n", "mundo")];
        for (input, expected) in test {
            let result = test_eval(input);
            assert_eq!(result, Object::String(expected.to_string()));
        }
    }

    #[test]
    fn test_eval_bang_prefix_expression() {
        let test = vec![("!true\n", false), ("!false\n", true)];
        for (input, expected) in test {
            let result = test_eval(input);
            assert_eq!(result, Object::Boolean(expected));
        }
    }
    #[test]
    fn test_eval_minus_prefix_expression() {
        let test = vec![("-5\n", -5), ("-77\n", -77)];
        for (input, expected) in test {
            let result = test_eval(input);
            assert_eq!(result, Object::Integer(expected));
        }
    }
    #[test]
    fn test_eval_float_minus_prefix_expression() {
        let test = vec![("-3.14", -3.14), ("-.3", -0.3)];
        for (input, expected) in test {
            let result = test_eval(input);
            assert_eq!(result, Object::Float(expected));
        }
    }
    #[test]
    fn test_eval_integer_infix_expressions() {
        let test = vec![
            ("10-5\n", 5),
            ("77+70\n", 147),
            ("20*20\n", 400),
            ("100/10\n", 10),
        ];
        for (input, expected) in test {
            let result = test_eval(input);
            assert_eq!(result, Object::Integer(expected));
        }
        let test = vec![
            ("10==5\n", false),
            ("10==10\n", true),
            ("77!=80\n", true),
            ("77!=77\n", false),
            ("21>20\n", true),
            ("21>22\n", false),
            ("21>=21\n", true),
            ("21>=22\n", false),
            ("19<20\n", true),
            ("21<20\n", false),
            ("20<=20\n", true),
            ("21<=20\n", false),
            ("21>20\n", true),
        ];
        for (input, expected) in test {
            let result = test_eval(input);
            assert_eq!(result, Object::Boolean(expected));
        }
    }

    #[test]
    fn test_eval_float_infix_expressions() {
        let test = vec![
            ("10.5+5.65", 16.15),
            ("7.5-7", 0.5),
            ("0.5*7", 3.5),
            ("11/10", 1.1),
            ("10.5/2", 5.25),
        ];
        for (input, expected) in test {
            let result = test_eval(input);
            assert_eq!(result, Object::Float(expected));
        }
        let test = vec![
            ("10.5==5.5", false),
            ("10.5==10.5", true),
            ("77.4!=80.7", true),
            ("77.4!=77.4", false),
            ("21.4>20.55", true),
            ("21.34>22.5", false),
            ("21.23>=21.23", true),
            ("21.13>=22.32", false),
            ("19.2343<20.34", true),
            ("21.314<20.33", false),
            ("20.12<=20.12", true),
            ("21.234<=20.234", false),
            ("21.1243>20.2432", true),
        ];
        for (input, expected) in test {
            let result = test_eval(input);
            assert_eq!(result, Object::Boolean(expected));
        }
    }

    #[test]
    fn test_eval_boolean_infix_expressions() {
        let test = vec![
            ("false==true\n", false),
            ("true==true\n", true),
            ("false==false\n", true),
            ("true!=false\n", true),
            ("true!=true\n", false),
            ("false!=false\n", false),
        ];
        for (input, expected) in test {
            let result = test_eval(input);
            assert_eq!(result, Object::Boolean(expected));
        }
    }

    #[test]
    fn test_eval_string_literal_infix_expressions() {
        let test = vec![("\"hola\" + \" mundo\"\n", "hola mundo")];
        for (input, expected) in test {
            let result = test_eval(input);
            assert_eq!(result, Object::String(expected.to_string()));
        }

        let test = vec![
            ("\"hola\" == \" mundo\"\n", false),
            (
                "\"interprete en rust\" == \"intereprete en python\"\n",
                false,
            ),
            ("\"rust\" != \"python\"\n", true),
            ("\"golang\" != \"golang\"\n", false),
        ];
        for (input, expected) in test {
            let result = test_eval(input);
            assert_eq!(result, Object::Boolean(expected));
        }
    }

    #[test]
    fn test_eval_if_expression_true_condition() {
        let input = "if (true) { 10\n } else { 20\n }\n";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(10));
    }

    #[test]
    fn test_eval_if_expression_false_condition() {
        let input = "if (false) { 10\n } else { 20\n }\n";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(20));
    }

    #[test]
    fn test_eval_if_expression_no_alternative() {
        let input = "if (false) { 10\n }\n";
        let result = test_eval(input);
        assert_eq!(result, Object::Null);
    }
    #[test]
    fn test_eval_return() {
        let input = "return 10\n";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(10));
    }
    #[test]
    fn test_eval_multiple_return() {
        let input = "if 10 > 1 {
                        if 10 > 1 {
                            return 10
                        }
                        return 1
                        }\n";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(10));
    }

    #[test]
    fn test_unknown_prefix_operator() {
        let input = "-true\n";
        let result = test_eval(input);
        assert_eq!(
            result,
            Object::Error("type mismatch, expected an INTEGER but found BOOLEAN".to_string())
        );
    }

    #[test]
    fn test_type_mismatch_infix() {
        let input = "5 + true\n";
        let result = test_eval(input);
        assert_eq!(
            result,
            Object::Error("type mismatch: INTEGER + BOOLEAN".to_string())
        );
    }

    #[test]
    fn test_division_by_zero() {
        let input = "5 / 0\n";
        let result = test_eval(input);
        assert_eq!(result, Object::Error("error division by 0".to_string()));
    }

    #[test]
    fn test_if_expression_with_non_boolean_condition() {
        let input = "if (5) { 10\n } else { 20\n }\n";
        let result = test_eval(input);
        assert_eq!(
            result,
            Object::Error("type mismatch, expected a BOOLEAN but found INTEGER".to_string())
        );
    }
    #[test]
    fn test_identifier_expression() {
        let input = "let x = 5\n x\n";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(5));

        let input = "let x = 5\n let y = x\n y\n";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(5));

        let input = "let x = 5\n let y = x\n let z = x + y + 5\n z\n";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(15));
    }

    #[test]
    fn test_let_statements() {
        let input = "let x = 5\n x\n";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(5));

        let input = "let x = 5 * 5\n x\n";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(25));

        let input = "let a = 5\n let b = a\n b\n";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(5));

        let input = "let a = 5\n let b = a\n let c = a + b + 5\n c\n";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(15));
    }

    #[test]
    fn test_identifier_not_found() {
        let input = "x\n";
        let result = test_eval(input);
        assert_eq!(result, Object::Error("identifier not found: x".to_string()));
    }

    #[test]
    fn test_function_definition() {
        let input = "fn(x) { x + 2\n }\n";
        let result = test_eval(input);

        match result {
            Object::Function(function) => {
                assert_eq!(function.parameters.len(), 1);
                assert_eq!(function.parameters[0].to_string(), "x");
                assert_eq!(function.body.to_string(), "(x + 2)");
            }
            _ => panic!("Expected Function, got {:?}", result),
        }
    }

    #[test]
    fn test_function_application() {
        let input = "let identity = fn(x) { x\n }\n identity(5)\n";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(5));

        let input = "let double = fn(x) { x * 2\n }\n double(5)\n";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(10));

        let input = "let add = fn(x, y) { x + y\n }\n add(5, 5)\n";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(10));

        let input = "let add = fn(x, y) { x + y\n }\n add(5 + 5, add(5, 5))\n";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(20));
    }

    #[test]
    fn test_closures() {
        let input =
            "let new_adder = fn(x) { fn(y) { x + y\n }\n }\n let add_two = new_adder(2)\n add_two(2)\n";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(4));
    }

    #[test]
    fn test_eval_while_with_return() {
        let input = "while (true) { return 5\n }\n";
        let result = test_eval(input);

        assert_eq!(result, Object::Integer(5));
    }

    #[test]
    fn test_eval_while() {
        let input = "let a = 1
                    while (true) { 
                        if a == 5 {
                            return 5
                        }
                        a = a + 1
                    }\n";
        let result = test_eval(input);

        assert_eq!(result, Object::Integer(5));
    }

    #[test]
    fn test_eval_assigment() {
        let input = "let x = 1
                     x = x + 3
                     x\n";
        let result = test_eval(input);

        assert_eq!(result, Object::Integer(4));
    }
}
