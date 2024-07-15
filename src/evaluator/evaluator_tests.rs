mod test {
    use crate::{
        evaluator::{
            evaluator::eval,
            object::object::{Environment, Object},
        },
        lexer::lexer::Lexer,
        parser::{ast::ast::Node, parser::Parser},
    };

    fn test_eval(input: &str) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let mut environment = Environment::new();

        return eval(Node::Program(program), &mut environment);
    }
    #[test]
    fn test_eval_integer_literal() {
        let input = "5;";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(5));
    }

    #[test]
    fn test_eval_boolean_literal() {
        let test = vec![("true;", true), ("false;", false)];
        for (input, expected) in test {
            let result = test_eval(input);
            assert_eq!(result, Object::Boolean(expected));
        }
    }

    #[test]
    fn test_eval_bang_prefix_expression() {
        let test = vec![("!true;", false), ("!false;", true)];
        for (input, expected) in test {
            let result = test_eval(input);
            assert_eq!(result, Object::Boolean(expected));
        }
    }
    #[test]
    fn test_eval_minus_prefix_expression() {
        let test = vec![("-5;", -5), ("-77;", -77)];
        for (input, expected) in test {
            let result = test_eval(input);
            assert_eq!(result, Object::Integer(expected));
        }
    }
    #[test]
    fn test_eval_integer_infix_expressions() {
        let test = vec![
            ("10-5;", 5),
            ("77+70;", 147),
            ("20*20", 400),
            ("100/10", 10),
        ];
        for (input, expected) in test {
            let result = test_eval(input);
            assert_eq!(result, Object::Integer(expected));
        }
        let test = vec![
            ("10==5;", false),
            ("10==10;", true),
            ("77!=80;", true),
            ("77!=77;", false),
            ("21>20", true),
            ("21>22", false),
            ("21>=21", true),
            ("21>=22", false),
            ("19<20", true),
            ("21<20", false),
            ("20<=20", true),
            ("21<=20", false),
            ("21>20", true),
        ];
        for (input, expected) in test {
            let result = test_eval(input);
            assert_eq!(result, Object::Boolean(expected));
        }
    }
    #[test]
    fn test_eval_boolean_infix_expressions() {
        let test = vec![
            ("false==true;", false),
            ("true==true;", true),
            ("false==false;", true),
            ("true!=false;", true),
            ("true!=true;", false),
            ("false!=false", false),
        ];
        for (input, expected) in test {
            let result = test_eval(input);
            assert_eq!(result, Object::Boolean(expected));
        }
    }

    #[test]
    fn test_eval_if_expression_true_condition() {
        let input = "if (true) { 10 } else { 20 };";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(10));
    }

    #[test]
    fn test_eval_if_expression_false_condition() {
        let input = "if (false) { 10 } else { 20 };";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(20));
    }

    #[test]
    fn test_eval_if_expression_no_alternative() {
        let input = "if (false) { 10 };";
        let result = test_eval(input);
        assert_eq!(result, Object::Null);
    }
    #[test]
    fn test_eval_return() {
        let input = "return 10;";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(10));
    }
    #[test]
    fn test_eval_multiple_return() {
        let input = "if 10 > 1 {
                        if 10 > 1 {
                            return 10; 
                        }
                        return 1;
                        }";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(10));
    }

    #[test]
    fn test_unknown_prefix_operator() {
        let input = "-true;";
        let result = test_eval(input);
        assert_eq!(
            result,
            Object::Error("type mismatch, expected an INTEGER but found BOOLEAN".to_string())
        );
    }

    #[test]
    fn test_type_mismatch_infix() {
        let input = "5 + true;";
        let result = test_eval(input);
        assert_eq!(
            result,
            Object::Error("type mismatch: INTEGER + BOOLEAN".to_string())
        );
    }

    #[test]
    fn test_division_by_zero() {
        let input = "5 / 0;";
        let result = test_eval(input);
        assert_eq!(result, Object::Error("error division by 0".to_string()));
    }

    #[test]
    fn test_if_expression_with_non_boolean_condition() {
        let input = "if (5) { 10 } else { 20 };";
        let result = test_eval(input);
        assert_eq!(
            result,
            Object::Error("type mismatch, expected a BOOLEAN but found INTEGER".to_string())
        );
    }
    #[test]
    fn test_identifier_expression() {
        let input = "let x = 5; x;";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(5));

        let input = "let x = 5; let y = x; y;";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(5));

        let input = "let x = 5; let y = x; let z = x + y + 5; z;";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(15));
    }

    #[test]
    fn test_let_statements() {
        let input = "let x = 5; x;";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(5));

        let input = "let x = 5 * 5; x;";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(25));

        let input = "let a = 5; let b = a; b;";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(5));

        let input = "let a = 5; let b = a; let c = a + b + 5; c;";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(15));
    }

    #[test]
    fn test_identifier_not_found() {
        let input = "x;";
        let result = test_eval(input);
        assert_eq!(result, Object::Error("identifier not found: x".to_string()));
    }
}
