mod test {
    use crate::{
        evaluator::{evaluator::eval, object::object::Object},
        lexer::lexer::Lexer,
        parser::{ast::ast::Node, parser::Parser},
    };

    fn test_eval(input: &str) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        return eval(Node::Program(program));
    }
    #[test]
    fn test_eval_integer_literal() {
        let input = "5;";
        let result = test_eval(input);
        assert_eq!(result, Object::Integer(5));
    }

    #[test]
    fn test_eval_boolean_literal_true() {
        let input = "true;";
        let result = test_eval(input);
        assert_eq!(result, Object::Boolean(true));
    }

    #[test]
    fn test_eval_boolean_literal_false() {
        let input = "false;";
        let result = test_eval(input);
        assert_eq!(result, Object::Boolean(false));
    }
}
