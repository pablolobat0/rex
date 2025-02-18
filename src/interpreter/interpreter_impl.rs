use crate::{
    common::lexer::lexer_impl::Lexer,
    interpreter::{evaluator::evaluator_impl::eval, parser::ast::Node},
};

use super::{evaluator::object::Environment, parser::parser_impl::Parser};

pub fn interpret_ast(input: String) {
    let mut environment = Environment::new();
    loop {
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if !parser.errors.is_empty() {
            print_parser_errors(parser.errors);
            continue;
        }

        println!("{}", eval(Node::Program(program), &mut environment));
    }
}

fn print_parser_errors(errors: Vec<String>) {
    for error in errors {
        println!("{}", error);
    }
}
