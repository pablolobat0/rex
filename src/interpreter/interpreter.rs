use crate::{
    common::lexer::lexer::Lexer,
    interpreter::{evaluator::evaluator::eval, parser::ast::Node},
};

use super::{evaluator::object::Environment, parser::parser::Parser};

pub fn interpret_ast(input: String) {
    let mut environment = Environment::new();
    loop {
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if parser.errors.len() != 0 {
            print_parser_errors(parser.errors);
            continue;
        }

        println!(
            "{}",
            eval(Node::Program(program), &mut environment).to_string()
        );
    }
}

fn print_parser_errors(errors: Vec<String>) {
    for error in errors {
        println!("{}", error);
    }
}
