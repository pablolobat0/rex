use crate::{
    evaluator::{evaluator::eval, object::object::Environment},
    lexer::lexer::Lexer,
    parser::{ast::ast::Node, parser::Parser},
};
use std::io::{self, Write};

const PROMPT: &str = "> ";

pub fn start() {
    let mut environment = Environment::new();
    loop {
        let mut input = String::new();
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        io::stdin()
            .read_line(&mut input)
            .expect("Error al leer la línea");

        let input = input.trim();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if parser.get_errors().len() != 0 {
            print_parser_errors(parser.get_errors());
            continue;
        }

        if program.get_lexeme() == "exit" {
            return;
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
