use crate::{
    common::lexer::lexer::Lexer,
    interpreter::evaluator::{evaluator::eval, object::Environment},
    interpreter::parser::{ast::Node, parser::Parser},
};
use std::env;
use std::io::{self, Write};

const PROMPT: &str = "> ";
const EXIT_COMMAND: &str = "exit";

pub fn start_ast() {
    let username = match env::var("USER") {
        Ok(val) => val,
        Err(_) => match env::var("USERNAME") {
            Ok(val) => val,
            Err(_) => "Usuario desconocido".to_string(),
        },
    };
    println!("Hola, {}! Bienvenido al intérprete.", username);

    let mut environment = Environment::new();
    loop {
        let mut input = String::new();
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        io::stdin()
            .read_line(&mut input)
            .expect("Error reading line");

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if parser.get_errors().len() != 0 {
            print_parser_errors(parser.get_errors());
            continue;
        }

        if program.get_lexeme() == EXIT_COMMAND {
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
