use crate::{
    common::lexer::lexer::Lexer,
    interpreter::{
        evaluator::{evaluator::eval, object::Environment},
        parser::{ast::Node, parser::Parser},
    },
    vm::{
        compiler::Compiler,
        vm::{InterpretResult, VirtualMachine},
    },
};
use std::io::{self, Write};
use std::{collections::HashMap, env};

const PROMPT: &str = "> ";
const EXIT_COMMAND: &str = "exit";

fn greetings() {
    let username = match env::var("USER") {
        Ok(val) => val,
        Err(_) => match env::var("USERNAME") {
            Ok(val) => val,
            Err(_) => "Usuario desconocido".to_string(),
        },
    };
    println!("Hola, {}! Bienvenido al intérprete.", username);
}

pub fn start_ast() {
    greetings();

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

        if parser.errors.len() != 0 {
            print_parser_errors(parser.errors);
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

pub fn start_vm() {
    greetings();

    // Empty globals
    let mut globals = HashMap::new();

    loop {
        let mut input = String::new();
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        io::stdin()
            .read_line(&mut input)
            .expect("error reading line");

        let mut lexer = Lexer::new(&input);
        let mut compiler = Compiler::new(&mut lexer);

        if !compiler.compile_one_statement() {
            print_parser_errors(compiler.errors);
            continue;
        }

        let mut vm = VirtualMachine::new_with_globals(&mut compiler, globals.clone());

        // Run the input
        if vm.interpret() == InterpretResult::Ok {
            // Update with current globals
            globals = vm.globals;
            match vm.stack.last() {
                Some(value) => print!("{}\n", value),
                None => {}
            }
        }
    }
}

fn print_parser_errors(errors: Vec<String>) {
    for error in errors {
        println!("{}", error);
    }
}
