use crate::lexer::{lexer::Lexer, token::token::TokenType};
use std::io::{self, Write};

const PROMPT: &str = "> ";

pub fn start() {
    loop {
        let mut input = String::new();
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        io::stdin()
            .read_line(&mut input)
            .expect("Error al leer la línea");

        let input = input.trim();
        let mut lexer = Lexer::new(input);

        loop {
            let token = lexer.next_token();
            if token.kind == TokenType::EOF {
                break;
            } else if token.lexeme == "exit" {
                return;
            }
            println!("{:?}", token);
        }
    }
}
