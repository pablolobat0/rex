use repl::start;
use std::env;

mod lexer;
mod repl;

fn main() {
    let username = match env::var("USER") {
        Ok(val) => val,
        Err(_) => match env::var("USERNAME") {
            Ok(val) => val,
            Err(_) => "Usuario desconocido".to_string(),
        },
    };
    println!("Hola, {}! Bienvenido al intérprete.", username);

    start();
}
