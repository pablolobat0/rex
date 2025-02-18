use clap::{arg, command, ArgAction};
use interpreter::interpreter_impl::interpret_ast;
use repl::start_ast;
use repl::start_vm;
use std::env;
use std::fs;
use std::path::PathBuf;
use vm::vm_impl::compile_and_run;

mod common;
mod interpreter;
mod repl;
mod vm;

#[derive(Debug)]
enum InterpreterType {
    VM,
    AST,
}

fn main() {
    let matches = command!()
        .arg(
            arg!(-a --ast "Use the AST interpreter instead of the default VM interpreter.")
                .action(ArgAction::SetTrue),
        )
        .arg(
            arg!([file] "The source file to interpret.").value_parser(clap::value_parser!(PathBuf)),
        )
        .get_matches();

    // Determine which interpreter to use
    let interpreter_type = if matches.get_flag("ast") {
        InterpreterType::AST
    } else {
        InterpreterType::VM
    };

    // Determine if a file was provided
    if let Some(file) = matches.get_one::<PathBuf>("file") {
        // Read the file content
        let code = fs::read_to_string(file).expect("Failed to read the file");
        match interpreter_type {
            InterpreterType::VM => compile_and_run(code),
            InterpreterType::AST => interpret_ast(code),
        }
    } else {
        match interpreter_type {
            InterpreterType::VM => start_vm(),
            InterpreterType::AST => start_ast(),
        }
    }
}
