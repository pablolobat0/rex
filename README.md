# Custom Language Interpreter

This repository contains an interpreter for a custom programming language written in Rust. 

The objective of this project is to create a simple toy interpreter with learning purposes. This language serves as an educational tool to learn the concepts of lexing, parsing, and evaluation in a programming language.

This project is based in the books [Writing An Interpreter in Go](https://interpreterbook.com/) and [Crafting Interpreters](https://craftinginterpreters.com/)

## Features

- **Arithmetic Operations**: Supports addition, subtraction, multiplication, and division.
- **Boolean Expressions**: Supports boolean literals and logical operations.
- **Conditionals**: Includes `if-else` expressions.
- **Functions**: Allows definition and invocation of user-defined functions.
- **Variables**: Supports variable declarations and scope handling.
- **String Manipulation**: Basic string operations including concatenation.
- **Tree-Walking Interpreter:** Traverses the abstract syntax tree (AST) directly, evaluating nodes as it encounters them.

### Prerequisites

- Rust: Ensure you have the latest stable version of Rust installed. You can download it from [rust-lang.org](https://www.rust-lang.org/).

### Running 
   ```sh
   git clone https://github.com/pablolobat0/interpreter.git
   cd interpreter
   cargo run
   ```

   ### TODO
   - [] **Virtual Machine Interpreter:** Right now the current version uses a tree-walking interpreter, which is not the most efficient way. A Virtual Machine Interpreter interprets bytecode and uses a stack instead of walking around a tree of objects, wich makes it faster and more efficient.

