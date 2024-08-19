use std::collections::HashMap;

use crate::common::lexer::{lexer::Lexer, token::Token, token::TokenType};

use super::chunk::{Chunk, OpCode, Value};

// Precedence order in parsing
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest,      // default value
    Assigment,   // =
    Equals,      // ==, !=
    LessGreater, // >, <, >=, <=
    Sum,         // +, -
    Product,     // *, /
    Prefix,      // -X, !X
    Call,        // myFunction(X)
}

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: &'a mut Lexer<'a>,
    current_token: Token,
    peek_token: Token,
    pub errors: Vec<String>,
    pub current_chunk: Chunk,
    precedences: HashMap<TokenType, Precedence>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer<'a>) -> Parser<'a> {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();

        Parser {
            lexer,
            current_token,
            peek_token,
            errors: vec![],
            current_chunk: Chunk::new(),
            precedences: create_precedences(),
        }
    }

    // Consumes a token, updating current and peek token
    fn next_token(&mut self) {
        self.current_token = std::mem::replace(&mut self.peek_token, self.lexer.next_token());
    }

    fn expect_peek(&mut self, token: TokenType) -> bool {
        if self.peek_token.kind == token {
            self.next_token(); // Consume token
            true
        } else {
            self.peek_error(token);
            false
        }
    }

    fn peek_error(&mut self, expected_token: TokenType) {
        let message = format!(
            "Expected next token to be {}, got {} with lexeme {} instead",
            expected_token, self.peek_token.kind, self.peek_token.lexeme
        );
        self.add_error(message, self.peek_token.line);
    }

    fn add_error(&mut self, message: String, line: u32) {
        self.errors.push(format!("Line {}: {}", line, message));
    }

    pub fn compile(&mut self) -> bool {
        self.errors.len() == 0
    }

    pub fn emit_bytecode(&mut self, byte: OpCode) {
        self.current_chunk
            .write(byte, self.current_token.line as usize);
    }

    fn number(&mut self) {
        let value: Value = self
            .current_token
            .lexeme
            .parse()
            .expect("Not a valid number");
        let index = self.current_chunk.add_constant(value);
        self.emit_bytecode(OpCode::Constant(index));
    }

    fn prefix_expression(&mut self) {
        let operator = self.current_token.kind;
        self.expression();

        match operator {
            TokenType::Minus => self.emit_bytecode(OpCode::Negate),
            _ => self.add_error(
                "Unknow prefix operator".to_string(),
                self.current_token.line,
            ),
        }
    }

    fn infix_expression(&mut self) {
        let operator = self.current_token.kind;
        self.expression();

        match operator {
            TokenType::Plus => self.emit_bytecode(OpCode::Add),
            TokenType::Minus => self.emit_bytecode(OpCode::Subtract),
            TokenType::Star => self.emit_bytecode(OpCode::Multiply),
            TokenType::Slash => self.emit_bytecode(OpCode::Divide),
            _ => self.add_error("Unknow infix operator".to_string(), self.current_token.line),
        }
    }

    fn expression(&mut self) {}
}

fn create_precedences() -> HashMap<TokenType, Precedence> {
    let mut precedences = HashMap::new();
    precedences.insert(TokenType::EqualEqual, Precedence::Equals);
    precedences.insert(TokenType::BangEqual, Precedence::Equals);
    precedences.insert(TokenType::Greater, Precedence::LessGreater);
    precedences.insert(TokenType::GreaterEqual, Precedence::LessGreater);
    precedences.insert(TokenType::Less, Precedence::LessGreater);
    precedences.insert(TokenType::LessEqual, Precedence::LessGreater);
    precedences.insert(TokenType::Plus, Precedence::Sum);
    precedences.insert(TokenType::Minus, Precedence::Sum);
    precedences.insert(TokenType::Star, Precedence::Product);
    precedences.insert(TokenType::Slash, Precedence::Product);
    precedences.insert(TokenType::LeftParen, Precedence::Call);
    precedences.insert(TokenType::Equal, Precedence::Assigment);

    precedences
}
