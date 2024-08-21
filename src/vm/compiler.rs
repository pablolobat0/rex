use std::collections::HashMap;

use crate::common::{
    lexer::{
        lexer::Lexer,
        token::{Token, TokenType},
    },
    precedences::{create_precedences, Precedence},
};

use super::chunk::{Chunk, OpCode, Value};

// Function types for prefix and infix parsing
type PrefixParseFn = fn(&mut Parser);
type InfixParseFn = fn(&mut Parser);

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: &'a mut Lexer<'a>,
    current_token: Token,
    peek_token: Token,
    pub errors: Vec<String>,
    pub current_chunk: Chunk,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
    precedences: HashMap<TokenType, Precedence>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer<'a>) -> Parser<'a> {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();

        let mut parser = Parser {
            lexer,
            current_token,
            peek_token,
            errors: vec![],
            current_chunk: Chunk::new(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
            precedences: create_precedences(),
        };

        // Prefix functions
        parser.register_prefix(TokenType::Integer, number);
        parser.register_prefix(TokenType::Float, number);
        parser.register_prefix(TokenType::True, literal);
        parser.register_prefix(TokenType::False, literal);
        parser.register_prefix(TokenType::Null, literal);
        parser.register_prefix(TokenType::Minus, prefix_expression);
        parser.register_prefix(TokenType::Bang, prefix_expression);
        // Infix functions
        parser.register_infix(TokenType::Plus, infix_expression);
        parser.register_infix(TokenType::Minus, infix_expression);
        parser.register_infix(TokenType::Star, infix_expression);
        parser.register_infix(TokenType::Slash, infix_expression);
        parser.register_infix(TokenType::EqualEqual, infix_expression);
        parser.register_infix(TokenType::BangEqual, infix_expression);
        parser.register_infix(TokenType::Greater, infix_expression);
        parser.register_infix(TokenType::GreaterEqual, infix_expression);
        parser.register_infix(TokenType::Less, infix_expression);
        parser.register_infix(TokenType::LessEqual, infix_expression);

        parser
    }

    fn register_prefix(&mut self, token_type: TokenType, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, func);
    }

    fn register_infix(&mut self, token_type: TokenType, func: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, func);
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
        self.expression(Precedence::Lowest);
        // Check compilation errors
        self.errors.len() == 0
    }

    pub fn emit_bytecode(&mut self, byte: OpCode) {
        self.current_chunk
            .write(byte, self.current_token.line as usize);
    }

    fn expression(&mut self, precedence: Precedence) {
        let prefix_fn = self.prefix_parse_fns.get(&self.current_token.kind);

        match prefix_fn {
            Some(function) => function(self),
            None => self.add_error(
                "Unknow prefix operator".to_string(),
                self.current_token.line,
            ),
        }

        while precedence < self.peek_precedence() {
            // Consume token
            self.next_token();
            let infix_fn = self.infix_parse_fns.get(&self.current_token.kind);
            match infix_fn {
                Some(function) => function(self),
                None => {
                    self.add_error("Unknow infix operator".to_string(), self.current_token.line)
                }
            }
        }
    }

    fn current_precedence(&self) -> Precedence {
        *self
            .precedences
            .get(&self.current_token.kind)
            .unwrap_or(&Precedence::Lowest)
    }

    fn peek_precedence(&self) -> Precedence {
        *self
            .precedences
            .get(&self.peek_token.kind)
            .unwrap_or(&Precedence::Lowest)
    }
}

// Prefix parsing functions

fn number(parser: &mut Parser) {
    let value = Value::Number(
        parser
            .current_token
            .lexeme
            .parse()
            .expect("Not a valid number"),
    );
    let index = parser.current_chunk.add_constant(value);
    parser.emit_bytecode(OpCode::Constant(index));
}

fn literal(parser: &mut Parser) {
    match parser.current_token.kind {
        TokenType::True => parser.emit_bytecode(OpCode::True),
        TokenType::False => parser.emit_bytecode(OpCode::False),
        TokenType::Null => parser.emit_bytecode(OpCode::Null),
        _ => return,
    }
}

fn prefix_expression(parser: &mut Parser) {
    let operator = parser.current_token.kind;
    // Consume current token
    parser.next_token();
    parser.expression(Precedence::Prefix);

    match operator {
        TokenType::Minus => parser.emit_bytecode(OpCode::Negate),
        TokenType::Bang => parser.emit_bytecode(OpCode::Not),
        _ => parser.add_error(
            "Unknow prefix operator".to_string(),
            parser.current_token.line,
        ),
    }
}

// Infix parsing functions

fn infix_expression(parser: &mut Parser) {
    let operator = parser.current_token.kind;
    let precedence = parser.current_precedence();
    // Consume current token
    parser.next_token();

    parser.expression(precedence);

    match operator {
        TokenType::Plus => parser.emit_bytecode(OpCode::Add),
        TokenType::Minus => parser.emit_bytecode(OpCode::Subtract),
        TokenType::Star => parser.emit_bytecode(OpCode::Multiply),
        TokenType::Slash => parser.emit_bytecode(OpCode::Divide),
        TokenType::EqualEqual => parser.emit_bytecode(OpCode::Equal),
        TokenType::BangEqual => parser.emit_bytecode(OpCode::NotEqual),
        TokenType::Less => parser.emit_bytecode(OpCode::Less),
        TokenType::LessEqual => parser.emit_bytecode(OpCode::LessEqual),
        TokenType::Greater => parser.emit_bytecode(OpCode::Greater),
        TokenType::GreaterEqual => parser.emit_bytecode(OpCode::GreaterEqual),
        _ => parser.add_error(
            "Unknow infix operator".to_string(),
            parser.current_token.line,
        ),
    }
}
