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
type PrefixParseFn = fn(&mut Compiler);
type InfixParseFn = fn(&mut Compiler);

#[derive(Debug)]
struct Local {
    name: Token,
    depth: i32,
}

#[derive(Debug)]
struct Scope {
    locals: Vec<Local>,
    depth: i32,
}

impl Scope {
    fn new() -> Scope {
        Scope {
            locals: vec![],
            depth: 0,
        }
    }

    fn begin_scope(&mut self) {
        self.depth += 1;
    }

    fn end_scope(&mut self) -> u32 {
        self.depth -= 1;

        // Remove variables that are out of the current scope
        let mut counter = 0;
        self.locals.retain(|local| {
            if local.depth > self.depth {
                counter += 1;
                false
            } else {
                true
            }
        });

        counter
    }

    fn add_local(&mut self, name: Token) {
        let local = Local {
            name,
            depth: -1, // Mark uninitialized
        };
        self.locals.push(local);
    }

    fn resolve_local(&self, name: &Token) -> Option<usize> {
        // Check name and if it's initialized
        self.locals
            .iter()
            .rev()
            .position(|local| local.name.lexeme == *name.lexeme && local.depth > -1)
            .map(|rev_index| self.locals.len() - 1 - rev_index)
    }
}

#[derive(Debug)]
pub struct Compiler<'a> {
    lexer: &'a mut Lexer<'a>,
    current_token: Token,
    peek_token: Token,
    pub errors: Vec<String>,
    pub current_chunk: Chunk,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
    precedences: HashMap<TokenType, Precedence>,
    current_scope: Scope,
}

impl<'a> Compiler<'a> {
    pub fn new(lexer: &'a mut Lexer<'a>) -> Compiler<'a> {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();

        let mut compiler = Compiler {
            lexer,
            current_token,
            peek_token,
            errors: vec![],
            current_chunk: Chunk::new(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
            precedences: create_precedences(),
            current_scope: Scope::new(),
        };

        // Prefix functions
        compiler.register_prefix(TokenType::Identifier, identifier);
        compiler.register_prefix(TokenType::Integer, number);
        compiler.register_prefix(TokenType::Float, number);
        compiler.register_prefix(TokenType::String, literal);
        compiler.register_prefix(TokenType::True, literal);
        compiler.register_prefix(TokenType::False, literal);
        compiler.register_prefix(TokenType::Null, literal);
        compiler.register_prefix(TokenType::Minus, prefix_expression);
        compiler.register_prefix(TokenType::Bang, prefix_expression);
        // Infix functions
        compiler.register_infix(TokenType::Plus, infix_expression);
        compiler.register_infix(TokenType::Minus, infix_expression);
        compiler.register_infix(TokenType::Star, infix_expression);
        compiler.register_infix(TokenType::Slash, infix_expression);
        compiler.register_infix(TokenType::EqualEqual, infix_expression);
        compiler.register_infix(TokenType::BangEqual, infix_expression);
        compiler.register_infix(TokenType::Greater, infix_expression);
        compiler.register_infix(TokenType::GreaterEqual, infix_expression);
        compiler.register_infix(TokenType::Less, infix_expression);
        compiler.register_infix(TokenType::LessEqual, infix_expression);

        compiler
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

    fn current_token_is(&self, token: TokenType) -> bool {
        return self.current_token.kind == token;
    }

    fn peek_token_is(&self, token: TokenType) -> bool {
        return self.peek_token.kind == token;
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

    pub fn compile_one_statement(&mut self) -> bool {
        self.one_statement();
        self.errors.len() == 0
    }

    fn one_statement(&mut self) {
        match self.current_token.kind {
            TokenType::Let => self.let_statement(),
            TokenType::LeftBrace => self.block(),
            _ => self.expression(Precedence::Lowest),
        }
    }

    pub fn compile(&mut self) -> bool {
        while self.current_token.kind != TokenType::EOF {
            self.statement();
            self.next_token();
        }
        // Check compilation errors
        self.errors.len() == 0
    }

    fn statement(&mut self) {
        match self.current_token.kind {
            TokenType::Let => self.let_statement(),
            TokenType::LeftBrace => self.block(),
            _ => self.expression_statement(),
        }
    }

    fn let_statement(&mut self) {
        if !self.expect_peek(TokenType::Identifier) {
            return;
        }

        self.declare_variable();

        let index = if self.current_scope.depth == 0 {
            Some(
                self.current_chunk
                    .add_constant(Value::String(self.current_token.lexeme.clone())),
            )
        } else {
            None
        };

        if self.peek_token_is(TokenType::Equal) {
            // Consume identifier
            self.next_token();
            // Consume =
            self.next_token();

            self.expression(Precedence::Lowest);
        } else {
            self.emit_bytecode(OpCode::Null);
        }

        self.parse_end_statement();

        if let Some(index) = index {
            self.emit_bytecode(OpCode::DefineGlobal(index));
        } else {
            // Remove uninitialized mark
            let last_local_index = self.current_scope.locals.len() - 1;
            self.current_scope.locals[last_local_index].depth = self.current_scope.depth;
        }
    }

    fn declare_variable(&mut self) {
        if self.current_scope.depth == 0 {
            return;
        }

        let has_existing_variable = self.current_scope.locals.iter().rev().any(|local| {
            local.depth == self.current_scope.depth
                && local.name.lexeme == self.current_token.lexeme
        });

        if has_existing_variable {
            self.add_error(
                "Already a variable with this name in this scope.".to_string(),
                self.current_token.line,
            );
        }

        self.current_scope.add_local(self.current_token.clone());
    }

    fn block(&mut self) {
        self.expect_peek(TokenType::NewLine);
        // Consume new line
        self.next_token();

        self.current_scope.begin_scope();

        // Parse all the statements in current block
        while !self.current_token_is(TokenType::RightBrace)
            && !self.current_token_is(TokenType::EOF)
        {
            self.statement();
            self.next_token();
        }

        let counter = self.current_scope.end_scope();
        for _ in 0..counter {
            self.emit_bytecode(OpCode::Pop);
        }

        self.parse_end_statement();
    }

    fn parse_end_statement(&mut self) {
        if !self.peek_token_is(TokenType::NewLine) && !self.peek_token_is(TokenType::EOF) {
            self.add_error(
                format!(
                    "Wrong expression end, expected new line or EOF, got: {}",
                    self.peek_token.lexeme
                ),
                self.peek_token.line,
            );
        } else {
            self.next_token();
        }
    }

    pub fn emit_bytecode(&mut self, byte: OpCode) {
        self.current_chunk
            .write(byte, self.current_token.line as usize);
    }
    fn expression_statement(&mut self) {
        self.expression(Precedence::Lowest);
        self.parse_end_statement();
        self.emit_bytecode(OpCode::Pop);
    }

    fn expression(&mut self, precedence: Precedence) {
        let prefix_fn = self.prefix_parse_fns.get(&self.current_token.kind);

        match prefix_fn {
            Some(function) => function(self),
            None => self.add_error(
                format!("Unknow prefix operator {}", self.current_token.lexeme),
                self.current_token.line,
            ),
        }

        while precedence < self.peek_precedence() {
            // Consume token
            self.next_token();
            let infix_fn = self.infix_parse_fns.get(&self.current_token.kind);
            match infix_fn {
                Some(function) => function(self),
                None => self.add_error(
                    format!("Unknow infix operator {}", self.current_token.lexeme),
                    self.current_token.line,
                ),
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

fn identifier(compiler: &mut Compiler) {
    let get_op;
    let set_op;

    if let Some(position) = compiler
        .current_scope
        .resolve_local(&compiler.current_token)
    {
        get_op = OpCode::GetLocal(position);
        set_op = OpCode::SetLocal(position);
    } else {
        let index = compiler
            .current_chunk
            .add_constant(Value::String(compiler.current_token.lexeme.clone()));
        get_op = OpCode::GetGlobal(index);
        set_op = OpCode::SetGlobal(index);
    }

    if compiler.peek_token_is(TokenType::Equal) {
        // Consume Identifier
        compiler.next_token();

        // Consume =
        compiler.next_token();

        compiler.expression(Precedence::Assigment);
        compiler.emit_bytecode(set_op);
    } else {
        compiler.emit_bytecode(get_op);
    }
}

fn number(compiler: &mut Compiler) {
    let value = Value::Number(
        compiler
            .current_token
            .lexeme
            .parse()
            .expect("Not a valid number"),
    );
    let index = compiler.current_chunk.add_constant(value);
    compiler.emit_bytecode(OpCode::Constant(index));
}

fn literal(compiler: &mut Compiler) {
    match compiler.current_token.kind {
        TokenType::String => {
            let index = compiler
                .current_chunk
                .add_constant(Value::String(compiler.current_token.lexeme.clone()));
            compiler.emit_bytecode(OpCode::Constant(index));
        }
        TokenType::True => compiler.emit_bytecode(OpCode::True),
        TokenType::False => compiler.emit_bytecode(OpCode::False),
        TokenType::Null => compiler.emit_bytecode(OpCode::Null),
        _ => return,
    }
}

fn prefix_expression(compiler: &mut Compiler) {
    let operator = compiler.current_token.kind;
    // Consume current token
    compiler.next_token();
    compiler.expression(Precedence::Prefix);

    match operator {
        TokenType::Minus => compiler.emit_bytecode(OpCode::Negate),
        TokenType::Bang => compiler.emit_bytecode(OpCode::Not),
        _ => compiler.add_error(
            format!("Unknow prefix operator {}", compiler.current_token.lexeme),
            compiler.current_token.line,
        ),
    }
}

// Infix parsing functions

fn infix_expression(compiler: &mut Compiler) {
    let operator = compiler.current_token.kind;
    let precedence = compiler.current_precedence();
    // Consume current token
    compiler.next_token();

    compiler.expression(precedence);

    match operator {
        TokenType::Plus => compiler.emit_bytecode(OpCode::Add),
        TokenType::Minus => compiler.emit_bytecode(OpCode::Subtract),
        TokenType::Star => compiler.emit_bytecode(OpCode::Multiply),
        TokenType::Slash => compiler.emit_bytecode(OpCode::Divide),
        TokenType::EqualEqual => compiler.emit_bytecode(OpCode::Equal),
        TokenType::BangEqual => compiler.emit_bytecode(OpCode::NotEqual),
        TokenType::Less => compiler.emit_bytecode(OpCode::Less),
        TokenType::LessEqual => compiler.emit_bytecode(OpCode::LessEqual),
        TokenType::Greater => compiler.emit_bytecode(OpCode::Greater),
        TokenType::GreaterEqual => compiler.emit_bytecode(OpCode::GreaterEqual),
        _ => compiler.add_error(
            format!("Unknow prefix operator {}", compiler.current_token.lexeme),
            compiler.current_token.line,
        ),
    }
}
