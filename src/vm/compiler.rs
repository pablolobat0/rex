use std::{cell::RefCell, collections::HashMap, rc::Rc, usize};

use crate::common::{
    lexer::{
        lexer_impl::Lexer,
        token::{Token, TokenType},
    },
    precedences::{create_precedences, Precedence},
};

use super::{
    chunk::{Chunk, OpCode, Value},
    object::{Function, FunctionType, Upvalue},
    vm_impl::InterpretResult,
};
use crate::vm::scope::Scope;

// Function types for prefix and infix parsing
type PrefixParseFn = fn(&mut Compiler);
type InfixParseFn = fn(&mut Compiler);

#[derive(Debug, Clone)]
pub struct Compiler<'a> {
    enclosing: Option<Rc<RefCell<Compiler<'a>>>>,
    lexer: Rc<RefCell<Lexer<'a>>>,
    current_token: Option<Token>,
    peek_token: Option<Token>,
    pub errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
    precedences: HashMap<TokenType, Precedence>,
    current_scope: Scope,
    function_type: FunctionType,
    pub function: Function,
}

impl<'a> Compiler<'a> {
    pub fn new(lexer: Rc<RefCell<Lexer<'a>>>, function_type: FunctionType) -> Compiler<'a> {
        let mut compiler = Compiler {
            enclosing: None,
            lexer,
            current_token: None,
            peek_token: None,
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
            precedences: create_precedences(),
            current_scope: Scope::new(),
            function_type,
            function: Function::new(),
        };

        if let FunctionType::Function(name) = &compiler.function_type {
            compiler.function.name.clone_from(name);
        }

        // Reserve first local for vm use
        compiler.current_scope.add_local(Token {
            kind: TokenType::Default,
            lexeme: String::new(),
            line: 0,
        });

        compiler.register_prefix_functions();
        compiler.register_infix_functions();

        compiler
    }
    pub fn new_new(
        enclosing: Option<Rc<RefCell<Compiler<'a>>>>,
        lexer: Rc<RefCell<Lexer<'a>>>,
        function_type: FunctionType,
    ) -> Compiler<'a> {
        let mut compiler = Compiler {
            enclosing,
            lexer,
            current_token: None,
            peek_token: None,
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
            precedences: create_precedences(),
            current_scope: Scope::new(),
            function_type,
            function: Function::new(),
        };

        if let FunctionType::Function(name) = &compiler.function_type {
            compiler.function.name.clone_from(name);
        }

        // Reserve first local for vm use
        compiler.current_scope.add_local(Token {
            kind: TokenType::Default,
            lexeme: String::new(),
            line: 0,
        });

        compiler.register_prefix_functions();
        compiler.register_infix_functions();

        compiler
    }

    fn register_prefix_functions(&mut self) {
        self.prefix_parse_fns
            .insert(TokenType::Identifier, identifier);
        self.prefix_parse_fns.insert(TokenType::Integer, number);
        self.prefix_parse_fns.insert(TokenType::Float, number);
        self.prefix_parse_fns.insert(TokenType::String, literal);
        self.prefix_parse_fns.insert(TokenType::True, literal);
        self.prefix_parse_fns.insert(TokenType::False, literal);
        self.prefix_parse_fns.insert(TokenType::Null, literal);
        self.prefix_parse_fns
            .insert(TokenType::Minus, prefix_expression);
        self.prefix_parse_fns
            .insert(TokenType::Bang, prefix_expression);
    }

    fn register_infix_functions(&mut self) {
        self.infix_parse_fns
            .insert(TokenType::Plus, infix_expression);
        self.infix_parse_fns
            .insert(TokenType::Minus, infix_expression);
        self.infix_parse_fns
            .insert(TokenType::Star, infix_expression);
        self.infix_parse_fns
            .insert(TokenType::Slash, infix_expression);
        self.infix_parse_fns
            .insert(TokenType::EqualEqual, infix_expression);
        self.infix_parse_fns
            .insert(TokenType::BangEqual, infix_expression);
        self.infix_parse_fns
            .insert(TokenType::Greater, infix_expression);
        self.infix_parse_fns
            .insert(TokenType::GreaterEqual, infix_expression);
        self.infix_parse_fns
            .insert(TokenType::Less, infix_expression);
        self.infix_parse_fns
            .insert(TokenType::LessEqual, infix_expression);
        self.infix_parse_fns
            .insert(TokenType::LeftParen, call_expression);
    }

    // Consumes a token, updating current and peek token
    fn next_token(&mut self) {
        self.current_token = self.peek_token.take();
        self.peek_token = Some(self.lexer.borrow_mut().next_token());
    }

    fn expect_peek(&mut self, token: TokenType) -> bool {
        if self.peek_token_is(token) {
            self.next_token(); // Consume token
            true
        } else {
            self.peek_error(token);
            false
        }
    }

    fn current_token_is(&self, token: TokenType) -> bool {
        self.current_token.as_ref().map(|t| t.kind) == Some(token)
    }

    fn peek_token_is(&self, token: TokenType) -> bool {
        self.peek_token.as_ref().map(|t| t.kind) == Some(token)
    }

    fn current_token_kind(&self) -> TokenType {
        self.current_token
            .as_ref()
            .map(|t| t.kind)
            .unwrap_or(TokenType::Error)
    }

    fn current_token_line(&self) -> u32 {
        self.current_token.as_ref().map(|t| t.line).unwrap_or(0)
    }

    fn current_token_lexeme(&self) -> String {
        self.current_token
            .as_ref()
            .map(|t| t.lexeme.clone())
            .unwrap_or("".to_string())
    }

    fn peek_error(&mut self, expected_token: TokenType) {
        let message = format!(
            "Expected next token to be {}, got {} with lexeme {} instead",
            expected_token,
            self.peek_token
                .as_ref()
                .map(|t| t.kind)
                .unwrap_or(TokenType::Error),
            self.peek_token
                .as_ref()
                .map(|t| t.lexeme.clone())
                .unwrap_or("".to_string())
        );
        self.add_error(
            message,
            self.peek_token.as_ref().map(|t| t.line).unwrap_or(0),
        );
    }

    fn current_error(&mut self, message: &str) {
        let error = format!(
            "{} {} with lexeme {}",
            message,
            self.current_token_kind(),
            self.current_token
                .as_ref()
                .map(|t| t.lexeme.clone())
                .unwrap_or_default()
        );
        self.add_error(error, self.peek_token.as_ref().map(|t| t.line).unwrap_or(0));
    }

    fn add_error(&mut self, message: String, line: u32) {
        self.errors.push(format!("Line {}: {}", line, message));
    }

    pub fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.function.chunk
    }

    pub fn compile(&mut self) -> InterpretResult {
        // Initialize current and peek token
        self.next_token();
        self.next_token();

        while !self.current_token_is(TokenType::EOF) {
            self.statement();
            self.next_token();
        }

        self.end_compiler()
    }

    fn end_compiler(&mut self) -> InterpretResult {
        // Implicit return
        self.emit_return();

        // Check compilation errors
        if self.errors.is_empty() {
            InterpretResult::Ok
        } else {
            InterpretResult::CompileError
        }
    }

    fn statement(&mut self) {
        match self.current_token_kind() {
            TokenType::Let => self.let_statement(),
            TokenType::Function => self.function_declaration(),
            TokenType::Return => self.return_statement(),
            TokenType::LeftBrace => self.block(),
            TokenType::If => self.if_statement(),
            TokenType::NewLine => (),
            TokenType::While => self.while_statement(),
            _ => self.expression_statement(),
        }
    }

    fn let_statement(&mut self) {
        if !self.expect_peek(TokenType::Identifier) {
            return;
        }

        let index = self.prepare_variable();

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

        self.finalize_variable(index);
    }

    fn prepare_variable(&mut self) -> Option<usize> {
        let lexeme = self.current_token_lexeme();
        let index = if self.current_scope.depth == 0 {
            Some(self.current_chunk().add_constant(Value::String(lexeme)))
        } else {
            self.declare_local_variable();
            None
        };

        index
    }

    fn finalize_variable(&mut self, index: Option<usize>) {
        if let Some(index) = index {
            self.emit_bytecode(OpCode::DefineGlobal(index));
        } else {
            self.remove_uninitialized_mark();
        }
    }

    fn remove_uninitialized_mark(&mut self) {
        if let Some(last) = self.current_scope.locals.last_mut() {
            last.depth = self.current_scope.depth;
        }
    }

    fn declare_local_variable(&mut self) {
        let has_existing_variable = self.current_scope.locals.iter().rev().any(|local| {
            local.depth == self.current_scope.depth
                && local.name.lexeme == self.current_token_lexeme()
        });

        if has_existing_variable {
            self.add_error(
                "Already a variable with this name in this scope.".to_string(),
                self.current_token_line(),
            );
        }

        if let Some(token) = self.current_token.take() {
            self.current_scope.add_local(token);
        };
    }

    fn function_declaration(&mut self) {
        self.expect_peek(TokenType::Identifier);

        let define_function_index = self.prepare_variable();

        if define_function_index.is_none() {
            self.remove_uninitialized_mark();
        }

        // New compiler for the function
        let lexeme = self.current_token_lexeme();
        let enclosing = Some(Rc::new(RefCell::new(self.clone())));
        let mut compiler = Compiler::new_new(
            enclosing,
            self.lexer.clone(),
            FunctionType::Function(lexeme),
        );
        // Initialize current and peek token
        compiler.current_token = self.current_token.take();
        compiler.peek_token = self.peek_token.take();

        compiler.parse_parameters();

        if !compiler.expect_peek(TokenType::LeftBrace) {
            return;
        }

        compiler.block();

        compiler.end_compiler();

        let execute_function_index = self
            .current_chunk()
            .add_constant(Value::Function(compiler.function.clone()));
        self.emit_bytecode(OpCode::Closure(execute_function_index));

        for upvalue in compiler.function.upvalues {
            if upvalue.is_local {
                self.emit_bytecode(OpCode::True);
            } else {
                self.emit_bytecode(OpCode::False);
            }

            self.emit_bytecode(OpCode::Constant(upvalue.index));
        }

        // Initialize current and peek token
        self.current_token = compiler.current_token.take();
        self.peek_token = compiler.peek_token.take();

        self.finalize_variable(define_function_index);
    }

    fn parse_parameters(&mut self) {
        // Unclosed scope because it ends when compiler ends
        self.current_scope.begin_scope();

        if !self.expect_peek(TokenType::LeftParen) {
            return;
        }

        // Function without parameters
        if self.peek_token_is(TokenType::RightParen) {
            self.next_token();
            return;
        }

        if !self.expect_peek(TokenType::Identifier) {
            return;
        }

        self.function.arity += 1;
        self.parse_parameter();

        while !self.peek_token_is(TokenType::RightParen) {
            if !self.expect_peek(TokenType::Comma) {
                return;
            }
            if !self.expect_peek(TokenType::Identifier) {
                return;
            }
            self.function.arity += 1;
            self.parse_parameter();
        }

        self.next_token();
    }

    fn parse_parameter(&mut self) {
        let index = self.prepare_variable();

        if index.is_none() {
            self.remove_uninitialized_mark();
        }
    }

    fn return_statement(&mut self) {
        // Consume return
        self.next_token();

        self.expression(Precedence::Lowest);

        self.emit_bytecode(OpCode::Return);
        self.parse_end_statement();
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
    }

    fn if_statement(&mut self) {
        // Consume 'if'
        self.next_token();

        // Parse the condition
        self.expression(Precedence::Lowest);

        // Emit the conditional jump
        let then_jump = self.current_chunk().code.len();
        self.emit_bytecode(OpCode::JumpIfFalse(0));

        self.emit_bytecode(OpCode::Pop);

        // Consume condition
        self.next_token();

        // Parse the 'then' branch
        self.statement();

        // Emit the else jump
        let else_jump = self.current_chunk().code.len();
        self.emit_bytecode(OpCode::Jump(0));

        // Patch the jump to point to the code after the 'then' branch
        self.patch_jump(then_jump);

        self.emit_bytecode(OpCode::Pop);

        if self.peek_token_is(TokenType::Else) {
            // Consume else
            self.next_token();

            if self.expect_peek(TokenType::LeftBrace) {
                self.block();
            }
        }

        // Patch the jump to point to the code after the 'else' branch
        self.patch_jump(else_jump);
    }

    fn patch_jump(&mut self, jump_offset: usize) {
        // Calcula el valor del salto, ajustado por los bytes de la instrucción de salto en sí.
        let jump = self.current_chunk().code.len() - jump_offset - 1;

        if let OpCode::JumpIfFalse(ref mut target) = self.current_chunk().code[jump_offset] {
            *target = jump;
        } else if let OpCode::Jump(ref mut target) = self.current_chunk().code[jump_offset] {
            *target = jump;
        } else {
            panic!("Expected a jump instruction at the given offset.");
        }
    }

    fn while_statement(&mut self) {
        // Consume while
        self.next_token();

        // Consume condition
        self.expression(Precedence::Lowest);

        // Emit the conditional jump
        let while_jump = self.current_chunk().code.len();
        self.emit_bytecode(OpCode::JumpIfFalse(0));

        self.emit_bytecode(OpCode::Pop);

        self.next_token();

        // Consume loop body
        self.statement();

        let last = self.current_chunk().code.len() - 1;
        self.emit_bytecode(OpCode::Loop(last));
        self.emit_bytecode(OpCode::Pop);

        // Patch the jump to point to the code after the while loop
        self.patch_jump(while_jump);
    }

    fn parse_end_statement(&mut self) {
        if !self.peek_token_is(TokenType::NewLine) && !self.peek_token_is(TokenType::EOF) {
            self.peek_error(TokenType::NewLine);
        } else {
            self.next_token();
        }
    }

    pub fn emit_bytecode(&mut self, byte: OpCode) {
        let line = self.current_token_line() as usize;
        self.current_chunk().write(byte, line);
    }

    fn emit_return(&mut self) {
        let line = self.current_token_line() as usize;
        self.current_chunk().write(OpCode::Null, line);
        self.current_chunk().write(OpCode::Return, line);
    }

    fn expression_statement(&mut self) {
        self.expression(Precedence::Lowest);
        self.parse_end_statement();
        self.emit_bytecode(OpCode::Pop);
    }

    fn expression(&mut self, precedence: Precedence) {
        let Some(prefix_fn) = self.prefix_parse_fns.get(&self.current_token_kind()) else {
            self.current_error("Unknow prefix operator ");
            return;
        };

        prefix_fn(self);

        while precedence < self.peek_precedence() {
            // Consume token
            self.next_token();
            let Some(infix_fn) = self.infix_parse_fns.get(&self.current_token_kind()) else {
                self.current_error("Unknow infix operator");
                return;
            };

            infix_fn(self);
        }
    }

    fn current_precedence(&self) -> Precedence {
        if let Some(token) = &self.current_token {
            *self
                .precedences
                .get(&token.kind)
                .unwrap_or(&Precedence::Lowest)
        } else {
            Precedence::Lowest
        }
    }

    fn peek_precedence(&self) -> Precedence {
        if let Some(token) = &self.peek_token {
            *self
                .precedences
                .get(&token.kind)
                .unwrap_or(&Precedence::Lowest)
        } else {
            Precedence::Lowest
        }
    }

    fn resolve_upvalue(&mut self, token: &Token) -> Option<usize> {
        // Search the upvalue in the enclosing
        let local = self
            .enclosing
            .as_ref()
            .and_then(|enclosing| enclosing.borrow().current_scope.resolve_local(token));

        if let Some(index) = local {
            return Some(self.add_upvalue(index, true));
        }

        // If it is not in the enclosing we have to search it in a recursive way
        let no_local = self
            .enclosing
            .as_ref()
            .and_then(|enclosing| enclosing.borrow_mut().resolve_upvalue(token));

        if let Some(index) = no_local {
            return Some(self.add_upvalue(index, false));
        }

        None
    }

    fn add_upvalue(&mut self, index: usize, is_local: bool) -> usize {
        // Before pushing check if it has already been pushed
        for (i, upvalue) in self.function.upvalues.iter().enumerate() {
            if upvalue.index == index && upvalue.is_local == is_local {
                return i;
            }
        }
        self.function.upvalues.push(Upvalue { index, is_local });

        return self.function.upvalues.len() - 1;
    }

    // Debug functions

    pub fn compile_one_statement(&mut self) -> bool {
        // Initialize current and peek token
        self.next_token();
        self.next_token();
        self.one_statement();
        self.errors.is_empty()
    }

    fn one_statement(&mut self) {
        match self.current_token_kind() {
            TokenType::Let => self.let_statement(),
            TokenType::LeftBrace => self.block(),
            TokenType::If => self.if_statement(),
            _ => self.expression(Precedence::Lowest),
        }
    }
}

// Prefix parsing functions

fn identifier(compiler: &mut Compiler) {
    let get_op;
    let set_op;

    let Some(token) = compiler.current_token.take() else {
        return;
    };

    if let Some(position) = compiler.current_scope.resolve_local(&token) {
        get_op = OpCode::GetLocal(position);
        set_op = OpCode::SetLocal(position);
    } else if let Some(position) = compiler.resolve_upvalue(&token) {
        get_op = OpCode::GetUpvalue(position);
        set_op = OpCode::SetUpvalue(position);
    } else {
        let index = compiler
            .current_chunk()
            .add_constant(Value::String(token.lexeme));
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
            .current_token_lexeme()
            .parse()
            .expect("Not a valid number"),
    );
    let index = compiler.current_chunk().add_constant(value);
    compiler.emit_bytecode(OpCode::Constant(index));
}

fn literal(compiler: &mut Compiler) {
    match compiler.current_token_kind() {
        TokenType::String => {
            let lexeme = compiler.current_token_lexeme();
            let index = compiler.current_chunk().add_constant(Value::String(lexeme));
            compiler.emit_bytecode(OpCode::Constant(index));
        }
        TokenType::True => compiler.emit_bytecode(OpCode::True),
        TokenType::False => compiler.emit_bytecode(OpCode::False),
        TokenType::Null => compiler.emit_bytecode(OpCode::Null),
        _ => (),
    }
}

fn prefix_expression(compiler: &mut Compiler) {
    let operator = compiler.current_token_kind();
    // Consume current token
    compiler.next_token();
    compiler.expression(Precedence::Prefix);

    match operator {
        TokenType::Minus => compiler.emit_bytecode(OpCode::Negate),
        TokenType::Bang => compiler.emit_bytecode(OpCode::Not),
        _ => compiler.current_error("Unknow prefix operator"),
    }
}

// Infix parsing functions

fn infix_expression(compiler: &mut Compiler) {
    let operator = compiler.current_token_kind();
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
        _ => compiler.current_error("Unknow prefix operator"),
    }
}

fn call_expression(compiler: &mut Compiler) {
    let mut arguments = 0;
    if compiler.peek_token_is(TokenType::RightParen) {
        compiler.emit_bytecode(OpCode::Call(0));
        //Consume left paren
        compiler.next_token();
        return;
    }

    //Consume left paren
    compiler.next_token();

    arguments += 1;
    compiler.expression(Precedence::Lowest);

    while !compiler.peek_token_is(TokenType::RightParen) {
        if !compiler.expect_peek(TokenType::Comma) {
            return;
        }
        // Consume comma
        compiler.next_token();

        compiler.expression(Precedence::Lowest);
        arguments += 1;
    }
    compiler.next_token();
    compiler.emit_bytecode(OpCode::Call(arguments));
}
