use crate::lexer::{
    lexer::Lexer,
    token::token::{Token, TokenType},
};

use super::ast::ast::{Expression, Identifier, LetStatement, Program, Statement};

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    peek_token: Token,
    pub errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Parser {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();

        Parser {
            lexer,
            current_token,
            peek_token,
            errors: vec![],
        }
    }

    pub fn get_errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn next_token(&mut self) {
        self.current_token = std::mem::replace(&mut self.peek_token, self.lexer.next_token());
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while self.current_token.kind != TokenType::EOF {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.current_token.kind {
            TokenType::Let => self.parse_let_statement().map(Statement::Let),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        let token: Token = self.current_token.clone();

        if !self.expect_peek(TokenType::Identifier) {
            return None;
        };
        let name = Identifier::new(
            self.current_token.clone(),
            self.current_token.lexeme.clone(),
        );

        if !self.expect_peek(TokenType::Equal) {
            return None;
        }

        let value = self.parse_expression();

        while !self.current_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(LetStatement::new(token, name, value))
    }

    fn expect_peek(&mut self, token: TokenType) -> bool {
        if self.peek_token.kind == token {
            self.next_token(); // Skip token
            true
        } else {
            self.peek_error(token);
            false
        }
    }

    fn current_token_is(&self, token: TokenType) -> bool {
        return self.current_token.kind == token;
    }

    fn parse_expression(&mut self) -> Expression {
        // Parse an expression (dummy implementation for now)
        Expression::Identifier(Identifier::new(
            self.current_token.clone(),
            self.current_token.lexeme.clone(),
        ))
    }

    fn peek_error(&mut self, expected_token: TokenType) {
        let message = format!(
            "Expected next token to be {}, got {} instead",
            expected_token, self.peek_token.kind
        );
        self.errors.push(message);
    }
}
