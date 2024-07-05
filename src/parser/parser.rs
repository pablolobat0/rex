use crate::lexer::{
    lexer::Lexer,
    token::token::{Token, TokenType},
};
use std::str::FromStr;

use crate::parser::ast::ast::Identifier;

use super::ast::ast::{
    Expression, ExpressionStatement, IntegerLiteral, LetStatement, Program, ReturnStatement,
    Statement,
};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
}

type PrefixParseFn = fn(&mut Parser<'_>) -> Option<Expression>;
type InfixParseFn = fn(&mut Parser, Expression) -> Option<Expression>;

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    peek_token: Token,
    pub errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Parser {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();

        let mut parser = Parser {
            lexer,
            current_token,
            peek_token,
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        parser.register_prefix(TokenType::Identifier, parse_identifier);
        parser.register_prefix(TokenType::Integer, parse_integer_literal);

        parser
    }

    fn register_prefix(&mut self, token_type: TokenType, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, func);
    }

    fn register_infix(&mut self, token_type: TokenType, func: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, func);
    }

    pub fn get_errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn peek_error(&mut self, expected_token: TokenType) {
        let message = format!(
            "Expected next token to be {}, got {} instead",
            expected_token, self.peek_token.kind
        );
        self.errors.push(message);
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
            TokenType::Return => self.parse_return_statement().map(Statement::Return),
            _ => self.parse_expression_statement().map(Statement::Expression),
        }
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        let let_token: Token = self.current_token.clone();

        if !self.expect_peek(TokenType::Identifier) {
            return None;
        };

        let identifier = Identifier::new(
            self.current_token.clone(),
            self.current_token.lexeme.clone(),
        );

        if !self.expect_peek(TokenType::Equal) {
            return None;
        }

        self.next_token(); // Skip =

        let value = self.parse_expression(Precedence::Lowest);

        while !self.current_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(LetStatement::new(let_token, identifier, value?))
    }

    fn parse_return_statement(&mut self) -> Option<ReturnStatement> {
        let token: Token = self.current_token.clone();

        self.next_token(); // skip return

        let value = self.parse_expression(Precedence::Lowest);
        while !self.current_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(ReturnStatement::new(token, value?))
    }

    fn parse_expression_statement(&mut self) -> Option<ExpressionStatement> {
        let expression = self.parse_expression(Precedence::Lowest);
        Some(ExpressionStatement::new(
            self.current_token.clone(),
            expression?,
        ))
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

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let prefix_fn = self.prefix_parse_fns.get(&self.current_token.kind)?;
        let left_exp = prefix_fn(self)?;
        Some(left_exp)
    }
}

pub fn parse_identifier(parser: &mut Parser<'_>) -> Option<Expression> {
    Some(Expression::Identifier(Identifier::new(
        parser.current_token.clone(),
        parser.current_token.lexeme.clone(),
    )))
}

pub fn parse_integer_literal(parser: &mut Parser<'_>) -> Option<Expression> {
    let value = match parser.current_token.lexeme.parse::<i64>() {
        Ok(num) => num,
        Err(_) => {
            parser.errors.push(format!(
                "could not parse {} as integer",
                parser.current_token.lexeme
            ));
            return None;
        }
    };

    Some(Expression::Integer(IntegerLiteral::new(
        parser.current_token.clone(),
        value,
    )))
}
