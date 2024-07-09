use crate::lexer::{
    lexer::Lexer,
    token::token::{Token, TokenType},
};

use crate::parser::ast::ast::Identifier;

use super::ast::ast::{
    BlockStatement, BooleanLiteral, Expression, ExpressionStatement, FunctionLiteral, IfExpression,
    InfixExpression, IntegerLiteral, LetStatement, PrefixExpression, Program, ReturnStatement,
    Statement,
};
use std::collections::HashMap;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
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
    precedences: HashMap<TokenType, Precedence>,
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
            precedences: create_precedences(),
        };

        parser.register_prefix(TokenType::Identifier, parse_identifier);
        parser.register_prefix(TokenType::Integer, parse_integer_literal);
        parser.register_prefix(TokenType::True, parse_boolean_literal);
        parser.register_prefix(TokenType::False, parse_boolean_literal);
        parser.register_prefix(TokenType::Minus, parse_prefix_expression);
        parser.register_prefix(TokenType::Bang, parse_prefix_expression);
        parser.register_prefix(TokenType::LeftParen, parse_grouped_expression);
        parser.register_prefix(TokenType::If, parse_if_expression);
        parser.register_prefix(TokenType::Function, parse_function_literal);

        parser.register_infix(TokenType::Plus, parse_infix_expression);
        parser.register_infix(TokenType::Minus, parse_infix_expression);
        parser.register_infix(TokenType::Star, parse_infix_expression);
        parser.register_infix(TokenType::Slash, parse_infix_expression);
        parser.register_infix(TokenType::EqualEqual, parse_infix_expression);
        parser.register_infix(TokenType::BangEqual, parse_infix_expression);
        parser.register_infix(TokenType::Greater, parse_infix_expression);
        parser.register_infix(TokenType::GreaterEqual, parse_infix_expression);
        parser.register_infix(TokenType::Less, parse_infix_expression);
        parser.register_infix(TokenType::LessEqual, parse_infix_expression);

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
        self.add_error(message, self.peek_token.line);
    }

    fn add_error(&mut self, message: String, line: u32) {
        self.errors.push(format!("Line {}: {}", line, message));
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

        let value = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => {
                return None;
            }
        };

        while !self.current_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(LetStatement::new(let_token, identifier, value))
    }

    fn parse_return_statement(&mut self) -> Option<ReturnStatement> {
        let token: Token = self.current_token.clone();

        self.next_token(); // skip return

        let value = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => {
                return None;
            }
        };

        while !self.current_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(ReturnStatement::new(token, value))
    }

    fn parse_expression_statement(&mut self) -> Option<ExpressionStatement> {
        let token = self.current_token.clone();
        let expression = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => {
                return None;
            }
        };
        let expression_statement = Some(ExpressionStatement::new(token, expression));

        if self.peek_token.kind == TokenType::Semicolon {
            self.next_token();
        }

        return expression_statement;
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
        let prefix_fn = match self.prefix_parse_fns.get(&self.current_token.kind) {
            Some(prefix_fn) => prefix_fn,
            None => {
                self.add_error(
                    format!(
                        "No prefix parse function found for token: {}",
                        self.current_token.kind
                    ),
                    self.current_token.line,
                );
                return None;
            }
        };

        let mut left_exp = match prefix_fn(self) {
            Some(expr) => expr,
            None => {
                return None;
            }
        };

        while !self.current_token_is(TokenType::Semicolon) && precedence < self.peek_precedence() {
            self.next_token(); // skip token
            let infix_fn = match self.infix_parse_fns.get(&self.current_token.kind) {
                Some(infix_fn) => infix_fn,
                None => {
                    self.add_error(
                        format!(
                            "No infix parse function found for token {}",
                            self.current_token.kind
                        ),
                        self.current_token.line,
                    );
                    return None;
                }
            };
            left_exp = infix_fn(self, left_exp)?;
        }

        Some(left_exp)
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

    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut block_statement = BlockStatement::new(self.current_token.clone());

        self.next_token(); // skip left brace

        while !self.current_token_is(TokenType::RightBrace)
            && !self.current_token_is(TokenType::EOF)
        {
            if let Some(statement) = self.parse_statement() {
                block_statement.add_statement(statement);
            }
            self.next_token();
        }

        block_statement
    }
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

    precedences
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
            parser.add_error(
                format!("could not parse {} as integer", parser.current_token.lexeme),
                parser.current_token.line,
            );
            return None;
        }
    };

    Some(Expression::Integer(IntegerLiteral::new(
        parser.current_token.clone(),
        value,
    )))
}

pub fn parse_boolean_literal(parser: &mut Parser<'_>) -> Option<Expression> {
    let value = match parser.current_token.lexeme.as_str() {
        "true" => true,
        "false" => false,
        _ => {
            parser.errors.push(format!(
                "expected 'true' or 'false', got {}",
                parser.current_token.lexeme
            ));
            return None;
        }
    };

    Some(Expression::Boolean(BooleanLiteral::new(
        parser.current_token.clone(),
        value,
    )))
}

pub fn parse_prefix_expression(parser: &mut Parser<'_>) -> Option<Expression> {
    let token = parser.current_token.clone();
    let operator = parser.current_token.lexeme.clone();

    parser.next_token();

    let right = match parser.parse_expression(Precedence::Prefix) {
        Some(expr) => expr,
        None => {
            parser.add_error(
                format!(
                    "could not parse right-hand side of prefix expression for operator {}",
                    operator
                ),
                parser.current_token.line,
            );
            return None;
        }
    };

    Some(Expression::Prefix(PrefixExpression::new(
        token, operator, right,
    )))
}

pub fn parse_infix_expression(parser: &mut Parser<'_>, left: Expression) -> Option<Expression> {
    let token = parser.current_token.clone();
    let operator = parser.current_token.lexeme.clone();
    let precedence = parser.current_precedence();

    parser.next_token();

    let right = match parser.parse_expression(precedence) {
        Some(expr) => expr,
        None => {
            parser.add_error(
                format!(
                    "could not parse right-hand side of infix expression for operator {}",
                    operator
                ),
                parser.current_token.line,
            );
            return None;
        }
    };

    Some(Expression::Infix(InfixExpression::new(
        token, left, operator, right,
    )))
}

fn parse_grouped_expression(parser: &mut Parser<'_>) -> Option<Expression> {
    parser.next_token(); // Skip left parenthesis
    let expression = parser.parse_expression(Precedence::Lowest);
    if !parser.expect_peek(TokenType::RightParen) {
        return None;
    }

    return expression;
}

fn parse_if_expression(parser: &mut Parser<'_>) -> Option<Expression> {
    let token = parser.current_token.clone(); // if token
    parser.next_token(); // skip if
    let condition = parser.parse_expression(Precedence::Lowest);

    if !parser.expect_peek(TokenType::LeftBrace) {
        return None;
    }

    let consequence = parser.parse_block_statement();
    parser.next_token(); // Skip rigth brace
    let mut alternative: Option<BlockStatement> = None;

    if parser.current_token_is(TokenType::Else) {
        if !parser.expect_peek(TokenType::LeftBrace) {
            return None;
        }

        alternative = Some(parser.parse_block_statement());
        parser.next_token(); // Skip rigth brace
    }

    return Some(Expression::If(IfExpression::new(
        token,
        condition?,
        consequence,
        alternative,
    )));
}

fn parse_function_literal(parser: &mut Parser<'_>) -> Option<Expression> {
    let token = parser.current_token.clone();

    if !parser.expect_peek(TokenType::LeftParen) {
        return None;
    }

    let arguments = match parse_arguments(parser) {
        Some(arguments) => arguments,
        None => return None,
    };

    if !parser.expect_peek(TokenType::LeftBrace) {
        return None;
    }

    let body = parser.parse_block_statement();

    return Some(Expression::Function(FunctionLiteral::new(
        token, arguments, body,
    )));
}

fn parse_arguments(parser: &mut Parser<'_>) -> Option<Vec<Identifier>> {
    let mut arguments = vec![];

    if parser.peek_token.kind == TokenType::RightParen {
        parser.next_token();
        return Some(arguments);
    }
    parser.next_token();

    let identifier = Identifier::new(
        parser.current_token.clone(),
        parser.current_token.lexeme.clone(),
    );

    arguments.push(identifier);

    while parser.peek_token.kind == TokenType::Comma {
        parser.next_token(); // Skip identifier
        parser.next_token(); // Skip comma
        let identifier = Identifier::new(
            parser.current_token.clone(),
            parser.current_token.lexeme.clone(),
        );
        arguments.push(identifier);
    }

    if !parser.expect_peek(TokenType::RightParen) {
        return None;
    }

    return Some(arguments);
}
