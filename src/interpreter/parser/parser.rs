use crate::common::{
    lexer::{
        lexer::Lexer,
        token::{Token, TokenType},
    },
    precedences::{create_precedences, Precedence},
};

use crate::interpreter::parser::ast::Identifier;

use super::ast::{
    BlockStatement, BooleanLiteral, CallExpression, Expression, ExpressionStatement, FloatLiteral,
    FunctionLiteral, IfExpression, InfixExpression, IntegerLiteral, LetStatement, PrefixExpression,
    Program, ReturnStatement, Statement, StringLiteral, WhileStatement,
};
use std::collections::HashMap;

// Function types for prefix and infix parsing
type PrefixParseFn = fn(&mut Parser<'_>) -> Option<Expression>;
type InfixParseFn = fn(&mut Parser, Expression) -> Option<Expression>;

// Default token for errors
const ERROR: Token = Token {
    kind: TokenType::Error,
    lexeme: String::new(),
    line: 0,
};

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Option<Token>,
    peek_token: Option<Token>,
    pub errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
    precedences: HashMap<TokenType, Precedence>,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Parser {
        let current_token = Some(lexer.next_token());
        let peek_token = Some(lexer.next_token());

        let mut parser = Parser {
            lexer,
            current_token,
            peek_token,
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
            precedences: create_precedences(),
        };

        // Add parser functions
        parser.register_prefix_functions();
        parser.register_infix_functions();

        parser
    }

    fn register_prefix_functions(&mut self) {
        self.prefix_parse_fns
            .insert(TokenType::Identifier, parse_identifier);
        self.prefix_parse_fns
            .insert(TokenType::Integer, parse_integer_literal);
        self.prefix_parse_fns
            .insert(TokenType::Float, parse_float_literal);
        self.prefix_parse_fns
            .insert(TokenType::True, parse_boolean_literal);
        self.prefix_parse_fns
            .insert(TokenType::False, parse_boolean_literal);
        self.prefix_parse_fns
            .insert(TokenType::String, parse_string_literal);
        self.prefix_parse_fns
            .insert(TokenType::Minus, parse_prefix_expression);
        self.prefix_parse_fns
            .insert(TokenType::Bang, parse_prefix_expression);
        self.prefix_parse_fns
            .insert(TokenType::LeftParen, parse_grouped_expression);
        self.prefix_parse_fns
            .insert(TokenType::If, parse_if_expression);
        self.prefix_parse_fns
            .insert(TokenType::Function, parse_function_literal);
    }

    fn register_infix_functions(&mut self) {
        self.infix_parse_fns
            .insert(TokenType::Plus, parse_infix_expression);
        self.infix_parse_fns
            .insert(TokenType::Minus, parse_infix_expression);
        self.infix_parse_fns
            .insert(TokenType::Star, parse_infix_expression);
        self.infix_parse_fns
            .insert(TokenType::Slash, parse_infix_expression);
        self.infix_parse_fns
            .insert(TokenType::EqualEqual, parse_infix_expression);
        self.infix_parse_fns
            .insert(TokenType::BangEqual, parse_infix_expression);
        self.infix_parse_fns
            .insert(TokenType::Greater, parse_infix_expression);
        self.infix_parse_fns
            .insert(TokenType::GreaterEqual, parse_infix_expression);
        self.infix_parse_fns
            .insert(TokenType::Less, parse_infix_expression);
        self.infix_parse_fns
            .insert(TokenType::LessEqual, parse_infix_expression);
        self.infix_parse_fns
            .insert(TokenType::Equal, parse_infix_expression);
        self.infix_parse_fns
            .insert(TokenType::LeftParen, parse_call_expression);
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
            self.current_token_type(),
            self.current_token
                .as_ref()
                .map(|t| t.lexeme.clone())
                .unwrap_or(String::new())
        );
        self.add_error(error, self.peek_token.as_ref().map(|t| t.line).unwrap_or(0));
    }

    fn add_error(&mut self, message: String, line: u32) {
        self.errors.push(format!("Line {}: {}", line, message));
    }

    // Consumes a token, updating current and peek token
    fn next_token(&mut self) {
        self.current_token = self.peek_token.take();
        self.peek_token = Some(self.lexer.next_token());
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

    fn current_token_type(&self) -> TokenType {
        self.current_token
            .as_ref()
            .map(|t| t.kind)
            .unwrap_or(TokenType::Error)
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

    // Generates the AST
    // <program> ::= <statement_list>
    // <statement_list> ::= <statement> <statement_list> | <statement>
    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        // The parsing continues until the end of the file
        while !self.current_token_is(TokenType::EOF) {
            if let Some(statement) = self.parse_statement() {
                program.add_statement(statement);
            }
            self.next_token();
        }

        program
    }

    // <statement> ::= <let_statement> | <return_statement> | <expression_statement>
    fn parse_statement(&mut self) -> Option<Statement> {
        // Skip lines without code
        while self.current_token_is(TokenType::NewLine) {
            self.next_token();
        }

        let statement = match self.current_token.as_ref().map(|t| t.kind) {
            Some(TokenType::Let) => self.parse_let_statement(),
            Some(TokenType::Return) => self.parse_return_statement(),
            Some(TokenType::While) => self.parse_while_statement(),
            None => None,
            _ => self.parse_expression_statement().map(Statement::Expression),
        };

        if self.peek_token_is(TokenType::EOF) {
            return statement;
        } else if !self.expect_peek(TokenType::NewLine) {
            return None;
        }

        statement
    }

    // let <identifier> = <expression>
    fn parse_let_statement(&mut self) -> Option<Statement> {
        let let_token = self.current_token.take();

        if !self.expect_peek(TokenType::Identifier) {
            return None;
        };

        let Some(token) = self.current_token.take() else {
            return None;
        };

        let identifier = Identifier::new(token);
        if !self.expect_peek(TokenType::Equal) {
            return None;
        }
        // Consume =
        self.next_token();

        // Parse right expression
        let Some(value) = self.parse_expression(Precedence::Lowest) else {
            return None;
        };

        Some(Statement::Let(LetStatement::new(
            let_token?, identifier, value,
        )))
    }

    // <return_statement> ::= return <expression>
    fn parse_return_statement(&mut self) -> Option<Statement> {
        let token = self.current_token.take();
        // Consume return
        self.next_token();

        let Some(value) = self.parse_expression(Precedence::Lowest) else {
            return None;
        };

        Some(Statement::Return(ReturnStatement::new(token?, value)))
    }

    fn parse_while_statement(&mut self) -> Option<Statement> {
        let token = self.current_token.take();
        // Consume while
        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(TokenType::LeftBrace) {
            return None;
        }

        let body = self.parse_block_statement();

        Some(Statement::While(WhileStatement::new(
            token?, condition?, body,
        )))
    }

    fn parse_expression_statement(&mut self) -> Option<ExpressionStatement> {
        let token = self.current_token.clone().unwrap_or(ERROR);
        let Some(expression) = self.parse_expression(Precedence::Lowest) else {
            return None;
        };

        Some(ExpressionStatement::new(token, expression))
    }

    // Top-Down Operator Precedence consists of parser_expression, prefix_fn
    // and infix_fn calling each other recursively using the current and peek
    // operator precedences
    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let Some(prefix_fn) =  self.prefix_parse_fns.get(&self.current_token_type()) else {
                self.current_error("No prefix parse function found for token: ");
                return None;
        };

        let Some(mut left_exp) = prefix_fn(self) else {
                return None;
        };

        while !self.current_token_is(TokenType::NewLine) && precedence < self.peek_precedence() {
            self.next_token(); // skip token
            let Some(infix_fn) = self.infix_parse_fns.get(&self.current_token_type()) else {
                    self.current_error("No infix parse function found for token: ");
                    return None;
            };

            left_exp = infix_fn(self, left_exp)?;
        }

        Some(left_exp)
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut block_statement = BlockStatement::new(self.current_token.take().unwrap_or(ERROR));
        // skip left brace
        self.next_token();

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

// Prefix functions
fn parse_identifier(parser: &mut Parser<'_>) -> Option<Expression> {
    if let Some(token) = parser.current_token.take() {
        Some(Expression::Identifier(Identifier::new(token)))
    } else {
        None
    }
}

fn parse_integer_literal(parser: &mut Parser<'_>) -> Option<Expression> {
    let Ok(value) =  parser.current_token.as_ref().
        map(|t| t.lexeme.clone()) .unwrap_or(String::new()) 
        .parse::<i64>() else {
            parser.current_error("Could not parse as integer: ");
            return None;
    };

    Some(Expression::Integer(IntegerLiteral::new(
        parser.current_token.take()?,
        value,
    )))
}

fn parse_float_literal(parser: &mut Parser<'_>) -> Option<Expression> {
    let Ok(value) = parser.current_token.as_ref()
        .map(|t| t.lexeme.clone()).unwrap_or(String::new())
        .parse::<f64>() else {
            parser.current_error("Could not parse as float: ");
            return None;
    };

    Some(Expression::Float(FloatLiteral::new(
        parser.current_token.take()?,
        value,
    )))
}

fn parse_boolean_literal(parser: &mut Parser<'_>) -> Option<Expression> {
    let value = match parser
        .current_token
        .as_ref()
        .map(|t| t.lexeme.as_str())
        .unwrap_or("")
    {
        "true" => true,
        "false" => false,
        _ => {
            parser.current_error("Expected boolean literal got insted: ");
            return None;
        }
    };

    Some(Expression::Boolean(BooleanLiteral::new(
        parser.current_token.take()?,
        value,
    )))
}

fn parse_string_literal(parser: &mut Parser<'_>) -> Option<Expression> {
    let lexeme = parser.current_token.as_ref().map(|t| t.lexeme.clone());
    Some(Expression::String(StringLiteral::new(
        parser.current_token.take()?,
        lexeme?,
    )))
}

fn parse_prefix_expression(parser: &mut Parser<'_>) -> Option<Expression> {
    let operator = parser.current_token.as_ref().map(|t| t.lexeme.clone());
    let token = parser.current_token.take();

    parser.next_token();

    let Some(right) = parser.parse_expression(Precedence::Prefix) else {
            parser.current_error("Could not parser right-side of prefix expression for operator: ");
            return None;
    };

    Some(Expression::Prefix(PrefixExpression::new(
        token?, operator?, right,
    )))
}

// <if_expression> ::= if <expression> <block_statement> [else <block_statement>]
fn parse_if_expression(parser: &mut Parser<'_>) -> Option<Expression> {
    // if token
    let token = parser.current_token.take();
    // Consume if
    parser.next_token();

    let condition = parser.parse_expression(Precedence::Lowest);

    if !parser.expect_peek(TokenType::LeftBrace) {
        return None;
    }

    let consequence = parser.parse_block_statement();
    let mut alternative: Option<BlockStatement> = None;

    if parser.peek_token_is(TokenType::Else) {
        // Consume right brace
        parser.next_token();
        if !parser.expect_peek(TokenType::LeftBrace) {
            return None;
        }

        alternative = Some(parser.parse_block_statement());
    }

    return Some(Expression::If(IfExpression::new(
        token?,
        condition?,
        consequence,
        alternative,
    )));
}

// <function_literal> ::= fn (<parameters>?) <block_statement>
fn parse_function_literal(parser: &mut Parser<'_>) -> Option<Expression> {
    let token = parser.current_token.take();

    if !parser.expect_peek(TokenType::LeftParen) {
        return None;
    }

    let Some(parameters) = parse_parameters(parser) else {
        return None;
    };

    if !parser.expect_peek(TokenType::LeftBrace) {
        return None;
    }

    let body = parser.parse_block_statement();

    return Some(Expression::Function(FunctionLiteral::new(
        token?, parameters, body,
    )));
}

// <parameters> ::= <identifier> (, <identifier>)*
fn parse_parameters(parser: &mut Parser<'_>) -> Option<Vec<Identifier>> {
    let mut arguments = vec![];

    if parser.peek_token_is(TokenType::RightParen) {
        parser.next_token();
        return Some(arguments);
    }

    if parser.current_token.is_none() {
        return None;
    }

    parser.next_token();

    let identifier = Identifier::new(parser.current_token.take()?);

    arguments.push(identifier);

    while parser.peek_token_is(TokenType::Comma) {
        // Consume identifier
        parser.next_token();
        // Consume comma
        parser.next_token();
        // Consume optional new line between parameters
        if parser.current_token_is(TokenType::NewLine) {
            parser.next_token();
        }

        arguments.push(Identifier::new(parser.current_token.take()?));
    }

    if !parser.expect_peek(TokenType::RightParen) {
        return None;
    }

    return Some(arguments);
}

//Infix functions

// <infix_expression> ::= <expression> <infix_operator> <expression>
fn parse_infix_expression(parser: &mut Parser<'_>, left: Expression) -> Option<Expression> {
    let operator = parser.current_token.as_ref().map(|t| t.lexeme.clone());
    let precedence = parser.current_precedence();
    let token = parser.current_token.take();

    parser.next_token();

    let Some(right) = parser.parse_expression(precedence) else {
            parser.current_error("Could not parse right-part of infix expression for operator: ");
            return None;
    };

    Some(Expression::Infix(InfixExpression::new(
        token?, left, operator?, right,
    )))
}

fn parse_grouped_expression(parser: &mut Parser<'_>) -> Option<Expression> {
    parser.next_token(); // Consume left parenthesis
    let expression = parser.parse_expression(Precedence::Lowest);
    if !parser.expect_peek(TokenType::RightParen) {
        return None;
    }

    return expression;
}

// <call_expression> ::= <expression> ( <arguments>? )
fn parse_call_expression(parser: &mut Parser<'_>, left: Expression) -> Option<Expression> {
    let token = parser.current_token.take();
    let Some(arguments)=parse_arguments(parser) else {
         return None;
    };
    Some(Expression::Call(CallExpression::new( token?, left, arguments)))
}

// <arguments> ::= <expression> (, <expression> )*
fn parse_arguments(parser: &mut Parser<'_>) -> Option<Vec<Expression>> {
    let mut arguments = vec![];
    if parser.peek_token_is(TokenType::RightParen) {
        parser.next_token();
        return Some(arguments);
    }
    parser.next_token();

    let Some(argument) = parser.parse_expression(Precedence::Lowest) else {
        return None;
    };
    arguments.push(argument);

    while parser.peek_token_is(TokenType::Comma) {
        // Consume argument
        parser.next_token();
        // Consume comma
        parser.next_token();
        // Consume optional new line between arguments
        if parser.current_token_is(TokenType::NewLine) {
            parser.next_token();
        }

        let Some(argument) = parser.parse_expression(Precedence::Lowest) else {
            return None;
        };
        arguments.push(argument);
    }

    if !parser.expect_peek(TokenType::RightParen) {
        return None;
    }

    return Some(arguments);
}
