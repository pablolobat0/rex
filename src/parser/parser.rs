use crate::lexer::{
    lexer::Lexer,
    token::{Token, TokenType},
};

use crate::parser::ast::Identifier;

use super::ast::{
    BlockStatement, BooleanLiteral, CallExpression, Expression, ExpressionStatement,
    FunctionLiteral, IfExpression, InfixExpression, IntegerLiteral, LetStatement, PrefixExpression,
    Program, ReturnStatement, Statement, StringLiteral, WhileStatement,
};
use std::collections::HashMap;

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

// Function types for prefix and infix parsing
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

        // Infix functions
        parser.register_prefix(TokenType::Identifier, parse_identifier);
        parser.register_prefix(TokenType::Integer, parse_integer_literal);
        parser.register_prefix(TokenType::True, parse_boolean_literal);
        parser.register_prefix(TokenType::False, parse_boolean_literal);
        parser.register_prefix(TokenType::String, parse_string_literal);
        parser.register_prefix(TokenType::Minus, parse_prefix_expression);
        parser.register_prefix(TokenType::Bang, parse_prefix_expression);
        parser.register_prefix(TokenType::LeftParen, parse_grouped_expression);
        parser.register_prefix(TokenType::If, parse_if_expression);
        parser.register_prefix(TokenType::Function, parse_function_literal);

        // Prefix functions
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
        parser.register_infix(TokenType::Equal, parse_infix_expression);
        parser.register_infix(TokenType::LeftParen, parse_call_expression);

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

    // Consumes a token, updating current and peek token
    fn next_token(&mut self) {
        self.current_token = std::mem::replace(&mut self.peek_token, self.lexer.next_token());
    }

    // Generates the AST
    // <program> ::= <statement_list>
    // <statement_list> ::= <statement> <statement_list> | <statement>
    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while self.current_token.kind != TokenType::EOF {
            if let Some(statement) = self.parse_statement() {
                program.add_statement(statement);
            }
            self.next_token();
        }

        program
    }

    // <statement> ::= <let_statement> | <return_statement> | <expression_statement>
    fn parse_statement(&mut self) -> Option<Statement> {
        match self.current_token.kind {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            TokenType::While => self.parse_while_statement(),
            _ => self.parse_expression_statement().map(Statement::Expression),
        }
    }

    // let <identifier> = <expression>
    fn parse_let_statement(&mut self) -> Option<Statement> {
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

        // The semicolon is optional to improve REPL experience
        while !self.current_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(Statement::Let(LetStatement::new(
            let_token, identifier, value,
        )))
    }

    // <return_statement> ::= return <expression>
    fn parse_return_statement(&mut self) -> Option<Statement> {
        let token: Token = self.current_token.clone();
        // Consume while
        self.next_token();

        let value = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => {
                return None;
            }
        };

        // The semicolon is optional to improve REPL experience
        while !self.current_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(Statement::Return(ReturnStatement::new(token, value)))
    }

    fn parse_while_statement(&mut self) -> Option<Statement> {
        let token = self.current_token.clone();
        // Consume while
        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(TokenType::LeftBrace) {
            return None;
        }

        let body = self.parse_block_statement();

        // Skip right brace
        self.next_token();

        Some(Statement::While(WhileStatement::new(
            token, condition?, body,
        )))
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

        // The semicolon is optional to improve REPL experience
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

    // Top-Down Operator Precedence consists of parser_expression, prefix_fn
    // and infix_fn calling each other recursively using the current and peek
    // operator precedences
    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let prefix_fn = match self.prefix_parse_fns.get(&self.current_token.kind) {
            Some(prefix_fn) => prefix_fn,
            None => return self.error_no_prefix_function(),
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
                None => return self.error_no_infix_function(),
            };
            left_exp = infix_fn(self, left_exp)?;
        }

        Some(left_exp)
    }

    fn error_no_prefix_function(&mut self) -> Option<Expression> {
        self.add_error(
            format!(
                "No prefix parse function found for token: {}",
                self.current_token.kind
            ),
            self.current_token.line,
        );
        return None;
    }

    fn error_no_infix_function(&mut self) -> Option<Expression> {
        self.add_error(
            format!(
                "No infix parse function found for token {}",
                self.current_token.kind
            ),
            self.current_token.line,
        );
        return None;
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
// Prefix functions
fn parse_identifier(parser: &mut Parser<'_>) -> Option<Expression> {
    Some(Expression::Identifier(Identifier::new(
        parser.current_token.clone(),
        parser.current_token.lexeme.clone(),
    )))
}

fn parse_integer_literal(parser: &mut Parser<'_>) -> Option<Expression> {
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

fn parse_boolean_literal(parser: &mut Parser<'_>) -> Option<Expression> {
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

fn parse_string_literal(parser: &mut Parser<'_>) -> Option<Expression> {
    Some(Expression::String(StringLiteral::new(
        parser.current_token.clone(),
        parser.current_token.lexeme.clone(),
    )))
}

fn parse_prefix_expression(parser: &mut Parser<'_>) -> Option<Expression> {
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

// <if_expression> ::= if <expression> <block_statement> [else <block_statement>]
fn parse_if_expression(parser: &mut Parser<'_>) -> Option<Expression> {
    let token = parser.current_token.clone(); // if token
                                              // Consume if
    parser.next_token();

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

// <function_literal> ::= fn (<parameters>?) <block_statement>
fn parse_function_literal(parser: &mut Parser<'_>) -> Option<Expression> {
    let token = parser.current_token.clone();

    if !parser.expect_peek(TokenType::LeftParen) {
        return None;
    }

    let parameters = match parse_parameters(parser) {
        Some(arguments) => arguments,
        None => return None,
    };

    if !parser.expect_peek(TokenType::LeftBrace) {
        return None;
    }

    let body = parser.parse_block_statement();

    return Some(Expression::Function(FunctionLiteral::new(
        token, parameters, body,
    )));
}

// <parameters> ::= <identifier> (, <identifier>)*
fn parse_parameters(parser: &mut Parser<'_>) -> Option<Vec<Identifier>> {
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
        // Skip identifier
        parser.next_token();
        // Skip comma
        parser.next_token();
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

//Infix functions

// <infix_expression> ::= <expression> <infix_operator> <expression>
fn parse_infix_expression(parser: &mut Parser<'_>, left: Expression) -> Option<Expression> {
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

// <call_expression> ::= <expression> ( <arguments>? )
fn parse_call_expression(parser: &mut Parser<'_>, left: Expression) -> Option<Expression> {
    let token = parser.current_token.clone();
    match parse_arguments(parser) {
        Some(arguments) => {
            return Some(Expression::Call(CallExpression::new(
                token, left, arguments,
            )))
        }
        None => return None,
    }
}

// <arguments> ::= <expression> (, <expression> )*
fn parse_arguments(parser: &mut Parser<'_>) -> Option<Vec<Expression>> {
    let mut arguments = vec![];
    if parser.peek_token.kind == TokenType::RightParen {
        parser.next_token();
        return Some(arguments);
    }
    parser.next_token();

    match parser.parse_expression(Precedence::Lowest) {
        Some(argument) => arguments.push(argument),
        None => return None,
    }

    while parser.peek_token.kind == TokenType::Comma {
        // Skip argument
        parser.next_token();
        // Skip comma
        parser.next_token();
        match parser.parse_expression(Precedence::Lowest) {
            Some(argument) => arguments.push(argument),
            None => return None,
        }
    }

    if !parser.expect_peek(TokenType::RightParen) {
        return None;
    }

    return Some(arguments);
}
