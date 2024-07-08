use std::u64;

use crate::lexer::token::token::Token;

pub trait Node {
    fn get_lexeme(&self) -> String;
}

// An expression computes a value
#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    Integer(IntegerLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
}

impl Node for Expression {
    fn get_lexeme(&self) -> String {
        match self {
            Expression::Identifier(identifier) => identifier.get_lexeme(),
            Expression::Integer(integer) => integer.get_lexeme(),
            Expression::Prefix(prefix_expression) => prefix_expression.get_lexeme(),
            Expression::Infix(infinx_expression) => infinx_expression.get_lexeme(),
        }
    }
}

// A statement expresses some action, but does not generate a value
#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl Node for Statement {
    fn get_lexeme(&self) -> String {
        match self {
            Statement::Let(statement) => statement.get_lexeme(),
            Statement::Return(statement) => statement.get_lexeme(),
            Statement::Expression(statement) => statement.get_lexeme(),
        }
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl Node for ExpressionStatement {
    fn get_lexeme(&self) -> String {
        self.expression.get_lexeme()
    }
}

impl ExpressionStatement {
    pub fn new(token: Token, expression: Expression) -> ExpressionStatement {
        ExpressionStatement { token, expression }
    }
}

// Root node of the AST
#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Program {
        Program { statements: vec![] }
    }
}

impl Node for Program {
    fn get_lexeme(&self) -> String {
        self.statements
            .iter()
            .map(|statement| statement.get_lexeme())
            .collect::<Vec<String>>()
            .join("\n")
    }
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub identifier: Identifier,
    pub value: Expression,
}

impl LetStatement {
    pub fn new(token: Token, identifier: Identifier, value: Expression) -> LetStatement {
        LetStatement {
            token,
            identifier,
            value,
        }
    }
}

impl Node for LetStatement {
    fn get_lexeme(&self) -> String {
        format!(
            "let {} = {}",
            self.identifier.get_lexeme(),
            self.value.get_lexeme()
        )
    }
}

#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    pub name: String,
}

impl Identifier {
    pub fn new(token: Token, name: String) -> Identifier {
        Identifier { token, name }
    }
}

impl Node for Identifier {
    fn get_lexeme(&self) -> String {
        return self.token.lexeme.clone();
    }
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub value: Expression,
}

impl Node for ReturnStatement {
    fn get_lexeme(&self) -> String {
        format!("{} {}", self.token.kind, self.value.get_lexeme())
    }
}

impl ReturnStatement {
    pub fn new(token: Token, value: Expression) -> ReturnStatement {
        ReturnStatement { token, value }
    }
}

#[derive(Debug)]
pub struct IntegerLiteral {
    token: Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }
}

impl IntegerLiteral {
    pub fn new(token: Token, value: i64) -> IntegerLiteral {
        IntegerLiteral { token, value }
    }
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Node for PrefixExpression {
    fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }
}

impl PrefixExpression {
    pub fn new(token: Token, operator: String, right: Expression) -> PrefixExpression {
        PrefixExpression {
            token,
            operator,
            right: Box::new(right),
        }
    }
}

#[derive(Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Node for InfixExpression {
    fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }
}

impl InfixExpression {
    pub fn new(
        token: Token,
        left: Expression,
        operator: String,
        right: Expression,
    ) -> InfixExpression {
        InfixExpression {
            token,
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }
}
