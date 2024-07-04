use crate::lexer::token::token::Token;

pub trait Node {
    fn get_lexeme(&self) -> String;
}

// An expression computes a value
#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
}

impl Node for Expression {
    fn get_lexeme(&self) -> String {
        match self {
            Expression::Identifier(identifier) => identifier.get_lexeme(),
        }
    }
}

// A statement expresses some action, but does not generate a value
#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
}

impl Node for Statement {
    fn get_lexeme(&self) -> String {
        match self {
            Statement::Let(statement) => statement.get_lexeme(),
            Statement::Return(statement) => statement.get_lexeme(),
        }
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
