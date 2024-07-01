use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    // Single character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Minus,
    Plus,
    Star,
    Slash,
    Equal,
    Greater,
    Less,
    Colon,
    Semicolon,
    Comma,
    Dot,
    Question,
    Bang,
    // Operators
    BangEqual,
    EqualEqual,
    GreaterEqual,
    LessEqual,
    And,
    Or,
    // Types
    Identifier,
    String,
    Integer,
    // Keywords
    Let,
    Function,
    Return,
    True,
    False,
    If,
    Else,
    Null,
    // End of file
    EOF,
    // Illegal
    Illegal,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenType,
    pub lexeme: String,
    pub line: u32,
}

impl Token {
    pub fn new(kind: TokenType, lexeme: String, line: u32) -> Token {
        let token = Token { kind, lexeme, line };
        token
    }
}

pub fn keywords() -> HashMap<&'static str, TokenType> {
    let mut m = HashMap::new();
    m.insert("let", TokenType::Let);
    m.insert("fn", TokenType::Function);
    m.insert("if", TokenType::If);
    m.insert("else", TokenType::Else);
    m.insert("return", TokenType::Return);
    m.insert("true", TokenType::True);
    m.insert("false", TokenType::False);
    m.insert("null", TokenType::Null);

    m
}
