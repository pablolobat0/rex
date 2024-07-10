use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
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

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let token_str = match self {
            TokenType::LeftParen => "(",
            TokenType::RightParen => ")",
            TokenType::LeftBrace => "{",
            TokenType::RightBrace => "}",
            TokenType::Illegal => "Illegal",
            TokenType::EOF => "EOF",
            TokenType::Identifier => "Identifier",
            TokenType::String => "String",
            TokenType::Integer => "Integer",
            TokenType::Plus => "Plus",
            TokenType::Minus => "Minus",
            TokenType::Star => "Star",
            TokenType::Slash => "Slash",
            TokenType::Equal => "Equal",
            TokenType::Less => "Less",
            TokenType::Greater => "Greater",
            TokenType::Colon => "Colon",
            TokenType::Semicolon => "Semicolon",
            TokenType::Comma => "Comma",
            TokenType::Dot => "Dot",
            TokenType::Question => "Question",
            TokenType::Bang => "Bang",
            TokenType::BangEqual => "BangEqual",
            TokenType::EqualEqual => "EqualEqual",
            TokenType::GreaterEqual => "GreaterEqual",
            TokenType::LessEqual => "LessEqual",
            TokenType::And => "And",
            TokenType::Or => "Or",
            TokenType::Let => "Let",
            TokenType::Function => "Function",
            TokenType::Return => "Return",
            TokenType::True => "True",
            TokenType::False => "False",
            TokenType::If => "If",
            TokenType::Else => "Else",
            TokenType::Null => "Null",
        };
        write!(f, "{}", token_str)
    }
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
