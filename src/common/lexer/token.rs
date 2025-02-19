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
    Comma,
    Dot,
    Question,
    Bang,
    NewLine, // Line terminator
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
    Float,
    // Keywords
    Let,
    Function,
    Return,
    True,
    False,
    If,
    Else,
    While,
    Null,
    // End of file
    EOF,
    Illegal,
    Error,
    Default,
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
            TokenType::Float => "Float",
            TokenType::Plus => "+",
            TokenType::Minus => "-",
            TokenType::Star => "*",
            TokenType::Slash => "/",
            TokenType::Equal => "=",
            TokenType::Less => "<",
            TokenType::Greater => ">",
            TokenType::Colon => ":",
            TokenType::Comma => ",",
            TokenType::Dot => ".",
            TokenType::NewLine => "\\n",
            TokenType::Question => "?",
            TokenType::Bang => "!",
            TokenType::BangEqual => "!=",
            TokenType::EqualEqual => "==",
            TokenType::GreaterEqual => ">=",
            TokenType::LessEqual => "<=",
            TokenType::And => "&&",
            TokenType::Or => "||",
            TokenType::Let => "let",
            TokenType::Function => "fn",
            TokenType::Return => "return",
            TokenType::True => "true",
            TokenType::False => "false",
            TokenType::If => "if",
            TokenType::Else => "else",
            TokenType::While => "while",
            TokenType::Null => "null",
            TokenType::Error => "error",
            TokenType::Default => "default",
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

impl Default for Token {
    fn default() -> Self {
        Token {
            kind: TokenType::Error,
            lexeme: String::new(),
            line: 0,
        }
    }
}

impl Token {
    pub fn new(kind: TokenType, lexeme: String, line: u32) -> Token {
        Token { kind, lexeme, line }
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
    m.insert("while", TokenType::While);
    m.insert("null", TokenType::Null);

    m
}
