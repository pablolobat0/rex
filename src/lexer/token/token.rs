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
    // Types
    Identifier,
    String,
    Integer,
    // Keywords
    Let,
    And,
    Or,
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
