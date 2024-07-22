use crate::lexer::token::{Token, TokenType};
use std::collections::HashMap;
use std::str::Chars;

use super::token::keywords;

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a str,
    chars: Chars<'a>,
    // Byte position in the input
    position: usize,
    current_char: Option<char>,
    next_char: Option<char>,
    // Line number starting at 1
    line: u32,
    keywords: HashMap<&'static str, TokenType>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut chars = input.chars();
        let current_char = chars.next();
        let next_char = chars.clone().next();
        Lexer {
            input,
            chars,
            position: 0,
            current_char,
            next_char,
            line: 1,
            keywords: keywords(),
        }
    }

    // Advance one position on input
    fn read_char(&mut self) {
        if let Some(c) = self.current_char {
            self.position += c.len_utf8();
        }

        self.current_char = self.chars.next();
        self.next_char = self.chars.clone().next();
    }

    // Return the next char without advancing
    fn peek_char(&self) -> Option<char> {
        self.next_char.clone()
    }

    fn skip_whitespaces(&mut self) {
        while let Some(c) = self.current_char {
            if c.is_whitespace() {
                if c == '\n' {
                    self.line += 1;
                }
                self.read_char();
            } else {
                break;
            }
        }
    }

    // Consumes characters until it can form a token
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespaces();

        let token = match self.current_char {
            Some('(') => {
                self.read_char();
                Token::new(TokenType::LeftParen, "(".to_string(), self.line)
            }
            Some(')') => {
                self.read_char();
                Token::new(TokenType::RightParen, ")".to_string(), self.line)
            }
            Some('{') => {
                self.read_char();
                Token::new(TokenType::LeftBrace, "{".to_string(), self.line)
            }
            Some('}') => {
                self.read_char();
                Token::new(TokenType::RightBrace, "}".to_string(), self.line)
            }
            Some('-') => {
                self.read_char();
                Token::new(TokenType::Minus, "-".to_string(), self.line)
            }
            Some('+') => {
                self.read_char();
                Token::new(TokenType::Plus, "+".to_string(), self.line)
            }
            Some('*') => {
                self.read_char();
                Token::new(TokenType::Star, "*".to_string(), self.line)
            }
            Some('/') => {
                self.read_char();
                if self.current_char == Some('/') {
                    self.read_one_line_comment();
                    return self.next_token();
                } else if self.current_char == Some('*') {
                    self.read_multiple_line_comment();
                    return self.next_token();
                } else {
                    Token::new(TokenType::Slash, "/".to_string(), self.line)
                }
            }
            Some('=') => {
                self.read_char();
                if self.current_char == Some('=') {
                    self.read_char();
                    Token::new(TokenType::EqualEqual, "==".to_string(), self.line)
                } else {
                    Token::new(TokenType::Equal, "=".to_string(), self.line)
                }
            }
            Some('>') => {
                self.read_char();
                if self.current_char == Some('=') {
                    self.read_char();
                    Token::new(TokenType::GreaterEqual, ">=".to_string(), self.line)
                } else {
                    Token::new(TokenType::Greater, ">".to_string(), self.line)
                }
            }
            Some('<') => {
                self.read_char();
                if self.current_char == Some('=') {
                    self.read_char();
                    Token::new(TokenType::LessEqual, "<=".to_string(), self.line)
                } else {
                    Token::new(TokenType::Less, "<".to_string(), self.line)
                }
            }
            Some(':') => {
                self.read_char();
                Token::new(TokenType::Colon, ":".to_string(), self.line)
            }
            Some(';') => {
                self.read_char();
                Token::new(TokenType::Semicolon, ";".to_string(), self.line)
            }
            Some(',') => {
                self.read_char();
                Token::new(TokenType::Comma, ",".to_string(), self.line)
            }
            Some('.') => {
                self.read_char();
                Token::new(TokenType::Dot, ".".to_string(), self.line)
            }
            Some('?') => {
                self.read_char();
                Token::new(TokenType::Question, "?".to_string(), self.line)
            }
            Some('!') => {
                self.read_char();
                if self.current_char == Some('=') {
                    self.read_char();
                    Token::new(TokenType::BangEqual, "!=".to_string(), self.line)
                } else {
                    Token::new(TokenType::Bang, "!".to_string(), self.line)
                }
            }
            Some('&') => {
                self.read_char();
                if self.current_char == Some('&') {
                    self.read_char();
                    Token::new(TokenType::And, "&&".to_string(), self.line)
                } else {
                    Token::new(TokenType::Illegal, "&".to_string(), self.line)
                }
            }
            Some('|') => {
                self.read_char();
                if self.current_char == Some('|') {
                    self.read_char();
                    Token::new(TokenType::Or, "||".to_string(), self.line)
                } else {
                    Token::new(TokenType::Illegal, "|".to_string(), self.line)
                }
            }
            Some('"') => self.read_string(),
            Some(c) => {
                if c.is_alphabetic() || c == '_' {
                    self.read_identifier_or_keyword()
                } else if c.is_digit(10) {
                    self.read_number()
                } else {
                    self.read_char();
                    Token::new(TokenType::Illegal, c.to_string(), self.line)
                }
            }
            None => Token::new(TokenType::EOF, "".to_string(), self.line),
        };

        token
    }

    fn read_one_line_comment(&mut self) {
        // Skip first char
        self.read_char();
        while let Some(c) = self.current_char {
            if c == '\n' {
                break;
            }
            self.read_char();
        }
    }

    fn read_multiple_line_comment(&mut self) {
        // Skip first char
        self.read_char();
        while let Some(c) = self.current_char {
            if c == '\n' {
                self.line += 1;
            } else if c == '*' && self.peek_char() == Some('/') {
                self.read_char(); // Skip *
                self.read_char(); // Skip /
                break;
            }
            self.read_char();
        }
    }

    fn read_identifier_or_keyword(&mut self) -> Token {
        let start_position = self.position;

        while let Some(c) = self.current_char {
            if !c.is_alphanumeric() && c != '_' {
                break;
            }
            self.read_char();
        }

        let lexeme: String = self.input[start_position..self.position].to_string();
        // Search if it is a keyword
        let kind = match self.keywords.get(lexeme.as_str()) {
            Some(&token_type) => token_type,
            None => TokenType::Identifier,
        };

        Token::new(kind, lexeme, self.line)
    }

    fn read_number(&mut self) -> Token {
        let start_position = self.position;

        while let Some(c) = self.current_char {
            if !c.is_digit(10) {
                break;
            }
            self.read_char();
        }

        let lexeme: String = self.input[start_position..self.position].to_string();

        Token::new(TokenType::Integer, lexeme, self.line)
    }

    fn read_string(&mut self) -> Token {
        // Skip "
        self.read_char();
        let start_position = self.position;

        while let Some(c) = self.current_char {
            if c == '"' {
                break;
            }
            self.read_char();
        }

        let lexeme: String = self.input[start_position..self.position].to_string();
        // Skip "
        self.read_char();

        Token::new(TokenType::String, lexeme, self.line)
    }
}
