use crate::lexer::token::token::{Token, TokenType};
use std::collections::HashMap;

use super::token::token::keywords;

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    current_char: Option<char>,
    line: u32,
    keywords: HashMap<&'static str, TokenType>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input,
            position: 0,
            current_char: input.chars().nth(0),
            line: 1,
            keywords: keywords(),
        }
    }

    pub fn read_char(&mut self) {
        self.position += 1;
        if self.position >= self.input.len() {
            self.current_char = None;
        } else {
            self.current_char = self.input.chars().nth(self.position);
        }
    }

    pub fn skip_whitespace(&mut self) {
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

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

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
                Token::new(TokenType::Slash, "/".to_string(), self.line)
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

    fn read_identifier_or_keyword(&mut self) -> Token {
        let start_position = self.position;
        self.read_char();
        while let Some(c) = self.current_char {
            if c.is_alphanumeric() || c == '_' {
                self.read_char();
            } else {
                break;
            }
        }
        let lexeme: String = self.input[start_position..self.position].to_string();
        let kind = match self.keywords.get(lexeme.as_str()) {
            Some(&token_type) => token_type,
            None => TokenType::Identifier,
        };

        Token::new(kind, lexeme, self.line)
    }

    fn read_number(&mut self) -> Token {
        let start_position = self.position;
        self.read_char();
        while let Some(c) = self.current_char {
            if c.is_digit(10) {
                self.read_char();
            } else {
                break;
            }
        }
        let lexeme: String = self.input[start_position..self.position].to_string();

        Token::new(TokenType::Integer, lexeme, self.line)
    }

    fn read_string(&mut self) -> Token {
        let start_position = self.position + 1;
        self.read_char();
        while let Some(c) = self.current_char {
            if c == '"' {
                break;
            }
            self.read_char();
        }
        let lexeme: String = self.input[start_position..self.position].to_string();
        self.read_char(); // Skip "

        Token::new(TokenType::String, lexeme, self.line)
    }
}
