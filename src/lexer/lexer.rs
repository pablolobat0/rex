use crate::lexer::token::token::{Token, TokenType};

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    current_char: Option<char>,
    line: u32,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input,
            position: 0,
            current_char: input.chars().nth(0),
            line: 1,
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
            Some('+') => {
                self.read_char();
                Token::new(TokenType::Plus, "+".to_string(), self.line)
            }
            Some('-') => {
                self.read_char();
                Token::new(TokenType::Minus, "-".to_string(), self.line)
            }
            Some('*') => {
                self.read_char();
                Token::new(TokenType::Star, "*".to_string(), self.line)
            }
            Some('/') => {
                self.read_char();
                Token::new(TokenType::Slash, "/".to_string(), self.line)
            }
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
            Some('=') => {
                self.read_char();
                Token::new(TokenType::Equal, "=".to_string(), self.line)
            }
            Some('<') => {
                self.read_char();
                Token::new(TokenType::Less, "<".to_string(), self.line)
            }
            Some('>') => {
                self.read_char();
                Token::new(TokenType::Greater, ">".to_string(), self.line)
            }
            None => Token::new(TokenType::EOF, "".to_string(), self.line),
            Some(_) => Token::new(TokenType::Illegal, "".to_string(), self.line),
        };

        token
    }
}
