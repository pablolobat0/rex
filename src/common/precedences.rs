use std::collections::HashMap;

use super::lexer::token::TokenType;

// Precedence order in parsing
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,      // default value
    Assigment,   // =
    Equals,      // ==, !=
    LessGreater, // >, <, >=, <=
    Sum,         // +, -
    Product,     // *, /
    Prefix,      // -X, !X
    Call,        // myFunction(X)
}

pub fn create_precedences() -> HashMap<TokenType, Precedence> {
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
