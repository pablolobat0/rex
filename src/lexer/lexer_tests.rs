#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lexer::Lexer;
    use crate::lexer::token::token::{Token, TokenType};

    #[test]
    fn test_next_token_simple() {
        let input = "+-*/(){}=<>";
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::new(TokenType::Plus, "+".to_string(), 1),
            Token::new(TokenType::Minus, "-".to_string(), 1),
            Token::new(TokenType::Star, "*".to_string(), 1),
            Token::new(TokenType::Slash, "/".to_string(), 1),
            Token::new(TokenType::LeftParen, "(".to_string(), 1),
            Token::new(TokenType::RightParen, ")".to_string(), 1),
            Token::new(TokenType::LeftBrace, "{".to_string(), 1),
            Token::new(TokenType::RightBrace, "}".to_string(), 1),
            Token::new(TokenType::Equal, "=".to_string(), 1),
            Token::new(TokenType::Less, "<".to_string(), 1),
            Token::new(TokenType::Greater, ">".to_string(), 1),
        ];

        for expected_token in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(token, expected_token);
        }

        assert_eq!(lexer.next_token().kind, TokenType::EOF);
    }

    #[test]
    fn test_next_token_invalid() {
        let input = "abc";
        let mut lexer = Lexer::new(input);

        let token = lexer.next_token();
        assert_eq!(token.kind, TokenType::Illegal);
    }
}
