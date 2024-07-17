#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lexer::Lexer;
    use crate::lexer::token::token::{Token, TokenType};

    #[test]
    fn test_next_token() {
        let input = "let var = 15;
                    let result = var + 15;
                    fn max(num1, num2) {
                        if num1 >= num2 {
                            return num1;
                        } else {
                            return num2;
                        }
                    }
                    \"Hola mundo\"
                    ";

        let expected_tokens = vec![
            Token::new(TokenType::Let, "let".to_string(), 1),
            Token::new(TokenType::Identifier, "var".to_string(), 1),
            Token::new(TokenType::Equal, "=".to_string(), 1),
            Token::new(TokenType::Integer, "15".to_string(), 1),
            Token::new(TokenType::Semicolon, ";".to_string(), 1),
            Token::new(TokenType::Let, "let".to_string(), 2),
            Token::new(TokenType::Identifier, "result".to_string(), 2),
            Token::new(TokenType::Equal, "=".to_string(), 2),
            Token::new(TokenType::Identifier, "var".to_string(), 2),
            Token::new(TokenType::Plus, "+".to_string(), 2),
            Token::new(TokenType::Integer, "15".to_string(), 2),
            Token::new(TokenType::Semicolon, ";".to_string(), 2),
            Token::new(TokenType::Function, "fn".to_string(), 3),
            Token::new(TokenType::Identifier, "max".to_string(), 3),
            Token::new(TokenType::LeftParen, "(".to_string(), 3),
            Token::new(TokenType::Identifier, "num1".to_string(), 3),
            Token::new(TokenType::Comma, ",".to_string(), 3),
            Token::new(TokenType::Identifier, "num2".to_string(), 3),
            Token::new(TokenType::RightParen, ")".to_string(), 3),
            Token::new(TokenType::LeftBrace, "{".to_string(), 3),
            Token::new(TokenType::If, "if".to_string(), 4),
            Token::new(TokenType::Identifier, "num1".to_string(), 4),
            Token::new(TokenType::GreaterEqual, ">=".to_string(), 4),
            Token::new(TokenType::Identifier, "num2".to_string(), 4),
            Token::new(TokenType::LeftBrace, "{".to_string(), 4),
            Token::new(TokenType::Return, "return".to_string(), 5),
            Token::new(TokenType::Identifier, "num1".to_string(), 5),
            Token::new(TokenType::Semicolon, ";".to_string(), 5),
            Token::new(TokenType::RightBrace, "}".to_string(), 6),
            Token::new(TokenType::Else, "else".to_string(), 6),
            Token::new(TokenType::LeftBrace, "{".to_string(), 6),
            Token::new(TokenType::Return, "return".to_string(), 7),
            Token::new(TokenType::Identifier, "num2".to_string(), 7),
            Token::new(TokenType::Semicolon, ";".to_string(), 7),
            Token::new(TokenType::RightBrace, "}".to_string(), 8),
            Token::new(TokenType::RightBrace, "}".to_string(), 9),
            Token::new(TokenType::String, "\"Hola mundo\"".to_string(), 10),
        ];

        test_lexer(input, expected_tokens);
    }

    fn test_lexer(input: &str, expected_tokens: Vec<Token>) {
        let mut lexer = Lexer::new(input);
        for expected_token in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(token, expected_token);
        }

        assert_eq!(lexer.next_token().kind, TokenType::EOF);
    }
}
