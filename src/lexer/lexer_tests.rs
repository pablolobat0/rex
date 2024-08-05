#[cfg(test)]
mod tests {
    use crate::lexer::lexer::Lexer;
    use crate::lexer::token::{Token, TokenType};

    #[test]
    fn test_next_token() {
        let input = "let var = 15
                    let result = var + 15
                    fn max(num1, num2) {
                        if num1 >= num2 {
                            return num1
                        } else {
                            return num2
                        }
                    }
                    \"Hola mundo\"
                    // Esto es un comentario
                    1
                    /* Esto es un comentario de
                    dos líneas */
                    2
                    let café = 42
                    3.14
                    .14
            ";

        let expected_tokens = vec![
            Token::new(TokenType::Let, "let".to_string(), 1),
            Token::new(TokenType::Identifier, "var".to_string(), 1),
            Token::new(TokenType::Equal, "=".to_string(), 1),
            Token::new(TokenType::Integer, "15".to_string(), 1),
            Token::new(TokenType::NewLine, "\n".to_string(), 1),
            Token::new(TokenType::Let, "let".to_string(), 2),
            Token::new(TokenType::Identifier, "result".to_string(), 2),
            Token::new(TokenType::Equal, "=".to_string(), 2),
            Token::new(TokenType::Identifier, "var".to_string(), 2),
            Token::new(TokenType::Plus, "+".to_string(), 2),
            Token::new(TokenType::Integer, "15".to_string(), 2),
            Token::new(TokenType::NewLine, "\n".to_string(), 2),
            Token::new(TokenType::Function, "fn".to_string(), 3),
            Token::new(TokenType::Identifier, "max".to_string(), 3),
            Token::new(TokenType::LeftParen, "(".to_string(), 3),
            Token::new(TokenType::Identifier, "num1".to_string(), 3),
            Token::new(TokenType::Comma, ",".to_string(), 3),
            Token::new(TokenType::Identifier, "num2".to_string(), 3),
            Token::new(TokenType::RightParen, ")".to_string(), 3),
            Token::new(TokenType::LeftBrace, "{".to_string(), 3),
            Token::new(TokenType::NewLine, "\n".to_string(), 3),
            Token::new(TokenType::If, "if".to_string(), 4),
            Token::new(TokenType::Identifier, "num1".to_string(), 4),
            Token::new(TokenType::GreaterEqual, ">=".to_string(), 4),
            Token::new(TokenType::Identifier, "num2".to_string(), 4),
            Token::new(TokenType::LeftBrace, "{".to_string(), 4),
            Token::new(TokenType::NewLine, "\n".to_string(), 4),
            Token::new(TokenType::Return, "return".to_string(), 5),
            Token::new(TokenType::Identifier, "num1".to_string(), 5),
            Token::new(TokenType::NewLine, "\n".to_string(), 5),
            Token::new(TokenType::RightBrace, "}".to_string(), 6),
            Token::new(TokenType::Else, "else".to_string(), 6),
            Token::new(TokenType::LeftBrace, "{".to_string(), 6),
            Token::new(TokenType::NewLine, "\n".to_string(), 6),
            Token::new(TokenType::Return, "return".to_string(), 7),
            Token::new(TokenType::Identifier, "num2".to_string(), 7),
            Token::new(TokenType::NewLine, "\n".to_string(), 7),
            Token::new(TokenType::RightBrace, "}".to_string(), 8),
            Token::new(TokenType::NewLine, "\n".to_string(), 8),
            Token::new(TokenType::RightBrace, "}".to_string(), 9),
            Token::new(TokenType::NewLine, "\n".to_string(), 9),
            Token::new(TokenType::String, "Hola mundo".to_string(), 10),
            Token::new(TokenType::NewLine, "\n".to_string(), 10),
            Token::new(TokenType::Integer, "1".to_string(), 12),
            Token::new(TokenType::NewLine, "\n".to_string(), 12),
            Token::new(TokenType::NewLine, "\n".to_string(), 14),
            Token::new(TokenType::Integer, "2".to_string(), 15),
            Token::new(TokenType::NewLine, "\n".to_string(), 15),
            Token::new(TokenType::Let, "let".to_string(), 16),
            Token::new(TokenType::Identifier, "café".to_string(), 16),
            Token::new(TokenType::Equal, "=".to_string(), 16),
            Token::new(TokenType::Integer, "42".to_string(), 16),
            Token::new(TokenType::NewLine, "\n".to_string(), 16),
            Token::new(TokenType::Float, "3.14".to_string(), 17),
            Token::new(TokenType::NewLine, "\n".to_string(), 17),
            Token::new(TokenType::Float, ".14".to_string(), 18),
            Token::new(TokenType::NewLine, "\n".to_string(), 18),
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
