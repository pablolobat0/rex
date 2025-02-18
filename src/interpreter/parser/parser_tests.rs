#[cfg(test)]
mod tests {
    use crate::interpreter::parser::parser::Parser;
    use crate::{
        common::lexer::lexer::Lexer,
        common::lexer::token::TokenType,
        interpreter::parser::ast::{Expression, Statement},
    };
    use core::panic;

    fn check_parser_errors(parser: &Parser) {
        let errors = &parser.errors;
        if errors.is_empty() {
            return;
        }

        println!("parser has {} errors", errors.len());
        for error in errors {
            println!("parser error: {}", error);
        }
        panic!("parser errors encountered");
    }

    fn check_parse_expression_statement(input: &str, expected: &str) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);

        assert_eq!(program.to_string(), expected, "input: {}", input);
    }

    #[test]
    fn test_let_statement() {
        let input = ["let x = 5", "let y = 10", "let foobar = 838383"];

        let tests = vec![("x", 5), ("y", 10), ("foobar", 838383)];

        for (i, (name, value)) in tests.iter().enumerate() {
            let lexer = Lexer::new(input[i]);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.statements.len(), 1);
            let stmt = &program.statements[0];

            match stmt {
                Statement::Let(let_stmt) => {
                    assert_eq!(let_stmt.token.kind, TokenType::Let);
                    assert_eq!(let_stmt.identifier.name, name.to_string());
                    assert_eq!(let_stmt.identifier.get_lexeme(), name.to_string());
                    assert_eq!(let_stmt.value.get_lexeme(), value.to_string());
                }
                _ => panic!("stmt is not a LetStatement. Got={:?}", stmt),
            }
        }
    }

    #[test]
    fn test_return_statement() {
        let input = ["return 5", "return 15", "return 55"];

        let tests = vec![("return", 5), ("return", 15), ("return", 55)];

        for (i, (name, value)) in tests.iter().enumerate() {
            let lexer = Lexer::new(input[i]);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.statements.len(), 1);
            let stmt = &program.statements[0];
            match stmt {
                Statement::Return(return_stmt) => {
                    assert_eq!(return_stmt.token.kind, TokenType::Return);
                    assert_eq!(return_stmt.token.lexeme, name.to_string());
                    assert_eq!(return_stmt.token.lexeme, name.to_string());
                    assert_eq!(return_stmt.value.get_lexeme(), value.to_string());
                }
                _ => panic!("stmt is not a LetStatement. Got={:?}", stmt),
            }
        }
    }

    #[test]
    fn test_identifier_statement() {
        let input = ["foobar", "x", "y"];

        let expected_values = vec!["foobar", "x", "y"];

        for (i, value) in expected_values.iter().enumerate() {
            let lexer = Lexer::new(input[i]);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.statements.len(), 1);
            let stmt = &program.statements[0];
            match stmt {
                Statement::Expression(expression_stmt) => match &expression_stmt.expression {
                    Expression::Identifier(identifier_exp) => {
                        assert_eq!(identifier_exp.get_lexeme(), value.to_string());
                    }
                    _ => panic!("stmt is not an Identifier. Got={:?}", stmt),
                },
                _ => panic!("stmt is not an ExpressionStatement. Got={:?}", stmt),
            }
        }
    }

    #[test]
    fn test_integer_literal_statement() {
        let input = ["5", "54", "90"];

        let expected_values = vec!["5", "54", "90"];

        for (i, value) in expected_values.iter().enumerate() {
            let lexer = Lexer::new(input[i]);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.statements.len(), 1);
            let stmt = &program.statements[0];
            match stmt {
                Statement::Expression(expression_stmt) => match &expression_stmt.expression {
                    Expression::Integer(integer_literal) => {
                        assert_eq!(integer_literal.get_lexeme(), value.to_string());
                    }
                    _ => panic!("stmt is not an Integer. Got={:?}", stmt),
                },
                _ => panic!("stmt is not an ExpressionStatement. Got={:?}", stmt),
            }
        }
    }

    #[test]
    fn test_float_literal() {
        let input = ["3.1416", "0.7835"];

        let expected_values = vec!["3.1416", "0.7835"];

        for (i, value) in expected_values.iter().enumerate() {
            let lexer = Lexer::new(input[i]);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.statements.len(), 1);
            let stmt = &program.statements[0];
            match stmt {
                Statement::Expression(expression_stmt) => match &expression_stmt.expression {
                    Expression::Float(float) => {
                        assert_eq!(float.get_lexeme(), value.to_string());
                    }
                    _ => panic!("stmt is not an Float Literal. Got={:?}", stmt),
                },
                _ => panic!("stmt is not an ExpressionStatement. Got={:?}", stmt),
            }
        }
    }

    #[test]
    fn test_boolean_literals() {
        let input = ["false", "true"];

        let expected_values = [false, true];

        for (i, value) in expected_values.iter().enumerate() {
            let lexer = Lexer::new(input[i]);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.statements.len(), 1);
            let stmt = &program.statements[0];
            match stmt {
                Statement::Expression(expression_stmt) => match &expression_stmt.expression {
                    Expression::Boolean(boolean_literal) => {
                        assert_eq!(boolean_literal.value, *value);
                    }
                    _ => panic!("stmt is not an Boolean. Got={:?}", stmt),
                },
                _ => panic!("stmt is not an ExpressionStatement. Got={:?}", stmt),
            }
        }
    }

    #[test]
    fn test_string_literals() {
        let input = ["\"Hola\"", "\"mundo\""];

        let expected_values = vec!["Hola", "mundo"];

        for (i, value) in expected_values.iter().enumerate() {
            let lexer = Lexer::new(input[i]);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            assert_eq!(program.statements.len(), 1);
            check_parser_errors(&parser);
            let stmt = &program.statements[0];
            match stmt {
                Statement::Expression(expression_stmt) => match &expression_stmt.expression {
                    Expression::String(string_literal) => {
                        assert_eq!(string_literal.get_lexeme(), value.to_string());
                    }
                    _ => panic!("stmt is not an String. Got={:?}", stmt),
                },
                _ => panic!("stmt is not an ExpressionStatement. Got={:?}", stmt),
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4\n-5 * 5", "(3 + 4)\n((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
        ];

        for (input, expected) in tests {
            check_parse_expression_statement(&input, expected);
        }
    }

    #[test]
    fn test_if_expression_with_multiple_statements() {
        let input = r#"
        if (x < y) {
            let a = 5
            a
        } else {
            let b = 10
            b
        }
        "#;
        let expected = "if (x < y) {\nlet a = 5\na\n} else {\nlet b = 10\nb\n}";
        check_parse_expression_statement(input, expected);
    }

    #[test]
    fn test_if_expression_without_alternative() {
        let input = r#"
        if (x < y) {
            let a = 5
            a
        }
        "#;
        let expected = "if (x < y) {\nlet a = 5\na\n}";
        check_parse_expression_statement(input, expected);
    }

    #[test]
    fn test_function_literal() {
        let input = "fn(x,\n y) { return 1\n }\n";
        let expected = "fn(x, y) {\nreturn 1\n}";
        check_parse_expression_statement(input, expected);
    }

    #[test]
    fn test_function_literal_no_arguments() {
        let input = "fn() { return 1\n }";
        let expected = "fn() {\nreturn 1\n}";
        check_parse_expression_statement(input, expected);
    }

    #[test]
    fn test_function_literal_with_multiple_statements() {
        let input = "fn(x) { let y = x + 1\n return y\n }\n";
        let expected = "fn(x) {\nlet y = (x + 1)\nreturn y\n}";
        check_parse_expression_statement(input, expected);
    }

    #[test]
    fn test_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5)\n";
        let expected = "add(1, (2 * 3), (4 + 5))";
        check_parse_expression_statement(input, expected);
    }

    #[test]
    fn test_call_expression_no_arguments() {
        let input = "doSomething()";
        let expected = "doSomething()";
        check_parse_expression_statement(input, expected);
    }

    #[test]
    fn test_call_expression_with_function_literal() {
        let input = "callFunction(fn(x) { return x + 1\n }, 5)\n";
        let expected = "callFunction(fn(x) {\nreturn (x + 1)\n}, 5)";
        check_parse_expression_statement(input, expected);
    }

    #[test]
    fn test_parse_while_statement() {
        let input = "while (x < 5) {
                x = x + 1
            }";
        let expected = "while (x < 5) {\n(x = (x + 1))\n}";
        check_parse_expression_statement(input, expected);
    }

    #[test]
    fn test_parse_while_loop_with_if_and_assignment() {
        let input = "while (true) { 
                        if (a == 5) {
                            return 5
                        }
                        a = a + 1
                    }\n";
        let expected = "while true {\nif (a == 5) {\nreturn 5\n}\n(a = (a + 1))\n}";
        check_parse_expression_statement(input, expected);
    }
}
