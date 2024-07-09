use crate::lexer::lexer::Lexer;
use crate::lexer::token::token::{Token, TokenType};
use crate::parser::ast::ast::{Identifier, LetStatement, Program, Statement};
use crate::parser::parser::Parser;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast::ast::{
        Expression, ExpressionStatement, InfixExpression, IntegerLiteral, Node, PrefixExpression,
    };

    #[test]
    fn test_let_and_return_statements() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
        return 5;
        return 15;
        return 55;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 6);

        let tests = vec![
            ("x", 5),
            ("y", 10),
            ("foobar", 838383),
            ("", 5),
            ("", 15),
            ("", 55),
        ];

        for (i, (expected_ident, expected_value)) in tests.iter().enumerate() {
            let stmt = &program.statements[i];
            test_let_and_return_statement(stmt, expected_ident, *expected_value);
        }
    }

    fn test_let_and_return_statement(stmt: &Statement, name: &str, value: i64) {
        match stmt {
            Statement::Let(let_stmt) => {
                assert_eq!(let_stmt.token.kind, TokenType::Let);
                assert_eq!(let_stmt.identifier.name, name);
                assert_eq!(let_stmt.identifier.get_lexeme(), name);
                assert_eq!(let_stmt.value.get_lexeme(), value.to_string());
            }
            Statement::Return(return_stmt) => {
                assert_eq!(return_stmt.token.kind, TokenType::Return);
                assert_eq!(return_stmt.value.get_lexeme(), value.to_string());
            }
            _ => panic!("stmt is not a LetStatement. Got={:?}", stmt),
        }
    }

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

    #[test]
    fn test_identifiers_and_integer_literals_statements() {
        let input = "
        foobar;
        x;
        y;
        5;
        54;
        90;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 6);

        let expected_values = vec!["foobar", "x", "y", "5", "54", "90"];

        for (i, expected_value) in expected_values.iter().enumerate() {
            let stmt = &program.statements[i];
            test_identifier_or_integer_literal(stmt, expected_value);
        }
    }

    fn test_identifier_or_integer_literal(stmt: &Statement, expected_value: &str) {
        match stmt {
            Statement::Expression(expression_stmt) => match &expression_stmt.expression {
                Expression::Identifier(identifier_exp) => {
                    assert_eq!(identifier_exp.get_lexeme(), expected_value);
                }
                Expression::Integer(integer_literal) => {
                    assert_eq!(integer_literal.get_lexeme(), expected_value);
                }
                _ => panic!("stmt is not an Identifier or an Integer. Got={:?}", stmt),
            },
            _ => panic!("stmt is not an ExpressionStatement. Got={:?}", stmt),
        }
    }

    #[test]
    fn test_boolean_literals() {
        let input = "
        false;
        true;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 2);

        let expected_values = vec![false, true];

        for (i, expected_value) in expected_values.iter().enumerate() {
            let stmt = &program.statements[i];
            test_boolean_literal(stmt, expected_value);
        }
    }

    fn test_boolean_literal(stmt: &Statement, expected_value: &bool) {
        match stmt {
            Statement::Expression(expression_stmt) => match &expression_stmt.expression {
                Expression::Boolean(boolean_literal) => {
                    assert_eq!(boolean_literal.value, *expected_value);
                }
                _ => panic!("stmt is not an Bollean. Got={:?}", stmt),
            },
            _ => panic!("stmt is not an ExpressionStatement. Got={:?}", stmt),
        }
    }

    #[test]
    fn test_prefix_expressions() {
        let prefix_tests = vec![("-5", "-", 5), ("!5", "!", 5)];

        for (input, operator, value) in prefix_tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(&parser);

            assert_eq!(program.statements.len(), 1, "Expected 1 statement");

            let stmt = &program.statements[0];
            match stmt {
                Statement::Expression(ExpressionStatement { expression, .. }) => match expression {
                    Expression::Prefix(PrefixExpression {
                        operator: op,
                        right,
                        ..
                    }) => {
                        assert_eq!(
                            op, operator,
                            "Expected operator '{}', got '{}'",
                            operator, op
                        );
                        match right.as_ref() {
                            Expression::Integer(IntegerLiteral {
                                value: int_value, ..
                            }) => {
                                assert_eq!(
                                    *int_value, value,
                                    "Expected value '{}', got '{}'",
                                    value, int_value
                                );
                            }
                            _ => panic!("Expected integer literal, got {:?}", right),
                        }
                    }
                    _ => panic!("Expected prefix expression, got {:?}", expression),
                },
                _ => panic!("Expected expression statement, got {:?}", stmt),
            }
        }
    }

    #[test]
    fn test_infix_expressions() {
        let infix_tests = vec![("6-5", "-", 6, 5), ("10*5", "*", 10, 5)];

        for (input, operator, left_value, right_value) in infix_tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(&parser);

            assert_eq!(program.statements.len(), 1, "Expected 1 statement");

            let stmt = &program.statements[0];
            match stmt {
                Statement::Expression(ExpressionStatement { expression, .. }) => match expression {
                    Expression::Infix(InfixExpression {
                        left,
                        operator: op,
                        right,
                        ..
                    }) => {
                        match left.as_ref() {
                            Expression::Integer(IntegerLiteral {
                                value: int_value, ..
                            }) => {
                                assert_eq!(
                                    *int_value, left_value,
                                    "Expected value '{}', got '{}'",
                                    left_value, int_value
                                );
                            }
                            _ => panic!("Expected integer literal, got {:?}", right),
                        }
                        assert_eq!(
                            op, operator,
                            "Expected operator '{}', got '{}'",
                            operator, op
                        );
                        match right.as_ref() {
                            Expression::Integer(IntegerLiteral {
                                value: int_value, ..
                            }) => {
                                assert_eq!(
                                    *int_value, right_value,
                                    "Expected value '{}', got '{}'",
                                    right_value, int_value
                                );
                            }
                            _ => panic!("Expected integer literal, got {:?}", right),
                        }
                    }
                    _ => panic!("Expected prefix expression, got {:?}", expression),
                },
                _ => panic!("Expected expression statement, got {:?}", stmt),
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b", "((-a) * b);"),
            ("!-a", "(!(-a));"),
            ("a + b + c", "((a + b) + c);"),
            ("a + b - c", "((a + b) - c);"),
            ("a * b * c", "((a * b) * c);"),
            ("a * b / c", "((a * b) / c);"),
            ("a + b / c", "(a + (b / c));"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f);"),
            ("3 + 4; -5 * 5", "(3 + 4);\n((-5) * 5);"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4));"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4));"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
            ),
            ("true", "true;"),
            ("false", "false;"),
            ("3 > 5 == false", "((3 > 5) == false);"),
            ("3 < 5 == true", "((3 < 5) == true);"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4);"),
            ("(5 + 5) * 2", "((5 + 5) * 2);"),
            ("2 / (5 + 5)", "(2 / (5 + 5));"),
            ("-(5 + 5)", "(-(5 + 5));"),
            ("!(true == true)", "(!(true == true));"),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            let actual = program.to_string();
            check_parse_expression_statement(&actual, expected);
        }
    }

    #[test]
    fn test_if_expression_with_multiple_statements() {
        let input = r#"
        if (x < y) {
            let a = 5;
            a;
        } else {
            let b = 10;
            b;
        }
        "#;
        let expected = "if (x < y) { let a = 5;\na; } else { let b = 10;\nb; }";
        check_parse_expression_statement(input, expected);
    }

    #[test]
    fn test_if_expression_without_alternative() {
        let input = r#"
        if (x < y) {
            let a = 5;
            a;
        }
        "#;
        let expected = "if (x < y) { let a = 5;\na; }";
        check_parse_expression_statement(input, expected);
    }

    fn check_parse_expression_statement(input: &str, expected: &str) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);

        assert_eq!(program.to_string(), expected, "input: {}", input);
    }

    #[test]
    fn test_function_literal() {
        let input = "fn(x, y) { return 1; }";
        let expected = "fn(x, y){return 1;}";
        check_parse_expression_statement(input, expected);
    }

    #[test]
    fn test_function_literal_no_arguments() {
        let input = "fn() { return 1; }";
        let expected = "fn(){return 1;}";
        check_parse_expression_statement(input, expected);
    }

    #[test]
    fn test_function_literal_with_multiple_statements() {
        let input = "fn(x) { let y = x + 1; return y; }";
        let expected = "fn(x){let y = (x + 1);\nreturn y;}";
        check_parse_expression_statement(input, expected);
    }
}
