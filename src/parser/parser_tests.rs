use crate::lexer::lexer::Lexer;
use crate::lexer::token::token::{Token, TokenType};
use crate::parser::ast::ast::{Identifier, LetStatement, Program, Statement};
use crate::parser::parser::Parser;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast::ast::{Expression, ExpressionStatement, Node};

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
            },
            _ => panic!("stmt is not an ExpressionStatement. Got={:?}", stmt),
        }
    }
}
