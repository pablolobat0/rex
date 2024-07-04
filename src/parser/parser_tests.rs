use crate::lexer::lexer::Lexer;
use crate::lexer::token::token::{Token, TokenType};
use crate::parser::ast::ast::{Identifier, LetStatement, Program, Statement};
use crate::parser::parser::Parser;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast::ast::Node;

    #[test]
    fn test_let_statements() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 3);

        let tests = vec![("x", 5), ("y", 10), ("foobar", 838383)];

        for (i, (expected_ident, expected_value)) in tests.iter().enumerate() {
            let stmt = &program.statements[i];
            test_let_statement(stmt, expected_ident, *expected_value);
        }
    }

    fn test_let_statement(stmt: &Statement, name: &str, value: i64) {
        match stmt {
            Statement::Let(let_stmt) => {
                assert_eq!(let_stmt.token.kind, TokenType::Let);
                assert_eq!(let_stmt.name.value, name);
                assert_eq!(let_stmt.name.get_lexeme(), name);
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
    fn test_return_statements() {
        let input = "
                    return 5;
                    return 10;
                    return 15;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 3);

        for i in 0..program.statements.len() {
            let stmt = &program.statements[i];
            test_return_statement(stmt);
        }
    }
    fn test_return_statement(stmt: &Statement) {
        match stmt {
            Statement::Return(return_stmt) => {
                assert_eq!(return_stmt.token.kind, TokenType::Return);
            }
            _ => panic!("stmt is not a ReturnStatement. Got={:?}", stmt),
        }
    }
}
