use crate::lexer::lexer::Lexer;
use crate::lexer::token::{Token, TokenType};
use crate::parser::ast::{
    Expression, ExpressionStatement, Identifier, InfixExpression, IntegerLiteral, LetStatement,
    Node, PrefixExpression, Program, Statement,
};
use crate::parser::parser::Parser;

#[cfg(test)]
mod tests {
    use core::panic;

    use super::*;

    #[test]
    fn test_let_and_return_statements() {
        let input = "
        let x = 5
        let y = 10
        let foobar = 838383
        return 5
        return 15
        return 55
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

    #[test]
    fn test_parse_while_statement() {
        let input = r#"
            while (x < 5) {
                x = x + 1
            }
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        // Parse the program
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "Expected 1 statement, got {}",
            program.statements.len()
        );

        let while_stmt = match &program.statements[0] {
            Statement::While(stmt) => stmt,
            _ => panic!("Expected while statement"),
        };

        assert_eq!(
            while_stmt.condition.to_string(),
            "(x < 5)",
            "Expected condition to be '(x < 5)', got '{}'",
            while_stmt.condition.to_string()
        );

        assert_eq!(
            while_stmt.body.statements.len(),
            1,
            "Expected 1 statement in body, got {}",
            while_stmt.body.statements.len()
        );
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
        foobar
        x
        y
        5
        54
        90
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
    fn test_parse_while_loop_with_if_and_assignment() {
        let input = "while (true) { 
                        if (a == 5) {
                            return 5
                        }
                        a = a + 1
                    }\n";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);

        // Verificar el número de statements
        assert_eq!(
            program.statements.len(),
            1,
            "El programa debe contener un statement"
        );

        // Verificar el statement while
        match &program.statements[0] {
            Statement::While(while_stmt) => {
                // Verificar la condición
                match &while_stmt.condition {
                    Expression::Boolean(bool_expr) => {
                        assert!(bool_expr.value, "Se esperaba que la condición fuera true")
                    }
                    _ => panic!("Se esperaba una expresión booleana en la condición del while"),
                }

                // Verificar el bloque de statements dentro del while
                let block_stmt = &while_stmt.body;
                assert_eq!(
                    block_stmt.statements.len(),
                    2,
                    "El bloque debe contener dos statements"
                );

                // Verificar el primer statement en el bloque: if (a == 5) { return 5; }
                match &block_stmt.statements[0] {
                    Statement::Expression(expr_stmt) => {
                        match &expr_stmt.expression {
                            Expression::If(if_expr) => {
                                // Verificar la condición del if
                                match &*if_expr.condition {
                                    Expression::Infix(infix_expr) => {
                                        match &*infix_expr.left {
                                            Expression::Identifier(ident) => assert_eq!(ident.name, "a", "Se esperaba el identificador 'a'"),
                                            _ => panic!("Se esperaba un identificador en el lado izquierdo de la expresión infija"),
                                        }
                                        assert_eq!(
                                            infix_expr.operator, "==",
                                            "Se esperaba el operador '=='"
                                        );
                                        match &*infix_expr.right {
                                            Expression::Integer(int_expr) => assert_eq!(int_expr.value, 5, "Se esperaba el valor entero 5"),
                                            _ => panic!("Se esperaba una expresión entera en el lado derecho de la expresión infija"),
                                        }
                                    }
                                    _ => panic!(
                                        "Se esperaba una expresión infija en la condición del if"
                                    ),
                                }

                                // Verificar el return statement dentro del bloque del if
                                let consequence = if_expr.consequence.clone();
                                let stmt = &consequence.statements[0];
                                match stmt {
                                        Statement::Return(return_stmt) => {
                                            match &return_stmt.value {
                                                Expression::Integer(int_expr) => assert_eq!(int_expr.value, 5, "Se esperaba el valor de retorno 5"),
                                                _ => panic!("Se esperaba una expresión entera en el statement de retorno"),
                                            }
                                        }
                                        _ => panic!("Se esperaba un statement de retorno dentro del bloque del if"),
                                    }
                            }
                            _ => panic!("Se esperaba un statement if dentro del while"),
                        }
                    }
                    _ => {
                        panic!("Se esperaba un statement de expresión dentro del bloque del while")
                    }
                }

                // Verificar el segundo statement en el bloque: a = a + 1;
                match &block_stmt.statements[1] {
                    Statement::Expression(expr_stmt) => match &expr_stmt.expression {
                        Expression::Infix(infix_expr) => {
                            match &*infix_expr.left {
                                    Expression::Identifier(ident) => assert_eq!(ident.name, "a", "Se esperaba el identificador 'a'"),
                                    _ => panic!("Se esperaba un identificador en el lado izquierdo de la expresión infija"),
                                }
                            assert_eq!(infix_expr.operator, "=", "Se esperaba el operador '='");
                            match &*infix_expr.right {
                                    Expression::Infix(infix_expr) => {
                                        match &*infix_expr.left {
                                            Expression::Identifier(ident) => assert_eq!(ident.name, "a", "Se esperaba el identificador 'a' en el lado izquierdo de la adición"),
                                            _ => panic!("Se esperaba un identificador en el lado izquierdo de la adición"),
                                        }
                                        assert_eq!(infix_expr.operator, "+", "Se esperaba el operador '+'");
                                        match &*infix_expr.right {
                                            Expression::Integer(int_expr) => assert_eq!(int_expr.value, 1, "Se esperaba el valor entero 1"),
                                            _ => panic!("Se esperaba una expresión entera en el lado derecho de la adición"),
                                        }
                                    }
                                    _ => panic!("Se esperaba una expresión infija en el lado derecho de la asignación"),
                                }
                        }
                        _ => {
                            panic!("Se esperaba una expresión infija en el statement de asignación")
                        }
                    },
                    _ => {
                        panic!("Se esperaba un statement de expresión dentro del bloque del while")
                    }
                }
            }
            _ => panic!("Se esperaba un statement while en el programa"),
        }
    }

    #[test]
    fn test_boolean_literals() {
        let input = "
        false
        true
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
    fn test_string_literals() {
        let input = "
        \"Hola\"
        \"mundo\"
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 2);

        let expected_values = vec!["Hola", "mundo"];

        for (i, expected_value) in expected_values.iter().enumerate() {
            let stmt = &program.statements[i];
            test_string_literal(stmt, expected_value);
        }
    }

    fn test_string_literal(stmt: &Statement, expected_value: &str) {
        match stmt {
            Statement::Expression(expression_stmt) => match &expression_stmt.expression {
                Expression::String(string_literal) => {
                    assert_eq!(string_literal.get_lexeme(), expected_value);
                }
                _ => panic!("stmt is not an String. Got={:?}", stmt),
            },
            _ => panic!("stmt is not an ExpressionStatement. Got={:?}", stmt),
        }
    }
    #[test]
    fn test_prefix_expressions() {
        let prefix_tests = vec![("-5\n", "-", 5), ("!5\n", "!", 5)];

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
        let infix_tests = vec![("6-5\n", "-", 6, 5), ("10*5\n", "*", 10, 5)];

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

    fn check_parse_expression_statement(input: &str, expected: &str) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);

        assert_eq!(program.to_string(), expected, "input: {}", input);
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
}
