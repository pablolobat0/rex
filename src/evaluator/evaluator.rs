use crate::parser::ast::ast::{Expression, Node, Statement};

use super::object::object::Object;

pub fn eval(node: Node) -> Object {
    match node {
        Node::Program(program) => eval_statements(program.statements),
        Node::Expression(expression) => eval_expression(expression),
        Node::Statement(statement) => eval_statement(statement),
    }
}

fn eval_statements(statements: Vec<Statement>) -> Object {
    let mut result = Object::Null;
    for statement in statements {
        result = eval(Node::Statement(statement));
    }
    result
}

fn eval_expression(expression: Expression) -> Object {
    match expression {
        Expression::Integer(integer) => Object::Integer(integer.value),
        Expression::Boolean(boolean) => Object::Boolean(boolean.value),
        _ => todo!(),
    }
}

fn eval_statement(statement: Statement) -> Object {
    match statement {
        Statement::Expression(expression_statement) => {
            eval(Node::Expression(expression_statement.expression))
        }
        _ => todo!(),
    }
}
