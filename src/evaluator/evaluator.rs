use super::object::object::Object;
use crate::parser::ast::ast::{Expression, Node, Statement};

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

pub fn eval(node: Node) -> Object {
    match node {
        Node::Program(program) => eval_statements(program.statements),
        Node::Expression(expression) => eval_expression(expression),
        Node::Statement(statement) => eval_statement(statement),
    }
}

fn eval_statements(statements: Vec<Statement>) -> Object {
    let mut result = NULL;
    for statement in statements {
        result = eval(Node::Statement(statement));
    }
    result
}

fn eval_expression(expression: Expression) -> Object {
    match expression {
        Expression::Integer(integer) => Object::Integer(integer.value),
        Expression::Boolean(boolean) => eval_boolean(boolean.value),
        Expression::Prefix(prefix_expression) => {
            let right = eval(Node::Expression(*prefix_expression.right));
            return eval_prefix_expression(&prefix_expression.operator, right);
        }
        _ => todo!(),
    }
}

fn eval_boolean(boolean: bool) -> Object {
    if boolean {
        TRUE
    } else {
        FALSE
    }
}

fn eval_prefix_expression(operator: &str, right: Object) -> Object {
    match operator {
        "!" => eval_bang_operator(right),
        "-" => eval_minus_prefix_operator(right),
        _ => todo!(),
    }
}

fn eval_bang_operator(object: Object) -> Object {
    match object {
        Object::Boolean(boolean) => eval_boolean(!boolean),
        _ => NULL,
    }
}

fn eval_minus_prefix_operator(object: Object) -> Object {
    match object {
        Object::Integer(value) => Object::Integer(-value),
        _ => NULL,
    }
}

fn eval_statement(statement: Statement) -> Object {
    match statement {
        Statement::Expression(expression_statement) => {
            return eval(Node::Expression(expression_statement.expression))
        }
        _ => todo!(),
    }
}
