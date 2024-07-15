use super::object::object::Object;
use crate::parser::ast::ast::{Expression, IfExpression, Node, Statement};

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

pub fn eval(node: Node) -> Object {
    match node {
        Node::Program(program) => eval_program_statements(program.statements),
        Node::Expression(expression) => eval_expression(expression),
        Node::Statement(statement) => eval_statement(statement),
    }
}

fn eval_program_statements(statements: Vec<Statement>) -> Object {
    let mut result = NULL;
    for statement in statements {
        result = eval(Node::Statement(statement));
        match result {
            Object::Return(return_value) => return *return_value,
            Object::Error(_) => return result,
            _ => continue,
        }
    }
    result
}

fn eval_expression(expression: Expression) -> Object {
    match expression {
        Expression::Integer(integer) => Object::Integer(integer.value),
        Expression::Boolean(boolean) => eval_boolean(boolean.value),
        Expression::Prefix(prefix_expression) => {
            let right = eval(Node::Expression(*prefix_expression.right));
            if is_error(&right) {
                return right;
            }

            return eval_prefix_expression(&prefix_expression.operator, right);
        }
        Expression::Infix(infix_expression) => {
            let right = eval(Node::Expression(*infix_expression.right));
            if is_error(&right) {
                return right;
            }
            let left = eval(Node::Expression(*infix_expression.left));
            if is_error(&left) {
                return left;
            }
            return eval_infix_expression(left, &infix_expression.operator, right);
        }
        Expression::If(if_expression) => eval_if_expression(if_expression),
        _ => Object::Error(format!("unkown expression: {}", expression.to_string())),
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
        _ => Object::Error(format!(
            "unkown operator: {}{}",
            operator,
            right.object_type()
        )),
    }
}

fn eval_bang_operator(object: Object) -> Object {
    match object {
        Object::Boolean(boolean) => eval_boolean(!boolean),
        _ => Object::Error(format!(
            "type mismatch, expected a BOOLEAN but found {}",
            object.object_type()
        )),
    }
}

fn eval_minus_prefix_operator(object: Object) -> Object {
    match object {
        Object::Integer(value) => Object::Integer(-value),
        _ => Object::Error(format!(
            "type mismatch, expected an INTEGER but found {}",
            object.object_type()
        )),
    }
}

fn eval_infix_expression(left: Object, operator: &str, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(left_value), Object::Integer(right_value)) => {
            eval_integer_infix_expression(left_value, operator, right_value)
        }
        (Object::Boolean(left_value), Object::Boolean(right_value)) => {
            eval_boolean_infix_expression(left_value, operator, right_value)
        }
        (left, right) => Object::Error(format!(
            "type mismatch: {} {} {}",
            left.object_type(),
            operator,
            right.object_type()
        )),
    }
}

fn eval_integer_infix_expression(left_value: i64, operator: &str, right_value: i64) -> Object {
    match operator {
        "+" => Object::Integer(left_value + right_value),
        "-" => Object::Integer(left_value - right_value),
        "*" => Object::Integer(left_value * right_value),
        "/" => {
            if right_value != 0 {
                Object::Integer(left_value / right_value)
            } else {
                Object::Error(format!("error division by 0"))
            }
        }
        "==" => eval_boolean(left_value == right_value),
        "!=" => eval_boolean(left_value != right_value),
        ">" => eval_boolean(left_value > right_value),
        ">=" => eval_boolean(left_value >= right_value),
        "<" => eval_boolean(left_value < right_value),
        "<=" => eval_boolean(left_value <= right_value),
        _ => Object::Error(format!("unknow operator: {}", operator)),
    }
}

fn eval_boolean_infix_expression(left_value: bool, operator: &str, right_value: bool) -> Object {
    match operator {
        "==" => eval_boolean(left_value == right_value),
        "!=" => eval_boolean(left_value != right_value),
        _ => Object::Error(format!("unknow operator: {}", operator)),
    }
}

fn eval_if_expression(node: IfExpression) -> Object {
    let condition = eval(Node::Expression(*node.condition));
    match condition {
        Object::Boolean(_) => {}
        _ => {
            return Object::Error(format!(
                "type mismatch, expected a BOOLEAN but found {}",
                condition.object_type()
            ))
        }
    }
    if is_truthy(&condition) {
        eval(Node::Statement(Statement::Block(node.consequence)))
    } else if let Some(alternative) = node.alternative {
        eval(Node::Statement(Statement::Block(alternative)))
    } else {
        NULL
    }
}

fn is_truthy(object: &Object) -> bool {
    match object {
        Object::Boolean(value) => *value,
        Object::Null => false,
        _ => false,
    }
}

fn eval_statement(statement: Statement) -> Object {
    match statement {
        Statement::Expression(expression_statement) => {
            return eval(Node::Expression(expression_statement.expression))
        }
        Statement::Return(return_statement) => {
            let return_value = eval(Node::Expression(return_statement.value));
            Object::Return(Box::new(return_value))
        }
        Statement::Block(block_statement) => eval_block_statements(block_statement.statements),
        _ => todo!(),
    }
}

fn eval_block_statements(statements: Vec<Statement>) -> Object {
    let mut result = NULL;
    for statement in statements {
        result = eval(Node::Statement(statement));
        match result {
            Object::Return(_) => return result,
            Object::Error(_) => return result,
            _ => continue,
        }
    }
    result
}

fn is_error(object: &Object) -> bool {
    match object {
        Object::Error(_) => true,
        _ => false,
    }
}
