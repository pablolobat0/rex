use std::{cell::RefCell, fmt::format, rc::Rc};

use super::object::{Environment, Function, Object};
use crate::parser::ast::{Expression, Identifier, IfExpression, Node, Statement, WhileStatement};

// Only need to be instantiated once
const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

pub fn eval(node: Node, environment: &mut Environment) -> Object {
    match node {
        Node::Program(program) => eval_program_statements(program.statements, environment),
        Node::Expression(expression) => eval_expression(expression, environment),
        Node::Statement(statement) => eval_statement(statement, environment),
    }
}

fn eval_program_statements(statements: Vec<Statement>, environment: &mut Environment) -> Object {
    let mut result = NULL;

    for statement in statements {
        result = eval(Node::Statement(statement), environment);
        match result {
            // When find a return, stop evaluating and return the value
            Object::Return(return_value) => return *return_value,
            Object::Error(_) => return result,
            _ => continue,
        }
    }

    result
}

fn eval_expression(expression: Expression, environment: &mut Environment) -> Object {
    match expression {
        Expression::Integer(integer) => Object::Integer(integer.value),
        Expression::Identifier(identifier) => eval_identifier(identifier, environment),
        Expression::Boolean(boolean) => eval_boolean(boolean.value),
        Expression::String(string) => Object::String(string.value),
        Expression::Prefix(prefix_expression) => {
            let right = eval(Node::Expression(*prefix_expression.right), environment);
            if is_error(&right) {
                return right;
            }

            return eval_prefix_expression(&prefix_expression.operator, right);
        }
        Expression::Infix(infix_expression) => {
            let right = eval(Node::Expression(*infix_expression.right), environment);
            if is_error(&right) {
                return right;
            }

            let left = eval(
                Node::Expression(*infix_expression.left.clone()),
                environment,
            );
            if is_error(&left) {
                return left;
            }

            if infix_expression.operator == "=".to_string() {
                match *infix_expression.left {
                    Expression::Identifier(identifier) => {
                        environment.set(&identifier.name, right.clone());
                        return NULL;
                    }
                    _ => return Object::Error(format!("Expected Identifier")),
                }
            }

            return eval_infix_expression(left, &infix_expression.operator, right);
        }
        Expression::If(if_expression) => eval_if_expression(if_expression, environment),
        Expression::Function(function_literal) => {
            return Object::Function(Function::new(
                function_literal.parameters,
                function_literal.body,
                environment.clone(),
            ))
        }
        Expression::Call(call_expression) => {
            let function = eval(Node::Expression(*call_expression.function), environment);
            if is_error(&function) {
                return function;
            }

            let arguments = eval_expressions(call_expression.arguments, environment);

            if arguments.len() == 1 && is_error(&arguments[0]) {
                return arguments[0].clone();
            }

            apply_function(function, arguments)
        }
    }
}

fn eval_identifier(identifier: Identifier, environment: &mut Environment) -> Object {
    let object = environment.get(&identifier.name);

    match object {
        Some(object) => object.clone(),
        None => Object::Error(format!("identifier not found: {}", identifier.name)),
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
        (Object::String(left_value), Object::String(right_value)) => {
            eval_string_infix_expression(left_value, operator, right_value)
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

fn eval_string_infix_expression(left_value: String, operator: &str, right_value: String) -> Object {
    match operator {
        "+" => Object::String(format!("{}{}", left_value, right_value)),
        "==" => eval_boolean(left_value == right_value),
        "!=" => eval_boolean(left_value != right_value),
        _ => Object::Error(format!("unknow operator: {}", operator)),
    }
}

fn eval_if_expression(node: IfExpression, environment: &mut Environment) -> Object {
    let condition = eval(Node::Expression(*node.condition), environment);
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
        eval(
            Node::Statement(Statement::Block(node.consequence)),
            environment,
        )
    } else if let Some(alternative) = node.alternative {
        eval(Node::Statement(Statement::Block(alternative)), environment)
    } else {
        NULL
    }
}

fn is_truthy(object: &Object) -> bool {
    match object {
        Object::Boolean(value) => *value,
        _ => false,
    }
}

fn eval_expressions(expressions: Vec<Expression>, environment: &mut Environment) -> Vec<Object> {
    let mut result = vec![];

    for expression in expressions {
        let object = eval(Node::Expression(expression), environment);

        if is_error(&object) {
            return vec![object];
        }

        result.push(object);
    }

    result
}

fn apply_function(function: Object, arguments: Vec<Object>) -> Object {
    match function {
        Object::Function(function) => {
            let mut extended_env = extend_function_env(
                &function,
                arguments,
                Rc::new(RefCell::new(function.environment.clone())),
            );

            let evaluated_body = eval(
                Node::Statement(Statement::Block(function.body)),
                &mut extended_env,
            );
            // We need to unwrap the value inside the return object
            unwrap_return_value(evaluated_body)
        }
        _ => {
            return Object::Error(format!(
                "expected FUNCTION, found: {}",
                function.object_type()
            ))
        }
    }
}

// Extends the function environment with the arguments
fn extend_function_env(
    function: &Function,
    arguments: Vec<Object>,
    environment: Rc<RefCell<Environment>>,
) -> Environment {
    let mut extended_env = Environment::new_enclosed(environment);

    for (i, parameter) in function.parameters.iter().enumerate() {
        extended_env.set(&parameter.name, arguments[i].clone());
    }

    extended_env
}

fn unwrap_return_value(object: Object) -> Object {
    match object {
        Object::Return(return_object) => *return_object,
        _ => object,
    }
}

fn eval_statement(statement: Statement, environment: &mut Environment) -> Object {
    match statement {
        Statement::Expression(expression_statement) => {
            return eval(
                Node::Expression(expression_statement.expression),
                environment,
            )
        }
        Statement::Return(return_statement) => {
            let return_value = eval(Node::Expression(return_statement.value), environment);
            Object::Return(Box::new(return_value))
        }
        Statement::Let(let_statement) => {
            let value = eval(Node::Expression(let_statement.value), environment);
            if is_error(&value) {
                return value;
            }

            environment.set(&let_statement.identifier.name, value.clone());

            value
        }
        Statement::While(while_statement) => eval_while_statement(while_statement, environment),
        Statement::Block(block_statement) => {
            eval_block_statements(block_statement.statements, environment)
        }
    }
}

fn eval_while_statement(node: WhileStatement, environment: &mut Environment) -> Object {
    let mut condition = eval(Node::Expression(node.condition.clone()), environment);
    if let Object::Error(_) = condition {
        return condition;
    }

    if !matches!(condition, Object::Boolean(_)) {
        return Object::Error(format!(
            "type mismatch: expected BOOLEAN but found {}",
            condition.object_type()
        ));
    }

    while is_truthy(&condition) {
        let result = eval(
            Node::Statement(Statement::Block(node.body.clone())),
            environment,
        );
        match result {
            Object::Return(return_object) => return *return_object,
            Object::Error(_) => return result,
            _ => {}
        }

        condition = eval(Node::Expression(node.condition.clone()), environment);
        if let Object::Error(_) = condition {
            return condition;
        }

        if !matches!(condition, Object::Boolean(_)) {
            return Object::Error(format!(
                "type mismatch: expected BOOLEAN but found {}",
                condition.object_type()
            ));
        }
    }

    return NULL;
}

fn eval_block_statements(statements: Vec<Statement>, environment: &mut Environment) -> Object {
    let mut result = NULL;

    for statement in statements {
        result = eval(Node::Statement(statement), environment);
        // When we find return or an error we stop evaluating
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
