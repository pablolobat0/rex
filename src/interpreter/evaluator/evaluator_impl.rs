use super::object::{Environment, Function, Object};
use crate::interpreter::parser::ast::{
    Expression, Identifier, IfExpression, Node, Statement, WhileStatement,
};

// Only need to be instantiated once
const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

pub fn eval(node: Node, environment: &mut Environment) -> Object {
    match node {
        Node::Program(program) => eval_program_statements(program.statements, environment),
        Node::Expression(expression) => eval_expression(expression, environment),
    }
}

fn eval_program_statements(statements: Vec<Statement>, environment: &mut Environment) -> Object {
    let mut result = NULL;

    for statement in statements {
        result = eval_statement(statement, environment);
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
        Expression::Float(float) => Object::Float(float.value),
        Expression::Identifier(identifier) => eval_identifier(identifier, environment),
        Expression::Boolean(boolean) => eval_boolean(boolean.value),
        Expression::String(string) => Object::String(string.value),
        Expression::Prefix(prefix_expression) => {
            let right = eval(Node::Expression(*prefix_expression.right), environment);
            if is_error(&right) {
                return right;
            }

            eval_prefix_expression(&prefix_expression.operator, right)
        }
        Expression::Infix(infix_expression) => {
            let left_string = infix_expression.left.to_string();
            let right_string = infix_expression.right.to_string();
            let right = eval_expression(*infix_expression.right, environment);
            if is_error(&right) {
                return right;
            }

            // Check if it's an assignation
            if &infix_expression.operator == "=" {
                let Expression::Identifier(identifier) = *infix_expression.left else {
                    return Object::Error(format!(
                        "Expected Identifier found {} {}",
                        left_string, right_string
                    ));
                };

                let name = identifier.name.clone();
                let object = eval_identifier(identifier, environment);
                if is_error(&object) {
                    return object;
                }

                environment.set(&name, right);
                return NULL;
            }

            let left = eval_expression(*infix_expression.left, environment);
            if is_error(&left) {
                return left;
            }

            eval_infix_expression(left, &infix_expression.operator, right)
        }
        Expression::If(if_expression) => eval_if_expression(if_expression, environment),
        Expression::Function(function_literal) => {
            // New environment for the function
            Object::Function(Function::new(
                function_literal.parameters,
                function_literal.body,
                environment.clone(),
            ))
        }
        Expression::Call(call_expression) => {
            let function = eval_expression(*call_expression.function, environment);
            if is_error(&function) {
                return function;
            }

            match eval_expressions(call_expression.arguments, environment) {
                Ok(arguments) => apply_function(function, arguments),
                Err(error) => error,
            }
        }
    }
}

fn eval_identifier(identifier: Identifier, environment: &mut Environment) -> Object {
    let object = environment.get(&identifier.name);

    match object {
        Some(object) => object,
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
        Object::Float(value) => Object::Float(-value),
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
        (Object::Float(left_value), Object::Float(right_value)) => {
            eval_float_infix_expression(left_value, operator, right_value)
        }
        (Object::Integer(left_value), Object::Float(right_value)) => {
            eval_float_infix_expression(left_value as f64, operator, right_value)
        }
        (Object::Float(left_value), Object::Integer(right_value)) => {
            eval_float_infix_expression(left_value, operator, right_value as f64)
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
                let result = left_value as f64 / right_value as f64;
                if result.fract() == 0.0 {
                    Object::Integer(result as i64)
                } else {
                    Object::Float(result)
                }
            } else {
                Object::Error("error division by 0".to_string())
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

fn eval_float_infix_expression(left_value: f64, operator: &str, right_value: f64) -> Object {
    match operator {
        "+" => Object::Float(left_value + right_value),
        "-" => Object::Float(left_value - right_value),
        "*" => Object::Float(left_value * right_value),
        "/" => {
            if right_value != 0.0 {
                Object::Float(left_value / right_value)
            } else {
                Object::Error("error division by 0".to_string())
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
    let Object::Boolean(_) = condition else {
        return Object::Error(format!(
            "type mismatch, expected a BOOLEAN but found {}",
            condition.object_type()
        ));
    };

    if is_truthy(&condition) {
        eval_block_statements(node.consequence.statements, environment)
    } else if let Some(alternative) = node.alternative {
        eval_block_statements(alternative.statements, environment)
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

fn eval_expressions(
    expressions: Vec<Expression>,
    environment: &mut Environment,
) -> Result<Vec<Object>, Object> {
    let mut result = vec![];

    for expression in expressions {
        let object = eval_expression(expression, environment);

        if is_error(&object) {
            return Err(object);
        }

        result.push(object);
    }

    Ok(result)
}

fn apply_function(function: Object, arguments: Vec<Object>) -> Object {
    match function {
        Object::Function(function) => {
            let mut extended_env =
                extend_function_env(function.parameters, arguments, function.environment);

            let evaluated_body = eval_block_statements(function.body.statements, &mut extended_env);
            // We need to unwrap the value inside the return object
            unwrap_return_value(evaluated_body)
        }
        _ => Object::Error(format!(
            "expected FUNCTION, found: {}",
            function.object_type()
        )),
    }
}

// Extends the function environment with the arguments
fn extend_function_env(
    parameters: Vec<Identifier>,
    arguments: Vec<Object>,
    environment: Environment,
) -> Environment {
    let mut extended_env = Environment::new_enclosed(environment);

    //
    for (parameter, argument) in parameters.iter().zip(arguments) {
        extended_env.set(&parameter.name, argument);
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
            eval_expression(expression_statement.expression, environment)
        }
        Statement::Return(return_statement) => {
            let return_value = eval_expression(return_statement.value, environment);
            Object::Return(Box::new(return_value))
        }
        Statement::Let(let_statement) => {
            let value = eval_expression(let_statement.value, environment);
            if is_error(&value) {
                return value;
            }

            environment.set(&let_statement.identifier.name, value);

            NULL
        }
        Statement::While(while_statement) => eval_while_statement(while_statement, environment),
    }
}

fn eval_while_statement(node: WhileStatement, environment: &mut Environment) -> Object {
    // Repeats until the condition is false
    loop {
        let condition = eval_expression(node.condition.clone(), environment);
        if is_error(&condition) {
            return condition;
        }

        if !is_truthy(&condition) {
            return NULL;
        }

        let result = eval_block_statements(node.body.statements.clone(), environment);

        match result {
            Object::Return(return_object) => return *return_object,
            Object::Error(_) => return result,
            _ => continue,
        }
    }
}

fn eval_block_statements(statements: Vec<Statement>, environment: &mut Environment) -> Object {
    let mut result = NULL;

    for statement in statements {
        result = eval_statement(statement, environment);
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
    matches!(object, Object::Error(_))
}
