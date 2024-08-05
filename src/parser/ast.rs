use crate::lexer::token::Token;

pub enum Node {
    Program(Program),
    Expression(Expression),
    Statement(Statement),
}

impl Node {
    pub fn get_lexeme(&self) -> String {
        match self {
            Node::Program(program) => program.get_lexeme(),
            Node::Expression(expression) => expression.get_lexeme(),
            Node::Statement(statement) => statement.get_lexeme(),
        }
    }
    pub fn to_string(&self) -> String {
        match self {
            Node::Program(program) => program.to_string(),
            Node::Expression(expression) => expression.to_string(),
            Node::Statement(statement) => statement.to_string(),
        }
    }
}

// Root node of the AST
#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Program {
        Program { statements: vec![] }
    }

    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
    }

    pub fn get_lexeme(&self) -> String {
        if self.statements.len() > 0 {
            return self.statements[0].get_lexeme();
        } else {
            return "".to_string();
        }
    }

    pub fn to_string(&self) -> String {
        self.statements
            .iter()
            .map(|statement| statement.to_string())
            .collect::<Vec<String>>()
            .join("\n")
    }
}

// An expression computes a value
#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier(Identifier),
    Integer(IntegerLiteral),
    Float(FloatLiteral),
    Boolean(BooleanLiteral),
    String(StringLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    If(IfExpression),
    Function(FunctionLiteral),
    Call(CallExpression),
}

impl Expression {
    pub fn get_lexeme(&self) -> String {
        match self {
            Expression::Identifier(identifier) => identifier.get_lexeme(),
            Expression::Integer(integer) => integer.get_lexeme(),
            Expression::Float(float) => float.get_lexeme(),
            Expression::Boolean(boolean) => boolean.get_lexeme(),
            Expression::String(string) => string.get_lexeme(),
            Expression::Prefix(prefix_expression) => prefix_expression.get_lexeme(),
            Expression::Infix(infinx_expression) => infinx_expression.get_lexeme(),
            Expression::If(if_expression) => if_expression.get_lexeme(),
            Expression::Function(function_literal) => function_literal.get_lexeme(),
            Expression::Call(call_expression) => call_expression.get_lexeme(),
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Expression::Identifier(identifier) => identifier.to_string(),
            Expression::Integer(integer) => integer.to_string(),
            Expression::Float(float) => float.to_string(),
            Expression::Boolean(boolean) => boolean.to_string(),
            Expression::String(string) => string.to_string(),
            Expression::Prefix(prefix_expression) => prefix_expression.to_string(),
            Expression::Infix(infinx_expression) => infinx_expression.to_string(),
            Expression::If(if_expression) => if_expression.to_string(),
            Expression::Function(function_literal) => function_literal.to_string(),
            Expression::Call(call_expression) => call_expression.to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub token: Token,
    pub name: String,
}

impl Identifier {
    pub fn new(token: Token, name: String) -> Identifier {
        Identifier { token, name }
    }

    pub fn get_lexeme(&self) -> String {
        return self.token.lexeme.clone();
    }

    pub fn to_string(&self) -> String {
        self.name.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntegerLiteral {
    token: Token,
    pub value: i64,
}

impl IntegerLiteral {
    pub fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }

    pub fn to_string(&self) -> String {
        self.token.lexeme.clone()
    }
    pub fn new(token: Token, value: i64) -> IntegerLiteral {
        IntegerLiteral { token, value }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BooleanLiteral {
    token: Token,
    pub value: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FloatLiteral {
    token: Token,
    pub value: f64,
}

impl FloatLiteral {
    pub fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }

    pub fn to_string(&self) -> String {
        self.token.lexeme.clone()
    }
    pub fn new(token: Token, value: f64) -> FloatLiteral {
        FloatLiteral { token, value }
    }
}

impl BooleanLiteral {
    pub fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }

    pub fn to_string(&self) -> String {
        self.token.lexeme.clone()
    }
    pub fn new(token: Token, value: bool) -> BooleanLiteral {
        BooleanLiteral { token, value }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteral {
    token: Token,
    pub value: String,
}

impl StringLiteral {
    pub fn new(token: Token, value: String) -> StringLiteral {
        StringLiteral { token, value }
    }

    pub fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }
    pub fn to_string(&self) -> String {
        self.token.lexeme.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixExpression {
    // Operator token
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl PrefixExpression {
    fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }

    fn to_string(&self) -> String {
        format!("({}{})", self.operator, self.right.to_string())
    }
    pub fn new(token: Token, operator: String, right: Expression) -> PrefixExpression {
        PrefixExpression {
            token,
            operator,
            right: Box::new(right),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct InfixExpression {
    // Operator token
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl InfixExpression {
    pub fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }

    pub fn to_string(&self) -> String {
        format!(
            "({} {} {})",
            self.left.to_string(),
            self.operator,
            self.right.to_string()
        )
    }
    pub fn new(
        token: Token,
        left: Expression,
        operator: String,
        right: Expression,
    ) -> InfixExpression {
        InfixExpression {
            token,
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpression {
    // IF token
    token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    // Else
    pub alternative: Option<BlockStatement>,
}

impl IfExpression {
    pub fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }

    pub fn to_string(&self) -> String {
        let condition_str = self.condition.to_string();
        let consequence_str = self.consequence.to_string();
        let alternative_str = match &self.alternative {
            Some(alternative) => alternative.to_string(),
            None => "".to_string(),
        };

        if self.alternative.is_some() {
            format!(
                "if {} {{\n{}\n}} else {{\n{}\n}}",
                condition_str, consequence_str, alternative_str
            )
        } else {
            format!("if {} {{\n{}\n}}", condition_str, consequence_str)
        }
    }
    pub fn new(
        token: Token,
        condition: Expression,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    ) -> IfExpression {
        IfExpression {
            token,
            condition: Box::new(condition),
            consequence,
            alternative,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionLiteral {
    // FUNCTION token
    token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl FunctionLiteral {
    pub fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }

    pub fn to_string(&self) -> String {
        let arguments = self
            .parameters
            .iter()
            .map(|argument| argument.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        format!(
            "{}({}) {{\n{}\n}}",
            self.token.lexeme,
            arguments,
            self.body.to_string()
        )
    }
    pub fn new(token: Token, arguments: Vec<Identifier>, body: BlockStatement) -> FunctionLiteral {
        FunctionLiteral {
            token,
            parameters: arguments,
            body,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpression {
    // ( token
    token: Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl CallExpression {
    pub fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }

    pub fn to_string(&self) -> String {
        let argumets = self
            .arguments
            .iter()
            .map(|argument| argument.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        format!("{}({})", self.function.to_string(), argumets)
    }

    pub fn new(token: Token, function: Expression, arguments: Vec<Expression>) -> CallExpression {
        CallExpression {
            token,
            function: Box::new(function),
            arguments,
        }
    }
}
// A statement expresses some action, but does not generate a value
#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Block(BlockStatement),
    While(WhileStatement),
}

impl Statement {
    pub fn get_lexeme(&self) -> String {
        match self {
            Statement::Let(statement) => statement.get_lexeme(),
            Statement::Return(statement) => statement.get_lexeme(),
            Statement::Expression(statement) => statement.get_lexeme(),
            Statement::Block(statement) => statement.get_lexeme(),
            Statement::While(statement) => statement.get_lexeme(),
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Statement::Let(statement) => statement.to_string(),
            Statement::Return(statement) => statement.to_string(),
            Statement::Expression(statement) => statement.to_string(),
            Statement::Block(statement) => statement.to_string(),
            Statement::While(statement) => statement.get_lexeme(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetStatement {
    // LET token
    pub token: Token,
    pub identifier: Identifier,
    pub value: Expression,
}

impl LetStatement {
    pub fn new(token: Token, identifier: Identifier, value: Expression) -> LetStatement {
        LetStatement {
            token,
            identifier,
            value,
        }
    }

    pub fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }

    pub fn to_string(&self) -> String {
        format!(
            "{} {} = {}",
            self.token.lexeme,
            self.identifier.to_string(),
            self.value.to_string()
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement {
    // RETURN token
    pub token: Token,
    pub value: Expression,
}

impl ReturnStatement {
    fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }

    pub fn to_string(&self) -> String {
        format!("{} {}", self.token.lexeme.clone(), self.value.to_string())
    }
    pub fn new(token: Token, value: Expression) -> ReturnStatement {
        ReturnStatement { token, value }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl ExpressionStatement {
    pub fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }

    pub fn to_string(&self) -> String {
        match &self.expression {
            Expression::Identifier(identifier) => identifier.to_string(),
            Expression::Integer(integer) => integer.to_string(),
            Expression::Float(float) => float.to_string(),
            Expression::Boolean(boolean) => boolean.to_string(),
            Expression::String(string) => string.to_string(),
            Expression::Prefix(prefix_expression) => prefix_expression.to_string(),
            Expression::Infix(infix_expression) => infix_expression.to_string(),
            Expression::If(if_expression) => if_expression.to_string(),
            Expression::Function(function_literal) => function_literal.to_string(),
            Expression::Call(call_expression) => call_expression.to_string(),
        }
    }

    pub fn new(token: Token, expression: Expression) -> ExpressionStatement {
        ExpressionStatement { token, expression }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockStatement {
    // { token
    token: Token,
    pub statements: Vec<Statement>,
}

impl BlockStatement {
    pub fn get_lexeme(&self) -> String {
        if self.statements.len() > 1 {
            return self.statements[0].get_lexeme();
        } else {
            return "".to_string();
        }
    }

    pub fn to_string(&self) -> String {
        self.statements
            .iter()
            .map(|statement| statement.to_string())
            .collect::<Vec<String>>()
            .join("\n")
    }

    pub fn new(token: Token) -> BlockStatement {
        BlockStatement {
            token,
            statements: vec![],
        }
    }

    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStatement {
    pub token: Token,
    pub condition: Expression,
    pub body: BlockStatement,
}

impl WhileStatement {
    pub fn new(token: Token, condition: Expression, body: BlockStatement) -> WhileStatement {
        WhileStatement {
            token,
            condition,
            body,
        }
    }

    pub fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }
    pub fn to_string(&self) -> String {
        format!(
            "{} {} {{ {} }}",
            self.get_lexeme(),
            self.condition.to_string(),
            self.body.to_string()
        )
    }
}
