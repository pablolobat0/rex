use crate::common::lexer::token::Token;
use core::fmt::Display;

pub enum Node {
    Program(Program),
    Expression(Expression),
}

// Root node of the AST
#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.statements
                .iter()
                .map(|statement| statement.to_string())
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

impl Program {
    pub fn new() -> Program {
        Program { statements: vec![] }
    }

    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
    }

    pub fn get_lexeme(&self) -> String {
        if !self.statements.is_empty() {
            self.statements[0].get_lexeme()
        } else {
            "".to_string()
        }
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

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let expression_stm = match self {
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
        };

        write!(f, "{}", expression_stm)
    }
}

impl Expression {
    #[cfg(test)]
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
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub token: Token,
    pub name: String,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Identifier {
    pub fn new(token: Token) -> Identifier {
        let name = token.lexeme.clone();
        Identifier { token, name }
    }

    #[cfg(test)]
    pub fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntegerLiteral {
    token: Token,
    pub value: i64,
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.lexeme)
    }
}

impl IntegerLiteral {
    #[cfg(test)]
    pub fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }

    pub fn new(token: Token, value: i64) -> IntegerLiteral {
        IntegerLiteral { token, value }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FloatLiteral {
    token: Token,
    pub value: f64,
}

impl Display for FloatLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.lexeme)
    }
}

impl FloatLiteral {
    #[cfg(test)]
    pub fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }

    pub fn new(token: Token, value: f64) -> FloatLiteral {
        FloatLiteral { token, value }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BooleanLiteral {
    token: Token,
    pub value: bool,
}

impl Display for BooleanLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.lexeme)
    }
}

impl BooleanLiteral {
    #[cfg(test)]
    pub fn get_lexeme(&self) -> String {
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

impl Display for StringLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.lexeme)
    }
}

impl StringLiteral {
    pub fn new(token: Token, value: String) -> StringLiteral {
        StringLiteral { token, value }
    }

    #[cfg(test)]
    pub fn get_lexeme(&self) -> String {
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

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

impl PrefixExpression {
    pub fn new(token: Token, operator: String, right: Expression) -> PrefixExpression {
        PrefixExpression {
            token,
            operator,
            right: Box::new(right),
        }
    }

    #[cfg(test)]
    fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
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

impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

impl InfixExpression {
    #[cfg(test)]
    pub fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
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

impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let condition_str = self.condition.to_string();
        let consequence_str = self.consequence.to_string();
        let alternative_str = match &self.alternative {
            Some(alternative) => alternative.to_string(),
            None => "".to_string(),
        };

        if self.alternative.is_some() {
            write!(
                f,
                "if {} {{\n{}\n}} else {{\n{}\n}}",
                condition_str, consequence_str, alternative_str
            )
        } else {
            write!(f, "if {} {{\n{}\n}}", condition_str, consequence_str)
        }
    }
}

impl IfExpression {
    #[cfg(test)]
    pub fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
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

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let arguments = self
            .parameters
            .iter()
            .map(|argument| argument.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        write!(
            f,
            "{}({}) {{\n{}\n}}",
            self.token.lexeme, arguments, self.body
        )
    }
}

impl FunctionLiteral {
    #[cfg(test)]
    pub fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
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

impl Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let argumets = self
            .arguments
            .iter()
            .map(|argument| argument.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        write!(f, "{}({})", self.function, argumets)
    }
}

impl CallExpression {
    #[cfg(test)]
    pub fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
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
    While(WhileStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let statement_str = match self {
            Statement::Let(statement) => statement.to_string(),
            Statement::Return(statement) => statement.to_string(),
            Statement::Expression(statement) => statement.to_string(),
            Statement::While(statement) => statement.to_string(),
        };

        write!(f, "{}", statement_str)
    }
}

impl Statement {
    pub fn get_lexeme(&self) -> String {
        match self {
            Statement::Let(statement) => statement.get_lexeme(),
            Statement::Return(statement) => statement.get_lexeme(),
            Statement::Expression(statement) => statement.get_lexeme(),
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

impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} = {}",
            self.token.lexeme, self.identifier, self.value
        )
    }
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
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement {
    // RETURN token
    pub token: Token,
    pub value: Expression,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.token.lexeme.clone(), self.value)
    }
}

impl ReturnStatement {
    fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
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

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let expression_str = match &self.expression {
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
        };

        write!(f, "{}", expression_str)
    }
}

impl ExpressionStatement {
    pub fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
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

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.statements
                .iter()
                .map(|statement| statement.to_string())
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

impl BlockStatement {
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

impl Display for WhileStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {{\n{}\n}}",
            self.get_lexeme(),
            self.condition,
            self.body
        )
    }
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
}
