use crate::lexer::token::token::Token;

pub trait Node {
    fn get_lexeme(&self) -> String;
    fn to_string(&self) -> String;
}

// An expression computes a value
#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    Integer(IntegerLiteral),
    Boolean(BooleanLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    If(IfExpression),
    Function(FunctionLiteral),
}

impl Node for Expression {
    fn get_lexeme(&self) -> String {
        match self {
            Expression::Identifier(identifier) => identifier.get_lexeme(),
            Expression::Integer(integer) => integer.get_lexeme(),
            Expression::Boolean(boolean) => boolean.get_lexeme(),
            Expression::Prefix(prefix_expression) => prefix_expression.get_lexeme(),
            Expression::Infix(infinx_expression) => infinx_expression.get_lexeme(),
            Expression::If(if_expression) => if_expression.get_lexeme(),
            Expression::Function(function_literal) => function_literal.get_lexeme(),
        }
    }

    fn to_string(&self) -> String {
        match self {
            Expression::Identifier(identifier) => identifier.to_string(),
            Expression::Integer(integer) => integer.to_string(),
            Expression::Boolean(boolean) => boolean.to_string(),
            Expression::Prefix(prefix_expression) => prefix_expression.to_string(),
            Expression::Infix(infinx_expression) => infinx_expression.to_string(),
            Expression::If(if_expression) => if_expression.to_string(),
            Expression::Function(function_literal) => function_literal.to_string(),
        }
    }
}

// A statement expresses some action, but does not generate a value
#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Block(BlockStatement),
}

impl Node for Statement {
    fn get_lexeme(&self) -> String {
        match self {
            Statement::Let(statement) => statement.get_lexeme(),
            Statement::Return(statement) => statement.get_lexeme(),
            Statement::Expression(statement) => statement.get_lexeme(),
            Statement::Block(statement) => statement.get_lexeme(),
        }
    }

    fn to_string(&self) -> String {
        match self {
            Statement::Let(statement) => statement.to_string(),
            Statement::Return(statement) => statement.to_string(),
            Statement::Expression(statement) => statement.to_string(),
            Statement::Block(statement) => statement.to_string(),
        }
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl Node for ExpressionStatement {
    fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }

    fn to_string(&self) -> String {
        match &self.expression {
            Expression::Identifier(identifier) => format!("{};", identifier.to_string()),
            Expression::Integer(integer) => format!("{};", integer.to_string()),
            Expression::Boolean(boolean) => format!("{};", boolean.to_string()),
            Expression::Prefix(prefix_expression) => format!("{};", prefix_expression.to_string()),
            Expression::Infix(infix_expression) => format!("{};", infix_expression.to_string()),
            Expression::If(if_expression) => if_expression.to_string(),
            Expression::Function(function_literal) => function_literal.to_string(),
        }
    }
}

impl ExpressionStatement {
    pub fn new(token: Token, expression: Expression) -> ExpressionStatement {
        ExpressionStatement { token, expression }
    }
}

#[derive(Debug)]
pub struct BlockStatement {
    token: Token,
    statements: Vec<Statement>,
}

impl Node for BlockStatement {
    fn get_lexeme(&self) -> String {
        if self.statements.len() > 1 {
            return self.statements[0].get_lexeme();
        } else {
            return "".to_string();
        }
    }

    fn to_string(&self) -> String {
        self.statements
            .iter()
            .map(|statement| statement.to_string())
            .collect::<Vec<String>>()
            .join("\n")
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
}

impl Node for Program {
    fn get_lexeme(&self) -> String {
        if self.statements.len() > 1 {
            return self.statements[0].get_lexeme();
        } else {
            return "".to_string();
        }
    }

    fn to_string(&self) -> String {
        self.statements
            .iter()
            .map(|statement| statement.to_string())
            .collect::<Vec<String>>()
            .join("\n")
    }
}

#[derive(Debug)]
pub struct LetStatement {
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
}

impl Node for LetStatement {
    fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }

    fn to_string(&self) -> String {
        format!(
            "{} {} = {};",
            self.token.lexeme,
            self.identifier.to_string(),
            self.value.to_string()
        )
    }
}

#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    pub name: String,
}

impl Identifier {
    pub fn new(token: Token, name: String) -> Identifier {
        Identifier { token, name }
    }
}

impl Node for Identifier {
    fn get_lexeme(&self) -> String {
        return self.token.lexeme.clone();
    }

    fn to_string(&self) -> String {
        self.name.clone()
    }
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub value: Expression,
}

impl Node for ReturnStatement {
    fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }

    fn to_string(&self) -> String {
        format!("{} {};", self.token.lexeme.clone(), self.value.to_string())
    }
}

impl ReturnStatement {
    pub fn new(token: Token, value: Expression) -> ReturnStatement {
        ReturnStatement { token, value }
    }
}

#[derive(Debug)]
pub struct IntegerLiteral {
    token: Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }

    fn to_string(&self) -> String {
        self.token.lexeme.clone()
    }
}

impl IntegerLiteral {
    pub fn new(token: Token, value: i64) -> IntegerLiteral {
        IntegerLiteral { token, value }
    }
}

#[derive(Debug)]
pub struct BooleanLiteral {
    token: Token,
    pub value: bool,
}

impl Node for BooleanLiteral {
    fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }

    fn to_string(&self) -> String {
        self.token.lexeme.clone()
    }
}

impl BooleanLiteral {
    pub fn new(token: Token, value: bool) -> BooleanLiteral {
        BooleanLiteral { token, value }
    }
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Node for PrefixExpression {
    fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }

    fn to_string(&self) -> String {
        format!("({}{})", self.operator, self.right.to_string())
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
}

#[derive(Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Node for InfixExpression {
    fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }

    fn to_string(&self) -> String {
        format!(
            "({} {} {})",
            self.left.to_string(),
            self.operator,
            self.right.to_string()
        )
    }
}

impl InfixExpression {
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

#[derive(Debug)]
pub struct IfExpression {
    token: Token,
    condition: Box<Expression>,
    consequence: BlockStatement,
    alternative: Option<BlockStatement>,
}

impl Node for IfExpression {
    fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }

    fn to_string(&self) -> String {
        let condition_str = self.condition.to_string();
        let consequence_str = self.consequence.to_string();
        let alternative_str = match &self.alternative {
            Some(alternative) => alternative.to_string(),
            None => "".to_string(),
        };

        if self.alternative.is_some() {
            format!(
                "if {} {{ {} }} else {{ {} }}",
                condition_str, consequence_str, alternative_str
            )
        } else {
            format!("if {} {{ {} }}", condition_str, consequence_str)
        }
    }
}

impl IfExpression {
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

#[derive(Debug)]
pub struct FunctionLiteral {
    token: Token,
    arguments: Vec<Identifier>,
    body: BlockStatement,
}

impl Node for FunctionLiteral {
    fn get_lexeme(&self) -> String {
        self.token.lexeme.clone()
    }

    fn to_string(&self) -> String {
        let arguments = self
            .arguments
            .iter()
            .map(|argument| argument.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        format!(
            "{}({}){{{}}}",
            self.token.lexeme,
            arguments,
            self.body.to_string()
        )
    }
}

impl FunctionLiteral {
    pub fn new(token: Token, arguments: Vec<Identifier>, body: BlockStatement) -> FunctionLiteral {
        FunctionLiteral {
            token,
            arguments,
            body,
        }
    }
}
