#[derive(Debug, PartialEq)]
pub struct Program {
    pub prog_name: String,
    pub fun_decl: FunctionDecls,
}

pub type FunctionDecls = Vec<FunctionDecl>;

#[derive(Debug, PartialEq, Default)]
pub struct FunctionDecl {
    pub function_name: String,
    pub paras: Vec<Expression>,
    pub body: Statements,
}

pub type Statements = Vec<Statement>;

#[derive(Debug, PartialEq)]
pub enum Statement {
    VarAssignment{
        target: Box<Expression>,
        value: Box<Expression>,
    },
    PointerAssignment {
        target: Box<Expression>,
        value: Box<Expression>,
    },
    LocalDecl {
        names: Vec<Expression>,
    },
    Output {
        target: Expression,
    },
    While {
        cond: Expression,
        body: Statements,
    },
    Return { value: Expression },
    If {
        cond: Expression,
        then: Statements,
        orelse: Option<Statements>,
    },
}

pub type Expressions = Vec<Expression>;

#[derive(Debug, PartialEq)]
pub enum Expression {
    Malloc,
    Input,
    Null,
    Number {
        value: i64,
    },
    Identifier {
        name: String,
    },
    Binop {
        a: Box<Expression>,
        op: Operator,
        b: Box<Expression>,
    },
    Unop {
        op: UnaryOperator,
        a: Box<Expression>,
    },
    Call {
        function_name: Box<Expression>,
        args: Vec<Expression>,
    },
    PointerInvocation {
        function_name: Box<Expression>,
        args: Vec<Expression>,
    },
}

/*
 * Operator precedence:
 * & *
 * * /
 * + -
 * = >
 */

#[derive(Debug, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mult,
    Div,
    Gt,
    Eq,
}

#[derive(Debug, PartialEq)]
pub enum UnaryOperator {
    Minus,
    Pointer,
    Dereference,
}