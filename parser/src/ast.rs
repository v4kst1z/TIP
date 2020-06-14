use crate::symbol::Symbols;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum TypeDecl {
    FunctionDecl(u64),
    ParameterDecl(u64),
    LocalDecl(u64)
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub prog_name: String,
    pub fun_decl: FunctionDecls,
    pub prog_env: Symbols
}


impl Program {
    pub fn new() -> Self {
        Program {
            prog_name: "".to_string(),
            fun_decl: vec![],
            prog_env: Symbols::new()
        }

    }
}

pub type FunctionDecls = Vec<FunctionDecl>;

#[derive(Debug, PartialEq)]
pub struct FunctionDecl {
    pub function_name: String,
    pub paras: Vec<Expression>,
    pub body: Statements,
    pub fun_id: u64,
    pub fun_env: Symbols

}

impl FunctionDecl {
    pub fn new(env: Symbols, fun_id: u64) -> Self {
        FunctionDecl {
            function_name: "".to_string(),
            paras: vec![],
            body: vec![],
            fun_id,
            fun_env: env
        }

    }
}

pub type Statements = Vec<Statement>;

#[derive(Debug, PartialEq)]
pub struct Statement {
    pub stmt: StatementDesc,
}

impl Statement {
    pub fn new(stmt: StatementDesc) -> Self {
        let stmt_out =  Statement {
                stmt,
        };
        stmt_out
    }
}


#[derive(Debug, PartialEq)]
pub enum StatementDesc {
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
    Error { value: Expression },
    If {
        cond: Expression,
        then: Statements,
        orelse: Option<Statements>,
    },
    Block {
        body: Statements,
    }
}

pub type Expressions = Vec<Expression>;

#[derive(Debug, PartialEq)]
pub struct Expression {
    pub expr: ExpressionDesc,
    pub expr_id: u64,
}

impl Expression {
    pub fn new(expr: ExpressionDesc, id: u64) -> Self {
        let expr_out = Expression {
                expr,
                expr_id: id
            };
        expr_out
    }
}

#[derive(Debug, PartialEq)]
pub enum ExpressionDesc {
    Malloc,
    Input,
    Null,
    Number {
        value: i64,
    },
    Identifier {
        name: String,
        id: u64,
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