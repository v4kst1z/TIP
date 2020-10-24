use crate::symbol::Symbols;
use std::cell::RefCell;
use std::rc::Rc;
use std::fmt;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum TypeDecl {
    FunctionDecl(u64),
    ParameterDecl(u64),
    LocalDecl(u64)
}

#[derive(PartialEq)]
pub struct Program {
    pub prog_name: String,
    pub fun_decl: FunctionDecls,
    pub prog_env: Symbols
}


impl Program {
    pub fn new() -> Self {
        let mut prog = Program {
            prog_name: "".to_string(),
            fun_decl: vec![],
            prog_env: Symbols::new()
        };
        prog.prog_env.begin_scope();
        prog

    }
}

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[*] prog name : {:?}\n", self.prog_name)?;
        for fun in self.fun_decl.iter() {
            write!(f, "{:?}\n", fun)?;
        }
        Ok(())
    }
}

pub type FunctionDecls = Vec<FunctionDecl>;

#[derive(PartialEq)]
pub struct FunctionDecl {
    pub function_name: String,
    pub paras: Vec<Rc<RefCell<Expression>>>,
    pub body: Statements,
    pub fun_id: u64,
    pub fun_env: Symbols

}

impl fmt::Debug for FunctionDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[*] function name : {:?}\n", self.function_name)?;
        write!(f, "[*] paras : {:?}\n", self.paras)?;
        write!(f, "[*] bosy : {:?}\n", self.body)?;
        Ok(())
    }
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

#[derive(PartialEq)]
pub struct Statement {
    pub stmt: StatementDesc,
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}\n", self.stmt)?;
        Ok(())
    }
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
        target: Rc<RefCell<Expression>>,
        value: Rc<RefCell<Expression>>,
    },
    PointerAssignment {
        target: Rc<RefCell<Expression>>,
        value: Rc<RefCell<Expression>>,
    },
    LocalDecl {
        names: Vec<Rc<RefCell<Expression>>>,
    },
    Output {
        target: Rc<RefCell<Expression>>,
    },
    While {
        cond: Rc<RefCell<Expression>>,
        body: Statements,
    },
    Return { value: Rc<RefCell<Expression>> },
    Err { value: Rc<RefCell<Expression>> },
    If {
        cond: Rc<RefCell<Expression>>,
        then: Statements,
        orelse: Option<Statements>,
    },
    Block {
        body: Statements,
    }
}

pub type Expressions = Vec<Rc<RefCell<Expression>>>;

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
        a: Rc<RefCell<Expression>>,
        op: Operator,
        b: Rc<RefCell<Expression>>,
    },
    Unop {
        op: UnaryOperator,
        a: Rc<RefCell<Expression>>,
    },
    Call {
        function_name: Rc<RefCell<Expression>>,
        args: Vec<Rc<RefCell<Expression>>>,
    },
    PointerInvocation {
        function_name: Rc<RefCell<Expression>>,
        args: Vec<Rc<RefCell<Expression>>>,
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

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum UnaryOperator {
    Minus,
    Pointer,
    Dereference,
}