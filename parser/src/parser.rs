use lexer::token::Token;
use std::fmt::Debug;
use std::io::{Error};
use crate::ast::{Expression, Program, FunctionDecl, Operator, UnaryOperator, Statements, Statement};
use lexer::token::Token::{Ident, Rparen, Comma};
use crate::ast::Expression::{Malloc, Input, Null, Number, Identifier, Binop, Unop, PointerInvocation};
use crate::ast::Statement::{VarAssignment, LocalDecl, Output, Return, While, If, PointerAssignment, Block};

#[derive(PartialEq)]
pub struct Parser<T: Iterator<Item = Token> + Debug + Clone> {
    token: T,
    tok0: Option<Token>,
    tok1: Option<Token>,
    pub result: Program
}

impl<T> Parser<T>
    where
        T: Iterator<Item = Token> + Debug + Clone {
    pub fn new(input: T) -> Self {
        let mut parser = Parser {
            token: input,
            tok0: None,
            tok1: None,
            result: Program { prog_name: "".to_string(),  fun_decl: vec![] }
        };
        parser.next_token();
        parser.next_token();

        parser
    }

    fn parse_fun_args(&mut self) -> Result<Vec<Expression>, Error> {
        let mut args_out = vec![];
        self.next_token(); //consume (
        loop {
            if self.tok0 == Some(Comma) {
                self.next_token(); // consume Comma
            } else if self.tok0 == Some(Rparen) {
                self.next_token(); // consume Rparen
                break;
            }

            let tmp_expr = self.parse_expr()?;
            args_out.push(tmp_expr);
        }
        Ok(args_out)
    }

    fn expr(&mut self) -> Result<Expression, Error> {
        match self.tok0.clone() {
            Some(Token::Alloc) => {
                self.next_token();
                Ok(Malloc)
            }
            Some(Token::Input) => {
                self.next_token();
                Ok(Input)
            }
            Some(Token::Null) => {
                self.next_token();
                Ok(Null)
            }
            Some(Token::Number(num)) => {
                self.next_token();
                Ok(Number {
                    value: num as i64,
                })
            }
            Some(Token::Ident(name)) => {
                if let Some(Token::Lparen) = self.tok1 {
                    let call_fun = Box::new(Expression::Identifier {name});
                    self.next_token();   //consume function name
                    let args = self.parse_fun_args()?;
                    //parse function arguments
                    Ok(Expression::Call {
                        function_name: call_fun,
                        args
                    })
                } else {
                    self.next_token();
                    Ok(Identifier {
                        name,
                    },)
                }
            }
            Some(Token::Lparen)  if self.tok1 == Some(Token::Mult) => {
                self.next_token();
                let expr = self.parse_expr()?;
                if self.tok0 == Some(Token::Rparen) && self.tok1 == Some(Token::Lparen) {
                    self.next_token();
                    let args = self.parse_fun_args()?;
                    Ok(PointerInvocation {
                        function_name: Box::new(expr),
                        args: args,
                    })
                } else {
                    self.next_token();
                    Ok(expr)
                }
            }
            Some(Token::Lparen)  => {
                self.next_token();
                let expr = self.parse_expr()?;
                if self.tok0 == Some(Token::Rparen) && self.tok1 == Some(Token::Lparen) {
                    self.next_token();
                    let args = self.parse_fun_args()?;
                    Ok(Expression::Call {
                        function_name: Box::new(expr),
                        args
                    })
                } else {
                    self.next_token();
                    Ok(expr)
                }

            }
            _ => panic!("Err parse expr! {:?}", self.token)
        }
    }

    fn unary_expr(&mut self) -> Result<Expression, Error> {
        match self.tok0 {
            Some(Token::Mult) => {
                self.next_token(); //consume *
                let expr = self.unary_expr()?;
                Ok(Unop {
                    op: UnaryOperator::Dereference,
                    a: Box::new(expr)
                })
            }
            Some(Token::Ampersand) => {
                self.next_token(); //consume &
                let expr = self.unary_expr()?;
                Ok(Unop {
                    op: UnaryOperator::Pointer,
                    a: Box::new(expr)
                })
            }
            Some(Token::Sub) => {
                self.next_token(); //consume -
                let expr = self.unary_expr()?;
                Ok(Unop {
                    op: UnaryOperator::Minus,
                    a: Box::new(expr)
                })
            }
            _ => self.expr(),
        }
    }

    fn multiplicative_expr(&mut self) -> Result<Expression, Error> {
        let mut expr = self.unary_expr()?;
        loop {
            let operator = match self.tok0 {
                Some(Token::Mult) => {
                    self.next_token(); //consume *
                    Operator::Mult
                }
                Some(Token::Div) => {
                    self.next_token(); //consume /
                    Operator::Div
                }
                _ => break,
            };
            let right = Box::new(self.unary_expr()?);
            expr = Binop {
                a: Box::new(expr),
                op: operator,
                b: right
            }
        }
        Ok(expr)
    }

    fn additive_expr(&mut self) -> Result<Expression, Error> {
        let mut expr = self.multiplicative_expr()?;
        loop {
            let operator = match self.tok0 {
                Some(Token::Plus) => {
                    self.next_token(); //consume +
                    Operator::Add
                }
                Some(Token::Sub) => {
                    self.next_token(); //consume -
                    Operator::Sub
                }
                _ => break,
            };
            let right = Box::new(self.multiplicative_expr()?);
            expr = Binop {
                a: Box::new(expr),
                op: operator,
                b: right
            }
        }

        Ok(expr)
    }

    fn relational_expr(&mut self) -> Result<Expression, Error> {
        let mut expr = self.additive_expr()?;
        loop {
            let operator = match self.tok0 {
                Some(Token::Great) => {
                    self.next_token(); //consume >
                    Operator::Gt
                }
                Some(Token::Equal) => {
                    self.next_token(); //consume ==
                    Operator::Eq
                }
                _ => break,
            };
            let right = Box::new(self.additive_expr()?);
            expr = Binop {
                a: Box::new(expr),
                op: operator,
                b: right
            }
        }
        Ok(expr)
    }

    fn parse_expr(&mut self) -> Result<Expression, Error> {
        self.relational_expr()
    }

    fn parse_decl_var(&mut self) -> Result<Vec<Expression>, Error> {
        let mut decl_vars = vec![];

        loop {
            if self.tok0 == Some(Token::Semi) {
                break;
            } else if self.tok0 == Some(Token::Comma) {
                self.next_token();
            }

            if let Some(Ident(decl_var_name)) = self.tok0.clone() {
                decl_vars.push(Expression::Identifier { name: decl_var_name });
                self.next_token();
            }

        }

        Ok(decl_vars)
    }

    fn parse_cond_expr(&mut self) -> Result<Expression, Error> {
        self.next_token(); //consume (
        let cond_expr = self.parse_expr()?;
        self.next_token(); // consume )

        Ok(cond_expr)
    }

    fn parse_block(&mut self, ) -> Result<Statements, Error> {
        let mut stmts = vec![];
        if self.tok0 == Some(Token::Lcurly) {
            self.next_token(); // consume {
            loop {
                if self.tok0 == Some(Token::Rcurly) {
                    self.next_token();
                    break;
                }
                stmts.push(self.parse_stmt()?);
            }
        } else {
            stmts.push(self.parse_stmt()?);
        }
        Ok(stmts)
    }


    fn parse_stmt(&mut self) -> Result<Statement, Error> {
        match self.tok0.clone() {
            Some(Ident(var_name)) => {
                self.next_token();

                if let Some(Token::Assign) = self.tok0.clone() {
                    self.next_token();
                    let value = self.parse_expr()?;
                    self.next_token(); //consume ;
                    Ok(VarAssignment {
                        target: Box::new(Expression::Identifier {name: var_name}),
                        value: Box::new(value)
                    })
                } else {
                    panic!("Err ident~~")
                }
            }
            Some(Token::Var)  => {
                self.next_token();
                let names = self.parse_decl_var()?;
                self.next_token(); //consume ;
                Ok(LocalDecl {
                    names
                })
            }
            Some(Token::Output) => {
                self.next_token();
                let out_expr = self.parse_expr()?;
                self.next_token(); //consume ;
                Ok(Output {
                    target: out_expr,
                })
            }
            Some(Token::Return) => {
                self.next_token();
                let ret_expr = self.parse_expr()?;
                self.next_token(); //consume ;
                Ok(Return {
                    value: ret_expr,
                })
            }
            Some(Token::Error) => {
                self.next_token();
                let err_expr = self.parse_expr()?;
                self.next_token(); //consume ;
                Ok(Statement::Error {
                    value: err_expr
                })
            }
            Some(Token::While) => {
                self.next_token();
                let cond_expr = self.parse_cond_expr()?;
                //self.next_token(); // consume {
                let out_stmts = self.parse_block()?;
                Ok(While {
                    cond: cond_expr,
                    body: out_stmts,
                })
            }
            Some(Token::If) => {
                self.next_token();
                let cond_expr = self.parse_cond_expr()?;
                let then_stmts = self.parse_block()?;
                if let Some(Token::Else) = self.tok0 {
                    self.next_token(); //consume else
                    let else_stmts = self.parse_block()?;
                    Ok(If {
                        cond: cond_expr,
                        then: then_stmts,
                        orelse: Some(else_stmts),
                    },)
                } else {
                    Ok(If {
                        cond: cond_expr,
                        then: then_stmts,
                        orelse: None,
                    },)
                }
            }
            Some(Token::Mult) => {
                let left = self.parse_expr()?;
                self.next_token();
                let right = self.parse_expr()?;
                self.next_token(); //consume ;
                Ok(PointerAssignment {
                    target: Box::new(left),
                    value: Box::new(right),
                })
            }
            Some(Token::Lcurly) => {
                Ok(Block {
                    body: self.parse_block()?,
                })
            }
            _ => panic!("Err in parse stmt~~ {:?}", self.result),
        }
    }

    fn parse_paras(&mut self) -> Result<Vec<Expression>, Error> {
        let mut paras = vec![];
        self.next_token(); // consume Lparen
        loop {
            if self.tok0 == Some(Comma) {
                self.next_token(); // consume Comma
            } else if self.tok0 == Some(Rparen) {
                self.next_token(); // consume Rparen
                break;
            }

            if let Some(Ident(para_name)) = self.tok0.clone() {
                paras.push(Expression::Identifier { name: para_name });
                self.next_token(); // consume para
            }
        }
        Ok(paras)
    }

    fn parse_func(&mut self) -> Result<FunctionDecl, Error> {
        let mut tmp_func = FunctionDecl::default();
        if let Some(Ident(fun_name)) = self.tok0.clone() {
            tmp_func.function_name = fun_name;
            self.next_token(); //consume function name
        }
        tmp_func.paras = self.parse_paras()?;
        tmp_func.body = self.parse_block()?;

        Ok(tmp_func)
    }

    pub fn parse_prog(&mut self)  {
        loop {
            match self.tok0.clone() {
                Some(_) => {
                    if let Ok(tmp_func) = self.parse_func() {
                        self.result.fun_decl.push(tmp_func);
                    } else {
                        panic!("Err in parse fun decl~~")
                    }
                },
                _ => {
                    break
                },
            }
        }

    }

    fn next_token(&mut self) -> Option<Token> {
        let tmp_token = self.tok0.clone();
        self.tok0 = self.tok1.clone();
        self.tok1 = self.token.next();

        tmp_token
    }
}