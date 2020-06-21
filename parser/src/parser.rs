use lexer::token::Token;
use std::fmt::Debug;
use crate::ast::*;
use lexer::token::Token::{
    Ident,
    Rparen,
    Comma
};
use crate::ast::{
    Expression,
    Program,
    FunctionDecl,
    Operator,
    UnaryOperator,
    Statements,
    Statement,
    ExpressionDesc
};
use crate::ast::ExpressionDesc::{
    Malloc,
    Input,
    Null,
    Number,
    Identifier,
    Binop,
    Unop,
    PointerInvocation
};
use crate::ast::StatementDesc::{
    VarAssignment,
    LocalDecl,
    Output,
    Return,
    While,
    If,
    PointerAssignment,
    Block
};
use std::io::Error;
use std::borrow::BorrowMut;
use crate::symbol::Symbols;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(PartialEq)]
pub struct Parser<T: Iterator<Item = Token> + Debug + Clone> {
    token: T,
    tok0: Option<Token>,
    tok1: Option<Token>,
    id: u64,
    pub result: Program
}

impl<T> Parser<T>
    where
        T: Iterator<Item = Token> + Debug + Clone
{
    pub fn new(input: T) -> Self {
        let mut parser = Parser {
            token: input,
            tok0: None,
            tok1: None,
            id: 0,
            result: Program::new()
        };
        parser.next_token();
        parser.next_token();

        parser
    }

    fn next_id(&mut self) {
        self.id += 1;
    }

    fn parse_fun_args(&mut self) -> Result<Vec<Rc<RefCell<Expression>>>, Error> {

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

    fn expr(&mut self) -> Result<Rc<RefCell<Expression>>, Error> {

        let out;
        match self.tok0.clone() {
            Some(Token::Alloc) => {
                self.next_token();
                out = Rc::new(
                    RefCell::new(
                        Expression::new(Malloc, self.id)
                    )
                );
                self.next_id();
                Ok(out)
            }
            Some(Token::Input) => {
                self.next_token();
                out = Rc::new(
                    RefCell::new(
                        Expression::new(Input, self.id)
                    )
                );
                self.next_id();
                Ok(out)
            }
            Some(Token::Null) => {
                self.next_token();
                out = Rc::new(
                    RefCell::new(
                        Expression::new(Null, self.id)
                    )
                );
                self.next_id();
                Ok(out)
            }
            Some(Token::Number(num)) => {
                self.next_token();
                out = Rc::new(
                    RefCell::new(
                        Expression::new(Number { value: num as i64, }, self.id)
                    )
                );
                self.next_id();
                Ok(out)
            }
            Some(Token::Ident(name)) => {
                if let Some(Token::Lparen) = self.tok1 {
                    let call_fun = RefCell::new(
                        Expression::new(ExpressionDesc::Identifier {name, id: 0 }, self.id)
                    );
                    self.next_id();
                    self.next_token();   //consume function name
                    let args = self.parse_fun_args()?;
                    //parse function arguments
                    out = Rc::new(
                        RefCell::new(
                            Expression::new(
                                ExpressionDesc::Call { function_name: Rc::new(call_fun), args },
                                self.id
                            )
                        )
                    );
                    self.next_id();
                    Ok(out)
                } else {
                    self.next_token();
                    out = Rc::new(
                        RefCell::new(
                            Expression::new(Identifier { name, id: 0 }, self.id)
                        )
                    );
                    self.next_id();
                    Ok(out)
                }
            }
            Some(Token::Lparen)  if self.tok1 == Some(Token::Mult) => {
                self.next_token();
                let expr = self.parse_expr()?;
                if self.tok0 == Some(Token::Rparen) && self.tok1 == Some(Token::Lparen) {
                    self.next_token();
                    let args = self.parse_fun_args()?;
                    out = Rc::new(
                        RefCell::new(
                            Expression::new(
                                PointerInvocation { function_name: expr, args, },
                                self.id
                            )
                        )
                    );
                    self.next_id();
                    Ok(out)
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
                    out = Rc::new(
                        RefCell::new(
                            Expression::new(
                                ExpressionDesc::Call { function_name: expr, args },
                                self.id
                            )
                        )
                    );
                    self.next_id();
                    Ok(out)
                } else {
                    self.next_token();
                    Ok(expr)
                }

            }
            _ => panic!("Err parse expr! {:?}", self.token)
        }
    }

    fn unary_expr(&mut self) -> Result<Rc<RefCell<Expression>>, Error> {

        let out;
        match self.tok0 {
            Some(Token::Mult) => {
                self.next_token(); //consume *
                let expr = self.unary_expr()?;
                out = Rc::new(
                    RefCell::new(
                        Expression::new(Unop { op: UnaryOperator::Dereference, a: expr }, self.id)
                    )
                );
                self.next_id();
                Ok(out)
            }
            Some(Token::Ampersand) => {
                self.next_token(); //consume &
                let expr = self.unary_expr()?;
                out = Rc::new(
                    RefCell::new(
                        Expression::new(Unop {
                            op: UnaryOperator::Pointer,
                            a: expr
                        }, self.id)
                    )
                );
                self.next_id();
                Ok(out)
            }
            Some(Token::Sub) => {
                self.next_token(); //consume -
                let expr = self.unary_expr()?;
                let expr1 = Rc::new(
                    RefCell::new(
                        Expression::new(
                            ExpressionDesc::Number { value: 0 },
                            self.id
                        )
                    )
                );
                self.next_id();
                out = Rc::new(
                    RefCell::new(
                        Expression::new(Binop {
                            a: expr1,
                            op: Operator::Sub,
                            b: expr
                        }, self.id)
                    )
                );
                self.next_id();
                Ok(out)
            }
            _ => self.expr(),
        }
    }

    fn multiplicative_expr(&mut self) -> Result<Rc<RefCell<Expression>>, Error> {

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
            let right = self.unary_expr()?;
            expr = Rc::new(
                RefCell::new(
                    Expression::new(Binop {
                        a: expr,
                        op: operator,
                        b: right
                    }, self.id)
                )
            );
            self.next_id();
        }
        Ok(expr)
    }

    fn additive_expr(&mut self) -> Result<Rc<RefCell<Expression>>, Error> {

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
            let right = self.multiplicative_expr()?;
            expr = Rc::new(
                RefCell::new(
                    Expression::new(Binop {
                        a: expr,
                        op: operator,
                        b: right
                    }, self.id)
                )
            );
            self.next_id();
        }

        Ok(expr)
    }

    fn relational_expr(&mut self) -> Result<Rc<RefCell<Expression>>, Error> {

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
            let right = self.additive_expr()?;
            expr = Rc::new(
                RefCell::new(
                    Expression::new(Binop {
                        a: expr,
                        op: operator,
                        b: right
                    }, self.id)
                )
            );
            self.next_id();
        }
        Ok(expr)
    }

    fn parse_expr(&mut self) -> Result<Rc<RefCell<Expression>>, Error> {
        self.relational_expr()
    }

    fn parse_decl_var(&mut self) -> Result<Vec<Rc<RefCell<Expression>>>, Error> {

        let mut decl_vars = vec![];

        loop {
            if self.tok0 == Some(Token::Semi) {
                break;
            } else if self.tok0 == Some(Token::Comma) {
                self.next_token();
            }

            if let Some(Ident(decl_var_name)) = self.tok0.clone() {
                let tmp_decl = ExpressionDesc::Identifier {
                    name: decl_var_name.clone(),
                    id: self.id
                };
                self.next_id();
                decl_vars.push(
                    Rc::new(
                        RefCell::new(
                            Expression::new(tmp_decl, self.id)
                        )
                    )
                );
                //func_env.fun_env.enter(decl_var_name, TypeDecl::LocalDecl(self.id));
                self.next_id();
                self.next_token();
            }

        }

        Ok(decl_vars)
    }

    fn parse_cond_expr(&mut self) -> Result<Rc<RefCell<Expression>>, Error> {

        self.next_token(); //consume (
        let cond_expr = self.parse_expr()?;
        self.next_token(); // consume )

        Ok(cond_expr)
    }

    fn parse_block(
        &mut self,
        func_env: &mut FunctionDecl
    ) -> Result<Statements, Error> {

        let mut stmts = vec![];
        if self.tok0 == Some(Token::Lcurly) {
            self.next_token(); // consume {
            loop {
                if self.tok0 == Some(Token::Rcurly) {
                    self.next_token();
                    break;
                }
                stmts.push(self.parse_stmt(func_env)?);
            }
        } else {
            stmts.push(self.parse_stmt(func_env)?);
        }
        Ok(stmts)
    }


    fn parse_stmt(
        &mut self,
        func_env: &mut FunctionDecl
    ) -> Result<Statement, Error> {

        let out;
        match self.tok0.clone() {
            Some(Ident(var_name)) => {
                self.next_token();

                if let Some(Token::Assign) = self.tok0.clone() {
                    self.next_token();
                    let value = self.parse_expr()?;
                    self.next_token(); //consume ;
                    let tmp_expr = Rc::new(
                        RefCell::new(
                            Expression::new(
                                ExpressionDesc::Identifier {
                                    name: var_name,
                                    id: 0
                                },
                                self.id
                            )
                        )
                    );
                    self.next_id();
                    out = Statement::new(VarAssignment {
                        target: tmp_expr,
                        value: value
                    });
                    Ok(out)
                } else {
                    panic!("Err ident~~")
                }
            }
            Some(Token::Var)  => {
                self.next_token();
                let names = self.parse_decl_var()?;
                self.next_token(); //consume ;
                out = Statement::new(LocalDecl {
                    names
                });
                Ok(out)
            }
            Some(Token::Output) => {
                self.next_token();
                let out_expr = self.parse_expr()?;
                self.next_token(); //consume ;
                out = Statement::new(
                    Output {
                        target: out_expr,
                    }
                );
                Ok(out)
            }
            Some(Token::Return) => {
                self.next_token();
                let ret_expr = self.parse_expr()?;
                self.next_token(); //consume ;
                out = Statement::new(
                    Return {
                        value: ret_expr,
                    }
                );
                Ok(out)
            }
            Some(Token::Error) => {
                self.next_token();
                let err_expr = self.parse_expr()?;
                self.next_token(); //consume ;
                out = Statement::new(StatementDesc::Err {
                    value: err_expr
                });
                Ok(out)
            }
            Some(Token::While) => {
                self.next_token();
                let cond_expr = self.parse_cond_expr()?;
                //self.next_token(); // consume {
                let out_stmts = self.parse_block(func_env)?;
                out = Statement::new(While {
                    cond: cond_expr,
                    body: out_stmts,
                });
                Ok(out)
            }
            Some(Token::If) => {
                self.next_token();
                let cond_expr = self.parse_cond_expr()?;
                let then_stmts = self.parse_block(func_env)?;
                if let Some(Token::Else) = self.tok0 {
                    self.next_token(); //consume else
                    let else_stmts = self.parse_block(func_env)?;
                    out = Statement::new(If {
                        cond: cond_expr,
                        then: then_stmts,
                        orelse: Some(else_stmts),
                    });
                    Ok(out)
                } else {
                    out = Statement::new(If {
                        cond: cond_expr,
                        then: then_stmts,
                        orelse: None,
                    });
                    Ok(out)
                }
            }
            Some(Token::Mult) => {
                let left = self.parse_expr()?;
                self.next_token();
                let right = self.parse_expr()?;
                self.next_token(); //consume ;
                out = Statement::new(PointerAssignment {
                    target: left,
                    value: right,
                });
                Ok(out)
            }
            Some(Token::Lcurly) => {
                out = Statement::new(Block {
                    body: self.parse_block(func_env)?,
                });
                Ok(out)
            }
            _ => panic!("Err in parse stmt~~ {:?}", self.result),
        }
    }

    fn parse_paras(&mut self) -> Result<Vec<Rc<RefCell<Expression>>>, Error> {
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
                let tmp_expr = Rc::new(
                    RefCell::new(
                        Expression::new(
                            ExpressionDesc::Identifier {
                                name: para_name.clone(),
                                id: 0
                            },
                            self.id
                        )
                    )
                );
                //func_env.fun_env.enter(para_name, TypeDecl::ParameterDecl(self.id));
                self.next_id();
                paras.push(tmp_expr);

                self.next_token(); // consume para
            }
        }
        Ok(paras)
    }

    fn parse_func(&mut self) -> Result<FunctionDecl, Error> {
        let mut tmp_func = FunctionDecl::new(
            Symbols::new(),
            self.id
        );
        //tmp_func.fun_env.begin_scope();
        self.next_id();
        if let Some(Ident(fun_name)) = self.tok0.clone() {
            tmp_func.function_name = fun_name;
            self.next_token(); //consume function name
        }
        self.result.prog_env.enter(
            tmp_func.function_name.clone(),
            TypeDecl::FunctionDecl(tmp_func.fun_id)
        );
        tmp_func.paras = self.parse_paras()?;
        tmp_func.body = self.parse_block(tmp_func.borrow_mut())?;

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