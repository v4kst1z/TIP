use std::cell::RefCell;
use std::rc::Rc;
use std::borrow::{
    Borrow,
    BorrowMut
};
use parser::ast::{
    Program,
    FunctionDecl,
    Statement,
    Expression,
    StatementDesc,
    ExpressionDesc,
    Statements,
    UnaryOperator
};

fn gen_id(fun_name: String, var_idx: &mut i32) -> Expression {
    let var_id = format!("normalize_var_{}_{}", fun_name, var_idx);
    let expr = Expression::new(
        ExpressionDesc::Identifier {
            name: var_id,
            id: 0
        },
        0
    );
    *var_idx += 1;
    expr
}

fn gen_normalize_expr_ast(
    expr: Rc<RefCell<Expression>>,
    fun_name: String,
    var_idx: &mut i32
) -> (Rc<RefCell<Expression>>, Vec<Statement>) {
    let mut stmts = Vec::new();
    match expr.as_ref().borrow().expr.borrow() {
        ExpressionDesc::Malloc | ExpressionDesc::Input | ExpressionDesc::Null => {
            (expr.clone(), vec![])
        }
        ExpressionDesc::Identifier { name: _, id: _ } => {
            (expr.clone(), vec![])
        }
        ExpressionDesc::Number { value: _ }  => {
            (expr.clone(), vec![])
        }
        ExpressionDesc::Binop { a, op, b } => {
            let mut process = |a: Rc<RefCell<Expression>> | -> Rc<RefCell<Expression>> {
                match a.as_ref().borrow().expr.borrow() {
                    ExpressionDesc::Identifier { name:_, id: _ } => {
                        a.clone()
                    }
                    ExpressionDesc::Number { value: _ } => {
                        a.clone()
                    }
                    _ => {
                        let (normalize_expr, mut ret_stmts) = gen_normalize_expr_ast(
                            a.clone(),
                            fun_name.clone(),
                            var_idx
                        );
                        stmts.append(ret_stmts.as_mut());
                        let l_expr = Rc::new(RefCell::new(gen_id(fun_name.clone(), var_idx)));
                        let l_decl = Statement::new(
                            StatementDesc::LocalDecl {
                                names: vec![l_expr.clone()]
                            }
                        );
                        let l_assign = Statement::new(
                            StatementDesc::VarAssignment {
                                target: l_expr.clone(),
                                value: normalize_expr
                            }
                        );
                        stmts.push(l_decl);
                        stmts.push(l_assign);
                        l_expr
                    }
                }
            };
            let l_expr = process(a.clone());
            let r_expr = process(b.clone());
            let p_expr = Rc::new(
                RefCell::new(
                    Expression::new(
                        ExpressionDesc::Binop {
                            a: l_expr,
                            op: op.clone(),
                            b: r_expr
                        },0
                    )
                )
            );
            (p_expr, stmts)
        }
        ExpressionDesc::Unop { op, a } => {
            let mut un_expr = Rc::new(RefCell::new(gen_id(fun_name.clone(), var_idx)));
            match a.as_ref().borrow().expr.borrow() {
                ExpressionDesc::Identifier { name: _, id: _ } => {
                    un_expr = a.clone();
                }
                _ => {
                    let (normalize_expr, mut unop_stmts) = gen_normalize_expr_ast(
                        a.clone(),
                        fun_name.clone(),
                        var_idx
                    );
                    let un_decl = Statement::new(
                        StatementDesc::LocalDecl {
                            names: vec![un_expr.clone()]
                        }
                    );
                    let un_assign = Statement::new(
                        StatementDesc::VarAssignment {
                            target: un_expr.clone(),
                            value: normalize_expr
                        }
                    );
                    stmts.append(unop_stmts.as_mut());
                    stmts.push(un_decl);
                    stmts.push(un_assign);
                }
            }

            let unop_expr = Rc::new(
                RefCell::new(
                    Expression::new(
                        ExpressionDesc::Unop {
                            op: op.clone(),
                            a: un_expr
                        }, 0
                    )
                )
            );
            (unop_expr, stmts)
        }
        ExpressionDesc::Call { function_name, args } => {
            let mut normalize_args = Vec::new();
            for arg in args.iter() {
                let (normalize_expr, mut arg_stmts) = gen_normalize_expr_ast(
                    arg.clone(),
                    fun_name.clone(),
                    var_idx
                );
                normalize_args.push(normalize_expr);
                stmts.append(arg_stmts.as_mut());
            }
            let call_expr = Rc::new(
                RefCell::new(
                    Expression::new(
                        ExpressionDesc::Call {
                            function_name: function_name.clone(),
                            args: normalize_args
                        }, 0
                    )
                )
            );
            (call_expr, stmts)
        }
        ExpressionDesc::PointerInvocation { function_name, args } => {
            let mut normalize_p_args = Vec::new();
            for p_arg in args.iter() {
                let (normalize_expr_p, mut p_arg_stmts) = gen_normalize_expr_ast(
                    p_arg.clone(),
                    fun_name.clone(),
                    var_idx
                );
                normalize_p_args.push(normalize_expr_p);
                stmts.append(p_arg_stmts.as_mut());
            }
            let pcall_expr = Rc::new(
                RefCell::new(
                    Expression::new(
                        ExpressionDesc::PointerInvocation {
                            function_name: function_name.clone(),
                            args: normalize_p_args
                        }, 0
                    )
                )
            );
            (pcall_expr, stmts)
        }
    }
}

fn gen_normalize_stmt_ast(
    stmt: &Statement,
    fun_name: String,
    var_idx: &mut i32
) -> Vec<Statement> {
    let mut normalize_stmts = Vec::new();
    match stmt.stmt.borrow() {
        StatementDesc::VarAssignment { target, value } => {
            let (normalize_expr, mut stmts) = gen_normalize_expr_ast(value.clone(), fun_name, var_idx);
            normalize_stmts.append(stmts.as_mut());
            let var_stmt = StatementDesc::VarAssignment { target: target.clone(), value: normalize_expr };
            normalize_stmts.push(Statement::new(var_stmt));
        }
        StatementDesc::LocalDecl { names } => {
            let mut normalize_decl = Vec::new();
            for decl in names.iter() {
                normalize_decl.push(decl.clone());
            }
            let decl_stmt = StatementDesc::LocalDecl { names: normalize_decl };
            normalize_stmts.push(Statement::new(decl_stmt));
        }
        StatementDesc::Output { target } => {
            let (normalize_expr, mut stmts) = gen_normalize_expr_ast(target.clone(), fun_name, var_idx);
            let output_stmt = StatementDesc::Output { target: normalize_expr };
            normalize_stmts.append(stmts.as_mut());
            normalize_stmts.push(Statement::new(output_stmt));
        }
        StatementDesc::While { cond, body } => {
            let (normalize_expr, mut stmts) = gen_normalize_expr_ast(cond.clone(), fun_name.clone(), var_idx);
            let mut while_stmts: Vec<Statement> = Vec::new();
            for stmt in body.iter() {
                while_stmts.append(
                    gen_normalize_stmt_ast(stmt, fun_name.clone(), var_idx).as_mut()
                );
            }
            normalize_stmts.append(stmts.as_mut());
            let while_stmt = StatementDesc::While {
                cond: normalize_expr,
                body: while_stmts
            };
            normalize_stmts.push(Statement::new(while_stmt));
        }
        StatementDesc::Return { value } => {
            match value.as_ref().borrow().expr.borrow() {
                ExpressionDesc::Identifier { name: _, id: _ } => {
                    let ret_stmt = StatementDesc::Return { value: value.clone() };
                    normalize_stmts.push(Statement::new(ret_stmt));
                }
                _ => {
                    let (normalize_expr, mut stmts) = gen_normalize_expr_ast(value.clone(), fun_name.clone(), var_idx);
                    let ret_expr = Rc::new(RefCell::new(gen_id(fun_name, var_idx)));
                    let stmt_decl = Statement::new(
                        StatementDesc::LocalDecl {
                            names: vec![ret_expr.clone()]
                        }
                    );
                    let stmt_assign = Statement::new(
                        StatementDesc::VarAssignment {
                            target: ret_expr.clone(),
                            value: normalize_expr
                        }
                    );
                    let stmt_ret = Statement::new(
                        StatementDesc::Return {
                            value: ret_expr
                        }
                    );
                    normalize_stmts.append(stmts.as_mut());
                    normalize_stmts.append(vec![stmt_decl, stmt_assign, stmt_ret].as_mut());
                }
            }
        }
        StatementDesc::Err { value } => {
            let (normalize_expr, mut stmts) = gen_normalize_expr_ast(value.clone(), fun_name, var_idx);
            let err_stmt = StatementDesc::Err { value: normalize_expr };
            normalize_stmts.append(stmts.as_mut());
            normalize_stmts.push(Statement::new(err_stmt));
        }
        StatementDesc::If { cond, then, orelse } => {
            let (normalize_expr, mut stmts) = gen_normalize_expr_ast(cond.clone(), fun_name.clone(), var_idx);
            normalize_stmts.append(stmts.as_mut());
            let mut if_then_stmts: Vec<Statement> = Vec::new();
            for stmt in then.iter() {
                if_then_stmts.append(
                    gen_normalize_stmt_ast(stmt, fun_name.clone(), var_idx).as_mut()
                );
            }
            let mut or_stmts: Option<Statements> = None;
            if orelse != &None {
                let mut or_else_stmts: Vec<Statement> = Vec::new();
                for stmt in orelse.as_ref().unwrap().iter() {
                    or_else_stmts.append(
                        gen_normalize_stmt_ast(stmt, fun_name.clone(), var_idx).as_mut()
                    );
                }
                or_stmts = Some(or_else_stmts);
            }
            normalize_stmts.append(stmts.as_mut());
            let if_stmt = StatementDesc::If {
                cond: normalize_expr,
                then: if_then_stmts,
                orelse: or_stmts
            };
            normalize_stmts.push(Statement::new(if_stmt));
        }
        StatementDesc::Block { body } => {
            let mut block_stmts: Vec<Statement> = Vec::new();
            for stmt in body.iter() {
                block_stmts.append(
                    gen_normalize_stmt_ast(stmt, fun_name.clone(), var_idx).as_mut()
                );
            }
            let block_stmt = StatementDesc::Block {
                body: block_stmts
            };
            normalize_stmts.push(Statement::new(block_stmt));
        }
        StatementDesc::PointerAssignment { target, value } => {
            match (target.as_ref().borrow().expr.borrow(), value.as_ref().borrow().expr.borrow()) {
                (ExpressionDesc::Unop { op, a: _ }, ExpressionDesc::Identifier { name: _, id: _ }) => {
                    if op == &UnaryOperator::Dereference {
                        let p_stmt = StatementDesc::PointerAssignment {
                            target: target.clone(),
                            value: value.clone()
                        };
                        normalize_stmts.push(Statement::new(p_stmt));
                    }
                }
                (ExpressionDesc::Unop { op, a: _ }, _) => {
                    if op == &UnaryOperator::Dereference {
                        let (normalize_expr, mut stmts) = gen_normalize_expr_ast(value.clone(), fun_name.clone(), var_idx);
                        let p_expr = Rc::new(RefCell::new(gen_id(fun_name, var_idx)));
                        let p_decl = Statement::new(
                            StatementDesc::LocalDecl {
                                names: vec![p_expr.clone()]
                            }
                        );
                        let p_assign = Statement::new(
                            StatementDesc::VarAssignment {
                                target: p_expr.clone(),
                                value: normalize_expr
                            }
                        );

                        let p_stmt = StatementDesc::PointerAssignment {
                            target: target.clone(),
                            value: p_expr
                        };
                        normalize_stmts.append(stmts.as_mut());
                        normalize_stmts.push(p_decl);
                        normalize_stmts.push(p_assign);
                        normalize_stmts.push(Statement::new(p_stmt));
                    }
                }
                _ => {
                    panic!("Err {:?}", stmt.stmt)
                }
            }
        }
    };
    normalize_stmts
}

fn gen_normalize_function_ast(
    fun: & FunctionDecl,
) -> FunctionDecl {
    let mut normalize_fun_ast = FunctionDecl::new(fun.fun_env.clone(), fun.fun_id);
    let fun_name = fun.function_name.clone();
    let mut idx = 0;
    normalize_fun_ast.function_name = fun.function_name.clone();
    normalize_fun_ast.paras = fun.paras.clone();
    for stmt in fun.body.iter() {
        let mut normalize_stmt = gen_normalize_stmt_ast(
            stmt,
            fun_name.clone(),
            idx.borrow_mut()
        );
        normalize_fun_ast.body.append(normalize_stmt.as_mut());
    }
    normalize_fun_ast
}

pub fn generate_normalize_ast(
    prog: &Program
) -> Program {
    let mut normalize_ast = Program::new();
    normalize_ast.prog_env = prog.prog_env.clone();
    for fun in prog.fun_decl.iter() {
        let normalize_fun = gen_normalize_function_ast(fun);
        normalize_ast.fun_decl.push(normalize_fun);
    }
    normalize_ast
}