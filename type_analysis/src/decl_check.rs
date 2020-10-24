use parser::symbol::Symbols;
use std::borrow::{BorrowMut};
use parser::ast::{
    Program,
    FunctionDecl,
    Statement,
    StatementDesc,
    ExpressionDesc,
    TypeDecl,
    UnaryOperator
};

/*
fn get_decl_id(decl: &TypeDecl) -> Option<u64> {
    return match decl {
        TypeDecl::FunctionDecl(id) => Some(*id),
        TypeDecl::LocalDecl(id) => Some(*id),
        TypeDecl::ParameterDecl(id) => Some(*id),
    }
}
*/

fn decl_check_expr(expr: &mut ExpressionDesc, global: Symbols) -> bool {
    match expr.borrow_mut() {
        ExpressionDesc::Identifier { name, id: _ } => {
            if let Some(_decl) = global.look(name.clone()) {
                //*id = get_decl_id(decl).unwrap();
                return true;
            }
            println!("Identifier {} used but not declared.", name);
            return false;
        }
        ExpressionDesc::Binop { a, op: _, b } => {
            return decl_check_expr(
                a.as_ref().borrow_mut().expr.borrow_mut(),
                global.clone()) && decl_check_expr(
                b.as_ref().borrow_mut().expr.borrow_mut(),
                global
            );
        }
        ExpressionDesc::Unop { op:_, a } => {
            return decl_check_expr(
                a.as_ref().borrow_mut().expr.borrow_mut(),
                global.clone()
            );
        }
        ExpressionDesc::Call { function_name, args } => {
            let mut out = check_fun_id(function_name.as_ref().borrow_mut().expr.borrow_mut(), global.clone());
            for expr in args.iter() {
                out = out && decl_check_expr(
                    expr.as_ref().borrow_mut().expr.borrow_mut(),
                    global.clone()
                );
            }
            return out;
        }
        ExpressionDesc::PointerInvocation { function_name, args } => {
            if let ExpressionDesc::Unop { op, a } = function_name.as_ref().borrow_mut().expr.borrow_mut() {
                if let UnaryOperator::Dereference = op {
                    let e_expr =  unsafe { (*a.as_ptr()).borrow_mut() };
                    let mut out = false;
                    if let ExpressionDesc::Identifier { name, id: _} = e_expr.expr.borrow_mut() {
                        if let Some(_decl) = global.look(name.clone()) {
                            //*id = get_decl_id(decl).unwrap();
                            out = true;
                        }
                    }
                    for expr in args.iter() {
                        out = out && decl_check_expr(
                            expr.as_ref().borrow_mut().expr.borrow_mut(),
                            global.clone()
                        );
                    }
                    return out;
                }
            }
            return false;
        }
        _ => true
    }
}

fn check_fun_id(expr: &mut ExpressionDesc,  global: Symbols) -> bool {
    if let ExpressionDesc::Identifier { name, id:_ } = expr {
        if let Some(_decl) = global.look(name.parse().unwrap()) {
            //*id = get_decl_id(decl).unwrap();
            return true;
        }
    }
    return false;
}

fn decl_check_stmt(stmt: &mut Statement,  global: &mut Symbols) -> bool {
    return match stmt.stmt.borrow_mut() {
        StatementDesc::VarAssignment { target, value } => {
            let target_expr = unsafe { (*target.as_ptr()).expr.borrow_mut() };
            let mut out = false;
            if let ExpressionDesc::Identifier { name, id: _} = target_expr {
                if let Some(_decl) = global.look(name.clone()) {
                    //*id = get_decl_id(decl).unwrap();
                    out = true;
                }
            }
            out && decl_check_expr(value.as_ref().borrow_mut().expr.borrow_mut(), global.to_owned())
        }
        StatementDesc::PointerAssignment { target, value } => {
            let target_expr = &target.as_ref().borrow().expr;
            if let ExpressionDesc::Unop {  op,   a } = target_expr {
                if let UnaryOperator::Dereference = op {
                    let mut out = false;
                    let expr = unsafe { (*a.as_ptr()).expr.borrow_mut() };
                    if let ExpressionDesc::Identifier { name, id: _ } = expr {
                        match global.look(name.clone()).expect("Identifier used but not declared.") {
                            TypeDecl::FunctionDecl(_0) => {
                                println!("Identifier {} used but not declared.", name);
                            }
                            TypeDecl::ParameterDecl(_decl_id) => {
                                //*id = *decl_id;
                                out = true;
                            }
                            TypeDecl::LocalDecl(_decl_id) => {
                                //*id = *decl_id;
                                out = true;
                            }
                        }
                    } else {
                        decl_check_expr(expr, global.to_owned());
                    }
                    return out && decl_check_expr(value.as_ref().borrow_mut().expr.borrow_mut(), global.to_owned());
                }
            }
            false
        }
        StatementDesc::LocalDecl { names } => {
            for expr in names.iter() {
                let tmp_expr = &expr.as_ref().borrow().expr;
                if let ExpressionDesc::Identifier { name, id } = tmp_expr {
                    if let None = global.look(name.clone()) {
                        global.enter(name.to_string(), TypeDecl::LocalDecl(*id));
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }
            }
            true
        }
        StatementDesc::Output { target } => {
            decl_check_expr(target.as_ref().borrow_mut().expr.borrow_mut(), global.to_owned())
        }
        StatementDesc::Return { value } => {
            decl_check_expr(value.as_ref().borrow_mut().expr.borrow_mut(), global.to_owned())
        }
        StatementDesc::Err { value } => {
            decl_check_expr(value.as_ref().borrow_mut().expr.borrow_mut(), global.to_owned())
        }
        StatementDesc::While { ref mut cond, ref mut body } => {
            let mut out = decl_check_expr(cond.as_ref().borrow_mut().expr.borrow_mut(), global.to_owned());
            for stmt in body.iter_mut() {
                out = out && decl_check_stmt(stmt, global);
            }
            out
        }
        StatementDesc::If { ref mut cond, ref mut then, ref mut orelse } => {
            let mut out = decl_check_expr(cond.as_ref().borrow_mut().expr.borrow_mut(), global.to_owned());
            for stmt in then.iter_mut() {
                out = out && decl_check_stmt(stmt, global);
            }
            if let Some(ref mut else_stmt) = orelse {
                for stmt in else_stmt.iter_mut() {
                    out = out && decl_check_stmt(stmt, global);
                }
            }
            out
        }
        StatementDesc::Block {ref mut body } => {
            let mut out = true;
            for stmt in body.iter_mut() {
                out = out && decl_check_stmt(stmt, global);
            }
            out
        }
    }
}

fn decl_check_fun(fun: &mut FunctionDecl, global: Symbols) -> bool {
    fun.fun_env = global;
    for para in fun.paras.iter_mut() {
        let tmp_expr = &para.as_ref().borrow().expr;
        if let ExpressionDesc::Identifier { name, id } = tmp_expr {
            fun.fun_env.enter(name.clone(), TypeDecl::ParameterDecl(*id));
        }
    }

    for stmt in fun.body.iter_mut() {
        if decl_check_stmt(
            stmt,
            fun.fun_env.borrow_mut()
        ) == false {
            return false;
        }
    }
    true
}

pub fn decl_check(prog: &mut Program) -> bool {
    let global_fun = prog.prog_env.clone();
    let mut out = true;
    for fun in prog.fun_decl.iter_mut() {
        out = out && decl_check_fun(fun, global_fun.clone())
    }
    out
}