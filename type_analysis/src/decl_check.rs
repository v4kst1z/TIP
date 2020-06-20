use parser::ast::{Program, FunctionDecl, Statement, Expression, StatementDesc, ExpressionDesc, TypeDecl, UnaryOperator};
use parser::symbol::Symbols;
use std::borrow::{BorrowMut, Borrow};

fn decl_check_expr(expr: &Expression, global: Symbols) -> bool {
    match &expr.expr {
        ExpressionDesc::Identifier { name, id: _ } => {
            if let Some(_) = global.look(name.clone()) {
                return true;
            }
            println!("Identifier {} used but not declared.", name);
            return false;
        }
        ExpressionDesc::Binop { a, op: _, b } => {
            return decl_check_expr((*a).borrow(), global.clone()) && decl_check_expr((*b).borrow(), global);
        }
        ExpressionDesc::Unop { op:_, a } => {
            return decl_check_expr((*a).borrow(), global.clone());
        }
        ExpressionDesc::Call { function_name, args } => {
            let mut out = check_fun_id(function_name.expr.borrow(), global.clone());
            for expr in args.iter() {
                out = out && decl_check_expr(expr, global.clone());
            }
            return out;
        }
        ExpressionDesc::PointerInvocation { function_name, args } => {
            if let ExpressionDesc::Unop { op, a } = &function_name.expr {
                if let UnaryOperator::Dereference = op {
                    let mut out = check_id(a.expr.borrow(), global.borrow()) ;
                    for expr in args.iter() {
                        out = out && decl_check_expr(expr, global.clone());
                    }
                    return out;
                }
            }
            return false;
        }
        _ => true
    }
}

fn check_fun_id(expr: &ExpressionDesc,  global: Symbols) -> bool {
    if let Some(name) = get_id(expr) {
        if let Some(_) = global.look(name) {
            return true;
        }
    }
    return false;
}

fn get_id(expr: &ExpressionDesc) -> Option<String> {
    return if let ExpressionDesc::Identifier { name, id: _ } = expr {
        Some(name.parse().unwrap())
    } else {
        None
    }
}

fn check_id(expr: &ExpressionDesc,  global: &Symbols) -> bool {
    if let Some(name) = get_id(expr.borrow()) {
        if let Some(_) = global.look(name.clone()) {
            return true;
        }
        println!("Identifier {} used but not declared.", name);
    }
    return false;
}

fn decl_check_stmt(stmt: &Statement,  global: &mut Symbols) -> bool {
    return match &stmt.stmt {
        StatementDesc::VarAssignment { target, value } => {
            check_id(target.expr.borrow(), global) && decl_check_expr((*value).borrow(), global.to_owned())
        }
        StatementDesc::PointerAssignment { target, value } => {
            if let ExpressionDesc::Unop { op, a } = &target.expr {
                if let UnaryOperator::Dereference = op {
                    let mut out = check_id(a.expr.borrow(), global);
                    out = out && decl_check_expr((*value).borrow(), global.to_owned());
                    return out;
                }
            }
            false
        }
        StatementDesc::LocalDecl { names } => {
            for expr in names.iter() {
                if let ExpressionDesc::Identifier { name, id } = &expr.expr {
                    global.enter(name.to_string(), TypeDecl::LocalDecl(*id));
                } else {
                    return false;
                }
            }
            true
        }
        StatementDesc::Output { target } => {
            decl_check_expr(target.borrow(), global.to_owned())
        }
        StatementDesc::Return { value } => {
            decl_check_expr(value.borrow(), global.to_owned())
        }
        StatementDesc::Err { value } => {
            decl_check_expr(value.borrow(), global.to_owned())
        }
        StatementDesc::While { cond, body } => {
            let mut out = decl_check_expr(cond.borrow(), global.to_owned());
            for stmt in body.iter() {
                out = out && decl_check_stmt(stmt, global);
            }
            out
        }
        StatementDesc::If { cond, then, orelse } => {
            let mut out = decl_check_expr(cond.borrow(), global.to_owned());
            for stmt in then.iter() {
                out = out && decl_check_stmt(stmt, global);
            }
            if let Some(else_stmt) = orelse {
                for stmt in else_stmt.iter() {
                    out = out && decl_check_stmt(stmt, global);
                }
            }
            out
        }
        StatementDesc::Block { body } => {
            let mut out = true;
            for stmt in body.iter() {
                out = out && decl_check_stmt(stmt, global);
            }
            out
        }
    }
}

fn decl_check_fun(fun: &mut FunctionDecl, global: Symbols) -> bool {
    fun.fun_env = global;
    for para in fun.paras.iter() {
        if let ExpressionDesc::Identifier { name, id } = &para.expr {
            fun.fun_env.enter(name.to_string(), TypeDecl::ParameterDecl(*id));
        }
    }

    for stmt in fun.body.iter_mut() {
        if decl_check_stmt(stmt, fun.fun_env.borrow_mut()) == false {
            return false;
        }
    }
    true
}

pub fn decl_check(mut prog: Program) -> bool {
    let global_fun = prog.prog_env.clone();
    let mut out = true;
    for fun in prog.fun_decl.iter_mut() {
        out = out && decl_check_fun(fun, global_fun.clone())
    }
    out
}