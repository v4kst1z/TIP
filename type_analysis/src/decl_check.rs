use parser::symbol::Symbols;
use std::borrow::{BorrowMut, Borrow};
use std::cell::RefCell;
use std::rc::Rc;
use parser::ast::{
    Program,
    FunctionDecl,
    Statement,
    Expression,
    StatementDesc,
    ExpressionDesc,
    TypeDecl,
    UnaryOperator
};


fn decl_check_expr(expr: Rc<RefCell<Expression>>, global: Symbols) -> bool {
    let tmp_expr = &*expr.as_ref().borrow();
    match &tmp_expr.expr {
        ExpressionDesc::Identifier { name, id: _ } => {
            if let Some(_) = global.look(name.clone()) {
                return true;
            }
            println!("Identifier {} used but not declared.", name);
            return false;
        }
        ExpressionDesc::Binop { a, op: _, b } => {
            return decl_check_expr(
                a.clone(),
                global.clone()) && decl_check_expr(b.clone(), global
            );
        }
        ExpressionDesc::Unop { op:_, a } => {
            return decl_check_expr(a.clone(), global.clone());
        }
        ExpressionDesc::Call { function_name, args } => {
            let mut out = check_fun_id(function_name.as_ref().borrow().expr.borrow(), global.clone());
            for expr in args.iter() {
                out = out && decl_check_expr(expr.clone(), global.clone());
            }
            return out;
        }
        ExpressionDesc::PointerInvocation { function_name, args } => {
            if let ExpressionDesc::Unop { ref  op, ref a } = function_name.as_ref().borrow().expr {
                if let UnaryOperator::Dereference = op {
                    //let tmp_a = a.clone();
                    let e_expr =  &*a.as_ref().borrow();
                    let mut out = check_id(e_expr.expr.borrow(), global.borrow()) ;
                    for expr in args.iter() {
                        out = out && decl_check_expr(expr.clone(), global.clone());
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

fn check_point_assign_id(expr: &ExpressionDesc,  global: &Symbols) -> bool {
    if let Some(name) = get_id(expr.borrow()) {
        return match global.look(name.clone()).expect("Identifier used but not declared.") {
            TypeDecl::FunctionDecl(_0) => {
                println!("Identifier {} used but not declared.", name);
                false
            }
            _ => {
                true
            }
        }
    }
    return false;
}

fn decl_check_stmt(stmt: &Statement,  global: &mut Symbols) -> bool {
    return match &stmt.stmt {
        StatementDesc::VarAssignment { target, value } => {
            let target_expr = &target.as_ref().borrow().expr;
            check_id(target_expr, global) && decl_check_expr(value.clone(), global.to_owned())
        }
        StatementDesc::PointerAssignment { target, value } => {
            let target_expr = &target.as_ref().borrow().expr;
            if let ExpressionDesc::Unop {  op,  a } = target_expr {
                if let UnaryOperator::Dereference = op {
                    let a_expr = &a.as_ref().borrow().expr;
                    return if let ExpressionDesc::Identifier { name: _, id: _ } = a_expr {
                        let mut out = check_point_assign_id(a_expr, global);
                        out = out && decl_check_expr(value.clone(), global.to_owned());
                        out
                    } else {
                        decl_check_expr(a.clone(), global.to_owned())
                    }
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
            decl_check_expr(target.clone(), global.to_owned())
        }
        StatementDesc::Return { value } => {
            decl_check_expr(value.clone(), global.to_owned())
        }
        StatementDesc::Err { value } => {
            decl_check_expr(value.clone(), global.to_owned())
        }
        StatementDesc::While { cond, body } => {
            let mut out = decl_check_expr(cond.clone(), global.to_owned());
            for stmt in body.iter() {
                out = out && decl_check_stmt(stmt, global);
            }
            out
        }
        StatementDesc::If { cond, then, orelse } => {
            let mut out = decl_check_expr(cond.clone(), global.to_owned());
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
    for para in fun.paras.iter_mut() {
        let tmp_expr = &para.as_ref().borrow().expr;
        if let ExpressionDesc::Identifier { name, id } = tmp_expr {
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

pub fn decl_check(prog: &mut Program) -> bool {
    let global_fun = prog.prog_env.clone();
    let mut out = true;
    for fun in prog.fun_decl.iter_mut() {
        out = out && decl_check_fun(fun, global_fun.clone())
    }
    out
}