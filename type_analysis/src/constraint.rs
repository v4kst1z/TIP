use parser::symbol::Symbols;
use std::rc::Rc;
use std::borrow::{Borrow, BorrowMut};
use std::cell::{RefCell};
use parser::ast::{
    Expression,
    ExpressionDesc,
    UnaryOperator,
    TypeDecl,
    Statement,
    StatementDesc,
    FunctionDecl,
    Program
};
use crate::constraint::TypeVar::{
    ExpTy,
    PointerTy,
    Alpha,
    Int,
    FunctionTy
};


#[derive(Debug, PartialEq)]
enum TypeVar {
    Alpha(usize),
    Int,
    ExpTy(Rc<RefCell<Expression>>),
    PointerTy(Box<TypeVar>),
    FunctionTy(Vec<Box<TypeVar>>, Box<TypeVar>)
}

#[derive(Debug, PartialEq)]
pub struct TypeConstraint(TypeVar, TypeVar);

fn get_expr_id(env: &Symbols, name: String) -> u64 {
    *match env.look(name).unwrap() {
        TypeDecl::FunctionDecl(id) => id,
        TypeDecl::ParameterDecl(id) => id,
        TypeDecl::LocalDecl(id) => id,
    }
}

fn get_name(expr: &ExpressionDesc) -> Option<String> {
    return match expr {
        ExpressionDesc::Identifier { name, id: _ } => {
            Some(name.to_owned())
        }
        ExpressionDesc::Unop { op: _, a } => {
            match a.as_ref().borrow().expr.borrow() {
                ExpressionDesc::Identifier { name, id: _ } => {
                    Some(name.to_owned())
                }
                _ => {
                    None
                }
            }
        }
        ExpressionDesc::Call { function_name, args: _ } => {
            if let ExpressionDesc::Identifier { name, id: _ } =  function_name.as_ref().borrow().expr.borrow() {
                Some(name.to_owned())
            } else { None }
        }
        _ => {
            None
        }
    };
}


fn generate_type_constraints_from_exp(
    expr:  Rc<RefCell<Expression>>,
    mut constraints: Vec<TypeConstraint>,
    env: &Symbols
) -> Vec<TypeConstraint> {

    match expr.as_ref().borrow().expr.borrow() {
        ExpressionDesc::Malloc | ExpressionDesc::Null => {
            let uid = constraints.len();
            constraints.push(TypeConstraint(ExpTy(expr.clone()), PointerTy(Box::new(Alpha(uid)))));
            return constraints;
        }
        ExpressionDesc::Input => {
            constraints.push(TypeConstraint(ExpTy(expr.clone()), Int));
            return constraints;
        }
        ExpressionDesc::Number  { value: _ } => {
            constraints.push(TypeConstraint(ExpTy(expr.clone()), Int));
            return constraints;
        }
        ExpressionDesc::Identifier { name: _, id: _ } => {
            return constraints;
        }
        ExpressionDesc::Binop { ref a, op: _,  ref b } => {
            let mut constraints = generate_type_constraints_from_exp(
                a.clone(), constraints, env.borrow()
            );
            constraints = generate_type_constraints_from_exp(
                b.clone(), constraints, env
            );

            constraints.push(TypeConstraint(ExpTy(a.clone()), Int));
            constraints.push(TypeConstraint(ExpTy(b.clone()), Int));
            constraints.push(TypeConstraint(ExpTy(expr.clone()), Int));
            return constraints;
        }
        ExpressionDesc::Unop { op, a } => {
            match op {
                UnaryOperator::Pointer => {
                    if let Some(name) = get_name(a.as_ref().borrow().expr.borrow()) {
                        unsafe {
                            (*a.as_ptr()).expr_id = get_expr_id(env, name.to_owned());
                        }
                        constraints.push(
                            TypeConstraint(ExpTy(expr.clone()),
                                           PointerTy(Box::new(ExpTy(a.clone()))))
                        );
                    }
                    return constraints;
                }
                UnaryOperator::Dereference => {
                    constraints.push(TypeConstraint(
                        ExpTy(a.clone()), PointerTy(Box::new(ExpTy(expr.clone()))))
                    );
                    return constraints;
                }
                _ => panic!("Err~~~")
            }
        }
        ExpressionDesc::Call { function_name, args } => {
            for expr in args.iter() {
                constraints = generate_type_constraints_from_exp(
                    expr.clone(),
                    constraints,
                    env.borrow()
                );
            }
            let mut para_cons = vec![];
            for para in args.iter() {
                para_cons.push(Box::new(ExpTy(para.clone())));
            }
            if let Some(name) = get_name(function_name.as_ref().borrow().expr.borrow()) {
                unsafe {
                    (*function_name.as_ptr()).expr_id = get_expr_id(env, name.to_owned());
                }
                constraints.push(TypeConstraint(
                    ExpTy(function_name.clone()),
                    FunctionTy(para_cons, Box::new(ExpTy(expr.clone())))
                ));
            }
            return constraints;
        }
        ExpressionDesc::PointerInvocation {function_name, args } => {
            for expr in args.iter() {
                constraints = generate_type_constraints_from_exp(
                    expr.clone(), constraints, env.borrow()
                );
            }
            let mut para_cons = vec![];
            for para in args.iter() {
                println!("{:?}", para);
                let para_name = get_name(para.as_ref().borrow().expr.borrow());
                unsafe {
                    (*para.as_ptr()).expr_id = get_expr_id(env, para_name.unwrap());
                }

                para_cons.push(Box::new(ExpTy(para.clone())));
            }
            constraints.push(TypeConstraint(
                ExpTy(function_name.clone()), FunctionTy(para_cons, Box::new(ExpTy(expr.clone())))
            ));
            return constraints;
        }
    }
}


fn generate_type_constraints_from_stmt(
    stmt: &mut Statement,
    mut constraints: Vec<TypeConstraint>,
    env: &Symbols
) -> Vec<TypeConstraint> {
    match stmt.stmt.borrow_mut(){
        StatementDesc::VarAssignment { ref mut target, ref mut value } => {
            constraints = generate_type_constraints_from_exp(
                value.clone(),
                constraints,
                env.borrow()
            );
            if let Some(ref name) = get_name(target.clone().as_ref().borrow().expr.borrow()) {
                unsafe {
                    (*target.as_ptr()).expr_id = get_expr_id(env, name.to_owned());
                }
                constraints.push(
                    TypeConstraint(ExpTy(target.clone()), ExpTy(value.clone()))
                );
            }
            constraints
        }
        StatementDesc::PointerAssignment { target, value } => {
            constraints = generate_type_constraints_from_exp(
                value.clone(),
                constraints,
                env.borrow()
            );
            constraints = generate_type_constraints_from_exp(
                target.clone(),
                constraints,
                env.borrow()
            );
            if let ExpressionDesc::Unop { op: UnaryOperator::Dereference, a } = target.as_ref().borrow().expr.borrow() {
                if let Some(name) = get_name(a.as_ref().borrow().expr.borrow()) {
                    unsafe {
                        (*a.as_ptr()).expr_id = get_expr_id(env, name.to_owned());
                    }
                    constraints.push(TypeConstraint(ExpTy(a.clone()), PointerTy(Box::new(ExpTy(value.clone())))));
                }
            } else {
                panic!("Expected dereference expression~");
            }
            constraints
        }
        StatementDesc::LocalDecl { names: _ } => {
            constraints
        },
        StatementDesc::Output { target } => {
            constraints = generate_type_constraints_from_exp(
                target.clone(),
                constraints,
                env
            );
            constraints.push(TypeConstraint(ExpTy(target.clone()), Int));
            constraints
        }
        StatementDesc::Return { value } => {
            constraints = generate_type_constraints_from_exp(
                value.clone(),
                constraints,
                env
            );
            constraints.push(TypeConstraint(ExpTy(value.clone()), Int));
            constraints
        }
        StatementDesc::While { ref mut cond, ref mut body } => {
            for stmt in body.iter_mut().rev() {
                constraints = generate_type_constraints_from_stmt(
                    stmt, constraints, env.borrow()
                );
            }
            constraints = generate_type_constraints_from_exp(
                cond.clone(), constraints, env
            );
            constraints.push(TypeConstraint(ExpTy(cond.clone()), Int));
            constraints
        }
        StatementDesc::Block { ref mut body } => {
            for stmt in body.iter_mut().rev() {
                constraints = generate_type_constraints_from_stmt(stmt, constraints, env.borrow());
            }
            constraints
        }
        StatementDesc::If { ref mut cond, ref mut then, ref mut orelse } => {
            for stmt in then.iter_mut().rev() {
                constraints = generate_type_constraints_from_stmt(
                    stmt, constraints, env.borrow()
                );
            }
            if let Some(ref mut or_else) = orelse {
                for stmt in or_else.iter_mut().rev() {
                    constraints = generate_type_constraints_from_stmt(
                        stmt, constraints, env.borrow()
                    );
                }
            }
            constraints = generate_type_constraints_from_exp(
                cond.clone(), constraints, env
            );
            constraints.push(TypeConstraint(ExpTy(cond.clone()), Int));
            constraints
        }
        StatementDesc::Err { value } => {
            constraints = generate_type_constraints_from_exp(
                value.clone(), constraints, env
            );
            constraints.push(TypeConstraint(ExpTy(value.clone()), Int));
            constraints
        }
    }
}


fn generate_type_constraints_from_fun(
    fun: &mut FunctionDecl,
    mut constraints: Vec<TypeConstraint>,
) -> Vec<TypeConstraint> {
    for stmt in fun.body.iter_mut().rev() {
        constraints = generate_type_constraints_from_stmt(
            stmt,
            constraints,
            fun.fun_env.borrow()
        );
    }
    constraints
}

pub fn generate_type_constraints_from_prog(
    prog: & mut Program,
) -> Vec<TypeConstraint> {
    let mut constraints = vec![];
    for fun in prog.fun_decl.iter_mut().rev() {
        constraints = generate_type_constraints_from_fun(
            fun,
            constraints
        );
    }
    constraints
}