use crate::constraint::{TypeVar, TypeConstraint};
use std::borrow::Borrow;
use parser::ast::ExpressionDesc;

fn subst_type(
    type_var: TypeVar,
    type_var_replace: TypeVar,
    type_var_expr: TypeVar
) -> TypeVar {
    if type_var == type_var_expr {  return type_var_replace; }
    return match type_var_expr.clone() {
        TypeVar::Alpha(_) | TypeVar::Int => type_var_expr,
        TypeVar::ExpTy(expr) => {
            match expr.as_ref().borrow().expr.borrow() {
                ExpressionDesc::Identifier {
                    name,
                    id: _
                } => {
                    match type_var {
                        TypeVar::ExpTy(tmp_expr) => {
                            match tmp_expr.as_ref().borrow().expr.borrow() {
                                ExpressionDesc::Identifier {
                                    name: tmp_name,
                                    id: _tmp_id
                                } => {
                                    if name == tmp_name  {  type_var_replace }
                                    else { type_var_expr }
                                }
                                _ => type_var_expr,
                            }
                        }
                        _ => type_var_expr,
                    }
                }
                _ => type_var_expr,
            }
        }
        TypeVar::PointerTy(expr) => {
            let out_type = subst_type(
                type_var,
                type_var_replace,
                *expr.clone()
            );
            TypeVar::PointerTy(Box::new(out_type))
        }
        TypeVar::FunctionTy(type_vars, type_var_ret) => {
            let paras = subst_types(
                type_var.clone(),
                type_var_replace.clone(),
                type_vars
            );
            let ret = subst_type(
                type_var.clone(),
                type_var_replace.clone(),
                *type_var_ret
            );
            TypeVar::FunctionTy(paras, Box::new(ret))
        }
    };
}

fn subst_types(
    type_var: TypeVar,
    type_var_replace: TypeVar,
    type_var_exprs: Vec<Box<TypeVar>>
) -> Vec<Box<TypeVar>> {
    let mut result = Vec::new();
    for type_var_expr in type_var_exprs {
        let tmp = subst_type(
            type_var.clone(),
            type_var_replace.clone(),
            *type_var_expr
        );
        result.push(Box::new(tmp));
    }
    result
}

fn subst_type_constraints(
    type_var: TypeVar,
    type_var_replace: TypeVar,
    type_constraints: Vec<TypeConstraint>
) -> Vec<TypeConstraint> {
    let mut result = Vec::new();
    for type_con in type_constraints {
        result.push(
            TypeConstraint (
                subst_type(
                    type_var.clone(),
                    type_var_replace.clone(),
                    type_con.0
                ),
                subst_type(
                    type_var.clone(),
                    type_var_replace.clone(),
                    type_con.1
                )
            )
        );
    }
    result
}

fn add_if_not_trivial(
    type_constraint: TypeConstraint,
    mut type_constraints: Vec<TypeConstraint>
) -> Vec<TypeConstraint> {
    return match type_constraint.borrow() {
        TypeConstraint(TypeVar::Int, TypeVar::Int) => type_constraints,
        TypeConstraint(TypeVar::Alpha(_), _) => type_constraints,
        TypeConstraint(_, TypeVar::Alpha(_)) => type_constraints,
        TypeConstraint(TypeVar::ExpTy(expr), TypeVar::Int) => {
            let expr_desc = unsafe { (*expr.as_ptr()).borrow().expr.borrow() };
            match expr_desc {
                ExpressionDesc::Input => type_constraints,
                ExpressionDesc::Number { value: _ } => type_constraints,
                _ => {
                    type_constraints.push(
                        type_constraint
                    );
                    type_constraints
                }
            }
        }
        TypeConstraint(TypeVar::Int, TypeVar::ExpTy(expr)) => {
            let expr_desc = unsafe { (*expr.as_ptr()).borrow().expr.borrow() };
            match expr_desc {
                ExpressionDesc::Number { value: _ } => type_constraints,
                _ => {
                    type_constraints.push(
                        type_constraint
                    );
                    type_constraints
                }
            }
        }
        _ => {
            type_constraints.push(
                type_constraint
            );
            type_constraints
        }
    };
}

fn unify_one(
    type_constraint: TypeConstraint,
    mut type_constraints: Vec<TypeConstraint>,
    substitutions: Vec<TypeConstraint>
) -> (Vec<TypeConstraint>, Vec<TypeConstraint>) {
    if type_constraint.0 == type_constraint.1 { return (type_constraints, substitutions); }
    return match type_constraint.borrow() {
        TypeConstraint(TypeVar::ExpTy(_expr), TypeVar::Alpha(_)) =>{
            let type_cons = subst_type_constraints(
                type_constraint.1.clone(),
                type_constraint.0.clone(),
                type_constraints
            );
            let subst =  subst_type_constraints(
                type_constraint.1.clone(),
                type_constraint.0.clone(),
                substitutions
            );
            let substs = add_if_not_trivial(type_constraint, subst);
            (type_cons, substs)
        }
        TypeConstraint(TypeVar::ExpTy(_expr), _) |
        TypeConstraint(TypeVar::Alpha(_), TypeVar::ExpTy(_expr)) =>{
            let type_cons = subst_type_constraints(
                type_constraint.0.clone(),
                type_constraint.1.clone(),
                type_constraints
            );
            let subst =  subst_type_constraints(
                type_constraint.0.clone(),
                type_constraint.1.clone(),
                substitutions
            );
            let substs = add_if_not_trivial(type_constraint, subst);
            (type_cons, substs)
        }
        TypeConstraint(_, TypeVar::ExpTy(_expr)) => {
            let type_cons = subst_type_constraints(
                type_constraint.1.clone(),
                type_constraint.0.clone(),
                type_constraints
            );
            let subst =  subst_type_constraints(
                type_constraint.1.clone(),
                type_constraint.0.clone(),
                substitutions
            );
            let substs = add_if_not_trivial(type_constraint, subst);
            (type_cons, substs)
        }
        TypeConstraint(TypeVar::PointerTy(expr1), TypeVar::PointerTy(expr2)) => {
            unify_one(
                TypeConstraint((**expr1).clone(), (**expr2).clone()),
                type_constraints,
                substitutions
            )
        }
        TypeConstraint(
            TypeVar::FunctionTy(args_expr1, ret_expr1),
            TypeVar::FunctionTy(args_expr2, ret_expr2)
        ) => {
            let add_args_constraints = |
                type_var1: TypeVar,
                type_var2: TypeVar,
                mut type_constraints: Vec<TypeConstraint> | -> Vec<TypeConstraint> {
                    type_constraints.push(
                        TypeConstraint(
                            type_var1,
                            type_var2
                        )
                    );
                    type_constraints
            };
            if args_expr1.len() != args_expr2.len() {  panic!("Err~"); }
            let _ = args_expr1
                .iter()
                .zip(args_expr2.iter())
                .map(|(type1, type2)| {
                    type_constraints = add_args_constraints((**type1).clone(), (**type2).clone(), type_constraints.clone());
                });
            type_constraints.push(
                TypeConstraint(
                    (**ret_expr1).clone(),
                    (**ret_expr2).clone()
                )
            );
            (type_constraints, substitutions)
        }
        _ => {
            panic!("The program is not typable. Cannot unify {:?}", type_constraint)
        }
    };
}

pub fn solve_type_constraints(
    mut type_constraints: Vec<TypeConstraint>,
) -> Vec<TypeConstraint> {
    let mut substitutions = Vec::new();
    while type_constraints.len() != 0 {
        let type_con = type_constraints.get(0).unwrap().clone();
        let (type_cons, substs) = unify_one(
            type_con,
            type_constraints[1..type_constraints.len()].to_owned(),
            substitutions.clone()
        );
        type_constraints = type_cons;
        substitutions = substs;
    }
    return substitutions;
}