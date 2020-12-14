use crate::value::{Value,Func,ValueRef};
use crate::parser::Expr;
use crate::env::Env;
use std::ops::Deref;

type EvalResult = Result<ValueRef, String>;

pub fn eval(expr: &Expr, env: &Env) -> EvalResult {
    match expr {
        Expr::Int(i) => Ok(ValueRef::new(Value::Int(*i))),
        Expr::Symbol(s) => {
            if let Some(val) = env.lookup(&s) {
                Ok(ValueRef::clone(&val))
            } else {
                Err(format!("failed to resolve symbol {}", s))
            }
        }
        Expr::List(terms) => eval_apply(&terms, env),
    }
}

fn eval_apply(terms: &[Expr], env: &Env) -> EvalResult {
    if let Some((head, tail)) = terms.split_first() {
        // TODO eval the head (needs environments and special forms)
        match eval(head, env) {
            Ok(val) => {
                match val.deref() {
                    Value::Func(f) => {
                        let args = eval_args(tail, env)?;
                        apply_func(f, args, env)
                    },
                    _ => Err(String::from("head evalautes to non-function")),
                }
            },
            _ => Err(String::from("could not evaluate head of apply"))
        }
    } else {
        Err(String::from("empty form"))
    }
}

fn eval_args(args: &[Expr], env: &Env) -> Result<Vec<ValueRef>, String> {
    args.into_iter().map(|term| { eval(term, env) }).collect()
}

fn apply_func(f: &Func, args: Vec<ValueRef>, _env: &Env) -> EvalResult {
    match f {
        Func::Prim(prim) => prim(args).map(|val| { ValueRef::new(val) }),
    }
}
