use crate::value::{Value,Func,ValueRef};
use crate::parser::Expr;
use crate::env::Env;
use std::ops::Deref;

type EvalResult = Result<ValueRef, String>;

pub fn eval(expr: &Expr, env: &Env) -> EvalResult {
    match expr {
        Expr::Symbol(s) => {
            if let Some(val) = env.lookup(&s) {
                Ok(ValueRef::clone(&val))
            } else {
                Err(format!("failed to resolve symbol {}", s))
            }
        }
        Expr::List(head, tail) => eval_apply(&head, &tail, env),
        Expr::Int(i) => Ok(ValueRef::new(Value::Int(*i))), // <= TODO use quote_expr
    }
}

fn quote_expr(expr: &Expr) -> ValueRef {
    let v = match expr {
        Expr::Symbol(s) => Value::Symbol(s.to_string()),
        Expr::Int(i) => Value::Int(*i),
        Expr::List(h, t) => Value::List((vec![h.deref()].into_iter()).chain(t.into_iter()).map(quote_expr).collect()),
    };
    ValueRef::new(v)
}

fn eval_apply(head: &Expr, tail: &[Expr], env: &Env) -> EvalResult {
    // special forms
    if let Expr::Symbol(s) = head {
        if s == "quote" {
            if tail.len() != 1 {
                return Err(String::from("invalid syntax (quote)"))
            }
            return Ok(quote_expr(&tail[0]))
        }
    }
    // regular application. Other forms, like left-left-lambda,
    // could be detected here.
    match eval(head, env) {
        Ok(val) => {
            match val.deref() {
                Value::Func(f) => {
                    let args = eval_args(tail, env)?;
                    apply_func(f, args, env)
                },
                _ => Err(String::from("head evaluates to non-function")),
            }
        },
        _ => Err(String::from("could not evaluate head of apply"))
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
