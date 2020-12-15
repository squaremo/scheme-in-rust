use crate::value::{Value,Func,ValueRef};
use crate::parser::Expr;
use crate::env::{Env,EnvRef};
use std::ops::Deref;

type EvalResult = Result<ValueRef, String>;

pub fn eval(expr: &Expr, env: &EnvRef) -> EvalResult {
    match expr {
        Expr::Symbol(s) => {
            if let Some(val) = env.lookup(&s) {
                Ok(ValueRef::clone(&val))
            } else {
                Err(format!("failed to resolve symbol {}", s))
            }
        },
        Expr::List(head, tail) => eval_list(&head, &tail, env),
        Expr::Nil => Ok(ValueRef::new(Value::List(vec![]))),
        Expr::Int(i) => Ok(ValueRef::new(Value::Int(*i))), // <= TODO use quote_expr
    }
}

fn quote_expr(expr: &Expr) -> ValueRef {
    let v = match expr {
        Expr::Symbol(s) => Value::Symbol(s.to_string()),
        Expr::Int(i) => Value::Int(*i),
        Expr::List(h, t) => Value::List((vec![h.deref()].into_iter()).chain(t.into_iter()).map(quote_expr).collect()),
        Expr::Nil => Value::List(vec![]),
    };
    ValueRef::new(v)
}

fn eval_list(head: &Expr, tail: &[Expr], env: &EnvRef) -> EvalResult {
    // special forms
    if let Expr::Symbol(s) = head {
        // (quote <expr>)
        if s == "quote" {
            if tail.len() != 1 {
                return Err(String::from("invalid syntax (quote <expr>)"))
            }
            return Ok(quote_expr(&tail[0]))
        }
        // (lambda args body...)
        if s == "lambda" {
            if tail.len() < 2 {
                return Err(String::from("invalid syntax (lambda <list>|<symbol> <expr>...)"))
            }
            // at present the parser will only recognise `(lambda args
            // body...)` or `(lambda (arg...) body...)`, and not a
            // dotted form. The first of those requires the same
            // processing as a dotted form; I just don't have to match
            // the case of some regular args _and_ a rest.
            let args = &tail[0];
            let body = &tail[1..];
            let func = Func::Lambda(args.clone(), body.to_vec(), env.clone());
            return Ok(ValueRef::new(Value::Func(func)))
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

fn eval_args(args: &[Expr], env: &EnvRef) -> Result<Vec<ValueRef>, String> {
    args.into_iter().map(|term| { eval(term, env) }).collect()
}

// `(begin ...)` or a lambda body
fn eval_begin(expr: &[Expr], env: &EnvRef) -> EvalResult {
    expr.into_iter().map(|term| { eval(term, env) })
        .last()
        .map_or_else(|| { Err(String::from("no expressions in begin")) },
                     |v| { v })
}

fn apply_func(f: &Func, vals: Vec<ValueRef>, _env: &EnvRef) -> EvalResult {
    match f {
        Func::Prim(prim) => prim(vals).map(|val| { ValueRef::new(val) }),
        Func::Lambda(args, body, funcEnv) => {
            let mut newEnv = Env::new(funcEnv);
            match args {
                Expr::Symbol(s) => {
                    newEnv.set_env(s, ValueRef::new(Value::List(vals)));
                },
                Expr::Nil => (),
                Expr::List(name, names) => {
                    // check we have the same number of values as we
                    // do arguments
                    if names.len()+1 != vals.len() {
                        return Err(String::from("arity mismatch"))
                    }
                    match name.deref() {
                        Expr::Symbol(s) => newEnv.set_env(s, vals[0].clone()),
                        _ => return Err(String::from("non-symbol formal arg")),
                    }
                    for (name, val) in names.into_iter().zip(vals.into_iter().skip(1)) {
                        match name {
                            Expr::Symbol(s) => newEnv.set_env(s, val.clone()),
                            _ => return Err(String::from("non-symbol formal arg")),
                        }
                    }
                },
                _ => return Err(String::from("invalid closure"))
            };
            eval_begin(&body, &EnvRef::new(newEnv))
        }
    }
}
