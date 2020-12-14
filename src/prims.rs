use crate::value::{ValueRef,Value,make_prim};
use crate::env::Env;

pub fn plus(args: Vec<ValueRef>) -> Result<Value, String> {
    let result: Result<i64, String> = args.into_iter().map(| arg | {
        match *arg {
            Value::Int(i) => Ok(i),
            _ => Err(String::from("attempt to add non-number"))
        }
    }).sum();
    result.map(|i| Value::Int(i))
}

pub fn mult(args: Vec<ValueRef>) -> Result<Value, String> {
    let result: Result<i64, String> = args.into_iter().try_fold(1i64, | acc, arg | {
        match *arg {
            Value::Int(i) => Ok(i * acc),
            _ => Err(String::from("attempt to multiply non-number"))
        }
    });
    result.map(Value::Int)
}

pub fn into_env(env: &mut Env) {
    env.set_env("+", make_prim(plus));
    env.set_env("*", make_prim(mult));
}
