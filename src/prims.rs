use crate::value::{ValueRef,Value,make_prim};
use crate::env::Env;

fn check_int(v: &Value) -> Result<i64, String> {
    match v {
        Value::Int(i) => Ok(*i),
        _ => Err(String::from("not an integer"))
    }
}

fn arith_group(args: &[ValueRef], op: &dyn Fn(i64, i64) -> i64, unit: i64) -> Result<Value, String> {
    args.into_iter().try_fold(unit, | acc, v | {
        let i = check_int(v)?;
        Ok(op(acc, i))
    }).map(Value::Int)
}

fn plus(args: Vec<ValueRef>) -> Result<Value, String> {
    arith_group(&args, &|a, b| a + b, 0)
}

fn mult(args: Vec<ValueRef>) -> Result<Value, String> {
    arith_group(&args, &|a, b| a * b, 1)
}

fn comp(args: &[ValueRef], compare: &dyn Fn(i64, i64) -> bool) -> Result<Value, String> {
    if args.len() < 2 {
        return Err(String::from("incorrect number of arguments to procedure"))
    }
    let mut a = check_int(&*args[0])?;
    for v in args.into_iter().skip(1) {
        let b = check_int(v)?;
        if !compare(a, b) {
            return Ok(Value::False)
        }
        a = b;
    }
    return Ok(Value::True)
}

pub fn lt(args: Vec<ValueRef>) -> Result<Value, String> {
    comp(&args, &|a, b| a < b)
}

pub fn into_env(env: &mut Env) {
    env.set_env("+", make_prim(plus));
    env.set_env("*", make_prim(mult));
    env.set_env("<", make_prim(lt));
}
