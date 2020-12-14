use crate::value::{ValueRef,Value,make_prim};
use crate::env::Env;

pub fn plus(args: Vec<ValueRef>) -> Result<Value, String> {
    let mut result: i64 = 0;
    for arg in args {
        match *arg {
            Value::Int(i) => result += i,
            _ => return Err(String::from("attempt to add non-number"))
        };
    }
    Ok(Value::Int(result))
}

pub fn into_env(env: &mut Env) {
    env.set_env("+", make_prim(plus));
}
