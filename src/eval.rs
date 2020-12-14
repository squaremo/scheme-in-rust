use crate::value::Value;

//const LAMBDA: Value = Value::Symbol(String::from("lambda"));

const PLUS: &str = "+";

pub fn eval(expr: Value) -> Result<Value, &'static str> {
    match expr {
        Value::Int(_) => Ok(expr),
        Value::Symbol(_) => Err("environments not implemented yet"),
        Value::List(terms) => eval_apply(terms),
    }
}

fn eval_apply(terms: Vec<Value>) -> Result<Value, &'static str> {
    if let Some((head, tail)) = terms.split_first() {
        // TODO eval the head (needs environments and special forms)
        match head {
            Value::Symbol(op) =>
                if op == PLUS {
                    prim_plus(tail.to_vec())
                } else {
                    Err("unknown function")
                },
            _ => Err("cannot evaluate head of apply")
        }
    } else {
        Err("empty form")
    }
}

fn eval_args(args: Vec<Value>) -> Result<Vec<Value>, &'static str> {
    args.into_iter().map(eval).collect()
}

fn prim_plus(terms: Vec<Value>) -> Result<Value, &'static str> {
    let mut result: i64 = 0;
    let args = eval_args(terms)?;
    for arg in args {
        match arg {
            Value::Int(i) => result += i,
            _ => return Err("attempt to add non-number")
        };
    }
    Ok(Value::Int(result))
}
