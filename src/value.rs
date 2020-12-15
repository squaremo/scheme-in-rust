use std::rc::Rc;

use crate::parser::Expr;
use crate::env::{EnvRef};

#[derive(Debug, PartialEq, Clone)] // <-- these expected by the parser combinators
pub enum Value {
    Symbol(String),
    Int(i64),
    List(Vec<ValueRef>),
    Func(Func),
    True,
    False,
    // more kinds of value go here: string, float
}

// ValueRef is used for values resulting from evaluation (i.e., all of
// them). The expressions that get constructed when parsing can be
// moved, because I don't need them other than to evaluate them. But
// values that get created as part of evaluation might go into the
// environment, or be captured by closures, or become part of other
// values.
pub type ValueRef = Rc<Value>;

pub type NativeFunc = fn(Vec<ValueRef>) -> Result<Value, String>;

#[derive(Debug, PartialEq, Clone)]
pub enum Func {
    Prim(NativeFunc),
    Lambda(Expr, Vec<Expr>, EnvRef), // vars, body, environment
}

pub fn make_prim(f: NativeFunc) -> ValueRef {
    ValueRef::new(Value::Func(Func::Prim(f)))
}
