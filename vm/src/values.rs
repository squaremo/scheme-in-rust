use std::rc::Rc;
use std::fmt;
use std::ptr;

use crate::frame::FrameRef;

// Values. These are kept on the heap and reference-counted. (This
// means I'm allocating Nil or Undefined values, and keeping pointers
// to them. For the sake of simplicity, I'll stick with that, at least
// for now).

pub type ValueRef = Rc<Value>;

#[derive(Debug,PartialEq)]
pub struct Primitive0 {
    pub name: &'static str,
    pub func: fn() -> Result<ValueRef, String>,
}

pub struct Primitive1 {
    pub name: &'static str,
    pub func: fn(&ValueRef) -> Result<ValueRef, String>,
}

// Rust can't derive Debug (or PartialEq) for these; I don't know why
// but I haven't thought hard about it.
impl fmt::Debug for Primitive1 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Primitive1")
            .field("name", &self.name)
            .finish()
    }
}

impl PartialEq for Primitive1 {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name &&
            self.func as usize == other.func as usize
    }
}

pub struct Primitive2 {
    pub name: &'static str,
    pub func: fn(&ValueRef, &ValueRef) -> Result<ValueRef, String>,
}

impl fmt::Debug for Primitive2 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Primitive2")
            .field("name", &self.name)
            .finish()
    }
}

impl PartialEq for Primitive2 {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name &&
            self.func as usize == other.func as usize
    }
}


// Ints and lists, for now.
#[derive(Debug,PartialEq)]
pub enum Value {
    Symbol(String),
    Int(i64),
    Cons(ValueRef, ValueRef), // allow improper lists
    Nil,
    Boolean(bool),
    Closure(usize, FrameRef), // pc value and env
    Prim0(Primitive0),
    Prim1(Primitive1),
    Prim2(Primitive2),
    Undefined,
}

impl Default for Value {
    fn default() -> Self { Value::Undefined }
}
