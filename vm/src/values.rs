use std::rc::Rc;

// Values. These are kept on the heap and reference-counted. (This
// means I'm allocating Nil or Undefined values, and keeping pointers
// to them. For the sake of simplicity, I'll stick with that, at least
// for now).

pub type ValueRef = Rc<Value>;

// Ints and lists, for now.
#[derive(Debug,PartialEq)]
pub enum Value {
    Int(i64),
    Cons(ValueRef, ValueRef), // allow improper lists
    Nil,
    Undefined,
}

impl Default for Value {
    fn default() -> Self { Value::Undefined }
}
