use std::rc::Rc;
use std::fmt;
use std::ptr;

use crate::frame::FrameRef;

// Values. These are kept on the heap and reference-counted. (This
// means I'm allocating Nil or Undefined values, and keeping pointers
// to them. For the sake of simplicity, I'll stick with that, at least
// for now).

pub type ValueRef = Rc<Value>;

// NativeProc are native procedures that get access to the registers
// of the VM. This is sometimes necessary for supporting input,
// output, or varargs; but mainly, it's just used to trampoline
// primitives into the correct calling convention, e.g., one of
// invoke0, invoke1, invoke2.

use crate::vm;

pub struct NativeProc {
    pub name: &'static str,
    pub func: fn(&mut vm::VM),
}

impl fmt::Debug for NativeProc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("NativeProc")
            .field("name", &self.name)
            .finish()
    }
}

impl PartialEq for NativeProc {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name &&
            self.func as usize == other.func as usize
    }
}

#[derive(Debug,PartialEq)]
pub enum Value {
    Symbol(String),
    Int(i64),
    String(String),
    Cons(ValueRef, ValueRef), // allow improper lists
    Nil,
    Boolean(bool),
    Closure(usize, FrameRef), // pc value and env
    Native(NativeProc),
    Undefined,
}

impl Default for Value {
    fn default() -> Self { Value::Undefined }
}
