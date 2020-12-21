mod instructions;
mod vm;
mod values;

use crate::instructions::Opcode;
use crate::values::{Value,ValueRef};
use crate::vm::VM;

fn main() {
    let globals = vec![ValueRef::new(Value::Int(4))];
    let prog = vec![Opcode::GLOBAL_REF(0), Opcode::FINISH];
    let mut vm = VM::new(globals, vec![], &prog);
    vm.step();
}
