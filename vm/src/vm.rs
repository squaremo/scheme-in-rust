use std::rc::Rc;
use std::ops::Deref;

use crate::instructions;
use crate::instructions::{Opcode};
use crate::values::{ValueRef,Value};

// Activation frames. These represent an environment for a
// closure. Because closures need their environment to be preserved,
// frames are kept on the heap; thus, the reference-counted FrameRef.

type FrameRef = Rc<Frame>;

#[derive(Clone)]
struct Frame {
    args: Vec<Option<ValueRef>>,
    next: Option<FrameRef>,
}

impl Frame {
    fn new(size: instructions::index) -> Frame {
        Frame{
            args: vec![None; size as usize],
            next: None,
        }
    }

    fn extend(next: FrameRef, size: instructions::index) -> Frame {
        Frame{
            args: vec![None; size as usize],
            next: Some(next),
        }
    }
}

// These things are put on the stack:
//
// - values
// - activation frames
//
// Both of these are allocated on the heap, so we get a
// reference-counted pointer to them.
#[derive(Clone)]
enum StackEntry {
    value(ValueRef),
    frame(FrameRef),
}

// The value register can have either a value (already allocated), or
// a frame under construction.
enum ValReg {
    value(ValueRef),
    new_frame(Frame),
}

pub struct VM<'a> {
    // globals are variables defined in the global scope.
    globals: Vec<ValueRef>,
    // constants are literals that appeared in the source.
    constants: Vec<ValueRef>,

    // The *val* register serves as the resting point for any value
    // that's just been calculated, or an activation frame under
    // construction.
    val: Option<ValReg>,
    // The *fun* register holds a closure value that is about to be
    // applied.
    fun: Option<ValueRef>,
    // The *arg1* and *arg2* registers hold values for use by
    // procedures of fixed arity 0, 1 or 2.
    arg1: Option<ValueRef>,
    arg2: Option<ValueRef>,
    // The *env* register holds the current activation frame.
    env: FrameRef,

    // The *pc* register holds the next instruction to execute. Here,
    // this is is a slice, so that the VM can just step through
    // instructions.
    pc: &'a[instructions::Opcode],

    // The stack holds values and activation frames of nested
    // invocations, waiting for control to return. Values are pushed
    // onto the stack while an activation frame is under construction.
    stack: Vec<StackEntry>,

    // halted means the program has finished
    halted: bool,
}

impl VM<'_> {

    pub fn new(globals: Vec<ValueRef>, constants: Vec<ValueRef>, program: &[instructions::Opcode]) -> VM {
        let top = Frame{
            args: vec![],
            next: None,
        };

        VM{
            globals: globals,
            constants: constants,
            val: None,
            fun: None,
            arg1: None,
            arg2: None,
            env: FrameRef::new(top),
            pc: program,
            stack: Vec::<StackEntry>::new(),
            halted: true,
        }
    }

    // This will allocate a new value on the heap; not very efficient,
    // but convenient, until I have immediate values.
    fn set_const(&mut self, v: Value) {
        self.val = Some(ValReg::value(ValueRef::new(v)));
    }

    fn set_val(&mut self, v: ValueRef) {
        self.val = Some(ValReg::value(v));
    }

    pub fn step(&mut self) {
        assert_ne!(self.pc.len(), 0, "no instruction to run at program counter");
        let instr = &self.pc[0];
        self.pc = &self.pc[1..];
        match instr {
            Opcode::SHALLOW_ARGUMENT_REF(i) => {
                assert!(self.env.args.len() >= (*i as usize), "SHALLOW_ARGUMENT_REF: index out of range for frame");
                if let Some(ref v) = &self.env.args[*i as usize] {
                    self.set_val(v.clone());
                } else {
                    assert!(false, "SHALLOW_ARGUMENT_REF: arg in frame is not initialised");
                }
            },
            // deep arg ref
            Opcode::GLOBAL_REF(i) => {
                self.set_val(self.globals[*i as usize].clone())
            },

            // ...

            Opcode::FINISH => {
                self.halted = true
            },

            // ...

            Opcode::EXTEND_ENV => {
                if let Some(ValReg::new_frame(ref f)) = self.val {
                    self.env = FrameRef::new(f.clone())
                } else {
                    assert!(false, "EXTEND_ENV: val register does not contain a frame");
                }
            },

            // ...

            Opcode::PUSH_VALUE => {
                if let Some(ValReg::value(ref v)) = self.val {
                    self.stack.push(StackEntry::value(v.clone()));
                } else {
                    assert!(false, "PUSH_VALUE: val register does not contain a value");
                }
            },
            Opcode::POP_ARG1 => {
                if let Some(StackEntry::value(ref v)) = self.stack.pop() {
                    self.arg1 = Some(v.clone());
                } else {
                    assert!(false, "POP_ARG1: top of stack did not contain value");
                }
            },

            // ...

            Opcode::POP_FRAME(i) => {
                if let Some(ValReg::new_frame(ref mut f)) = self.val {
                    assert!(f.args.len() > *i as usize, "POP_FRAME: index out of range for frame");
                    if let Some(StackEntry::value(ref v)) = self.stack.pop() {
                        let args = &mut f.args;
                        args[*i as usize] = Some(v.clone());
                    } else {
                        assert!(false, "POP_FRAME: pop from stack was a frame not a value");
                    }
                } else {
                    assert!(false, "POP_FRAME: val register does not contain a frame");
                }
            }

            // ...

            Opcode::ALLOCATE_FRAME(i) => {
                self.val = Some(ValReg::new_frame(
                    Frame::extend(self.env.clone(), *i),
                ));
            }

            // ...

            Opcode::INT_1 => {
                self.set_const(Value::Int(1));
            },

            // ...

            Opcode::CALL2_PLUS => {
                if let (Some(ValReg::value(ref v1)), Some(ref v2)) = (&self.val, &self.arg1) {
                    prim_plus(v1, v2).and_then(|v| Ok(self.set_val(v)))
                        .expect("CALL2_PLUS failed");
                } else {
                    assert!(false, "CALL2_PLUS: did not find values in val and arg1");
                }
            }

            _ => assert!(false, "{:#?} not yet implemented", instr)
        }
    }

    // Run until the program finishes, then return the content of the
    // *val* register if it's a value, or None otherwise.
    pub fn run_until_halt(&mut self) -> Option<ValueRef> {
        self.halted = false;
        while !self.halted {
            self.step()
        }
        self.val.as_ref().and_then(| val | {
            match val {
                ValReg::value(ref v) => Some(ValueRef::clone(v)),
                _ => None,
            }
        })
    }
}

fn prim_plus(v1: &ValueRef, v2: &ValueRef) -> Result<ValueRef, String> {
    if let (Value::Int(a), Value::Int(b)) = (v1.clone().deref(), v2.clone().deref()) {
        Ok(ValueRef::new(Value::Int(a+b)))
    } else {
        Err(format!("operands are not both integers"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // It's very useful to have a "compiler" on hand to generate some
    // of these. I use my code from
    // https://github.com/squaremo/lisp-in-small-pieces, with SISC, like so:
    //
    //     $ sisc
    //     #;> (load "../lisp-in-small-pieces/chapter7.4.ss")
    //     #;> (disassemble (meaning '((lambda (v)) 1) r.init #t))
    //
    // The latter two arguments to `meaning` are the lexical
    // environment, and whether it's a tail call. For the snippets
    // used in testing, these can be `r.init` (an empty lexical
    // environment) and `#t` (yes, in tail call position).

    #[test]
    fn test_global_ref() {
        let one = ValueRef::new(Value::Int(1));
        let globals = vec![one.clone()];
        let prog = vec![Opcode::GLOBAL_REF(0), Opcode::FINISH];
        let mut vm = VM::new(globals, vec![], &prog);
        assert_eq!(vm.run_until_halt(), Some(one));
    }

    #[test]
    fn test_env_shallow() {
        // this is something like
        //     ((lambda (v) v) 1)
        let prog = vec![
            Opcode::INT_1, // put `1` in *val*
            Opcode::PUSH_VALUE, // push *val* to stack
            Opcode::ALLOCATE_FRAME(1), // allocate a frame in *val*
            Opcode::POP_FRAME(0), // pop from the stack and put in `0`th slot of frame in *val*
            Opcode::EXTEND_ENV, // make the frame in *val* the current environment in *env*
            Opcode::SHALLOW_ARGUMENT_REF(0), // put the value from `0`th slot of current environment in *val*
            Opcode::FINISH, // halt.
        ];

        let mut vm = VM::new(vec![], vec![], &prog);
        if let Some(v) = vm.run_until_halt() {
        } else {
            assert!(false, "running VM did not result in a value");
        }
    }
}
