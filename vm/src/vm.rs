use std::rc::Rc;
use std::cell::RefCell;
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
    args: Vec<RefCell<Option<ValueRef>>>,
    next: Option<FrameRef>,
}

impl Frame {
    fn new(size: instructions::index) -> Frame {
        Frame{
            args: vec![RefCell::new(None); size as usize],
            next: None,
        }
    }

    fn extend(next: FrameRef, size: instructions::index) -> Frame {
        Frame{
            args: vec![RefCell::new(None); size as usize],
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
    globals: Vec<Option<ValueRef>>,
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

    pub fn new(constants: Vec<ValueRef>, program: &[instructions::Opcode]) -> VM {
        let top = Frame{
            args: vec![],
            next: None,
        };

        VM{
            globals: vec![None; 1000],
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
        match instr {
            Opcode::SHALLOW_ARGUMENT_REF(i) => {
                assert!(self.env.args.len() >= (*i as usize), "SHALLOW_ARGUMENT_REF: index out of range for frame");
                 // this needs to be cloned so that self is not borrowed
                let arg = self.env.args[*i as usize].clone();
                // this needs to be longer lived than the match expression
                let arg1 = arg.borrow();
                if let Some(ref v) = *arg1 {
                    self.set_val(v.clone());
                } else {
                    assert!(false, "SHALLOW_ARGUMENT_REF: arg in frame is not initialised");
                }
            },
            Opcode::DEEP_ARGUMENT_REF(rank, index) => {
                let mut left = *rank;
                let mut env = &self.env;
                while left > 0 {
                    left -= 1;
                    if let Some(ref next) = env.next {
                        env = next;
                    } else {
                        assert!(false, "DEEP_ARGUMENT_REF: rank overflows environment")
                    }
                }
                assert!(env.args.len() > *index as usize, "DEEP_ARGUMENT_REF: index out of range for frame");
                let arg = env.args[*index as usize].clone();
                let arg1 = arg.borrow();
                if let Some(ref v) = *arg1 {
                    self.set_val(v.clone());
                } else {
                    assert!(false, "DEEP_ARGUMENT_REF: arg in frame is uninitialised")
                }
            },
            Opcode::GLOBAL_REF(i) => {
                // NB does not check!! (*it'll panic in unwrap ...)
                self.set_val(self.globals[*i as usize].as_ref().unwrap().clone())
            },
            Opcode::CHECKED_GLOBAL_REF(index) => {
                if let Some(ref v) = self.globals[*index as usize] {
                    // // patch the instruction with one that doesn't do
                    // // this check (*it does really).
                    // self.pc[0] = Opcode::GLOBAL_REF(*index);
                    self.set_val(v.clone());
                } else {
                    assert!(false, "CHECKED_GLOBAL_REF: global is uninitialised");
                }
            },
            Opcode::CONSTANT(index) => {
                assert!(self.constants.len() > *index as usize, "CONSTANT index out of bounds");
                self.set_val(self.constants[*index as usize].clone());
            },

            // ... all the predefined values, and generic predefined

            Opcode::FINISH => {
                self.halted = true
            },
            Opcode::SET_SHALLOW_ARGUMENT(index) => {
                assert!(self.env.args.len() > *index as usize, "SET_SHALLOW_ARGUMENT: index out of bounds");
                if let Some(ValReg::value(ref v)) = self.val {
                    *self.env.args[*index as usize].borrow_mut() = Some(v.clone());
                } else {
                    assert!(false, "SET_SHALLOW_ARGUMENT: value register does not have a value");
                }
            },
            Opcode::SET_DEEP_ARGUMENT(rank, index) => {
                let mut left = *rank;
                let mut env = &self.env;
                while left > 0 {
                    left -= 1;
                    if let Some(ref next) = env.next {
                        env = next;
                    } else {
                        assert!(false, "SET_DEEP_ARGUMENT: rank overflows environment")
                    }
                }
                assert!(env.args.len() > *index as usize, "SET_DEEP_ARGUMENT: index out of range for frame");
                if let Some(ValReg::value(ref v)) = self.val {
                    *env.args[*index as usize].borrow_mut() = Some(v.clone());
                } else {
                    assert!(false, "SET_DEEP_ARGUMENT: value register does not have a value");
                }
            }

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
                        *args[*i as usize].borrow_mut() = Some(v.clone());
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
            Opcode::INT_NEG1 => {
                self.set_const(Value::Int(-1));
            },
            Opcode::INT_0 => {
                self.set_const(Value::Int(0));
            },
            Opcode::INT_1 => {
                self.set_const(Value::Int(1));
            },
            Opcode::INT_2 => {
                self.set_const(Value::Int(2));
            },
            Opcode::INT_3 => {
                self.set_const(Value::Int(3));
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
        self.pc = &self.pc[1..];
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
        let globals = vec![Some(one.clone())];
        let prog = vec![Opcode::GLOBAL_REF(0), Opcode::FINISH];
        let mut vm = VM::new(vec![], &prog);
        vm.globals = globals;
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

        let mut vm = VM::new(vec![], &prog);
        if let Some(v) = vm.run_until_halt() {
        } else {
            assert!(false, "running VM did not result in a value");
        }
    }

    #[test]
    fn test_call2_plus() {
        // (+ 2 3)
        let prog = vec![
            Opcode::INT_2,
            Opcode::PUSH_VALUE,
            Opcode::INT_3,
            Opcode::POP_ARG1,
            Opcode::CALL2_PLUS,
            Opcode::FINISH,
        ];
        let mut vm = VM::new(vec![], &prog);
        if let Some(ref v) = vm.run_until_halt() {
            assert_eq!(*v.deref(), Value::Int(5))
        } else {
            assert!(false, "running program did not result in a (correct) value");
        }
    }

    #[test]
    fn test_shallow_set() {
        // this is artificial and isn't the compilation of a program
        let prog = vec![
            // first there has to be an env with something in it
            Opcode::INT_3,
            Opcode::PUSH_VALUE,
            Opcode::ALLOCATE_FRAME(1),
            Opcode::POP_FRAME(0),
            Opcode::EXTEND_ENV,
            // now there's an environment in *env* with a slot that
            // can be assigned to
            Opcode::INT_NEG1,
            Opcode::SET_SHALLOW_ARGUMENT(0),
            Opcode::INT_0,
            Opcode::SHALLOW_ARGUMENT_REF(0),
            Opcode::FINISH,
        ];
        let mut vm = VM::new(vec![], &prog);
        if let Some(ref v) = vm.run_until_halt() {
            assert_eq!(*v.deref(), Value::Int(-1))
        } else {
            assert!(false, "running program did not result in a (correct) value");
        }
    }

    #[test]
    fn test_deep_env() {
        // this is artificial and isn't the compilation of a program
        let prog = vec![
            Opcode::INT_2,
            Opcode::PUSH_VALUE,
            Opcode::ALLOCATE_FRAME(1),
            Opcode::POP_FRAME(0),
            Opcode::EXTEND_ENV,

            Opcode::INT_1,
            Opcode::PUSH_VALUE,
            Opcode::ALLOCATE_FRAME(1),
            Opcode::POP_FRAME(0),
            Opcode::EXTEND_ENV,

            Opcode::INT_0,
            Opcode::PUSH_VALUE,
            Opcode::ALLOCATE_FRAME(1),
            Opcode::POP_FRAME(0),
            Opcode::EXTEND_ENV,

            Opcode::INT_3,
            Opcode::SET_DEEP_ARGUMENT(2, 0), // originally INT_2

            // Add the numbers from (1, 0) and (2, 0)
            Opcode::DEEP_ARGUMENT_REF(1, 0), // = INT_1 -> *val*
            Opcode::PUSH_VALUE, // INT_1 -> stack
            Opcode::DEEP_ARGUMENT_REF(2, 0), // INT_3 -> *val*
            Opcode::POP_ARG1, // INT_1 -> *arg1*
            Opcode::CALL2_PLUS, // *val* + *arg1* -> *val*
            Opcode::FINISH,
        ];
        let mut vm = VM::new(vec![], &prog);
        if let Some(ref v) = vm.run_until_halt() {
            assert_eq!(*v.deref(), Value::Int(4))
        } else {
            assert!(false, "running program did not result in a (correct) value");
        }
    }
}
