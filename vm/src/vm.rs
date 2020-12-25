use std::ops::Deref;

use crate::instructions;
use crate::instructions::{Opcode};
use crate::values::{ValueRef,Value};
use crate::frame::{Frame,FrameRef};
use crate::read;

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
    return_address(usize),
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

    code: &'a[instructions::Opcode],
    // The *pc* register holds the next instruction to execute. This
    // is an index into `code`.
    pc: usize,

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
            code: program,
            pc: 0,
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

    fn call0(&mut self, prim: fn() -> Result<ValueRef, String>) {
        match prim() {
            Ok(v) => self.set_val(v),
            Err(e) => assert!(false, e),
        }
    }

    fn call1(&mut self, prim: fn(&ValueRef) -> Result<ValueRef, String>) {
        if let Some(ValReg::value(ref v1)) = &self.val {
            match prim(v1) {
                Ok(v) => self.set_val(v),
                Err(e) => assert!(false, e),
            }
        } else {
            assert!(false, "CALL1: val register does not contain value");
        }
    }

    fn call2(&mut self, prim: fn(&ValueRef, &ValueRef) -> Result<ValueRef, String>) {
        if let (Some(ValReg::value(ref v1)), Some(ref v2)) = (&self.val, &self.arg1) {
            match prim(v1, v2) {
                Ok(v) => self.set_val(v),
                Err(e) => assert!(false, e),
            }
        } else {
            assert!(false, "CALL1: val register does not contain value");
        }
    }

    pub fn step(&mut self) {
        assert!(self.pc < self.code.len(), "no instruction to run at program counter");
        let instr = &self.code[self.pc];
        self.pc += 1;
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
                    // once checked, this instruction can be patched
                    // to use GLOBAL_REF instead, which doesn't do the
                    // checking (it does really, by using `unwrap` --
                    // sshhh). HOWEVER: while I'm using a slice for
                    // *pc* I can't patch the instruction being run.
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
            },
            Opcode::SET_GLOBAL(index) => {
                if let Some(ValReg::value(ref v)) = self.val {
                    self.globals[*index as usize] = Some(v.clone());
                } else {
                    assert!(false, "SET_GLOBAL: val register does not contain a value");
                }
            },
            Opcode::GOTO(offset) => {
                assert!(self.code.len() > self.pc + *offset as usize, "GOTO: offset would jump past end of code");
                self.pc += *offset as usize;
            },
            Opcode::JUMP(offset) => {
                assert!(self.code.len() > self.pc + *offset as usize, "JUMP: offset would jump past end of code");
                if let Some(ValReg::value(ref v)) = self.val {
                    if let Value::Boolean(false) = v.deref() {
                        self.pc += *offset as usize;
                    }
                } else {
                    assert!(false, "JUMP: val register does not contain a value");
                }
            },
            Opcode::EXTEND_ENV => {
                if let Some(ValReg::new_frame(ref f)) = self.val {
                    self.env = FrameRef::new(f.clone())
                } else {
                    assert!(false, "EXTEND_ENV: val register does not contain a frame");
                }
            },
            Opcode::UNLINK_ENV => {
                if let Some(next) = &self.env.next {
                    self.env = next.clone();
                }
            },
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
            Opcode::POP_2ARG => {
                if let Some(StackEntry::value(ref v)) = self.stack.pop() {
                    self.arg1 = Some(v.clone());
                } else {
                    assert!(false, "POP_2ARG: top of stack did not contain value");
                }
                if let Some(StackEntry::value(ref v)) = self.stack.pop() {
                    self.arg2 = Some(v.clone());
                } else {
                    assert!(false, "POP_2ARG: top of stack did not contain value");
                }
            },
            Opcode::PRESERVE_ENV => {
                self.stack.push(StackEntry::frame(self.env.clone()));
            },
            Opcode::RESTORE_ENV => {
                if let Some(StackEntry::frame(env)) = self.stack.pop() {
                    self.env = env.clone();
                } else {
                    assert!(false, "RESTORE_ENV: top of stack did not contain an environment")
                }
            },
            Opcode::POP_FUNCTION => {
                if let Some(StackEntry::value(v)) = self.stack.pop() {
                    self.fun = Some(v.clone());
                } else {
                    assert!(false, "POP_FUNCTION: top of stack did not contain a value")
                }
            },
            Opcode::CREATE_CLOSURE(offset) => {
                let c = Value::Closure(self.pc + *offset as usize, self.env.clone());
                self.val = Some(ValReg::value(ValueRef::new(c)));
            },
            Opcode::RETURN => {
                if let Some(StackEntry::return_address(pc)) = self.stack.pop() {
                    self.pc = pc;
                } else {
                    assert!(false, "POP_FUNCTION: top of stack did not contain a return address");
                }
            },
            Opcode::PACK_FRAME(index) => {
                // in general, for a function application you don't
                // know the arity of the function -- it could be just
                // a value you found. So you have to build an
                // activation record for all the arguments provided,
                // then let the function itself (if it takes varargs)
                // cons the extra args into a list.
                if let Some(ValReg::new_frame(ref f)) = self.val {
                    let mut i = f.args.len() - 1;
                    let mut varargs = ValueRef::new(Value::Nil);
                    while i > *index as usize {
                        i -= 1;
                        let arg = f.args[i].borrow();
                        if let Some(ref v) = *arg {
                            varargs = ValueRef::new(Value::Cons(v.clone(), varargs.clone()));
                        } else {
                            assert!(false, "PACK_FRAME: argument is undefined")
                        }
                    }
                    *f.args[*index as usize].borrow_mut() = Some(varargs);
                } else {
                    assert!(false, "PACK_FRAME: val register does not contain a frame");
                }
            },
            Opcode::FUNCTION_INVOKE => { // non-tailcall
                if let Some(ref funp) = self.fun {
                    match funp.deref() {
                        Value::Closure(pc, env) => {
                            self.stack.push(StackEntry::return_address(self.pc));
                            // the function will use the frame in *val*, just
                            // leave it there.
                            self.pc = *pc;
                            self.env = env.clone();
                        },
                        _ => {
                            assert!(false, "FUNCTION_INVOKE: value in fun register is not a function");
                        }
                    }
                } else {
                    assert!(false, "FUNCTION_INVOKE: no value in fun register");
                }
            },
            Opcode::FUNCTION_GOTO => { // tailcall
                if let Some(ref funp) = self.fun {
                    match funp.deref() {
                        Value::Closure(pc, env) => {
                            // the function will use the frame in *val*, just
                            // leave it there.
                            self.pc = *pc;
                            self.env = env.clone();
                        },
                        _ => {
                            assert!(false, "FUNCTION_GOTO: value in fun register is not a function");
                        }
                    }
                } else {
                    assert!(false, "FUNCTION_GOTO: no value in fun register");
                }
            },
            Opcode::POP_CONS_FRAME(index) => {
                if let Some(StackEntry::value(ref v)) = self.stack.pop() {
                    if let Some(ValReg::new_frame(ref f)) = self.val {
                        let mut arg = f.args[*index as usize].borrow_mut();
                        if let Some(ref vargs) = *arg {
                            // it's supposed to be a list; whatever it is, cons to it
                            let cons = Value::Cons(v.clone(), vargs.clone());
                            *arg = Some(ValueRef::new(cons));
                        } else {
                            assert!(false, "POP_CONS_ARGUMENT: argument is not a list");
                        }
                    } else {
                        assert!(false, "POP_CONS_ARGUMENT: val register does not contain a frame");
                    }
                } else {
                    assert!(false, "POP_CONS_ARGUMENT: top of stack is not a value");
                }
            },
            Opcode::ALLOCATE_FRAME(i) => {
                self.val = Some(ValReg::new_frame(
                    Frame::extend(self.env.clone(), *i),
                ));
            },
            Opcode::ALLOCATE_DOTTED_FRAME(i) => {
                let newf = Frame::extend(self.env.clone(), *i);
                {
                    let mut restarg = newf.args[*i as usize - 1].borrow_mut();
                    *restarg = Some(ValueRef::new(Value::Nil));
                }
                self.val = Some(ValReg::new_frame(newf));
            },
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
            },
            Opcode::ARITY_EQUAL(index) => {
                if let Some(ValReg::new_frame(ref f)) = self.val {
                    // should really be an error result here (and elsewhere)
                    assert_eq!(f.args.len(), *index as usize, "ARITY_EQUAL: number of arguments does not match arity of function");
                } else {
                    assert!(false, "ARITY_EQUAL: value in val register is not a frame");
                }
            },
            Opcode::ARITY_GE(index) => {
                if let Some(ValReg::new_frame(ref f)) = self.val {
                    // should really be an error result here (and elsewhere)
                    assert!(f.args.len() >= *index as usize, "ARITY_GE: number of arguments too few for function");
                } else {
                    assert!(false, "ARITY_GE: value in val register is not a frame");
                }
            },
            Opcode::INT(i) => {
                self.set_const(Value::Int(*i as i64));
            },
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
            Opcode::CALL0_newline => {
                self.call0(prim_newline);
            },
            // TODO: read
            Opcode::CALL1_car => {
                self.call1(prim_car);
            },
            Opcode::CALL1_cdr => {
                self.call1(prim_cdr);
            },
            Opcode::CALL1_pair_p => {
                self.call1(prim_pair_p);
            },
            Opcode::CALL1_symbol_p => {
                self.call1(prim_symbol_p);
            },
            Opcode::CALL2_cons => {
                self.call2(prim_cons);
            },
            Opcode::CALL2_eq_p => {
                self.call2(prim_eq_p);
            },
            Opcode::CALL2_PLUS => {
                self.call2(prim_plus);
            },
            Opcode::CALL2_MINUS => {
                self.call2(prim_minus);
            },
            Opcode::CALL2_EQUAL => {
                self.call2(prim_equal);
            },
            Opcode::CALL2_LT => {
                self.call2(prim_lt);
            },
            Opcode::CALL2_GT => {
                self.call2(prim_gt);
            },
            Opcode::CALL2_LTE => {
                self.call2(prim_lte);
            },
            Opcode::CALL2_GTE => {
                self.call2(prim_gte);
            },
            Opcode::CALL2_TIMES => {
                self.call2(prim_times);
            },
            Opcode::CALL2_DIVIDE => {
                self.call2(prim_divide);
            },

            _ => assert!(false, "{:#?} not yet implemented", instr)
        }
    }

    // Run until the program finishes, then return the content of the
    // *val* register if it's a value, or None otherwise.
    pub fn run_until_halt(&mut self) -> Option<ValueRef> {
        self.halted = false;
        // Give any function invoked at the top an address to return
        // to. This assumes every code snippet ends with `FINISH`.
        self.stack.push(StackEntry::return_address(self.code.len() - 1));
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

fn prim_newline() -> Result<ValueRef, String> {
    println!();
    Ok(ValueRef::new(Value::Undefined))
}

fn prim_car(v: &ValueRef) -> Result<ValueRef, String> {
    if let Value::Cons(ref head, _) = v.deref() {
        Ok(head.clone())
    } else {
        Err("value is not a cons".to_string())
    }
}

fn prim_cdr(v: &ValueRef) -> Result<ValueRef, String> {
    if let Value::Cons(_, ref tail) = v.deref() {
        Ok(tail.clone())
    } else {
        Err("value is not a cons".to_string())
    }
}

fn prim_pair_p(v: &ValueRef) -> Result<ValueRef, String> {
    if let Value::Cons(_, _) = v.deref() {
        Ok(ValueRef::new(Value::Boolean(true)))
    } else {
        Ok(ValueRef::new(Value::Boolean(false)))
    }
}

fn prim_symbol_p(v: &ValueRef) -> Result<ValueRef, String> {
    if let Value::Symbol(_) = v.deref() {
        Ok(ValueRef::new(Value::Boolean(true)))
    } else {
        Ok(ValueRef::new(Value::Boolean(false)))
    }
}

fn prim_cons(hd: &ValueRef, tl: &ValueRef) -> Result<ValueRef, String> {
    Ok(ValueRef::new(Value::Cons(hd.clone(), tl.clone())))
}

fn prim_eq_p(v1: &ValueRef, v2: &ValueRef) -> Result<ValueRef, String> {
    // eq? is pointer equality
    if ValueRef::as_ptr(v1) == ValueRef::as_ptr(v2) {
        Ok(ValueRef::new(Value::Boolean(true)))
    } else {
        Ok(ValueRef::new(Value::Boolean(false)))
    }
}

fn arith(op: &Fn (i64, i64) -> i64, v1: &ValueRef, v2: &ValueRef) -> Result<ValueRef, String> {
    if let (Value::Int(a), Value::Int(b)) = (v1.deref(), v2.deref()) {
        Ok(ValueRef::new(Value::Int(op(*a, *b))))
    } else {
        Err(format!("operands are not both integers"))
    }
}

fn prim_plus(v1: &ValueRef, v2: &ValueRef) -> Result<ValueRef, String> {
    arith(&|a, b| a+b, v1, v2)
}

fn prim_minus(v1: &ValueRef, v2: &ValueRef) -> Result<ValueRef, String> {
    arith(&|a, b| a-b, v1, v2)
}

fn prim_times(v1: &ValueRef, v2: &ValueRef) -> Result<ValueRef, String> {
    arith(&|a, b| a*b, v1, v2)
}

fn prim_divide(v1: &ValueRef, v2: &ValueRef) -> Result<ValueRef, String> {
    arith(&|a, b| a/b, v1, v2)
}

fn compare(cmp: &Fn (i64, i64) -> bool, v1: &ValueRef, v2: &ValueRef) -> Result<ValueRef, String> {
    if let (Value::Int(a), Value::Int(b)) = (v1.deref(), v2.deref()) {
        Ok(ValueRef::new(Value::Boolean(cmp(*a, *b))))
    } else {
        Err(format!("operands are not both integers"))
    }
}

fn prim_equal(v1: &ValueRef, v2: &ValueRef) -> Result<ValueRef, String> {
    // `=`, not `equal?`
    compare(&|a,b| a==b, v1, v2)
}

fn prim_lt(v1: &ValueRef, v2: &ValueRef) -> Result<ValueRef, String> {
    // `=`, not `equal?`
    compare(&|a,b| a<b, v1, v2)
}

fn prim_gt(v1: &ValueRef, v2: &ValueRef) -> Result<ValueRef, String> {
    compare(&|a,b| a>b, v1, v2)
}

fn prim_lte(v1: &ValueRef, v2: &ValueRef) -> Result<ValueRef, String> {
    // `=`, not `equal?`
    compare(&|a,b| a<=b, v1, v2)
}

fn prim_gte(v1: &ValueRef, v2: &ValueRef) -> Result<ValueRef, String> {
    // `=`, not `equal?`
    compare(&|a,b| a>=b, v1, v2)
}


#[cfg(test)]
mod tests {
    use super::*;

    // compiler.ss is useful for generating these; either
    //
    //     ./compiler.ss file.ss
    //
    // or
    //
    //     ./compiler.ss <<EOF
    //     ((lambda vargs vargs) 1 2 3)
    //     EOF

    #[test]
    fn test_global_ref() {
        let one = ValueRef::new(Value::Int(1));
        let globals = vec![Some(one.clone())];
        let prog = vec![
            Opcode::GLOBAL_REF(0),
            Opcode::FINISH
        ];
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

    #[test]
    fn test_dotted_abstraction() {
        // this is something like
        // (define (fn a . b)
        //   b)
        //
        // (fn 1 2 3 4)
        let prog = vec![
            Opcode::CREATE_CLOSURE(1),
            Opcode::GOTO(4),
            Opcode::PACK_FRAME(1), // cons all args in slot >= 1 into slot 1
            Opcode::EXTEND_ENV,
            Opcode::SHALLOW_ARGUMENT_REF(1), // the list just consed
            Opcode::RETURN,
            // rather than assigning the closure somewhere, just push
            // onto stack
            Opcode::PUSH_VALUE,
            Opcode::INT_1, Opcode::PUSH_VALUE,
            Opcode::INT_2, Opcode::PUSH_VALUE,
            Opcode::INT_3, Opcode::PUSH_VALUE,
            Opcode::INT(4), Opcode::PUSH_VALUE,
            Opcode::ALLOCATE_FRAME(5),
            Opcode::POP_FRAME(3),
            Opcode::POP_FRAME(2),
            Opcode::POP_FRAME(1),
            Opcode::POP_FRAME(0),
            Opcode::POP_FUNCTION,
            Opcode::FUNCTION_GOTO,
            Opcode::FINISH,
        ];
        let mut vm = VM::new(vec![], &prog);
        if let Some(ref v) = vm.run_until_halt() {
            assert_eq!(*v, mklist(vec![Value::Int(2), Value::Int(3), Value::Int(4)]));
        } else {
            assert!(false, "running program did not result in a (correct) value");
        }
    }

    #[test]
    fn test_pop_cons() {
        // ((lambda vargs vargs) 1 2 3)
        let prog = vec![
            Opcode::INT_1, Opcode::PUSH_VALUE,
            Opcode::INT_2, Opcode::PUSH_VALUE,
            Opcode::INT_3, Opcode::PUSH_VALUE,
            Opcode::ALLOCATE_DOTTED_FRAME(1),
            Opcode::POP_CONS_FRAME(0),
            Opcode::POP_CONS_FRAME(0),
            Opcode::POP_CONS_FRAME(0),
            Opcode::EXTEND_ENV,
            Opcode::SHALLOW_ARGUMENT_REF(0),
            Opcode::FINISH,
        ];
        let mut vm = VM::new(vec![], &prog);
        if let Some(ref v) = vm.run_until_halt() {
            assert_eq!(*v, mklist(vec![Value::Int(1), Value::Int(2), Value::Int(3)]));
        } else {
            assert!(false, "running program did not result in a (correct) value");
        }
    }


    fn mklist(vals: Vec<Value>) -> ValueRef {
        let mut list = Value::Nil;
        for v in vals.into_iter().rev() {
            list = Value::Cons(ValueRef::new(v), ValueRef::new(list))
        }
        ValueRef::new(list)
    }
}
