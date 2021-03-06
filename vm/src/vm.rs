use std::ops::Deref;
use std::io;
use std::cell::RefCell;

use crate::instructions;
use crate::instructions::{Opcode};
use crate::values::{ValueRef,Value,NativeProc};
use crate::frame::{Frame,FrameRef};
use crate::read;

// These things are put on the stack:
//
// - values
// - activation frames
//
// Both of these are allocated on the heap, so we get a
// reference-counted pointer to them.
#[derive(Clone,Debug)]
enum StackEntry {
    value(ValueRef),
    frame(FrameRef),
    return_address(usize),
}

// The value register can have either a value (already allocated), or
// a frame under construction.
#[derive(Debug)]
enum ValReg {
    value(ValueRef),
    new_frame(Frame),
}

pub struct VM<'a> {
    // globals are variables defined in the global scope.
    globals: Vec<Option<ValueRef>>,
    // constants are literals that appeared in the source, i.e., they
    // are supplied along with the code.
    constants: Vec<ValueRef>,
    // predefined values are common values that usually have dedicated
    // opcodes. The indices must line up with what the compiler
    // expects.
    predefined: Vec<ValueRef>,

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

    // This is mainly for testing. Schemes often (by specification?)
    // have a dynamic variable for the current output port, but I have
    // not implemented either ports or dynamic variables.
    output_stream: Option<&'a mut (dyn io::Write)>,

    // halted means the program has finished
    halted: bool,

    // print a trace
    pub trace: bool,
}

fn make_predefined() -> Vec<ValueRef> {
    vec![
        // these first few also have dedicated opcodes
        Value::Boolean(true),  // HASHT
        Value::Boolean(false), // HASHF
        Value::Nil,
        Value::Native(NativeProc{name: "cons", func: invoke_cons}),
        Value::Native(NativeProc{name: "car", func: invoke_car}),
        Value::Native(NativeProc{name: "cdr", func: invoke_cdr}),
        Value::Native(NativeProc{name: "pair?", func: invoke_pair_p}),
        Value::Native(NativeProc{name: "null?", func: invoke_null_p}),
        Value::Native(NativeProc{name: "symbol?", func: invoke_symbol_p}),
        Value::Native(NativeProc{name: "eq?", func: invoke_eq_p}),
        Value::Native(NativeProc{name: "equal?", func: invoke_equal_p}),
        // these are referred to only by index, e.g., PREDEFINED(13)
        Value::Native(NativeProc{name: "read", func: invoke_read}),
        Value::Native(NativeProc{name: "newline", func: invoke_newline}),
        Value::Native(NativeProc{name: "display", func: invoke_display}),
        Value::Native(NativeProc{name: "+", func: invoke_plus}),
        Value::Native(NativeProc{name: "-", func: invoke_minus}),
        Value::Native(NativeProc{name: "=", func: invoke_equals}),
        Value::Native(NativeProc{name: "<", func: invoke_lt}),
        Value::Native(NativeProc{name: ">", func: invoke_gt}),
        Value::Native(NativeProc{name: "<=", func: invoke_lte}),
        Value::Native(NativeProc{name: ">=", func: invoke_gte}),
        Value::Native(NativeProc{name: "*", func: invoke_times}),
        Value::Native(NativeProc{name: "/", func: invoke_divide}),
        Value::Native(NativeProc{name: "make-vector", func: invoke_make_vector}),
        Value::Native(NativeProc{name: "vector-length", func: invoke_vector_length}),
        Value::Native(NativeProc{name: "vector-ref", func: invoke_vector_ref}),
        Value::Native(NativeProc{name: "vector-set!", func: invoke_vector_set}),
    ].into_iter().map(ValueRef::new).collect()
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
            predefined: make_predefined(),
            val: None,
            fun: None,
            arg1: None,
            arg2: None,
            env: FrameRef::new(top),
            code: program,
            pc: 0,
            stack: Vec::<StackEntry>::new(),
            output_stream: None,
            halted: true,
            trace: false,
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
        if let (Some(ref v1), Some(ValReg::value(ref v2))) = (&self.arg1, &self.val) {
            match prim(v1, v2) {
                Ok(v) => self.set_val(v),
                Err(e) => assert!(false, e),
            }
        } else {
            assert!(false, "CALL2: val or arg1 register does not contain value");
        }
    }

    fn invoke0(&mut self, prim: fn() -> Result<ValueRef, String>) {
        match prim() {
            Ok(v) => self.set_val(v),
            Err(e) => assert!(false, e),
        };
    }

    // like call1, but gets arguments from the frame in *val*.
    fn invoke1(&mut self, prim: fn(&ValueRef) -> Result<ValueRef, String>) {
        let result =
            if let Some(ValReg::new_frame(ref frame)) = self.val {
                assert!(frame.args.len() == 2, "INVOKE1: frame does not have expected number of arguments");
                let arg1 = frame.args[0].borrow();
                if let Some(ref v1) = *arg1 {
                    prim(v1)
                } else {
                    Err(String::from("INVOKE1: argument is not initialised"))
                }
            } else {
                Err(String::from("INVOKE1: value in val register is not a frame"))
            };
        match result {
            Ok(result) => self.set_val(result),
            Err(e) => assert!(false, "INVOKE1: primitive returned error")
        };
    }

    // like call2, but gets arguments from the frame in *val*
    fn invoke2(&mut self, prim: fn(&ValueRef, &ValueRef) -> Result<ValueRef, String>) {
        let result =
            if let Some(ValReg::new_frame(ref frame)) = self.val {
                assert!(frame.args.len() == 3, "INVOKE2: frame does not have expected number of arguments");
                let arg1 = frame.args[0].borrow();
                let arg2 = frame.args[1].borrow();
                if let (Some(ref v1), Some(ref v2)) = (&*arg1, &*arg2) {
                    prim(v1, v2)
                } else {
                    Err(String::from("INVOKE2: an argument is not initialised"))
                }
            } else {
                Err(String::from("INVOKE2: value in val register is not a frame"))
            };
        match result {
            Ok(result) => self.set_val(result),
            Err(e) => assert!(false, "INVOKE2: primitive returned error")
        };
    }

    fn invoke3(&mut self, prim: fn(&ValueRef, &ValueRef, &ValueRef) -> Result<ValueRef, String>) {
        let result =
            if let Some(ValReg::new_frame(ref frame)) = self.val {
                assert!(frame.args.len() == 4, "INVOKE3: frame does not have expected number of arguments");
                let arg1 = frame.args[0].borrow();
                let arg2 = frame.args[1].borrow();
                let arg3 = frame.args[1].borrow();
                if let (Some(ref v1), Some(ref v2), Some(ref v3)) = (&*arg1, &*arg2, &*arg3) {
                    prim(v1, v2, v3)
                } else {
                    Err(String::from("INVOKE3: an argument is not initialised"))
                }
            } else {
                Err(String::from("INVOKE3: value in val register is not a frame"))
            };
        match result {
            Ok(result) => self.set_val(result),
            Err(e) => assert!(false, "INVOKE3: primitive returned error")
        };
    }

    fn doreturn(&mut self) {
        if let Some(StackEntry::return_address(pc)) = self.stack.pop() {
            self.pc = pc;
        } else {
            assert!(false, "RETURN: top of stack did not contain a return address");
        }
    }

    fn print_trace(&self) {
        fn print_stack(stack: &Vec<StackEntry>) {
            eprint!("[");
            stack.iter().for_each(|entry| {
                match entry {
                    StackEntry::value(ref v) => {
                        let mut s = String::new();
                        write_value(v, &mut s);
                        eprint!("{}, ", s);
                    },
                    StackEntry::frame(_) => {
                        eprint!("frame, ");
                    },
                    StackEntry::return_address(pc) => {
                        eprint!("return address {}, ", pc);
                    },
                }
            });
            eprint!("]");
        }
        eprint!("[TRACE] == stack == "); print_stack(&self.stack); eprintln!();
        eprint!("[TRACE] == *val* == "); match &self.val {
            None => eprint!("#empty#"),
            Some(r) => {
                match r {
                    ValReg::value(ref v) => {
                        let mut s = String::new();
                        write_value(v, &mut s);
                        eprint!("{}", s);
                    },
                    ValReg::new_frame(_) => {
                        eprint!("frame");
                    }
                }
            }
        }; eprintln!();
    }

    pub fn step(&mut self) {
        assert!(self.pc < self.code.len(), "no instruction to run at program counter");
        let instr = &self.code[self.pc];

        if self.trace {
            eprintln!("[TRACE] pc: {} instr: {:?}, stack height: {}", self.pc, instr, self.stack.len());
            self.print_trace();
        }
        self.pc += 1;
        match instr {
            // Getting values from the environment
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
                    // sshhh).
                    self.set_val(v.clone());
                } else {
                    assert!(false, "CHECKED_GLOBAL_REF: global is uninitialised");
                }
            },
            Opcode::CONSTANT(index) => {
                assert!(self.constants.len() > *index as usize, "CONSTANT index out of bounds");
                self.set_val(self.constants[*index as usize].clone());
            },

            // Predefined values
            Opcode::PREDEFINED_HASHT     => self.set_val(ValueRef::clone(&self.predefined[0])),
            Opcode::PREDEFINED_HASHF     => self.set_val(ValueRef::clone(&self.predefined[1])),
            Opcode::PREDEFINED_NIL       => self.set_val(ValueRef::clone(&self.predefined[2])),
            Opcode::PREDEFINED_CONS      => self.set_val(ValueRef::clone(&self.predefined[3])),
            Opcode::PREDEFINED_CAR       => self.set_val(ValueRef::clone(&self.predefined[4])),
            Opcode::PREDEFINED_CDR       => self.set_val(ValueRef::clone(&self.predefined[5])),
            Opcode::PREDEFINED_PAIR_P    => self.set_val(ValueRef::clone(&self.predefined[6])),
            Opcode::PREDEFINED_SYMBOL_P  => self.set_val(ValueRef::clone(&self.predefined[7])),
            Opcode::PREDEFINED_EQ        => self.set_val(ValueRef::clone(&self.predefined[8])),
            Opcode::PREDEFINED(i) => {
                self.set_val(ValueRef::clone(&self.predefined[*i as usize]));
            },

            // HALT.
            Opcode::FINISH => {
                self.halted = true
            },

            // Mutation
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

            // For compiling alternatives and closures
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

            // Function-calling machinery
            Opcode::EXTEND_ENV => {
                if let Some(ValReg::new_frame(ref mut f)) = self.val {
                    f.next = Some(self.env.clone());
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
                let top = self.stack.pop();
                if let Some(StackEntry::frame(env)) = top {
                    self.env = env.clone();
                } else {
                    println!("{:?}", top);
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
                self.doreturn();
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
                        Value::Native(NativeProc{name: _, func: f}) => {
                            self.stack.push(StackEntry::return_address(self.pc));
                            f(self);
                            self.doreturn();
                        }
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
                        Value::Native(NativeProc{name: _, func: f}) => {
                            f(self);
                            self.doreturn();
                        }
                        v => {
                            assert!(false, "FUNCTION_GOTO: value in fun register is not a function, it is {:?}", v);
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
                    Frame::new(*i),
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

            // A general opcode for integers, then some specialised
            // ones.
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

            // CALL* opcodes: these called commonly-used primitives,
            // immediately.

            // newline needs access to the VM, so this doesn't use self.call(...)
            Opcode::CALL0_newline => {
                invoke_newline(self);
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
            Opcode::CALL1_null_p => {
                self.call1(prim_null_p);
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
            Opcode::CALL2_equal_p => {
                self.call2(prim_equal_p);
            },
            Opcode::CALL2_PLUS => {
                self.call2(prim_plus);
            },
            Opcode::CALL2_MINUS => {
                self.call2(prim_minus);
            },
            Opcode::CALL2_EQUALS => {
                self.call2(prim_equals);
            },
            Opcode::CALL2_LT => {
                self.call2(prim_lt);
            },
            Opcode::CALL2_GT => {
                self.call2(prim_gt);
            },
            Opcode::CALL2_LE => {
                self.call2(prim_lte);
            },
            Opcode::CALL2_GE => {
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

// Primitives: these come in two forms, one for calling directly by
// CALL* opcodes, which get arguments from registers, and one for
// creating native procedures as values which get arguments from an
// activation frame. In the case of nullary procedures, the calling
// convention is the same, since there's no arguments to fetch.

// newline is an exception; it does not take arguments, and needs
// access to the VM, so it does not delegate to vm.invoke*.
fn invoke_newline(vm: &mut VM) {
    match vm.output_stream {
        Some(ref mut writer) => { write!(writer, "\n").expect("failed to write to output"); },
        None => { println!(); }
    };
    vm.set_val(ValueRef::new(Value::Undefined));
}

fn prim_car(v: &ValueRef) -> Result<ValueRef, String> {
    if let Value::Cons(ref head, _) = v.deref() {
        Ok(head.clone())
    } else {
        Err("value is not a cons".to_string())
    }
}
fn invoke_car(vm: &mut VM) {
    vm.invoke1(prim_car);
}

fn prim_cdr(v: &ValueRef) -> Result<ValueRef, String> {
    if let Value::Cons(_, ref tail) = v.deref() {
        Ok(tail.clone())
    } else {
        Err("value is not a cons".to_string())
    }
}
fn invoke_cdr(vm: &mut VM) {
    vm.invoke1(prim_cdr);
}

fn prim_pair_p(v: &ValueRef) -> Result<ValueRef, String> {
    if let Value::Cons(_, _) = v.deref() {
        Ok(ValueRef::new(Value::Boolean(true)))
    } else {
        Ok(ValueRef::new(Value::Boolean(false)))
    }
}
fn invoke_pair_p(vm: &mut VM) {
    vm.invoke1(prim_pair_p);
}

fn prim_null_p(v: &ValueRef) -> Result<ValueRef, String> {
    if let Value::Nil = v.deref() {
        Ok(ValueRef::new(Value::Boolean(true)))
    } else {
        Ok(ValueRef::new(Value::Boolean(false)))
    }
}
fn invoke_null_p(vm: &mut VM) {
    vm.invoke1(prim_null_p);
}

fn prim_symbol_p(v: &ValueRef) -> Result<ValueRef, String> {
    if let Value::Symbol(_) = v.deref() {
        Ok(ValueRef::new(Value::Boolean(true)))
    } else {
        Ok(ValueRef::new(Value::Boolean(false)))
    }
}
fn invoke_symbol_p(vm: &mut VM) {
    vm.invoke1(prim_symbol_p);
}

fn prim_cons(hd: &ValueRef, tl: &ValueRef) -> Result<ValueRef, String> {
    Ok(ValueRef::new(Value::Cons(hd.clone(), tl.clone())))
}
fn invoke_cons(vm: &mut VM) {
    vm.invoke2(prim_cons);
}

// eq? is supposed to test for the objects being the same, but it's
// assumed that symbols are interned and {#t, #f, nil} are all
// singletons, so I need a special case for those here.
fn prim_eq_p(v1: &ValueRef, v2: &ValueRef) -> Result<ValueRef, String> {
    // eq? is pointer equality
    if ValueRef::as_ptr(v1) == ValueRef::as_ptr(v2) {
        Ok(ValueRef::new(Value::Boolean(true)))
    } else {
        let res = match (v1.deref(), v2.deref()) {
            (Value::Symbol(s1), Value::Symbol(s2)) =>  s1 == s2,
            (Value::Nil, Value::Nil) => true,
            (Value::Boolean(b1), Value::Boolean(b2)) => b1 == b2,
            _ => false
        };
        Ok(ValueRef::new(Value::Boolean(res)))
    }
}
fn invoke_eq_p(vm: &mut VM) {
    vm.invoke2(prim_eq_p);
}

fn prim_equal_p(v1: &ValueRef, v2: &ValueRef) -> Result<ValueRef, String> {
    fn eq(v1: &ValueRef, v2: &ValueRef) -> bool {
        match (v1.deref(), v2.deref()) {
            (Value::Nil, Value::Nil) => true,
            (Value::Int(i1), Value::Int(i2)) => i1 == i2,
            (Value::Symbol(s1), Value::Symbol(s2)) => s1 == s2,
            (Value::String(s1), Value::String(s2)) => s1 == s2,
            (Value::Boolean(b1), Value::Boolean(b2)) => b1 == b2,
            (Value::Cons(hd1, tl1), Value::Cons(hd2, tl2)) => {
                if eq(hd1, hd2) {
                    eq(tl1, tl2) // naughty stack-eating recursion
                } else {
                    false
                }
            },
            (Value::Vector(v1), Value::Vector(v2)) => {
                v1.len() == v2.len() &&
                    v1.into_iter().zip(v2.into_iter()).all(
                        |(a, b)| { eq(&*a.borrow(), &*b.borrow()) }
                    )
            },
            _ => false
        }
    }
    Ok(ValueRef::new(Value::Boolean(eq(v1, v2))))
}
fn invoke_equal_p(vm: &mut VM) {
    vm.invoke2(prim_eq_p);
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
fn invoke_plus(vm: &mut VM) {
    vm.invoke2(prim_plus);
}

fn prim_minus(v1: &ValueRef, v2: &ValueRef) -> Result<ValueRef, String> {
    arith(&|a, b| a-b, v1, v2)
}
fn invoke_minus(vm: &mut VM) {
    vm.invoke2(prim_minus);
}

fn prim_times(v1: &ValueRef, v2: &ValueRef) -> Result<ValueRef, String> {
    arith(&|a, b| a*b, v1, v2)
}
fn invoke_times(vm: &mut VM) {
    vm.invoke2(prim_times);
}

fn prim_divide(v1: &ValueRef, v2: &ValueRef) -> Result<ValueRef, String> {
    arith(&|a, b| a/b, v1, v2)
}
fn invoke_divide(vm: &mut VM) {
    vm.invoke2(prim_divide);
}

fn compare(cmp: &Fn (i64, i64) -> bool, v1: &ValueRef, v2: &ValueRef) -> Result<ValueRef, String> {
    if let (Value::Int(a), Value::Int(b)) = (v1.deref(), v2.deref()) {
        Ok(ValueRef::new(Value::Boolean(cmp(*a, *b))))
    } else {
        Err(format!("operands are not both integers"))
    }
}

fn prim_equals(v1: &ValueRef, v2: &ValueRef) -> Result<ValueRef, String> {
    // `=`, not `equal?`
    compare(&|a,b| a==b, v1, v2)
}
fn invoke_equals(vm: &mut VM) {
    vm.invoke2(prim_equals);
}

fn prim_lt(v1: &ValueRef, v2: &ValueRef) -> Result<ValueRef, String> {
    // `=`, not `equal?`
    compare(&|a,b| a<b, v1, v2)
}
fn invoke_lt(vm: &mut VM) {
    vm.invoke2(prim_lt);
}

fn prim_gt(v1: &ValueRef, v2: &ValueRef) -> Result<ValueRef, String> {
    compare(&|a,b| a>b, v1, v2)
}
fn invoke_gt(vm: &mut VM) {
    vm.invoke2(prim_gt);
}

fn prim_lte(v1: &ValueRef, v2: &ValueRef) -> Result<ValueRef, String> {
    // `=`, not `equal?`
    compare(&|a,b| a<=b, v1, v2)
}
fn invoke_lte(vm: &mut VM) {
    vm.invoke2(prim_lte);
}

fn prim_gte(v1: &ValueRef, v2: &ValueRef) -> Result<ValueRef, String> {
    // `=`, not `equal?`
    compare(&|a,b| a>=b, v1, v2)
}
fn invoke_gte(vm: &mut VM) {
    vm.invoke2(prim_gte);
}

fn invoke_read(_: &mut VM) {
    assert!(false, "not implemented")
}

fn integer_arg(arg: &ValueRef) -> Result<i64, String> {
    if let Value::Int(ref i) = arg.deref() {
        Ok(*i)
    } else {
        Err(format!("expected integer as argument, got {:?}", arg))
    }
}

fn usize_arg(arg: &ValueRef) -> Result<usize, String> {
    use std::convert::TryInto;
    let i = integer_arg(arg)?;
    i.try_into().map_err(|e| { format!("argument out of bounds for int: {}", e) })
}

fn vector_arg(arg: &ValueRef) -> Result<&Vec<RefCell<ValueRef>>, String> {
    if let Value::Vector(v) = arg.deref() {
        Ok(v)
    } else {
        Err(format!("expected vector as argument, got {:?}", arg))
    }
}

fn builtin_make_vector(len: &ValueRef) -> Result<ValueRef, String> {
    let i = usize_arg(len)?;
    let v = Value::new_vector(i as usize);
    Ok(ValueRef::new(v))
}
fn invoke_make_vector(vm: &mut VM) {
    vm.invoke1(builtin_make_vector);
}

fn builtin_vector_length(vector: &ValueRef) -> Result<ValueRef, String> {
    let v = vector_arg(vector)?;
    Ok(ValueRef::new(Value::Int(v.len() as i64)))
}
fn invoke_vector_length(vm: &mut VM) {
    vm.invoke1(builtin_vector_length);
}

fn builtin_vector_ref(vector: &ValueRef, index: &ValueRef) -> Result<ValueRef, String> {
    let i = usize_arg(index)?;
    let v = vector_arg(vector)?;
    if i < 0 || i >= v.len() {
        Err(format!("index out of bounds: {}", i))
    } else {
        let cell = v[i].borrow();
        Ok(cell.clone())
    }
}
fn invoke_vector_ref(vm: &mut VM) {
    vm.invoke2(builtin_vector_ref);
}

fn builtin_vector_set(vector: &ValueRef, index: &ValueRef, val: &ValueRef) -> Result<ValueRef, String> {
    let v = vector_arg(vector)?;
    let i = usize_arg(index)?;
    if i < 0 || i >= v.len() {
        Err(format!("index out of bounds: {}", i))
    } else {
        let mut cell = v[i].borrow_mut();
        *cell = val.clone();
        Ok(ValueRef::new(Value::Undefined))
    }
}
fn invoke_vector_set(vm: &mut VM) {
    vm.invoke3(builtin_vector_set);
}

// If this used a global, or thread-local, for the output port, it
// would be better implemented in Scheme. As it is, I need "native"
// procedures with access to the registers anyway, so I can cheat
// here.
fn invoke_display(vm: &mut VM) {
    if let Some(ValReg::new_frame(ref frame)) = vm.val {
        assert_eq!(frame.args.len(), 2, "expected 1 argument to display");
        let arg = frame.args[0].borrow();
        if let Some(ref val) = *arg {
            let mut s = String::new();
            write_value(val, &mut s);
            match &mut vm.output_stream {
                Some(writer) => { write!(writer, "{}", s).expect("unable to write to writer") },
                None => { print!("{}", s) },
            };
        } else {
            assert!(false, "argument is not initialised");
        }
    } else {
        assert!(false, "val register does not contain a frame");
    }
    vm.set_val(ValueRef::new(Value::Undefined));
}

fn write_value(v: &ValueRef, acc: &mut String) {
    match v.deref() {
        Value::Symbol(ref s) => acc.push_str(s),
        Value::Int(i) => acc.push_str(&i.to_string()),
        Value::Boolean(b) => if *b { acc.push_str("#t") } else { acc.push_str("#f") },
        Value::String(s) => acc.push_str(s),
        Value::Cons(hd, tl) => {
            acc.push('(');
            write_value(hd, acc);
            let mut cell = tl;
            loop {
                match cell.deref() {
                    Value::Cons(hd1, tl1) => {
                        acc.push(' ');
                        write_value(hd1, acc);
                        cell = tl1;
                    },
                    Value::Nil => {
                        acc.push(')');
                        break;
                    },
                    _ => {
                        acc.push_str(" . ");
                        write_value(cell, acc);
                        acc.push(')');
                        break;
                    }
                }
            }
        },
        Value::Nil => acc.push_str("'()"),
        Value::Vector(v) => {
            acc.push_str(&format!("#{}(", v.len()));
            v.iter().enumerate().for_each(|(i, e)| {
                write_value(&e.borrow(), acc);
                if i < v.len() - 1 {
                    acc.push(' ');
                }
            });
            acc.push(')');
        },
        Value::Closure(_, _) => acc.push_str("#<lambda>"),
        Value::Native(NativeProc{name, func: _}) => {
            acc.push_str("#<native procedure ");
            acc.push_str(name);
            acc.push_str(">");
        },
        Value::Undefined => acc.push_str("#<undefined>"),
        _ => assert!(false, "not implemented"),
    };
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

    fn test_compare(prim: fn(&ValueRef, &ValueRef) -> Result<ValueRef, String>, v1: &ValueRef, v2: &ValueRef, expected: bool) {
        let res = prim(&v1, &v2).expect("equals? should not error");
        if let Value::Boolean(b) = res.deref() {
            assert_eq!(*b, expected, "(equal? {:?} {:?})", v1.deref(), v2.deref());
        } else {
                assert!(false, "expected boolean result, got {:?}", res.deref());
        }
    }

    #[test]
    fn test_equal() {

        fn test_eq(v1: ValueRef, v2: ValueRef) {
            test_compare(prim_equal_p, &v1, &v2, true);
        }
        fn test_noteq(v1: ValueRef, v2: ValueRef) {
            test_compare(prim_equal_p, &v1, &v2, false);
        }

        test_eq(ValueRef::new(Value::Int(2)), ValueRef::new(Value::Int(2)));
        test_eq(ValueRef::new(Value::String("foobar".to_string())),
                ValueRef::new(Value::String("foobar".to_string())));
        test_eq(ValueRef::new(Value::Symbol("symmmmbol".to_string())),
                ValueRef::new(Value::Symbol("symmmmbol".to_string())));
        test_eq(mklist(vec![Value::Int(1), Value::String("barfoo".to_string())]),
                mklist(vec![Value::Int(1), Value::String("barfoo".to_string())]));
        test_eq(mkvec(vec![Value::Int(1), Value::String("barfoo".to_string())]),
                mkvec(vec![Value::Int(1), Value::String("barfoo".to_string())]));

        test_noteq(ValueRef::new(Value::Boolean(true)), ValueRef::new(Value::Boolean(false)));
        test_noteq(mklist(vec![Value::Int(2), Value::String("foobar".to_string())]),
                   mklist(vec![Value::Int(2), Value::String("foobar".to_string()), Value::Nil]));
        test_noteq(mkvec(vec![Value::Symbol("boofar".to_string()), Value::Int(2)]),
                   mkvec(vec![Value::Symbol("boofar".to_string()), Value::Int(3)]));
    }

    #[test]
    fn test_eq_p() {
        // the same pointer is eq?
        let lst = mklist(vec![Value::Int(2), Value::Boolean(false)]);
        test_compare(prim_eq_p, &lst, &lst, true);
        // symbols with the same string are eq?
        test_compare(prim_eq_p,
                     &ValueRef::new(Value::Symbol(String::from("eq?"))),
                     &ValueRef::new(Value::Symbol(String::from("eq?"))), true);

        // lists and vectors are not eq?, even if their contents are eq?
        test_compare(prim_eq_p,
                     &mkvec(vec![Value::Symbol("notequal".to_string())]),
                     &mkvec(vec![Value::Symbol("notequal".to_string())]), false);
    }

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

    #[test]
    fn test_predefined() {
        // just lump the predefined values into a list:
        // ((lambda vargs vargs) #t #f '())
        let prog = vec![
            Opcode::PREDEFINED_HASHT, Opcode::PUSH_VALUE,
            Opcode::PREDEFINED_HASHF, Opcode::PUSH_VALUE,
            Opcode::PREDEFINED_NIL, Opcode::PUSH_VALUE,
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
            assert_eq!(*v, mklist(vec![Value::Boolean(true), Value::Boolean(false), Value::Nil]));
        } else {
            assert!(false, "running program did not result in a (correct) value");
        }
    }

    #[test]
    fn test_invoke_1_2() {
        // Putting in a let-form makes the compiler unable to use
        // call*, so I can test function-invoke and function-goto.
        // This is
        //     ((lambda (cons cdr) (cons 0 (cdr '(1 2 3)))) cons cdr)
        let consts = vec![mklist(vec![Value::Int(1), Value::Int(2), Value::Int(3)])];
        let prog = vec![
            Opcode::PREDEFINED_CONS, Opcode::PUSH_VALUE, // ) the args to the lambda
            Opcode::PREDEFINED_CDR, Opcode::PUSH_VALUE,  // )
            Opcode::ALLOCATE_FRAME(3), // ) put them in the frame ...
            Opcode::POP_FRAME(1),      // )
            Opcode::POP_FRAME(0),      // )
            Opcode::EXTEND_ENV,        // enter the lambda body
            Opcode::SHALLOW_ARGUMENT_REF(0), Opcode::PUSH_VALUE, // eval head of (cons 0 ...)
            Opcode::INT_0, Opcode::PUSH_VALUE, // eval arg0 of (cons 0 ...)
            Opcode::SHALLOW_ARGUMENT_REF(1), Opcode::PUSH_VALUE, // push cdr
            Opcode::CONSTANT(0), Opcode::PUSH_VALUE, // start eval (cdr '(1 2 3))
            Opcode::ALLOCATE_FRAME(2), // ) put '(1 2 3) in frame
            Opcode::POP_FRAME(0),      // )
            Opcode::POP_FUNCTION, // cdr -> *fun*
            Opcode::PRESERVE_ENV, // not a tail call; put the environment on the stack
            Opcode::FUNCTION_INVOKE, // call *fun* with the frame in *val*
            Opcode::RESTORE_ENV, // restore the env
            Opcode::PUSH_VALUE, // result from cdr to stack
            Opcode::ALLOCATE_FRAME(3), // ) put args for cons into frame
            Opcode::POP_FRAME(1),      // )
            Opcode::POP_FRAME(0),      // )
            Opcode::POP_FUNCTION,
            Opcode::FUNCTION_GOTO, // tail-call cons
            Opcode::FINISH,
        ];
        let mut vm = VM::new(consts, &prog);
        if let Some(ref v) = vm.run_until_halt() {
            assert_eq!(*v, mklist(vec![Value::Int(0), Value::Int(2), Value::Int(3)]));
        } else {
            assert!(false, "running program did not result in a (correct) value");
        }
    }

    #[test]
    fn test_display() {
        fn display(v: ValueRef, expected: &str) {
            let consts = vec![v];
            let prog = vec![
                Opcode::PREDEFINED(13), Opcode::PUSH_VALUE,
                Opcode::CONSTANT(0), Opcode::PUSH_VALUE,
                Opcode::ALLOCATE_FRAME(2),
                Opcode::POP_FRAME(0),
                Opcode::POP_FUNCTION,
                Opcode::FUNCTION_GOTO,
                Opcode::FINISH,
            ];

            let mut output = Vec::<u8>::new();
            {
                use std::io::Cursor;
                let out = &mut Cursor::new(&mut output);
                let mut vm = VM::new(consts, &prog);
                vm.output_stream = Some(out);
                match vm.run_until_halt() {
                    Some(_) => (),
                    _ => {
                        assert!(false, "running program did not result in a (correct) value");
                    }
                };
                vm.output_stream = None;
            }
            let out = String::from_utf8(output).expect("output is UTF8");
            assert_eq!(out, String::from(expected));
        }

        display(ValueRef::new(Value::Int(1)), "1");
        display(ValueRef::new(Value::Boolean(true)), "#t");
        display(ValueRef::new(Value::Boolean(false)), "#f");
        display(ValueRef::new(Value::Nil), "'()");
        display(ValueRef::new(Value::Vector((1..10).map(|i| {
            RefCell::new(ValueRef::new(Value::Int(i)))
        }).collect())), "#9(1 2 3 4 5 6 7 8 9)");
        display(ValueRef::new(Value::Undefined), "#<undefined>");
        display(mklist(vec![
            Value::Int(0), Value::Boolean(false),
            Value::Cons(ValueRef::new(Value::Nil),
                        ValueRef::new(Value::Symbol(String::from("foobar")))),
        ]), "(0 #f ('() . foobar))");
    }

    fn mklist(vals: Vec<Value>) -> ValueRef {
        let mut list = Value::Nil;
        for v in vals.into_iter().rev() {
            list = Value::Cons(ValueRef::new(v), ValueRef::new(list))
        }
        ValueRef::new(list)
    }

    fn mkvec(vals: Vec<Value>) -> ValueRef {
        let vec: Vec<RefCell<ValueRef>> = vals.into_iter().map(|v| {
            RefCell::new(ValueRef::new(v))
        }).collect();
        ValueRef::new(Value::Vector(vec))
    }
}
