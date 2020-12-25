// This has all the opcodes for the VM. This is based on the ops in
// section 7.4 of LISP in Small Pieces; my code for that and other
// chapters is in https://github.com/squaremo/lisp-in-small-pieces/.

// slots, stack levels, globals all get indexed with 8 bits.
pub type index = u8;

// code offsets are 16 bits.
pub type offset = u16;

// (These are set out in the same order as defined in the
// `define-instruction-set` call.)

// The instructions are an enum; this means they don't travel as bytes
// as well, but it's easier to see what's happening. To make it more
// compact, I could use a union type with enum instructions and byte
// arguments as the members.
#[derive(Debug,PartialEq)]
#[allow(dead_code)]
#[allow(non_camel_case_types)]
pub enum Opcode {
    // get a value from the activation frame and put in the `*val*`
    // register, slot `i`.
    SHALLOW_ARGUMENT_REF(index),
    // get a value from an activation frame `i` levels up, slot `j`,
    // and put it in the `*val*` register.
    DEEP_ARGUMENT_REF(index, index),
    // put the value of a global slot `i` in the `*val*` register.
    GLOBAL_REF(index),
    // put the value of global slot `i` in the `*val*` register,
    // panicking if it is undefined.
    CHECKED_GLOBAL_REF(index),
    // put the value of constant in slot `i` in the `*val*` register.
    CONSTANT(index),

    // A bunch of predefined values, that get their own opcodes. All
    // of these put the predefined value in the `*val*` register. (NB:
    // some primitives also get inlined when invoked directly; see
    // CALL* below).
    PREDEFINED_HASHT,
    PREDEFINED_HASHF,
    PREDEFINED_NIL,
    PREDEFINED_CONS,
    PREDEFINED_CAR,
    PREDEFINED_CDR,
    PREDEFINED_PAIR_P,
    PREDEFINED_SYMBOL_P,
    PREDEFINED_EQ,
    // then there's a few "second level" predefined values, that
    // dispatch on an index.
    PREDEFINED(index),

    // Call the exit handler.
    FINISH,

    // get the value in the *val* register and set it in the
    // activation frame, slot i.
    SET_SHALLOW_ARGUMENT(index),
    // set the value in the *val* register into the activation frame i
    // levels up, slot j.
    SET_DEEP_ARGUMENT(index, index),
    // set the global in the globals storage, slot i, to the value
    // from the *val* register.
    SET_GLOBAL(index),

    // Add the given offset the `*pc*` (program counter) register ; I
    // can't be bothered having two varieties (16-bit "long" and 8 bit
    // "short").
    GOTO(offset),
    // Add the offset to the `*pc*` register if the `*val*` register
    // is `#f`.
    JUMP(offset),

    // link the activation frame in the `*val*` register as the parent
    // of the frame in the `*env*` register.
    EXTEND_ENV,
    // replace the activation frame in `*env*` with its parent frame.
    UNLINK_ENV,

    // push the value in the *val* register onto the stack.
    PUSH_VALUE,
    // pop a value from the stack and put it in the *arg1* register.
    POP_ARG1,
    // pop two values from the stack and put them in the *arg1* and
    // *arg2* registers; the top value goes in *arg2*.
    POP_2ARG,
    // push the current activation frame, in the *env* register, onto
    // the stack.
    PRESERVE_ENV,
    // pop from the stack and replace the activation frame in the
    // *env* register.
    RESTORE_ENV,
    // pop from the stack and put the value in the *fun* (function)
    // register.
    POP_FUNCTION,

    // allocate a closure value referring to the code at the offset
    // given, and the activation frame in the *env* register, then put
    // that value in the *val* register.
    CREATE_CLOSURE(offset),

    // pop from the stack and set the *pc* register to the address
    // given.
    RETURN,

    // ready an activation frame in the *val* register by taking the
    // "excess" arguments (those in slots after the arity given) and
    // making a list from them, then putting that list in the arity+1
    // slot of the frame.
    PACK_FRAME(index),

    // invoke the function in the *fun* register as a non-tail call.
    FUNCTION_INVOKE,
    // invoke the function in the *fun* register as a tail call.
    FUNCTION_GOTO,

    // pop a value from the stack, and cons it onto the value in the
    // given slot of the activation frame in the *val* register. This
    // is used to accumulate rest args during application of a
    // procedure known to be dotted; e.g.,
    //
    //     ((lambda vargs (...)) a b c)
    POP_CONS_FRAME(index),

    // allocate an activation frame of the size given
    // and put it in the *val* register.
    ALLOCATE_FRAME(index),
    // allocate an activation frame for a dotted application and put
    // it in the *val* register.
    ALLOCATE_DOTTED_FRAME(index),

    // pop a value from the stack and put it in the given slot in the
    // activation frame in the *val* register.
    POP_FRAME(index),

    // check that the size of the activation frame in the *val*
    // register is the given arity, and panic if not.
    ARITY_EQUAL(index),
    // check that the size of the activation frame in the *val*
    // register is at least the given arity, and panic if not.
    ARITY_GE(index),

    // these put small or commonly used integers into the *val*
    // register.
    INT(index),
    INT_NEG1,
    INT_0,
    INT_1,
    INT_2,
    INT_3,

    // these are primitives that are implemented in host code and
    // called directly by the interpreter.
    CALL0_newline,
    CALL0_read,
    CALL1_car,
    CALL1_cdr,
    CALL1_pair_p,
    CALL1_symbol_p,
    CALL2_cons,
    CALL2_eq_p,
    CALL2_PLUS,
    CALL2_MINUS,
    CALL2_EQUAL,
    CALL2_LT,
    CALL2_GT,
    CALL2_LTE,
    CALL2_GTE,
    CALL2_TIMES,
    CALL2_DIVIDE,
}
