// Implements a translation from the intermediate symbolic form, to
// opcodes.

use std::ops::Deref;
use std::convert::TryFrom;

use crate::read::Expr;
use crate::instructions;
use crate::instructions::Opcode;
use crate::values::{Value,ValueRef};

pub struct Program {
    pub constants: Vec<ValueRef>,
    pub code: Vec<Opcode>,
}

fn assoc_cdr<'a>(exprs: &'a [Expr], key: &'a str) -> Result<&'a [Expr], String> {
    for e in exprs {
        if let Expr::List(h, t) = e {
            match h.deref() {
                Expr::Symbol(ref s) if s == key => {
                    return Ok(&t);
                }
                _ => ()
            }
        }
    }
    Err(format!("{} not found", key))
}

pub fn translate(program: Expr) -> Result<Program, String> {
    let exprs = [program];
    let prog = assoc_cdr(&exprs, "program")?;
    let consts = assoc_cdr(prog, "constants")?;
    let code = assoc_cdr(prog, "code")?;

    let constvalues = to_values(consts);
    let instructions = to_instructions(code)?;
    Ok(Program{
        constants: constvalues,
        code: instructions,
    })
}

fn to_values(exprs: &[Expr]) -> Vec<ValueRef> {
    exprs.into_iter().map(valuify).collect()
}

fn valuify(expr: &Expr) -> ValueRef {
    match expr {
        Expr::Int(i) => ValueRef::new(Value::Int(*i)),
        Expr::Symbol(s) => ValueRef::new(Value::Symbol(String::from(s))),
        Expr::List(h, t) => {
            let items = (vec![h.deref()].into_iter()).chain(t.into_iter());
            items.map(valuify).rev().fold(ValueRef::new(Value::Nil), |a, v| {
                ValueRef::new(Value::Cons(v, a))
            })
        },
        Expr::Nil => ValueRef::new(Value::Nil),
        Expr::True => ValueRef::new(Value::Boolean(true)),
        Expr::False => ValueRef::new(Value::Boolean(false)),
    }
}

fn to_instructions(exprs: &[Expr]) -> Result<Vec<Opcode>, String> {
    exprs.into_iter().map(|instr| {
        if let Expr::List(h, t) = instr {
            if let Expr::Symbol(ref s) = h.deref() {
                return to_opcode(s, t)
            }
        }
        return Err(format!("unrecognised instruction {:?}", instr))
    }).collect::<Result<Vec<Opcode>, String>>()
}

fn to_opcode(sym: &str, args: &[Expr]) -> Result<Opcode, String> {
    match sym {
        "shallow-argument-ref" => opcode_index1(args, Opcode::SHALLOW_ARGUMENT_REF),
        "deep-argument-ref" => opcode_index2(args, Opcode::DEEP_ARGUMENT_REF),
        "global-ref" => opcode_index1(args, Opcode::GLOBAL_REF), // shouldn't see this, but CHECKED_... instead.
        "checked-global-ref" => opcode_index1(args, Opcode::CHECKED_GLOBAL_REF),
        "constant" => opcode_index1(args, Opcode::CONSTANT),
        "predefined_hasht" => Ok(Opcode::PREDEFINED_HASHT),
        "predefined_hashf" => Ok(Opcode::PREDEFINED_HASHF),
        "predefined_nil" => Ok(Opcode::PREDEFINED_NIL),
        "predefined_cons" => Ok(Opcode::PREDEFINED_CONS),
        "predefined_car" => Ok(Opcode::PREDEFINED_CAR),
        "predefined_cdr" => Ok(Opcode::PREDEFINED_CDR),
        "predefined_pair?" => Ok(Opcode::PREDEFINED_PAIR_P),
        "predefined_symbol?" => Ok(Opcode::PREDEFINED_SYMBOL_P),
        "predefined_eq?" => Ok(Opcode::PREDEFINED_EQ),
        "predefined" => opcode_index1(args, Opcode::PREDEFINED),
        "finish" => Ok(Opcode::FINISH),
        "set-shallow-argument!" => opcode_index1(args, Opcode::SET_SHALLOW_ARGUMENT),
        "set-deep-argument!" => opcode_index2(args, Opcode::SET_DEEP_ARGUMENT),
        "set-global!" => opcode_index1(args, Opcode::SET_GLOBAL),
        "goto" => opcode_index1(args, Opcode::GOTO),
        "jump" => opcode_index1(args, Opcode::JUMP),
        "extend-env" => Ok(Opcode::EXTEND_ENV),
        "unlink-env" => Ok(Opcode::UNLINK_ENV),
        "push-value" => Ok(Opcode::PUSH_VALUE),
        "pop-arg1" => Ok(Opcode::POP_ARG1),
        "pop-2arg" => Ok(Opcode::POP_2ARG),
        "preserve-env" => Ok(Opcode::PRESERVE_ENV),
        "restore-env" => Ok(Opcode::RESTORE_ENV),
        "pop-function" => Ok(Opcode::POP_FUNCTION),
        "create-closure" => opcode_index1(args, Opcode::CREATE_CLOSURE),
        "return" => Ok(Opcode::RETURN),
        "pack-frame!" => opcode_index1(args, Opcode::PACK_FRAME),
        "function-invoke" => Ok(Opcode::FUNCTION_INVOKE),
        "function-goto" => Ok(Opcode::FUNCTION_GOTO),
        "pop-cons-frame!" => opcode_index1(args, Opcode::POP_CONS_FRAME),
        "allocate-frame" => opcode_index1(args, Opcode::ALLOCATE_FRAME),
        "pop-frame!" => opcode_index1(args, Opcode::POP_FRAME),
        "arity=?" => opcode_index1(args, Opcode::ARITY_EQUAL),
        "arity>=?" => opcode_index1(args, Opcode::ARITY_GE),
        "int" => opcode_index1(args, Opcode::INT),
        "int_neg1" => Ok(Opcode::INT_NEG1),
        "int_0" => Ok(Opcode::INT_0),
        "int_1" => Ok(Opcode::INT_1),
        "int_2" => Ok(Opcode::INT_2),
        "int_3" => Ok(Opcode::INT_3),
        "constant" => opcode_index1(args, Opcode::CONSTANT),
        "call0_newline" => Ok(Opcode::CALL0_newline),
        "call0_read" => Ok(Opcode::CALL0_read),
        "call1_car" => Ok(Opcode::CALL1_car),
        "call1_cdr" => Ok(Opcode::CALL1_cdr),
        "call1_pair_p" => Ok(Opcode::CALL1_pair_p),
        "call1_null_p" => Ok(Opcode::CALL1_null_p),
        "call1_symbol_p" => Ok(Opcode::CALL1_symbol_p),
        "call2_cons" => Ok(Opcode::CALL2_cons),
        "call2_eq_p" => Ok(Opcode::CALL2_eq_p),
        "call2_equal_p" => Ok(Opcode::CALL2_equal_p),
        "call2_plus" => Ok(Opcode::CALL2_PLUS),
        "call2_minus" => Ok(Opcode::CALL2_MINUS),
        "call2_equals" => Ok(Opcode::CALL2_EQUALS),
        "call2_lt" => Ok(Opcode::CALL2_LT),
        "call2_gt" => Ok(Opcode::CALL2_GT),
        "call2_lte" => Ok(Opcode::CALL2_LTE),
        "call2_gte" => Ok(Opcode::CALL2_GTE),
        "call2_times" => Ok(Opcode::CALL2_TIMES),
        "call2_divide" => Ok(Opcode::CALL2_DIVIDE),

        _ => Err(format!("Unrecognised symbol {}", sym))
    }
}

fn opcode_index1<T: TryFrom<i64>>(args: &[Expr], opcode: fn(T) -> Opcode) -> Result<Opcode, String> {
    if args.len() == 1 {
        if let Expr::Int(i) = args[0] {
            let ind = index_from(i)?;
            return Ok(opcode(ind))
        }
    }
    return Err(format!("Expected 1 arg, got {:#?}", args))
}

fn opcode_index2<S: TryFrom<i64>, T: TryFrom<i64>>(args: &[Expr], opcode: fn(S, T) -> Opcode) -> Result<Opcode, String> {
    if args.len() == 2 {
        if let (Expr::Int(i1), Expr::Int(i2)) = (&args[0], &args[1]) {
            let ind1 = index_from(*i1)?;
            let ind2 = index_from(*i2)?;
            return Ok(opcode(ind1, ind2))
        }
    }
    return Err(format!("Expected 2 args, got {:#?}", args))
}

fn index_from<T: TryFrom<i64>>(i: i64) -> Result<T, String> {
    TryFrom::try_from(i).or_else(|_| Err(format!("could not convert argument")))
}
