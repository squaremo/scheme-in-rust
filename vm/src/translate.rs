// Implements a translation from the intermediate symbolic form, to
// opcodes.

use std::ops::Deref;

use crate::read::Expr;
use crate::instructions::Opcode;
use crate::values::ValueRef;

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
    vec![]
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
        "int_1" => Ok(Opcode::INT_1),
        "push-value" => Ok(Opcode::PUSH_VALUE),
        "pop-arg1" => Ok(Opcode::POP_ARG1),
        "call2_plus" => Ok(Opcode::CALL2_PLUS),
        _ => Err(format!("Unrecognised symbol {}", sym))
    }
}
