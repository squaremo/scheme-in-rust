mod instructions;
mod vm;
mod values;
mod read;
mod translate;
mod frame;

use std::env;
use std::fs;

use crate::instructions::Opcode;
use crate::values::{Value,ValueRef};
use crate::vm::VM;

const PROMPT: &str = ";#> ";

fn main() {
    let args: Vec<String> = env::args().collect();
    // crude but effective
    assert_eq!(args.len(), 2, "usage: cargo run <file>");

    let filename = &args[1];
    let contents = fs::read_to_string(filename)
        .expect("could not read file");

    match read::parse_line(&contents) {
        Ok((_, expr)) => {
            eprintln!("Read file");
            match translate::translate(expr) {
                Ok(program) => {
                    eprintln!("Translated program");
                    let mut code = program.code;
                    code.push(Opcode::FINISH);
                    let mut vm = VM::new(program.constants, &mut code);
                    vm.trace = true;
                    let val = vm.run_until_halt();
                    eprintln!("{:?}", val);
                },
                Err(e) => println!("Compilation error: {}", e)
            }
        },
        Err(e) => println!("Parse error: {}", e)
    }
}
