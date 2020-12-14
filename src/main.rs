// `extern` is used in the how-to; more modern is to just use
// `use`. See
//
//     https://doc.rust-lang.org/edition-guide/rust-2018/module-system/path-clarity.html
use rustyline;

// look in parser.rs, value.rs, etc.
mod parser;
mod value;
mod eval;

const PROMPT: &str = "; ";

fn main() {
    let mut reader = rustyline::Editor::<()>::new();
    loop {
        match reader.readline(PROMPT) {
            Ok(line) =>
                match parser::parse_line(&line) {
                    Ok((_, expr)) => {
                        match eval::eval(expr) {
                            Ok(val) => println!("{:#?}", val),
                            Err(e) => println!("Error: {}", e),
                        }
                    },
                    Err(e) => println!("Error: {}", e)
                },
            Err(e) => {
                use rustyline::error::ReadlineError::*;
                match e {
                    Eof | Interrupted => break,
                    _ => println!("Couldn't readline: {}", e)
                }
            }
        }
    }
}
