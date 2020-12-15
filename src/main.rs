// `extern` is used in the how-to; more modern is to just use
// `use`. See
//
//     https://doc.rust-lang.org/edition-guide/rust-2018/module-system/path-clarity.html
use rustyline;

// look in parser.rs, value.rs, etc.
mod parser;
mod value;
mod eval;
mod env;
mod prims;

const PROMPT: &str = "; ";

fn main() {
    let mut reader = rustyline::Editor::<()>::new();
    let mut global = env::Env::top_env();
    prims::into_env(&mut global);
    let globalEnv = env::EnvRef::new(global);
    loop {
        match reader.readline(PROMPT) {
            Ok(line) =>
                match parser::parse_line(&line) {
                    Ok((_, expr)) => {
                         // cloning the env here will increment it,
                         // then it'll be decremented when dropped at
                         // the end of the block.
                        match eval::eval(&expr, &globalEnv) {
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
