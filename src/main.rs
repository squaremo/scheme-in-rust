// `extern` is used in the how-to; more modern is to just use
// `use`. See
//
//     https://doc.rust-lang.org/edition-guide/rust-2018/module-system/path-clarity.html
use rustyline;

const PROMPT: &str = "; ";

fn main() {
    // the `mut` means I can _and must_ reassign the value
    let mut reader = rustyline::Editor::<()>::new();
    loop {
        match reader.readline(PROMPT) {
            Ok(line) =>
                if line.trim() == "(exit)" {
                    println!("Cheerio!");
                    break;
                } else {
                    println!("{}",line); // println accepts a format string?
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
