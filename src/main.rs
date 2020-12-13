// `extern` is used in the how-to; more modern is to just `use`.
use rustyline;

const PROMPT: &str = "; ";

fn main() {
    // the `mut` means I can _and must_ reassign the value
    let mut done = false;
    let mut reader = rustyline::Editor::<()>::new();
    while !done {
        match reader.readline(PROMPT) {
            Ok(line) =>
                if line.trim() == "(exit)" {
                    println!("Cheerio!");
                    done = true;
                } else {
                    println!("{}",line); // println accepts a format string?
                },
            Err(e) => {
                use rustyline::error::ReadlineError::*;
                match e {
                    Eof | Interrupted => done = true,
                    _ => println!("Couldn't readline: {}", e)
                }
            }
        }
    }
}
