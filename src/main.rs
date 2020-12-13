// `extern` is used in the how-to; more modern is to just `use`.
use rustyline;

fn main() {
    // the `mut` means I can _and must_ reassign the value
    let mut done = false;
    let mut reader = rustyline::Editor::<()>::new();
    while !done {
        match reader.readline(">> ") {
            Ok(line) =>
                if line == "(exit)" {
                    done = true;
                } else {
                    println!("{}",line); // println accepts a format string?
                },
            Err(rustyline::error::ReadlineError::Eof) => done = true,
            Err(rustyline::error::ReadlineError::Interrupted) => {
                println!("Cheerio then");
                done = true
            },
            Err(e) => println!("Couldn't readline: {}", e),
        }
    }
}
