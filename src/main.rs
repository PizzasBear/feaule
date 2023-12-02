pub mod code;
pub mod lexer;
pub mod parser;

use std::{collections::HashSet, io, path::Path};

pub use lexer::lex;

use code::CodeBufs;

fn main() -> io::Result<()> {
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    let file = Path::new("in.txt");

    let mut bufs = CodeBufs::new();
    bufs.add_file(file.to_path_buf(), std::fs::read_to_string(file)?);

    let (tokens, errors) = lex(0, bufs.buf(0).code(), &mut HashSet::new());
    // `a {hello} big` => ("My name is", "max", "and")
    println!("tokens = {tokens:#?}");
    println!();
    if !errors.is_empty() {
        eprintln!("Errors:");
        for error in errors {
            eprintln!("{error}");
            for span in error.spans() {
                span.show(&bufs, io::stderr().lock())?;
            }
            eprintln!();
        }
    }

    Ok(())
}
