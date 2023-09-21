use std::io;

use crate::repl::start;

pub mod lexer;
pub mod repl;
pub mod token;

fn main() {
    println!("Hello! This is the Monkey Programming language!");
    println!("Feel free to type in the code");
    start(io::stdin(), io::stdout());
}
