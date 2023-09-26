use std::io::{Stdin, Stdout, Write};

use crate::{evaluator::Evaluator, lexer::Lexer, parser::Parser};

pub fn start(stdin: Stdin, mut stdout: Stdout) {
    let mut evaluator = Evaluator::new();
    loop {
        write!(stdout, ">> ").expect("should have written prompt string >>");
        stdout.flush().expect("should have flushed stdout!");

        let mut input = String::new();

        if let Err(e) = stdin.read_line(&mut input) {
            writeln!(stdout, "Error: {e}").expect("should have written error message");
            return;
        }

        let lexer = Lexer::new(input.as_str());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("error parsing program");

        if parser.errors().len() != 0 {
            print_parse_errors(&stdout, parser.errors());
            continue;
        }

        let evaluated = evaluator.eval_program(program);

        writeln!(stdout, "{evaluated}").expect("parsed program should be written to stdout");
    }
}

fn print_parse_errors(mut stdout: &Stdout, errors: &Vec<String>) {
    writeln!(
        stdout,
        "
     /\\_/\\
    ( o.o )
    > ^ <
"
    )
    .unwrap();
    writeln!(stdout, "Oops! We ran into parser errors")
        .expect("error message info should be written to stdout");
    for error in errors {
        writeln!(stdout, "\t-> {error}").expect("error should be written to stdout");
    }
}
