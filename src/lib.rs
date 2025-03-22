use core::fmt;
use std::collections::HashMap;
use std::rc::Rc;

use inference::Inference;
use interpret::{Interpreter, Value};

mod compile;
mod inference;
mod interpret;
mod parse;
mod tokenize;

pub struct Vm {
    inference: Rc<Inference>,
    interpreter: Interpreter,
}

impl Vm {
    pub fn new() -> Vm {
        Inference::init().unwrap();

        Vm {
            inference: Rc::new(Inference::new().unwrap()),
            interpreter: Interpreter::new(),
        }
    }

    pub fn register_class(&mut self, file_name: String, source: &str) -> Result<(), Error> {
        let tokens = tokenize::tokenize(source).map_err(|e| Error::SyntaxError(e))?;
        let ast = parse::parse(tokens).map_err(|e| Error::ParseError(e))?;
        let class = compile::compile(ast, HashMap::new(), self.inference.clone())
            .map_err(|e| Error::CompileError(e))?;

        for (i, instr) in class.bytecode.iter().enumerate() {
            println!("{:04} {}", i, instr);
        }

        self.interpreter.register_file(file_name, class);
        Ok(())
    }

    pub fn exec(&mut self, file_name: &str, id: usize) -> Result<Option<Value>, Error> {
        self.interpreter
            .exec(file_name, id)
            .map_err(|e| Error::RuntimeError(e))
    }
}

#[derive(Debug)]
pub enum Error {
    SyntaxError(tokenize::TokenizerError),
    ParseError(parse::ParseError),
    CompileError(compile::CompileError),
    RuntimeError(interpret::RuntimeError),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::SyntaxError(ref e) => e.fmt(f),
            Error::ParseError(ref e) => e.fmt(f),
            Error::CompileError(ref e) => e.fmt(f),
            Error::RuntimeError(ref e) => e.fmt(f),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::*;

    fn run(source: &str, expected_output: &str) {
        let mut vm = Vm::new();

        if let Err(e) = vm.register_class("test".to_owned(), source) {
            panic!("{}", e);
        };

        if let Err(e) = vm.exec("test", 0) {
            panic!("{}", e);
        }
    }

    #[test]
    fn test() {
        run(
            "
when the program starts,
    greet the user

when greeting the user,
    launch the app program
",
            "Hello, world!",
        );
    }
}
