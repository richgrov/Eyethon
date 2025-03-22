use core::fmt;
use std::collections::HashMap;
use std::rc::Rc;

use compile::Compiler;
use inference::Inference;
use interpret::Interpreter;

pub use interpret::Value;

mod compile;
mod inference;
mod interpret;
mod parse;
mod tokenize;

pub struct Vm {
    inference: Rc<Inference>,
    compiler: Compiler,
    interpreter: Interpreter,
}

impl Vm {
    pub fn init() {
        Inference::init().unwrap();
    }

    pub fn new() -> Vm {
        let inference = Rc::new(Inference::new().unwrap());
        Vm {
            inference: inference.clone(),
            compiler: Compiler::new(HashMap::new(), inference),
            interpreter: Interpreter::new(),
        }
    }

    pub fn register_class(&mut self, file_name: String, source: &str) -> Result<(), Error> {
        let tokens = tokenize::tokenize(source).map_err(|e| Error::SyntaxError(e))?;
        let ast = parse::parse(tokens).map_err(|e| Error::ParseError(e))?;
        let class = self
            .compiler
            .emit_class_bytecode(ast)
            .map_err(|e| Error::CompileError(e))?;

        println!("Bytecode:");
        for (i, instr) in class.bytecode.iter().enumerate() {
            println!("{:04} {}", i, instr);
        }
        println!("\n");

        self.interpreter.register_file(file_name, class);
        Ok(())
    }

    pub fn when(
        &mut self,
        phrase: &str,
        func: impl Fn(Vec<Value>) -> Value + 'static,
    ) -> Result<(), Error> {
        let func_id = self.interpreter.add_builtin_function(Box::new(func));
        self.compiler
            .register_native_func(phrase, func_id)
            .map_err(|e| Error::InferenceError(e))?;
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
    InferenceError(inference::InferenceError),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::SyntaxError(ref e) => e.fmt(f),
            Error::ParseError(ref e) => e.fmt(f),
            Error::CompileError(ref e) => e.fmt(f),
            Error::RuntimeError(ref e) => e.fmt(f),
            Error::InferenceError(ref e) => e.fmt(f),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;

    use crate::*;

    fn run(source: &str, expected_output: &str) {
        let mut vm = Vm::new();

        let output = Rc::new(RefCell::new(String::new()));
        let out = output.clone();
        if let Err(e) = vm.when("printing to the terminal", move |args| {
            for arg in args {
                out.borrow_mut().push_str(&format!("{}\n", arg));
            }
            Value::Null
        }) {
            panic!("{}", e);
        }

        if let Err(e) = vm.register_class("test".to_owned(), source) {
            panic!("{}", e);
        };

        if let Err(e) = vm.exec("test", 0) {
            panic!("{}", e);
        }

        assert_eq!(*output.borrow(), expected_output);
    }

    #[test]
    fn test() {
        run(
            r#"
when the program starts,
    greet the user as "Bob" with "hello"

when greeting a user (name) with (message),
    print (message) to the terminal
    print (name) to terminal
        "#,
            "hello\nBob\n",
        );
    }
}
