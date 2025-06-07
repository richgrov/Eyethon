use std::collections::HashMap;

use interpret::Interpreter;

mod compile;
mod interpret;
mod parse;
mod tokenize;

pub struct Vm {
    interpreter: Interpreter,
}

impl Vm {
    pub fn new() -> Vm {
        Vm {
            interpreter: Interpreter::new(),
        }
    }

    pub fn run(&mut self, source: &str, fallback_class_name: String) -> Result<(), Error> {
        let tokens = tokenize::tokenize(source).map_err(|e| Error::SyntaxError(e))?;
        let ast = parse::parse(tokens).map_err(|e| Error::ParseError(e))?;
        let class = compile::compile(ast, HashMap::new(), fallback_class_name.clone())
            .map_err(|e| Error::CompileError(e))?;

        self.interpreter.register_class(class).unwrap();

        self.interpreter
            .new_instance(&fallback_class_name)
            .map_err(|e| Error::RuntimeError(e))?;

        Ok(())
    }
}

pub enum Error {
    SyntaxError(tokenize::TokenizerError),
    ParseError(parse::ParseError),
    CompileError(compile::CompileError),
    RuntimeError(interpret::RuntimeError),
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::rc::Rc;

    use interpret::Value;

    use crate::*;

    #[derive(PartialEq, Eq, Ord)]
    struct Test {
        name: String,
        source: String,
    }

    impl PartialOrd for Test {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            self.name.partial_cmp(&other.name)
        }
    }

    fn get_tests() -> impl Iterator<Item = Test> {
        std::fs::read_dir("tests/")
            .unwrap()
            .filter_map(|file| file.ok().map(|file| file.path()))
            .map(|path| Test {
                name: path.clone().into_os_string().into_string().unwrap(),
                source: std::fs::read_to_string(&path)
                    .expect(&format!("failed to read {:?}", path)),
            })
    }

    fn run_test(test: &Test, class: compile::ClassBytecode) -> bool {
        println!("{} bytecode:", test.name);
        for (i, instr) in class.bytecode.iter().enumerate() {
            println!("{:04} {}", i, instr);
        }
        println!("{:?}", class.functions);

        let class_name = class.name.clone();

        let mut interpreter = interpret::Interpreter::new();

        let output = Rc::new(RefCell::new("GDTEST_OK\n".to_owned()));
        let out = output.clone();

        interpreter.set_global(
            "is_equal_approx",
            Value::NativeFunction(Rc::new(move |args| {
                if args.len() != 2 {
                    return Value::Bool(false);
                }

                let a = match &args[0] {
                    Value::Float(f) => *f,
                    Value::Integer(i) => *i as f64,
                    _ => return Value::Bool(false),
                };

                let b = match &args[1] {
                    Value::Float(f) => *f,
                    Value::Integer(i) => *i as f64,
                    _ => return Value::Bool(false),
                };

                Value::Bool(a == b)
            })),
        );

        interpreter.set_global(
            "print",
            Value::NativeFunction(Rc::new(move |args| {
                let mut output = out.borrow_mut();

                for arg in args {
                    output.push_str(&format!("{}", arg));
                }
                output.push('\n');

                Value::Null
            })),
        );

        interpreter.set_global("Ellipsis", Value::String("Ellipsis".to_owned()));

        if let Err(e) = interpreter.register_class(class) {
            eprintln!("register class failed: {}", e);
            return false;
        }

        let object = match interpreter.new_instance(&class_name) {
            Ok(o) => o,
            Err(e) => {
                eprintln!("instantiation of {} failed: {}", test.name, e);
                return false;
            }
        };

        true
    }

    fn run_tests(tests: impl Iterator<Item = Test>) {
        let mut total_tokenized = 0;
        let mut failed_tokenized = 0;
        let mut total_parsed = 0;
        let mut failed_parsed = 0;
        let mut total_compiled = 0;
        let mut failed_compiled = 0;
        let mut total_runs = 0;
        let mut failed_runs = 0;

        let classes = tests
            .filter_map(|test| {
                total_tokenized += 1;
                match tokenize::tokenize(&test.source) {
                    Ok(tokens) => Some((test, tokens)),
                    Err(error) => {
                        eprintln!("failed to tokenize {}: {}", test.name, error);
                        failed_tokenized += 1;
                        None
                    }
                }
            })
            .filter_map(|(test, tokens)| {
                total_parsed += 1;
                match parse::parse(tokens) {
                    Ok(statements) => Some((test, statements)),
                    Err(error) => {
                        eprintln!("failed to parse {}: {}", test.name, error);
                        failed_parsed += 1;
                        None
                    }
                }
            })
            .filter_map(|(test, statements)| {
                total_compiled += 1;
                match compile::compile(statements, HashMap::new(), "Test".to_owned()) {
                    Ok(class) => Some((test, class)),
                    Err(error) => {
                        eprintln!("failed to compile {}: {}", test.name, error);
                        failed_compiled += 1;
                        None
                    }
                }
            });

        for (test, class) in classes {
            if run_test(&test, class) {
                total_runs += 1;
            } else {
                failed_runs += 1;
            }
        }

        println!(
            "{}/{} failed to tokenize",
            failed_tokenized, total_tokenized
        );
        println!("{}/{} failed to parse", failed_parsed, total_parsed);
        println!("{}/{} failed to compile", failed_compiled, total_compiled);
        println!("{}/{} failed to run", failed_runs, total_runs);

        if failed_tokenized != 0 || failed_parsed != 0 || failed_compiled != 0 || failed_runs != 0 {
            panic!("some tests failed");
        }
    }

    #[test]
    fn parser_features() {
        let mut tests: Vec<_> = get_tests().collect();
        tests.sort();
        run_tests(tests.into_iter());
    }
}
