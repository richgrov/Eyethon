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

    struct Test {
        name: String,
        source: String,
        output: Option<String>,
    }

    fn get_tests(category: &str) -> impl Iterator<Item = Test> {
        std::fs::read_dir("tests/".to_owned() + category)
            .unwrap()
            .filter_map(|file| file.ok().map(|file| file.path()))
            .filter(|path| match path.extension() {
                Some(ext) if ext == "gd" => true,
                _ => false,
            })
            .map(|path| {
                let file_name = path
                    .file_name()
                    .unwrap()
                    .to_os_string()
                    .into_string()
                    .unwrap();

                let output = if file_name.ends_with(".notest.gd") {
                    None
                } else {
                    Some(
                        std::fs::read_to_string(path.with_extension("out"))
                            .expect(&format!("failed to read test output for {:?}", path)),
                    )
                };

                Test {
                    name: path.clone().into_os_string().into_string().unwrap(),
                    source: std::fs::read_to_string(&path).expect("failed to read test source"),
                    output,
                }
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
            "print",
            Value::NativeFunction(Rc::new(move |args| {
                let mut output = out.borrow_mut();

                for arg in args {
                    output.push_str(&format!("{}\n", arg));
                }

                Value::Null
            })),
        );

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

        if let Some(expected) = &test.output {
            match interpreter.call_method(object, "test") {
                Ok(Some(obj)) => {
                    eprintln!("{} returned a value: {}", test.name, obj);
                    return false;
                }
                Ok(None) => {}
                Err(e) => {
                    eprintln!("call {} failed: {}", test.name, e);
                    return false;
                }
            }

            let actual = output.borrow();
            if actual.ne(expected) {
                eprintln!("-- expected output for {} --", test.name);
                eprintln!("{}", expected);
                eprintln!("-- actual output --");
                eprintln!("{}", actual);
                eprintln!("-- end --");
                return false;
            }
        }

        true
    }

    fn run_tests(tests: impl Iterator<Item = Test>) {
        let mut failed_tokenized = 0;
        let mut failed_parsed = 0;
        let mut failed_compiled = 0;
        let mut failed_runs = 0;
        let mut succeeded = 0;

        let classes = tests
            .filter_map(|test| match tokenize::tokenize(&test.source) {
                Ok(tokens) => Some((test, tokens)),
                Err(error) => {
                    eprintln!("failed to tokenize {}: {}", test.name, error);
                    failed_tokenized += 1;
                    None
                }
            })
            .filter_map(|(test, tokens)| match parse::parse(tokens) {
                Ok(statements) => Some((test, statements)),
                Err(error) => {
                    eprintln!("failed to parse {}: {}", test.name, error);
                    failed_parsed += 1;
                    None
                }
            })
            .filter_map(|(test, statements)| {
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
                succeeded += 1;
            } else {
                failed_runs += 1;
            }
        }

        let total = failed_tokenized + failed_parsed + failed_compiled + failed_runs + succeeded;
        println!("{}/{} failed to tokenize", failed_tokenized, total);
        println!("{}/{} failed to parse", failed_parsed, total);
        println!("{}/{} failed to compile", failed_compiled, total);
        println!("{}/{} failed to run", failed_runs, total);
        println!("{}/{} succeeded", succeeded, total);

        if succeeded != total {
            panic!();
        }
    }

    #[test]
    fn parser_features() {
        run_tests(get_tests("parser/features"));
    }

    #[test]
    fn runtime_features() {
        run_tests(get_tests("parser/features"));
    }
}
