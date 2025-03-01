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
        let class = compile::compile(ast, HashMap::new()).map_err(|e| Error::CompileError(e))?;

        self.interpreter
            .register_class(class, fallback_class_name.clone());

        self.interpreter
            .run(&fallback_class_name)
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
    use std::collections::HashMap;

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

    fn run_test(test: &Test) -> bool {
        let tokens = match tokenize::tokenize(&test.source) {
            Ok(t) => t,
            Err(e) => {
                eprintln!("tokenization for {} failed: {}", test.name, e);
                return false;
            }
        };

        let _ = match parse::parse(tokens) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("parse {} failed: {}", test.name, e);
                return false;
            }
        };

        true
    }

    #[test]
    fn parser_features() {
        let mut failed = 0;
        let mut total = 0;

        for test in get_tests("parser/features") {
            if !run_test(&test) {
                failed += 1;
            }
            total += 1;
        }

        if failed > 0 || total == 0 {
            panic!("{}/{} cases failed", failed, total);
        }
    }

    #[test]
    fn main() {
        let file = "<internal>";
        let script = std::fs::read_to_string("test.gd").unwrap();

        let tokens = match tokenize::tokenize(&script) {
            Ok(t) => t,
            Err(e) => {
                eprintln!("error: {}: {}", file, e);
                std::process::exit(1);
            }
        };

        let statements = parse::parse(tokens).unwrap();

        let mut annotation_handlers: HashMap<String, Box<dyn Fn()>> = HashMap::new();
        annotation_handlers.insert(
            "icon".to_owned(),
            Box::new(|| {
                println!("icon annotation");
            }),
        );

        let bytecode = compile::compile(statements, annotation_handlers).unwrap();
        println!("{:?}", bytecode);

        let mut vm = interpret::Interpreter::new();
        vm.register_class(bytecode, "main".to_owned());
        vm.run("main").unwrap();
    }
}
