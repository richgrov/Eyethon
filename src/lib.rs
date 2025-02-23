mod compile;
mod parse;
mod tokenize;
mod vm;

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::vm::*;
    use crate::*;

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
        annotation_handlers.insert("icon".to_owned(), Box::new(|| {}));

        let bytecode = compile::compile(statements, annotation_handlers);

        let mut vm = VM::new();
        vm.register_native("print", |args| {
            for list in args {
                print!("(");
                for f in &list.0 {
                    print!("{}", f);
                }
                println!(")");
            }

            Value(vec![])
        });

        match vm.run(&[]) {
            Ok(()) => {}
            Err(e) => {
                eprintln!("error: {:?}", e);
            }
        }
    }
}
