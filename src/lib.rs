pub mod parse;
pub mod tokenize;
pub mod vm;

#[cfg(test)]
mod tests {
    use crate::vm::*;
    use crate::*;

    #[test]
    fn main() {
        let file = "<internal>";
        let script = "var a = print(0.1234)";

        let tokens = match tokenize::tokenize(script) {
            Ok(t) => t,
            Err(e) => {
                eprintln!("error: {}: {}", file, e);
                std::process::exit(1);
            }
        };

        let statements = match parse::parse(tokens) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("error: {}: {}", file, e);
                std::process::exit(1);
            }
        };

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

        match vm.run(&statements) {
            Ok(()) => {}
            Err(e) => {
                eprintln!("error: {:?}", e);
            }
        }
    }
}
