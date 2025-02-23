use std::collections::HashMap;

use crate::parse::{Statement, StatementType};

pub struct ClassBytecode {
    name: Option<String>,
    extends: Option<String>,
}

pub enum CompileError {
    InvalidAnnotation,
}

pub type AnnotationHandler = Box<dyn Fn()>;

struct Compiler {
    annotation_handlers: HashMap<String, AnnotationHandler>,
    class_name: Option<String>,
    extends: Option<String>,
}

impl Compiler {
    pub fn new(annotation_handlers: HashMap<String, AnnotationHandler>) -> Compiler {
        Compiler {
            annotation_handlers,

            class_name: None,
            extends: None,
        }
    }

    pub fn emit_class_bytecode(
        mut self,
        statements: Vec<Statement>,
    ) -> Result<ClassBytecode, CompileError> {
        for statement in statements {
            self.handle_statement(statement);
        }

        Ok(ClassBytecode {
            name: self.class_name,
            extends: self.extends,
        })
    }

    fn handle_statement(&mut self, statement: Statement) {
        println!("{:?}", statement);
    }
}

pub fn compile(
    statements: Vec<Statement>,
    annotation_handlers: HashMap<String, AnnotationHandler>,
) -> Result<ClassBytecode, CompileError> {
    Compiler::new(annotation_handlers).emit_class_bytecode(statements)
}
