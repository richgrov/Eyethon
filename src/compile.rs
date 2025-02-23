use std::collections::HashMap;

use crate::parse::{Statement, StatementType};

pub struct ClassBytecode {
    name: Option<String>,
    extends: Option<String>,
}

pub enum CompileError {
    InvalidAnnotation { name: String },
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
            self.handle_statement(statement)?;
        }

        Ok(ClassBytecode {
            name: self.class_name,
            extends: self.extends,
        })
    }

    fn handle_statement(&mut self, statement: Statement) -> Result<(), CompileError> {
        match statement.ty {
            StatementType::Annotation { name, target, .. } => {
                let handler = self
                    .annotation_handlers
                    .get(&name)
                    .ok_or(CompileError::InvalidAnnotation { name })?;

                handler();
                self.handle_statement(*target)?;
            }
            _ => {
                println!("{:?}", statement);
            }
        }

        Ok(())
    }
}

pub fn compile(
    statements: Vec<Statement>,
    annotation_handlers: HashMap<String, AnnotationHandler>,
) -> Result<ClassBytecode, CompileError> {
    Compiler::new(annotation_handlers).emit_class_bytecode(statements)
}
