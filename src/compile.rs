use std::collections::HashMap;

use crate::parse::{Statement, StatementType};

#[derive(Debug)]
pub struct ClassBytecode {
    name: Option<String>,
    extends: Option<String>,
}

#[derive(Debug)]
pub enum CompileError {
    InvalidAnnotation { name: String },
    InvalidClassName { line: usize, column: usize },
    InvalidExtends { line: usize, column: usize },
}

pub type AnnotationHandler = Box<dyn Fn()>;

struct Compiler {
    annotation_handlers: HashMap<String, AnnotationHandler>,
    class_name: Option<String>,
    extends: Option<String>,
    non_class_name_statement_seen: bool,
}

impl Compiler {
    pub fn new(annotation_handlers: HashMap<String, AnnotationHandler>) -> Compiler {
        Compiler {
            annotation_handlers,

            class_name: None,
            extends: None,
            non_class_name_statement_seen: false,
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
            StatementType::ClassName(name) => {
                if self.non_class_name_statement_seen {
                    return Err(CompileError::InvalidClassName {
                        line: statement.line,
                        column: statement.column,
                    });
                }

                if let Some(_) = self.class_name {
                    return Err(CompileError::InvalidClassName {
                        line: statement.line,
                        column: statement.column,
                    });
                }

                let _ = self.class_name.insert(name);
            }
            StatementType::Extends(name) => {
                if self.non_class_name_statement_seen {
                    return Err(CompileError::InvalidExtends {
                        line: statement.line,
                        column: statement.column,
                    });
                }

                if let Some(_) = self.extends {
                    return Err(CompileError::InvalidExtends {
                        line: statement.line,
                        column: statement.column,
                    });
                }

                let _ = self.extends.insert(name);
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
