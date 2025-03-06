use std::collections::HashMap;
use std::fmt;

use crate::parse::{Expression, ExpressionType, Statement, StatementType, VariableType};

#[derive(Debug)]
pub struct ClassBytecode {
    pub name: String,
    pub extends: Option<String>,
    pub bytecode: Vec<Instruction>,
    pub functions: HashMap<String, usize>,
}

#[derive(Debug)]
pub enum Instruction {
    Duplicate(usize),
    PushInt(i64),
    PushFloat(f64),
    PushString(String),
    PushGlobal(String),
    Call { n_args: usize },
    Store,
    Return,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::Duplicate(n) => write!(f, "dup {}", n),
            Instruction::PushInt(i) => write!(f, "pushi {}", i),
            Instruction::PushFloat(fl) => write!(f, "pushf {}", fl),
            Instruction::PushString(s) => write!(f, "pushs \"{}\"", s),
            Instruction::PushGlobal(s) => write!(f, "pushg \"{}\"", s),
            Instruction::Call { n_args } => write!(f, "call {}", n_args),
            Instruction::Store => write!(f, "store"),
            Instruction::Return => write!(f, "return"),
        }
    }
}

#[derive(Debug)]
pub enum CompileError {
    InvalidAnnotation {
        name: String,
    },
    InvalidClassName {
        line: usize,
        column: usize,
    },
    InvalidExtends {
        line: usize,
        column: usize,
    },
    NotAllowedAtTopLevel(Statement),
    NotImplemented {
        line: usize,
        column: usize,
        message: String,
    },
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CompileError::InvalidAnnotation { name } => write!(f, "unknown annotation: {}", name),
            CompileError::InvalidClassName { line, column } => {
                write!(f, "{}:{}: class_name not allowed here", line, column)
            }
            CompileError::InvalidExtends { line, column } => {
                write!(f, "{}:{}: extends not allowed here", line, column)
            }
            CompileError::NotAllowedAtTopLevel(statement) => {
                write!(
                    f,
                    "{}:{}: {:?} not allowed here",
                    statement.line, statement.column, statement.ty
                )
            }
            CompileError::NotImplemented {
                line,
                column,
                message,
            } => write!(f, "{}:{}: not implemented: {}", line, column, message),
        }
    }
}

pub type AnnotationHandler = Box<dyn Fn()>;

struct VariableInfo {
    index: usize,
    konst: bool,
    ty: VariableType,
}

struct Compiler {
    annotation_handlers: HashMap<String, AnnotationHandler>,
    class_name: Option<String>,
    fallback_class_name: String,
    extends: Option<String>,
    non_class_name_statement_seen: bool,
    function_entry_points: HashMap<String, usize>,
    function_scope_stack: Vec<()>,
}

impl Compiler {
    pub fn new(
        annotation_handlers: HashMap<String, AnnotationHandler>,
        fallback_class_name: String,
    ) -> Compiler {
        Compiler {
            annotation_handlers,
            fallback_class_name,

            class_name: None,
            extends: None,
            non_class_name_statement_seen: false,
            function_entry_points: HashMap::new(),
            function_scope_stack: Vec::with_capacity(4),
        }
    }

    pub fn emit_class_bytecode(
        mut self,
        statements: Vec<Statement>,
    ) -> Result<ClassBytecode, CompileError> {
        let mut instructions = Vec::new();
        let mut functions = Vec::new();

        for statement in statements {
            match statement.ty {
                StatementType::ClassName {
                    class_name,
                    extends,
                } => {
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

                    let _ = self.class_name.insert(class_name);

                    if let Some(ext) = extends {
                        if let Some(_) = self.extends {
                            return Err(CompileError::InvalidClassName {
                                line: statement.line,
                                column: statement.column,
                            });
                        }

                        let _ = self.extends.insert(ext);
                    }
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
                StatementType::Expression(Expression {
                    ty: ExpressionType::Function(func),
                    ..
                }) => {
                    functions.push(func);
                }
                StatementType::Var {
                    konst,
                    static_,
                    identifier,
                    ty,
                    value,
                    getter,
                    setter,
                } => {
                    let Some(val) = value else {
                        continue;
                    };

                    instructions.push(Instruction::Duplicate(0));
                    instructions.push(Instruction::PushString(identifier));

                    let mut expression_instructions = Vec::new();
                    Self::evaluate_expression(&mut expression_instructions, val)?;
                    instructions.extend(expression_instructions);

                    instructions.push(Instruction::Store);
                }
                StatementType::Pass
                | StatementType::If { .. }
                | StatementType::Return(_)
                | StatementType::Expression(_)
                | StatementType::For { .. }
                | StatementType::Match { .. }
                | StatementType::While { .. }
                | StatementType::Assert { .. } => {
                    return Err(CompileError::NotAllowedAtTopLevel(statement))
                }
                other => {
                    return Err(CompileError::NotImplemented {
                        line: statement.line,
                        column: statement.column,
                        message: format!("{:?}", other),
                    })
                }
            }
        }

        instructions.push(Instruction::Return);

        let mut function_entry_points = HashMap::with_capacity(functions.len());
        for function in functions {
            function_entry_points.insert(function.name, instructions.len());
            for statement in function.statements {
                self.handle_statement(&mut instructions, statement)?;
            }
            instructions.push(Instruction::Return);
        }

        Ok(ClassBytecode {
            name: self.class_name.unwrap_or(self.fallback_class_name),
            extends: self.extends,
            bytecode: instructions,
            functions: function_entry_points,
        })
    }

    fn handle_statement(
        &mut self,
        instructions: &mut Vec<Instruction>,
        statement: Statement,
    ) -> Result<(), CompileError> {
        match statement.ty {
            StatementType::Annotation { name, target, .. } => {
                let handler = self
                    .annotation_handlers
                    .get(&name)
                    .ok_or(CompileError::InvalidAnnotation { name })?;

                handler();
                self.handle_statement(instructions, *target)?;
            }
            StatementType::Expression(expr) => {
                Self::evaluate_expression(instructions, expr)?;
            }
            other => {
                return Err(CompileError::NotImplemented {
                    line: statement.line,
                    column: statement.column,
                    message: format!("{:?} not implemented yet", other),
                })
            }
        }

        Ok(())
    }

    fn evaluate_expression(
        instructions: &mut Vec<Instruction>,
        expression: Expression,
    ) -> Result<(), CompileError> {
        match expression.ty {
            ExpressionType::String(str) => {
                instructions.push(Instruction::PushString(str));
            }
            ExpressionType::Integer(i) => {
                instructions.push(Instruction::PushInt(i));
            }
            ExpressionType::Float(f) => {
                instructions.push(Instruction::PushFloat(f));
            }
            ExpressionType::FunctionCall(expr) => {
                let n_args = expr.args.len();
                for arg in expr.args {
                    Self::evaluate_expression(instructions, arg)?;
                }

                Self::evaluate_expression(instructions, *expr.callee)?;

                instructions.push(Instruction::Call { n_args });
            }
            other => {
                return Err(CompileError::NotImplemented {
                    line: expression.line,
                    column: expression.column,
                    message: format!("{:?} not implemented yet", other),
                })
            }
        }

        Ok(())
    }
}

pub fn compile(
    statements: Vec<Statement>,
    annotation_handlers: HashMap<String, AnnotationHandler>,
    fallback_class_name: String,
) -> Result<ClassBytecode, CompileError> {
    Compiler::new(annotation_handlers, fallback_class_name).emit_class_bytecode(statements)
}
