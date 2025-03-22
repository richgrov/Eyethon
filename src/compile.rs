use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use ndarray_linalg::Norm;

use crate::inference::{Embedding, Inference, InferenceError};
use crate::parse::{Expression, ExpressionType, Statement, StatementType, VariableType};

#[derive(Debug)]
pub struct ClassBytecode {
    pub extends: Option<String>,
    pub bytecode: Vec<Instruction>,
    pub handler_addresses: Vec<usize>,
    pub member_variables: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Duplicate(usize),
    PushNull,
    PushBool(bool),
    PushInt(i64),
    PushFloat(f64),
    PushString(String),
    PushGlobal(String),
    PushMember(usize),
    Pop,
    MakeArray { len: usize },
    Do { action: i32, n_args: usize },
    Store,
    Return,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::Duplicate(n) => write!(f, "dup {}", n),
            Instruction::PushNull => write!(f, "pushn"),
            Instruction::PushBool(b) => write!(f, "pushb {}", b),
            Instruction::PushInt(i) => write!(f, "pushi {}", i),
            Instruction::PushFloat(fl) => write!(f, "pushf {}", fl),
            Instruction::PushString(s) => write!(f, "pushs \"{}\"", s),
            Instruction::PushGlobal(s) => write!(f, "pushg \"{}\"", s),
            Instruction::PushMember(n) => write!(f, "pushmem {}", n),
            Instruction::Pop => write!(f, "pop"),
            Instruction::MakeArray { len } => write!(f, "mkarray {}", len),
            Instruction::Do { action, n_args } => write!(f, "do {}, {}", action, n_args),
            Instruction::Store => write!(f, "store"),
            Instruction::Return => write!(f, "ret"),
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
    UnknownAction {
        phrase: String,
        suggestion: Option<String>,
        line: usize,
        column: usize,
    },
    NotAllowedAtTopLevel(Statement),
    NotImplemented {
        line: usize,
        column: usize,
        message: String,
    },
    OnlyAllowedAtTopLevel,
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
            CompileError::UnknownAction {
                phrase,
                suggestion,
                line,
                column,
            } => {
                if let Some(s) = suggestion {
                    write!(f, "{}:{}: don't know how to '{}'. Note: found 'when {}', but it wasn't similar enough to the command", line, column, phrase, s)
                } else {
                    write!(f, "{}:{}: don't know how to '{}'", line, column, phrase)
                }
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
            CompileError::OnlyAllowedAtTopLevel => {
                write!(f, "not allowed here")
            }
        }
    }
}

pub type AnnotationHandler = Box<dyn Fn()>;

struct VariableInfo {
    index: usize,
    konst: bool,
    ty: VariableType,
}

struct FunctionScope {
    num_upvalues: usize,
    locals: Vec<String>,
}

#[derive(Debug, Clone)]
struct Handler {
    phrase: String,
    address: usize,
}

pub struct Compiler {
    annotation_handlers: HashMap<String, AnnotationHandler>,
    class_name: Option<String>,
    extends: Option<String>,
    non_class_name_statement_seen: bool,
    member_variables: Vec<String>,
    when_handlers: Vec<Handler>,
    function_scope_stack: Vec<FunctionScope>,
    inference: Rc<Inference>,
    handlers: HashMap<usize, i32>,
}

impl Compiler {
    const MINIMUM_SIMILARITY: f32 = 0.8;
    const SIMILARITY_SUGGESTION_THRESHOLD: f32 = 0.5;

    pub fn new(
        annotation_handlers: HashMap<String, AnnotationHandler>,
        inference: Rc<Inference>,
    ) -> Compiler {
        Compiler {
            annotation_handlers,

            class_name: None,
            extends: None,
            non_class_name_statement_seen: false,
            member_variables: Vec::new(),
            when_handlers: Vec::with_capacity(8),
            function_scope_stack: Vec::with_capacity(4),
            inference,
            handlers: HashMap::new(),
        }
    }

    pub fn emit_class_bytecode(
        &mut self,
        statements: Vec<Statement>,
    ) -> Result<ClassBytecode, CompileError> {
        let mut instructions = Vec::new();
        let mut handlers = Vec::with_capacity(8);

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
                StatementType::Var {
                    konst,
                    static_,
                    identifier,
                    ty,
                    value,
                    getter,
                    setter,
                } => {
                    self.member_variables.push(identifier.clone());

                    let Some(val) = value else {
                        continue;
                    };

                    instructions.push(Instruction::Duplicate(0));
                    instructions.push(Instruction::PushString(identifier));

                    let mut expression_instructions = Vec::new();
                    self.evaluate_expression(&mut expression_instructions, val)?;
                    instructions.extend(expression_instructions);

                    instructions.push(Instruction::Store);
                }
                StatementType::When {
                    phrase,
                    parameters,
                    statements,
                } => {
                    let embedding = self.inference.embed(&phrase).unwrap();
                    let embed_id = self.inference.store(embedding);
                    self.when_handlers.push(Handler { phrase, address: 0 });
                    handlers.push((parameters, statements));
                    self.handlers
                        .insert(embed_id, self.when_handlers.len() as i32 - 1);
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

        for (i, (parameters, statements)) in handlers.into_iter().enumerate() {
            self.when_handlers[i].address = instructions.len();
            let num_args = parameters.len();

            self.function_scope_stack.push(FunctionScope {
                num_upvalues: 0,
                locals: parameters.into_iter().map(|(_, name)| name).collect(),
            });

            for statement in statements {
                self.handle_statement(&mut instructions, statement)?;
            }

            let func_scope = self.function_scope_stack.pop().unwrap();
            let num_locals = func_scope.locals.len() - num_args;

            for _ in 0..num_locals {
                instructions.push(Instruction::Pop);
            }

            instructions.push(Instruction::Return);
        }

        Ok(ClassBytecode {
            extends: self.extends.take(),
            bytecode: instructions,
            handler_addresses: self
                .when_handlers
                .drain(..)
                .map(|handler| handler.address)
                .collect(),
            member_variables: self.member_variables.drain(..).collect(),
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
            StatementType::Action { phrase, arguments } => {
                let n_args = arguments.len();
                for (_, arg) in arguments {
                    self.evaluate_expression(instructions, arg)?;
                }

                let embedding = self.inference.embed(&phrase).unwrap();
                let lookup_res = self.inference.lookup(&embedding);

                let embed_id = match lookup_res {
                    Some((embed_id, similarity)) if similarity > Self::MINIMUM_SIMILARITY => {
                        embed_id
                    }
                    Some((handler_idx, similarity)) => {
                        let suggestion = if similarity >= Self::SIMILARITY_SUGGESTION_THRESHOLD {
                            Some(self.when_handlers[handler_idx as usize].phrase.clone())
                        } else {
                            None
                        };

                        return Err(CompileError::UnknownAction {
                            phrase,
                            suggestion,
                            line: statement.line,
                            column: statement.column,
                        });
                    }
                    None => {
                        return Err(CompileError::UnknownAction {
                            phrase,
                            suggestion: None,
                            line: statement.line,
                            column: statement.column,
                        })
                    }
                };

                instructions.push(Instruction::Do {
                    action: *self.handlers.get(&embed_id).unwrap(),
                    n_args,
                });

                instructions.push(Instruction::Pop);
            }
            StatementType::Expression(expr) => {
                self.evaluate_expression(instructions, expr)?;
                instructions.push(Instruction::Pop);
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
                if static_ || getter.is_some() || setter.is_some() {
                    return Err(CompileError::OnlyAllowedAtTopLevel);
                }

                let func_scope = self.function_scope_stack.last_mut().unwrap();
                func_scope.locals.push(identifier);
                if let Some(val) = value {
                    self.evaluate_expression(instructions, val)?;
                } else {
                    instructions.push(Instruction::PushNull);
                }
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
        &mut self,
        instructions: &mut Vec<Instruction>,
        expression: Expression,
    ) -> Result<(), CompileError> {
        match expression.ty {
            ExpressionType::Identifier(identifier) => {
                let func_scope = self.function_scope_stack.last().unwrap();

                if let Some(var_idx) = func_scope.locals.iter().position(|var| *var == identifier) {
                    instructions.push(Instruction::Duplicate(var_idx + func_scope.num_upvalues));
                    return Ok(());
                }

                if let Some(var_idx) = self
                    .member_variables
                    .iter()
                    .position(|var| *var == identifier)
                {
                    instructions.push(Instruction::PushMember(var_idx));
                    return Ok(());
                }

                /*if let Some(_) = self.function_entry_points.get(&identifier) {
                    instructions.push(Instruction::PushMemberFunction(identifier));
                    return Ok(());
                }*/

                instructions.push(Instruction::PushGlobal(identifier));
            }
            ExpressionType::Null => {
                instructions.push(Instruction::PushNull);
            }
            ExpressionType::String(str) => {
                instructions.push(Instruction::PushString(str));
            }
            ExpressionType::Bool(b) => {
                instructions.push(Instruction::PushBool(b));
            }
            ExpressionType::Integer(i) => {
                instructions.push(Instruction::PushInt(i));
            }
            ExpressionType::Float(f) => {
                instructions.push(Instruction::PushFloat(f));
            }
            ExpressionType::Parenthesis(expr) => {
                self.evaluate_expression(instructions, *expr)?;
            }
            ExpressionType::Array(expressions) => {
                let len = expressions.len();
                for expression in expressions {
                    self.evaluate_expression(instructions, expression)?;
                }
                instructions.push(Instruction::MakeArray { len });
            }
            ExpressionType::FunctionCall(_) => {
                /*let n_args = expr.args.len();
                for arg in expr.args {
                    self.evaluate_expression(instructions, arg)?;
                }

                self.evaluate_expression(instructions, *expr.callee)?;

                instructions.push(Instruction::Call { n_args });*/
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

    pub fn register_native_func(
        &mut self,
        phrase: &str,
        func_id: i32,
    ) -> Result<(), InferenceError> {
        let embedding = self.inference.embed(phrase)?;
        let embed_id = self.inference.store(embedding);
        self.handlers.insert(embed_id, func_id);
        Ok(())
    }
}
