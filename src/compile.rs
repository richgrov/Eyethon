use std::collections::HashMap;
use std::fmt;

use ndarray::{Array, Array1};
use ndarray_linalg::Norm;
use ort::session::builder::GraphOptimizationLevel;
use ort::value::Tensor;
use tokenizers::Tokenizer;

use crate::parse::{Expression, ExpressionType, Statement, StatementType, VariableType};

#[derive(Debug)]
pub struct ClassBytecode {
    pub name: String,
    pub extends: Option<String>,
    pub bytecode: Vec<Instruction>,
    pub functions: HashMap<String, usize>,
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
    PushMemberFunction(String),
    Pop,
    MakeArray { len: usize },
    Call { n_args: usize },
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
            Instruction::PushMemberFunction(s) => write!(f, "pushmemf \"{}\"", s),
            Instruction::Pop => write!(f, "pop"),
            Instruction::MakeArray { len } => write!(f, "mkarray {}", len),
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

struct Compiler {
    annotation_handlers: HashMap<String, AnnotationHandler>,
    class_name: Option<String>,
    fallback_class_name: String,
    extends: Option<String>,
    non_class_name_statement_seen: bool,
    member_variables: Vec<String>,
    function_entry_points: HashMap<String, usize>,
    function_scope_stack: Vec<FunctionScope>,

    tokenizer: Tokenizer,
    ort_session: ort::session::Session,
}

impl Compiler {
    pub fn new(
        annotation_handlers: HashMap<String, AnnotationHandler>,
        fallback_class_name: String,
    ) -> Compiler {
        let tokenizer = Tokenizer::from_bytes(include_bytes!("./tokenizer.json")).unwrap();
        ort::init().commit().unwrap();

        let ort_session = ort::session::Session::builder()
            .unwrap()
            .with_optimization_level(GraphOptimizationLevel::Level1)
            .unwrap()
            .with_intra_threads(1)
            .unwrap()
            .commit_from_memory(include_bytes!("./model.onnx"))
            .unwrap();

        Compiler {
            annotation_handlers,
            fallback_class_name,

            class_name: None,
            extends: None,
            non_class_name_statement_seen: false,
            member_variables: Vec::new(),
            function_entry_points: HashMap::new(),
            function_scope_stack: Vec::with_capacity(4),

            tokenizer,
            ort_session,
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
                    self.function_entry_points.insert(func.name.clone(), 0);
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
            let num_args = function.args.len();

            self.function_scope_stack.push(FunctionScope {
                num_upvalues: 1,
                locals: function.args.into_iter().map(|arg| arg.name).collect(),
            });

            for statement in function.statements {
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
            name: self.class_name.unwrap_or(self.fallback_class_name),
            extends: self.extends,
            bytecode: instructions,
            functions: function_entry_points,
            member_variables: self.member_variables,
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

                if let Some(_) = self.function_entry_points.get(&identifier) {
                    instructions.push(Instruction::PushMemberFunction(identifier));
                    return Ok(());
                }

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
            ExpressionType::Array(expressions) => {
                let len = expressions.len();
                for expression in expressions {
                    self.evaluate_expression(instructions, expression)?;
                }
                instructions.push(Instruction::MakeArray { len });
            }
            ExpressionType::FunctionCall(expr) => {
                let n_args = expr.args.len();
                for arg in expr.args {
                    self.evaluate_expression(instructions, arg)?;
                }

                self.evaluate_expression(instructions, *expr.callee)?;

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

    fn embed(&self, text: &str) -> Result<ndarray::Array1<f32>, ort::Error> {
        let encoding = self.tokenizer.encode(text, true).unwrap();
        let ids = encoding.get_ids();
        let attention = encoding.get_attention_mask();

        let id_tensor = Tensor::from_array((
            [1, ids.len()],
            ids.iter().map(|x| *x as i64).collect::<Vec<_>>(),
        ))?;

        let attention_tensor = Tensor::from_array((
            [1, attention.len()],
            attention.iter().map(|x| *x as i64).collect::<Vec<_>>(),
        ))?;

        let result = self
            .ort_session
            .run(ort::inputs![id_tensor, attention_tensor]?)?;

        let output = result[0].try_extract_tensor::<f32>()?;
        let att = Array::from_shape_vec(
            [1, attention.len()],
            attention.iter().map(|x| *x as f32).collect::<Vec<_>>(),
        )
        .unwrap();

        let mut mean = Array1::<f32>::zeros([768]);
        for (token_idx, weight) in attention.iter().enumerate() {
            let token = output
                .slice(ndarray::s![0, token_idx, ..])
                .mapv(|v| v * (*weight as f32));
            mean += &token;
        }
        mean /= att.sum();

        let norm = mean.norm();
        return Ok(mean / norm);
    }
}

pub fn compile(
    statements: Vec<Statement>,
    annotation_handlers: HashMap<String, AnnotationHandler>,
    fallback_class_name: String,
) -> Result<ClassBytecode, CompileError> {
    Compiler::new(annotation_handlers, fallback_class_name).emit_class_bytecode(statements)
}
