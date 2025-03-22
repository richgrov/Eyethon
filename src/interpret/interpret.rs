use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::compile::{ClassBytecode, Instruction};

use super::Value;

pub struct Interpreter {
    source_files: Rc<RefCell<HashMap<String, ClassBytecode>>>,
    globals: HashMap<String, Value>,
    builtin_funcs: Vec<Box<dyn Fn(Vec<Value>) -> Value>>,
}

#[derive(Debug)]
pub enum RuntimeError {
    NotSettable(Value),
    NotCallable,
    NoSuchMethod(String),
    UndefinedGlobal(String),
    BadStack,
    OutOfInstructions,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeError::NotSettable(value) => write!(f, "{} is not settable", value),
            RuntimeError::NotCallable => write!(f, "not callable"),
            RuntimeError::NoSuchMethod(name) => write!(f, "no such method: {}", name),
            RuntimeError::UndefinedGlobal(name) => write!(f, "undefined global {}", name),
            RuntimeError::BadStack => write!(f, "bad stack"),
            RuntimeError::OutOfInstructions => write!(f, "out of instructions"),
        }
    }
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            source_files: Rc::new(RefCell::new(HashMap::new())),
            globals: HashMap::new(),
            builtin_funcs: Vec::new(),
        }
    }

    pub fn set_global(&mut self, name: &str, value: Value) {
        self.globals.insert(name.to_string(), value);
    }

    pub fn register_file(&mut self, file_name: String, class: ClassBytecode) {
        self.source_files.borrow_mut().insert(file_name, class);
    }

    pub fn exec(&mut self, file_name: &str, index: usize) -> Result<Option<Value>, RuntimeError> {
        let func_addr = {
            let source_files = self.source_files.borrow();
            let code = source_files.get(file_name).unwrap();

            *code
                .handler_addresses
                .get(index)
                .ok_or(RuntimeError::NoSuchMethod(index.to_string()))?
        };

        self.call_function(file_name, func_addr, vec![])
    }

    fn call_function(
        &mut self,
        file_name: &str,
        func_address: usize,
        args: Vec<Value>,
    ) -> Result<Option<Value>, RuntimeError> {
        let source_files = self.source_files.clone();
        let source_files = source_files.borrow();
        let code = source_files.get(file_name).unwrap();

        let instructions = &code.bytecode[func_address..];

        let mut pc = 0;
        let mut stack = args.to_vec();

        while pc < instructions.len() {
            match &instructions[pc] {
                Instruction::Duplicate(index) => {
                    let Some(value) = stack.get(*index) else {
                        return Err(RuntimeError::BadStack);
                    };
                    stack.push(value.clone());
                }
                Instruction::PushNull => {
                    stack.push(Value::Null);
                }
                Instruction::PushBool(b) => {
                    stack.push(Value::Bool(*b));
                }
                Instruction::PushInt(i) => {
                    stack.push(Value::Integer(*i));
                }
                Instruction::PushFloat(f) => {
                    stack.push(Value::Float(*f));
                }
                Instruction::Pop => {
                    stack.pop();
                }
                Instruction::MakeArray { len } => {
                    let elements = stack.split_off(stack.len() - len);
                    stack.push(Value::Array(elements));
                }
                Instruction::PushString(s) => {
                    stack.push(Value::String(s.clone()));
                }
                Instruction::PushGlobal(identifier) => {
                    let Some(global) = self.globals.get(identifier) else {
                        return Err(RuntimeError::UndefinedGlobal(identifier.to_owned()));
                    };
                    stack.push(global.clone());
                }
                Instruction::PushMember(index) => {
                    let this = &stack[0];
                    let Value::Object { variables, .. } = this else {
                        panic!();
                    };
                    let member = variables.borrow()[*index].clone();
                    stack.push(member);
                }
                Instruction::Do { action, n_args } => {
                    let args = stack.split_off(stack.len() - n_args);

                    let result = if *action >= 0 {
                        let address = {
                            let source_files = self.source_files.borrow();
                            let code = source_files.get(file_name).unwrap();
                            *code.handler_addresses.get(*action as usize).unwrap()
                        };

                        self.call_function(file_name, address, args)?
                    } else {
                        let func_idx = -action - 1;
                        Some(self.builtin_funcs[func_idx as usize](args))
                    };

                    stack.push(result.unwrap_or(Value::Null));
                }
                Instruction::Store => {
                    let val = stack.pop().unwrap();
                    let key = stack.pop().unwrap();
                    let obj = stack.pop().unwrap();

                    match obj {
                        Value::Dictionary(dict) => {
                            dict.borrow_mut().insert(key, val);
                        }
                        Value::Object {
                            variables,
                            variable_names,
                            ..
                        } => {
                            let Value::String(attribute) = key else {
                                panic!();
                            };

                            let index = variable_names.get(&attribute).unwrap();
                            variables.borrow_mut()[*index] = val;
                        }
                        other => {
                            return Err(RuntimeError::NotSettable(other));
                        }
                    }
                }
                Instruction::Return => {
                    if stack.len() == args.len() {
                        return Ok(None);
                    } else if stack.len() == args.len() + 1 {
                        return Ok(stack.pop());
                    } else {
                        return Err(RuntimeError::BadStack);
                    }
                }
            }

            pc += 1;
        }

        return Err(RuntimeError::OutOfInstructions);
    }

    pub fn add_builtin_function(&mut self, func: Box<dyn Fn(Vec<Value>) -> Value>) -> i32 {
        self.builtin_funcs.push(func);
        -(self.builtin_funcs.len() as i32)
    }
}
