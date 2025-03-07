use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::compile::{ClassBytecode, Instruction};

use super::Value;

pub struct Interpreter {
    classes: Rc<RefCell<HashMap<String, ClassBytecode>>>,
    class_objects: HashMap<String, Value>,
    globals: HashMap<String, Value>,
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
            classes: Rc::new(RefCell::new(HashMap::new())),
            class_objects: HashMap::new(),
            globals: HashMap::new(),
        }
    }

    pub fn set_global(&mut self, name: &str, value: Value) {
        self.globals.insert(name.to_string(), value);
    }

    pub fn register_class(&mut self, class: ClassBytecode) -> Result<(), RuntimeError> {
        self.class_objects.insert(
            class.name.clone(),
            Value::Object {
                variables: Rc::new(RefCell::new(Vec::new())),
                variable_names: HashMap::new(),
                class_name: "".to_owned(),
            },
        );
        self.classes.borrow_mut().insert(class.name.clone(), class);

        Ok(())
    }

    pub fn new_instance(&mut self, class_name: &str) -> Result<Value, RuntimeError> {
        let classes = self.classes.clone();
        let classes = classes.borrow();
        let class = classes.get(class_name).unwrap();

        let num_members = class.member_variables.len();
        let mut variable_names = HashMap::with_capacity(num_members);
        for (i, name) in class.member_variables.iter().enumerate() {
            variable_names.insert(name.to_owned(), i);
        }

        let this = Value::Object {
            variables: Rc::new(RefCell::new(vec![Value::Null; num_members])),
            variable_names,
            class_name: class_name.to_owned(),
        };

        self.call_function(class_name, 0, &vec![this.clone()], &[])?;

        Ok(this)
    }

    pub fn call_method(
        &mut self,
        this: Value,
        method_name: &str,
    ) -> Result<Option<Value>, RuntimeError> {
        let class_name = match &this {
            Value::Object { class_name, .. } => class_name,
            _ => panic!("not callable"),
        };
        let class_name = class_name.to_owned();

        let classes = self.classes.clone();
        let classes = classes.borrow();
        let class = &classes.get(&class_name).unwrap();

        let func_addr = class
            .functions
            .get(method_name)
            .ok_or(RuntimeError::NoSuchMethod(method_name.to_owned()))?;

        let upvalues = vec![this];
        self.call_function(&class_name, *func_addr, &upvalues, &[])
    }

    fn call_function(
        &mut self,
        class_name: &str,
        func_address: usize,
        upvalues: &[Value],
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        let classes = self.classes.clone();
        let classes = classes.borrow();
        let class = &classes.get(class_name).unwrap();
        let instructions = &class.bytecode[func_address..];

        let mut pc = 0;
        let mut stack = Vec::with_capacity(upvalues.len() + args.len());
        stack.extend_from_slice(upvalues);
        stack.extend_from_slice(args);

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
                Instruction::PushMemberFunction(name) => {
                    let this = &stack[0];
                    let Value::Object { class_name, .. } = this else {
                        panic!();
                    };

                    let classes = self.classes.borrow();
                    let class = classes.get(class_name).unwrap();

                    let address = class.functions.get(name).unwrap();
                    stack.push(Value::Function {
                        address: *address,
                        upvalues: vec![this.clone()],
                    });
                }
                Instruction::Call { n_args } => {
                    let callee = stack.pop().unwrap();
                    match callee {
                        Value::Function { address, upvalues } => {
                            let args = stack.split_off(stack.len() - n_args);
                            let result =
                                self.call_function(class_name, address, &upvalues, &args)?;
                            stack.push(result.unwrap_or(Value::Null));
                        }
                        Value::NativeFunction(func) => {
                            let args = stack.split_off(stack.len() - n_args);
                            let result = func(args);
                            stack.push(result);
                        }
                        _ => return Err(RuntimeError::NotCallable),
                    }
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
                    let num_inputs = upvalues.len() + args.len();
                    if stack.len() == num_inputs {
                        return Ok(None);
                    } else if stack.len() == num_inputs + 1 {
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
}
