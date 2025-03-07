use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::rc::Rc;

#[derive(Clone)]
pub enum Value {
    Null,
    Bool(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Array(Vec<Value>),
    Dictionary(Rc<RefCell<HashMap<Value, Value>>>),
    Object {
        variables: Rc<RefCell<Vec<Value>>>,
        variable_names: HashMap<String, usize>,
        class_name: String,
    },
    Function {
        address: usize,
        upvalues: Vec<Value>,
    },
    NativeFunction(Rc<dyn Fn(Vec<Value>) -> Value>),
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Null => 0.hash(state),
            Value::Bool(b) => b.hash(state),
            Value::Integer(i) => i.hash(state),
            Value::Float(f) => f.to_bits().hash(state),
            Value::String(s) => s.hash(state),
            Value::Array(o) => {
                o.len().hash(state);
                for value in o {
                    value.hash(state);
                }
            }
            Value::Dictionary(o) => {
                let dict = o.borrow();
                dict.len().hash(state);

                for (key, value) in dict.deref() {
                    key.hash(state);
                    value.hash(state);
                }
            }
            Value::Object {
                variables,
                class_name,
                ..
            } => {
                let variables = variables.borrow();
                variables.hash(state);
                class_name.hash(state);
            }
            Value::Function { address, upvalues } => {
                address.hash(state);
                upvalues.hash(state);
            }
            Value::NativeFunction(func) => {
                let ptr = Rc::<_>::as_ptr(func);
                ptr.hash(state);
            }
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Null, Value::Null) => true,
            (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
            (Value::Integer(i1), Value::Integer(i2)) => i1 == i2,
            (Value::Float(f1), Value::Float(f2)) => f1 == f2,
            (Value::String(s1), Value::String(s2)) => s1 == s2,
            (Value::Dictionary(o1), Value::Dictionary(o2)) => {
                let dict1 = o1.borrow();
                let dict2 = o2.borrow();
                dict1.deref() == dict2.deref()
            }
            (
                Value::Object {
                    variables: v1,
                    class_name: c1,
                    ..
                },
                Value::Object {
                    variables: v2,
                    class_name: c2,
                    ..
                },
            ) => {
                let vars1 = v1.borrow();
                let vars2 = v2.borrow();
                c1 == c2 && vars1.deref() == vars2.deref()
            }
            (
                Value::Function {
                    address: addr1,
                    upvalues: up1,
                },
                Value::Function {
                    address: addr2,
                    upvalues: up2,
                },
            ) => std::ptr::addr_eq(addr1, addr2) && up1.deref() == up2.deref(),
            (Value::NativeFunction(rc1), Value::NativeFunction(rc2)) => {
                std::ptr::addr_eq(Rc::<_>::as_ptr(rc1), Rc::<_>::as_ptr(rc2))
            }
            _ => false,
        }
    }
}

impl Eq for Value {}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Integer(i) => write!(f, "{}", i),
            Value::Float(fl) => write!(f, "{:?}", fl),
            Value::String(s) => write!(f, "{}", s),
            Value::Array(a) => {
                write!(f, "[")?;
                for (i, v) in a.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, "]")
            }
            Value::Dictionary(o) => {
                write!(f, "{{")?;
                for (i, (k, v)) in o.borrow().iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", k, v)?;
                }
                write!(f, "}}")
            }
            Value::Object {
                variables,
                variable_names,
                class_name,
            } => {
                let vars = variables.borrow();

                write!(f, "{} {{", class_name)?;
                for (i, (name, index)) in variable_names.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, vars[*index])?;
                }
                write!(f, "}}")
            }
            Value::Function { address, .. } => write!(f, "<function at {:p}>", address),
            Value::NativeFunction(_) => write!(f, "<native function>"),
        }
    }
}
