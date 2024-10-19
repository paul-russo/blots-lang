use anyhow::{anyhow, Result};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, fmt::Display};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct LambdaDef {
    pub name: Option<String>,
    pub args: Vec<String>,
    pub body: String,
    pub scope: HashMap<String, Value>,
}

impl LambdaDef {
    pub fn set_name(&mut self, name: String) {
        self.name = Some(name);
    }
}

impl PartialOrd for LambdaDef {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.args > other.args {
            Some(std::cmp::Ordering::Greater)
        } else if self.args < other.args {
            Some(std::cmp::Ordering::Less)
        } else {
            Some(std::cmp::Ordering::Equal)
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, PartialOrd)]
pub enum SpreadValue {
    List(Vec<Value>),
    String(String),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, PartialOrd)]
pub enum Value {
    Number(f64),
    List(Vec<Value>),
    Spread(SpreadValue),
    Bool(bool),
    Lambda(LambdaDef),
    BuiltIn(String),
    String(String),
    Null,
}

impl IntoIterator for Value {
    type Item = Value;
    type IntoIter = std::vec::IntoIter<Value>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Value::Spread(SpreadValue::List(l)) => l.into_iter(), // Yields an iterator over the values in the spread list.
            Value::Spread(SpreadValue::String(s)) => s
                .chars()
                .map(|c| Value::String(c.to_string()))
                .collect::<Vec<Value>>()
                .into_iter(), // Yields an iterator over the characters in the string.
            _ => vec![self].into_iter(), // Yields a single value wrapped in a Vec
        }
    }
}

impl Value {
    pub fn get_type(&self) -> &str {
        match self {
            Value::Number(_) => "number",
            Value::List(_) => "list",
            Value::Spread(_) => "spread",
            Value::Bool(_) => "bool",
            Value::Lambda(_) => "function",
            Value::String(_) => "string",
            Value::Null => "null",
            Value::BuiltIn(_) => "built-in function",
        }
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }

    pub fn is_list(&self) -> bool {
        matches!(self, Value::List(_))
    }

    pub fn is_spread(&self) -> bool {
        matches!(self, Value::Spread(_))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Value::Bool(_))
    }

    pub fn is_lambda(&self) -> bool {
        matches!(self, Value::Lambda(_))
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Value::String(_))
    }

    pub fn is_null(&self) -> bool {
        matches!(self, Value::Null)
    }

    pub fn is_built_in(&self) -> bool {
        matches!(self, Value::BuiltIn(_))
    }

    pub fn as_number(&self) -> Result<f64> {
        match self {
            Value::Number(n) => Ok(*n),
            _ => Err(anyhow!("expected a number, but got a {}", self.get_type())),
        }
    }

    pub fn as_list(&self) -> Result<&Vec<Value>> {
        match self {
            Value::List(l) => Ok(l),
            _ => Err(anyhow!("expected a list, but got a {}", self.get_type())),
        }
    }

    pub fn as_spread(&self) -> Result<&SpreadValue> {
        match self {
            Value::Spread(v) => Ok(v),
            _ => Err(anyhow!("expected a spread, but got a {}", self.get_type())),
        }
    }

    pub fn as_bool(&self) -> Result<bool> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => Err(anyhow!("expected a boolean, but got a {}", self.get_type())),
        }
    }

    pub fn as_lambda(&self) -> Result<&LambdaDef> {
        match self {
            Value::Lambda(def) => Ok(def),
            _ => Err(anyhow!("expected a lambda, but got a {}", self.get_type())),
        }
    }

    pub fn as_string(&self) -> Result<&str> {
        match self {
            Value::String(s) => Ok(s),
            _ => Err(anyhow!("expected a string, but got a {}", self.get_type())),
        }
    }

    pub fn as_null(&self) -> Result<()> {
        match self {
            Value::Null => Ok(()),
            _ => Err(anyhow!("expected a null, but got a {}", self.get_type())),
        }
    }

    pub fn as_built_in(&self) -> Result<&str> {
        match self {
            Value::BuiltIn(s) => Ok(s),
            _ => Err(anyhow!(
                "expected a built-in function, but got a {}",
                self.get_type()
            )),
        }
    }

    pub fn stringify(&self) -> String {
        match self {
            Value::String(s) => format!("{}", s),
            _ => format!("{}", self),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::List(l) => {
                write!(f, "[")?;
                for (i, value) in l.iter().enumerate() {
                    write!(f, "{}", value)?;
                    if i < l.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Value::Spread(v) => match v {
                SpreadValue::List(l) => {
                    write!(f, "...[")?;
                    for (i, value) in l.iter().enumerate() {
                        write!(f, "{}", value)?;
                        if i < l.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, "]")
                }
                SpreadValue::String(s) => write!(f, "...\"{}\"", s),
            },
            Value::Bool(b) => write!(f, "{}", b),
            Value::Lambda(def) => {
                write!(f, "(")?;
                for (i, arg) in def.args.iter().enumerate() {
                    write!(f, "{}", arg)?;
                    if i < def.args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") => {}", def.body)
            }
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Null => write!(f, "null"),
            Value::BuiltIn(s) => write!(f, "{} (built-in)", s),
        }
    }
}
