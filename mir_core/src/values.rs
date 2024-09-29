use std::fmt::Display;

use anyhow::{anyhow, Result};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, PartialOrd)]
pub struct LambdaDef {
    pub args: Vec<String>,
    pub body: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, PartialOrd)]
pub enum Value {
    Number(f64),
    List(Vec<Value>),
    Spread(Vec<Value>),
    Bool(bool),
    Lambda(LambdaDef),
}

impl IntoIterator for Value {
    type Item = Value;
    type IntoIter = std::vec::IntoIter<Value>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Value::Spread(l) => l.into_iter(), // Yields an iterator over the values in the spread list.
            _ => vec![self].into_iter(),       // Yields a single value wrapped in a Vec
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
            Value::Lambda(_) => "lambda",
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

    pub fn to_number(&self) -> Result<f64> {
        match self {
            Value::Number(n) => Ok(*n),
            _ => Err(anyhow!("expected a number, but got a {}", self.get_type())),
        }
    }

    pub fn to_list(&self) -> Result<&Vec<Value>> {
        match self {
            Value::List(l) => Ok(l),
            _ => Err(anyhow!("expected a list, but got a {}", self.get_type())),
        }
    }

    pub fn to_spread(&self) -> Result<&Vec<Value>> {
        match self {
            Value::Spread(l) => Ok(l),
            _ => Err(anyhow!("expected a spread, but got a {}", self.get_type())),
        }
    }

    pub fn to_bool(&self) -> Result<bool> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => Err(anyhow!("expected a boolean, but got a {}", self.get_type())),
        }
    }

    pub fn to_lambda(&self) -> Result<&LambdaDef> {
        match self {
            Value::Lambda(def) => Ok(def),
            _ => Err(anyhow!("expected a lambda, but got a {}", self.get_type())),
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
            Value::Spread(l) => {
                write!(f, "...[")?;
                for (i, value) in l.iter().enumerate() {
                    write!(f, "{}", value)?;
                    if i < l.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
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
        }
    }
}
