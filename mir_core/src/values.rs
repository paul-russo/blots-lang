use std::fmt::Display;

use anyhow::{anyhow, Result};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Value {
    Number(f64),
    List(Vec<Value>),
    Spread(Vec<Value>),
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
    pub fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }

    pub fn is_list(&self) -> bool {
        matches!(self, Value::List(_))
    }

    pub fn is_spread(&self) -> bool {
        matches!(self, Value::Spread(_))
    }

    pub fn to_number(&self) -> Result<f64> {
        match self {
            Value::Number(n) => Ok(*n),
            _ => Err(anyhow!("expected a number, but got a list")),
        }
    }

    pub fn to_list(&self) -> Result<&Vec<Value>> {
        match self {
            Value::List(l) => Ok(l),
            _ => Err(anyhow!("expected a list, but got a number")),
        }
    }

    pub fn to_spread(&self) -> Result<&Vec<Value>> {
        match self {
            Value::Spread(l) => Ok(l),
            _ => Err(anyhow!("expected a spread, but got a number")),
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
        }
    }
}
