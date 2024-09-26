use std::collections::HashMap;

use anyhow::{anyhow, Result};
use serde::{Deserialize, Serialize};

use crate::{expressions::evaluate_expression, parser::get_pairs};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct UserDefinedFunctionDef {
    pub name: String,
    pub args: Vec<String>,
    pub body: String,
}

pub enum FunctionArity {
    Exact(usize),
    AtLeast(usize),
}

pub struct BuiltInFunctionDef {
    pub name: String,
    pub arity: FunctionArity,
    pub body: fn(Vec<f64>) -> f64,
}

pub enum FunctionDef {
    UserDefined(UserDefinedFunctionDef),
    BuiltIn(BuiltInFunctionDef),
}

pub static BUILT_IN_FUNCTION_IDENTS: [&str; 21] = [
    "sqrt", "sin", "cos", "tan", "asin", "acos", "atan", "log", "log10", "exp", "abs", "floor",
    "ceil", "round", "trunc", "min", "max", "avg", "sum", "prod", "median",
];

impl FunctionDef {
    pub fn check_arity(&self, arg_count: usize) -> Result<()> {
        match self {
            FunctionDef::UserDefined(UserDefinedFunctionDef {
                args: expected_args,
                name,
                ..
            }) => {
                if arg_count == expected_args.len() {
                    Ok(())
                } else {
                    let expected_args = if expected_args.len() == 1 {
                        "1 argument".to_string()
                    } else {
                        format!("{} arguments", expected_args.len())
                    };

                    let given_args = if arg_count == 1 {
                        "1 was given".to_string()
                    } else {
                        format!("{} were given", arg_count)
                    };

                    Err(anyhow!(
                        "function {} takes {}, but {}",
                        name,
                        expected_args,
                        given_args
                    ))
                }
            }
            FunctionDef::BuiltIn(BuiltInFunctionDef { arity, name, .. }) => match arity {
                FunctionArity::Exact(expected) => {
                    if arg_count == *expected {
                        Ok(())
                    } else {
                        Err(anyhow!(
                            "function {} takes exactly {} arguments, but {} were given",
                            name,
                            expected,
                            arg_count
                        ))
                    }
                }
                FunctionArity::AtLeast(expected) => {
                    if arg_count >= *expected {
                        Ok(())
                    } else {
                        Err(anyhow!(
                            "function {} takes at least {} arguments, but {} were given",
                            name,
                            expected,
                            arg_count
                        ))
                    }
                }
            },
        }
    }

    pub fn call(
        &self,
        args: Vec<f64>,
        variables: &HashMap<String, f64>,
        function_defs: &HashMap<String, UserDefinedFunctionDef>,
    ) -> Result<f64> {
        self.check_arity(args.len())?;

        match self {
            FunctionDef::UserDefined(UserDefinedFunctionDef {
                args: expected_args,
                body,
                ..
            }) => {
                let mut new_variables = variables.clone();
                for (arg, value) in expected_args.iter().zip(args.iter()) {
                    new_variables.insert(arg.clone(), *value);
                }

                evaluate_expression(
                    get_pairs(body)?.next().unwrap().into_inner(),
                    &new_variables,
                    function_defs,
                )
            }
            FunctionDef::BuiltIn(BuiltInFunctionDef { body, .. }) => Ok(body(args)),
        }
    }
}

pub fn get_built_in_function_def(name: &str) -> Option<BuiltInFunctionDef> {
    match name {
        "sqrt" => Some(BuiltInFunctionDef {
            name: String::from("sqrt"),
            arity: FunctionArity::Exact(1),
            body: |args| args[0].sqrt(),
        }),
        "sin" => Some(BuiltInFunctionDef {
            name: String::from("sin"),
            arity: FunctionArity::Exact(1),
            body: |args| args[0].sin(),
        }),
        "cos" => Some(BuiltInFunctionDef {
            name: String::from("cos"),
            arity: FunctionArity::Exact(1),
            body: |args| args[0].cos(),
        }),
        "tan" => Some(BuiltInFunctionDef {
            name: String::from("tan"),
            arity: FunctionArity::Exact(1),
            body: |args| args[0].tan(),
        }),
        "asin" => Some(BuiltInFunctionDef {
            name: String::from("asin"),
            arity: FunctionArity::Exact(1),
            body: |args| args[0].asin(),
        }),
        "acos" => Some(BuiltInFunctionDef {
            name: String::from("acos"),
            arity: FunctionArity::Exact(1),
            body: |args| args[0].acos(),
        }),
        "atan" => Some(BuiltInFunctionDef {
            name: String::from("atan"),
            arity: FunctionArity::Exact(1),
            body: |args| args[0].atan(),
        }),
        "log" => Some(BuiltInFunctionDef {
            name: String::from("log"),
            arity: FunctionArity::Exact(1),
            body: |args| args[0].ln(),
        }),
        "log10" => Some(BuiltInFunctionDef {
            name: String::from("log10"),
            arity: FunctionArity::Exact(1),
            body: |args| args[0].log10(),
        }),
        "exp" => Some(BuiltInFunctionDef {
            name: String::from("exp"),
            arity: FunctionArity::Exact(1),
            body: |args| args[0].exp(),
        }),
        "abs" => Some(BuiltInFunctionDef {
            name: String::from("abs"),
            arity: FunctionArity::Exact(1),
            body: |args| args[0].abs(),
        }),
        "floor" => Some(BuiltInFunctionDef {
            name: String::from("floor"),
            arity: FunctionArity::Exact(1),
            body: |args| args[0].floor(),
        }),
        "ceil" => Some(BuiltInFunctionDef {
            name: String::from("ceil"),
            arity: FunctionArity::Exact(1),
            body: |args| args[0].ceil(),
        }),
        "round" => Some(BuiltInFunctionDef {
            name: String::from("round"),
            arity: FunctionArity::Exact(1),
            body: |args| args[0].round(),
        }),
        "trunc" => Some(BuiltInFunctionDef {
            name: String::from("trunc"),
            arity: FunctionArity::Exact(1),
            body: |args| args[0].trunc(),
        }),
        "min" => Some(BuiltInFunctionDef {
            name: String::from("min"),
            arity: FunctionArity::AtLeast(2),
            body: |args| args.iter().copied().fold(f64::INFINITY, f64::min),
        }),
        "max" => Some(BuiltInFunctionDef {
            name: String::from("max"),
            arity: FunctionArity::AtLeast(2),
            body: |args| args.iter().copied().fold(f64::NEG_INFINITY, f64::max),
        }),
        "avg" => Some(BuiltInFunctionDef {
            name: String::from("avg"),
            arity: FunctionArity::AtLeast(2),
            body: |args| args.iter().sum::<f64>() / args.len() as f64,
        }),
        "sum" => Some(BuiltInFunctionDef {
            name: String::from("sum"),
            arity: FunctionArity::AtLeast(2),
            body: |args| args.iter().sum(),
        }),
        "prod" => Some(BuiltInFunctionDef {
            name: String::from("prod"),
            arity: FunctionArity::AtLeast(2),
            body: |args| args.iter().product(),
        }),
        "median" => Some(BuiltInFunctionDef {
            name: String::from("median"),
            arity: FunctionArity::AtLeast(2),
            body: |mut args| {
                args.sort_by(|a, b| a.partial_cmp(b).unwrap());
                let len = args.len();
                if len % 2 == 0 {
                    (args[len / 2 - 1] + args[len / 2]) / 2.0
                } else {
                    args[len / 2]
                }
            },
        }),
        _ => None,
    }
}

pub fn is_built_in_function(name: &str) -> bool {
    get_built_in_function_def(name).is_some()
}

pub fn get_function_def(
    name: &str,
    function_defs: &HashMap<String, UserDefinedFunctionDef>,
) -> Option<FunctionDef> {
    function_defs
        .get(name)
        .map(|def| FunctionDef::UserDefined(def.clone()))
        .or_else(|| get_built_in_function_def(name).map(FunctionDef::BuiltIn))
}
