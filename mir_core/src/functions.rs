use std::collections::{HashMap, HashSet};

use anyhow::{anyhow, Result};
use serde::{Deserialize, Serialize};

use crate::{expressions::evaluate_expression, parser::get_pairs, values::Value};

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
    pub body: fn(Vec<Value>) -> Result<Value>,
}

pub enum FunctionDef {
    UserDefined(UserDefinedFunctionDef),
    BuiltIn(BuiltInFunctionDef),
}

pub static BUILT_IN_FUNCTION_IDENTS: [&str; 28] = [
    "sqrt", "sin", "cos", "tan", "asin", "acos", "atan", "log", "log10", "exp", "abs", "floor",
    "ceil", "round", "trunc", "min", "max", "avg", "sum", "prod", "median", "len", "head", "tail",
    "slice", "concat", "dot", "unique",
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
        args: Vec<Value>,
        variables: &HashMap<String, Value>,
        function_defs: &HashMap<String, UserDefinedFunctionDef>,
    ) -> Result<Value> {
        self.check_arity(args.len())?;

        match self {
            FunctionDef::UserDefined(UserDefinedFunctionDef {
                args: expected_args,
                body,
                ..
            }) => {
                let mut new_variables = variables.clone();
                for (arg, value) in expected_args.iter().zip(args.iter()) {
                    new_variables.insert(arg.clone(), value.clone());
                }

                evaluate_expression(
                    get_pairs(body)?.next().unwrap().into_inner(),
                    &new_variables,
                    function_defs,
                )
            }
            FunctionDef::BuiltIn(BuiltInFunctionDef { body, .. }) => body(args),
        }
    }
}

pub fn get_built_in_function_def(name: &str) -> Option<BuiltInFunctionDef> {
    match name {
        "sqrt" => Some(BuiltInFunctionDef {
            name: String::from("sqrt"),
            arity: FunctionArity::Exact(1),
            body: |args| Ok(Value::Number(args[0].to_number()?.sqrt())),
        }),
        "sin" => Some(BuiltInFunctionDef {
            name: String::from("sin"),
            arity: FunctionArity::Exact(1),
            body: |args| Ok(Value::Number(args[0].to_number()?.sin())),
        }),
        "cos" => Some(BuiltInFunctionDef {
            name: String::from("cos"),
            arity: FunctionArity::Exact(1),
            body: |args| Ok(Value::Number(args[0].to_number()?.cos())),
        }),
        "tan" => Some(BuiltInFunctionDef {
            name: String::from("tan"),
            arity: FunctionArity::Exact(1),
            body: |args| Ok(Value::Number(args[0].to_number()?.tan())),
        }),
        "asin" => Some(BuiltInFunctionDef {
            name: String::from("asin"),
            arity: FunctionArity::Exact(1),
            body: |args| Ok(Value::Number(args[0].to_number()?.asin())),
        }),
        "acos" => Some(BuiltInFunctionDef {
            name: String::from("acos"),
            arity: FunctionArity::Exact(1),
            body: |args| Ok(Value::Number(args[0].to_number()?.acos())),
        }),
        "atan" => Some(BuiltInFunctionDef {
            name: String::from("atan"),
            arity: FunctionArity::Exact(1),
            body: |args| Ok(Value::Number(args[0].to_number()?.atan())),
        }),
        "log" => Some(BuiltInFunctionDef {
            name: String::from("log"),
            arity: FunctionArity::Exact(1),
            body: |args| Ok(Value::Number(args[0].to_number()?.ln())),
        }),
        "log10" => Some(BuiltInFunctionDef {
            name: String::from("log10"),
            arity: FunctionArity::Exact(1),
            body: |args| Ok(Value::Number(args[0].to_number()?.log10())),
        }),
        "exp" => Some(BuiltInFunctionDef {
            name: String::from("exp"),
            arity: FunctionArity::Exact(1),
            body: |args| Ok(Value::Number(args[0].to_number()?.exp())),
        }),
        "abs" => Some(BuiltInFunctionDef {
            name: String::from("abs"),
            arity: FunctionArity::Exact(1),
            body: |args| Ok(Value::Number(args[0].to_number()?.abs())),
        }),
        "floor" => Some(BuiltInFunctionDef {
            name: String::from("floor"),
            arity: FunctionArity::Exact(1),
            body: |args| Ok(Value::Number(args[0].to_number()?.floor())),
        }),
        "ceil" => Some(BuiltInFunctionDef {
            name: String::from("ceil"),
            arity: FunctionArity::Exact(1),
            body: |args| Ok(Value::Number(args[0].to_number()?.ceil())),
        }),
        "round" => Some(BuiltInFunctionDef {
            name: String::from("round"),
            arity: FunctionArity::Exact(1),
            body: |args| Ok(Value::Number(args[0].to_number()?.round())),
        }),
        "trunc" => Some(BuiltInFunctionDef {
            name: String::from("trunc"),
            arity: FunctionArity::Exact(1),
            body: |args| Ok(Value::Number(args[0].to_number()?.trunc())),
        }),
        "min" => Some(BuiltInFunctionDef {
            name: String::from("min"),
            arity: FunctionArity::AtLeast(2),
            body: |args| {
                Ok(Value::Number(
                    args.iter()
                        .map(|a| a.to_number())
                        .collect::<Result<Vec<f64>>>()?
                        .iter()
                        .copied()
                        .fold(f64::INFINITY, f64::min),
                ))
            },
        }),
        "max" => Some(BuiltInFunctionDef {
            name: String::from("max"),
            arity: FunctionArity::AtLeast(2),
            body: |args| {
                Ok(Value::Number(
                    args.iter()
                        .map(|a| a.to_number())
                        .collect::<Result<Vec<f64>>>()?
                        .iter()
                        .copied()
                        .fold(f64::NEG_INFINITY, f64::max),
                ))
            },
        }),
        "avg" => Some(BuiltInFunctionDef {
            name: String::from("avg"),
            arity: FunctionArity::AtLeast(2),
            body: |args| {
                Ok(Value::Number(
                    args.iter()
                        .map(|a| a.to_number())
                        .collect::<Result<Vec<f64>>>()?
                        .iter()
                        .sum::<f64>()
                        / args.len() as f64,
                ))
            },
        }),
        "sum" => Some(BuiltInFunctionDef {
            name: String::from("sum"),
            arity: FunctionArity::AtLeast(2),
            body: |args| {
                Ok(Value::Number(
                    args.iter()
                        .map(|a| a.to_number())
                        .collect::<Result<Vec<f64>>>()?
                        .iter()
                        .sum(),
                ))
            },
        }),
        "prod" => Some(BuiltInFunctionDef {
            name: String::from("prod"),
            arity: FunctionArity::AtLeast(2),
            body: |args| {
                Ok(Value::Number(
                    args.iter()
                        .map(|a| a.to_number())
                        .collect::<Result<Vec<f64>>>()?
                        .iter()
                        .product(),
                ))
            },
        }),
        "median" => Some(BuiltInFunctionDef {
            name: String::from("median"),
            arity: FunctionArity::AtLeast(2),
            body: |args| {
                let mut nums = args
                    .into_iter()
                    .map(|a| a.to_number())
                    .collect::<Result<Vec<f64>>>()?;

                nums.sort_by(|a, b| a.partial_cmp(b).unwrap());
                let len = nums.len();
                if len % 2 == 0 {
                    Ok(Value::Number((nums[len / 2 - 1] + nums[len / 2]) / 2.0))
                } else {
                    Ok(Value::Number(nums[len / 2]))
                }
            },
        }),
        "len" => Some(BuiltInFunctionDef {
            name: String::from("len"),
            arity: FunctionArity::Exact(1),
            body: |args| Ok(Value::Number(args[0].to_list()?.len() as f64)),
        }),
        "head" => Some(BuiltInFunctionDef {
            name: String::from("head"),
            arity: FunctionArity::Exact(1),
            body: |args| {
                Ok(args[0]
                    .to_list()?
                    .first()
                    .cloned()
                    .unwrap_or(Value::List(vec![])))
            },
        }),
        "tail" => Some(BuiltInFunctionDef {
            name: String::from("tail"),
            arity: FunctionArity::Exact(1),
            body: |args| {
                let list = args[0].to_list()?;
                Ok(Value::List(list.iter().skip(1).cloned().collect()))
            },
        }),
        "slice" => Some(BuiltInFunctionDef {
            name: String::from("slice"),
            arity: FunctionArity::Exact(3),
            body: |args| {
                let list = args[0].to_list()?;
                let start = args[1].to_number()? as usize;
                let end = args[2].to_number()? as usize;

                Ok(Value::List(list[start..end].to_vec()))
            },
        }),
        "concat" => Some(BuiltInFunctionDef {
            name: String::from("concat"),
            arity: FunctionArity::AtLeast(2),
            body: |args| {
                let mut list = vec![];
                for arg in args {
                    match arg {
                        Value::List(l) => list.extend(l),
                        Value::Number(_) => list.push(arg),
                        Value::Spread(l) => list.extend(l),
                        Value::Bool(_) => list.push(arg),
                    }
                }

                Ok(Value::List(list))
            },
        }),
        "dot" => Some(BuiltInFunctionDef {
            name: String::from("dot"),
            arity: FunctionArity::Exact(2),
            body: |args| {
                let a = args[0].to_list()?;
                let b = args[1].to_list()?;

                if a.len() != b.len() {
                    return Err(anyhow!(
                        "cannot calculate dot product of lists with different lengths"
                    ));
                }

                Ok(Value::Number(
                    a.iter()
                        .zip(b.iter())
                        .map(|(a, b)| {
                            let a_num = a.to_number()?;
                            let b_num = b.to_number()?;
                            Ok(a_num * b_num)
                        })
                        .collect::<Result<Vec<f64>>>()?
                        .iter()
                        .sum(),
                ))
            },
        }),
        "unique" => Some(BuiltInFunctionDef {
            name: String::from("unique"),
            arity: FunctionArity::Exact(1),
            body: |args| {
                let list = args[0].to_list()?.clone();
                let mut unique_list = vec![];

                for item in list {
                    if !unique_list.contains(&item) {
                        unique_list.push(item);
                    }
                }

                Ok(Value::List(unique_list))
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
