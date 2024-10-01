use std::{collections::HashMap, sync::LazyLock};

use anyhow::{anyhow, Result};
use pest::iterators::Pairs;

use crate::{
    expressions::evaluate_expression,
    parser::{get_pairs, Rule},
    values::{LambdaDef, SpreadValue, Value},
};

pub enum FunctionArity {
    Exact(usize),
    AtLeast(usize),
}

pub struct BuiltInFunctionDef {
    pub name: String,
    pub arity: FunctionArity,
    pub body: fn(args: Vec<Value>, variables: &HashMap<String, Value>) -> Result<Value>,
}

pub enum FunctionDef<'a> {
    BuiltIn(&'a BuiltInFunctionDef),
    Lambda(&'a LambdaDef),
}

pub static BUILT_IN_FUNCTION_DEFS: LazyLock<HashMap<&str, BuiltInFunctionDef>> =
    LazyLock::new(|| {
        let mut built_ins_map = HashMap::new();

        built_ins_map.insert(
            "sqrt",
            BuiltInFunctionDef {
                name: String::from("sqrt"),
                arity: FunctionArity::Exact(1),
                body: |args, _| Ok(Value::Number(args[0].as_number()?.sqrt())),
            },
        );
        built_ins_map.insert(
            "sin",
            BuiltInFunctionDef {
                name: String::from("sin"),
                arity: FunctionArity::Exact(1),
                body: |args, _| Ok(Value::Number(args[0].as_number()?.sin())),
            },
        );
        built_ins_map.insert(
            "cos",
            BuiltInFunctionDef {
                name: String::from("cos"),
                arity: FunctionArity::Exact(1),
                body: |args, _| Ok(Value::Number(args[0].as_number()?.cos())),
            },
        );
        built_ins_map.insert(
            "tan",
            BuiltInFunctionDef {
                name: String::from("tan"),
                arity: FunctionArity::Exact(1),
                body: |args, _| Ok(Value::Number(args[0].as_number()?.tan())),
            },
        );
        built_ins_map.insert(
            "asin",
            BuiltInFunctionDef {
                name: String::from("asin"),
                arity: FunctionArity::Exact(1),
                body: |args, _| Ok(Value::Number(args[0].as_number()?.asin())),
            },
        );
        built_ins_map.insert(
            "acos",
            BuiltInFunctionDef {
                name: String::from("acos"),
                arity: FunctionArity::Exact(1),
                body: |args, _| Ok(Value::Number(args[0].as_number()?.acos())),
            },
        );
        built_ins_map.insert(
            "atan",
            BuiltInFunctionDef {
                name: String::from("atan"),
                arity: FunctionArity::Exact(1),
                body: |args, _| Ok(Value::Number(args[0].as_number()?.atan())),
            },
        );
        built_ins_map.insert(
            "log",
            BuiltInFunctionDef {
                name: String::from("log"),
                arity: FunctionArity::Exact(1),
                body: |args, _| Ok(Value::Number(args[0].as_number()?.ln())),
            },
        );
        built_ins_map.insert(
            "log10",
            BuiltInFunctionDef {
                name: String::from("log10"),
                arity: FunctionArity::Exact(1),
                body: |args, _| Ok(Value::Number(args[0].as_number()?.log10())),
            },
        );
        built_ins_map.insert(
            "exp",
            BuiltInFunctionDef {
                name: String::from("exp"),
                arity: FunctionArity::Exact(1),
                body: |args, _| Ok(Value::Number(args[0].as_number()?.exp())),
            },
        );
        built_ins_map.insert(
            "abs",
            BuiltInFunctionDef {
                name: String::from("abs"),
                arity: FunctionArity::Exact(1),
                body: |args, _| Ok(Value::Number(args[0].as_number()?.abs())),
            },
        );
        built_ins_map.insert(
            "floor",
            BuiltInFunctionDef {
                name: String::from("floor"),
                arity: FunctionArity::Exact(1),
                body: |args, _| Ok(Value::Number(args[0].as_number()?.floor())),
            },
        );
        built_ins_map.insert(
            "ceil",
            BuiltInFunctionDef {
                name: String::from("ceil"),
                arity: FunctionArity::Exact(1),
                body: |args, _| Ok(Value::Number(args[0].as_number()?.ceil())),
            },
        );
        built_ins_map.insert(
            "round",
            BuiltInFunctionDef {
                name: String::from("round"),
                arity: FunctionArity::Exact(1),
                body: |args, _| Ok(Value::Number(args[0].as_number()?.round())),
            },
        );
        built_ins_map.insert(
            "trunc",
            BuiltInFunctionDef {
                name: String::from("trunc"),
                arity: FunctionArity::Exact(1),
                body: |args, _| Ok(Value::Number(args[0].as_number()?.trunc())),
            },
        );
        built_ins_map.insert(
            "min",
            BuiltInFunctionDef {
                name: String::from("min"),
                arity: FunctionArity::AtLeast(2),
                body: |args, _| {
                    Ok(Value::Number(
                        args.iter()
                            .map(|a| a.as_number())
                            .collect::<Result<Vec<f64>>>()?
                            .iter()
                            .copied()
                            .fold(f64::INFINITY, f64::min),
                    ))
                },
            },
        );
        built_ins_map.insert(
            "max",
            BuiltInFunctionDef {
                name: String::from("max"),
                arity: FunctionArity::AtLeast(2),
                body: |args, _| {
                    Ok(Value::Number(
                        args.iter()
                            .map(|a| a.as_number())
                            .collect::<Result<Vec<f64>>>()?
                            .iter()
                            .copied()
                            .fold(f64::NEG_INFINITY, f64::max),
                    ))
                },
            },
        );
        built_ins_map.insert(
            "avg",
            BuiltInFunctionDef {
                name: String::from("avg"),
                arity: FunctionArity::AtLeast(2),
                body: |args, _| {
                    Ok(Value::Number(
                        args.iter()
                            .map(|a| a.as_number())
                            .collect::<Result<Vec<f64>>>()?
                            .iter()
                            .sum::<f64>()
                            / args.len() as f64,
                    ))
                },
            },
        );
        built_ins_map.insert(
            "sum",
            BuiltInFunctionDef {
                name: String::from("sum"),
                arity: FunctionArity::AtLeast(2),
                body: |args, _| {
                    Ok(Value::Number(
                        args.iter()
                            .map(|a| a.as_number())
                            .collect::<Result<Vec<f64>>>()?
                            .iter()
                            .sum(),
                    ))
                },
            },
        );
        built_ins_map.insert(
            "prod",
            BuiltInFunctionDef {
                name: String::from("prod"),
                arity: FunctionArity::AtLeast(2),
                body: |args, _| {
                    Ok(Value::Number(
                        args.iter()
                            .map(|a| a.as_number())
                            .collect::<Result<Vec<f64>>>()?
                            .iter()
                            .product(),
                    ))
                },
            },
        );
        built_ins_map.insert(
            "median",
            BuiltInFunctionDef {
                name: String::from("median"),
                arity: FunctionArity::AtLeast(2),
                body: |args, _| {
                    let mut nums = args
                        .into_iter()
                        .map(|a| a.as_number())
                        .collect::<Result<Vec<f64>>>()?;

                    nums.sort_by(|a, b| a.partial_cmp(b).unwrap());
                    let len = nums.len();
                    if len % 2 == 0 {
                        Ok(Value::Number((nums[len / 2 - 1] + nums[len / 2]) / 2.0))
                    } else {
                        Ok(Value::Number(nums[len / 2]))
                    }
                },
            },
        );
        built_ins_map.insert(
            "range",
            BuiltInFunctionDef {
                name: String::from("range"),
                arity: FunctionArity::Exact(1),
                body: |args, _| {
                    let n = args[0].as_number()? as i64;
                    Ok(Value::List(
                        (0..n).map(|e| Value::Number(e as f64)).collect(),
                    ))
                },
            },
        );
        built_ins_map.insert(
            "len",
            BuiltInFunctionDef {
                name: String::from("len"),
                arity: FunctionArity::Exact(1),
                body: |args, _| Ok(Value::Number(args[0].as_list()?.len() as f64)),
            },
        );
        built_ins_map.insert(
            "head",
            BuiltInFunctionDef {
                name: String::from("head"),
                arity: FunctionArity::Exact(1),
                body: |args, _| {
                    Ok(args[0]
                        .as_list()?
                        .first()
                        .cloned()
                        .unwrap_or(Value::List(vec![])))
                },
            },
        );
        built_ins_map.insert(
            "tail",
            BuiltInFunctionDef {
                name: String::from("tail"),
                arity: FunctionArity::Exact(1),
                body: |args, _| {
                    let list = args[0].as_list()?;
                    Ok(Value::List(list.iter().skip(1).cloned().collect()))
                },
            },
        );
        built_ins_map.insert(
            "slice",
            BuiltInFunctionDef {
                name: String::from("slice"),
                arity: FunctionArity::Exact(3),
                body: |args, _| {
                    let list = args[0].as_list()?;
                    let start = args[1].as_number()? as usize;
                    let end = args[2].as_number()? as usize;

                    Ok(Value::List(list[start..end].to_vec()))
                },
            },
        );
        built_ins_map.insert(
            "concat",
            BuiltInFunctionDef {
                name: String::from("concat"),
                arity: FunctionArity::AtLeast(2),
                body: |args, _| {
                    let mut list = vec![];
                    for arg in args {
                        match arg {
                            Value::List(l) => list.extend(l),
                            Value::Number(_) => list.push(arg),
                            Value::Spread(SpreadValue::List(l)) => list.extend(l),
                            Value::Spread(SpreadValue::String(s)) => {
                                list.extend(s.chars().map(|c| Value::String(c.to_string())))
                            }
                            Value::Bool(_) => list.push(arg),
                            Value::Lambda(_) => list.push(arg),
                            Value::String(_) => list.push(arg),
                        }
                    }

                    Ok(Value::List(list))
                },
            },
        );
        built_ins_map.insert(
            "dot",
            BuiltInFunctionDef {
                name: String::from("dot"),
                arity: FunctionArity::Exact(2),
                body: |args, _| {
                    let a = args[0].as_list()?;
                    let b = args[1].as_list()?;

                    if a.len() != b.len() {
                        return Err(anyhow!(
                            "cannot calculate dot product of lists with different lengths"
                        ));
                    }

                    Ok(Value::Number(
                        a.iter()
                            .zip(b.iter())
                            .map(|(a, b)| {
                                let a_num = a.as_number()?;
                                let b_num = b.as_number()?;
                                Ok(a_num * b_num)
                            })
                            .collect::<Result<Vec<f64>>>()?
                            .iter()
                            .sum(),
                    ))
                },
            },
        );
        built_ins_map.insert(
            "unique",
            BuiltInFunctionDef {
                name: String::from("unique"),
                arity: FunctionArity::Exact(1),
                body: |args, _| {
                    let list = args[0].as_list()?.clone();
                    let mut unique_list = vec![];

                    for item in list {
                        if !unique_list.contains(&item) {
                            unique_list.push(item);
                        }
                    }

                    Ok(Value::List(unique_list))
                },
            },
        );
        built_ins_map.insert(
            "map",
            BuiltInFunctionDef {
                name: String::from("map"),
                arity: FunctionArity::Exact(2),
                body: |args, variables| {
                    let lambda = FunctionDef::Lambda(args[0].as_lambda()?);
                    let parsed_body = lambda.get_parsed_body();

                    let list = args[1].as_list()?;
                    let mut new_list = vec![];

                    for item in list {
                        new_list.push(lambda.call(
                            vec![item.clone()],
                            variables,
                            parsed_body.clone(),
                        )?);
                    }

                    Ok(Value::List(new_list))
                },
            },
        );
        built_ins_map.insert(
            "reduce",
            BuiltInFunctionDef {
                name: String::from("reduce"),
                arity: FunctionArity::Exact(3),
                body: |args, variables| {
                    let lambda = FunctionDef::Lambda(args[0].as_lambda()?);
                    let parsed_body = lambda.get_parsed_body();

                    let list = args[1].as_list()?;
                    let initial = args[2].clone();

                    let mut acc = initial;
                    for item in list {
                        acc =
                            lambda.call(vec![acc, item.clone()], variables, parsed_body.clone())?;
                    }

                    Ok(acc)
                },
            },
        );
        built_ins_map.insert(
            "filter",
            BuiltInFunctionDef {
                name: String::from("filter"),
                arity: FunctionArity::Exact(2),
                body: |args, variables| {
                    let lambda = FunctionDef::Lambda(args[0].as_lambda()?);
                    let parsed_body = lambda.get_parsed_body();

                    let list = args[1].as_list()?;
                    let mut new_list = vec![];

                    for item in list {
                        if lambda
                            .call(vec![item.clone()], variables, parsed_body.clone())?
                            .as_bool()?
                        {
                            new_list.push(item.clone());
                        }
                    }

                    Ok(Value::List(new_list))
                },
            },
        );
        built_ins_map.insert(
            "every",
            BuiltInFunctionDef {
                name: String::from("every"),
                arity: FunctionArity::Exact(2),
                body: |args, variables| {
                    let lambda = FunctionDef::Lambda(args[0].as_lambda()?);
                    let parsed_body = lambda.get_parsed_body();

                    let list = args[1].as_list()?;

                    for item in list {
                        if !lambda
                            .call(vec![item.clone()], variables, parsed_body.clone())?
                            .as_bool()?
                        {
                            return Ok(Value::Bool(false));
                        }
                    }

                    Ok(Value::Bool(true))
                },
            },
        );
        built_ins_map.insert(
            "some",
            BuiltInFunctionDef {
                name: String::from("some"),
                arity: FunctionArity::Exact(2),
                body: |args, variables| {
                    let lambda = FunctionDef::Lambda(args[0].as_lambda()?);
                    let parsed_body = lambda.get_parsed_body();

                    let list = args[1].as_list()?;

                    for item in list {
                        if lambda
                            .call(vec![item.clone()], variables, parsed_body.clone())?
                            .as_bool()?
                        {
                            return Ok(Value::Bool(true));
                        }
                    }

                    Ok(Value::Bool(false))
                },
            },
        );
        built_ins_map.insert(
            "none",
            BuiltInFunctionDef {
                name: String::from("none"),
                arity: FunctionArity::Exact(2),
                body: |args, variables| {
                    let lambda = FunctionDef::Lambda(args[0].as_lambda()?);
                    let parsed_body = lambda.get_parsed_body();

                    let list = args[1].as_list()?;

                    for item in list {
                        if lambda
                            .call(vec![item.clone()], variables, parsed_body.clone())?
                            .as_bool()?
                        {
                            return Ok(Value::Bool(false));
                        }
                    }

                    Ok(Value::Bool(true))
                },
            },
        );
        built_ins_map.insert(
            "sort",
            BuiltInFunctionDef {
                name: String::from("sort"),
                arity: FunctionArity::Exact(1),
                body: |args, _| {
                    let mut list = args[0].as_list()?.clone();
                    list.sort_by(|a, b| a.partial_cmp(b).unwrap());
                    Ok(Value::List(list))
                },
            },
        );
        built_ins_map.insert(
            "reverse",
            BuiltInFunctionDef {
                name: String::from("reverse"),
                arity: FunctionArity::Exact(1),
                body: |args, _| {
                    let mut list = args[0].as_list()?.clone();
                    list.reverse();
                    Ok(Value::List(list))
                },
            },
        );
        built_ins_map.insert(
            "split",
            BuiltInFunctionDef {
                name: String::from("split"),
                arity: FunctionArity::Exact(2),
                body: |args, _| {
                    let delimeter = args[0].as_string()?;
                    let s = args[1].as_string()?;

                    Ok(Value::List(
                        s.split(&delimeter)
                            .map(|s| Value::String(s.to_string()))
                            .collect(),
                    ))
                },
            },
        );
        built_ins_map.insert(
            "join",
            BuiltInFunctionDef {
                name: String::from("join"),
                arity: FunctionArity::Exact(2),
                body: |args, _| {
                    let delimeter = args[0].as_string()?;
                    let list = args[1].as_list()?;

                    Ok(Value::String(
                        list.iter()
                            .map(|v| v.stringify())
                            .collect::<Vec<String>>()
                            .join(&delimeter),
                    ))
                },
            },
        );
        built_ins_map.insert(
            "replace",
            BuiltInFunctionDef {
                name: String::from("replace"),
                arity: FunctionArity::Exact(3),
                body: |args, _| {
                    let old = args[0].as_string()?;
                    let new = args[1].as_string()?;
                    let s = args[2].as_string()?;

                    Ok(Value::String(s.replace(old, new)))
                },
            },
        );
        built_ins_map.insert(
            "typeof",
            BuiltInFunctionDef {
                name: String::from("typeof"),
                arity: FunctionArity::Exact(1),
                body: |args, _| Ok(Value::String(String::from(args[0].get_type()))),
            },
        );

        built_ins_map
    });

pub static BUILT_IN_FUNCTION_IDENTS: LazyLock<Vec<&str>> =
    LazyLock::new(|| BUILT_IN_FUNCTION_DEFS.keys().copied().collect());

impl<'a> FunctionDef<'a> {
    pub fn check_arity(&self, arg_count: usize) -> Result<()> {
        match self {
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
            FunctionDef::Lambda(LambdaDef { args, name, .. }) => {
                if arg_count == args.len() {
                    Ok(())
                } else {
                    let ident = name
                        .clone()
                        .map_or(String::from("anonymous function"), |n| {
                            format!("function {}", n)
                        });

                    let expected_args = if args.len() == 1 {
                        "1 argument".to_string()
                    } else {
                        format!("{} arguments", args.len())
                    };

                    let given_args = if arg_count == 1 {
                        "1 was given".to_string()
                    } else {
                        format!("{} were given", arg_count)
                    };

                    Err(anyhow!(
                        "{} takes {}, but {}",
                        ident,
                        expected_args,
                        given_args
                    ))
                }
            }
        }
    }

    pub fn get_parsed_body(&self) -> Option<Result<Pairs<Rule>, pest::error::Error<Rule>>> {
        match self {
            FunctionDef::BuiltIn(_) => None,
            FunctionDef::Lambda(LambdaDef { body, .. }) => Some(get_pairs(body)),
        }
    }

    pub fn call(
        &self,
        args: Vec<Value>,
        variables: &HashMap<String, Value>,
        parsed_body: Option<Result<Pairs<Rule>, pest::error::Error<Rule>>>,
    ) -> Result<Value> {
        self.check_arity(args.len())?;

        match self {
            FunctionDef::Lambda(LambdaDef {
                args: expected_args,
                body,
                scope,
                ..
            }) => {
                let mut new_variables = variables.clone();

                for (var, val) in scope.iter() {
                    new_variables.insert(var.clone(), val.clone());
                }

                for (arg, value) in expected_args.iter().zip(args.iter()) {
                    new_variables.insert(arg.clone(), value.clone());
                }

                evaluate_expression(
                    parsed_body
                        .unwrap_or_else(|| get_pairs(body))?
                        .next()
                        .unwrap()
                        .into_inner(),
                    &mut new_variables,
                )
            }
            FunctionDef::BuiltIn(BuiltInFunctionDef { body, .. }) => body(args, variables),
        }
    }
}

pub fn is_built_in_function(name: &str) -> bool {
    BUILT_IN_FUNCTION_DEFS.contains_key(name)
}

pub fn get_built_in_function_def(name: &str) -> Option<&BuiltInFunctionDef> {
    BUILT_IN_FUNCTION_DEFS.get(name)
}

pub fn get_function_def<'a>(
    name: &'a str,
    variables: &'a HashMap<String, Value>,
) -> Option<FunctionDef<'a>> {
    variables
        .get(name)
        .and_then(|v| v.as_lambda().ok().map(|v| FunctionDef::Lambda(v)))
        .or_else(|| get_built_in_function_def(name).map(|def| FunctionDef::BuiltIn(def)))
}
