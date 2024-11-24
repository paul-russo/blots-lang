use anyhow::{anyhow, Result};
use dyn_fmt::AsStrFormatExt;
use pest::iterators::Pairs;
use std::{cell::RefCell, cmp::Ordering, collections::HashMap, rc::Rc, sync::LazyLock};

use crate::{
    expressions::evaluate_expression,
    parser::{get_pairs, Rule},
    values::{FunctionArity, IterableValue, LambdaArg, LambdaDef, Value},
};

pub struct BuiltInFunctionDef {
    pub name: String,
    pub arity: FunctionArity,
    pub body: fn(
        args: Vec<Value>,
        variables: Rc<RefCell<HashMap<String, Value>>>,
        call_depth: usize,
    ) -> Result<Value>,
}

pub enum FunctionDef<'a> {
    BuiltIn(&'a BuiltInFunctionDef),
    Lambda(&'a LambdaDef),
}

pub static BUILT_IN_FUNCTION_DEFS: LazyLock<HashMap<&str, BuiltInFunctionDef>> = LazyLock::new(
    || {
        let mut built_ins_map = HashMap::new();

        built_ins_map.insert(
            "sqrt",
            BuiltInFunctionDef {
                name: String::from("sqrt"),
                arity: FunctionArity::Exact(1),
                body: |args, _, _| Ok(Value::Number(args[0].as_number()?.sqrt())),
            },
        );
        built_ins_map.insert(
            "sin",
            BuiltInFunctionDef {
                name: String::from("sin"),
                arity: FunctionArity::Exact(1),
                body: |args, _, _| Ok(Value::Number(args[0].as_number()?.sin())),
            },
        );
        built_ins_map.insert(
            "cos",
            BuiltInFunctionDef {
                name: String::from("cos"),
                arity: FunctionArity::Exact(1),
                body: |args, _, _| Ok(Value::Number(args[0].as_number()?.cos())),
            },
        );
        built_ins_map.insert(
            "tan",
            BuiltInFunctionDef {
                name: String::from("tan"),
                arity: FunctionArity::Exact(1),
                body: |args, _, _| Ok(Value::Number(args[0].as_number()?.tan())),
            },
        );
        built_ins_map.insert(
            "asin",
            BuiltInFunctionDef {
                name: String::from("asin"),
                arity: FunctionArity::Exact(1),
                body: |args, _, _| Ok(Value::Number(args[0].as_number()?.asin())),
            },
        );
        built_ins_map.insert(
            "acos",
            BuiltInFunctionDef {
                name: String::from("acos"),
                arity: FunctionArity::Exact(1),
                body: |args, _, _| Ok(Value::Number(args[0].as_number()?.acos())),
            },
        );
        built_ins_map.insert(
            "atan",
            BuiltInFunctionDef {
                name: String::from("atan"),
                arity: FunctionArity::Exact(1),
                body: |args, _, _| Ok(Value::Number(args[0].as_number()?.atan())),
            },
        );
        built_ins_map.insert(
            "log",
            BuiltInFunctionDef {
                name: String::from("log"),
                arity: FunctionArity::Exact(1),
                body: |args, _, _| Ok(Value::Number(args[0].as_number()?.ln())),
            },
        );
        built_ins_map.insert(
            "log10",
            BuiltInFunctionDef {
                name: String::from("log10"),
                arity: FunctionArity::Exact(1),
                body: |args, _, _| Ok(Value::Number(args[0].as_number()?.log10())),
            },
        );
        built_ins_map.insert(
            "exp",
            BuiltInFunctionDef {
                name: String::from("exp"),
                arity: FunctionArity::Exact(1),
                body: |args, _, _| Ok(Value::Number(args[0].as_number()?.exp())),
            },
        );
        built_ins_map.insert(
            "abs",
            BuiltInFunctionDef {
                name: String::from("abs"),
                arity: FunctionArity::Exact(1),
                body: |args, _, _| Ok(Value::Number(args[0].as_number()?.abs())),
            },
        );
        built_ins_map.insert(
            "floor",
            BuiltInFunctionDef {
                name: String::from("floor"),
                arity: FunctionArity::Exact(1),
                body: |args, _, _| Ok(Value::Number(args[0].as_number()?.floor())),
            },
        );
        built_ins_map.insert(
            "ceil",
            BuiltInFunctionDef {
                name: String::from("ceil"),
                arity: FunctionArity::Exact(1),
                body: |args, _, _| Ok(Value::Number(args[0].as_number()?.ceil())),
            },
        );
        built_ins_map.insert(
            "round",
            BuiltInFunctionDef {
                name: String::from("round"),
                arity: FunctionArity::Exact(1),
                body: |args, _, _| Ok(Value::Number(args[0].as_number()?.round())),
            },
        );
        built_ins_map.insert(
            "trunc",
            BuiltInFunctionDef {
                name: String::from("trunc"),
                arity: FunctionArity::Exact(1),
                body: |args, _, _| Ok(Value::Number(args[0].as_number()?.trunc())),
            },
        );
        built_ins_map.insert(
            "min",
            BuiltInFunctionDef {
                name: String::from("min"),
                arity: FunctionArity::AtLeast(2),
                body: |args, _, _| {
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
                body: |args, _, _| {
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
                body: |args, _, _| {
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
                body: |args, _, _| {
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
                body: |args, _, _| {
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
                body: |args, _, _| {
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
                body: |args, _, _| {
                    let n = args[0].as_number()? as i64;

                    if n > 10_000 {
                        return Err(anyhow!("range must be no larger than 10,000"));
                    }

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
                body: |args, _, _| match &args[0] {
                    Value::List(l) => Ok(Value::Number(l.len() as f64)),
                    Value::String(s) => Ok(Value::Number(s.len() as f64)),
                    _ => Err(anyhow!("argument must be a list or string")),
                },
            },
        );
        built_ins_map.insert(
            "head",
            BuiltInFunctionDef {
                name: String::from("head"),
                arity: FunctionArity::Exact(1),
                body: |args, _, _| match &args[0] {
                    Value::List(l) => Ok(l.first().cloned().unwrap_or(Value::List(vec![]))),
                    Value::String(s) => Ok(Value::String(s.get(0..1).unwrap_or("").to_string())),
                    _ => Err(anyhow!("argument must be a list or string")),
                },
            },
        );
        built_ins_map.insert(
            "tail",
            BuiltInFunctionDef {
                name: String::from("tail"),
                arity: FunctionArity::Exact(1),
                body: |args, _, _| match &args[0] {
                    Value::List(l) => Ok(Value::List(l.get(1..).unwrap_or([].as_slice()).to_vec())),
                    Value::String(s) => Ok(Value::String(s.get(1..).unwrap_or("").to_string())),
                    _ => Err(anyhow!("argument must be a list or string")),
                },
            },
        );
        built_ins_map.insert(
            "slice",
            BuiltInFunctionDef {
                name: String::from("slice"),
                arity: FunctionArity::Exact(3),
                body: |args, _, _| {
                    let start = args[0].as_number()? as usize;
                    let end = args[1].as_number()? as usize;

                    match &args[2] {
                        Value::List(l) => l
                            .get(start..end)
                            .map_or(Err(anyhow!("index out of bounds")), |l| {
                                Ok(Value::List(l.to_vec()))
                            }),
                        Value::String(s) => s
                            .get(start..end)
                            .map_or(Err(anyhow!("index out of bounds")), |s| {
                                Ok(Value::String(s.to_string()))
                            }),
                        _ => Err(anyhow!("argument must be a list or string")),
                    }
                },
            },
        );
        built_ins_map.insert(
            "concat",
            BuiltInFunctionDef {
                name: String::from("concat"),
                arity: FunctionArity::AtLeast(2),
                body: |args, _, _| {
                    let mut list = vec![];
                    for arg in args {
                        match arg {
                            Value::List(l) => list.extend(l),
                            Value::Spread(IterableValue::List(l)) => list.extend(l),
                            Value::Spread(IterableValue::String(s)) => {
                                list.extend(s.chars().map(|c| Value::String(c.to_string())))
                            }
                            _ => list.push(arg),
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
                body: |args, _, _| {
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
                body: |args, _, _| {
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
                body: |args, variables, call_depth| {
                    let def = get_function_def(&args[0])
                        .ok_or(anyhow!("expected the first argument to be a function"))?;
                    let parsed_body = def.get_parsed_body();

                    let list = args[1].as_list()?;
                    let mut new_list = vec![];

                    for (i, item) in list.iter().enumerate() {
                        new_list.push(def.call(
                            if !def.check_arity(2).is_err() {
                                vec![item.clone(), Value::Number(i as f64)]
                            } else {
                                vec![item.clone()]
                            },
                            Rc::clone(&variables),
                            parsed_body.clone(),
                            call_depth,
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
                body: |args, variables, call_depth| {
                    let def = get_function_def(&args[0])
                        .ok_or(anyhow!("expected the first argument to be a function"))?;
                    let parsed_body = def.get_parsed_body();

                    let list = args[1].as_list()?;
                    let initial = args[2].clone();

                    let mut acc = initial;
                    for (i, item) in list.iter().enumerate() {
                        acc = def.call(
                            if !def.check_arity(3).is_err() {
                                vec![acc, item.clone(), Value::Number(i as f64)]
                            } else {
                                vec![acc, item.clone()]
                            },
                            Rc::clone(&variables),
                            parsed_body.clone(),
                            call_depth,
                        )?;
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
                body: |args, variables, call_depth| {
                    let def = get_function_def(&args[0])
                        .ok_or(anyhow!("expected the first argument to be a function"))?;
                    let parsed_body = def.get_parsed_body();

                    let list = args[1].as_list()?;
                    let mut new_list = vec![];

                    for (i, item) in list.iter().enumerate() {
                        if def
                            .call(
                                if !def.check_arity(2).is_err() {
                                    vec![item.clone(), Value::Number(i as f64)]
                                } else {
                                    vec![item.clone()]
                                },
                                Rc::clone(&variables),
                                parsed_body.clone(),
                                call_depth,
                            )?
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
                body: |args, variables, call_depth| {
                    let def = get_function_def(&args[0])
                        .ok_or(anyhow!("expected the first argument to be a function"))?;
                    let parsed_body = def.get_parsed_body();

                    let list = args[1].as_list()?;

                    for (i, item) in list.iter().enumerate() {
                        if !def
                            .call(
                                if !def.check_arity(2).is_err() {
                                    vec![item.clone(), Value::Number(i as f64)]
                                } else {
                                    vec![item.clone()]
                                },
                                Rc::clone(&variables),
                                parsed_body.clone(),
                                call_depth,
                            )?
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
                body: |args, variables, call_depth| {
                    let def = get_function_def(&args[0])
                        .ok_or(anyhow!("expected the first argument to be a function"))?;
                    let parsed_body = def.get_parsed_body();

                    let list = args[1].as_list()?;

                    for (i, item) in list.iter().enumerate() {
                        if def
                            .call(
                                if !def.check_arity(2).is_err() {
                                    vec![item.clone(), Value::Number(i as f64)]
                                } else {
                                    vec![item.clone()]
                                },
                                Rc::clone(&variables),
                                parsed_body.clone(),
                                call_depth,
                            )?
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
            "sort",
            BuiltInFunctionDef {
                name: String::from("sort"),
                arity: FunctionArity::Exact(1),
                body: |args, _, _| {
                    let mut list = args[0].as_list()?.clone();
                    list.sort_by(|a, b| a.partial_cmp(b).unwrap());
                    Ok(Value::List(list))
                },
            },
        );
        built_ins_map.insert(
            "sort_by",
            BuiltInFunctionDef {
                name: String::from("sort_by"),
                arity: FunctionArity::Exact(2),
                body: |args, variables, call_depth| {
                    let def = get_function_def(&args[0])
                        .ok_or(anyhow!("expected the first argument to be a function"))?;
                    let parsed_body = def.get_parsed_body();

                    let mut list = args[1].as_list()?.clone();
                    let mut err: Option<Result<Value, anyhow::Error>> = None;

                    list.sort_by(|a, b| {
                        let call_result = def.call(
                            vec![a.clone(), b.clone()],
                            Rc::clone(&variables),
                            parsed_body.clone(),
                            call_depth,
                        );

                        if let Err(e) = call_result {
                            if err.is_none() {
                                err = Some(Err(e));
                            }

                            return Ordering::Equal;
                        }

                        let number_result = call_result.unwrap().as_number();
                        if let Err(e) = number_result {
                            if err.is_none() {
                                err = Some(Err(e));
                            }

                            return Ordering::Equal;
                        }

                        match number_result.unwrap() {
                            n if n.is_sign_positive() => Ordering::Greater,
                            n if n.is_sign_negative() => Ordering::Less,
                            _ => Ordering::Equal,
                        }
                    });

                    err.unwrap_or(Ok(Value::List(list)))
                },
            },
        );
        built_ins_map.insert(
            "reverse",
            BuiltInFunctionDef {
                name: String::from("reverse"),
                arity: FunctionArity::Exact(1),
                body: |args, _, _| {
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
                body: |args, _, _| {
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
                body: |args, _, _| {
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
                body: |args, _, _| {
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
                body: |args, _, _| Ok(Value::String(args[0].get_type().to_string())),
            },
        );
        built_ins_map.insert(
            "percentile",
            BuiltInFunctionDef {
                name: String::from("percentile"),
                arity: FunctionArity::Exact(2),
                body: |args, _, _| {
                    let p = args[0].as_number()?;
                    let list = args[1].as_list()?;

                    if p < 0.0 || p > 100.0 {
                        return Err(anyhow!("percentile must be between 0 and 100"));
                    }

                    let mut nums = list
                        .iter()
                        .map(|a| a.as_number())
                        .collect::<Result<Vec<f64>>>()?;

                    nums.sort_by(|a, b| a.partial_cmp(b).unwrap());
                    let index = (p / 100.0 * (nums.len() - 1) as f64).round() as usize;

                    Ok(Value::Number(nums[index]))
                },
            },
        );
        built_ins_map.insert(
            "uppercase",
            BuiltInFunctionDef {
                name: String::from("uppercase"),
                arity: FunctionArity::Exact(1),
                body: |args, _, _| Ok(Value::String(args[0].as_string()?.to_uppercase())),
            },
        );
        built_ins_map.insert(
            "lowercase",
            BuiltInFunctionDef {
                name: String::from("lowercase"),
                arity: FunctionArity::Exact(1),
                body: |args, _, _| Ok(Value::String(args[0].as_string()?.to_lowercase())),
            },
        );
        built_ins_map.insert(
            "to_string",
            BuiltInFunctionDef {
                name: String::from("to_string"),
                arity: FunctionArity::Exact(1),
                body: |args, _, _| Ok(Value::String(args[0].stringify())),
            },
        );
        built_ins_map.insert(
            "to_number",
            BuiltInFunctionDef {
                name: String::from("to_number"),
                arity: FunctionArity::Exact(1),
                body: |args, _, _| Ok(Value::Number(args[0].as_string()?.parse()?)),
            },
        );
        built_ins_map.insert(
            "arity",
            BuiltInFunctionDef {
                name: String::from("arity"),
                arity: FunctionArity::Exact(1),
                body: |args, _, _| {
                    Ok(Value::Number(match args[0].as_lambda()?.get_arity() {
                        FunctionArity::Exact(n) => n as f64,
                        FunctionArity::AtLeast(n) => n as f64,
                        FunctionArity::Between(min, _max) => min as f64,
                    }))
                },
            },
        );
        built_ins_map.insert(
            "includes",
            BuiltInFunctionDef {
                name: String::from("includes"),
                arity: FunctionArity::Exact(2),
                body: |args, _, _| {
                    let needle = &args[0];

                    match &args[1] {
                        Value::List(l) => Ok(Value::Bool(l.contains(needle))),
                        Value::String(s) => Ok(Value::Bool(s.contains(needle.as_string()?))),
                        _ => Err(anyhow!("second argument must be a list or string")),
                    }
                },
            },
        );
        built_ins_map.insert(
            "collect",
            BuiltInFunctionDef {
                name: String::from("collect"),
                arity: FunctionArity::Exact(1),
                body: |args, _, _| {
                    let iterable = args[0].as_each()?.to_owned();
                    let values: Vec<Value> = iterable.into_iter().collect();
                    Ok(Value::List(values))
                },
            },
        );

        built_ins_map.insert(
            "format",
            BuiltInFunctionDef {
                name: String::from("format"),
                arity: FunctionArity::AtLeast(1),
                body: |args, _, _| {
                    let format_str = args[0]
                        .as_string()
                        .map_err(|_| anyhow!("first argument must be a string"))?;

                    let format_args = &args[1..]
                        .into_iter()
                        .map(|v| v.stringify())
                        .collect::<Vec<String>>();

                    Ok(Value::String(format_str.format(format_args)))
                },
            },
        );

        #[cfg(not(target_arch = "wasm32"))]
        built_ins_map.insert(
            "print",
            BuiltInFunctionDef {
                name: String::from("print"),
                arity: FunctionArity::AtLeast(1),
                body: |args, _, _| {
                    let output = if args.len() == 1 {
                        args[0].stringify()
                    } else {
                        let format_str = args[0].as_string().map_err(|_| {
                            anyhow!("first argument must be a formatting string if multiple arguments are given")
                        })?;
                        let format_args = &args[1..].into_iter().map(|v| v.stringify()).collect::<Vec<String>>();
                        format_str.format(format_args)
                    };

                    println!("{}", output);

                    Ok(Value::Null)
                },
            },
        );

        built_ins_map
    },
);

pub static BUILT_IN_FUNCTION_IDENTS: LazyLock<Vec<&str>> =
    LazyLock::new(|| BUILT_IN_FUNCTION_DEFS.keys().copied().collect());

impl<'a> FunctionDef<'a> {
    pub fn get_name(&self) -> String {
        match self {
            FunctionDef::BuiltIn(BuiltInFunctionDef { name, .. }) => {
                format!("built-in function \"{}\"", name)
            }
            FunctionDef::Lambda(LambdaDef { name, .. }) => name
                .clone()
                .map_or(String::from("anonymous function"), |n| {
                    format!("function \"{}\"", n)
                }),
        }
    }

    pub fn check_arity(&self, arg_count: usize) -> Result<()> {
        match self {
            FunctionDef::BuiltIn(BuiltInFunctionDef { arity, .. }) => match arity {
                FunctionArity::Exact(expected) => {
                    if arg_count == *expected {
                        Ok(())
                    } else {
                        Err(anyhow!(
                            "{} takes exactly {} arguments, but {} were given",
                            self.get_name(),
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
                            "{} takes at least {} arguments, but {} were given",
                            self.get_name(),
                            expected,
                            arg_count
                        ))
                    }
                }
                FunctionArity::Between(min, max) => {
                    if arg_count >= *min && arg_count <= *max {
                        Ok(())
                    } else {
                        Err(anyhow!(
                            "{} takes between {} and {} arguments, but {} were given",
                            self.get_name(),
                            min,
                            max,
                            arg_count
                        ))
                    }
                }
            },
            FunctionDef::Lambda(def) => {
                let arity = def.get_arity();

                match arity {
                    FunctionArity::Exact(expected) => {
                        if arg_count == expected {
                            Ok(())
                        } else {
                            Err(anyhow!(
                                "{} takes exactly {} arguments, but {} were given",
                                self.get_name(),
                                expected,
                                arg_count
                            ))
                        }
                    }
                    FunctionArity::AtLeast(expected) => {
                        if arg_count >= expected {
                            Ok(())
                        } else {
                            Err(anyhow!(
                                "{} takes at least {} arguments, but {} were given",
                                self.get_name(),
                                expected,
                                arg_count
                            ))
                        }
                    }
                    FunctionArity::Between(min, max) => {
                        if arg_count >= min && arg_count <= max {
                            Ok(())
                        } else {
                            Err(anyhow!(
                                "{} takes between {} and {} arguments, but {} were given",
                                self.get_name(),
                                min,
                                max,
                                arg_count
                            ))
                        }
                    }
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
        variables: Rc<RefCell<HashMap<String, Value>>>,
        parsed_body: Option<Result<Pairs<Rule>, pest::error::Error<Rule>>>,
        call_depth: usize,
    ) -> Result<Value> {
        self.check_arity(args.len())?;

        if call_depth > 100 {
            return Err(anyhow!(
                "in {}: maximum call depth of 100 exceeded",
                self.get_name()
            ));
        }

        match self {
            FunctionDef::Lambda(LambdaDef {
                args: expected_args,
                body,
                scope,
                ..
            }) => {
                let mut new_variables = variables.borrow_mut().clone();

                for (var, val) in scope.iter() {
                    new_variables.insert(var.clone(), val.clone());
                }

                for (idx, expected_arg) in expected_args.iter().enumerate() {
                    match expected_arg {
                        LambdaArg::Required(name) => {
                            new_variables.insert(name.clone(), args[idx].clone());
                        }
                        LambdaArg::Optional(name) => {
                            new_variables.insert(
                                name.clone(),
                                args.get(idx).cloned().unwrap_or(Value::Null),
                            );
                        }
                        LambdaArg::Rest(name) => {
                            new_variables.insert(
                                name.clone(),
                                Value::List(args.iter().skip(idx).cloned().collect()),
                            );
                        }
                    }
                }

                evaluate_expression(
                    parsed_body
                        .unwrap_or_else(|| get_pairs(body))?
                        .next()
                        .unwrap()
                        .into_inner(),
                    Rc::new(RefCell::new(new_variables)),
                    call_depth + 1,
                )
                .map_err(|error| anyhow!("in {}: {}", self.get_name(), error))
            }
            FunctionDef::BuiltIn(BuiltInFunctionDef { body, .. }) => {
                body(args, variables, call_depth + 1)
                    .map_err(|error| anyhow!("in {}: {}", self.get_name(), error))
            }
        }
    }
}

pub fn is_built_in_function(name: &str) -> bool {
    BUILT_IN_FUNCTION_DEFS.contains_key(name)
}

pub fn get_built_in_function_def(name: &str) -> Option<FunctionDef> {
    BUILT_IN_FUNCTION_DEFS
        .get(name)
        .map(|def| FunctionDef::BuiltIn(def))
}

pub fn get_function_def<'a>(value: &'a Value) -> Option<FunctionDef<'a>> {
    match value {
        Value::Lambda(lambda) => Some(FunctionDef::Lambda(lambda)),
        Value::BuiltIn(ident) => Some(get_built_in_function_def(ident)?),
        _ => None,
    }
}
