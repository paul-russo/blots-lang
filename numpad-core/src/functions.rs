use crate::{
    expressions::evaluate_expression,
    heap::{Heap, HeapPointer},
    parser::{get_pairs, Rule},
    values::{FunctionArity, LambdaArg, LambdaDef, ReifiedValue, Value},
};
use anyhow::{anyhow, Result};
use dyn_fmt::AsStrFormatExt;
use pest::iterators::Pairs;
use std::{cell::RefCell, collections::HashMap, rc::Rc, sync::LazyLock};

#[cfg(not(target_arch = "wasm32"))]
use std::sync::Mutex;

#[cfg(not(target_arch = "wasm32"))]
use crate::stats::FunctionCallStats;

#[cfg(not(target_arch = "wasm32"))]
pub static FUNCTION_CALLS: LazyLock<Mutex<Vec<FunctionCallStats>>> =
    LazyLock::new(|| Mutex::new(Vec::new()));

#[derive(Debug)]
pub struct BuiltInFunctionDef {
    pub name: String,
    pub arity: FunctionArity,
    pub body: fn(
        args: Vec<Value>,
        heap: Rc<RefCell<Heap>>,
        bindings: Rc<RefCell<HashMap<String, Value>>>,
        call_depth: usize,
    ) -> Result<Value>,
}

#[derive(Debug, Clone)]
pub enum FunctionDef<'a> {
    BuiltIn(&'a BuiltInFunctionDef),
    Lambda(LambdaDef),
}

pub struct BuiltInFunctionDefs<'a> {
    pub map: HashMap<&'a str, BuiltInFunctionDef>,
    pub idents_to_ids: HashMap<&'a str, usize>,
    pub idents: Vec<&'a str>,
}

impl<'a> BuiltInFunctionDefs<'a> {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            idents_to_ids: HashMap::new(),
            idents: Vec::new(),
        }
    }

    pub fn insert(&mut self, ident: &'a str, def: BuiltInFunctionDef) {
        let id = self.idents.len();
        self.map.insert(ident, def);
        self.idents_to_ids.insert(ident, id);
        self.idents.push(ident);
    }

    pub fn get(&self, ident: &str) -> Option<&BuiltInFunctionDef> {
        self.map.get(ident)
    }

    pub fn get_ident(&self, id: usize) -> Option<&str> {
        self.idents.get(id).copied()
    }

    pub fn get_id(&self, ident: &str) -> Option<usize> {
        self.idents_to_ids.get(ident).copied()
    }

    pub fn get_idents(&self) -> Vec<&str> {
        self.idents.clone()
    }
}

static BUILT_IN_FUNCTION_DEFS: LazyLock<BuiltInFunctionDefs> = LazyLock::new(|| {
    let mut map = BuiltInFunctionDefs::new();

    map.insert(
        "sqrt",
        BuiltInFunctionDef {
            name: String::from("sqrt"),
            arity: FunctionArity::Exact(1),
            body: |args, _, _, _| Ok(Value::Number(args[0].as_number()?.sqrt())),
        },
    );
    map.insert(
        "sin",
        BuiltInFunctionDef {
            name: String::from("sin"),
            arity: FunctionArity::Exact(1),
            body: |args, _, _, _| Ok(Value::Number(args[0].as_number()?.sin())),
        },
    );
    map.insert(
        "cos",
        BuiltInFunctionDef {
            name: String::from("cos"),
            arity: FunctionArity::Exact(1),
            body: |args, _, _, _| Ok(Value::Number(args[0].as_number()?.cos())),
        },
    );
    map.insert(
        "tan",
        BuiltInFunctionDef {
            name: String::from("tan"),
            arity: FunctionArity::Exact(1),
            body: |args, _, _, _| Ok(Value::Number(args[0].as_number()?.tan())),
        },
    );
    map.insert(
        "asin",
        BuiltInFunctionDef {
            name: String::from("asin"),
            arity: FunctionArity::Exact(1),
            body: |args, _, _, _| Ok(Value::Number(args[0].as_number()?.asin())),
        },
    );
    map.insert(
        "acos",
        BuiltInFunctionDef {
            name: String::from("acos"),
            arity: FunctionArity::Exact(1),
            body: |args, _, _, _| Ok(Value::Number(args[0].as_number()?.acos())),
        },
    );
    map.insert(
        "atan",
        BuiltInFunctionDef {
            name: String::from("atan"),
            arity: FunctionArity::Exact(1),
            body: |args, _, _, _| Ok(Value::Number(args[0].as_number()?.atan())),
        },
    );
    map.insert(
        "log",
        BuiltInFunctionDef {
            name: String::from("log"),
            arity: FunctionArity::Exact(1),
            body: |args, _, _, _| Ok(Value::Number(args[0].as_number()?.ln())),
        },
    );
    map.insert(
        "log10",
        BuiltInFunctionDef {
            name: String::from("log10"),
            arity: FunctionArity::Exact(1),
            body: |args, _, _, _| Ok(Value::Number(args[0].as_number()?.log10())),
        },
    );
    map.insert(
        "exp",
        BuiltInFunctionDef {
            name: String::from("exp"),
            arity: FunctionArity::Exact(1),
            body: |args, _, _, _| Ok(Value::Number(args[0].as_number()?.exp())),
        },
    );
    map.insert(
        "abs",
        BuiltInFunctionDef {
            name: String::from("abs"),
            arity: FunctionArity::Exact(1),
            body: |args, _, _, _| Ok(Value::Number(args[0].as_number()?.abs())),
        },
    );
    map.insert(
        "floor",
        BuiltInFunctionDef {
            name: String::from("floor"),
            arity: FunctionArity::Exact(1),
            body: |args, _, _, _| Ok(Value::Number(args[0].as_number()?.floor())),
        },
    );
    map.insert(
        "ceil",
        BuiltInFunctionDef {
            name: String::from("ceil"),
            arity: FunctionArity::Exact(1),
            body: |args, _, _, _| Ok(Value::Number(args[0].as_number()?.ceil())),
        },
    );
    map.insert(
        "round",
        BuiltInFunctionDef {
            name: String::from("round"),
            arity: FunctionArity::Exact(1),
            body: |args, _, _, _| Ok(Value::Number(args[0].as_number()?.round())),
        },
    );
    map.insert(
        "trunc",
        BuiltInFunctionDef {
            name: String::from("trunc"),
            arity: FunctionArity::Exact(1),
            body: |args, _, _, _| Ok(Value::Number(args[0].as_number()?.trunc())),
        },
    );
    map.insert(
        "min",
        BuiltInFunctionDef {
            name: String::from("min"),
            arity: FunctionArity::AtLeast(2),
            body: |args, _, _, _| {
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
    map.insert(
        "max",
        BuiltInFunctionDef {
            name: String::from("max"),
            arity: FunctionArity::AtLeast(2),
            body: |args, _, _, _| {
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
    map.insert(
        "avg",
        BuiltInFunctionDef {
            name: String::from("avg"),
            arity: FunctionArity::AtLeast(2),
            body: |args, _, _, _| {
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
    map.insert(
        "sum",
        BuiltInFunctionDef {
            name: String::from("sum"),
            arity: FunctionArity::AtLeast(2),
            body: |args, _, _, _| {
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
    map.insert(
        "prod",
        BuiltInFunctionDef {
            name: String::from("prod"),
            arity: FunctionArity::AtLeast(2),
            body: |args, _, _, _| {
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
    map.insert(
        "median",
        BuiltInFunctionDef {
            name: String::from("median"),
            arity: FunctionArity::AtLeast(2),
            body: |args, _, _, _| {
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
    map.insert(
        "range",
        BuiltInFunctionDef {
            name: String::from("range"),
            arity: FunctionArity::Exact(1),
            body: |args, heap, _, _| {
                let n = args[0].as_number()? as i64;

                if n > 10_000 {
                    return Err(anyhow!("range must be no larger than 10,000"));
                }

                let values = (0..n).map(|e| Value::Number(e as f64)).collect();
                let list = heap.borrow_mut().insert_list(values);

                Ok(list)
            },
        },
    );
    map.insert(
        "len",
        BuiltInFunctionDef {
            name: String::from("len"),
            arity: FunctionArity::Exact(1),
            body: |args, heap, _, _| match &args[0] {
                Value::List(l) => Ok(Value::Number(
                    l.reify(&heap.borrow()).as_list()?.len() as f64
                )),
                Value::String(s) => Ok(Value::Number(
                    s.reify(&heap.borrow()).as_string()?.len() as f64
                )),
                _ => Err(anyhow!("argument must be a list or string")),
            },
        },
    );
    map.insert(
        "head",
        BuiltInFunctionDef {
            name: String::from("head"),
            arity: FunctionArity::Exact(1),
            body: |args, heap, _, _| match &args[0] {
                Value::List(p) => p
                    .reify(&heap.borrow())
                    .as_list()?
                    .first()
                    .copied()
                    .ok_or(anyhow!("empty list")),
                Value::String(p) => {
                    let val = {
                        p.reify(&heap.borrow())
                            .as_string()?
                            .get(0..1)
                            .ok_or(anyhow!("empty list"))?
                            .to_string()
                    };

                    Ok(heap.borrow_mut().insert_string(val))
                }
                _ => Err(anyhow!("argument must be a list or string")),
            },
        },
    );
    map.insert(
        "tail",
        BuiltInFunctionDef {
            name: String::from("tail"),
            arity: FunctionArity::Exact(1),
            body: |args, heap, _, _| match &args[0] {
                Value::List(p) => {
                    let val = {
                        p.reify(&heap.borrow())
                            .as_list()?
                            .get(1..)
                            .unwrap_or([].as_slice())
                            .to_vec()
                    };

                    Ok(heap.borrow_mut().insert_list(val))
                }
                Value::String(s) => {
                    let val = {
                        s.reify(&heap.borrow())
                            .as_string()?
                            .get(1..)
                            .unwrap_or("")
                            .to_string()
                    };

                    Ok(heap.borrow_mut().insert_string(val))
                }
                _ => Err(anyhow!("argument must be a list or string")),
            },
        },
    );
    // map.insert(
    //     "slice",
    //     BuiltInFunctionDef {
    //         name: String::from("slice"),
    //         arity: FunctionArity::Exact(3),
    //         body: |args, _, _, _| {
    //             let start = args[0].as_number()? as usize;
    //             let end = args[1].as_number()? as usize;

    //             match &args[2] {
    //                 Value::List(l) => l
    //                     .get(start..end)
    //                     .map_or(Err(anyhow!("index out of bounds")), |l| {
    //                         Ok(Value::List(l.to_vec()))
    //                     }),
    //                 Value::String(s) => s
    //                     .get(start..end)
    //                     .map_or(Err(anyhow!("index out of bounds")), |s| {
    //                         Ok(Value::String(s.to_string()))
    //                     }),
    //                 _ => Err(anyhow!("argument must be a list or string")),
    //             }
    //         },
    //     },
    // );
    // map.insert(
    //     "concat",
    //     BuiltInFunctionDef {
    //         name: String::from("concat"),
    //         arity: FunctionArity::AtLeast(2),
    //         body: |args, _, _, _| {
    //             let mut list = vec![];
    //             for arg in args {
    //                 match arg {
    //                     Value::List(l) => list.extend(l),
    //                     Value::Spread(IterableValue::List(l)) => list.extend(l),
    //                     Value::Spread(IterableValue::String(s)) => {
    //                         list.extend(s.chars().map(|c| Value::String(c.to_string())))
    //                     }
    //                     _ => list.push(arg),
    //                 }
    //             }

    //             Ok(Value::List(list))
    //         },
    //     },
    // );
    // map.insert(
    //     "dot",
    //     BuiltInFunctionDef {
    //         name: String::from("dot"),
    //         arity: FunctionArity::Exact(2),
    //         body: |args, _, _, _| {
    //             let a = args[0].as_list()?;
    //             let b = args[1].as_list()?;

    //             if a.len() != b.len() {
    //                 return Err(anyhow!(
    //                     "cannot calculate dot product of lists with different lengths"
    //                 ));
    //             }

    //             Ok(Value::Number(
    //                 a.iter()
    //                     .zip(b.iter())
    //                     .map(|(a, b)| {
    //                         let a_num = a.as_number()?;
    //                         let b_num = b.as_number()?;
    //                         Ok(a_num * b_num)
    //                     })
    //                     .collect::<Result<Vec<f64>>>()?
    //                     .iter()
    //                     .sum(),
    //             ))
    //         },
    //     },
    // );
    // map.insert(
    //     "unique",
    //     BuiltInFunctionDef {
    //         name: String::from("unique"),
    //         arity: FunctionArity::Exact(1),
    //         body: |args, _, _, _| {
    //             let list = args[0].as_list()?.clone();
    //             let mut unique_list = vec![];

    //             for item in list {
    //                 if !unique_list.contains(&item) {
    //                     unique_list.push(item);
    //                 }
    //             }

    //             Ok(Value::List(unique_list))
    //         },
    //     },
    // );
    map.insert(
        "map",
        BuiltInFunctionDef {
            name: String::from("map"),
            arity: FunctionArity::Exact(2),
            body: |args, heap, bindings, call_depth| {
                let (def, list) = {
                    let borrowed_heap = &heap.borrow();
                    (
                        get_function_def(&args[0], borrowed_heap)
                            .ok_or(anyhow!("expected the first argument to be a function"))?,
                        args[1].as_list(borrowed_heap)?.clone(),
                    )
                };

                let parsed_body = def.get_parsed_body();
                let mut new_list = vec![];
                for (i, item) in list.iter().enumerate() {
                    new_list.push(def.call(
                        if !def.check_arity(2).is_err() {
                            vec![*item, Value::Number(i as f64)]
                        } else {
                            vec![*item]
                        },
                        Rc::clone(&heap),
                        Rc::clone(&bindings),
                        parsed_body.clone(),
                        call_depth,
                    )?);
                }

                Ok(heap.borrow_mut().insert_list(new_list))
            },
        },
    );
    // map.insert(
    //     "reduce",
    //     BuiltInFunctionDef {
    //         name: String::from("reduce"),
    //         arity: FunctionArity::Exact(3),
    //         body: |args, heap, bindings, call_depth| {
    //             let def = get_function_def(&args[0])
    //                 .ok_or(anyhow!("expected the first argument to be a function"))?;
    //             let parsed_body = def.get_parsed_body();

    //             let list = args[1].as_list()?;
    //             let initial = args[2].clone();

    //             let mut acc = initial;
    //             for (i, item) in list.iter().enumerate() {
    //                 acc = def.call(
    //                     if !def.check_arity(3).is_err() {
    //                         vec![acc, item.clone(), Value::Number(i as f64)]
    //                     } else {
    //                         vec![acc, item.clone()]
    //                     },
    //                     heap,
    //                     bindings,
    //                     parsed_body.clone(),
    //                     call_depth,
    //                 )?;
    //             }

    //             Ok(acc)
    //         },
    //     },
    // );
    map.insert(
        "filter",
        BuiltInFunctionDef {
            name: String::from("filter"),
            arity: FunctionArity::Exact(2),
            body: |args, heap, bindings, call_depth| {
                let (def, list) = {
                    let borrowed_heap = &heap.borrow();
                    (
                        get_function_def(&args[0], borrowed_heap)
                            .ok_or(anyhow!("expected the first argument to be a function"))?,
                        args[1].as_list(borrowed_heap)?.clone(),
                    )
                };

                let parsed_body = def.get_parsed_body();
                let mut new_list = vec![];
                for (i, item) in list.iter().enumerate() {
                    if def
                        .call(
                            if !def.check_arity(2).is_err() {
                                vec![item.clone(), Value::Number(i as f64)]
                            } else {
                                vec![item.clone()]
                            },
                            Rc::clone(&heap),
                            Rc::clone(&bindings),
                            parsed_body.clone(),
                            call_depth,
                        )?
                        .as_bool()?
                    {
                        new_list.push(item.clone());
                    }
                }

                Ok(heap.borrow_mut().insert_list(new_list))
            },
        },
    );
    // map.insert(
    //     "every",
    //     BuiltInFunctionDef {
    //         name: String::from("every"),
    //         arity: FunctionArity::Exact(2),
    //         body: |args, heap, bindings, call_depth| {
    //             let def = get_function_def(&args[0])
    //                 .ok_or(anyhow!("expected the first argument to be a function"))?;
    //             let parsed_body = def.get_parsed_body();

    //             let list = args[1].as_list()?;

    //             for (i, item) in list.iter().enumerate() {
    //                 if !def
    //                     .call(
    //                         if !def.check_arity(2).is_err() {
    //                             vec![item.clone(), Value::Number(i as f64)]
    //                         } else {
    //                             vec![item.clone()]
    //                         },
    //                         heap,
    //                         bindings,
    //                         parsed_body.clone(),
    //                         call_depth,
    //                     )?
    //                     .as_bool()?
    //                 {
    //                     return Ok(Value::Bool(false));
    //                 }
    //             }

    //             Ok(Value::Bool(true))
    //         },
    //     },
    // );
    // map.insert(
    //     "some",
    //     BuiltInFunctionDef {
    //         name: String::from("some"),
    //         arity: FunctionArity::Exact(2),
    //         body: |args, heap, bindings, call_depth| {
    //             let def = get_function_def(&args[0])
    //                 .ok_or(anyhow!("expected the first argument to be a function"))?;
    //             let parsed_body = def.get_parsed_body();

    //             let list = args[1].as_list()?;

    //             for (i, item) in list.iter().enumerate() {
    //                 if def
    //                     .call(
    //                         if !def.check_arity(2).is_err() {
    //                             vec![item.clone(), Value::Number(i as f64)]
    //                         } else {
    //                             vec![item.clone()]
    //                         },
    //                         heap,
    //                         bindings,
    //                         parsed_body.clone(),
    //                         call_depth,
    //                     )?
    //                     .as_bool()?
    //                 {
    //                     return Ok(Value::Bool(true));
    //                 }
    //             }

    //             Ok(Value::Bool(false))
    //         },
    //     },
    // );
    // map.insert(
    //     "sort",
    //     BuiltInFunctionDef {
    //         name: String::from("sort"),
    //         arity: FunctionArity::Exact(1),
    //         body: |args, _, _, _| {
    //             let mut list = args[0].as_list()?.clone();
    //             list.sort_by(|a, b| a.partial_cmp(b).unwrap());
    //             Ok(Value::List(list))
    //         },
    //     },
    // );
    // map.insert(
    //     "sort_by",
    //     BuiltInFunctionDef {
    //         name: String::from("sort_by"),
    //         arity: FunctionArity::Exact(2),
    //         body: |args, heap, bindings, call_depth| {
    //             let def = get_function_def(&args[0])
    //                 .ok_or(anyhow!("expected the first argument to be a function"))?;
    //             let parsed_body = def.get_parsed_body();

    //             let mut list = args[1].as_list()?.clone();
    //             let mut err: Option<Result<Value, anyhow::Error>> = None;

    //             list.sort_by(|a, b| {
    //                 let call_result = def.call(
    //                     vec![a.clone(), b.clone()],
    //                     heap,
    //                     bindings,
    //                     parsed_body.clone(),
    //                     call_depth,
    //                 );

    //                 if let Err(e) = call_result {
    //                     if err.is_none() {
    //                         err = Some(Err(e));
    //                     }

    //                     return Ordering::Equal;
    //                 }

    //                 let number_result = call_result.unwrap().as_number();
    //                 if let Err(e) = number_result {
    //                     if err.is_none() {
    //                         err = Some(Err(e));
    //                     }

    //                     return Ordering::Equal;
    //                 }

    //                 match number_result.unwrap() {
    //                     n if n.is_sign_positive() => Ordering::Greater,
    //                     n if n.is_sign_negative() => Ordering::Less,
    //                     _ => Ordering::Equal,
    //                 }
    //             });

    //             err.unwrap_or(Ok(Value::List(list)))
    //         },
    //     },
    // );
    map.insert(
        "reverse",
        BuiltInFunctionDef {
            name: String::from("reverse"),
            arity: FunctionArity::Exact(1),
            body: |args, heap, _, _| {
                let mut list = { args[0].as_list(&heap.borrow())?.clone() };
                list.reverse();
                Ok(heap.borrow_mut().insert_list(list))
            },
        },
    );
    map.insert(
        "split",
        BuiltInFunctionDef {
            name: String::from("split"),
            arity: FunctionArity::Exact(2),
            body: |args, heap, _, _| {
                let borrowed_heap = &heap.borrow();
                let delimeter = args[0].as_string(borrowed_heap)?;
                let s = args[1].as_string(borrowed_heap)?;

                let list = s
                    .split(&delimeter)
                    .map(|s| heap.borrow_mut().insert_string(s.to_string()))
                    .collect();

                Ok(heap.borrow_mut().insert_list(list))
            },
        },
    );
    map.insert(
        "join",
        BuiltInFunctionDef {
            name: String::from("join"),
            arity: FunctionArity::Exact(2),
            body: |args, heap, _, _| {
                let borrowed_heap = &heap.borrow();
                let delimeter = args[0].as_string(borrowed_heap)?;
                let list = args[1].as_list(borrowed_heap)?;
                let joined_string = list
                    .iter()
                    .map(|v| v.stringify(borrowed_heap))
                    .collect::<Vec<String>>()
                    .join(&delimeter);

                Ok(heap.borrow_mut().insert_string(joined_string))
            },
        },
    );
    map.insert(
        "replace",
        BuiltInFunctionDef {
            name: String::from("replace"),
            arity: FunctionArity::Exact(3),
            body: |args, heap, _, _| {
                let borrowed_heap = &heap.borrow();
                let old = args[0].as_string(borrowed_heap)?;
                let new = args[1].as_string(borrowed_heap)?;
                let s = args[2].as_string(borrowed_heap)?;

                Ok(heap.borrow_mut().insert_string(s.replace(old, new)))
            },
        },
    );
    map.insert(
        "typeof",
        BuiltInFunctionDef {
            name: String::from("typeof"),
            arity: FunctionArity::Exact(1),
            body: |args, heap, _, _| {
                Ok(heap
                    .borrow_mut()
                    .insert_string(args[0].get_type().to_string()))
            },
        },
    );
    map.insert(
        "percentile",
        BuiltInFunctionDef {
            name: String::from("percentile"),
            arity: FunctionArity::Exact(2),
            body: |args, heap, _, _| {
                let p = args[0].as_number()?;
                let heap = &heap.borrow();
                let list = args[1].as_list(heap)?;

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
    map.insert(
        "uppercase",
        BuiltInFunctionDef {
            name: String::from("uppercase"),
            arity: FunctionArity::Exact(1),
            body: |args, heap, _, _| {
                let string = args[0].as_string(&heap.borrow())?.to_uppercase();
                Ok(heap.borrow_mut().insert_string(string))
            },
        },
    );
    map.insert(
        "lowercase",
        BuiltInFunctionDef {
            name: String::from("lowercase"),
            arity: FunctionArity::Exact(1),
            body: |args, heap, _, _| {
                let string = args[0].as_string(&heap.borrow())?.to_lowercase();
                Ok(heap.borrow_mut().insert_string(string))
            },
        },
    );
    map.insert(
        "to_string",
        BuiltInFunctionDef {
            name: String::from("to_string"),
            arity: FunctionArity::Exact(1),
            body: |args, heap, _, _| {
                let string = args[0].stringify(&heap.borrow());
                Ok(heap.borrow_mut().insert_string(string))
            },
        },
    );
    map.insert(
        "to_number",
        BuiltInFunctionDef {
            name: String::from("to_number"),
            arity: FunctionArity::Exact(1),
            body: |args, heap, _, _| Ok(Value::Number(args[0].as_string(&heap.borrow())?.parse()?)),
        },
    );
    map.insert(
        "arity",
        BuiltInFunctionDef {
            name: String::from("arity"),
            arity: FunctionArity::Exact(1),
            body: |args, heap, _, _| {
                Ok(Value::Number(
                    match args[0].as_lambda(&heap.borrow())?.get_arity() {
                        FunctionArity::Exact(n) => n as f64,
                        FunctionArity::AtLeast(n) => n as f64,
                        FunctionArity::Between(min, _max) => min as f64,
                    },
                ))
            },
        },
    );
    map.insert(
        "includes",
        BuiltInFunctionDef {
            name: String::from("includes"),
            arity: FunctionArity::Exact(2),
            body: |args, heap, _, _| {
                let needle = &args[0];

                match &args[1].reify(&heap.borrow())? {
                    ReifiedValue::List(l, _) => Ok(Value::Bool((*l).contains(needle))),
                    ReifiedValue::String(s, _) => {
                        Ok(Value::Bool(s.contains(needle.as_string(&heap.borrow())?)))
                    }
                    _ => Err(anyhow!("second argument must be a list or string")),
                }
            },
        },
    );
    map.insert(
        "collect",
        BuiltInFunctionDef {
            name: String::from("collect"),
            arity: FunctionArity::Exact(1),
            body: |args, heap, _, _| {
                let values: Vec<Value> = {
                    let borrowed_heap = &heap.borrow();
                    let iterable = args[0].as_each(borrowed_heap)?.to_owned();
                    iterable.into_iter().collect()
                };
                Ok(heap.borrow_mut().insert_list(values))
            },
        },
    );
    map.insert(
        "format",
        BuiltInFunctionDef {
            name: String::from("format"),
            arity: FunctionArity::AtLeast(1),
            body: |args, heap, _, _| {
                let format_str = {
                    let borrowed_heap = &heap.borrow();
                    args[0]
                        .as_string(borrowed_heap)
                        .map_err(|_| anyhow!("first argument must be a string"))?
                        .to_string()
                };

                let format_args = {
                    let borrowed_heap = &heap.borrow();
                    &args[1..]
                        .into_iter()
                        .map(|v| v.stringify(borrowed_heap))
                        .collect::<Vec<String>>()
                };

                Ok(heap
                    .borrow_mut()
                    .insert_string(format_str.format(format_args)))
            },
        },
    );
    map.insert(
        "add2",
        BuiltInFunctionDef {
            name: String::from("add2"),
            arity: FunctionArity::Exact(1),
            body: |args, _, _, _| Ok(Value::Number(args[0].as_number()? + 2.0)),
        },
    );

    #[cfg(not(target_arch = "wasm32"))]
        map.insert(
            "print",
            BuiltInFunctionDef {
                name: String::from("print"),
                arity: FunctionArity::AtLeast(1),
                body: |args, heap, _, _| {
                    let borrowed_heap = &heap.borrow();

                    let output = if args.len() == 1 {
                        args[0].stringify(borrowed_heap)
                    } else {
                        let format_str = args[0].as_string(borrowed_heap).map_err(|_| {
                            anyhow!("first argument must be a formatting string if multiple arguments are given")
                        })?;
                        let format_args = &args[1..].into_iter().map(|v| v.stringify(borrowed_heap)).collect::<Vec<String>>();
                        format_str.format(format_args)
                    };

                    println!("{}", output);

                    Ok(Value::Null)
                },
            },
        );

    #[cfg(not(target_arch = "wasm32"))]
    map.insert(
        "time_now",
        BuiltInFunctionDef {
            name: String::from("time_now"),
            arity: FunctionArity::Exact(0),
            body: |_, _, _, _| {
                Ok(Value::Number(
                    std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .unwrap()
                        .as_secs_f64(),
                ))
            },
        },
    );

    map
});

static BUILT_IN_FUNCTION_IDENTS: LazyLock<Vec<&str>> =
    LazyLock::new(|| BUILT_IN_FUNCTION_DEFS.get_idents());

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
        heap: Rc<RefCell<Heap>>,
        bindings: Rc<RefCell<HashMap<String, Value>>>,
        parsed_body: Option<Result<Pairs<Rule>, pest::error::Error<Rule>>>,
        call_depth: usize,
    ) -> Result<Value> {
        #[cfg(not(target_arch = "wasm32"))]
        let start = std::time::Instant::now();

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
                ..
            }) => {
                #[cfg(not(target_arch = "wasm32"))]
                let start_var_env = std::time::Instant::now();

                let mut new_bindings = bindings.borrow().clone();

                for (idx, expected_arg) in expected_args.iter().enumerate() {
                    match expected_arg {
                        LambdaArg::Required(name) => {
                            new_bindings.insert(name.clone(), args[idx].clone());
                        }
                        LambdaArg::Optional(name) => {
                            new_bindings.insert(
                                name.clone(),
                                args.get(idx).copied().unwrap_or(Value::Null),
                            );
                        }
                        LambdaArg::Rest(name) => {
                            new_bindings.insert(
                                name.clone(),
                                heap.borrow_mut()
                                    .insert_list(args.iter().skip(idx).copied().collect()),
                            );
                        }
                    }
                }

                #[cfg(not(target_arch = "wasm32"))]
                let end_var_env = std::time::Instant::now();

                let return_value = evaluate_expression(
                    parsed_body
                        .unwrap_or_else(|| get_pairs(body))?
                        .next()
                        .unwrap()
                        .into_inner(),
                    Rc::clone(&heap),
                    Rc::new(RefCell::new(new_bindings)),
                    call_depth + 1,
                )
                .map_err(|error| anyhow!("in {}: {}", self.get_name(), error));

                #[cfg(not(target_arch = "wasm32"))]
                FUNCTION_CALLS.lock().unwrap().push(FunctionCallStats {
                    name: self.get_name(),
                    start,
                    end: std::time::Instant::now(),
                    start_var_env: Some(start_var_env),
                    end_var_env: Some(end_var_env),
                });

                return return_value;
            }
            FunctionDef::BuiltIn(BuiltInFunctionDef { body, .. }) => {
                let return_value = body(args, heap, bindings, call_depth + 1)
                    .map_err(|error| anyhow!("in {}: {}", self.get_name(), error));

                #[cfg(not(target_arch = "wasm32"))]
                FUNCTION_CALLS.lock().unwrap().push(FunctionCallStats {
                    name: self.get_name(),
                    start,
                    end: std::time::Instant::now(),
                    start_var_env: None,
                    end_var_env: None,
                });

                return return_value;
            }
        }
    }
}

pub fn is_built_in_function(ident: &str) -> bool {
    BUILT_IN_FUNCTION_DEFS.get_id(ident).is_some()
}

pub fn get_built_in_function_def(id: usize) -> Option<FunctionDef<'static>> {
    BUILT_IN_FUNCTION_DEFS
        .get(BUILT_IN_FUNCTION_IDENTS[id])
        .map(|def| FunctionDef::BuiltIn(def))
}

pub fn get_built_in_function_def_by_ident(ident: &str) -> Option<FunctionDef<'static>> {
    BUILT_IN_FUNCTION_DEFS
        .get(ident)
        .map(|def| FunctionDef::BuiltIn(def))
}

pub fn get_built_in_function_id(ident: &str) -> Option<usize> {
    BUILT_IN_FUNCTION_DEFS.get_id(ident)
}

pub fn get_built_in_function_ident(id: usize) -> Option<&'static str> {
    BUILT_IN_FUNCTION_DEFS.get_ident(id)
}

pub fn get_built_in_function_idents() -> Vec<&'static str> {
    BUILT_IN_FUNCTION_IDENTS.iter().copied().collect()
}

pub fn get_function_def<'h>(value: &Value, heap: &'h Heap) -> Option<FunctionDef<'static>> {
    match value {
        Value::Lambda(pointer) => Some(FunctionDef::Lambda(
            pointer.reify(heap).as_lambda().ok()?.clone(),
        )),
        Value::BuiltIn(id) => Some(get_built_in_function_def(*id)?),
        _ => None,
    }
}
