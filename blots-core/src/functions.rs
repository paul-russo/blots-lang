use crate::{
    expressions::evaluate_ast,
    heap::{Heap, HeapPointer, IterablePointer},
    values::{FunctionArity, LambdaArg, LambdaDef, ReifiedValue, Value},
};
use anyhow::{anyhow, Result};
use dyn_fmt::AsStrFormatExt;
use std::{cell::RefCell, cmp::Ordering, collections::HashMap, rc::Rc, sync::LazyLock};

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
            arity: FunctionArity::Between(1, 2),
            body: |args, _, _, _| {
                let num = args[0].as_number()?;
                if args.len() == 1 {
                    Ok(Value::Number(num.round()))
                } else {
                    let decimal_places = args[1].as_number()? as i32;
                    let multiplier = 10_f64.powi(decimal_places);
                    Ok(Value::Number((num * multiplier).round() / multiplier))
                }
            },
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
            arity: FunctionArity::AtLeast(1),
            body: |args, heap, _, _| {
                let nums = if args.len() == 1 {
                    // Check if single argument is a list
                    match &args[0] {
                        Value::List(_) => {
                            let borrowed_heap = heap.borrow();
                            let list = args[0].as_list(&borrowed_heap)?;
                            list.iter()
                                .map(|a| a.as_number())
                                .collect::<Result<Vec<f64>>>()?
                        }
                        _ => vec![args[0].as_number()?],
                    }
                } else {
                    args.iter()
                        .map(|a| a.as_number())
                        .collect::<Result<Vec<f64>>>()?
                };

                if nums.is_empty() {
                    return Err(anyhow!("min requires at least one number"));
                }

                Ok(Value::Number(
                    nums.iter().copied().fold(f64::INFINITY, f64::min),
                ))
            },
        },
    );
    map.insert(
        "max",
        BuiltInFunctionDef {
            name: String::from("max"),
            arity: FunctionArity::AtLeast(1),
            body: |args, heap, _, _| {
                let nums = if args.len() == 1 {
                    // Check if single argument is a list
                    match &args[0] {
                        Value::List(_) => {
                            let borrowed_heap = heap.borrow();
                            let list = args[0].as_list(&borrowed_heap)?;
                            list.iter()
                                .map(|a| a.as_number())
                                .collect::<Result<Vec<f64>>>()?
                        }
                        _ => vec![args[0].as_number()?],
                    }
                } else {
                    args.iter()
                        .map(|a| a.as_number())
                        .collect::<Result<Vec<f64>>>()?
                };

                if nums.is_empty() {
                    return Err(anyhow!("max requires at least one number"));
                }

                Ok(Value::Number(
                    nums.iter().copied().fold(f64::NEG_INFINITY, f64::max),
                ))
            },
        },
    );
    map.insert(
        "avg",
        BuiltInFunctionDef {
            name: String::from("avg"),
            arity: FunctionArity::AtLeast(1),
            body: |args, heap, _, _| {
                let nums = if args.len() == 1 {
                    // Check if single argument is a list
                    match &args[0] {
                        Value::List(_) => {
                            let borrowed_heap = heap.borrow();
                            let list = args[0].as_list(&borrowed_heap)?;
                            list.iter()
                                .map(|a| a.as_number())
                                .collect::<Result<Vec<f64>>>()?
                        }
                        _ => vec![args[0].as_number()?],
                    }
                } else {
                    args.iter()
                        .map(|a| a.as_number())
                        .collect::<Result<Vec<f64>>>()?
                };

                if nums.is_empty() {
                    return Err(anyhow!("avg requires at least one number"));
                }

                Ok(Value::Number(nums.iter().sum::<f64>() / nums.len() as f64))
            },
        },
    );
    map.insert(
        "sum",
        BuiltInFunctionDef {
            name: String::from("sum"),
            arity: FunctionArity::AtLeast(1),
            body: |args, heap, _, _| {
                let nums = if args.len() == 1 {
                    // Check if single argument is a list
                    match &args[0] {
                        Value::List(_) => {
                            let borrowed_heap = heap.borrow();
                            let list = args[0].as_list(&borrowed_heap)?;
                            list.iter()
                                .map(|a| a.as_number())
                                .collect::<Result<Vec<f64>>>()?
                        }
                        _ => vec![args[0].as_number()?],
                    }
                } else {
                    args.iter()
                        .map(|a| a.as_number())
                        .collect::<Result<Vec<f64>>>()?
                };

                if nums.is_empty() {
                    return Err(anyhow!("sum requires at least one number"));
                }

                Ok(Value::Number(nums.iter().sum()))
            },
        },
    );
    map.insert(
        "prod",
        BuiltInFunctionDef {
            name: String::from("prod"),
            arity: FunctionArity::AtLeast(1),
            body: |args, heap, _, _| {
                let nums = if args.len() == 1 {
                    // Check if single argument is a list
                    match &args[0] {
                        Value::List(_) => {
                            let borrowed_heap = heap.borrow();
                            let list = args[0].as_list(&borrowed_heap)?;
                            list.iter()
                                .map(|a| a.as_number())
                                .collect::<Result<Vec<f64>>>()?
                        }
                        _ => vec![args[0].as_number()?],
                    }
                } else {
                    args.iter()
                        .map(|a| a.as_number())
                        .collect::<Result<Vec<f64>>>()?
                };

                if nums.is_empty() {
                    return Err(anyhow!("prod requires at least one number"));
                }

                Ok(Value::Number(nums.iter().product()))
            },
        },
    );
    map.insert(
        "median",
        BuiltInFunctionDef {
            name: String::from("median"),
            arity: FunctionArity::AtLeast(1),
            body: |args, heap, _, _| {
                let mut nums = if args.len() == 1 {
                    // Check if single argument is a list
                    match &args[0] {
                        Value::List(_) => {
                            let borrowed_heap = heap.borrow();
                            let list = args[0].as_list(&borrowed_heap)?;
                            list.iter()
                                .map(|a| a.as_number())
                                .collect::<Result<Vec<f64>>>()?
                        }
                        _ => vec![args[0].as_number()?],
                    }
                } else {
                    args.into_iter()
                        .map(|a| a.as_number())
                        .collect::<Result<Vec<f64>>>()?
                };

                if nums.is_empty() {
                    return Err(anyhow!("median requires at least one number"));
                }

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
            arity: FunctionArity::Between(1, 2),
            body: |args, heap, _, _| {
                let (start, end) = if args.len() == 1 {
                    (0, args[0].as_number()? as i64)
                } else {
                    (args[0].as_number()? as i64, args[1].as_number()? as i64)
                };

                let values = (start..=end).map(|e| Value::Number(e as f64)).collect();
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
                Value::List(p) => Ok(p
                    .reify(&heap.borrow())
                    .as_list()?
                    .first()
                    .copied()
                    .unwrap_or(Value::Null)),
                Value::String(p) => {
                    let val = {
                        p.reify(&heap.borrow())
                            .as_string()?
                            .get(0..1)
                            .unwrap_or("")
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
    map.insert(
        "slice",
        BuiltInFunctionDef {
            name: String::from("slice"),
            arity: FunctionArity::Exact(3),
            body: |args, heap, _, _| {
                let start = args[1].as_number()? as usize;
                let end = args[2].as_number()? as usize;

                match args[0] {
                    Value::List(_) => {
                        let l = {
                            let borrowed_heap = &heap.borrow();
                            args[0].as_list(borrowed_heap)?.clone()
                        };

                        l.get(start..end)
                            .map_or(Err(anyhow!("index out of bounds")), |l| {
                                Ok(heap.borrow_mut().insert_list(l.to_vec()))
                            })
                    }
                    Value::String(_) => {
                        let s = {
                            let borrowed_heap = &heap.borrow();
                            args[0].as_string(borrowed_heap)?.to_string()
                        };

                        s.get(start..end)
                            .map_or(Err(anyhow!("index out of bounds")), |s| {
                                Ok(heap.borrow_mut().insert_string(s.to_string()))
                            })
                    }
                    _ => Err(anyhow!("argument must be a list or string")),
                }
            },
        },
    );
    map.insert(
        "concat",
        BuiltInFunctionDef {
            name: String::from("concat"),
            arity: FunctionArity::AtLeast(2),
            body: |args, heap, _, _| {
                let mut list = vec![];

                for arg in args {
                    match arg {
                        Value::List(p) => list.extend(p.reify(&heap.borrow()).as_list()?.clone()),
                        Value::Spread(IterablePointer::List(p)) => {
                            list.extend(p.reify(&heap.borrow()).as_list()?.clone())
                        }
                        Value::Spread(IterablePointer::String(p)) => {
                            let string = {
                                let borrowed_heap = &heap.borrow();
                                p.reify(borrowed_heap).as_string()?.to_string()
                            };

                            list.extend(
                                string
                                    .chars()
                                    .map(|c| heap.borrow_mut().insert_string(c.to_string())),
                            );
                        }
                        _ => list.push(arg),
                    }
                }

                Ok(heap.borrow_mut().insert_list(list))
            },
        },
    );
    map.insert(
        "dot",
        BuiltInFunctionDef {
            name: String::from("dot"),
            arity: FunctionArity::Exact(2),
            body: |args, heap, _, _| {
                let (a, b) = {
                    let borrowed_heap = &heap.borrow();
                    (
                        args[0].as_list(borrowed_heap)?.clone(),
                        args[1].as_list(borrowed_heap)?.clone(),
                    )
                };

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
    map.insert(
        "unique",
        BuiltInFunctionDef {
            name: String::from("unique"),
            arity: FunctionArity::Exact(1),
            body: |args, heap, _, _| {
                let list = {
                    let borrowed_heap = &heap.borrow();
                    args[0].as_list(borrowed_heap)?.clone()
                };
                let mut unique_list = vec![];

                for item in list {
                    if !unique_list.contains(&item) {
                        unique_list.push(item);
                    }
                }

                Ok(heap.borrow_mut().insert_list(unique_list))
            },
        },
    );
    map.insert(
        "map",
        BuiltInFunctionDef {
            name: String::from("map"),
            arity: FunctionArity::Exact(2),
            body: |args, heap, bindings, call_depth| {
                let (list, def) = {
                    let borrowed_heap = &heap.borrow();
                    (
                        args[0].as_list(borrowed_heap)?.clone(),
                        get_function_def(&args[1], borrowed_heap)
                            .ok_or(anyhow!("expected the second argument to be a function"))?,
                    )
                };

                let takes_index = !def.check_arity(2).is_err();
                let mut new_list = Vec::with_capacity(list.len());

                for (i, item) in list.iter().enumerate() {
                    new_list.push(def.call(
                        args[1],
                        if takes_index {
                            vec![*item, Value::Number(i as f64)]
                        } else {
                            vec![*item]
                        },
                        Rc::clone(&heap),
                        Rc::clone(&bindings),
                        call_depth,
                    )?);
                }

                Ok(heap.borrow_mut().insert_list(new_list))
            },
        },
    );
    map.insert(
        "reduce",
        BuiltInFunctionDef {
            name: String::from("reduce"),
            arity: FunctionArity::Exact(3),
            body: |args, heap, bindings, call_depth| {
                let (list, def) = {
                    let borrowed_heap = &heap.borrow();
                    (
                        args[0].as_list(borrowed_heap)?.clone(),
                        get_function_def(&args[1], borrowed_heap)
                            .ok_or(anyhow!("expected the second argument to be a function"))?,
                    )
                };

                let initial = args[2].clone();

                let mut acc = initial;
                for (i, item) in list.iter().enumerate() {
                    acc = def.call(
                        args[1],
                        if !def.check_arity(3).is_err() {
                            vec![acc, item.clone(), Value::Number(i as f64)]
                        } else {
                            vec![acc, item.clone()]
                        },
                        Rc::clone(&heap),
                        Rc::clone(&bindings),
                        call_depth,
                    )?;
                }

                Ok(acc)
            },
        },
    );
    map.insert(
        "filter",
        BuiltInFunctionDef {
            name: String::from("filter"),
            arity: FunctionArity::Exact(2),
            body: |args, heap, bindings, call_depth| {
                let (list, def) = {
                    let borrowed_heap = &heap.borrow();
                    (
                        args[0].as_list(borrowed_heap)?.clone(),
                        get_function_def(&args[1], borrowed_heap)
                            .ok_or(anyhow!("expected the second argument to be a function"))?,
                    )
                };

                let mut new_list = vec![];
                for (i, item) in list.iter().enumerate() {
                    if def
                        .call(
                            args[1],
                            if !def.check_arity(2).is_err() {
                                vec![item.clone(), Value::Number(i as f64)]
                            } else {
                                vec![item.clone()]
                            },
                            Rc::clone(&heap),
                            Rc::clone(&bindings),
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
    map.insert(
        "every",
        BuiltInFunctionDef {
            name: String::from("every"),
            arity: FunctionArity::Exact(2),
            body: |args, heap, bindings, call_depth| {
                let (list, def) = {
                    let borrowed_heap = &heap.borrow();
                    (
                        args[0].as_list(borrowed_heap)?.clone(),
                        get_function_def(&args[1], borrowed_heap)
                            .ok_or(anyhow!("expected the second argument to be a function"))?,
                    )
                };

                for (i, item) in list.iter().enumerate() {
                    if !def
                        .call(
                            args[1],
                            if !def.check_arity(2).is_err() {
                                vec![item.clone(), Value::Number(i as f64)]
                            } else {
                                vec![item.clone()]
                            },
                            Rc::clone(&heap),
                            Rc::clone(&bindings),
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
    map.insert(
        "some",
        BuiltInFunctionDef {
            name: String::from("some"),
            arity: FunctionArity::Exact(2),
            body: |args, heap, bindings, call_depth| {
                let (list, def) = {
                    let borrowed_heap = &heap.borrow();
                    (
                        args[0].as_list(borrowed_heap)?.clone(),
                        get_function_def(&args[1], borrowed_heap)
                            .ok_or(anyhow!("expected the second argument to be a function"))?,
                    )
                };

                for (i, item) in list.iter().enumerate() {
                    if def
                        .call(
                            args[1],
                            if !def.check_arity(2).is_err() {
                                vec![item.clone(), Value::Number(i as f64)]
                            } else {
                                vec![item.clone()]
                            },
                            Rc::clone(&heap),
                            Rc::clone(&bindings),
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
    map.insert(
        "sort",
        BuiltInFunctionDef {
            name: String::from("sort"),
            arity: FunctionArity::Exact(1),
            body: |args, heap, _, _| {
                let mut list = {
                    let borrowed_heap = &heap.borrow();
                    args[0].as_list(borrowed_heap)?.clone()
                };
                list.sort_by(|a, b| a.partial_cmp(b).unwrap());

                Ok(heap.borrow_mut().insert_list(list))
            },
        },
    );
    map.insert(
        "sort_by",
        BuiltInFunctionDef {
            name: String::from("sort_by"),
            arity: FunctionArity::Exact(2),
            body: |args, heap, bindings, call_depth| {
                let (mut list, def) = {
                    let borrowed_heap = &heap.borrow();
                    (
                        args[0].as_list(borrowed_heap)?.clone(),
                        get_function_def(&args[1], borrowed_heap)
                            .ok_or(anyhow!("expected the second argument to be a function"))?,
                    )
                };

                let mut err: Option<Result<Value, anyhow::Error>> = None;

                list.sort_by(|a, b| {
                    let call_result = def.call(
                        args[1],
                        vec![a.clone(), b.clone()],
                        Rc::clone(&heap),
                        Rc::clone(&bindings),
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

                err.unwrap_or(Ok(heap.borrow_mut().insert_list(list)))
            },
        },
    );
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
                let (s, delimeter) = {
                    let borrowed_heap = &heap.borrow();
                    (
                        args[0].as_string(borrowed_heap)?.to_string(),
                        args[1].as_string(borrowed_heap)?.to_string(),
                    )
                };

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
                let joined_string = {
                    let borrowed_heap = &heap.borrow();
                    let delimeter = args[1].as_string(borrowed_heap)?;
                    let list = args[0].as_list(borrowed_heap)?;
                    list.iter()
                        .map(|v| v.stringify(borrowed_heap))
                        .collect::<Vec<String>>()
                        .join(&delimeter)
                };

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
                let string = {
                    let borrowed_heap = heap.borrow();
                    let old = args[1].as_string(&borrowed_heap)?.to_string();
                    let new = args[2].as_string(&borrowed_heap)?.to_string();
                    let s = args[0].as_string(&borrowed_heap)?.to_string();
                    s.replace(&old, &new)
                };

                Ok(heap.borrow_mut().insert_string(string))
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
                let p = args[1].as_number()?;
                let heap = &heap.borrow();
                let list = args[0].as_list(heap)?;

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
                let needle = {
                    let borrowed_heap = &heap.borrow();
                    args[1].as_string(borrowed_heap)?.to_string()
                };

                match &args[0].reify(&heap.borrow())? {
                    ReifiedValue::List(l, _) => Ok(Value::Bool(
                        (*l).iter()
                            .find(|v| v.as_string(&heap.borrow()).unwrap().eq(&needle))
                            .is_some(),
                    )),
                    ReifiedValue::String(s, _) => Ok(Value::Bool(s.contains(&needle))),
                    _ => Err(anyhow!("second argument must be a list or string")),
                }
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
        "keys",
        BuiltInFunctionDef {
            name: String::from("keys"),
            arity: FunctionArity::Exact(1),
            body: |args, heap, _, _| {
                let record = args[0].as_record(&heap.borrow())?.clone();
                let keys = {
                    let key_strings = record.keys().cloned().collect::<Vec<String>>();
                    key_strings
                        .iter()
                        .map(|k| heap.borrow_mut().insert_string(k.to_string()))
                        .collect()
                };

                Ok(heap.borrow_mut().insert_list(keys))
            },
        },
    );
    map.insert(
        "values",
        BuiltInFunctionDef {
            name: String::from("values"),
            arity: FunctionArity::Exact(1),
            body: |args, heap, _, _| {
                let record = args[0].as_record(&heap.borrow())?.clone();
                let values = record.values().cloned().collect();

                Ok(heap.borrow_mut().insert_list(values))
            },
        },
    );
    map.insert(
        "entries",
        BuiltInFunctionDef {
            name: String::from("entries"),
            arity: FunctionArity::Exact(1),
            body: |args, heap, _, _| {
                let record = args[0].as_record(&heap.borrow())?.clone();
                let entries = record
                    .iter()
                    .map(|(k, v)| {
                        let entry = {
                            let mut borrowed_heap = heap.borrow_mut();
                            vec![borrowed_heap.insert_string(k.to_string()), *v]
                        };
                        heap.borrow_mut().insert_list(entry)
                    })
                    .collect();

                Ok(heap.borrow_mut().insert_list(entries))
            },
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

    pub fn call(
        &self,
        this_value: Value,
        args: Vec<Value>,
        heap: Rc<RefCell<Heap>>,
        bindings: Rc<RefCell<HashMap<String, Value>>>,
        call_depth: usize,
    ) -> Result<Value> {
        #[cfg(not(target_arch = "wasm32"))]
        let start = std::time::Instant::now();

        self.check_arity(args.len())?;

        if call_depth > 1000 {
            return Err(anyhow!(
                "in {}: maximum call depth of 1000 exceeded",
                self.get_name()
            ));
        }

        match self {
            FunctionDef::Lambda(LambdaDef {
                name,
                args: expected_args,
                body,
                scope,
            }) => {
                #[cfg(not(target_arch = "wasm32"))]
                let start_var_env = std::time::Instant::now();

                let mut new_bindings = scope.clone();

                if let Some(fn_name) = name {
                    new_bindings.insert(fn_name.clone(), this_value);
                }

                if let Some(inputs) = bindings.borrow().get("inputs") {
                    new_bindings.insert(String::from("inputs"), inputs.clone());
                }

                for (idx, expected_arg) in expected_args.iter().enumerate() {
                    match expected_arg {
                        LambdaArg::Required(arg_name) => {
                            new_bindings.insert(arg_name.clone(), args[idx].clone());
                        }
                        LambdaArg::Optional(arg_name) => {
                            new_bindings.insert(
                                arg_name.clone(),
                                args.get(idx).copied().unwrap_or(Value::Null),
                            );
                        }
                        LambdaArg::Rest(arg_name) => {
                            new_bindings.insert(
                                arg_name.clone(),
                                heap.borrow_mut()
                                    .insert_list(args.iter().skip(idx).copied().collect()),
                            );
                        }
                    }
                }

                #[cfg(not(target_arch = "wasm32"))]
                let end_var_env = std::time::Instant::now();

                let return_value = evaluate_ast(
                    body,
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::rc::Rc;

    #[test]
    fn test_range_function() {
        // Test single argument
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        let range_fn =
            get_built_in_function_def(get_built_in_function_id("range").unwrap()).unwrap();

        // Test range(4)
        let args = vec![Value::Number(4.0)];
        let result = match range_fn {
            FunctionDef::BuiltIn(def) => {
                (def.body)(args, heap.clone(), bindings.clone(), 0).unwrap()
            }
            _ => panic!("Expected built-in function"),
        };

        let heap_borrow = heap.borrow();
        let list = result.as_list(&heap_borrow).unwrap();
        assert_eq!(list.len(), 5);
        assert_eq!(list[0], Value::Number(0.0));
        assert_eq!(list[4], Value::Number(4.0));
    }

    #[test]
    fn test_range_function_two_args() {
        // Test two arguments
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        let range_fn =
            get_built_in_function_def(get_built_in_function_id("range").unwrap()).unwrap();

        // Test range(4, 10)
        let args = vec![Value::Number(4.0), Value::Number(10.0)];
        let result = match range_fn {
            FunctionDef::BuiltIn(def) => {
                (def.body)(args, heap.clone(), bindings.clone(), 0).unwrap()
            }
            _ => panic!("Expected built-in function"),
        };

        let heap_borrow = heap.borrow();
        let list = result.as_list(&heap_borrow).unwrap();
        assert_eq!(list.len(), 7);
        assert_eq!(list[0], Value::Number(4.0));
        assert_eq!(list[6], Value::Number(10.0));
    }

    #[test]
    fn test_round_function_single_arg() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        let round_fn =
            get_built_in_function_def(get_built_in_function_id("round").unwrap()).unwrap();

        // Test basic rounding
        let test_cases = vec![
            (42.4, 42.0),
            (42.5, 43.0),
            (42.6, 43.0),
            (-42.4, -42.0),
            (-42.5, -43.0),
            (-42.6, -43.0),
            (0.0, 0.0),
            (1.999, 2.0),
            (-1.999, -2.0),
        ];

        for (input, expected) in test_cases {
            let args = vec![Value::Number(input)];
            let result = match &round_fn {
                FunctionDef::BuiltIn(def) => {
                    (def.body)(args, heap.clone(), bindings.clone(), 0).unwrap()
                }
                _ => panic!("Expected built-in function"),
            };
            assert_eq!(
                result,
                Value::Number(expected),
                "round({}) should be {}",
                input,
                expected
            );
        }
    }

    #[test]
    fn test_round_function_with_decimal_places() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        let round_fn =
            get_built_in_function_def(get_built_in_function_id("round").unwrap()).unwrap();

        // Test rounding to decimal places
        let test_cases = vec![
            (42.4543, 0.0, 42.0),
            (42.4543, 1.0, 42.5),
            (42.4543, 2.0, 42.45),
            (42.4543, 3.0, 42.454),
            (42.4543, 4.0, 42.4543),
            (3.14159, 4.0, 3.1416),
            (0.005, 2.0, 0.01),
            (0.995, 2.0, 1.0),
            // Note: 9.995 has floating point representation issues
            // In binary, it's actually slightly less than 9.995
            (9.995, 2.0, 9.99),
            (-9.995, 2.0, -9.99),
        ];

        for (input, places, expected) in test_cases {
            let args = vec![Value::Number(input), Value::Number(places)];
            let result = match &round_fn {
                FunctionDef::BuiltIn(def) => {
                    (def.body)(args, heap.clone(), bindings.clone(), 0).unwrap()
                }
                _ => panic!("Expected built-in function"),
            };

            // Use approximate comparison for floating point
            if let Value::Number(result_num) = result {
                assert!(
                    (result_num - expected).abs() < 1e-10,
                    "round({}, {}) = {} should be close to {}",
                    input,
                    places,
                    result_num,
                    expected
                );
            } else {
                panic!("Expected number result");
            }
        }
    }

    #[test]
    fn test_round_function_negative_decimal_places() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        let round_fn =
            get_built_in_function_def(get_built_in_function_id("round").unwrap()).unwrap();

        // Test rounding to tens, hundreds, etc.
        let test_cases = vec![
            (1234.567, -1.0, 1230.0),
            (1234.567, -2.0, 1200.0),
            (1234.567, -3.0, 1000.0),
            (1234.567, -4.0, 0.0),
            (5678.9, -1.0, 5680.0),
            (5678.9, -2.0, 5700.0),
            (5678.9, -3.0, 6000.0),
            (-1234.567, -1.0, -1230.0),
            (-1234.567, -2.0, -1200.0),
            (1500.0, -3.0, 2000.0),
            (-1500.0, -3.0, -2000.0),
        ];

        for (input, places, expected) in test_cases {
            let args = vec![Value::Number(input), Value::Number(places)];
            let result = match &round_fn {
                FunctionDef::BuiltIn(def) => {
                    (def.body)(args, heap.clone(), bindings.clone(), 0).unwrap()
                }
                _ => panic!("Expected built-in function"),
            };
            assert_eq!(
                result,
                Value::Number(expected),
                "round({}, {}) should be {}",
                input,
                places,
                expected
            );
        }
    }

    #[test]
    fn test_round_function_edge_cases() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        let round_fn =
            get_built_in_function_def(get_built_in_function_id("round").unwrap()).unwrap();

        // Test edge cases
        let test_cases = vec![
            (f64::INFINITY, 0.0, f64::INFINITY),
            (f64::NEG_INFINITY, 0.0, f64::NEG_INFINITY),
            (0.0, 5.0, 0.0),
            (-0.0, 5.0, -0.0),
            (1e-10, 5.0, 0.0),
            (1e-10, 15.0, 1e-10),
        ];

        for (input, places, expected) in test_cases {
            let args = vec![Value::Number(input), Value::Number(places)];
            let result = match &round_fn {
                FunctionDef::BuiltIn(def) => {
                    (def.body)(args, heap.clone(), bindings.clone(), 0).unwrap()
                }
                _ => panic!("Expected built-in function"),
            };

            if let Value::Number(result_num) = result {
                if expected.is_infinite() {
                    assert!(
                        result_num.is_infinite()
                            && result_num.is_sign_positive() == expected.is_sign_positive(),
                        "round({}, {}) should be {}",
                        input,
                        places,
                        expected
                    );
                } else if expected == 0.0 || expected == -0.0 {
                    assert!(
                        result_num.abs() < 1e-10,
                        "round({}, {}) = {} should be close to 0",
                        input,
                        places,
                        result_num
                    );
                } else {
                    assert!(
                        (result_num - expected).abs() < 1e-15,
                        "round({}, {}) = {} should be close to {}",
                        input,
                        places,
                        result_num,
                        expected
                    );
                }
            } else {
                panic!("Expected number result");
            }
        }
    }
}
