use crate::{
    expressions::evaluate_ast,
    heap::{Heap, HeapPointer, IterablePointer},
    values::{FunctionArity, LambdaArg, LambdaDef, ReifiedValue, Value},
};
use anyhow::{Result, anyhow};
use dyn_fmt::AsStrFormatExt;
use std::{cell::RefCell, collections::HashMap, rc::Rc, sync::LazyLock};

#[cfg(not(target_arch = "wasm32"))]
use std::sync::Mutex;

#[cfg(not(target_arch = "wasm32"))]
use crate::stats::FunctionCallStats;

#[cfg(not(target_arch = "wasm32"))]
pub static FUNCTION_CALLS: LazyLock<Mutex<Vec<FunctionCallStats>>> =
    LazyLock::new(|| Mutex::new(Vec::new()));

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, serde::Serialize, serde::Deserialize,
)]
pub enum BuiltInFunction {
    // Math functions
    Sqrt,
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Log,
    Log10,
    Exp,
    Abs,
    Floor,
    Ceil,
    Round,
    Trunc,

    // Aggregate functions
    Min,
    Max,
    Avg,
    Sum,
    Prod,
    Median,
    Percentile,

    // List functions
    Range,
    Len,
    Head,
    Tail,
    Slice,
    Concat,
    Dot,
    Unique,
    Sort,
    SortBy,
    Reverse,

    // Higher-order functions
    Map,
    Reduce,
    Filter,
    Every,
    Some,

    // String functions
    Split,
    Join,
    Replace,
    Trim,
    Uppercase,
    Lowercase,
    ToString,
    ToNumber,
    Includes,
    Format,

    // Type functions
    Typeof,
    Arity,

    // Record functions
    Keys,
    Values,
    Entries,

    // Platform-specific functions
    #[cfg(not(target_arch = "wasm32"))]
    Print,
    #[cfg(not(target_arch = "wasm32"))]
    TimeNow,
}

#[derive(Debug, Clone)]
pub enum FunctionDef {
    BuiltIn(BuiltInFunction),
    Lambda(LambdaDef),
}

impl BuiltInFunction {
    pub fn from_ident(ident: &str) -> Option<Self> {
        match ident {
            "sqrt" => Some(Self::Sqrt),
            "sin" => Some(Self::Sin),
            "cos" => Some(Self::Cos),
            "tan" => Some(Self::Tan),
            "asin" => Some(Self::Asin),
            "acos" => Some(Self::Acos),
            "atan" => Some(Self::Atan),
            "log" => Some(Self::Log),
            "log10" => Some(Self::Log10),
            "exp" => Some(Self::Exp),
            "abs" => Some(Self::Abs),
            "floor" => Some(Self::Floor),
            "ceil" => Some(Self::Ceil),
            "round" => Some(Self::Round),
            "trunc" => Some(Self::Trunc),
            "min" => Some(Self::Min),
            "max" => Some(Self::Max),
            "avg" => Some(Self::Avg),
            "sum" => Some(Self::Sum),
            "prod" => Some(Self::Prod),
            "median" => Some(Self::Median),
            "percentile" => Some(Self::Percentile),
            "range" => Some(Self::Range),
            "len" => Some(Self::Len),
            "head" => Some(Self::Head),
            "tail" => Some(Self::Tail),
            "slice" => Some(Self::Slice),
            "concat" => Some(Self::Concat),
            "dot" => Some(Self::Dot),
            "unique" => Some(Self::Unique),
            "sort" => Some(Self::Sort),
            "sort_by" => Some(Self::SortBy),
            "reverse" => Some(Self::Reverse),
            "map" => Some(Self::Map),
            "reduce" => Some(Self::Reduce),
            "filter" => Some(Self::Filter),
            "every" => Some(Self::Every),
            "some" => Some(Self::Some),
            "split" => Some(Self::Split),
            "join" => Some(Self::Join),
            "replace" => Some(Self::Replace),
            "trim" => Some(Self::Trim),
            "uppercase" => Some(Self::Uppercase),
            "lowercase" => Some(Self::Lowercase),
            "to_string" => Some(Self::ToString),
            "to_number" => Some(Self::ToNumber),
            "includes" => Some(Self::Includes),
            "format" => Some(Self::Format),
            "typeof" => Some(Self::Typeof),
            "arity" => Some(Self::Arity),
            "keys" => Some(Self::Keys),
            "values" => Some(Self::Values),
            "entries" => Some(Self::Entries),
            #[cfg(not(target_arch = "wasm32"))]
            "print" => Some(Self::Print),
            #[cfg(not(target_arch = "wasm32"))]
            "time_now" => Some(Self::TimeNow),
            _ => None,
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            Self::Sqrt => "sqrt",
            Self::Sin => "sin",
            Self::Cos => "cos",
            Self::Tan => "tan",
            Self::Asin => "asin",
            Self::Acos => "acos",
            Self::Atan => "atan",
            Self::Log => "log",
            Self::Log10 => "log10",
            Self::Exp => "exp",
            Self::Abs => "abs",
            Self::Floor => "floor",
            Self::Ceil => "ceil",
            Self::Round => "round",
            Self::Trunc => "trunc",
            Self::Min => "min",
            Self::Max => "max",
            Self::Avg => "avg",
            Self::Sum => "sum",
            Self::Prod => "prod",
            Self::Median => "median",
            Self::Percentile => "percentile",
            Self::Range => "range",
            Self::Len => "len",
            Self::Head => "head",
            Self::Tail => "tail",
            Self::Slice => "slice",
            Self::Concat => "concat",
            Self::Dot => "dot",
            Self::Unique => "unique",
            Self::Sort => "sort",
            Self::SortBy => "sort_by",
            Self::Reverse => "reverse",
            Self::Map => "map",
            Self::Reduce => "reduce",
            Self::Filter => "filter",
            Self::Every => "every",
            Self::Some => "some",
            Self::Split => "split",
            Self::Join => "join",
            Self::Replace => "replace",
            Self::Trim => "trim",
            Self::Uppercase => "uppercase",
            Self::Lowercase => "lowercase",
            Self::ToString => "to_string",
            Self::ToNumber => "to_number",
            Self::Includes => "includes",
            Self::Format => "format",
            Self::Typeof => "typeof",
            Self::Arity => "arity",
            Self::Keys => "keys",
            Self::Values => "values",
            Self::Entries => "entries",
            #[cfg(not(target_arch = "wasm32"))]
            Self::Print => "print",
            #[cfg(not(target_arch = "wasm32"))]
            Self::TimeNow => "time_now",
        }
    }

    pub fn arity(&self) -> FunctionArity {
        match self {
            // Math functions - single argument
            Self::Sqrt
            | Self::Sin
            | Self::Cos
            | Self::Tan
            | Self::Asin
            | Self::Acos
            | Self::Atan
            | Self::Log
            | Self::Log10
            | Self::Exp
            | Self::Abs
            | Self::Floor
            | Self::Ceil
            | Self::Trunc => FunctionArity::Exact(1),

            // Round can take 1 or 2 arguments
            Self::Round => FunctionArity::Between(1, 2),

            // Aggregate functions
            Self::Min | Self::Max | Self::Avg | Self::Sum | Self::Prod | Self::Median => {
                FunctionArity::AtLeast(1)
            }

            // Range can take 1 or 2 arguments
            Self::Range => FunctionArity::Between(1, 2),

            // List functions
            Self::Len | Self::Head | Self::Tail | Self::Unique | Self::Sort | Self::Reverse => {
                FunctionArity::Exact(1)
            }
            Self::Slice => FunctionArity::Exact(3),
            Self::Concat => FunctionArity::AtLeast(2),
            Self::Dot | Self::Percentile => FunctionArity::Exact(2),

            // Higher-order functions
            Self::Map | Self::Filter | Self::Every | Self::Some | Self::SortBy => {
                FunctionArity::Exact(2)
            }
            Self::Reduce => FunctionArity::Exact(3),

            // String functions
            Self::Split | Self::Join | Self::Includes => FunctionArity::Exact(2),
            Self::Replace => FunctionArity::Exact(3),
            Self::Trim | Self::Uppercase | Self::Lowercase | Self::ToString | Self::ToNumber => {
                FunctionArity::Exact(1)
            }
            Self::Format => FunctionArity::AtLeast(1),

            // Type functions
            Self::Typeof | Self::Arity => FunctionArity::Exact(1),

            // Record functions
            Self::Keys | Self::Values | Self::Entries => FunctionArity::Exact(1),

            // Platform-specific functions
            #[cfg(not(target_arch = "wasm32"))]
            Self::Print => FunctionArity::AtLeast(1),
            #[cfg(not(target_arch = "wasm32"))]
            Self::TimeNow => FunctionArity::Exact(0),
        }
    }

    pub fn call(
        &self,
        args: Vec<Value>,
        heap: Rc<RefCell<Heap>>,
        bindings: Rc<RefCell<HashMap<String, Value>>>,
        call_depth: usize,
    ) -> Result<Value> {
        match self {
            // Math functions
            Self::Sqrt => Ok(Value::Number(args[0].as_number()?.sqrt())),
            Self::Sin => Ok(Value::Number(args[0].as_number()?.sin())),
            Self::Cos => Ok(Value::Number(args[0].as_number()?.cos())),
            Self::Tan => Ok(Value::Number(args[0].as_number()?.tan())),
            Self::Asin => Ok(Value::Number(args[0].as_number()?.asin())),
            Self::Acos => Ok(Value::Number(args[0].as_number()?.acos())),
            Self::Atan => Ok(Value::Number(args[0].as_number()?.atan())),
            Self::Log => Ok(Value::Number(args[0].as_number()?.ln())),
            Self::Log10 => Ok(Value::Number(args[0].as_number()?.log10())),
            Self::Exp => Ok(Value::Number(args[0].as_number()?.exp())),
            Self::Abs => Ok(Value::Number(args[0].as_number()?.abs())),
            Self::Floor => Ok(Value::Number(args[0].as_number()?.floor())),
            Self::Ceil => Ok(Value::Number(args[0].as_number()?.ceil())),
            Self::Trunc => Ok(Value::Number(args[0].as_number()?.trunc())),

            Self::Round => {
                let num = args[0].as_number()?;
                if args.len() == 1 {
                    Ok(Value::Number(num.round()))
                } else {
                    let decimal_places = args[1].as_number()? as i32;
                    let multiplier = 10_f64.powi(decimal_places);
                    Ok(Value::Number((num * multiplier).round() / multiplier))
                }
            }

            // Aggregate functions
            Self::Min => {
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
            }

            Self::Max => {
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
            }

            Self::Avg => {
                let nums = if args.len() == 1 {
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
            }

            Self::Prod => {
                let nums = if args.len() == 1 {
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
            }

            Self::Range => {
                let (start, end) = match args[..] {
                    [Value::Number(start)] => (0.0, start),
                    [Value::Number(start), Value::Number(end)] => (start, end),
                    _ => return Err(anyhow!("range requires 1 or 2 numbers")),
                };

                if start > end {
                    return Err(anyhow!(
                        "range requires start to be less than or equal to end"
                    ));
                }

                if !f64::is_finite(start) || !f64::is_finite(end) {
                    return Err(anyhow!("range requires finite numbers"));
                }

                let start_i64 = start as i64;
                let end_i64 = end as i64;
                let length = end_i64 - start_i64;

                if length > u32::MAX as i64 {
                    return Err(anyhow!(
                        "list would be longer than the maximum length of {}",
                        u32::MAX
                    ));
                }

                let values = (start_i64..end_i64)
                    .map(|e| Value::Number(e as f64))
                    .collect();
                let list = heap.borrow_mut().insert_list(values);

                Ok(list)
            }

            Self::Sum => {
                let nums = if args.len() == 1 {
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
            }

            // Remaining aggregate functions
            Self::Median => {
                let mut nums = if args.len() == 1 {
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
            }

            Self::Percentile => {
                let p = args[1].as_number()?;
                let heap = &heap.borrow();
                let list = args[0].as_list(heap)?;

                if !(0.0..=100.0).contains(&p) {
                    return Err(anyhow!("percentile must be between 0 and 100"));
                }

                let mut nums = list
                    .iter()
                    .map(|a| a.as_number())
                    .collect::<Result<Vec<f64>>>()?;

                nums.sort_by(|a, b| a.partial_cmp(b).unwrap());
                let index = (p / 100.0 * (nums.len() - 1) as f64).round() as usize;

                Ok(Value::Number(nums[index]))
            }

            // List functions
            Self::Len => match &args[0] {
                Value::List(l) => Ok(Value::Number(
                    l.reify(&heap.borrow()).as_list()?.len() as f64
                )),
                Value::String(s) => Ok(Value::Number(
                    s.reify(&heap.borrow()).as_string()?.len() as f64
                )),
                _ => Err(anyhow!("argument must be a list or string")),
            },

            Self::Head => match &args[0] {
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

            Self::Tail => match &args[0] {
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

            Self::Slice => {
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
            }

            Self::Concat => {
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
            }

            Self::Dot => {
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
            }

            Self::Unique => {
                let list = {
                    let borrowed_heap = &heap.borrow();
                    args[0].as_list(borrowed_heap)?.clone()
                };
                let mut unique_list = vec![];
                let borrowed_heap = heap.borrow();

                for item in list {
                    let mut is_duplicate = false;
                    for existing in &unique_list {
                        if item.equals(existing, &borrowed_heap)? {
                            is_duplicate = true;
                            break;
                        }
                    }
                    if !is_duplicate {
                        unique_list.push(item);
                    }
                }

                drop(borrowed_heap);
                Ok(heap.borrow_mut().insert_list(unique_list))
            }

            Self::Sort => {
                let mut list = {
                    let borrowed_heap = &heap.borrow();
                    args[0].as_list(borrowed_heap)?.clone()
                };
                let borrowed_heap = heap.borrow();
                list.sort_by(|a, b| {
                    a.compare(b, &borrowed_heap)
                        .unwrap_or(None)
                        .unwrap_or(std::cmp::Ordering::Equal)
                });
                drop(borrowed_heap);
                Ok(heap.borrow_mut().insert_list(list))
            }

            Self::Reverse => {
                let mut list = { args[0].as_list(&heap.borrow())?.clone() };
                list.reverse();
                Ok(heap.borrow_mut().insert_list(list))
            }

            // String functions
            Self::Split => {
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
            }

            Self::Join => {
                let joined_string = {
                    let borrowed_heap = &heap.borrow();
                    let delimeter = args[1].as_string(borrowed_heap)?;
                    let list = args[0].as_list(borrowed_heap)?;
                    list.iter()
                        .map(|v| v.stringify_internal(borrowed_heap))
                        .collect::<Vec<String>>()
                        .join(delimeter)
                };

                Ok(heap.borrow_mut().insert_string(joined_string))
            }

            Self::Replace => {
                let string = {
                    let borrowed_heap = heap.borrow();
                    let old = args[1].as_string(&borrowed_heap)?.to_string();
                    let new = args[2].as_string(&borrowed_heap)?.to_string();
                    let s = args[0].as_string(&borrowed_heap)?.to_string();
                    s.replace(&old, &new)
                };

                Ok(heap.borrow_mut().insert_string(string))
            }

            Self::Trim => {
                let string = {
                    let borrowed_heap = heap.borrow();
                    args[0].as_string(&borrowed_heap)?.trim().to_string()
                };

                Ok(heap.borrow_mut().insert_string(string))
            }

            Self::Uppercase => {
                let string = args[0].as_string(&heap.borrow())?.to_uppercase();
                Ok(heap.borrow_mut().insert_string(string))
            }

            Self::Lowercase => {
                let string = args[0].as_string(&heap.borrow())?.to_lowercase();
                Ok(heap.borrow_mut().insert_string(string))
            }

            Self::ToString => {
                let string = args[0].stringify_internal(&heap.borrow());
                Ok(heap.borrow_mut().insert_string(string))
            }

            Self::ToNumber => Ok(Value::Number(args[0].as_string(&heap.borrow())?.parse()?)),

            Self::Includes => {
                let needle = {
                    let borrowed_heap = &heap.borrow();
                    args[1].as_string(borrowed_heap)?.to_string()
                };

                match &args[0].reify(&heap.borrow())? {
                    ReifiedValue::List(l, _) => Ok(Value::Bool(
                        (*l).iter()
                            .any(|v| v.as_string(&heap.borrow()).unwrap().eq(&needle)),
                    )),
                    ReifiedValue::String(s, _) => Ok(Value::Bool(s.contains(&needle))),
                    _ => Err(anyhow!("second argument must be a list or string")),
                }
            }

            Self::Format => {
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
                        .iter()
                        .map(|v| v.stringify_internal(borrowed_heap))
                        .collect::<Vec<String>>()
                };

                Ok(heap
                    .borrow_mut()
                    .insert_string(format_str.format(format_args)))
            }

            // Type functions
            Self::Typeof => Ok(heap
                .borrow_mut()
                .insert_string(args[0].get_type().to_string())),

            Self::Arity => {
                let arity = match args[0] {
                    Value::Lambda(p) => p.reify(&heap.borrow()).as_lambda()?.get_arity(),
                    Value::BuiltIn(built_in) => built_in.arity(),
                    _ => return Err(anyhow!("argument must be a function or built-in function")),
                };

                Ok(Value::Number(match arity {
                    FunctionArity::Exact(n) => n as f64,
                    FunctionArity::AtLeast(n) => n as f64,
                    FunctionArity::Between(min, _max) => min as f64,
                }))
            }

            // Record functions
            Self::Keys => {
                let record = args[0].as_record(&heap.borrow())?.clone();
                let keys = {
                    let key_strings = record.keys().cloned().collect::<Vec<String>>();
                    key_strings
                        .iter()
                        .map(|k| heap.borrow_mut().insert_string(k.to_string()))
                        .collect()
                };

                Ok(heap.borrow_mut().insert_list(keys))
            }

            Self::Values => {
                let record = args[0].as_record(&heap.borrow())?.clone();
                let values = record.values().cloned().collect();

                Ok(heap.borrow_mut().insert_list(values))
            }

            Self::Entries => {
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
            }

            // Platform-specific functions
            #[cfg(not(target_arch = "wasm32"))]
            Self::Print => {
                let borrowed_heap = &heap.borrow();

                let output = if args.len() == 1 {
                    args[0].stringify_internal(borrowed_heap)
                } else {
                    let format_str = args[0].as_string(borrowed_heap).map_err(|_| {
                        anyhow!("first argument must be a formatting string if multiple arguments are given")
                    })?;
                    let format_args = &args[1..]
                        .iter()
                        .map(|v| v.stringify_internal(borrowed_heap))
                        .collect::<Vec<String>>();
                    format_str.format(format_args)
                };

                // Print to stderr, to not pollute stdout
                eprintln!("{}", output);

                Ok(Value::Null)
            }

            #[cfg(not(target_arch = "wasm32"))]
            Self::TimeNow => Ok(Value::Number(
                std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .as_secs_f64(),
            )),

            // Higher-order functions
            Self::Map => {
                let func = &args[1];
                let list = {
                    let borrowed_heap = &heap.borrow();
                    let list = args[0].as_list(borrowed_heap)?.clone();
                    let func_def = get_function_def(func, borrowed_heap)
                        .ok_or_else(|| anyhow!("second argument must be a function"))?;
                    drop(func_def);
                    list
                };

                let mut mapped_list = vec![];
                for item in list {
                    let func_def = get_function_def(func, &heap.borrow())
                        .ok_or_else(|| anyhow!("second argument must be a function"))?;
                    let result = func_def.call(
                        Value::Null,
                        vec![item],
                        Rc::clone(&heap),
                        Rc::clone(&bindings),
                        call_depth + 1,
                    )?;
                    mapped_list.push(result);
                }

                Ok(heap.borrow_mut().insert_list(mapped_list))
            }

            Self::Filter => {
                let func = &args[1];
                let list = {
                    let borrowed_heap = &heap.borrow();
                    args[0].as_list(borrowed_heap)?.clone()
                };

                let mut filtered_list = vec![];
                for item in list {
                    let func_def = get_function_def(func, &heap.borrow())
                        .ok_or_else(|| anyhow!("second argument must be a function"))?;
                    let result = func_def.call(
                        Value::Null,
                        vec![item],
                        Rc::clone(&heap),
                        Rc::clone(&bindings),
                        call_depth + 1,
                    )?;
                    if result.as_bool()? {
                        filtered_list.push(item);
                    }
                }

                Ok(heap.borrow_mut().insert_list(filtered_list))
            }

            Self::Reduce => {
                let func = &args[1];
                let initial = args[2];
                let list = {
                    let borrowed_heap = &heap.borrow();
                    args[0].as_list(borrowed_heap)?.clone()
                };

                let mut accumulator = initial;
                for item in list {
                    let func_def = get_function_def(func, &heap.borrow())
                        .ok_or_else(|| anyhow!("second argument must be a function"))?;
                    accumulator = func_def.call(
                        Value::Null,
                        vec![accumulator, item],
                        Rc::clone(&heap),
                        Rc::clone(&bindings),
                        call_depth + 1,
                    )?;
                }

                Ok(accumulator)
            }

            Self::Every => {
                let func = &args[1];
                let list = {
                    let borrowed_heap = &heap.borrow();
                    args[0].as_list(borrowed_heap)?.clone()
                };

                for item in list {
                    let func_def = get_function_def(func, &heap.borrow())
                        .ok_or_else(|| anyhow!("second argument must be a function"))?;
                    let result = func_def.call(
                        Value::Null,
                        vec![item],
                        Rc::clone(&heap),
                        Rc::clone(&bindings),
                        call_depth + 1,
                    )?;
                    if !result.as_bool()? {
                        return Ok(Value::Bool(false));
                    }
                }

                Ok(Value::Bool(true))
            }

            Self::Some => {
                let func = &args[1];
                let list = {
                    let borrowed_heap = &heap.borrow();
                    args[0].as_list(borrowed_heap)?.clone()
                };

                for item in list {
                    let func_def = get_function_def(func, &heap.borrow())
                        .ok_or_else(|| anyhow!("second argument must be a function"))?;
                    let result = func_def.call(
                        Value::Null,
                        vec![item],
                        Rc::clone(&heap),
                        Rc::clone(&bindings),
                        call_depth + 1,
                    )?;
                    if result.as_bool()? {
                        return Ok(Value::Bool(true));
                    }
                }

                Ok(Value::Bool(false))
            }

            Self::SortBy => {
                let func = &args[1];
                let mut list = {
                    let borrowed_heap = &heap.borrow();
                    args[0].as_list(borrowed_heap)?.clone()
                };

                list.sort_by(|a, b| {
                    let func_def_a = get_function_def(func, &heap.borrow());
                    let func_def_b = get_function_def(func, &heap.borrow());

                    match (func_def_a, func_def_b) {
                        (Some(fd_a), Some(fd_b)) => {
                            let result_a = fd_a.call(
                                Value::Null,
                                vec![*a],
                                Rc::clone(&heap),
                                Rc::clone(&bindings),
                                call_depth + 1,
                            );
                            let result_b = fd_b.call(
                                Value::Null,
                                vec![*b],
                                Rc::clone(&heap),
                                Rc::clone(&bindings),
                                call_depth + 1,
                            );

                            match (result_a, result_b) {
                                (Ok(val_a), Ok(val_b)) => val_a
                                    .compare(&val_b, &heap.borrow())
                                    .unwrap_or(None)
                                    .unwrap_or(std::cmp::Ordering::Equal),
                                _ => std::cmp::Ordering::Equal,
                            }
                        }
                        _ => std::cmp::Ordering::Equal,
                    }
                });

                Ok(heap.borrow_mut().insert_list(list))
            }
        }
    }

    pub fn all() -> Vec<Self> {
        vec![
            Self::Sqrt,
            Self::Sin,
            Self::Cos,
            Self::Tan,
            Self::Asin,
            Self::Acos,
            Self::Atan,
            Self::Log,
            Self::Log10,
            Self::Exp,
            Self::Abs,
            Self::Floor,
            Self::Ceil,
            Self::Round,
            Self::Trunc,
            Self::Min,
            Self::Max,
            Self::Avg,
            Self::Sum,
            Self::Prod,
            Self::Median,
            Self::Percentile,
            Self::Range,
            Self::Len,
            Self::Head,
            Self::Tail,
            Self::Slice,
            Self::Concat,
            Self::Dot,
            Self::Unique,
            Self::Sort,
            Self::SortBy,
            Self::Reverse,
            Self::Map,
            Self::Reduce,
            Self::Filter,
            Self::Every,
            Self::Some,
            Self::Split,
            Self::Join,
            Self::Replace,
            Self::Trim,
            Self::Uppercase,
            Self::Lowercase,
            Self::ToString,
            Self::ToNumber,
            Self::Includes,
            Self::Format,
            Self::Typeof,
            Self::Arity,
            Self::Keys,
            Self::Values,
            Self::Entries,
            #[cfg(not(target_arch = "wasm32"))]
            Self::Print,
            #[cfg(not(target_arch = "wasm32"))]
            Self::TimeNow,
        ]
    }

    pub fn all_names() -> Vec<&'static str> {
        Self::all().iter().map(|f| f.name()).collect()
    }
}

impl FunctionDef {
    pub fn get_name(&self) -> String {
        match self {
            FunctionDef::BuiltIn(built_in) => {
                format!("built-in function \"{}\"", built_in.name())
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
            FunctionDef::BuiltIn(built_in) => match built_in.arity() {
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

                // Start with current environment as fallback for late binding
                let mut new_bindings = bindings.borrow().clone();

                // Override with captured scope (captured variables take precedence)
                for (key, value) in scope {
                    new_bindings.insert(key.clone(), *value);
                }

                // Add self-reference if named
                if let Some(fn_name) = name {
                    new_bindings.insert(fn_name.clone(), this_value);
                }

                // Preserve inputs if present
                if let Some(inputs) = new_bindings.get("inputs").copied() {
                    new_bindings.insert(String::from("inputs"), inputs);
                }

                // Add function arguments (highest precedence)
                for (idx, expected_arg) in expected_args.iter().enumerate() {
                    match expected_arg {
                        LambdaArg::Required(arg_name) => {
                            new_bindings.insert(arg_name.clone(), args[idx]);
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

                return_value
            }
            FunctionDef::BuiltIn(built_in) => {
                let return_value = built_in
                    .call(args, heap, bindings, call_depth + 1)
                    .map_err(|error| anyhow!("in {}: {}", self.get_name(), error));

                #[cfg(not(target_arch = "wasm32"))]
                FUNCTION_CALLS.lock().unwrap().push(FunctionCallStats {
                    name: self.get_name(),
                    start,
                    end: std::time::Instant::now(),
                    start_var_env: None,
                    end_var_env: None,
                });

                return_value
            }
        }
    }
}

pub fn is_built_in_function(ident: &str) -> bool {
    BuiltInFunction::from_ident(ident).is_some()
}

pub fn get_built_in_function_def_by_ident(ident: &str) -> Option<FunctionDef> {
    BuiltInFunction::from_ident(ident).map(FunctionDef::BuiltIn)
}

pub fn get_built_in_function_idents() -> Vec<&'static str> {
    BuiltInFunction::all_names()
}

pub fn get_function_def(value: &Value, heap: &Heap) -> Option<FunctionDef> {
    match value {
        Value::Lambda(pointer) => Some(FunctionDef::Lambda(
            pointer.reify(heap).as_lambda().ok()?.clone(),
        )),
        Value::BuiltIn(built_in) => Some(FunctionDef::BuiltIn(*built_in)),
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
        let range_fn = BuiltInFunction::Range;

        // Test range(4) - exclusive, so [0, 1, 2, 3]
        let args = vec![Value::Number(4.0)];
        let result = range_fn
            .call(args, heap.clone(), bindings.clone(), 0)
            .unwrap();

        let heap_borrow = heap.borrow();
        let list = result.as_list(&heap_borrow).unwrap();
        assert_eq!(list.len(), 4);
        assert_eq!(list[0], Value::Number(0.0));
        assert_eq!(list[3], Value::Number(3.0));
    }

    #[test]
    fn test_range_function_two_args() {
        // Test two arguments
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        let range_fn = BuiltInFunction::Range;

        // Test range(4, 10) - exclusive, so [4, 5, 6, 7, 8, 9]
        let args = vec![Value::Number(4.0), Value::Number(10.0)];
        let result = range_fn
            .call(args, heap.clone(), bindings.clone(), 0)
            .unwrap();

        let heap_borrow = heap.borrow();
        let list = result.as_list(&heap_borrow).unwrap();
        assert_eq!(list.len(), 6);
        assert_eq!(list[0], Value::Number(4.0));
        assert_eq!(list[5], Value::Number(9.0));
    }

    #[test]
    fn test_round_function_single_arg() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        let round_fn = BuiltInFunction::Round;

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
            let result = round_fn
                .call(args, heap.clone(), bindings.clone(), 0)
                .unwrap();
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
        let round_fn = BuiltInFunction::Round;

        // Test rounding to decimal places
        let test_cases = vec![
            (42.4543, 0.0, 42.0),
            (42.4543, 1.0, 42.5),
            (42.4543, 2.0, 42.45),
            (42.4543, 3.0, 42.454),
            (42.4543, 4.0, 42.4543),
            (4.14159, 4.0, 4.1416),
            (0.005, 2.0, 0.01),
            (0.995, 2.0, 1.0),
            // Note: 9.995 has floating point representation issues
            // In binary, it's actually slightly less than 9.995
            (9.995, 2.0, 9.99),
            (-9.995, 2.0, -9.99),
        ];

        for (input, places, expected) in test_cases {
            let args = vec![Value::Number(input), Value::Number(places)];
            let result = round_fn
                .call(args, heap.clone(), bindings.clone(), 0)
                .unwrap();

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
        let round_fn = BuiltInFunction::Round;

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
            let result = round_fn
                .call(args, heap.clone(), bindings.clone(), 0)
                .unwrap();
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
        let round_fn = BuiltInFunction::Round;

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
            let result = round_fn
                .call(args, heap.clone(), bindings.clone(), 0)
                .unwrap();

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
