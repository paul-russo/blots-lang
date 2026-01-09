use crate::{
    environment::Environment,
    expressions::evaluate_ast,
    heap::{Heap, HeapPointer, IterablePointer},
    units,
    values::{FunctionArity, LambdaArg, LambdaDef, ReifiedValue, Value},
};
use anyhow::{Result, anyhow};
use dyn_fmt::AsStrFormatExt;
use indexmap::IndexMap;
use std::{cell::RefCell, collections::HashMap, rc::Rc, sync::LazyLock};

#[cfg(not(target_arch = "wasm32"))]
use std::sync::Mutex;

#[cfg(not(target_arch = "wasm32"))]
use crate::stats::FunctionCallStats;

#[cfg(not(target_arch = "wasm32"))]
pub static FUNCTION_CALLS: LazyLock<Mutex<Vec<FunctionCallStats>>> =
    LazyLock::new(|| Mutex::new(Vec::new()));

#[cfg(not(target_arch = "wasm32"))]
pub fn get_function_call_stats() -> Vec<FunctionCallStats> {
    FUNCTION_CALLS.lock().unwrap().clone()
}

#[cfg(not(target_arch = "wasm32"))]
pub fn clear_function_call_stats() {
    FUNCTION_CALLS.lock().unwrap().clear();
}

// Cache built-in function names since they're static
pub static BUILTIN_FUNCTION_NAMES: LazyLock<Vec<&'static str>> =
    LazyLock::new(BuiltInFunction::all_names);

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
    Random,

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
    Any,
    All,

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
    Includes,
    Format,

    // Type functions
    Typeof,
    Arity,

    // Record functions
    Keys,
    Values,
    Entries,

    // List utility functions
    GroupBy,
    CountBy,
    Flatten,
    Zip,
    Chunk,

    // Conversion functions
    ToString,
    ToNumber,
    ToBool,
    Convert,

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
            "random" => Some(Self::Random),
            "min" => Some(Self::Min),
            "max" => Some(Self::Max),
            "avg" => Some(Self::Avg),
            "sum" => Some(Self::Sum),
            "prod" => Some(Self::Prod),
            "median" => Some(Self::Median),
            "percentile" => Some(Self::Percentile),
            "range" => Some(Self::Range),
            "any" => Some(Self::Any),
            "all" => Some(Self::All),
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
            "to_bool" => Some(Self::ToBool),
            "convert" => Some(Self::Convert),
            "includes" => Some(Self::Includes),
            "format" => Some(Self::Format),
            "typeof" => Some(Self::Typeof),
            "arity" => Some(Self::Arity),
            "keys" => Some(Self::Keys),
            "values" => Some(Self::Values),
            "entries" => Some(Self::Entries),
            "group_by" => Some(Self::GroupBy),
            "count_by" => Some(Self::CountBy),
            "flatten" => Some(Self::Flatten),
            "zip" => Some(Self::Zip),
            "chunk" => Some(Self::Chunk),
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
            Self::Random => "random",
            Self::Min => "min",
            Self::Max => "max",
            Self::Avg => "avg",
            Self::Sum => "sum",
            Self::Prod => "prod",
            Self::Median => "median",
            Self::Percentile => "percentile",
            Self::Range => "range",
            Self::Any => "any",
            Self::All => "all",
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
            Self::Includes => "includes",
            Self::Format => "format",
            Self::Typeof => "typeof",
            Self::Arity => "arity",
            Self::Keys => "keys",
            Self::Values => "values",
            Self::Entries => "entries",
            Self::GroupBy => "group_by",
            Self::CountBy => "count_by",
            Self::Flatten => "flatten",
            Self::Zip => "zip",
            Self::Chunk => "chunk",
            Self::ToString => "to_string",
            Self::ToNumber => "to_number",
            Self::ToBool => "to_bool",
            Self::Convert => "convert",
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
            | Self::Trunc
            | Self::Random => FunctionArity::Exact(1),

            // Round can take 1 or 2 arguments
            Self::Round => FunctionArity::Between(1, 2),

            // Aggregate functions
            Self::Min | Self::Max | Self::Avg | Self::Sum | Self::Prod | Self::Median => {
                FunctionArity::AtLeast(1)
            }

            // Range can take 1 or 2 arguments
            Self::Range => FunctionArity::Between(1, 2),

            // List functions
            Self::Len
            | Self::Head
            | Self::Tail
            | Self::Unique
            | Self::Sort
            | Self::Reverse
            | Self::Any
            | Self::All => FunctionArity::Exact(1),
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
            Self::Trim
            | Self::Uppercase
            | Self::Lowercase
            | Self::ToString
            | Self::ToNumber
            | Self::ToBool => FunctionArity::Exact(1),
            Self::Convert => FunctionArity::Exact(3),
            Self::Format => FunctionArity::AtLeast(1),

            // Type functions
            Self::Typeof | Self::Arity => FunctionArity::Exact(1),

            // Record functions
            Self::Keys | Self::Values | Self::Entries => FunctionArity::Exact(1),

            // List utility functions
            Self::GroupBy | Self::CountBy => FunctionArity::Exact(2),
            Self::Flatten => FunctionArity::Exact(1),
            Self::Zip => FunctionArity::AtLeast(2),
            Self::Chunk => FunctionArity::Exact(2),

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
        bindings: Rc<Environment>,
        call_depth: usize,
        source: &str,
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

            Self::Random => {
                let seed = args[0].as_number()? as u64;
                let mut rng = fastrand::Rng::with_seed(seed);
                Ok(Value::Number(rng.f64()))
            }

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
                        let slice = {
                            let borrowed_heap = heap.borrow();
                            let list = args[0].as_list(&borrowed_heap)?;
                            list.get(start..end)
                                .ok_or_else(|| anyhow!("index out of bounds"))?
                                .to_vec()
                        };

                        Ok(heap.borrow_mut().insert_list(slice))
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
                        Value::List(p) => {
                            let borrowed_heap = heap.borrow();
                            let values = p.reify(&borrowed_heap).as_list()?;
                            list.extend(values.iter().copied());
                        }
                        Value::Spread(IterablePointer::List(p)) => {
                            let borrowed_heap = heap.borrow();
                            let values = p.reify(&borrowed_heap).as_list()?;
                            list.extend(values.iter().copied());
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
                let borrowed_heap = heap.borrow();
                let a = args[0].as_list(&borrowed_heap)?;
                let b = args[1].as_list(&borrowed_heap)?;

                if a.len() != b.len() {
                    return Err(anyhow!(
                        "cannot calculate dot product of lists with different lengths"
                    ));
                }

                let mut sum = 0.0;
                for (a, b) in a.iter().zip(b.iter()) {
                    sum += a.as_number()? * b.as_number()?;
                }
                Ok(Value::Number(sum))
            }

            Self::Unique => {
                let mut unique_list = vec![];
                let borrowed_heap = heap.borrow();
                let list = args[0].as_list(&borrowed_heap)?;

                for item in list.iter() {
                    let mut is_duplicate = false;
                    for existing in &unique_list {
                        if item.equals(existing, &borrowed_heap)? {
                            is_duplicate = true;
                            break;
                        }
                    }
                    if !is_duplicate {
                        unique_list.push(*item);
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

            Self::Any => {
                let borrowed_heap = heap.borrow();
                let list = args[0].as_list(&borrowed_heap)?;
                Ok(Value::Bool(
                    list.iter().any(|v| v.as_bool().unwrap_or(false)),
                ))
            }

            Self::All => {
                let borrowed_heap = heap.borrow();
                let list = args[0].as_list(&borrowed_heap)?;
                Ok(Value::Bool(
                    list.iter().all(|v| v.as_bool().unwrap_or(false)),
                ))
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

            Self::Includes => {
                match &args[0].reify(&heap.borrow())? {
                    // If haystack is a list, check for structural equality with any-type needle
                    ReifiedValue::List(l, _) => {
                        let borrowed_heap = heap.borrow();
                        for item in (*l).iter() {
                            if item.equals(&args[1], &borrowed_heap)? {
                                return Ok(Value::Bool(true));
                            }
                        }
                        Ok(Value::Bool(false))
                    }
                    // If haystack is a string, require needle to be a string and do substring search
                    ReifiedValue::String(s, _) => {
                        let needle = {
                            let borrowed_heap = &heap.borrow();
                            args[1]
                                .as_string(borrowed_heap)
                                .map_err(|_| anyhow!("second argument must be a string"))?
                                .to_string()
                        };
                        Ok(Value::Bool(s.contains(&needle)))
                    }
                    _ => Err(anyhow!("first argument must be a list or string")),
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
                        .map(|v| v.stringify_for_display(borrowed_heap))
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
                let key_strings = {
                    let borrowed_heap = heap.borrow();
                    args[0]
                        .as_record(&borrowed_heap)?
                        .keys()
                        .cloned()
                        .collect::<Vec<String>>()
                };
                let mut borrowed_heap = heap.borrow_mut();
                let keys = key_strings
                    .into_iter()
                    .map(|k| borrowed_heap.insert_string(k))
                    .collect();

                Ok(borrowed_heap.insert_list(keys))
            }

            Self::Values => {
                let values = {
                    let borrowed_heap = heap.borrow();
                    args[0]
                        .as_record(&borrowed_heap)?
                        .values()
                        .copied()
                        .collect::<Vec<Value>>()
                };

                Ok(heap.borrow_mut().insert_list(values))
            }

            Self::Entries => {
                let entries = {
                    let borrowed_heap = heap.borrow();
                    args[0]
                        .as_record(&borrowed_heap)?
                        .iter()
                        .map(|(k, v)| (k.clone(), *v))
                        .collect::<Vec<(String, Value)>>()
                };
                let mut borrowed_heap = heap.borrow_mut();
                let entry_values = entries
                    .into_iter()
                    .map(|(k, v)| {
                        let key = borrowed_heap.insert_string(k);
                        borrowed_heap.insert_list(vec![key, v])
                    })
                    .collect();

                Ok(borrowed_heap.insert_list(entry_values))
            }

            // List utility functions
            Self::GroupBy => {
                let func = &args[1];
                let list_ptr = args[0].as_list_pointer()?;
                let func_def = {
                    let borrowed_heap = heap.borrow();
                    get_function_def(func, &borrowed_heap)
                        .ok_or_else(|| anyhow!("second argument must be a function"))?
                };
                let list_len = {
                    let borrowed_heap = heap.borrow();
                    list_ptr.reify(&borrowed_heap).as_list()?.len()
                };

                let mut groups: IndexMap<String, Vec<Value>> = IndexMap::new();
                for idx in 0..list_len {
                    let item = {
                        let borrowed_heap = heap.borrow();
                        let list = list_ptr.reify(&borrowed_heap).as_list()?;
                        list[idx]
                    };
                    let key_result = func_def.call(
                        Value::Null,
                        vec![item],
                        Rc::clone(&heap),
                        Rc::clone(&bindings),
                        call_depth + 1,
                        source,
                    )?;

                    let key = match key_result {
                        Value::String(_) => key_result.as_string(&heap.borrow())?.to_string(),
                        _ => {
                            return Err(anyhow!(
                                "group_by key function must return a string, but got a {}",
                                key_result.get_type()
                            ));
                        }
                    };

                    groups.entry(key).or_default().push(item);
                }

                // Convert groups to record of lists
                let record: IndexMap<String, Value> = groups
                    .into_iter()
                    .map(|(k, v)| (k, heap.borrow_mut().insert_list(v)))
                    .collect();

                Ok(heap.borrow_mut().insert_record(record))
            }

            Self::CountBy => {
                let func = &args[1];
                let list_ptr = args[0].as_list_pointer()?;
                let func_def = {
                    let borrowed_heap = heap.borrow();
                    get_function_def(func, &borrowed_heap)
                        .ok_or_else(|| anyhow!("second argument must be a function"))?
                };
                let list_len = {
                    let borrowed_heap = heap.borrow();
                    list_ptr.reify(&borrowed_heap).as_list()?.len()
                };

                let mut counts: IndexMap<String, f64> = IndexMap::new();
                for idx in 0..list_len {
                    let item = {
                        let borrowed_heap = heap.borrow();
                        let list = list_ptr.reify(&borrowed_heap).as_list()?;
                        list[idx]
                    };
                    let key_result = func_def.call(
                        Value::Null,
                        vec![item],
                        Rc::clone(&heap),
                        Rc::clone(&bindings),
                        call_depth + 1,
                        source,
                    )?;

                    let key = match key_result {
                        Value::String(_) => key_result.as_string(&heap.borrow())?.to_string(),
                        _ => {
                            return Err(anyhow!(
                                "count_by key function must return a string, but got a {}",
                                key_result.get_type()
                            ));
                        }
                    };

                    *counts.entry(key).or_insert(0.0) += 1.0;
                }

                // Convert counts to record of numbers
                let record: IndexMap<String, Value> = counts
                    .into_iter()
                    .map(|(k, v)| (k, Value::Number(v)))
                    .collect();

                Ok(heap.borrow_mut().insert_record(record))
            }

            Self::Flatten => {
                let mut result = vec![];
                let borrowed_heap = heap.borrow();
                let list = args[0].as_list(&borrowed_heap)?;

                for item in list.iter().copied() {
                    match item {
                        Value::List(pointer) => {
                            let inner = pointer.reify(&borrowed_heap).as_list()?;
                            result.extend(inner.iter().copied());
                        }
                        _ => result.push(item),
                    }
                }

                drop(borrowed_heap);
                Ok(heap.borrow_mut().insert_list(result))
            }

            Self::Zip => {
                // Extract all lists from args
                let list_ptrs = args
                    .iter()
                    .map(|arg| {
                        arg.as_list_pointer()
                            .map_err(|_| anyhow!("all arguments to zip must be lists"))
                    })
                    .collect::<Result<Vec<_>>>()?;

                // Find max length
                let max_len = {
                    let borrowed_heap = heap.borrow();
                    let mut max_len = 0;
                    for ptr in &list_ptrs {
                        let len = ptr.reify(&borrowed_heap).as_list()?.len();
                        max_len = max_len.max(len);
                    }
                    max_len
                };

                // Build tuples
                let mut result = Vec::with_capacity(max_len);
                for i in 0..max_len {
                    let tuple = {
                        let borrowed_heap = heap.borrow();
                        let mut tuple = Vec::with_capacity(list_ptrs.len());
                        for ptr in &list_ptrs {
                            let list = ptr.reify(&borrowed_heap).as_list()?;
                            tuple.push(list.get(i).copied().unwrap_or(Value::Null));
                        }
                        tuple
                    };
                    result.push(heap.borrow_mut().insert_list(tuple));
                }

                Ok(heap.borrow_mut().insert_list(result))
            }

            Self::Chunk => {
                let n = args[1].as_number()? as usize;

                if n == 0 {
                    return Err(anyhow!("chunk size must be greater than 0"));
                }

                let chunk_values = {
                    let borrowed_heap = heap.borrow();
                    let list = args[0].as_list(&borrowed_heap)?;
                    list.chunks(n).map(|chunk| chunk.to_vec()).collect::<Vec<_>>()
                };
                let mut borrowed_heap = heap.borrow_mut();
                let chunks = chunk_values
                    .into_iter()
                    .map(|chunk| borrowed_heap.insert_list(chunk))
                    .collect();

                Ok(borrowed_heap.insert_list(chunks))
            }

            // Conversion functions
            Self::ToString => match args[0] {
                Value::String(_) => Ok(args[0]), // If it's already a string, just return it
                _ => {
                    let string = args[0].stringify_internal(&heap.borrow());
                    Ok(heap.borrow_mut().insert_string(string))
                }
            },

            Self::ToNumber => match args[0] {
                Value::Number(_) => Ok(args[0]), // If it's already a number, just return it
                Value::Bool(b) => Ok(Value::Number(if b { 1.0 } else { 0.0 })),
                _ => Ok(Value::Number(args[0].as_string(&heap.borrow())?.parse()?)),
            },

            Self::ToBool => match args[0] {
                Value::Bool(_) => Ok(args[0]), // If it's already a boolean, just return it
                Value::Number(_) => Ok(Value::Bool(args[0].as_number()? != 0.0)),
                _ => Err(anyhow!(
                    "expected a boolean or number, but got a {}",
                    args[0].get_type()
                )),
            },

            Self::Convert => {
                let value = args[0].as_number()?;
                let from_unit = {
                    let borrowed_heap = &heap.borrow();
                    args[1].as_string(borrowed_heap)?.to_string()
                };
                let to_unit = {
                    let borrowed_heap = &heap.borrow();
                    args[2].as_string(borrowed_heap)?.to_string()
                };

                let result = units::convert(value, &from_unit, &to_unit)?;
                Ok(Value::Number(result))
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
                let list_ptr = args[0].as_list_pointer()?;
                let func_def = {
                    let borrowed_heap = heap.borrow();
                    get_function_def(func, &borrowed_heap)
                        .ok_or_else(|| anyhow!("second argument must be a function"))?
                };
                let list_len = {
                    let borrowed_heap = heap.borrow();
                    list_ptr.reify(&borrowed_heap).as_list()?.len()
                };
                let func_accepts_two_args = func_def.arity().can_accept(2);

                let mut mapped_list = Vec::with_capacity(list_len);
                for idx in 0..list_len {
                    let item = {
                        let borrowed_heap = heap.borrow();
                        let list = list_ptr.reify(&borrowed_heap).as_list()?;
                        list[idx]
                    };
                    let args = if func_accepts_two_args {
                        vec![item, Value::Number(idx as f64)]
                    } else {
                        vec![item]
                    };

                    let result = func_def.call(
                        Value::Null,
                        args,
                        Rc::clone(&heap),
                        Rc::clone(&bindings),
                        call_depth + 1,
                        source,
                    )?;
                    mapped_list.push(result);
                }

                Ok(heap.borrow_mut().insert_list(mapped_list))
            }

            Self::Filter => {
                let func = &args[1];
                let list_ptr = args[0].as_list_pointer()?;
                let func_def = {
                    let borrowed_heap = heap.borrow();
                    get_function_def(func, &borrowed_heap)
                        .ok_or_else(|| anyhow!("second argument must be a function"))?
                };
                let list_len = {
                    let borrowed_heap = heap.borrow();
                    list_ptr.reify(&borrowed_heap).as_list()?.len()
                };
                let func_accepts_two_args = func_def.arity().can_accept(2);

                let mut filtered_list = vec![];
                for idx in 0..list_len {
                    let item = {
                        let borrowed_heap = heap.borrow();
                        let list = list_ptr.reify(&borrowed_heap).as_list()?;
                        list[idx]
                    };
                    let args = if func_accepts_two_args {
                        vec![item, Value::Number(idx as f64)]
                    } else {
                        vec![item]
                    };

                    let result = func_def.call(
                        Value::Null,
                        args,
                        Rc::clone(&heap),
                        Rc::clone(&bindings),
                        call_depth + 1,
                        source,
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
                let list_ptr = args[0].as_list_pointer()?;
                let func_def = {
                    let borrowed_heap = heap.borrow();
                    get_function_def(func, &borrowed_heap)
                        .ok_or_else(|| anyhow!("second argument must be a function"))?
                };
                let list_len = {
                    let borrowed_heap = heap.borrow();
                    list_ptr.reify(&borrowed_heap).as_list()?.len()
                };
                let func_accepts_three_args = func_def.arity().can_accept(3);

                let mut accumulator = initial;
                for idx in 0..list_len {
                    let item = {
                        let borrowed_heap = heap.borrow();
                        let list = list_ptr.reify(&borrowed_heap).as_list()?;
                        list[idx]
                    };
                    let args = if func_accepts_three_args {
                        vec![accumulator, item, Value::Number(idx as f64)]
                    } else {
                        vec![accumulator, item]
                    };

                    accumulator = func_def.call(
                        Value::Null,
                        args,
                        Rc::clone(&heap),
                        Rc::clone(&bindings),
                        call_depth + 1,
                        source,
                    )?;
                }

                Ok(accumulator)
            }

            Self::Every => {
                let func = &args[1];
                let list_ptr = args[0].as_list_pointer()?;
                let func_def = {
                    let borrowed_heap = heap.borrow();
                    get_function_def(func, &borrowed_heap)
                        .ok_or_else(|| anyhow!("second argument must be a function"))?
                };
                let list_len = {
                    let borrowed_heap = heap.borrow();
                    list_ptr.reify(&borrowed_heap).as_list()?.len()
                };
                let func_accepts_two_args = func_def.arity().can_accept(2);

                for idx in 0..list_len {
                    let item = {
                        let borrowed_heap = heap.borrow();
                        let list = list_ptr.reify(&borrowed_heap).as_list()?;
                        list[idx]
                    };
                    let args = if func_accepts_two_args {
                        vec![item, Value::Number(idx as f64)]
                    } else {
                        vec![item]
                    };

                    let result = func_def.call(
                        Value::Null,
                        args,
                        Rc::clone(&heap),
                        Rc::clone(&bindings),
                        call_depth + 1,
                        source,
                    )?;
                    if !result.as_bool()? {
                        return Ok(Value::Bool(false));
                    }
                }

                Ok(Value::Bool(true))
            }

            Self::Some => {
                let func = &args[1];
                let list_ptr = args[0].as_list_pointer()?;
                let func_def = {
                    let borrowed_heap = heap.borrow();
                    get_function_def(func, &borrowed_heap)
                        .ok_or_else(|| anyhow!("second argument must be a function"))?
                };
                let list_len = {
                    let borrowed_heap = heap.borrow();
                    list_ptr.reify(&borrowed_heap).as_list()?.len()
                };
                let func_accepts_two_args = func_def.arity().can_accept(2);

                for idx in 0..list_len {
                    let item = {
                        let borrowed_heap = heap.borrow();
                        let list = list_ptr.reify(&borrowed_heap).as_list()?;
                        list[idx]
                    };
                    let args = if func_accepts_two_args {
                        vec![item, Value::Number(idx as f64)]
                    } else {
                        vec![item]
                    };

                    let result = func_def.call(
                        Value::Null,
                        args,
                        Rc::clone(&heap),
                        Rc::clone(&bindings),
                        call_depth + 1,
                        source,
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
                    // Only look up the function once, not twice
                    let func_def = get_function_def(func, &heap.borrow());

                    match func_def {
                        Some(fd) => {
                            let result_a = fd.call(
                                Value::Null,
                                vec![*a],
                                Rc::clone(&heap),
                                Rc::clone(&bindings),
                                call_depth + 1,
                                source,
                            );
                            let result_b = fd.call(
                                Value::Null,
                                vec![*b],
                                Rc::clone(&heap),
                                Rc::clone(&bindings),
                                call_depth + 1,
                                source,
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
            Self::Random,
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
            Self::Any,
            Self::All,
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
            Self::ToBool,
            Self::Convert,
            Self::Includes,
            Self::Format,
            Self::Typeof,
            Self::Arity,
            Self::Keys,
            Self::Values,
            Self::Entries,
            Self::GroupBy,
            Self::CountBy,
            Self::Flatten,
            Self::Zip,
            Self::Chunk,
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

    pub fn arity(&self) -> FunctionArity {
        match self {
            FunctionDef::BuiltIn(built_in) => built_in.arity(),
            FunctionDef::Lambda(lambda_def) => lambda_def.get_arity(),
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
        bindings: Rc<Environment>,
        call_depth: usize,
        source: &str,
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
                source: lambda_source,
            }) => {
                #[cfg(not(target_arch = "wasm32"))]
                let start_var_env = std::time::Instant::now();

                // Build local bindings for this call (O(1) - no clone of parent environment!)
                let mut local_bindings = HashMap::new();

                // Add self-reference if named
                if let Some(fn_name) = name {
                    local_bindings.insert(fn_name.clone(), this_value);
                }

                // Preserve inputs if present in parent
                if let Some(inputs) = bindings.get("inputs") {
                    local_bindings.insert(String::from("inputs"), inputs);
                }

                // Add function arguments (highest precedence)
                for (idx, expected_arg) in expected_args.iter().enumerate() {
                    match expected_arg {
                        LambdaArg::Required(arg_name) => {
                            local_bindings.insert(arg_name.clone(), args[idx]);
                        }
                        LambdaArg::Optional(arg_name) => {
                            local_bindings.insert(
                                arg_name.clone(),
                                args.get(idx).copied().unwrap_or(Value::Null),
                            );
                        }
                        LambdaArg::Rest(arg_name) => {
                            local_bindings.insert(
                                arg_name.clone(),
                                heap.borrow_mut()
                                    .insert_list(args.iter().skip(idx).copied().collect()),
                            );
                        }
                    }
                }

                #[cfg(not(target_arch = "wasm32"))]
                let end_var_env = std::time::Instant::now();

                let parent_env = if scope.is_empty() {
                    Rc::clone(&bindings)
                } else {
                    Rc::new(Environment::extend_shared(Rc::clone(&bindings), scope.as_rc()))
                };
                // Create new environment that extends captured scope (O(1) instead of O(n) clone!)
                let new_env = Rc::new(Environment::extend_with(parent_env, local_bindings));

                let return_value = evaluate_ast(
                    body,
                    Rc::clone(&heap),
                    new_env,
                    call_depth + 1,
                    lambda_source.clone(),
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
                    .call(args, heap, bindings, call_depth + 1, source)
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
    use crate::environment::Environment;
    use std::cell::RefCell;
    use std::rc::Rc;

    #[test]
    fn test_range_function() {
        // Test single argument
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        let range_fn = BuiltInFunction::Range;

        // Test range(4) - exclusive, so [0, 1, 2, 3]
        let args = vec![Value::Number(4.0)];
        let result = range_fn
            .call(args, heap.clone(), bindings.clone(), 0, "")
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
        let bindings = Rc::new(Environment::new());
        let range_fn = BuiltInFunction::Range;

        // Test range(4, 10) - exclusive, so [4, 5, 6, 7, 8, 9]
        let args = vec![Value::Number(4.0), Value::Number(10.0)];
        let result = range_fn
            .call(args, heap.clone(), bindings.clone(), 0, "")
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
        let bindings = Rc::new(Environment::new());
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
                .call(args, heap.clone(), bindings.clone(), 0, "")
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
        let bindings = Rc::new(Environment::new());
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
                .call(args, heap.clone(), bindings.clone(), 0, "")
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
        let bindings = Rc::new(Environment::new());
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
                .call(args, heap.clone(), bindings.clone(), 0, "")
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
        let bindings = Rc::new(Environment::new());
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
                .call(args, heap.clone(), bindings.clone(), 0, "")
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

    #[test]
    fn test_random_function_deterministic() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        let random_fn = BuiltInFunction::Random;

        // Test that same seed produces same result
        let seed = 42.0;
        let args1 = vec![Value::Number(seed)];
        let result1 = random_fn
            .call(args1, heap.clone(), bindings.clone(), 0, "")
            .unwrap();

        let args2 = vec![Value::Number(seed)];
        let result2 = random_fn
            .call(args2, heap.clone(), bindings.clone(), 0, "")
            .unwrap();

        assert_eq!(result1, result2, "Same seed should produce same result");
    }

    #[test]
    fn test_random_function_different_seeds() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        let random_fn = BuiltInFunction::Random;

        // Test that different seeds produce different results
        let args1 = vec![Value::Number(42.0)];
        let result1 = random_fn
            .call(args1, heap.clone(), bindings.clone(), 0, "")
            .unwrap();

        let args2 = vec![Value::Number(100.0)];
        let result2 = random_fn
            .call(args2, heap.clone(), bindings.clone(), 0, "")
            .unwrap();

        assert_ne!(
            result1, result2,
            "Different seeds should produce different results"
        );
    }

    #[test]
    fn test_random_function_range() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        let random_fn = BuiltInFunction::Random;

        // Test that output is in range [0, 1)
        for seed in [0.0, 1.0, 42.0, 100.0, 999.0, 12345.0] {
            let args = vec![Value::Number(seed)];
            let result = random_fn
                .call(args, heap.clone(), bindings.clone(), 0, "")
                .unwrap();

            if let Value::Number(num) = result {
                assert!(
                    (0.0..1.0).contains(&num),
                    "Random value {} should be in range [0, 1)",
                    num
                );
            } else {
                panic!("Expected number result");
            }
        }
    }

    #[test]
    fn test_random_function_negative_seed() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        let random_fn = BuiltInFunction::Random;

        // Test with negative seeds (they get cast to u64)
        let args = vec![Value::Number(-42.0)];
        let result = random_fn
            .call(args, heap.clone(), bindings.clone(), 0, "")
            .unwrap();

        if let Value::Number(num) = result {
            assert!(
                (0.0..1.0).contains(&num),
                "Random value {} should be in range [0, 1)",
                num
            );
        } else {
            panic!("Expected number result");
        }
    }

    #[test]
    fn test_map_with_index() {
        use crate::values::{CapturedScope, LambdaDef};

        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        // Create a list [10, 20, 30]
        let list = heap.borrow_mut().insert_list(vec![
            Value::Number(10.0),
            Value::Number(20.0),
            Value::Number(30.0),
        ]);

        // Create a lambda (x, i) => x + i
        let lambda = LambdaDef {
            name: None,
            args: vec![
                crate::values::LambdaArg::Required("x".to_string()),
                crate::values::LambdaArg::Required("i".to_string()),
            ],
            body: crate::ast::Spanned::dummy(crate::ast::Expr::BinaryOp {
                op: crate::ast::BinaryOp::Add,
                left: Box::new(crate::ast::Spanned::dummy(crate::ast::Expr::Identifier(
                    "x".to_string(),
                ))),
                right: Box::new(crate::ast::Spanned::dummy(crate::ast::Expr::Identifier(
                    "i".to_string(),
                ))),
            }),
            scope: CapturedScope::default(),
            source: Rc::from(""),
        };
        let lambda_value = heap.borrow_mut().insert_lambda(lambda);

        // Call map
        let result = BuiltInFunction::Map
            .call(
                vec![list, lambda_value],
                heap.clone(),
                bindings.clone(),
                0,
                "",
            )
            .unwrap();

        // Verify result is [10, 21, 32]
        let heap_borrow = heap.borrow();
        let result_list = result.as_list(&heap_borrow).unwrap();
        assert_eq!(result_list.len(), 3);
        assert_eq!(result_list[0], Value::Number(10.0)); // 10 + 0
        assert_eq!(result_list[1], Value::Number(21.0)); // 20 + 1
        assert_eq!(result_list[2], Value::Number(32.0)); // 30 + 2
    }

    #[test]
    fn test_filter_with_index() {
        use crate::values::{CapturedScope, LambdaDef};

        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        // Create a list [10, 20, 30, 40]
        let list = heap.borrow_mut().insert_list(vec![
            Value::Number(10.0),
            Value::Number(20.0),
            Value::Number(30.0),
            Value::Number(40.0),
        ]);

        // Create a lambda (x, i) => i > 1
        let lambda = LambdaDef {
            name: None,
            args: vec![
                crate::values::LambdaArg::Required("x".to_string()),
                crate::values::LambdaArg::Required("i".to_string()),
            ],
            body: crate::ast::Spanned::dummy(crate::ast::Expr::BinaryOp {
                op: crate::ast::BinaryOp::Greater,
                left: Box::new(crate::ast::Spanned::dummy(crate::ast::Expr::Identifier(
                    "i".to_string(),
                ))),
                right: Box::new(crate::ast::Spanned::dummy(crate::ast::Expr::Number(1.0))),
            }),
            scope: CapturedScope::default(),
            source: Rc::from(""),
        };
        let lambda_value = heap.borrow_mut().insert_lambda(lambda);

        // Call filter
        let result = BuiltInFunction::Filter
            .call(
                vec![list, lambda_value],
                heap.clone(),
                bindings.clone(),
                0,
                "",
            )
            .unwrap();

        // Verify result is [30, 40] (indices 2 and 3)
        let heap_borrow = heap.borrow();
        let result_list = result.as_list(&heap_borrow).unwrap();
        assert_eq!(result_list.len(), 2);
        assert_eq!(result_list[0], Value::Number(30.0));
        assert_eq!(result_list[1], Value::Number(40.0));
    }

    #[test]
    fn test_reduce_with_index() {
        use crate::values::{CapturedScope, LambdaDef};

        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        // Create a list [10, 20, 30]
        let list = heap.borrow_mut().insert_list(vec![
            Value::Number(10.0),
            Value::Number(20.0),
            Value::Number(30.0),
        ]);

        // Create a lambda (acc, x, i) => acc + x + i
        let lambda = LambdaDef {
            name: None,
            args: vec![
                crate::values::LambdaArg::Required("acc".to_string()),
                crate::values::LambdaArg::Required("x".to_string()),
                crate::values::LambdaArg::Required("i".to_string()),
            ],
            body: crate::ast::Spanned::dummy(crate::ast::Expr::BinaryOp {
                op: crate::ast::BinaryOp::Add,
                left: Box::new(crate::ast::Spanned::dummy(crate::ast::Expr::BinaryOp {
                    op: crate::ast::BinaryOp::Add,
                    left: Box::new(crate::ast::Spanned::dummy(crate::ast::Expr::Identifier(
                        "acc".to_string(),
                    ))),
                    right: Box::new(crate::ast::Spanned::dummy(crate::ast::Expr::Identifier(
                        "x".to_string(),
                    ))),
                })),
                right: Box::new(crate::ast::Spanned::dummy(crate::ast::Expr::Identifier(
                    "i".to_string(),
                ))),
            }),
            scope: CapturedScope::default(),
            source: Rc::from(""),
        };
        let lambda_value = heap.borrow_mut().insert_lambda(lambda);

        // Call reduce with initial value 0
        let result = BuiltInFunction::Reduce
            .call(
                vec![list, lambda_value, Value::Number(0.0)],
                heap.clone(),
                bindings.clone(),
                0,
                "",
            )
            .unwrap();

        // Verify result is 63 (0 + 10+0 + 20+1 + 30+2)
        assert_eq!(result, Value::Number(63.0));
    }

    #[test]
    fn test_map_backward_compatible() {
        use crate::values::{CapturedScope, LambdaDef};

        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        // Create a list [10, 20, 30]
        let list = heap.borrow_mut().insert_list(vec![
            Value::Number(10.0),
            Value::Number(20.0),
            Value::Number(30.0),
        ]);

        // Create a lambda x => x * 2 (only one argument)
        let lambda = LambdaDef {
            name: None,
            args: vec![crate::values::LambdaArg::Required("x".to_string())],
            body: crate::ast::Spanned::dummy(crate::ast::Expr::BinaryOp {
                op: crate::ast::BinaryOp::Multiply,
                left: Box::new(crate::ast::Spanned::dummy(crate::ast::Expr::Identifier(
                    "x".to_string(),
                ))),
                right: Box::new(crate::ast::Spanned::dummy(crate::ast::Expr::Number(2.0))),
            }),
            scope: CapturedScope::default(),
            source: Rc::from(""),
        };
        let lambda_value = heap.borrow_mut().insert_lambda(lambda);

        // Call map
        let result = BuiltInFunction::Map
            .call(
                vec![list, lambda_value],
                heap.clone(),
                bindings.clone(),
                0,
                "",
            )
            .unwrap();

        // Verify result is [20, 40, 60] - backward compatible, no index passed
        let heap_borrow = heap.borrow();
        let result_list = result.as_list(&heap_borrow).unwrap();
        assert_eq!(result_list.len(), 3);
        assert_eq!(result_list[0], Value::Number(20.0));
        assert_eq!(result_list[1], Value::Number(40.0));
        assert_eq!(result_list[2], Value::Number(60.0));
    }

    #[test]
    fn test_convert_length() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        let convert_fn = BuiltInFunction::Convert;

        // Test km to m
        let from_unit = heap.borrow_mut().insert_string("km".to_string());
        let to_unit = heap.borrow_mut().insert_string("m".to_string());
        let args = vec![Value::Number(1.0), from_unit, to_unit];
        let result = convert_fn
            .call(args, heap.clone(), bindings.clone(), 0, "")
            .unwrap();
        assert_eq!(result, Value::Number(1000.0));

        // Test miles to km
        let from_unit = heap.borrow_mut().insert_string("miles".to_string());
        let to_unit = heap.borrow_mut().insert_string("km".to_string());
        let args = vec![Value::Number(1.0), from_unit, to_unit];
        let result = convert_fn
            .call(args, heap.clone(), bindings.clone(), 0, "")
            .unwrap();
        if let Value::Number(n) = result {
            assert!((n - 1.609344).abs() < 1e-6);
        } else {
            panic!("Expected number");
        }
    }

    #[test]
    fn test_convert_temperature() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        let convert_fn = BuiltInFunction::Convert;

        // Test 0°C to °F (should be 32°F)
        let from_unit = heap.borrow_mut().insert_string("celsius".to_string());
        let to_unit = heap.borrow_mut().insert_string("fahrenheit".to_string());
        let args = vec![Value::Number(0.0), from_unit, to_unit];
        let result = convert_fn
            .call(args, heap.clone(), bindings.clone(), 0, "")
            .unwrap();
        if let Value::Number(n) = result {
            assert!((n - 32.0).abs() < 1e-10);
        }

        // Test 100°C to °F (should be 212°F)
        let from_unit = heap.borrow_mut().insert_string("celsius".to_string());
        let to_unit = heap.borrow_mut().insert_string("fahrenheit".to_string());
        let args = vec![Value::Number(100.0), from_unit, to_unit];
        let result = convert_fn
            .call(args, heap.clone(), bindings.clone(), 0, "")
            .unwrap();
        if let Value::Number(n) = result {
            assert!((n - 212.0).abs() < 1e-10);
        }
    }

    #[test]
    fn test_convert_mass() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        let convert_fn = BuiltInFunction::Convert;

        // Test 1 kg to lbs
        let from_unit = heap.borrow_mut().insert_string("kg".to_string());
        let to_unit = heap.borrow_mut().insert_string("lbs".to_string());
        let args = vec![Value::Number(1.0), from_unit, to_unit];
        let result = convert_fn
            .call(args, heap.clone(), bindings.clone(), 0, "")
            .unwrap();
        if let Value::Number(n) = result {
            assert!((n - 2.20462).abs() < 0.001);
        }
    }

    #[test]
    fn test_convert_information_storage() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        let convert_fn = BuiltInFunction::Convert;

        // Test 1 kibibyte to bytes
        let from_unit = heap.borrow_mut().insert_string("kibibytes".to_string());
        let to_unit = heap.borrow_mut().insert_string("bytes".to_string());
        let args = vec![Value::Number(1.0), from_unit, to_unit];
        let result = convert_fn
            .call(args, heap.clone(), bindings.clone(), 0, "")
            .unwrap();
        assert_eq!(result, Value::Number(1024.0));
    }

    #[test]
    fn test_convert_same_unit() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        let convert_fn = BuiltInFunction::Convert;

        // Test converting to same unit
        let from_unit = heap.borrow_mut().insert_string("meters".to_string());
        let to_unit = heap.borrow_mut().insert_string("m".to_string());
        let args = vec![Value::Number(42.0), from_unit, to_unit];
        let result = convert_fn
            .call(args, heap.clone(), bindings.clone(), 0, "")
            .unwrap();
        assert_eq!(result, Value::Number(42.0));
    }

    #[test]
    fn test_convert_incompatible_units() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        let convert_fn = BuiltInFunction::Convert;

        // Test incompatible units
        let from_unit = heap.borrow_mut().insert_string("kg".to_string());
        let to_unit = heap.borrow_mut().insert_string("meters".to_string());
        let args = vec![Value::Number(1.0), from_unit, to_unit];
        let result = convert_fn.call(args, heap.clone(), bindings.clone(), 0, "");
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Cannot convert"));
    }

    #[test]
    fn test_convert_unknown_unit() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        let convert_fn = BuiltInFunction::Convert;

        // Test unknown unit
        let from_unit = heap.borrow_mut().insert_string("foobar".to_string());
        let to_unit = heap.borrow_mut().insert_string("meters".to_string());
        let args = vec![Value::Number(1.0), from_unit, to_unit];
        let result = convert_fn.call(args, heap.clone(), bindings.clone(), 0, "");
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Unknown unit"));
    }

    #[test]
    fn test_convert_case_insensitive() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        let convert_fn = BuiltInFunction::Convert;

        // Test case insensitivity
        let from_unit = heap.borrow_mut().insert_string("KM".to_string());
        let to_unit = heap.borrow_mut().insert_string("M".to_string());
        let args = vec![Value::Number(1.0), from_unit, to_unit];
        let result = convert_fn
            .call(args, heap.clone(), bindings.clone(), 0, "")
            .unwrap();
        assert_eq!(result, Value::Number(1000.0));
    }

    #[test]
    fn test_flatten() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        // Create [[1, 2], [3, 4]]
        let inner1 = heap
            .borrow_mut()
            .insert_list(vec![Value::Number(1.0), Value::Number(2.0)]);
        let inner2 = heap
            .borrow_mut()
            .insert_list(vec![Value::Number(3.0), Value::Number(4.0)]);
        let list = heap.borrow_mut().insert_list(vec![inner1, inner2]);

        let result = BuiltInFunction::Flatten
            .call(vec![list], heap.clone(), bindings.clone(), 0, "")
            .unwrap();

        let heap_borrow = heap.borrow();
        let result_list = result.as_list(&heap_borrow).unwrap();
        assert_eq!(result_list.len(), 4);
        assert_eq!(result_list[0], Value::Number(1.0));
        assert_eq!(result_list[1], Value::Number(2.0));
        assert_eq!(result_list[2], Value::Number(3.0));
        assert_eq!(result_list[3], Value::Number(4.0));
    }

    #[test]
    fn test_flatten_with_non_list_elements() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        // Create [[1, 2], 3, [4, 5]]
        let inner1 = heap
            .borrow_mut()
            .insert_list(vec![Value::Number(1.0), Value::Number(2.0)]);
        let inner2 = heap
            .borrow_mut()
            .insert_list(vec![Value::Number(4.0), Value::Number(5.0)]);
        let list = heap
            .borrow_mut()
            .insert_list(vec![inner1, Value::Number(3.0), inner2]);

        let result = BuiltInFunction::Flatten
            .call(vec![list], heap.clone(), bindings.clone(), 0, "")
            .unwrap();

        let heap_borrow = heap.borrow();
        let result_list = result.as_list(&heap_borrow).unwrap();
        assert_eq!(result_list.len(), 5);
        assert_eq!(result_list[0], Value::Number(1.0));
        assert_eq!(result_list[1], Value::Number(2.0));
        assert_eq!(result_list[2], Value::Number(3.0));
        assert_eq!(result_list[3], Value::Number(4.0));
        assert_eq!(result_list[4], Value::Number(5.0));
    }

    #[test]
    fn test_flatten_one_level_only() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        // Create [[1, [2, 3]], [4]]
        let deep_inner = heap
            .borrow_mut()
            .insert_list(vec![Value::Number(2.0), Value::Number(3.0)]);
        let inner1 = heap
            .borrow_mut()
            .insert_list(vec![Value::Number(1.0), deep_inner]);
        let inner2 = heap.borrow_mut().insert_list(vec![Value::Number(4.0)]);
        let list = heap.borrow_mut().insert_list(vec![inner1, inner2]);

        let result = BuiltInFunction::Flatten
            .call(vec![list], heap.clone(), bindings.clone(), 0, "")
            .unwrap();

        let heap_borrow = heap.borrow();
        let result_list = result.as_list(&heap_borrow).unwrap();
        // Should be [1, [2, 3], 4] - the nested list stays nested
        assert_eq!(result_list.len(), 3);
        assert_eq!(result_list[0], Value::Number(1.0));
        // result_list[1] should still be a list
        assert!(matches!(result_list[1], Value::List(_)));
        assert_eq!(result_list[2], Value::Number(4.0));
    }

    #[test]
    fn test_zip_equal_lengths() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        let list1 = heap
            .borrow_mut()
            .insert_list(vec![Value::Number(1.0), Value::Number(2.0)]);
        let str_a = heap.borrow_mut().insert_string("a".to_string());
        let str_b = heap.borrow_mut().insert_string("b".to_string());
        let list2 = heap.borrow_mut().insert_list(vec![str_a, str_b]);

        let result = BuiltInFunction::Zip
            .call(vec![list1, list2], heap.clone(), bindings.clone(), 0, "")
            .unwrap();

        let heap_borrow = heap.borrow();
        let result_list = result.as_list(&heap_borrow).unwrap();
        assert_eq!(result_list.len(), 2);

        let pair1 = result_list[0].as_list(&heap_borrow).unwrap();
        assert_eq!(pair1.len(), 2);
        assert_eq!(pair1[0], Value::Number(1.0));

        let pair2 = result_list[1].as_list(&heap_borrow).unwrap();
        assert_eq!(pair2.len(), 2);
        assert_eq!(pair2[0], Value::Number(2.0));
    }

    #[test]
    fn test_zip_unequal_lengths_pads_with_null() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        let list1 = heap.borrow_mut().insert_list(vec![Value::Number(1.0)]);
        let str_a = heap.borrow_mut().insert_string("a".to_string());
        let str_b = heap.borrow_mut().insert_string("b".to_string());
        let str_c = heap.borrow_mut().insert_string("c".to_string());
        let list2 = heap.borrow_mut().insert_list(vec![str_a, str_b, str_c]);

        let result = BuiltInFunction::Zip
            .call(vec![list1, list2], heap.clone(), bindings.clone(), 0, "")
            .unwrap();

        let heap_borrow = heap.borrow();
        let result_list = result.as_list(&heap_borrow).unwrap();
        assert_eq!(result_list.len(), 3); // Pads to longest

        let pair1 = result_list[0].as_list(&heap_borrow).unwrap();
        assert_eq!(pair1[0], Value::Number(1.0));

        let pair2 = result_list[1].as_list(&heap_borrow).unwrap();
        assert_eq!(pair2[0], Value::Null); // Padded with null

        let pair3 = result_list[2].as_list(&heap_borrow).unwrap();
        assert_eq!(pair3[0], Value::Null); // Padded with null
    }

    #[test]
    fn test_zip_three_lists() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        let list1 = heap
            .borrow_mut()
            .insert_list(vec![Value::Number(1.0), Value::Number(2.0)]);
        let str_a = heap.borrow_mut().insert_string("a".to_string());
        let str_b = heap.borrow_mut().insert_string("b".to_string());
        let list2 = heap.borrow_mut().insert_list(vec![str_a, str_b]);
        let list3 = heap
            .borrow_mut()
            .insert_list(vec![Value::Bool(true), Value::Bool(false)]);

        let result = BuiltInFunction::Zip
            .call(
                vec![list1, list2, list3],
                heap.clone(),
                bindings.clone(),
                0,
                "",
            )
            .unwrap();

        let heap_borrow = heap.borrow();
        let result_list = result.as_list(&heap_borrow).unwrap();
        assert_eq!(result_list.len(), 2);

        let tuple1 = result_list[0].as_list(&heap_borrow).unwrap();
        assert_eq!(tuple1.len(), 3);
        assert_eq!(tuple1[0], Value::Number(1.0));
        assert_eq!(tuple1[2], Value::Bool(true));
    }

    #[test]
    fn test_chunk() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        let list = heap.borrow_mut().insert_list(vec![
            Value::Number(1.0),
            Value::Number(2.0),
            Value::Number(3.0),
            Value::Number(4.0),
            Value::Number(5.0),
        ]);

        let result = BuiltInFunction::Chunk
            .call(
                vec![list, Value::Number(2.0)],
                heap.clone(),
                bindings.clone(),
                0,
                "",
            )
            .unwrap();

        let heap_borrow = heap.borrow();
        let result_list = result.as_list(&heap_borrow).unwrap();
        assert_eq!(result_list.len(), 3);

        let chunk1 = result_list[0].as_list(&heap_borrow).unwrap();
        assert_eq!(chunk1.len(), 2);
        assert_eq!(chunk1[0], Value::Number(1.0));
        assert_eq!(chunk1[1], Value::Number(2.0));

        let chunk2 = result_list[1].as_list(&heap_borrow).unwrap();
        assert_eq!(chunk2.len(), 2);
        assert_eq!(chunk2[0], Value::Number(3.0));
        assert_eq!(chunk2[1], Value::Number(4.0));

        let chunk3 = result_list[2].as_list(&heap_borrow).unwrap();
        assert_eq!(chunk3.len(), 1); // Last chunk smaller
        assert_eq!(chunk3[0], Value::Number(5.0));
    }

    #[test]
    fn test_chunk_zero_size_error() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        let list = heap
            .borrow_mut()
            .insert_list(vec![Value::Number(1.0), Value::Number(2.0)]);

        let result = BuiltInFunction::Chunk.call(
            vec![list, Value::Number(0.0)],
            heap.clone(),
            bindings.clone(),
            0,
            "",
        );

        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("chunk size must be greater than 0")
        );
    }

    #[test]
    fn test_chunk_empty_list() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        let list = heap.borrow_mut().insert_list(vec![]);

        let result = BuiltInFunction::Chunk
            .call(
                vec![list, Value::Number(2.0)],
                heap.clone(),
                bindings.clone(),
                0,
                "",
            )
            .unwrap();

        let heap_borrow = heap.borrow();
        let result_list = result.as_list(&heap_borrow).unwrap();
        assert_eq!(result_list.len(), 0);
    }

    #[test]
    fn test_group_by() {
        use crate::values::{CapturedScope, LambdaDef};

        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        // Use strings like "a", "b", "a" as input with identity lambda
        let str_a1 = heap.borrow_mut().insert_string("a".to_string());
        let str_b = heap.borrow_mut().insert_string("b".to_string());
        let str_a2 = heap.borrow_mut().insert_string("a".to_string());
        let simple_list = heap.borrow_mut().insert_list(vec![str_a1, str_b, str_a2]);

        // Identity lambda: x => x
        let lambda = LambdaDef {
            name: None,
            args: vec![crate::values::LambdaArg::Required("x".to_string())],
            body: crate::ast::Spanned::dummy(crate::ast::Expr::Identifier("x".to_string())),
            scope: CapturedScope::default(),
            source: Rc::from(""),
        };
        let lambda_value = heap.borrow_mut().insert_lambda(lambda);

        let result = BuiltInFunction::GroupBy
            .call(
                vec![simple_list, lambda_value],
                heap.clone(),
                bindings.clone(),
                0,
                "",
            )
            .unwrap();

        let heap_borrow = heap.borrow();
        let record = result.as_record(&heap_borrow).unwrap();
        assert_eq!(record.len(), 2); // "a" and "b"
        assert!(record.contains_key("a"));
        assert!(record.contains_key("b"));

        let group_a = record.get("a").unwrap().as_list(&heap_borrow).unwrap();
        assert_eq!(group_a.len(), 2); // "a" appears twice

        let group_b = record.get("b").unwrap().as_list(&heap_borrow).unwrap();
        assert_eq!(group_b.len(), 1); // "b" appears once
    }

    #[test]
    fn test_group_by_non_string_key_error() {
        use crate::values::{CapturedScope, LambdaDef};

        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        let list = heap
            .borrow_mut()
            .insert_list(vec![Value::Number(1.0), Value::Number(2.0)]);

        // Lambda that returns a number: x => x
        let lambda = LambdaDef {
            name: None,
            args: vec![crate::values::LambdaArg::Required("x".to_string())],
            body: crate::ast::Spanned::dummy(crate::ast::Expr::Identifier("x".to_string())),
            scope: CapturedScope::default(),
            source: Rc::from(""),
        };
        let lambda_value = heap.borrow_mut().insert_lambda(lambda);

        let result = BuiltInFunction::GroupBy.call(
            vec![list, lambda_value],
            heap.clone(),
            bindings.clone(),
            0,
            "",
        );

        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("must return a string")
        );
    }

    #[test]
    fn test_count_by() {
        use crate::values::{CapturedScope, LambdaDef};

        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        // Use strings as input with identity lambda
        let str_a1 = heap.borrow_mut().insert_string("a".to_string());
        let str_b = heap.borrow_mut().insert_string("b".to_string());
        let str_a2 = heap.borrow_mut().insert_string("a".to_string());
        let str_a3 = heap.borrow_mut().insert_string("a".to_string());
        let list = heap
            .borrow_mut()
            .insert_list(vec![str_a1, str_b, str_a2, str_a3]);

        // Identity lambda: x => x
        let lambda = LambdaDef {
            name: None,
            args: vec![crate::values::LambdaArg::Required("x".to_string())],
            body: crate::ast::Spanned::dummy(crate::ast::Expr::Identifier("x".to_string())),
            scope: CapturedScope::default(),
            source: Rc::from(""),
        };
        let lambda_value = heap.borrow_mut().insert_lambda(lambda);

        let result = BuiltInFunction::CountBy
            .call(
                vec![list, lambda_value],
                heap.clone(),
                bindings.clone(),
                0,
                "",
            )
            .unwrap();

        let heap_borrow = heap.borrow();
        let record = result.as_record(&heap_borrow).unwrap();
        assert_eq!(record.len(), 2); // "a" and "b"

        assert_eq!(*record.get("a").unwrap(), Value::Number(3.0)); // "a" appears 3 times
        assert_eq!(*record.get("b").unwrap(), Value::Number(1.0)); // "b" appears 1 time
    }

    #[test]
    fn test_count_by_empty_list() {
        use crate::values::{CapturedScope, LambdaDef};

        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        let list = heap.borrow_mut().insert_list(vec![]);

        // Identity lambda: x => x
        let lambda = LambdaDef {
            name: None,
            args: vec![crate::values::LambdaArg::Required("x".to_string())],
            body: crate::ast::Spanned::dummy(crate::ast::Expr::Identifier("x".to_string())),
            scope: CapturedScope::default(),
            source: Rc::from(""),
        };
        let lambda_value = heap.borrow_mut().insert_lambda(lambda);

        let result = BuiltInFunction::CountBy
            .call(
                vec![list, lambda_value],
                heap.clone(),
                bindings.clone(),
                0,
                "",
            )
            .unwrap();

        let heap_borrow = heap.borrow();
        let record = result.as_record(&heap_borrow).unwrap();
        assert_eq!(record.len(), 0); // Empty record
    }
}
