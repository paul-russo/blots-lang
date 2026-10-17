//! Arithmetic on whole lists of numbers.
//!
//! An arithmetic operator applied to a list applies to each of its elements, and an expression
//! such as `(xs * 2 + 1) / 3` compiles to one instruction per operator, each consuming the list
//! the previous one produced. Executed one at a time, every instruction would build a complete
//! intermediate list. Instead, the VM looks ahead from the first of them: for as long as the
//! next instruction is the only reader of the previous result, it joins the chain, and the
//! chain as a whole is evaluated in a single pass over the elements, in blocks small enough to
//! stay in the cache. When the result of a chain goes straight into `sum` and friends, not
//! even the final list is built.
//!
//! All of this only covers lists of numbers. Everything else (strings to concatenate, values
//! an operator rejects, lists of different lengths) is left to the general implementation,
//! which does the same work one instruction at a time and reports errors the way it always
//! has. Arithmetic on numbers has no side effects, so giving up part of the way through a
//! chain is invisible.

use super::{MAX_CALL_DEPTH, Vm};
use crate::{
    bytecode::{Instr, LAST_USE_B, LAST_USE_C, Op},
    expressions::modulo,
    functions::{BuiltInFunction, product_of_nothing, sum_of_nothing},
    heap::{Heap, HeapValue},
    values::Value,
};

/// The most instructions evaluated as one chain.
const MAX_STEPS: usize = 8;

/// The number of elements that are taken through a chain together.
const BLOCK: usize = 128;

#[derive(Clone, Copy)]
enum Arithmetic {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
}

/// Where an arithmetic instruction finds its operands.
#[derive(Clone, Copy)]
enum Form {
    /// Both in registers.
    Registers,
    /// The left one in a register, the right one in the following code word.
    NumberRight,
    /// The right one in a register, the left one in the following code word.
    NumberLeft,
}

/// The operand of a chain step that is not the running result.
#[derive(Clone, Copy)]
enum Operand<'h> {
    Number(f64),
    /// A list as long as the one the chain started from.
    List(&'h [Value]),
}

/// One instruction of a chain: an operator applied to the running result and another operand.
#[derive(Clone, Copy)]
struct Step<'h> {
    op: Arithmetic,
    operand: Operand<'h>,

    /// Whether the running result is the right-hand operand.
    flipped: bool,
}

/// A built-in that reduces the numbers a chain produces to one number.
#[derive(Clone, Copy)]
enum Fold {
    Sum,
    Avg,
    Prod,
    Min,
    Max,
}

/// What a chain evaluates to.
enum Outcome {
    List(Vec<Value>),
    Number(f64),
}

fn arithmetic_of(op: Op) -> Option<(Arithmetic, Form)> {
    Some(match op {
        Op::Add => (Arithmetic::Add, Form::Registers),
        Op::Sub => (Arithmetic::Sub, Form::Registers),
        Op::Mul => (Arithmetic::Mul, Form::Registers),
        Op::Div => (Arithmetic::Div, Form::Registers),
        Op::Mod => (Arithmetic::Mod, Form::Registers),
        Op::Pow => (Arithmetic::Pow, Form::Registers),
        Op::AddK => (Arithmetic::Add, Form::NumberRight),
        Op::SubK => (Arithmetic::Sub, Form::NumberRight),
        Op::MulK => (Arithmetic::Mul, Form::NumberRight),
        Op::DivK => (Arithmetic::Div, Form::NumberRight),
        Op::ModK => (Arithmetic::Mod, Form::NumberRight),
        Op::PowK => (Arithmetic::Pow, Form::NumberRight),
        Op::KAdd => (Arithmetic::Add, Form::NumberLeft),
        Op::KSub => (Arithmetic::Sub, Form::NumberLeft),
        Op::KMul => (Arithmetic::Mul, Form::NumberLeft),
        Op::KDiv => (Arithmetic::Div, Form::NumberLeft),
        Op::KMod => (Arithmetic::Mod, Form::NumberLeft),
        Op::KPow => (Arithmetic::Pow, Form::NumberLeft),
        _ => return None,
    })
}

fn fold_of(built_in: BuiltInFunction) -> Option<Fold> {
    Some(match built_in {
        BuiltInFunction::Sum => Fold::Sum,
        BuiltInFunction::Avg => Fold::Avg,
        BuiltInFunction::Prod => Fold::Prod,
        BuiltInFunction::Min => Fold::Min,
        BuiltInFunction::Max => Fold::Max,
        _ => return None,
    })
}

/// The elements of `value` if it is a list.
fn elements(heap: &Heap, value: Value) -> Option<&[Value]> {
    let Value::List(pointer) = value else {
        return None;
    };

    match heap.get(pointer.index()) {
        Some(HeapValue::List(items)) => Some(items),
        _ => None,
    }
}

/// Copy the numbers in `values` to `numbers`, which is as long. Reports `false` when one of
/// the values is not a number.
#[inline]
fn load(values: &[Value], numbers: &mut [f64]) -> bool {
    for (value, slot) in values.iter().zip(numbers) {
        let Value::Number(number) = value else {
            return false;
        };
        *slot = *number;
    }

    true
}

/// Replace each of `values` by the result of applying `op` to it and the next of `others`.
#[inline(always)]
fn apply(op: Arithmetic, flipped: bool, values: &mut [f64], others: impl Iterator<Item = f64>) {
    macro_rules! each {
        (|$x:ident, $y:ident| $result:expr) => {
            if flipped {
                for (value, $x) in values.iter_mut().zip(others) {
                    let $y = *value;
                    *value = $result;
                }
            } else {
                for (value, $y) in values.iter_mut().zip(others) {
                    let $x = *value;
                    *value = $result;
                }
            }
        };
    }

    match op {
        Arithmetic::Add => each!(|x, y| x + y),
        Arithmetic::Sub => each!(|x, y| x - y),
        Arithmetic::Mul => each!(|x, y| x * y),
        Arithmetic::Div => each!(|x, y| x / y),
        Arithmetic::Mod => each!(|x, y| modulo(x, y)),
        Arithmetic::Pow => each!(|x, y| x.powf(y)),
    }
}

/// Take the elements of `source` through `steps`, a block at a time, handing each block of
/// results to `sink`. Reports `false` as soon as an element of any of the lists involved turns
/// out not to be a number; the sink may have seen some blocks by then.
fn evaluate(source: &[Value], steps: &[Step], mut sink: impl FnMut(&[f64])) -> bool {
    let mut results = [0.0; BLOCK];
    let mut operands = [0.0; BLOCK];

    for start in (0..source.len()).step_by(BLOCK) {
        let end = (start + BLOCK).min(source.len());
        let results = &mut results[..end - start];
        if !load(&source[start..end], results) {
            return false;
        }

        for step in steps {
            match step.operand {
                Operand::Number(number) => {
                    apply(step.op, step.flipped, results, std::iter::repeat(number));
                }
                Operand::List(items) => {
                    let operands = &mut operands[..end - start];
                    if !load(&items[start..end], operands) {
                        return false;
                    }

                    apply(step.op, step.flipped, results, operands.iter().copied());
                }
            }
        }

        sink(results);
    }

    true
}

/// The list of results of a chain, or `None` when the lists are not all numbers.
fn map(source: &[Value], steps: &[Step]) -> Option<Vec<Value>> {
    let mut items = Vec::with_capacity(source.len());
    let complete = evaluate(source, steps, |results| {
        items.extend(results.iter().map(|number| Value::Number(*number)));
    });

    complete.then_some(items)
}

/// What `fold` makes of the results of a chain over a list that is not empty, exactly as the
/// built-in computes it. `None` when the lists are not all numbers.
fn reduce(source: &[Value], steps: &[Step], fold: Fold) -> Option<f64> {
    let mut total = match fold {
        Fold::Sum | Fold::Avg => sum_of_nothing(),
        Fold::Prod => product_of_nothing(),
        Fold::Min => f64::INFINITY,
        Fold::Max => f64::NEG_INFINITY,
    };

    let complete = evaluate(source, steps, |results| {
        let results = results.iter().copied();
        total = match fold {
            Fold::Sum | Fold::Avg => results.fold(total, |sum, number| sum + number),
            Fold::Prod => results.fold(total, |product, number| product * number),
            Fold::Min => results.fold(total, f64::min),
            Fold::Max => results.fold(total, f64::max),
        };
    });
    if !complete {
        return None;
    }

    Some(match fold {
        Fold::Avg => total / source.len() as f64,
        _ => total,
    })
}

impl Vm {
    /// Execute the arithmetic instruction `instr`, one of whose operands is a list, together
    /// with the instructions after it that carry its result further. Reports `false`, having
    /// changed nothing, when that takes the general implementation.
    ///
    /// # Safety
    /// As for [`Vm::step`]: `instr` is the instruction right before the saved `ip` of the
    /// innermost frame.
    pub(super) unsafe fn broadcast(&mut self, instr: Instr) -> bool {
        // SAFETY: guaranteed by the caller.
        let Some((outcome, target, resume)) = (unsafe { self.evaluate_chain(instr) }) else {
            return false;
        };

        let value = match outcome {
            Outcome::List(items) => self.heap.borrow_mut().insert_list(items),
            Outcome::Number(number) => Value::Number(number),
        };

        // SAFETY: `target` is a register of the innermost frame, and `resume` points at the
        // instruction after the last one of the chain.
        unsafe {
            super::store_value(self.top().base.add(target as usize), value);
            (*self.top).ip = resume;
        }

        true
    }

    /// Evaluate the chain that starts with `instr`: what it comes to, the register that gets
    /// it, and where execution continues.
    ///
    /// # Safety
    /// As for [`Vm::broadcast`].
    unsafe fn evaluate_chain(&self, instr: Instr) -> Option<(Outcome, u16, *const u64)> {
        let (op, form) = arithmetic_of(instr.op)?;
        let frame = self.top();
        let register = |index: u16| -> Value {
            // SAFETY: the compiler only emits register indices that lie within the frame.
            unsafe { *frame.base.add(index as usize) }
        };

        // SAFETY (for every read through `cursor` below): arithmetic instructions are
        // followed by their number operand, if they take one, and then by at least one more
        // instruction, since every path through a proto ends in a return or an error.
        let mut cursor = frame.ip;
        let number_operand = |cursor: *const u64| Value::Number(f64::from_bits(unsafe { *cursor }));
        let (lhs, rhs) = match form {
            Form::Registers => (register(instr.b), register(instr.c)),
            Form::NumberRight => (register(instr.b), number_operand(cursor)),
            Form::NumberLeft => (number_operand(cursor), register(instr.b)),
        };
        if !matches!(form, Form::Registers) {
            cursor = unsafe { cursor.add(1) };
        }

        let heap = self.heap.borrow();
        let (source, operand, flipped) = match (lhs, rhs) {
            (Value::List(_), Value::Number(number)) => {
                (elements(&heap, lhs)?, Operand::Number(number), false)
            }
            (Value::Number(number), Value::List(_)) => {
                (elements(&heap, rhs)?, Operand::Number(number), true)
            }
            (Value::List(_), Value::List(_)) => {
                let (source, other) = (elements(&heap, lhs)?, elements(&heap, rhs)?);
                if source.len() != other.len() {
                    return None;
                }

                (source, Operand::List(other), false)
            }
            _ => return None,
        };

        let mut steps = [Step {
            op,
            operand,
            flipped,
        }; MAX_STEPS];
        let mut count = 1;
        let mut result = instr.a;

        // An instruction joins the chain when it is the last to read the result so far and
        // combines it with a number or with a list of the same length.
        while count < MAX_STEPS {
            let next = unsafe { Instr::decode(*cursor) };
            let Some((op, form)) = arithmetic_of(next.op) else {
                break;
            };

            let takes = |operand: u16, flag: u8| operand == result && next.x & flag != 0;
            let (other, flipped) = match form {
                Form::Registers if takes(next.b, LAST_USE_B) && next.c != result => {
                    (register(next.c), false)
                }
                Form::Registers if takes(next.c, LAST_USE_C) && next.b != result => {
                    (register(next.b), true)
                }
                Form::NumberRight if takes(next.b, LAST_USE_B) => {
                    (number_operand(unsafe { cursor.add(1) }), false)
                }
                Form::NumberLeft if takes(next.b, LAST_USE_B) => {
                    (number_operand(unsafe { cursor.add(1) }), true)
                }
                _ => break,
            };

            let operand = match other {
                Value::Number(number) => Operand::Number(number),
                Value::List(_) => match elements(&heap, other) {
                    Some(items) if items.len() == source.len() => Operand::List(items),
                    _ => break,
                },
                _ => break,
            };

            steps[count] = Step {
                op,
                operand,
                flipped,
            };
            count += 1;
            result = next.a;
            cursor = unsafe {
                cursor.add(if matches!(form, Form::Registers) {
                    1
                } else {
                    2
                })
            };
        }

        let steps = &steps[..count];

        // The call of an aggregate function with the result as its only argument can take the
        // numbers as they come, as long as the call is sure to succeed.
        if !source.is_empty()
            && let Some((fold, target)) = self.fold_after(unsafe { Instr::decode(*cursor) }, result)
        {
            let number = reduce(source, steps, fold)?;

            return Some((Outcome::Number(number), target, unsafe { cursor.add(1) }));
        }

        Some((Outcome::List(map(source, steps)?), result, cursor))
    }

    /// When `call` passes register `argument` (and nothing else) to a built-in that folds a
    /// list of numbers, and would get as far as running it: the fold and the register the
    /// call stores its result in.
    fn fold_after(&self, call: Instr, argument: u16) -> Option<(Fold, u16)> {
        if call.op != Op::Call || call.b != 1 || call.a as usize + 1 != argument as usize {
            return None;
        }

        // A profiled call is reported, and one that is nested too deep fails.
        let frame = self.top();
        if self.is_profiling() || frame.depth() > MAX_CALL_DEPTH {
            return None;
        }

        // SAFETY: the compiler only emits register indices that lie within the frame.
        let Value::BuiltIn(built_in) = (unsafe { *frame.base.add(call.a as usize) }) else {
            return None;
        };

        Some((fold_of(built_in)?, call.c))
    }
}
