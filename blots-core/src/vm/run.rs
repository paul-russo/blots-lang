//! The instruction loop of the VM.
//!
//! The loop keeps the state of the innermost frame (the frame record, the instruction pointer
//! and the base of its registers) in local variables and works on raw pointers throughout.
//! Everything that is not worth doing inline is delegated to the methods of [`Vm`], which see
//! the frame stack through `Vm::top` and each frame's saved `ip`: the loop writes both back
//! right before it calls out (see `cold!`), and picks up whatever may have moved afterwards.

use super::{
    Builder, CALLBACK_CALL_DEPTH, DEPTH_MASK, ENTRY, Frame, MAX_CALL_DEPTH, PROFILED, TAIL_SHIFT,
    UNBOUND, Vm, copy_value, depth_exceeded, is_unbound, store_value,
};
use crate::{
    ast::BinaryOp,
    bytecode::{Instr, Op, Proto},
    error::RuntimeError,
    heap::HeapValue,
    values::{LambdaDef, Value},
};
use std::{hint::unreachable_unchecked, rc::Rc};

/// `x < y`, or `None` when the two cannot be ordered because one of them is NaN.
#[inline(always)]
fn less(x: f64, y: f64) -> Option<bool> {
    if x < y {
        Some(true)
    } else if x >= y {
        Some(false)
    } else {
        None
    }
}

/// `x <= y`, or `None` when the two cannot be ordered because one of them is NaN.
#[inline(always)]
fn less_eq(x: f64, y: f64) -> Option<bool> {
    if x <= y {
        Some(true)
    } else if x > y {
        Some(false)
    } else {
        None
    }
}

/// The number in a register that is known to hold one.
///
/// # Safety
/// `slot` must point to a register holding a `Value::Number`.
#[inline(always)]
unsafe fn number_in(slot: *const Value) -> f64 {
    // SAFETY: guaranteed by the caller.
    match unsafe { &*slot } {
        Value::Number(number) => *number,
        _ => unsafe { unreachable_unchecked() },
    }
}

/// The counter or address kept in a loop state register. Loop state lives in registers the
/// program cannot see, stored as the bit pattern of a number so that the registers stay
/// ordinary values.
///
/// # Safety
/// `slot` must point to a register written by [`set_state`].
#[inline(always)]
unsafe fn state(slot: *const Value) -> u64 {
    // SAFETY: guaranteed by the caller.
    unsafe { number_in(slot) }.to_bits()
}

/// # Safety
/// `slot` must point to a register that is valid for writes.
#[inline(always)]
unsafe fn set_state(slot: *mut Value, bits: u64) {
    // SAFETY: guaranteed by the caller.
    unsafe { slot.write(Value::Number(f64::from_bits(bits))) }
}

/// Replace the content of a loop state register without rewriting its tag.
///
/// # Safety
/// `slot` must point to a register written by [`set_state`].
#[inline(always)]
unsafe fn update_state(slot: *mut Value, bits: u64) {
    // SAFETY: guaranteed by the caller.
    match unsafe { &mut *slot } {
        Value::Number(number) => *number = f64::from_bits(bits),
        _ => unsafe { unreachable_unchecked() },
    }
}

/// Whether any of the `count` values starting at `first` is a spread value.
///
/// # Safety
/// `first` must point to `count` consecutive initialised values.
#[inline(always)]
unsafe fn has_spread(first: *const Value, count: usize) -> bool {
    // SAFETY: guaranteed by the caller.
    (0..count).any(|offset| matches!(unsafe { &*first.add(offset) }, Value::Spread(_)))
}

/// Run the innermost frame of `vm`, and every frame it calls, until it returns or fails.
///
/// # Safety
/// The innermost frame must have been set up by `Vm::run_chunk` or `Vm::call_function`: its
/// `ip` points at the first instruction of code that is alive, its registers lie within the
/// value stack, and its parameters are bound.
pub(super) unsafe fn execute(vm: &mut Vm) -> Result<Value, RuntimeError> {
    let vm: *mut Vm = vm;

    // SAFETY (for everything below): the compiler only emits register indices below the
    // proto's `max_stack`, table indices that exist, and jumps that land on instructions, and
    // every path through a proto ends in a return or an error. The stack covers `max_stack`
    // registers from the base of every active frame (plus the slot below the base of a
    // function frame, which holds the value the function was called through). The frame
    // records, the stack and the heap are only ever touched through `vm` between two
    // instructions, never while a reference into them is held, and `frame` and `base` are
    // re-derived after everything that may have moved them.
    unsafe {
        let mut frame: *mut Frame = (*vm).top;
        let mut ip: *const u64 = (*frame).ip;
        let mut base: *mut Value = (*frame).base;

        macro_rules! reg {
            ($index:expr) => {{
                debug_assert!(($index as usize) < (*(*frame).proto).max_stack);
                base.add($index as usize)
            }};
        }

        // Publish the state of the loop, so that the rest of the VM can be called.
        macro_rules! cold {
            () => {{
                (*frame).ip = ip;
                (*vm).top = frame;
                &mut *vm
            }};
        }

        // Fail with an error raised by the loop itself.
        macro_rules! throw {
            ($error:expr) => {{
                let error = RuntimeError::from($error);
                return Err(cold!().unwind(error));
            }};
        }

        // Unwrap the result of a call made through `cold!`. The frame stack is still published
        // when such a call fails (and `frame` may be stale), so the error unwinds as it is.
        macro_rules! check {
            ($result:expr) => {
                match $result {
                    Ok(value) => value,
                    Err(error) => return Err((*vm).unwind(RuntimeError::from(error))),
                }
            };
        }

        // Pick the innermost frame up again after a call that may have run other frames or
        // moved the stacks, but that left this frame the innermost one.
        macro_rules! reload {
            () => {{
                frame = (*vm).top;
                base = (*frame).base;
            }};
        }

        // Continue with whatever frame is the innermost one now.
        macro_rules! resume {
            () => {{
                frame = (*vm).top;
                ip = (*frame).ip;
                base = (*frame).base;
            }};
        }

        'run: loop {
            let instr = Instr::decode(*ip);
            ip = ip.add(1);

            // Instructions finish with `continue 'run`; binary operators that their fast path
            // does not cover leave through `'slow` with the operator, the operands, and the
            // number of operand words to skip once the result is in.
            let (op, lhs, rhs, skip): (BinaryOp, Value, Value, usize) = 'slow: {
                macro_rules! store_number {
                    ($value:expr, $skip:expr) => {{
                        reg!(instr.a).write(Value::Number($value));
                        ip = ip.add($skip);
                        continue 'run;
                    }};
                }

                macro_rules! store_bool {
                    ($value:expr, $skip:expr) => {{
                        store_value(reg!(instr.a), Value::Bool($value));
                        ip = ip.add($skip);
                        continue 'run;
                    }};
                }

                // Stores the outcome of an ordering comparison, unless it has none (see `less`).
                macro_rules! store_ordered {
                    ($value:expr, $skip:expr) => {{
                        if let Some(holds) = $value {
                            store_value(reg!(instr.a), Value::Bool(holds));
                            ip = ip.add($skip);
                            continue 'run;
                        }
                    }};
                }

                // A binary operator with a fast path for numbers. Everything else leaves through
                // `$slow` with the operator, the operands, and the number of operand words to skip
                // once the result is in.
                macro_rules! numeric {
                    // `R[a] = R[b] <op> R[c]`
                    (rr, $op:expr, |$x:ident, $y:ident| $fast:expr => $store:ident) => {{
                        let (lhs, rhs) = (reg!(instr.b), reg!(instr.c));
                        if let (Value::Number($x), Value::Number($y)) = (&*lhs, &*rhs) {
                            let ($x, $y) = (*$x, *$y);
                            $store!($fast, 0);
                        }

                        break 'slow ($op, *lhs, *rhs, 0);
                    }};
                    // `R[a] = R[b] <op> k`
                    (rk, $op:expr, |$x:ident, $y:ident| $fast:expr => $store:ident) => {{
                        let lhs = reg!(instr.b);
                        let $y = f64::from_bits(*ip);
                        if let Value::Number($x) = &*lhs {
                            let $x = *$x;
                            $store!($fast, 1);
                        }

                        break 'slow ($op, *lhs, Value::Number($y), 1);
                    }};
                    // `R[a] = k <op> R[b]`
                    (kr, $op:expr, |$x:ident, $y:ident| $fast:expr => $store:ident) => {{
                        let rhs = reg!(instr.b);
                        let $x = f64::from_bits(*ip);
                        if let Value::Number($y) = &*rhs {
                            let $y = *$y;
                            $store!($fast, 1);
                        }

                        break 'slow ($op, Value::Number($x), *rhs, 1);
                    }};
                }

                macro_rules! logical {
                    ($op:expr, |$x:ident, $y:ident| $fast:expr) => {{
                        let (lhs, rhs) = (reg!(instr.b), reg!(instr.c));
                        if let (Value::Bool($x), Value::Bool($y)) = (&*lhs, &*rhs) {
                            let ($x, $y) = (*$x, *$y);
                            store_value(reg!(instr.a), Value::Bool($fast));
                            continue 'run;
                        }

                        break 'slow ($op, *lhs, *rhs, 0);
                    }};
                }

                // A fused compare-and-branch. Numbers are compared inline; everything else (and NaN,
                // for its error) goes through the shared operator implementation.
                macro_rules! branch {
                    // `R[b] <op> R[c]`, followed by the jump to take when it does not hold.
                    (rr, $op:expr, |$x:ident, $y:ident| $fast:expr) => {{
                        let (lhs, rhs) = (reg!(instr.b), reg!(instr.c));
                        let fast: Option<bool> = match (&*lhs, &*rhs) {
                            (Value::Number($x), Value::Number($y)) => {
                                let ($x, $y) = (*$x, *$y);
                                $fast
                            }
                            _ => None,
                        };
                        let holds = match fast {
                            Some(holds) => holds,
                            None => {
                                let holds = check!(cold!().compare_slow($op, *lhs, *rhs));
                                reload!();
                                holds
                            }
                        };

                        if holds {
                            ip = ip.add(1);
                        } else {
                            ip = ip.add(1).offset(Instr::decode(*ip).sbx());
                        }
                    }};
                    // `R[a] <op> k`, where `k` is the following code word and the jump offset is
                    // relative to it.
                    (rk, $op:expr, |$x:ident, $y:ident| $fast:expr) => {{
                        let lhs = reg!(instr.a);
                        let $y = f64::from_bits(*ip);
                        let fast: Option<bool> = match &*lhs {
                            Value::Number($x) => {
                                let $x = *$x;
                                $fast
                            }
                            _ => None,
                        };
                        let holds = match fast {
                            Some(holds) => holds,
                            None => {
                                let rhs = Value::Number($y);
                                let holds = check!(cold!().compare_slow($op, *lhs, rhs));
                                reload!();
                                holds
                            }
                        };

                        if holds {
                            ip = ip.add(1);
                        } else {
                            ip = ip.offset(instr.sbx());
                        }
                    }};
                }

                // A comparison against `null`. Only lists need the shared implementation, which
                // broadcasts the comparison over their elements.
                macro_rules! null_test {
                    ($slot:expr, $op:expr, $when_null:expr) => {{
                        let operand = $slot;
                        match &*operand {
                            Value::List(_) => {
                                let (lhs, rhs) = match instr.x {
                                    1 => (Value::Null, *operand),
                                    _ => (*operand, Value::Null),
                                };

                                Err(($op, lhs, rhs))
                            }
                            Value::Null => Ok($when_null),
                            _ => Ok(!$when_null),
                        }
                    }};
                }

                macro_rules! null_branch {
                    ($op:expr, $when_null:expr) => {{
                        let holds = match null_test!(reg!(instr.a), $op, $when_null) {
                            Ok(holds) => holds,
                            Err((op, lhs, rhs)) => {
                                let holds = check!(cold!().compare_slow(op, lhs, rhs));
                                reload!();
                                holds
                            }
                        };

                        if !holds {
                            ip = ip.offset(instr.sbx());
                        }
                    }};
                }

                match instr.op {
                    Op::Move => copy_value(reg!(instr.a), reg!(instr.b)),
                    Op::LoadK => {
                        let consts = &(*(*frame).proto).consts;
                        debug_assert!(instr.bx() < consts.len());
                        copy_value(reg!(instr.a), consts.as_ptr().add(instr.bx()));
                    }
                    Op::LoadNum => {
                        reg!(instr.a).write(Value::Number(f64::from_bits(*ip)));
                        ip = ip.add(1);
                    }
                    Op::LoadStr => {
                        let value = cold!().load_string(instr.bx());
                        store_value(reg!(instr.a), value);
                    }
                    Op::LoadUnbound => store_value(reg!(instr.a), UNBOUND),

                    Op::GetName => {
                        let value = check!(cold!().get_name(instr.bx(), instr.x == 1));
                        store_value(reg!(instr.a), value);
                    }
                    Op::EnsureGlobal => {
                        if is_unbound(&*reg!(instr.a)) {
                            let value = check!(cold!().get_global(instr.bx()));
                            store_value(reg!(instr.a), value);
                        }
                    }
                    Op::GetCapture => {
                        debug_assert!(!(*frame).def.is_null());

                        match (*(*frame).def).scope.slot_ref(instr.b as usize) {
                            Some(value) => copy_value(reg!(instr.a), value),
                            None => {
                                let reference = instr.c as usize;
                                let value =
                                    check!(cold!().capture_fallback(reference, instr.x == 1));
                                store_value(reg!(instr.a), value);
                            }
                        }
                    }
                    Op::GetCaptureRef => {
                        debug_assert!(!(*frame).def.is_null());
                        let (slot, _) = (&(*(*frame).proto).slot_refs)[instr.bx()];

                        match (*(*frame).def).scope.slot_ref(slot as usize) {
                            Some(value) => copy_value(reg!(instr.a), value),
                            None => {
                                let value =
                                    check!(cold!().capture_fallback(instr.bx(), instr.x == 1));
                                store_value(reg!(instr.a), value);
                            }
                        }
                    }
                    Op::GetEnvParam => {
                        let value = check!(cold!().env_slot(instr.bx(), true));
                        store_value(reg!(instr.a), value);
                    }
                    Op::GetEnvCapture => {
                        let value = check!(cold!().env_slot(instr.bx(), false));
                        store_value(reg!(instr.a), value);
                    }
                    Op::GetInput => {
                        let value = check!(cold!().input(instr.bx()));
                        store_value(reg!(instr.a), value);
                    }

                    Op::Add => {
                        numeric!(rr, BinaryOp::Add, |x, y| x + y => store_number)
                    }
                    Op::Sub => {
                        numeric!(rr, BinaryOp::Subtract, |x, y| x - y => store_number)
                    }
                    Op::Mul => {
                        numeric!(rr, BinaryOp::Multiply, |x, y| x * y => store_number)
                    }
                    Op::Div => {
                        numeric!(rr, BinaryOp::Divide, |x, y| x / y => store_number)
                    }
                    Op::Mod => {
                        numeric!(rr, BinaryOp::Modulo, |x, y| x % y => store_number)
                    }
                    Op::Pow => {
                        numeric!(rr, BinaryOp::Power, |x, y| x.powf(y) => store_number)
                    }
                    Op::Eq => {
                        numeric!(rr, BinaryOp::Equal, |x, y| x == y => store_bool)
                    }
                    Op::Ne => {
                        numeric!(rr, BinaryOp::NotEqual, |x, y| x != y => store_bool)
                    }
                    Op::Lt => {
                        numeric!(rr, BinaryOp::Less, |x, y| less(x, y) => store_ordered)
                    }
                    Op::Le => {
                        numeric!(rr, BinaryOp::LessEq, |x, y| less_eq(x, y) => store_ordered)
                    }
                    Op::Gt => {
                        numeric!(rr, BinaryOp::Greater, |x, y| less(y, x) => store_ordered)
                    }
                    Op::Ge => {
                        numeric!(rr, BinaryOp::GreaterEq, |x, y| less_eq(y, x) => store_ordered)
                    }
                    Op::DotEq => {
                        numeric!(rr, BinaryOp::DotEqual, |x, y| x == y => store_bool)
                    }
                    Op::DotNe => {
                        numeric!(rr, BinaryOp::DotNotEqual, |x, y| x != y => store_bool)
                    }
                    Op::DotLt => {
                        numeric!(rr, BinaryOp::DotLess, |x, y| less(x, y) => store_ordered)
                    }
                    Op::DotLe => {
                        numeric!(rr, BinaryOp::DotLessEq, |x, y| less_eq(x, y) => store_ordered)
                    }
                    Op::DotGt => {
                        numeric!(rr, BinaryOp::DotGreater, |x, y| less(y, x) => store_ordered)
                    }
                    Op::DotGe => {
                        numeric!(rr, BinaryOp::DotGreaterEq, |x, y| less_eq(y, x) => store_ordered)
                    }
                    Op::And => logical!(BinaryOp::And, |x, y| x && y),
                    Op::NaturalAnd => logical!(BinaryOp::NaturalAnd, |x, y| x && y),
                    Op::Or => logical!(BinaryOp::Or, |x, y| x || y),
                    Op::NaturalOr => logical!(BinaryOp::NaturalOr, |x, y| x || y),
                    Op::Via => break 'slow (BinaryOp::Via, *reg!(instr.b), *reg!(instr.c), 0),
                    Op::Into => break 'slow (BinaryOp::Into, *reg!(instr.b), *reg!(instr.c), 0),
                    Op::Where => break 'slow (BinaryOp::Where, *reg!(instr.b), *reg!(instr.c), 0),
                    Op::Coalesce => {
                        let (lhs, rhs) = (reg!(instr.b), reg!(instr.c));

                        match (&*lhs, &*rhs) {
                            (Value::List(_), _) | (_, Value::List(_)) => {
                                break 'slow (BinaryOp::Coalesce, *lhs, *rhs, 0);
                            }
                            (Value::Null, _) => copy_value(reg!(instr.a), rhs),
                            _ => copy_value(reg!(instr.a), lhs),
                        }
                    }

                    Op::AddK => {
                        numeric!(rk, BinaryOp::Add, |x, y| x + y => store_number)
                    }
                    Op::SubK => {
                        numeric!(rk, BinaryOp::Subtract, |x, y| x - y => store_number)
                    }
                    Op::MulK => {
                        numeric!(rk, BinaryOp::Multiply, |x, y| x * y => store_number)
                    }
                    Op::DivK => {
                        numeric!(rk, BinaryOp::Divide, |x, y| x / y => store_number)
                    }
                    Op::ModK => {
                        numeric!(rk, BinaryOp::Modulo, |x, y| x % y => store_number)
                    }
                    Op::PowK => {
                        numeric!(rk, BinaryOp::Power, |x, y| x.powf(y) => store_number)
                    }
                    Op::EqK => {
                        numeric!(rk, BinaryOp::Equal, |x, y| x == y => store_bool)
                    }
                    Op::NeK => {
                        numeric!(rk, BinaryOp::NotEqual, |x, y| x != y => store_bool)
                    }
                    Op::LtK => {
                        numeric!(rk, BinaryOp::Less, |x, y| less(x, y) => store_ordered)
                    }
                    Op::LeK => {
                        numeric!(rk, BinaryOp::LessEq, |x, y| less_eq(x, y) => store_ordered)
                    }
                    Op::GtK => {
                        numeric!(rk, BinaryOp::Greater, |x, y| less(y, x) => store_ordered)
                    }
                    Op::GeK => {
                        numeric!(rk, BinaryOp::GreaterEq, |x, y| less_eq(y, x) => store_ordered)
                    }

                    Op::KAdd => {
                        numeric!(kr, BinaryOp::Add, |x, y| x + y => store_number)
                    }
                    Op::KSub => {
                        numeric!(kr, BinaryOp::Subtract, |x, y| x - y => store_number)
                    }
                    Op::KMul => {
                        numeric!(kr, BinaryOp::Multiply, |x, y| x * y => store_number)
                    }
                    Op::KDiv => {
                        numeric!(kr, BinaryOp::Divide, |x, y| x / y => store_number)
                    }
                    Op::KMod => {
                        numeric!(kr, BinaryOp::Modulo, |x, y| x % y => store_number)
                    }
                    Op::KPow => {
                        numeric!(kr, BinaryOp::Power, |x, y| x.powf(y) => store_number)
                    }

                    Op::JmpNotEq => branch!(rr, BinaryOp::Equal, |x, y| Some(x == y)),
                    Op::JmpNotNe => branch!(rr, BinaryOp::NotEqual, |x, y| Some(x != y)),
                    Op::JmpNotLt => branch!(rr, BinaryOp::Less, |x, y| less(x, y)),
                    Op::JmpNotLe => branch!(rr, BinaryOp::LessEq, |x, y| less_eq(x, y)),
                    Op::JmpNotGt => branch!(rr, BinaryOp::Greater, |x, y| less(y, x)),
                    Op::JmpNotGe => branch!(rr, BinaryOp::GreaterEq, |x, y| less_eq(y, x)),
                    Op::JmpNotEqK => branch!(rk, BinaryOp::Equal, |x, y| Some(x == y)),
                    Op::JmpNotNeK => branch!(rk, BinaryOp::NotEqual, |x, y| Some(x != y)),
                    Op::JmpNotLtK => branch!(rk, BinaryOp::Less, |x, y| less(x, y)),
                    Op::JmpNotLeK => branch!(rk, BinaryOp::LessEq, |x, y| less_eq(x, y)),
                    Op::JmpNotGtK => branch!(rk, BinaryOp::Greater, |x, y| less(y, x)),
                    Op::JmpNotGeK => branch!(rk, BinaryOp::GreaterEq, |x, y| less_eq(y, x)),

                    Op::EqNull => match null_test!(reg!(instr.b), BinaryOp::Equal, true) {
                        Ok(holds) => store_value(reg!(instr.a), Value::Bool(holds)),
                        Err((op, lhs, rhs)) => break 'slow (op, lhs, rhs, 0),
                    },
                    Op::NeNull => match null_test!(reg!(instr.b), BinaryOp::NotEqual, false) {
                        Ok(holds) => store_value(reg!(instr.a), Value::Bool(holds)),
                        Err((op, lhs, rhs)) => break 'slow (op, lhs, rhs, 0),
                    },
                    Op::JmpNotEqNull => null_branch!(BinaryOp::Equal, true),
                    Op::JmpNotNeNull => null_branch!(BinaryOp::NotEqual, false),

                    Op::Neg => match &*reg!(instr.b) {
                        Value::Number(number) => reg!(instr.a).write(Value::Number(-*number)),
                        other => throw!(other.as_number().unwrap_err()),
                    },
                    Op::Not => match &*reg!(instr.b) {
                        Value::Bool(boolean) => {
                            store_value(reg!(instr.a), Value::Bool(!*boolean));
                        }
                        other => throw!(other.as_bool().unwrap_err()),
                    },
                    Op::Factorial => {
                        let value = check!(cold!().factorial(*reg!(instr.b)));
                        store_value(reg!(instr.a), value);
                    }

                    Op::Jmp => ip = ip.offset(instr.sbx()),
                    Op::JmpIfFalse => match &*reg!(instr.a) {
                        Value::Bool(true) => {}
                        Value::Bool(false) => ip = ip.offset(instr.sbx()),
                        other => throw!(other.as_bool().unwrap_err()),
                    },

                    Op::Call => {
                        let a = instr.a as usize;
                        let argc = instr.b as usize;

                        // A call to a user function whose code is already known and needs no
                        // argument adjustment only has to push a frame record.
                        'general: {
                            let Value::Lambda(pointer) = &*reg!(a) else {
                                break 'general;
                            };
                            let Some(HeapValue::Lambda(def)) =
                                (*vm).heap_ref().get(pointer.index())
                            else {
                                break 'general;
                            };
                            let def: *const LambdaDef = Rc::as_ptr(def);
                            let Some(code) = (*def).scope.code.get() else {
                                break 'general;
                            };
                            let proto: *const Proto = Rc::as_ptr(code);
                            let callee_base = base.add(a + 1);
                            let meta = (*frame).meta;

                            if (*proto).fast_arity as usize != argc
                                || !(*proto).belongs_to(&(*def).body, &(*def).args)
                                || callee_base > (*vm).stack_limit
                                || frame == (*vm).frame_limit
                                || (meta & DEPTH_MASK) as usize > MAX_CALL_DEPTH
                                || (instr.x == 0 && has_spread(callee_base, argc))
                            {
                                break 'general;
                            }

                            (*frame).ip = ip;
                            frame = frame.add(1);
                            (*frame).base = callee_base;
                            (*frame).proto = proto;
                            (*frame).def = def;
                            (*frame).meta = (meta & DEPTH_MASK) + 1;
                            ip = (*proto).code.as_ptr();
                            base = callee_base;
                            continue 'run;
                        }

                        check!(cold!().call_slow(a, argc, instr.c));
                        resume!();
                    }
                    Op::CallList => {
                        let a = instr.a as usize;
                        let argc = cold!().place_list_args(a);
                        check!((*vm).call_slow(a, argc, instr.c));
                        resume!();
                    }
                    Op::CallSelf => {
                        let a = instr.a as usize;
                        let argc = (instr.x & 0x7f) as usize;
                        let callee_base = base.add(a + 1);
                        let meta = (*frame).meta;

                        // The function calls itself through its own name: the value the frame
                        // was called through is the callee.
                        copy_value(callee_base.sub(1), base.sub(1));

                        // The low half of `meta` is the depth, plus a high bit when the
                        // function was redefined and has to be looked up again.
                        if meta as u32 > MAX_CALL_DEPTH as u32
                            || callee_base > (*vm).stack_limit
                            || frame == (*vm).frame_limit
                            || (instr.x & 0x80 == 0 && has_spread(callee_base, argc))
                        {
                            check!(cold!().call_slow(a, argc, instr.c));
                            resume!();
                            continue 'run;
                        }

                        let (proto, def) = ((*frame).proto, (*frame).def);
                        (*frame).ip = ip;
                        frame = frame.add(1);
                        (*frame).base = callee_base;
                        (*frame).proto = proto;
                        (*frame).def = def;
                        (*frame).meta = (meta & DEPTH_MASK) + 1;
                        ip = ip.sub(instr.b as usize);
                        base = callee_base;
                    }
                    Op::TailSelf => {
                        let a = instr.a as usize;
                        let argc = (instr.x & 0x7f) as usize;
                        let args = base.add(a + 1);
                        let meta = (*frame).meta;

                        if meta as u32 > MAX_CALL_DEPTH as u32
                            || (instr.x & 0x80 == 0 && has_spread(args, argc))
                        {
                            // An ordinary call, whose result the following instruction returns.
                            copy_value(args.sub(1), base.sub(1));
                            check!(cold!().call_slow(a, argc, instr.c));
                            resume!();
                            continue 'run;
                        }

                        // The frame starts over with the new arguments, one level deeper, and
                        // remembers where it did so for error reports.
                        for offset in 0..argc {
                            copy_value(base.add(offset), args.add(offset));
                        }
                        (*frame).meta = meta + 1 + (1 << TAIL_SHIFT);
                        (*frame).tail_ip = ip;
                        ip = ip.sub(instr.b as usize);
                    }
                    Op::Return => {
                        let value = reg!(instr.a);

                        if (*frame).meta & (ENTRY | PROFILED) != 0 {
                            let value = *value;
                            if cold!().finish_frame() {
                                return Ok(value);
                            }

                            resume!();
                            let target = Instr::decode(*ip.sub(1)).c;
                            store_value(reg!(target), value);
                            continue 'run;
                        }

                        // The result goes where the call instruction of the caller wants it.
                        frame = frame.sub(1);
                        ip = (*frame).ip;
                        let caller = (*frame).base;
                        let target = Instr::decode(*ip.sub(1)).c;
                        copy_value(caller.add(target as usize), value);
                        base = caller;
                    }

                    Op::Closure => {
                        let value = check!(cold!().make_closure(instr.bx()));
                        store_value(reg!(instr.a), value);
                    }

                    Op::NewList => {
                        let first = base.add(instr.b as usize);
                        let value = cold!().new_list(first, instr.c as usize);
                        store_value(reg!(instr.a), value);
                    }
                    Op::ListBegin => {
                        let source = base.add(instr.a as usize + 1);
                        cold!().list_begin(instr.x, source);
                    }
                    Op::ListAppend => {
                        let first = base.add(instr.a as usize);
                        check!(cold!().list_append(first, instr.b as usize));
                    }
                    Op::ListPush => {
                        let Some(Builder::List(list)) = (*vm).builders.last_mut() else {
                            unreachable!("no list is being built");
                        };

                        if list.len() == list.capacity() {
                            list.reserve(1);
                        }

                        let len = list.len();
                        copy_value(list.as_mut_ptr().add(len), reg!(instr.a));
                        list.set_len(len + 1);
                    }
                    Op::ListEnd => {
                        let value = cold!().list_end();
                        store_value(reg!(instr.a), value);
                    }

                    Op::NewRecord => {
                        let first = base.add(instr.b as usize);
                        let value = cold!().new_record(instr.c as usize, first);
                        store_value(reg!(instr.a), value);
                    }
                    Op::RecordBegin => cold!().record_begin(),
                    Op::RecordSet => cold!().record_set(instr.bx(), *reg!(instr.a)),
                    Op::RecordKey => check!(cold!().record_key(*reg!(instr.a))),
                    Op::RecordValue => cold!().record_value(*reg!(instr.a)),
                    Op::RecordSpread => check!(cold!().record_spread(*reg!(instr.a))),
                    Op::RecordEnd => {
                        let value = cold!().record_end();
                        store_value(reg!(instr.a), value);
                    }

                    Op::Index => {
                        let (object, index) = (reg!(instr.b), reg!(instr.c));

                        if let (Value::List(pointer), Value::Number(number)) = (&*object, &*index)
                            && let Some(HeapValue::List(items)) =
                                (*vm).heap_ref().get(pointer.index())
                        {
                            // A negative index counts from the end; one that is still negative
                            // after that wraps around to a position no list has.
                            let position = match *number as i64 {
                                index if index >= 0 => index as usize,
                                index => (items.len() as i64).wrapping_add(index) as usize,
                            };

                            match items.get(position) {
                                Some(item) => copy_value(reg!(instr.a), item),
                                None => store_value(reg!(instr.a), Value::Null),
                            }
                            continue 'run;
                        }

                        let value = check!(cold!().index(*object, *index));
                        store_value(reg!(instr.a), value);
                    }
                    Op::GetField => {
                        let site = instr.c as usize | (instr.x as usize) << 16;
                        let object = reg!(instr.b);

                        // The field usually sits where this access site found it last time.
                        if let Value::Record(pointer) = &*object
                            && let Some(HeapValue::Record(record)) =
                                (*vm).heap_ref().get(pointer.index())
                        {
                            let proto = &*(*frame).proto;
                            let cached = proto.field_caches[site].get() as usize;

                            if let Some((key, value)) = record.get_index(cached)
                                && *key == proto.fields[site]
                            {
                                copy_value(reg!(instr.a), value);
                                continue 'run;
                            }
                        }

                        let value = check!(cold!().field(*object, site));
                        store_value(reg!(instr.a), value);
                    }
                    Op::Spread => {
                        let value = check!(cold!().spread(*reg!(instr.b)));
                        store_value(reg!(instr.a), value);
                    }

                    Op::Raise => {
                        let error = cold!().raise(instr.bx());
                        return Err((*vm).unwind(error));
                    }
                    Op::CheckUnbound => check!(cold!().check_unbound(instr.bx())),
                    Op::NameLambda => {
                        if let Value::Lambda(pointer) = *reg!(instr.a) {
                            cold!().name_lambda(pointer, instr.bx(), instr.x == 1);
                        }
                    }
                    Op::SetGlobal => cold!().set_global(instr.bx(), *reg!(instr.a)),

                    Op::AddDepth => {
                        (*frame).meta = (*frame).meta.wrapping_add(instr.sbx() as u64);
                    }

                    Op::RangePrep => {
                        let first = base.add(instr.b as usize);
                        let (first, count) = check!(cold!().range_bounds(first, instr.c as usize));
                        let state = reg!(instr.a);

                        set_state(state, 0);
                        set_state(state.add(1), count);
                        state.add(2).write(Value::Number(first));
                    }
                    Op::ForRange => {
                        let slot = reg!(instr.a);
                        let next = state(slot);

                        if next >= state(slot.add(1)) {
                            ip = ip.offset(instr.sbx());
                            continue 'run;
                        }

                        let index = next as i64 as f64;
                        slot.add(3)
                            .write(Value::Number(number_in(slot.add(2)) + index));
                        if instr.x == 1 {
                            slot.add(4).write(Value::Number(index));
                        }
                        update_state(slot, next + 1);
                    }
                    Op::ForRangeNext => {
                        let slot = reg!(instr.a);
                        let next = state(slot);

                        if next < state(slot.add(1)) {
                            let index = next as i64 as f64;
                            slot.add(3)
                                .write(Value::Number(number_in(slot.add(2)) + index));
                            if instr.x == 1 {
                                slot.add(4).write(Value::Number(index));
                            }
                            update_state(slot, next + 1);
                            ip = ip.offset(instr.sbx());
                        }
                    }
                    Op::IterPrep => {
                        let slot = reg!(instr.a);
                        let (first, end) = check!(cold!().iter_bounds(*slot.add(1)));

                        set_state(slot, first as u64);
                        set_state(slot.add(4), end as u64);
                    }
                    Op::ForList => {
                        let slot = reg!(instr.a);
                        let cursor = state(slot) as *const Value;

                        if cursor as u64 == state(slot.add(4)) {
                            ip = ip.offset(instr.sbx());
                            continue 'run;
                        }

                        copy_value(slot.add(2), cursor);
                        if instr.x == 1 {
                            slot.add(3).write(Value::Number(0.0));
                        }
                        update_state(slot, cursor.add(1) as u64);
                    }
                    Op::ForListNext => {
                        let slot = reg!(instr.a);
                        let cursor = state(slot) as *const Value;

                        if cursor as u64 != state(slot.add(4)) {
                            copy_value(slot.add(2), cursor);
                            if instr.x == 1 {
                                let index = number_in(slot.add(3));
                                slot.add(3).write(Value::Number(index + 1.0));
                            }
                            update_state(slot, cursor.add(1) as u64);
                            ip = ip.offset(instr.sbx());
                        }
                    }
                    Op::CallbackDepth => {
                        let depth = ((*frame).meta & DEPTH_MASK) as usize;
                        if depth + CALLBACK_CALL_DEPTH > MAX_CALL_DEPTH {
                            throw!(depth_exceeded("anonymous function"));
                        }
                    }
                }

                continue 'run;
            };

            // What the fast paths above do not cover: broadcasting, strings, the callback
            // operators, and every error case.
            let value = check!(cold!().binary_slow(op, lhs, rhs));
            reload!();
            store_value(reg!(instr.a), value);
            ip = ip.add(skip);
        }
    }
}
