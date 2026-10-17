//! The instruction loop of the VM.
//!
//! The loop keeps the state of the innermost frame (the frame record, the instruction pointer
//! and the base of its registers) in local variables and works on raw pointers throughout. It
//! only implements the common case of each instruction, and only where that case takes no
//! call into the rest of the VM: whatever is left is handed to [`Vm::step`], which executes
//! any instruction in full. That hand-over is the only place where the loop publishes its
//! state (the frame stack is seen through `Vm::top` and each frame's saved `ip`) and picks it
//! up again, so everywhere else the state lives in machine registers.

use super::{
    Builder, CALLBACK_CALL_DEPTH, DEPTH_MASK, ENTRY, Frame, MAX_CALL_DEPTH, PROFILED, TAIL_SHIFT,
    UNBOUND, Vm, copy_value, has_spread, is_unbound, less, less_eq, list_next, range_next,
    step::Flow, store_value,
};
use crate::{
    bytecode::{Instr, Op, Proto},
    error::RuntimeError,
    expressions::modulo,
    heap::{Heap, HeapValue},
    values::{LambdaDef, Value},
};
use std::rc::Rc;

/// The jump offset of the instruction in the code word before `ip`.
///
/// The offset is read from the code again, on its own, rather than taken out of the decoded
/// instruction. That keeps the path from one instruction pointer to the next short (a single
/// load), and since the compiler has to leave the load where it is, a conditional jump stays
/// a branch the CPU can predict instead of becoming a conditional move, which would make
/// fetching the next instruction wait for the operands of the comparison.
///
/// # Safety
/// `ip` must point right after an instruction word.
#[inline(always)]
unsafe fn jump_offset(ip: *const u64) -> isize {
    // SAFETY: guaranteed by the caller. `b` and `c`, which hold the offset, are the upper
    // half of the instruction word.
    #[cfg(target_endian = "little")]
    let offset = unsafe { ip.sub(1).cast::<i32>().add(1).read_volatile() as isize };
    #[cfg(not(target_endian = "little"))]
    let offset = unsafe { Instr::decode(ip.sub(1).read_volatile()).sbx() };

    offset
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
    // function frame, which holds the value the function was called through). Nothing in the
    // loop itself allocates on the heap or moves the stacks; `Vm::step` may do both, and
    // `frame`, `ip` and `base` are re-derived after it.
    unsafe {
        let heap: *const Heap = (*vm).heap.as_ptr();
        let mut frame: *mut Frame = (*vm).top;
        let mut ip: *const u64 = (*frame).ip;
        let mut base: *mut Value = (*frame).base;

        'run: loop {
            let instr = Instr::decode(*ip);
            ip = ip.add(1);

            macro_rules! reg {
                ($index:expr) => {{
                    debug_assert!(($index as usize) < (*(*frame).proto).max_stack);
                    base.add($index as usize)
                }};
            }

            // `R[a] = R[b] <op> R[c]` on two numbers.
            macro_rules! arithmetic {
                (|$x:ident, $y:ident| $value:expr) => {{
                    if let (Value::Number($x), Value::Number($y)) =
                        (&*reg!(instr.b), &*reg!(instr.c))
                    {
                        let ($x, $y) = (*$x, *$y);
                        reg!(instr.a).write(Value::Number($value));
                        continue 'run;
                    }
                }};
            }

            // `R[a] = R[b] <op> k` on a number, where `k` is the following code word.
            macro_rules! arithmetic_k {
                (|$x:ident, $y:ident| $value:expr) => {{
                    if let Value::Number($x) = &*reg!(instr.b) {
                        let ($x, $y) = (*$x, f64::from_bits(*ip));
                        reg!(instr.a).write(Value::Number($value));
                        ip = ip.add(1);
                        continue 'run;
                    }
                }};
            }

            // `R[a] = k <op> R[b]` on a number, where `k` is the following code word.
            macro_rules! k_arithmetic {
                (|$x:ident, $y:ident| $value:expr) => {{
                    if let Value::Number($y) = &*reg!(instr.b) {
                        let ($x, $y) = (f64::from_bits(*ip), *$y);
                        reg!(instr.a).write(Value::Number($value));
                        ip = ip.add(1);
                        continue 'run;
                    }
                }};
            }

            // `R[a] = R[b] <op> R[c]` for a comparison of two numbers that can be ordered.
            macro_rules! comparison {
                (|$x:ident, $y:ident| $holds:expr) => {{
                    if let (Value::Number($x), Value::Number($y)) =
                        (&*reg!(instr.b), &*reg!(instr.c))
                        && let Some(holds) = {
                            let ($x, $y) = (*$x, *$y);
                            $holds
                        }
                    {
                        store_value(reg!(instr.a), Value::Bool(holds));
                        continue 'run;
                    }
                }};
            }

            // `R[a] = R[b] <op> k` for a comparison of two numbers that can be ordered.
            macro_rules! comparison_k {
                (|$x:ident, $y:ident| $holds:expr) => {{
                    if let Value::Number($x) = &*reg!(instr.b)
                        && let Some(holds) = {
                            let ($x, $y) = (*$x, f64::from_bits(*ip));
                            $holds
                        }
                    {
                        store_value(reg!(instr.a), Value::Bool(holds));
                        ip = ip.add(1);
                        continue 'run;
                    }
                }};
            }

            macro_rules! logical {
                (|$x:ident, $y:ident| $value:expr) => {{
                    if let (Value::Bool($x), Value::Bool($y)) = (&*reg!(instr.b), &*reg!(instr.c)) {
                        let ($x, $y) = (*$x, *$y);
                        store_value(reg!(instr.a), Value::Bool($value));
                        continue 'run;
                    }
                }};
            }

            // A fused compare-and-branch of two numbers that can be ordered: `R[b] <op> R[c]`
            // is followed by the jump to take when it does not hold.
            macro_rules! branch {
                (|$x:ident, $y:ident| $holds:expr) => {{
                    if let (Value::Number($x), Value::Number($y)) =
                        (&*reg!(instr.b), &*reg!(instr.c))
                        && let Some(holds) = {
                            let ($x, $y) = (*$x, *$y);
                            $holds
                        }
                    {
                        ip = ip.add(1);
                        if !holds {
                            ip = ip.offset(jump_offset(ip));
                        }
                        continue 'run;
                    }
                }};
            }

            // The same for `R[a] <op> k`, where `k` is the following code word and the jump
            // offset is relative to it.
            macro_rules! branch_k {
                (|$x:ident, $y:ident| $holds:expr) => {{
                    if let Value::Number($x) = &*reg!(instr.a)
                        && let Some(holds) = {
                            let ($x, $y) = (*$x, f64::from_bits(*ip));
                            $holds
                        }
                    {
                        if holds {
                            ip = ip.add(1);
                        } else {
                            ip = ip.offset(jump_offset(ip));
                        }
                        continue 'run;
                    }
                }};
            }

            // Every arm either finishes its instruction and continues with the next one, or
            // leaves the instruction untouched and falls out of the `match`.
            match instr.op {
                // Reserved for testing `Vm::step`: with this configuration every instruction
                // goes through it.
                _ if cfg!(blots_vm_step_only) => {}

                Op::Move => {
                    copy_value(reg!(instr.a), reg!(instr.b));
                    continue 'run;
                }
                Op::LoadK => {
                    let consts = &(*(*frame).proto).consts;
                    debug_assert!(instr.bx() < consts.len());
                    copy_value(reg!(instr.a), consts.as_ptr().add(instr.bx()));
                    continue 'run;
                }
                Op::LoadNum => {
                    reg!(instr.a).write(Value::Number(f64::from_bits(*ip)));
                    ip = ip.add(1);
                    continue 'run;
                }
                Op::LoadUnbound => {
                    store_value(reg!(instr.a), UNBOUND);
                    continue 'run;
                }
                Op::EnsureGlobal => {
                    if !is_unbound(&*reg!(instr.a)) {
                        continue 'run;
                    }
                }
                Op::GetCapture => {
                    debug_assert!(!(*frame).def.is_null());

                    if let Some(value) = (*(*frame).def).scope.slot_ref(instr.b as usize) {
                        copy_value(reg!(instr.a), value);
                        continue 'run;
                    }
                }
                Op::GetCaptureRef => {
                    debug_assert!(!(*frame).def.is_null());
                    let (slot, _) = (&(*(*frame).proto).slot_refs)[instr.bx()];

                    if let Some(value) = (*(*frame).def).scope.slot_ref(slot as usize) {
                        copy_value(reg!(instr.a), value);
                        continue 'run;
                    }
                }

                Op::Add => arithmetic!(|x, y| x + y),
                Op::Sub => arithmetic!(|x, y| x - y),
                Op::Mul => arithmetic!(|x, y| x * y),
                Op::Div => arithmetic!(|x, y| x / y),
                Op::Mod => arithmetic!(|x, y| modulo(x, y)),
                Op::Pow => arithmetic!(|x, y| x.powf(y)),
                Op::Eq | Op::DotEq => comparison!(|x, y| Some(x == y)),
                Op::Ne | Op::DotNe => comparison!(|x, y| Some(x != y)),
                Op::Lt | Op::DotLt => comparison!(|x, y| less(x, y)),
                Op::Le | Op::DotLe => comparison!(|x, y| less_eq(x, y)),
                Op::Gt | Op::DotGt => comparison!(|x, y| less(y, x)),
                Op::Ge | Op::DotGe => comparison!(|x, y| less_eq(y, x)),
                Op::And | Op::NaturalAnd => logical!(|x, y| x && y),
                Op::Or | Op::NaturalOr => logical!(|x, y| x || y),
                Op::Coalesce => {
                    let (lhs, rhs) = (reg!(instr.b), reg!(instr.c));

                    match (&*lhs, &*rhs) {
                        (Value::List(_), _) | (_, Value::List(_)) => {}
                        (Value::Null, _) => {
                            copy_value(reg!(instr.a), rhs);
                            continue 'run;
                        }
                        _ => {
                            copy_value(reg!(instr.a), lhs);
                            continue 'run;
                        }
                    }
                }

                Op::AddK => arithmetic_k!(|x, y| x + y),
                Op::SubK => arithmetic_k!(|x, y| x - y),
                Op::MulK => arithmetic_k!(|x, y| x * y),
                Op::DivK => arithmetic_k!(|x, y| x / y),
                Op::ModK => arithmetic_k!(|x, y| modulo(x, y)),
                Op::PowK => arithmetic_k!(|x, y| x.powf(y)),
                Op::EqK => comparison_k!(|x, y| Some(x == y)),
                Op::NeK => comparison_k!(|x, y| Some(x != y)),
                Op::LtK => comparison_k!(|x, y| less(x, y)),
                Op::LeK => comparison_k!(|x, y| less_eq(x, y)),
                Op::GtK => comparison_k!(|x, y| less(y, x)),
                Op::GeK => comparison_k!(|x, y| less_eq(y, x)),

                Op::KAdd => k_arithmetic!(|x, y| x + y),
                Op::KSub => k_arithmetic!(|x, y| x - y),
                Op::KMul => k_arithmetic!(|x, y| x * y),
                Op::KDiv => k_arithmetic!(|x, y| x / y),
                Op::KMod => k_arithmetic!(|x, y| modulo(x, y)),
                Op::KPow => k_arithmetic!(|x, y| x.powf(y)),

                Op::JmpNotEq => branch!(|x, y| Some(x == y)),
                Op::JmpNotNe => branch!(|x, y| Some(x != y)),
                Op::JmpNotLt => branch!(|x, y| less(x, y)),
                Op::JmpNotLe => branch!(|x, y| less_eq(x, y)),
                Op::JmpNotGt => branch!(|x, y| less(y, x)),
                Op::JmpNotGe => branch!(|x, y| less_eq(y, x)),
                Op::JmpNotEqK => branch_k!(|x, y| Some(x == y)),
                Op::JmpNotNeK => branch_k!(|x, y| Some(x != y)),
                Op::JmpNotLtK => branch_k!(|x, y| less(x, y)),
                Op::JmpNotLeK => branch_k!(|x, y| less_eq(x, y)),
                Op::JmpNotGtK => branch_k!(|x, y| less(y, x)),
                Op::JmpNotGeK => branch_k!(|x, y| less_eq(y, x)),

                // Comparing a list against `null` broadcasts over its elements.
                Op::EqNull | Op::NeNull => {
                    let operand = &*reg!(instr.b);

                    if !matches!(operand, Value::List(_)) {
                        let is_null = matches!(operand, Value::Null);
                        let holds = is_null == (instr.op == Op::EqNull);
                        store_value(reg!(instr.a), Value::Bool(holds));
                        continue 'run;
                    }
                }
                Op::JmpNotEqNull | Op::JmpNotNeNull => {
                    let operand = &*reg!(instr.a);

                    if !matches!(operand, Value::List(_)) {
                        let is_null = matches!(operand, Value::Null);
                        if is_null != (instr.op == Op::JmpNotEqNull) {
                            ip = ip.offset(jump_offset(ip));
                        }
                        continue 'run;
                    }
                }

                Op::Neg => {
                    if let Value::Number(number) = &*reg!(instr.b) {
                        reg!(instr.a).write(Value::Number(-*number));
                        continue 'run;
                    }
                }
                Op::Not => {
                    if let Value::Bool(boolean) = &*reg!(instr.b) {
                        store_value(reg!(instr.a), Value::Bool(!*boolean));
                        continue 'run;
                    }
                }

                Op::Jmp => {
                    ip = ip.offset(jump_offset(ip));
                    continue 'run;
                }
                Op::JmpIfFalse => {
                    if let Value::Bool(holds) = &*reg!(instr.a) {
                        if !*holds {
                            ip = ip.offset(jump_offset(ip));
                        }
                        continue 'run;
                    }
                }

                // A call to a user function whose code is already known and needs no argument
                // adjustment only has to push a frame record.
                Op::Call => {
                    let a = instr.a as usize;
                    let argc = instr.b as usize;

                    if let Value::Lambda(pointer) = &*reg!(a)
                        && let Some(HeapValue::Lambda(def)) = (*heap).get(pointer.index())
                        && let Some(code) = def.scope.code.get()
                    {
                        let def: *const LambdaDef = Rc::as_ptr(def);
                        let proto: *const Proto = Rc::as_ptr(code);
                        let callee_base = base.add(a + 1);
                        let meta = (*frame).meta;

                        if (*proto).fast_arity as usize == argc
                            && (*proto).belongs_to(&(*def).body, &(*def).args)
                            && callee_base <= (*vm).stack_limit
                            && frame != (*vm).frame_limit
                            && (meta & DEPTH_MASK) as usize <= MAX_CALL_DEPTH
                            && (instr.x == 1 || !has_spread(callee_base, argc))
                        {
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
                    }
                }
                Op::CallSelf => {
                    let argc = (instr.x & 0x7f) as usize;
                    let callee_base = base.add(instr.a as usize + 1);
                    let meta = (*frame).meta;

                    // The low half of `meta` is the depth, plus a high bit when the function
                    // was redefined and has to be looked up again.
                    if meta as u32 <= MAX_CALL_DEPTH as u32
                        && callee_base <= (*vm).stack_limit
                        && frame != (*vm).frame_limit
                        && (instr.x & 0x80 != 0 || !has_spread(callee_base, argc))
                    {
                        // The function calls itself through its own name: the value the frame
                        // was called through is the callee.
                        copy_value(callee_base.sub(1), base.sub(1));

                        let (proto, def) = ((*frame).proto, (*frame).def);
                        (*frame).ip = ip;
                        frame = frame.add(1);
                        (*frame).base = callee_base;
                        (*frame).proto = proto;
                        (*frame).def = def;
                        (*frame).meta = (meta & DEPTH_MASK) + 1;
                        ip = ip.sub(instr.b as usize);
                        base = callee_base;
                        continue 'run;
                    }
                }
                Op::TailSelf => {
                    let argc = (instr.x & 0x7f) as usize;
                    let args = base.add(instr.a as usize + 1);
                    let meta = (*frame).meta;

                    if meta as u32 <= MAX_CALL_DEPTH as u32
                        && (instr.x & 0x80 != 0 || !has_spread(args, argc))
                    {
                        // The frame starts over with the new arguments, one level deeper, and
                        // remembers where it did so for error reports.
                        for offset in 0..argc {
                            copy_value(base.add(offset), args.add(offset));
                        }
                        (*frame).meta = meta + 1 + (1 << TAIL_SHIFT);
                        (*frame).tail_ip = ip;
                        ip = ip.sub(instr.b as usize);
                        continue 'run;
                    }
                }
                Op::Return => {
                    if (*frame).meta & (ENTRY | PROFILED) == 0 {
                        let value = reg!(instr.a);

                        // The result goes where the call instruction of the caller wants it.
                        frame = frame.sub(1);
                        ip = (*frame).ip;
                        let caller = (*frame).base;
                        let target = Instr::decode(*ip.sub(1)).c;
                        copy_value(caller.add(target as usize), value);
                        base = caller;
                        continue 'run;
                    }
                }

                Op::ListPush => {
                    if let Some(Builder::List(list)) = (*vm).builders.last_mut()
                        && list.len() < list.capacity()
                    {
                        let len = list.len();
                        copy_value(list.as_mut_ptr().add(len), reg!(instr.a));
                        list.set_len(len + 1);
                        continue 'run;
                    }
                }

                Op::Index => {
                    if let (Value::List(pointer), Value::Number(number)) =
                        (&*reg!(instr.b), &*reg!(instr.c))
                        && let Some(HeapValue::List(items)) = (*heap).get(pointer.index())
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
                }
                Op::GetField => {
                    // The field usually sits where this access site found it last time.
                    if let Value::Record(pointer) = &*reg!(instr.b)
                        && let Some(HeapValue::Record(record)) = (*heap).get(pointer.index())
                    {
                        let site = instr.c as usize | (instr.x as usize) << 16;
                        let proto = &*(*frame).proto;
                        let cached = proto.field_caches[site].get() as usize;

                        if let Some((key, value)) = record.get_index(cached)
                            && *key == proto.fields[site]
                        {
                            copy_value(reg!(instr.a), value);
                            continue 'run;
                        }
                    }
                }

                Op::AddDepth => {
                    (*frame).meta = (*frame).meta.wrapping_add(instr.sbx() as u64);
                    continue 'run;
                }
                Op::CallbackDepth => {
                    let depth = ((*frame).meta & DEPTH_MASK) as usize;
                    if depth + CALLBACK_CALL_DEPTH <= MAX_CALL_DEPTH {
                        continue 'run;
                    }
                }
                Op::ForRange => {
                    if !range_next(reg!(instr.a), instr.x == 1) {
                        ip = ip.offset(jump_offset(ip));
                    }
                    continue 'run;
                }
                Op::ForRangeNext => {
                    if range_next(reg!(instr.a), instr.x == 1) {
                        ip = ip.offset(jump_offset(ip));
                    }
                    continue 'run;
                }
                Op::ForList => {
                    if !list_next(reg!(instr.a), instr.x == 1, true) {
                        ip = ip.offset(jump_offset(ip));
                    }
                    continue 'run;
                }
                Op::ForListNext => {
                    if list_next(reg!(instr.a), instr.x == 1, false) {
                        ip = ip.offset(jump_offset(ip));
                    }
                    continue 'run;
                }

                _ => {}
            }

            // Whatever the fast paths above do not cover.
            (*frame).ip = ip;
            (*vm).top = frame;

            match (*vm).step(instr) {
                Ok(Flow::Continue) => {}
                Ok(Flow::Finished(value)) => return Ok(value),
                Err(error) => return Err(error),
            }

            frame = (*vm).top;
            ip = (*frame).ip;
            base = (*frame).base;
        }
    }
}
