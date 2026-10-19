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
    Builder, CALLBACK_CALL_DEPTH, DEPTH_MASK, ENTRY, Frame, MAX_CALL_DEPTH, NOT_TRUNCATED,
    PROFILED, TAIL_SHIFT, UNBOUND, Vm, copy_value, is_unbound, leaves_remainder, less, less_eq,
    list_next, load_value, range_next, register, same_strings, step::Flow, store_value,
    string_order, truncate, whole,
};
use crate::{
    bytecode::{Instr, Op, Proto, REGISTER_SCALE, RemainderTest, register_index},
    error::RuntimeError,
    expressions::{modulo, small_remainder},
    heap::{Heap, HeapValue},
    values::{LambdaDef, Value},
};
use std::rc::Rc;

/// Where the jump in the code word at `at` leads: its offset counts from the following word.
///
/// The offset is read with a volatile load, which the compiler has to leave where it is. That
/// keeps a conditional jump a branch the CPU can predict instead of turning it into a
/// conditional move, which would make fetching the next instruction wait for the operands of
/// the comparison.
///
/// # Safety
/// `at` must point at an instruction word whose jump offset leads to an instruction.
#[inline(always)]
unsafe fn jump_target(at: *const u64) -> *const u64 {
    // SAFETY: guaranteed by the caller. `b` and `c`, which hold the offset, are the upper
    // half of the instruction word.
    unsafe {
        #[cfg(target_endian = "little")]
        let offset = at.cast::<i32>().add(1).read_volatile() as isize;
        #[cfg(not(target_endian = "little"))]
        let offset = Instr::decode(at.read_volatile()).sbx();

        at.add(1).offset(offset)
    }
}

/// The parameter that an argument of a tail call of a function to itself is waiting to be
/// moved into: `arg` is the register the argument is in, which is where an ordinary call
/// whose scratch register is the one `window` names would take it from. An argument is as
/// far from its parameter as the first argument register of that call is from the first
/// register of the frame.
///
/// # Safety
/// `arg` must be an argument register of such a call in a frame of that function.
#[inline(always)]
unsafe fn waiting_param(arg: *mut Value, window: u16) -> *mut Value {
    // The arguments of the call start one register above its scratch register.
    let first_argument =
        window as usize * (size_of::<Value>() / REGISTER_SCALE) + size_of::<Value>();

    // SAFETY: the parameter lies that far below the argument, which is within the frame.
    unsafe { arg.byte_sub(first_argument) }
}

/// Marks the path it is called on as one that is rarely taken, which keeps the code for it
/// out of the way of the common path.
#[cold]
#[inline(always)]
fn rare() {}

/// Run the innermost frame of `vm`, and every frame it calls, until it returns or fails.
///
/// # Safety
/// The innermost frame must have been set up by `Vm::run_chunk` or `Vm::call_function`: its
/// `ip` points at the first instruction of code that is alive, its registers lie within the
/// value stack, and its parameters are bound.
pub(super) unsafe fn execute(vm: &mut Vm) -> Result<Value, RuntimeError> {
    let vm: *mut Vm = vm;

    // SAFETY: for everything below, the compiler only emits register indices below the
    // proto's `max_stack`, table indices that exist, and jumps that land on instructions, and
    // every path through a proto ends in a return or an error. The stack covers `max_stack`
    // registers from the base of every active frame (plus the slot below the base of a
    // function frame, which holds the value the function was called through). Nothing in the
    // loop itself allocates on the heap or moves the stacks; `Vm::step` may do both, and
    // `frame`, `ip` and `base` are re-derived after it.
    unsafe {
        let heap: *const Heap = (*vm).heap.as_ptr();

        // The slots of the heap, looked up again whenever `Vm::step` may have added to them
        // or a compaction pass may have moved what they hold.
        let mut slots: *const [HeapValue] = (*heap).slots();
        let mut frame: *mut Frame = (*vm).top;
        let mut ip: *const u64 = (*frame).ip;
        let mut base: *mut Value = (*frame).base;

        'run: loop {
            let instr = &*ip.cast::<Instr>();

            // The register an operand of the instruction names.
            macro_rules! reg {
                ($operand:expr) => {{
                    debug_assert!(register_index($operand) < (*(*frame).proto).max_stack);
                    register(base, $operand)
                }};
            }

            // What a pointer refers to in the heap, if it is in the heap at all.
            macro_rules! heap_value {
                ($pointer:expr) => {
                    (&*slots).get($pointer.index())
                };
            }

            // The instruction is done: carry on `$words` code words further on.
            macro_rules! next {
                ($words:expr) => {{
                    ip = ip.add($words);
                    continue 'run;
                }};
            }

            // The instruction is done: carry on where the jump in the code word at `$at` leads.
            macro_rules! jump {
                ($at:expr) => {{
                    ip = jump_target($at);
                    continue 'run;
                }};
            }

            // The frame starts over with the arguments that are now in its parameters: one
            // level deeper than `$meta` says it was, and remembering where it did so for
            // error reports.
            macro_rules! restart {
                ($meta:expr) => {{
                    (*frame).meta = $meta + 1 + (1 << TAIL_SHIFT);
                    (*frame).tail_ip = ip.add(1);
                    ip = ip.add(1).sub(instr.b as usize);
                    continue 'run;
                }};
            }

            // `R[a]` is what `$name` is bound to, when it is bound to something: by the frame,
            // or by one of the frames below it (see `Vm::find`, which looks at the frame stack
            // but leaves the stacks and the heap alone).
            macro_rules! find {
                ($name:expr) => {{
                    (*frame).ip = ip.add(1);
                    (*vm).top = frame;

                    if let Some(value) = (*vm).find($name) {
                        store_value(reg!(instr.a), value);
                        next!(1);
                    }
                }};
            }

            // `R[a]` is what slot reference `$reference` of the frame's proto is bound to,
            // whose capture slot the closure was made without: the function itself, when
            // that is the name of the function and nothing can shadow it (operand `x`), or
            // else what the name is bound to wherever it is.
            macro_rules! late_capture {
                ($reference:expr) => {{
                    let references = &(*(*frame).proto).slot_refs;
                    debug_assert!($reference < references.len());
                    let (_, name) = *references.get_unchecked($reference);

                    if instr.x == 0 && (*(*frame).def).name == Some(name) {
                        copy_value(reg!(instr.a), base.sub(1));
                        next!(1);
                    }

                    find!(name);
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
                        next!(1);
                    }
                }};
            }

            // `R[a] = R[b] <op> k` on a number, where `k` is the following code word.
            macro_rules! arithmetic_k {
                (|$x:ident, $y:ident| $value:expr) => {{
                    if let Value::Number($x) = &*reg!(instr.b) {
                        let ($x, $y) = (*$x, f64::from_bits(*ip.add(1)));
                        reg!(instr.a).write(Value::Number($value));
                        next!(2);
                    }
                }};
            }

            // `R[a] = k <op> R[b]` on a number, where `k` is the following code word.
            macro_rules! k_arithmetic {
                (|$x:ident, $y:ident| $value:expr) => {{
                    if let Value::Number($y) = &*reg!(instr.b) {
                        let ($x, $y) = (f64::from_bits(*ip.add(1)), *$y);
                        reg!(instr.a).write(Value::Number($value));
                        next!(2);
                    }
                }};
            }

            // `R[a] = R[b] <op> R[c]` for a comparison of two numbers that can be ordered, or
            // of two strings: `$strings` finds out about those what `$found` then stands for.
            macro_rules! comparison {
                (|$x:ident, $y:ident| $holds:expr, $strings:ident, |$found:ident| $string_holds:expr) => {{
                    let (lhs, rhs) = (reg!(instr.b), reg!(instr.c));

                    if let (Value::Number($x), Value::Number($y)) = (&*lhs, &*rhs)
                        && let Some(holds) = {
                            let ($x, $y) = (*$x, *$y);
                            $holds
                        }
                    {
                        store_value(reg!(instr.a), Value::Bool(holds));
                        next!(1);
                    }

                    if let (Value::String(first), Value::String(second)) = (&*lhs, &*rhs)
                        && let Some($found) = $strings(slots, *first, *second)
                    {
                        store_value(reg!(instr.a), Value::Bool($string_holds));
                        next!(1);
                    }
                }};
            }

            // `R[a] = R[b] <op> k` for a comparison of two numbers that can be ordered.
            macro_rules! comparison_k {
                (|$x:ident, $y:ident| $holds:expr) => {{
                    if let Value::Number($x) = &*reg!(instr.b)
                        && let Some(holds) = {
                            let ($x, $y) = (*$x, f64::from_bits(*ip.add(1)));
                            $holds
                        }
                    {
                        store_value(reg!(instr.a), Value::Bool(holds));
                        next!(2);
                    }
                }};
            }

            macro_rules! logical {
                (|$x:ident, $y:ident| $value:expr) => {{
                    if let (Value::Bool($x), Value::Bool($y)) = (&*reg!(instr.b), &*reg!(instr.c)) {
                        let ($x, $y) = (*$x, *$y);
                        store_value(reg!(instr.a), Value::Bool($value));
                        next!(1);
                    }
                }};
            }

            // A fused compare-and-branch of two numbers that can be ordered, or of two strings
            // (see `comparison`): `R[b] <op> R[c]` is followed by the jump to take when it
            // does not hold.
            macro_rules! branch {
                (|$x:ident, $y:ident| $holds:expr, $strings:ident, |$found:ident| $string_holds:expr) => {{
                    let (lhs, rhs) = (reg!(instr.b), reg!(instr.c));

                    if let (Value::Number($x), Value::Number($y)) = (&*lhs, &*rhs)
                        && let Some(holds) = {
                            let ($x, $y) = (*$x, *$y);
                            $holds
                        }
                    {
                        if holds {
                            next!(2);
                        }
                        jump!(ip.add(1));
                    }

                    if let (Value::String(first), Value::String(second)) = (&*lhs, &*rhs)
                        && let Some($found) = $strings(slots, *first, *second)
                    {
                        if $string_holds {
                            next!(2);
                        }
                        jump!(ip.add(1));
                    }
                }};
            }

            // The same for `R[a] <op> k`, where `k` is the following code word and the jump
            // offset is relative to it.
            macro_rules! branch_k {
                (|$x:ident, $y:ident| $holds:expr) => {{
                    if let Value::Number($x) = &*reg!(instr.a)
                        && let Some(holds) = {
                            let ($x, $y) = (*$x, f64::from_bits(*ip.add(1)));
                            $holds
                        }
                    {
                        if holds {
                            next!(2);
                        }
                        jump!(ip);
                    }
                }};
            }

            // A fused test of what the number `R[a]` leaves when it is divided by a power of
            // two, whose two operand words the jump follows: the branch goes the way of
            // `$equal` when that is the remainder asked for. A whole number shows it in its
            // bits; any other number is divided.
            macro_rules! remainder_branch {
                ($equal:expr) => {{
                    if let Value::Number(number) = &*reg!(instr.a) {
                        let test = RemainderTest::read(ip.add(1));
                        let leaves = match whole(*number) {
                            Some(whole) => test.holds_for(whole),
                            None => leaves_remainder(*number, test),
                        };

                        if leaves == $equal {
                            next!(3);
                        }
                        jump!(ip);
                    }
                }};
            }

            // `R[a] = (R[b] == null)` when `$null` is true, `R[a] = (R[b] != null)` otherwise.
            // Comparing a list against `null` broadcasts over its elements.
            macro_rules! null_test {
                ($null:expr) => {{
                    let operand = &*reg!(instr.b);

                    if !matches!(operand, Value::List(_)) {
                        let holds = matches!(operand, Value::Null) == $null;
                        store_value(reg!(instr.a), Value::Bool(holds));
                        next!(1);
                    }
                }};
            }

            // The fused compare-and-branch form of `null_test` on `R[a]`.
            macro_rules! null_branch {
                ($null:expr) => {{
                    let operand = &*reg!(instr.a);

                    if !matches!(operand, Value::List(_)) {
                        if matches!(operand, Value::Null) == $null {
                            next!(1);
                        }
                        jump!(ip);
                    }
                }};
            }

            // Whether the frame is at a depth that it can call a built-in function from. (A
            // frame that has to look itself up again is taken for one that cannot.)
            macro_rules! calls_built_ins {
                () => {
                    (*frame).meta as u32 <= MAX_CALL_DEPTH as u32
                };
            }

            // `R[a] = F(R[b])` for a built-in function `F` that comes from one number to
            // another by looking at nothing else.
            macro_rules! number_function {
                (|$x:ident| $value:expr) => {{
                    if let Value::Number($x) = &*reg!(instr.b)
                        && calls_built_ins!()
                    {
                        let $x = *$x;
                        reg!(instr.a).write(Value::Number($value));
                        next!(1);
                    }
                }};
            }

            // `R[a] = F(R[b], R[c])` for `min` (`$wins` is `<`) or `max` (`$wins` is `>`) of
            // two numbers of which one wins against the other, or which are one and the same.
            // That leaves two things to the function: a NaN, which it passes over, and two
            // zeros that differ in their sign, of which it finds the one that the standard
            // library of the machine this is compiled for finds.
            macro_rules! extreme {
                ($wins:tt) => {{
                    if let (Value::Number(x), Value::Number(y)) =
                        (&*reg!(instr.b), &*reg!(instr.c))
                        && calls_built_ins!()
                    {
                        let (x, y) = (*x, *y);
                        let same = x == y && x.to_bits() == y.to_bits();
                        if x $wins y || y $wins x || same {
                            let extreme = if y $wins x { y } else { x };
                            reg!(instr.a).write(Value::Number(extreme));
                            next!(1);
                        }
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
                    next!(1);
                }
                Op::LoadK => {
                    let consts = &(*(*frame).proto).consts;
                    debug_assert!(instr.bx() < consts.len());
                    load_value(reg!(instr.a), consts.as_ptr().add(instr.bx()));
                    next!(1);
                }
                Op::LoadNum => {
                    reg!(instr.a).write(Value::Number(f64::from_bits(*ip.add(1))));
                    next!(2);
                }
                Op::LoadStr => {
                    let literals = &(*(*frame).proto).literals;
                    debug_assert!(instr.bx() < literals.len());
                    let literal = literals.get_unchecked(instr.bx());

                    // The string that was made for the literal in this run is still where
                    // it was put.
                    if literal.run() == (*vm).run_number {
                        load_value(reg!(instr.a), literal.value());
                        next!(1);
                    }
                }
                Op::LoadUnbound => {
                    store_value(reg!(instr.a), UNBOUND);
                    next!(1);
                }
                Op::GetName => {
                    let names = &(*(*frame).proto).names;
                    debug_assert!(instr.bx() < names.len());
                    find!(*names.get_unchecked(instr.bx()));
                }
                Op::EnsureGlobal => {
                    if !is_unbound(&*reg!(instr.a)) {
                        next!(1);
                    }
                }
                Op::EnsureName => {
                    if !is_unbound(&*reg!(instr.a)) {
                        next!(1);
                    }

                    let names = &(*(*frame).proto).names;
                    debug_assert!(instr.bx() < names.len());
                    find!(*names.get_unchecked(instr.bx()));
                }
                Op::GetCapture => {
                    debug_assert!(!(*frame).def.is_null());

                    if let Some(value) = (*(*frame).def).scope.slot_ref(instr.b as usize) {
                        load_value(reg!(instr.a), value);
                        next!(1);
                    }

                    late_capture!(instr.c as usize);
                }
                Op::GetCaptureRef => {
                    debug_assert!(!(*frame).def.is_null());
                    let (slot, _) = (&(*(*frame).proto).slot_refs)[instr.bx()];

                    if let Some(value) = (*(*frame).def).scope.slot_ref(slot as usize) {
                        load_value(reg!(instr.a), value);
                        next!(1);
                    }

                    late_capture!(instr.bx());
                }

                Op::Add => arithmetic!(|x, y| x + y),
                Op::Sub => arithmetic!(|x, y| x - y),
                Op::Mul => arithmetic!(|x, y| x * y),
                Op::Div => arithmetic!(|x, y| x / y),
                Op::Mod => arithmetic!(|x, y| modulo(x, y)),
                Op::Pow => arithmetic!(|x, y| x.powf(y)),
                Op::Eq | Op::DotEq => {
                    comparison!(|x, y| Some(x == y), same_strings, |same| same)
                }
                Op::Ne | Op::DotNe => {
                    comparison!(|x, y| Some(x != y), same_strings, |same| !same)
                }
                Op::Lt | Op::DotLt => {
                    comparison!(|x, y| less(x, y), string_order, |order| order.is_lt())
                }
                Op::Le | Op::DotLe => {
                    comparison!(|x, y| less_eq(x, y), string_order, |order| order.is_le())
                }
                Op::Gt | Op::DotGt => {
                    comparison!(|x, y| less(y, x), string_order, |order| order.is_gt())
                }
                Op::Ge | Op::DotGe => {
                    comparison!(|x, y| less_eq(y, x), string_order, |order| order.is_ge())
                }
                Op::And | Op::NaturalAnd => logical!(|x, y| x && y),
                Op::Or | Op::NaturalOr => logical!(|x, y| x || y),
                Op::Coalesce => {
                    let (lhs, rhs) = (reg!(instr.b), reg!(instr.c));

                    match (&*lhs, &*rhs) {
                        (Value::List(_), _) | (_, Value::List(_)) => {}
                        (Value::Null, _) => {
                            copy_value(reg!(instr.a), rhs);
                            next!(1);
                        }
                        _ => {
                            copy_value(reg!(instr.a), lhs);
                            next!(1);
                        }
                    }
                }

                Op::AddK => arithmetic_k!(|x, y| x + y),
                Op::SubK => arithmetic_k!(|x, y| x - y),
                Op::MulK => arithmetic_k!(|x, y| x * y),
                Op::DivK => arithmetic_k!(|x, y| x / y),
                Op::ModK => {
                    if let Value::Number(x) = &*reg!(instr.b) {
                        // A divisor that is a small whole number is operand `c` as well,
                        // which is zero for any other.
                        let remainder = match small_remainder(*x, instr.c as u32) {
                            Some(remainder) => remainder,
                            None => modulo(*x, f64::from_bits(*ip.add(1))),
                        };
                        reg!(instr.a).write(Value::Number(remainder));
                        next!(2);
                    }
                }
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

                Op::JmpNotEq => branch!(|x, y| Some(x == y), same_strings, |same| same),
                Op::JmpNotNe => branch!(|x, y| Some(x != y), same_strings, |same| !same),
                Op::JmpNotLt => {
                    branch!(|x, y| less(x, y), string_order, |order| order.is_lt())
                }
                Op::JmpNotLe => {
                    branch!(|x, y| less_eq(x, y), string_order, |order| order.is_le())
                }
                Op::JmpNotGt => {
                    branch!(|x, y| less(y, x), string_order, |order| order.is_gt())
                }
                Op::JmpNotGe => {
                    branch!(|x, y| less_eq(y, x), string_order, |order| order.is_ge())
                }
                Op::JmpNotEqK => branch_k!(|x, y| Some(x == y)),
                Op::JmpNotNeK => branch_k!(|x, y| Some(x != y)),
                Op::JmpNotLtK => branch_k!(|x, y| less(x, y)),
                Op::JmpNotLeK => branch_k!(|x, y| less_eq(x, y)),
                Op::JmpNotGtK => branch_k!(|x, y| less(y, x)),
                Op::JmpNotGeK => branch_k!(|x, y| less_eq(y, x)),
                Op::JmpNotRemEq => remainder_branch!(true),
                Op::JmpNotRemNe => remainder_branch!(false),

                Op::EqNull => null_test!(true),
                Op::NeNull => null_test!(false),
                Op::JmpNotEqNull => null_branch!(true),
                Op::JmpNotNeNull => null_branch!(false),

                Op::Neg => {
                    if let Value::Number(number) = &*reg!(instr.b) {
                        reg!(instr.a).write(Value::Number(-*number));
                        next!(1);
                    }
                }
                Op::Not => {
                    if let Value::Bool(boolean) = &*reg!(instr.b) {
                        store_value(reg!(instr.a), Value::Bool(!*boolean));
                        next!(1);
                    }
                }

                Op::Jmp => jump!(ip),
                Op::JmpIfFalse => {
                    if let Value::Bool(holds) = &*reg!(instr.a) {
                        if *holds {
                            next!(1);
                        }
                        jump!(ip);
                    }
                }

                // A call to a user function whose code is already known and needs no argument
                // adjustment only has to push a frame record.
                Op::Call => {
                    let callee = reg!(instr.a);
                    let argc = instr.b as usize;

                    if let Value::Lambda(pointer) = &*callee
                        && let Some(HeapValue::Lambda(def)) = heap_value!(pointer)
                        && (def.scope.entry.run() == (*vm).run_number
                            || def.enters_own_code((*vm).run_number))
                    {
                        // The function is called through its own value, which is what the
                        // entry that this run found for it is the code for.
                        let proto: *const Proto = def.scope.entry.code();
                        let def: *const LambdaDef = Rc::as_ptr(def);
                        let callee_base = callee.add(1);
                        let meta = (*frame).meta;

                        if (*proto).fast_arity as usize == argc
                            && callee_base <= (*vm).stack_limit
                            && frame != (*vm).frame_limit
                            && (meta & DEPTH_MASK) as usize <= MAX_CALL_DEPTH
                        {
                            (*frame).ip = ip.add(1);
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
                    let callee_base = reg!(instr.a).add(1);
                    let meta = (*frame).meta;

                    // The low half of `meta` is the depth, plus a high bit when the function
                    // was redefined and has to be looked up again.
                    if meta as u32 <= MAX_CALL_DEPTH as u32
                        && callee_base <= (*vm).stack_limit
                        && frame != (*vm).frame_limit
                    {
                        // The function calls itself through its own name: the value the frame
                        // was called through is the callee.
                        copy_value(callee_base.sub(1), base.sub(1));

                        let (proto, def) = ((*frame).proto, (*frame).def);
                        (*frame).ip = ip.add(1);
                        frame = frame.add(1);
                        (*frame).base = callee_base;
                        (*frame).proto = proto;
                        (*frame).def = def;
                        (*frame).meta = (meta & DEPTH_MASK) + 1;
                        ip = ip.add(1).sub(instr.b as usize);
                        base = callee_base;
                        continue 'run;
                    }
                }
                Op::TailSelf0 => {
                    let meta = (*frame).meta;

                    if meta as u32 <= MAX_CALL_DEPTH as u32 {
                        restart!(meta);
                    }
                }
                Op::TailSelf1 => {
                    let meta = (*frame).meta;

                    if meta as u32 <= MAX_CALL_DEPTH as u32 {
                        let arg = reg!(instr.a);
                        copy_value(waiting_param(arg, instr.c), arg);
                        restart!(meta);
                    }
                }
                Op::TailSelf => {
                    let meta = (*frame).meta;

                    if meta as u32 <= MAX_CALL_DEPTH as u32 {
                        let args = reg!(instr.a);
                        let params = waiting_param(args, instr.c);

                        for offset in 0..instr.x as usize {
                            copy_value(params.add(offset), args.add(offset));
                        }
                        restart!(meta);
                    }
                }
                Op::Sqrt => number_function!(|x| x.sqrt()),
                Op::Sin => number_function!(|x| x.sin()),
                Op::Cos => number_function!(|x| x.cos()),
                Op::Tan => number_function!(|x| x.tan()),
                Op::Asin => number_function!(|x| x.asin()),
                Op::Acos => number_function!(|x| x.acos()),
                Op::Atan => number_function!(|x| x.atan()),
                Op::Log => number_function!(|x| x.ln()),
                Op::Log10 => number_function!(|x| x.log10()),
                Op::Exp => number_function!(|x| x.exp()),
                Op::Abs => number_function!(|x| x.abs()),
                Op::Floor => number_function!(|x| x.floor()),
                Op::Ceil => number_function!(|x| x.ceil()),
                Op::Round => number_function!(|x| x.round()),
                Op::Trunc => number_function!(|x| x.trunc()),
                Op::Min => extreme!(<),
                Op::Max => extreme!(>),
                Op::Len => {
                    // The length of a string is the number of its bytes.
                    let len = match &*reg!(instr.b) {
                        Value::List(pointer) => match heap_value!(pointer) {
                            Some(HeapValue::List(items)) => Some(items.len()),
                            _ => None,
                        },
                        Value::String(pointer) => match heap_value!(pointer) {
                            Some(HeapValue::String(text)) => Some(text.len()),
                            _ => None,
                        },
                        _ => None,
                    };

                    if let Some(len) = len
                        && calls_built_ins!()
                    {
                        reg!(instr.a).write(Value::Number(len as f64));
                        next!(1);
                    }
                }
                Op::Return => {
                    let meta = (*frame).meta;

                    if meta & (ENTRY | PROFILED) == 0 {
                        let value = reg!(instr.a);

                        // The result goes where the call instruction of the caller wants it,
                        // and the caller carries on after that instruction.
                        frame = frame.sub(1);
                        ip = (*frame).ip;
                        let caller = (*frame).base;
                        let target = (*ip.sub(1).cast::<Instr>()).c;
                        copy_value(register(caller, target), value);
                        base = caller;
                        continue 'run;
                    }

                    if meta & PROFILED == 0 {
                        // The frame was entered from native code, which is where the result
                        // goes: the loop is done.
                        let value = reg!(instr.a).read();
                        (*vm).top = frame.sub(1);

                        return Ok(value);
                    }
                }

                Op::ListPush => {
                    if let Some(Builder::List(list)) = (*vm).builders.last_mut()
                        && list.len() < list.capacity()
                    {
                        let len = list.len();
                        copy_value(list.as_mut_ptr().add(len), reg!(instr.a));
                        list.set_len(len + 1);
                        next!(1);
                    }
                }

                Op::Index => 'index: {
                    if let (Value::List(pointer), Value::Number(number)) =
                        (&*reg!(instr.b), &*reg!(instr.c))
                        && let Some(HeapValue::List(items)) = heap_value!(pointer)
                    {
                        // Read as an unsigned position, a negative index lies beyond the end
                        // of every list, so one comparison finds the indices that count from
                        // the start and are in range.
                        let index = truncate(*number);
                        let mut position = index as usize;

                        if position >= items.len() {
                            rare();
                            if index == NOT_TRUNCATED {
                                break 'index;
                            }

                            // A negative index counts from the end; one that is still
                            // negative after that wraps around to a position no list has,
                            // which is also where an index beyond the end stays.
                            position = (items.len() as i64).wrapping_add(index) as usize;
                            if position >= items.len() {
                                store_value(reg!(instr.a), Value::Null);
                                next!(1);
                            }
                        }

                        load_value(reg!(instr.a), items.as_ptr().add(position));
                        next!(1);
                    }

                    // A record is asked for what it has under a string, which is nothing
                    // when that is not one of its keys.
                    if let (Value::Record(pointer), Value::String(key)) =
                        (&*reg!(instr.b), &*reg!(instr.c))
                        && let Some(HeapValue::Record(record)) = heap_value!(pointer)
                        && let Some(HeapValue::String(key)) = heap_value!(key)
                    {
                        match record.position(key.as_str()) {
                            Some(position) => {
                                load_value(reg!(instr.a), record.as_slice().as_ptr().add(position));
                            }
                            None => store_value(reg!(instr.a), Value::Null),
                        }
                        next!(1);
                    }
                }
                Op::GetField => {
                    // The field usually sits where this access site found it last time.
                    if let Value::Record(pointer) = &*reg!(instr.b)
                        && let Some(HeapValue::Record(record)) = heap_value!(pointer)
                    {
                        let site = instr.c as usize | (instr.x as usize) << 16;
                        let proto = &*(*frame).proto;
                        debug_assert!(site < proto.field_caches.len());
                        let cache = proto.field_caches.get_unchecked(site);
                        let position = cache.position();
                        let values = record.as_slice();

                        // A record with the table seen last time has the field in the same
                        // place, when it has that many of the table's keys.
                        if Rc::as_ptr(record.table()) == cache.keys() {
                            if position < values.len() {
                                load_value(reg!(instr.a), values.as_ptr().add(position));
                                next!(1);
                            }

                            // The field is a key that the table has behind those of the
                            // record.
                            rare();
                            store_value(reg!(instr.a), Value::Null);
                            next!(1);
                        }

                        // A record with another table may have the field in the same place.
                        if let Some((key, value)) = record.get_index(position)
                            && **key == *proto.fields[site]
                        {
                            load_value(reg!(instr.a), value);
                            next!(1);
                        }
                    }
                }

                Op::AddDepth => {
                    (*frame).meta = (*frame).meta.wrapping_add(instr.sbx() as u64);
                    next!(1);
                }
                Op::CallbackDepth => {
                    let depth = ((*frame).meta & DEPTH_MASK) as usize;
                    if depth + CALLBACK_CALL_DEPTH <= MAX_CALL_DEPTH {
                        next!(1);
                    }
                }
                Op::ForRange => {
                    if range_next(reg!(instr.a), instr.x == 1) {
                        next!(1);
                    }
                    jump!(ip);
                }
                Op::ForRangeNext => {
                    if range_next(reg!(instr.a), instr.x == 1) {
                        jump!(ip);
                    }
                    next!(1);
                }
                Op::ForList => {
                    if list_next(reg!(instr.a), instr.x == 1, true) {
                        next!(1);
                    }
                    jump!(ip);
                }
                Op::ForListNext => {
                    if list_next(reg!(instr.a), instr.x == 1, false) {
                        jump!(ip);
                    }
                    next!(1);
                }

                // These always take a call into the rest of the VM. Naming them, instead of
                // leaving them to a wildcard, tells the compiler that every opcode has an arm,
                // so the jump to the arm needs no range check.
                Op::GetEnvParam
                | Op::GetEnvCapture
                | Op::GetInput
                | Op::Via
                | Op::Into
                | Op::Where
                | Op::Factorial
                | Op::CallBuiltIn
                | Op::CallList
                | Op::Closure
                | Op::NewList
                | Op::NewListSpread
                | Op::ListBegin
                | Op::ListAppend
                | Op::ListEnd
                | Op::NewRecord
                | Op::RecordBegin
                | Op::RecordSet
                | Op::RecordKey
                | Op::RecordValue
                | Op::RecordSpread
                | Op::RecordEnd
                | Op::Spread
                | Op::Raise
                | Op::CheckUnbound
                | Op::NameLambda
                | Op::SetGlobal
                | Op::RangePrep
                | Op::IterPrep => {}
            }

            // Whatever the fast paths above do not cover.
            (*frame).ip = ip.add(1);
            (*vm).top = frame;

            match (*vm).step(*instr) {
                Ok(Flow::Continue) => {}
                Ok(Flow::Finished(value)) => return Ok(value),
                Err(error) => return Err(error),
            }

            // Only `Vm::step` adds to the heap, so this is where the heap is found to have
            // grown, and between two instructions is where it can be compacted: everything
            // the run goes on with is where the VM can find it.
            if (*vm).compaction_is_due(&*heap) {
                (*vm).compact();
            }

            frame = (*vm).top;
            ip = (*frame).ip;
            base = (*frame).base;
            slots = (*heap).slots();
        }
    }
}
