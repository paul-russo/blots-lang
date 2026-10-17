//! The register VM that runs the bytecode produced by [`crate::compiler`].
//!
//! Every call gets a window of registers on the VM's value stack. A callee's window starts at
//! its first argument, so arguments are passed without being copied, and returning is a matter
//! of popping a frame record. Calls between user functions never recurse on the native stack;
//! only built-ins that take callbacks (and the `via`/`into`/`where` operators) re-enter the
//! VM, and they do so on the same value stack.
//!
//! Scoping is dynamic: a name that is not bound in the function that reads it is looked up in
//! the function that called it, and so on down to the environment the VM was created with. The
//! frame stack is that scope chain. Each frame knows, through its proto's local table, which
//! of its registers hold which variables at the instruction it is currently executing.
//!
//! This module holds the VM's state, its entry points and everything that does not run once
//! per instruction; the instruction loop itself lives in [`run`].

mod broadcast;
mod run;
mod step;

use crate::{
    ast::{BinaryOp, Span},
    bytecode::{CaptureSource, FAST_FRAME_SLOTS, Proto, Wrap},
    compiler::{compile_lambda, compile_self_variant},
    environment::Environment,
    error::RuntimeError,
    expressions::{OpSite, apply_binary_op, flatten_spread_value},
    functions::{BuiltInFunction, FunctionDef, check_lambda_arity, lambda_name},
    heap::{
        CONSTANTS, Heap, HeapPointer, HeapValue, IterablePointer, LambdaPointer, StringPointer,
    },
    intern::{Symbol, inputs_symbol},
    record::{Record, RecordBuilder},
    values::{CapturedScope, LambdaArg, LambdaDef, Value},
};
use std::{cell::RefCell, hint::unreachable_unchecked, mem::MaybeUninit, ptr, rc::Rc};

/// A call made from a body that is itself nested this deep fails.
const MAX_CALL_DEPTH: usize = 1000;

/// How much deeper than its caller the callback of a higher-order built-in is called: one
/// level for the built-in, one for the callback.
const CALLBACK_CALL_DEPTH: usize = 2;

/// The smallest value stack worth allocating.
const MIN_STACK: usize = 1024;

/// The smallest frame stack worth allocating.
const MIN_FRAMES: usize = 32;

/// The call depth a frame's body runs at lives in these bits of [`Frame::meta`].
const DEPTH_MASK: u64 = 0x7fff_ffff;

/// Set on a frame whose function was redefined in the heap while it was running: calls the
/// frame makes to "itself" have to go through the heap again.
const DEOPT: u64 = 1 << 31;

/// The number of times the frame replaced itself with a tail call lives above this bit.
const TAIL_SHIFT: u32 = 32;
const TAIL_MASK: u64 = 0x00ff_ffff;

/// Set on a frame that was entered from native code, which gets the result instead of a
/// calling frame.
const ENTRY: u64 = 1 << 56;

/// Set on a frame whose call is being timed for the profiler.
const PROFILED: u64 = 1 << 57;

/// The content of a register whose variable has not been assigned (yet). No expression can
/// produce this value, so it never escapes into the program.
const UNBOUND: Value = Value::Spread(IterablePointer::String(StringPointer::new(usize::MAX)));

/// Whether a value occupies exactly two machine words, which lets the VM move it as two
/// word-sized loads and stores.
const WORD_PAIR: bool =
    size_of::<Value>() == 2 * size_of::<u64>() && align_of::<Value>() >= align_of::<u64>();

#[inline(always)]
fn is_unbound(value: &Value) -> bool {
    matches!(value, Value::Spread(IterablePointer::String(pointer)) if pointer.index() == usize::MAX)
}

/// Copy a value from one slot to another.
///
/// Registers are written one word at a time (a tag and a payload), and a CPU cannot forward
/// two separate pending stores into one wide load: copying a freshly written register with a
/// single 16-byte move stalls until both stores have retired. Reading the two words
/// separately keeps such copies on the fast path.
///
/// # Safety
/// `src` must point to an initialised value and `dst` to a slot that is valid for writes.
#[inline(always)]
unsafe fn copy_value(dst: *mut Value, src: *const Value) {
    if !WORD_PAIR {
        // SAFETY: guaranteed by the caller.
        unsafe { dst.write(src.read()) };
        return;
    }

    let (src, dst) = (
        src.cast::<MaybeUninit<u64>>(),
        dst.cast::<MaybeUninit<u64>>(),
    );

    // SAFETY: a value spans exactly two words (`WORD_PAIR`), which are moved as opaque,
    // possibly uninitialised bytes. The volatile reads only keep the two loads from being
    // merged into one.
    unsafe {
        let low = src.read_volatile();
        let high = src.add(1).read_volatile();
        dst.write(low);
        dst.add(1).write(high);
    }
}

/// Store a value into a slot as two full words, so that it can be copied onwards right away
/// (see [`copy_value`]) whatever the width of its payload.
///
/// # Safety
/// `dst` must point to a slot that is valid for writes.
#[inline(always)]
unsafe fn store_value(dst: *mut Value, value: Value) {
    if !WORD_PAIR {
        // SAFETY: guaranteed by the caller.
        unsafe { dst.write(value) };
        return;
    }

    let words = (&raw const value).cast::<MaybeUninit<u64>>();
    let dst = dst.cast::<MaybeUninit<u64>>();

    // SAFETY: as in `copy_value`; the volatile writes keep the stores from being narrowed to
    // the bytes of the payload that are actually defined.
    unsafe {
        dst.write_volatile(words.read());
        dst.add(1).write_volatile(words.add(1).read());
    }
}

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

/// Advance the inlined range loop whose state starts at `slot` (see `Op::RangePrep`): when
/// the range has another element, store it (and its index, if the loop wants one) and report
/// `true`.
///
/// # Safety
/// `slot` must point to loop state registers set up by `Op::RangePrep`.
#[inline(always)]
unsafe fn range_next(slot: *mut Value, with_index: bool) -> bool {
    // SAFETY: guaranteed by the caller.
    unsafe {
        let next = state(slot);
        if next >= state(slot.add(1)) {
            return false;
        }

        let index = next as i64 as f64;
        slot.add(3)
            .write(Value::Number(number_in(slot.add(2)) + index));
        if with_index {
            slot.add(4).write(Value::Number(index));
        }
        update_state(slot, next + 1);
    }

    true
}

/// Advance the inlined list loop whose state starts at `slot` (see `Op::IterPrep`): when the
/// list has another element, store it (and its index, if the loop wants one; `first` says
/// whether this is the first element) and report `true`.
///
/// # Safety
/// `slot` must point to loop state registers set up by `Op::IterPrep`, and the list they
/// point into must be alive.
#[inline(always)]
unsafe fn list_next(slot: *mut Value, with_index: bool, first: bool) -> bool {
    // SAFETY: guaranteed by the caller.
    unsafe {
        let cursor = state(slot) as *const Value;
        if cursor as u64 == state(slot.add(4)) {
            return false;
        }

        copy_value(slot.add(2), cursor);
        if with_index {
            let index = if first {
                0.0
            } else {
                number_in(slot.add(3)) + 1.0
            };
            slot.add(3).write(Value::Number(index));
        }
        update_state(slot, cursor.add(1) as u64);
    }

    true
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

/// The position of the element a (possibly negative) index refers to.
#[inline(always)]
fn element_position(index: i64, len: usize) -> Option<usize> {
    if index >= 0 {
        return Some(index as usize);
    }

    let adjusted = len as i64 + index;
    (adjusted >= 0).then_some(adjusted as usize)
}

fn built_in_name(built_in: BuiltInFunction) -> String {
    format!("built-in function \"{}\"", built_in.name())
}

fn depth_exceeded(function_name: &str) -> RuntimeError {
    RuntimeError::new(format!(
        "in {}: maximum call depth of {} exceeded",
        function_name, MAX_CALL_DEPTH
    ))
}

/// Merge a spread value into a record literal: lists and strings contribute their elements
/// under their positions, records contribute their entries.
fn spread_into_record(
    heap: &Rc<RefCell<Heap>>,
    entries: &mut RecordBuilder,
    iterable: IterablePointer,
) -> Result<(), RuntimeError> {
    match iterable {
        IterablePointer::List(pointer) => {
            let heap = heap.borrow();
            for (position, value) in pointer.reify(&heap).as_list()?.iter().enumerate() {
                entries.insert(position.to_string(), *value);
            }
        }
        IterablePointer::String(pointer) => {
            let text = pointer.reify(&heap.borrow()).as_string()?.to_string();
            for (position, character) in text.chars().enumerate() {
                let value = heap.borrow_mut().insert_string(character.to_string());
                entries.insert(position.to_string(), value);
            }
        }
        IterablePointer::Record(pointer) => {
            let heap = heap.borrow();
            for (key, value) in pointer.reify(&heap).as_record()? {
                entries.insert(key.clone(), *value);
            }
        }
    }

    Ok(())
}

/// A list or record literal (or a spread argument list) that is still being assembled.
enum Builder {
    List(Vec<Value>),
    Record {
        entries: RecordBuilder,

        /// The evaluated key of a `[key]: value` entry whose value is being computed.
        key: Option<String>,
    },
}

#[cfg(not(target_arch = "wasm32"))]
struct CallProfile {
    start: std::time::Instant,
    start_var_env: std::time::Instant,
    end_var_env: std::time::Instant,
}

/// The record of an active call. Frames are plain data: what they point to is kept alive by
/// the heap (function definitions and, through them, their code) or by the VM itself.
#[derive(Clone, Copy)]
struct Frame {
    /// The code word after the instruction being executed. Only kept up to date while the
    /// frame is suspended in a call or when something is about to inspect the frame stack.
    ip: *const u64,

    /// The frame's first register. The slot below it holds the value the function was called
    /// through, which its own name is bound to inside the body.
    base: *mut Value,

    proto: *const Proto,

    /// The function being run; null for chunks.
    def: *const LambdaDef,

    /// The call depth the body runs at, the tail-call count and the frame flags.
    meta: u64,

    /// The saved `ip` of the tail call that last restarted the frame (only meaningful when
    /// the tail-call count is not zero).
    tail_ip: *const u64,
}

const NO_FRAME: Frame = Frame {
    ip: ptr::null(),
    base: ptr::null_mut(),
    proto: ptr::null(),
    def: ptr::null(),
    meta: 0,
    tail_ip: ptr::null(),
};

impl Frame {
    #[inline(always)]
    fn depth(&self) -> usize {
        (self.meta & DEPTH_MASK) as usize
    }

    fn tail_calls(&self) -> usize {
        ((self.meta >> TAIL_SHIFT) & TAIL_MASK) as usize
    }

    fn proto(&self) -> &Proto {
        // SAFETY: the code of an active frame outlives it (see `Vm::proto_for`).
        unsafe { &*self.proto }
    }

    fn def(&self) -> Option<&LambdaDef> {
        // SAFETY: the definition of an active frame is kept alive by the heap, by the native
        // caller that entered the frame, or by `Vm::retired`.
        unsafe { self.def.as_ref() }
    }

    /// The index of the code word `ip` was saved after.
    fn pc_of(&self, ip: *const u64) -> usize {
        // SAFETY: a saved `ip` points into (or one past) the frame's code.
        let offset = unsafe { ip.offset_from(self.proto().code.as_ptr()) };
        (offset as usize).wrapping_sub(1)
    }

    /// The index of the instruction the frame is executing (or suspended in).
    fn pc(&self) -> usize {
        self.pc_of(self.ip)
    }

    /// The value the function was called through.
    fn this_value(&self) -> Value {
        // SAFETY: every function frame is laid out right above its callee value.
        unsafe { *self.base.sub(1) }
    }
}

pub struct Vm {
    heap: Rc<RefCell<Heap>>,

    /// Where names that no frame binds are resolved, and where top-level assignments go.
    env: Rc<Environment>,

    stack: Vec<Value>,

    /// The highest frame base that leaves [`FAST_FRAME_SLOTS`] registers free: calls into
    /// small frames at or below it need no further stack check. Null while profiling, which
    /// sends every call down the slow path where it is timed.
    stack_limit: *mut Value,

    /// Frame records, always fully initialised. The first one is a placeholder that never
    /// runs, so that `top` has somewhere to point when no frame is active.
    frames: Vec<Frame>,

    /// The innermost active frame (or the placeholder). While the instruction loop runs it
    /// tracks the frame stack itself and only writes this back before calling out of the loop.
    top: *mut Frame,

    /// The last frame record: pushing a frame above it needs the frame stack to grow first.
    frame_limit: *mut Frame,

    builders: Vec<Builder>,

    /// The source text of the chunk at the bottom of the frame stack.
    chunk_source: Option<Rc<str>>,

    /// Definitions that were replaced in the heap while a frame was still running them.
    retired: Vec<Rc<LambdaDef>>,

    /// Code compiled for definitions that carry the code of another definition.
    orphans: Vec<Rc<Proto>>,

    #[cfg(not(target_arch = "wasm32"))]
    profiling: bool,

    /// The timings of the active frames flagged [`PROFILED`], innermost last.
    #[cfg(not(target_arch = "wasm32"))]
    profiles: Vec<CallProfile>,
}

impl Vm {
    pub fn new(heap: Rc<RefCell<Heap>>, bindings: Rc<Environment>) -> Self {
        Self {
            heap,
            env: bindings,
            stack: Vec::new(),
            stack_limit: ptr::null_mut(),
            frames: Vec::new(),
            top: ptr::null_mut(),
            frame_limit: ptr::null_mut(),
            builders: Vec::new(),
            chunk_source: None,
            retired: Vec::new(),
            orphans: Vec::new(),
            #[cfg(not(target_arch = "wasm32"))]
            profiling: crate::functions::is_profiling_enabled(),
            #[cfg(not(target_arch = "wasm32"))]
            profiles: Vec::new(),
        }
    }

    #[inline(always)]
    fn is_profiling(&self) -> bool {
        #[cfg(not(target_arch = "wasm32"))]
        let profiling = self.profiling;
        #[cfg(target_arch = "wasm32")]
        let profiling = false;

        profiling
    }

    /// Whether calls to higher-order built-ins may be compiled into inline loops. Profiling
    /// reports every call, so it needs the callbacks to stay real calls.
    pub fn inlines_callbacks(&self) -> bool {
        !self.is_profiling()
    }

    /// Run a compiled top-level expression whose spans refer to `source`.
    pub fn run_chunk(
        &mut self,
        chunk: &Rc<Proto>,
        source: Rc<str>,
        call_depth: usize,
    ) -> Result<Value, RuntimeError> {
        debug_assert!(
            self.frame_count() == 0,
            "chunks run at the bottom of the stack"
        );

        let base = 1;
        self.ensure_stack(base + chunk.max_stack);
        self.stack[base..base + chunk.cache_regs].fill(UNBOUND);
        self.chunk_source = Some(source);

        // SAFETY: the stack was just sized to hold the chunk's registers from `base` on.
        let base = unsafe { self.stack.as_mut_ptr().add(base) };
        self.push_frame(Frame {
            ip: chunk.code.as_ptr(),
            base,
            proto: Rc::as_ptr(chunk),
            def: ptr::null(),
            meta: call_depth.min(DEPTH_MASK as usize / 2) as u64 | ENTRY,
            tail_ip: ptr::null(),
        });

        self.run()
    }

    /// Call a function value from native code: from a built-in or operator that was handed a
    /// callback, or from an embedder. `call_depth` is the depth of the caller.
    pub fn call_function(
        &mut self,
        def: &FunctionDef,
        this_value: Value,
        args: &[Value],
        call_depth: usize,
    ) -> Result<Value, RuntimeError> {
        def.check_arity(args.len())?;

        if call_depth > MAX_CALL_DEPTH {
            return Err(depth_exceeded(&def.get_name()));
        }

        let lambda = match def {
            FunctionDef::Lambda(lambda) => lambda,
            FunctionDef::BuiltIn(built_in) => {
                return self
                    .run_built_in(*built_in, args.to_vec(), call_depth + 1)
                    .map_err(|error| error.with_function_context(&built_in_name(*built_in)));
            }
        };

        #[cfg(not(target_arch = "wasm32"))]
        let start = self.profiling.then(std::time::Instant::now);

        let generic = self.proto_for(lambda)?;
        let proto = self.select_variant(generic, lambda, this_value);

        // SAFETY: `proto_for` and `select_variant` return code that outlives the call.
        let max_stack = unsafe { (*proto).max_stack };
        let base = self.entry_base();
        self.ensure_stack(base + max_stack.max(args.len()));
        self.stack[base - 1] = this_value;
        self.stack[base..base + args.len()].copy_from_slice(args);
        self.bind_params(proto, base, args.len());

        #[allow(unused_mut)]
        let mut meta = (call_depth + 1) as u64 | ENTRY;
        #[cfg(not(target_arch = "wasm32"))]
        if let Some(start) = start {
            self.profiles.push(CallProfile {
                start,
                start_var_env: start,
                end_var_env: std::time::Instant::now(),
            });
            meta |= PROFILED;
        }

        // SAFETY: the stack was just sized to hold the frame's registers from `base` on.
        let base = unsafe { self.stack.as_mut_ptr().add(base) };
        self.push_frame(Frame {
            // SAFETY: as above.
            ip: unsafe { (*proto).code.as_ptr() },
            base,
            proto,
            def: Rc::as_ptr(lambda),
            meta,
            tail_ip: ptr::null(),
        });

        self.run()
    }

    /// Run the innermost frame, and every frame it calls, until it returns.
    fn run(&mut self) -> Result<Value, RuntimeError> {
        let builders = self.builders.len();

        // SAFETY: the innermost frame was set up by `run_chunk` or `call_function`: its code
        // is alive, its registers lie within the value stack and its parameters are bound.
        let result = unsafe { run::execute(self) };

        if result.is_err() {
            self.builders.truncate(builders);
        }
        if self.frame_count() == 0 {
            self.retired.clear();
        }

        result
    }

    // ---------------------------------------------------------------------------------------
    // Frames and the value stack
    // ---------------------------------------------------------------------------------------

    /// The number of active frames; they are `frames[1..=frame_count]`.
    fn frame_count(&self) -> usize {
        if self.top.is_null() {
            return 0;
        }

        // SAFETY: `top` points into the frame records whenever there are any.
        unsafe { self.top.offset_from(self.frames.as_ptr()) as usize }
    }

    #[inline(always)]
    fn top(&self) -> &Frame {
        debug_assert!(self.frame_count() > 0, "the VM has no active frame");

        // SAFETY: `top` points at the innermost active frame.
        unsafe { &*self.top }
    }

    fn active_frames(&self) -> &[Frame] {
        match self.frame_count() {
            0 => &[],
            count => &self.frames[1..=count],
        }
    }

    fn active_frames_mut(&mut self) -> &mut [Frame] {
        match self.frame_count() {
            0 => &mut [],
            count => &mut self.frames[1..=count],
        }
    }

    fn push_frame(&mut self, frame: Frame) {
        if self.top == self.frame_limit {
            self.grow_frames();
        }

        // SAFETY: `top` is below the last frame record, so the record above it exists.
        unsafe {
            self.top = self.top.add(1);
            self.top.write(frame);
        }
    }

    fn pop_frame(&mut self) -> Frame {
        debug_assert!(self.frame_count() > 0, "the VM has no active frame");

        // SAFETY: `top` points at the innermost active frame, which sits above the placeholder.
        unsafe {
            let frame = *self.top;
            self.top = self.top.sub(1);

            frame
        }
    }

    #[cold]
    fn grow_frames(&mut self) {
        let count = self.frame_count();
        let len = (self.frames.len() * 2).max(MIN_FRAMES);
        self.frames.resize(len, NO_FRAME);

        // SAFETY: the records hold at least the placeholder and every active frame.
        unsafe {
            let start = self.frames.as_mut_ptr();
            self.top = start.add(count);
            self.frame_limit = start.add(len - 1);
        }
    }

    /// Drop the innermost frame because its function returned, reporting whether it was
    /// entered from native code (which then gets the result).
    #[inline(never)]
    fn finish_frame(&mut self) -> bool {
        let frame = self.pop_frame();
        self.finish_profile(&frame);

        frame.meta & ENTRY != 0
    }

    /// The index of a stack slot that belongs to a frame.
    fn slot_index(&self, slot: *const Value) -> usize {
        // SAFETY: frame registers always point into the value stack.
        unsafe { slot.offset_from(self.stack.as_ptr()) as usize }
    }

    /// The base of a frame entered from native code: above every active frame, leaving one
    /// slot for the value the function is called through.
    fn entry_base(&self) -> usize {
        match self.active_frames().last() {
            Some(frame) => self.slot_index(frame.base) + frame.proto().max_stack + 1,
            None => 1,
        }
    }

    /// Make sure the stack holds `needed` slots plus the headroom small frames rely on.
    #[inline(always)]
    fn ensure_stack(&mut self, needed: usize) {
        if needed + FAST_FRAME_SLOTS > self.stack.len() {
            self.grow_stack(needed + FAST_FRAME_SLOTS);
        }
    }

    #[cold]
    fn grow_stack(&mut self, needed: usize) {
        let len = needed.max(self.stack.len() * 2).max(MIN_STACK);
        let bases: Vec<usize> = self
            .active_frames()
            .iter()
            .map(|frame| self.slot_index(frame.base))
            .collect();

        let mut stack = Vec::with_capacity(len);
        stack.extend_from_slice(&self.stack);
        stack.resize(len, Value::Null);
        self.stack = stack;

        let start = self.stack.as_mut_ptr();
        for (frame, base) in self.active_frames_mut().iter_mut().zip(bases) {
            // SAFETY: the new stack is at least as long as the old one.
            frame.base = unsafe { start.add(base) };
        }

        self.stack_limit = if self.is_profiling() {
            ptr::null_mut()
        } else {
            // SAFETY: the stack is longer than the headroom.
            unsafe { start.add(len - FAST_FRAME_SLOTS) }
        };
    }

    fn source_of(&self, frame: &Frame) -> Rc<str> {
        match (frame.def(), &self.chunk_source) {
            (Some(def), _) => Rc::clone(&def.source),
            (None, Some(source)) => Rc::clone(source),
            (None, None) => Rc::from(""),
        }
    }

    /// An error located at the instruction the innermost frame is executing.
    fn span_error(&self, message: String) -> RuntimeError {
        let frame = self.top();

        match frame.proto().span_at(frame.pc()) {
            Some(span) => RuntimeError::with_span(message, span, self.source_of(frame)),
            None => RuntimeError::new(message),
        }
    }

    fn unknown_identifier(&self, name: Symbol) -> RuntimeError {
        let message = if CONSTANTS.contains_key(name.as_str()) {
            format!(
                "unknown identifier: {}. Did you mean constants.{}?",
                name, name
            )
        } else {
            format!("unknown identifier: {}", name)
        };

        self.span_error(message)
    }

    /// Point an error raised by the call the innermost frame is executing at that call, unless
    /// it already points somewhere more specific.
    fn at_call_site(&self, error: RuntimeError) -> RuntimeError {
        let frame = self.top();

        match frame.proto().span_at(frame.pc()) {
            Some(span) => error.with_call_site(span, self.source_of(frame)),
            None => error,
        }
    }

    /// Resolve a name through the scope chain: every active frame, innermost first, then the
    /// boundary environment. Within a frame, block variables shadow parameters, which shadow
    /// the function's own name, which shadows its captures.
    fn lookup(&self, name: Symbol) -> Option<Value> {
        for frame in self.active_frames().iter().rev() {
            let proto = frame.proto();
            let pc = frame.pc() as u32;

            for local in proto.locals.iter().rev() {
                if local.name != name || pc < local.start || pc >= local.end {
                    continue;
                }

                // SAFETY: the registers of the proto's locals lie within the frame.
                let value = unsafe { *frame.base.add(local.reg as usize) };
                if !is_unbound(&value) {
                    return Some(value);
                }
            }

            if let Some(position) = proto.params.iter().position(|param| *param == name) {
                // SAFETY: parameters occupy the first registers of the frame.
                return Some(unsafe { *frame.base.add(position) });
            }

            let Some(def) = frame.def() else {
                continue;
            };
            if def.name == Some(name) {
                return Some(frame.this_value());
            }
            if let Some(value) = def.scope.get(name) {
                return Some(value);
            }
        }

        self.env.get(name)
    }

    /// Resolve a name the way an identifier expression does, failing with the usual error.
    fn lookup_or_unknown(&self, name: Symbol) -> Result<Value, RuntimeError> {
        self.lookup(name)
            .ok_or_else(|| self.unknown_identifier(name))
    }

    // ---------------------------------------------------------------------------------------
    // Calls
    // ---------------------------------------------------------------------------------------

    /// The compiled body of a function, compiling it first if this is the first time the
    /// definition is called. The code lives as long as the definition (or, for the rare
    /// definition that carries another one's code, as long as the VM).
    fn proto_for(&mut self, def: &LambdaDef) -> Result<*const Proto, RuntimeError> {
        let Some(code) = def.scope.code.get() else {
            let code = compile_lambda(&def.args, &def.body, self.inlines_callbacks())?;
            let pointer = Rc::as_ptr(&code);
            let _ = def.scope.code.set(code);

            return Ok(pointer);
        };

        if code.belongs_to(&def.body, &def.args) {
            return Ok(Rc::as_ptr(code));
        }

        // A definition assembled from the parts of another one can carry that one's code.
        let known = self
            .orphans
            .iter()
            .find(|orphan| orphan.belongs_to(&def.body, &def.args));
        if let Some(orphan) = known {
            return Ok(Rc::as_ptr(orphan));
        }

        let code = compile_lambda(&def.args, &def.body, self.inlines_callbacks())?;
        let pointer = Rc::as_ptr(&code);
        self.orphans.push(code);

        Ok(pointer)
    }

    /// The code to run for a call of `def` through `this_value`: the variant of `generic` in
    /// which the function calls itself directly when the function's own name is known to
    /// resolve to the function (see [`crate::bytecode::SelfRef`]), `generic` otherwise.
    fn select_variant(
        &self,
        generic: *const Proto,
        def: &LambdaDef,
        this_value: Value,
    ) -> *const Proto {
        // SAFETY: `generic` comes from `proto_for`.
        let proto = unsafe { &*generic };
        if proto.self_refs.is_empty() || self.is_profiling() {
            return generic;
        }

        let (Some(name), Value::Lambda(pointer)) = (def.name, this_value) else {
            return generic;
        };

        // The self-binding has to be the definition being called, so that a call through it
        // is a call of the running function.
        let is_current = matches!(
            self.heap.borrow().get(pointer.index()),
            Some(HeapValue::Lambda(current)) if ptr::eq(Rc::as_ptr(current), def)
        );
        if !is_current {
            return generic;
        }

        let self_ref = proto.self_refs.iter().find(|self_ref| {
            self_ref.name == name && def.scope.slot_ref(self_ref.slot as usize).is_none()
        });
        let Some(self_ref) = self_ref else {
            return generic;
        };

        let variant = self_ref.variant.get_or_init(|| {
            compile_self_variant(
                &def.args,
                &def.body,
                self.inlines_callbacks(),
                name,
                self_ref.slot,
            )
            .ok()
            .filter(|variant| variant.max_stack <= FAST_FRAME_SLOTS)
        });

        match variant {
            Some(variant) => Rc::as_ptr(variant),
            None => generic,
        }
    }

    /// Turn the `argc` arguments at `base` into the parameter registers the body expects:
    /// missing optional parameters become null and a rest parameter collects what is left.
    fn bind_params(&mut self, proto: *const Proto, base: usize, argc: usize) {
        // SAFETY: the caller passes code that is alive.
        let proto = unsafe { &*proto };
        if proto.simple_params && argc == proto.params.len() {
            return;
        }

        let Some(params) = proto.args.as_ref() else {
            return;
        };

        for (position, param) in params.iter().enumerate() {
            match param {
                LambdaArg::Required(_) | LambdaArg::Optional(_) => {
                    if position >= argc {
                        self.stack[base + position] = Value::Null;
                    }
                }
                LambdaArg::Rest(_) => {
                    let rest = if position < argc {
                        self.stack[base + position..base + argc].to_vec()
                    } else {
                        Vec::new()
                    };
                    self.stack[base + position] = self.heap.borrow_mut().insert_list(rest);
                }
            }
        }
    }

    /// Copy call arguments that were gathered at runtime into the registers a call expects
    /// them in: the ones following register `a` of the innermost frame.
    fn place_args(&mut self, a: usize, args: &[Value]) {
        let base = self.slot_index(self.top().base) + a + 1;
        self.ensure_stack(base + args.len());
        self.stack[base..base + args.len()].copy_from_slice(args);
    }

    /// Hand the list being built to the call at register `a` of the innermost frame as its
    /// arguments, returning how many there are.
    fn place_list_args(&mut self, a: usize) -> usize {
        let Some(Builder::List(args)) = self.builders.pop() else {
            unreachable!("no argument list is being built");
        };
        self.place_args(a, &args);

        args.len()
    }

    /// Call the value in register `a` of the innermost frame (whose `ip` is saved) with the
    /// `argc` arguments that follow it. A built-in runs to completion and its result is
    /// stored in register `dst`; for a user function a frame is pushed, to be run by the
    /// caller. Either way the caller continues with the innermost frame.
    #[inline(never)]
    fn call_slow(&mut self, a: usize, argc: usize, dst: u16) -> Result<(), RuntimeError> {
        let argc = self.flatten_args(a, argc)?;
        let frame = *self.top();
        let depth = frame.depth();
        let base = self.slot_index(frame.base) + a + 1;
        let callee = self.stack[base - 1];

        match callee {
            Value::Lambda(pointer) => {
                let def: *const LambdaDef = match self.heap.borrow().get(pointer.index()) {
                    Some(HeapValue::Lambda(def)) => Rc::as_ptr(def),
                    _ => panic!("Dangling pointer!"),
                };

                // SAFETY: the heap keeps the definition alive for as long as the VM runs; a
                // definition that is replaced while it is in use is retired, not dropped.
                let lambda = unsafe { &*def };
                let generic = self
                    .proto_for(lambda)
                    .map_err(|error| self.at_call_site(error))?;

                // SAFETY: `proto_for` returns code that is alive.
                let simple = unsafe { (*generic).simple_arity } as usize == argc;
                if !simple {
                    check_lambda_arity(lambda, argc).map_err(|error| self.at_call_site(error))?;
                }

                if depth > MAX_CALL_DEPTH {
                    return Err(self.at_call_site(depth_exceeded(&lambda_name(lambda))));
                }

                #[cfg(not(target_arch = "wasm32"))]
                let start = self.profiling.then(std::time::Instant::now);

                let proto = self.select_variant(generic, lambda, callee);

                // SAFETY: as above.
                let max_stack = unsafe { (*proto).max_stack };
                self.ensure_stack(base + max_stack.max(argc));
                self.bind_params(proto, base, argc);

                #[allow(unused_mut)]
                let mut meta = (depth + 1) as u64;
                #[cfg(not(target_arch = "wasm32"))]
                if let Some(start) = start {
                    self.profiles.push(CallProfile {
                        start,
                        start_var_env: start,
                        end_var_env: std::time::Instant::now(),
                    });
                    meta |= PROFILED;
                }

                // SAFETY: the stack was just sized to hold the frame's registers.
                let base = unsafe { self.stack.as_mut_ptr().add(base) };
                self.push_frame(Frame {
                    // SAFETY: as above.
                    ip: unsafe { (*proto).code.as_ptr() },
                    base,
                    proto,
                    def,
                    meta,
                    tail_ip: ptr::null(),
                });

                Ok(())
            }
            Value::BuiltIn(built_in) => {
                FunctionDef::BuiltIn(built_in)
                    .check_arity(argc)
                    .map_err(|error| self.at_call_site(error))?;

                if depth > MAX_CALL_DEPTH {
                    return Err(self.at_call_site(depth_exceeded(&built_in_name(built_in))));
                }

                let result = if built_in.takes_callback() {
                    let args = self.stack[base..base + argc].to_vec();
                    self.run_built_in(built_in, args, depth + 1)
                } else {
                    self.run_plain_built_in(built_in, base, argc, depth + 1)
                };

                match result {
                    Ok(value) => {
                        let target = self.slot_index(self.top().base) + dst as usize;
                        self.stack[target] = value;

                        Ok(())
                    }
                    Err(error) => {
                        let error = error.with_function_context(&built_in_name(built_in));
                        Err(self.at_call_site(error))
                    }
                }
            }
            _ => {
                let message = format!(
                    "can't call a non-function: {}",
                    callee.stringify_internal(&self.heap.borrow())
                );
                Err(self.span_error(message))
            }
        }
    }

    /// Expand the spread values among the `argc` arguments that follow register `a` of the
    /// innermost frame, returning the resulting argument count.
    fn flatten_args(&mut self, a: usize, argc: usize) -> Result<usize, RuntimeError> {
        let base = self.slot_index(self.top().base) + a + 1;
        let given = &self.stack[base..base + argc];
        if !given.iter().any(|arg| matches!(arg, Value::Spread(_))) {
            return Ok(argc);
        }

        let mut args = Vec::with_capacity(argc);
        for arg in given {
            match arg {
                Value::Spread(pointer) => {
                    let values = flatten_spread_value(pointer, &self.heap)?;
                    args.extend(values.iter().copied());
                }
                value => args.push(*value),
            }
        }
        self.place_args(a, &args);

        Ok(args.len())
    }

    /// Run a built-in that may call back into the VM. The arguments are owned because the
    /// value stack can move while the callbacks run.
    fn run_built_in(
        &mut self,
        built_in: BuiltInFunction,
        args: Vec<Value>,
        depth: usize,
    ) -> Result<Value, RuntimeError> {
        #[cfg(not(target_arch = "wasm32"))]
        let start = self.profiling.then(std::time::Instant::now);

        let heap = Rc::clone(&self.heap);
        let result =
            built_in.call_with(&args, &heap, depth, &mut |def, this_value, args, depth| {
                self.call_function(def, this_value, args, depth)
            });

        #[cfg(not(target_arch = "wasm32"))]
        if let Some(start) = start {
            Self::profile_built_in(built_in, start);
        }

        result
    }

    /// Run a built-in that takes no callbacks directly on the arguments in the value stack.
    fn run_plain_built_in(
        &mut self,
        built_in: BuiltInFunction,
        base: usize,
        argc: usize,
        depth: usize,
    ) -> Result<Value, RuntimeError> {
        #[cfg(not(target_arch = "wasm32"))]
        let start = self.profiling.then(std::time::Instant::now);

        let args = &self.stack[base..base + argc];
        let result = built_in.call_with(args, &self.heap, depth, &mut |_, _, _, _| {
            Err(RuntimeError::new(format!(
                "{} cannot call functions",
                built_in_name(built_in)
            )))
        });

        #[cfg(not(target_arch = "wasm32"))]
        if let Some(start) = start {
            Self::profile_built_in(built_in, start);
        }

        result
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn profile_built_in(built_in: BuiltInFunction, start: std::time::Instant) {
        crate::functions::FUNCTION_CALLS
            .lock()
            .unwrap()
            .push(crate::stats::FunctionCallStats {
                name: built_in_name(built_in),
                start,
                end: std::time::Instant::now(),
                start_var_env: None,
                end_var_env: None,
            });
    }

    /// Report the call of a frame that is going away to the profiler, if it was being timed.
    fn finish_profile(&mut self, frame: &Frame) {
        #[cfg(not(target_arch = "wasm32"))]
        if frame.meta & PROFILED != 0
            && let (Some(profile), Some(def)) = (self.profiles.pop(), frame.def())
        {
            crate::functions::FUNCTION_CALLS.lock().unwrap().push(
                crate::stats::FunctionCallStats {
                    name: lambda_name(def),
                    start: profile.start,
                    end: std::time::Instant::now(),
                    start_var_env: Some(profile.start_var_env),
                    end_var_env: Some(profile.end_var_env),
                },
            );
        }

        #[cfg(target_arch = "wasm32")]
        let _ = frame;
    }

    /// Abandon every frame down to (and including) the one the current `run` was entered
    /// through, decorating `error` (raised by the instruction the innermost frame is
    /// executing) with the context each level adds on the way out.
    #[cold]
    #[inline(never)]
    fn unwind(&mut self, mut error: RuntimeError) -> RuntimeError {
        loop {
            let frame = self.pop_frame();
            let proto = frame.proto();
            let pc = frame.pc();
            let source = self.source_of(&frame);

            // Handlers are recorded innermost first.
            for handler in &proto.handlers {
                if pc < handler.start as usize || pc >= handler.end as usize {
                    continue;
                }

                for wrap in &handler.wraps {
                    error = match wrap {
                        Wrap::FunctionContext(name) => error.with_function_context(name),
                        Wrap::CallSite(span) => error.with_call_site(*span, Rc::clone(&source)),
                    };
                }
            }

            if let Some(def) = frame.def() {
                let name = lambda_name(def);
                error = error.with_function_context(&name);

                // Each tail call replaced a frame that would have added its own context: the
                // innermost of them points the error at the call, and every one of them names
                // the function again.
                let tail_calls = frame.tail_calls();
                if tail_calls > 0 {
                    if let Some(span) = proto.span_at(frame.pc_of(frame.tail_ip)) {
                        error = error.with_call_site(span, Rc::clone(&source));
                    }

                    let context = format!("in {}: ", name).repeat(tail_calls);
                    error.message.insert_str(0, &context);
                }
            }

            self.finish_profile(&frame);

            if frame.meta & ENTRY != 0 {
                return error;
            }

            error = self.at_call_site(error);
        }
    }

    // ---------------------------------------------------------------------------------------
    // Instructions that are not worth inlining into the loop
    // ---------------------------------------------------------------------------------------

    /// Apply a binary operator the instruction's fast path does not cover: broadcasting,
    /// strings, the callback operators, and every error case.
    #[inline(never)]
    fn binary_slow(&mut self, op: BinaryOp, lhs: Value, rhs: Value) -> Result<Value, RuntimeError> {
        let frame = self.top();
        let span = frame
            .proto()
            .span_at(frame.pc())
            .unwrap_or_else(Span::dummy);
        let source = self.source_of(frame);
        let depth = frame.depth();
        let heap = Rc::clone(&self.heap);

        apply_binary_op(
            op,
            lhs,
            rhs,
            &heap,
            OpSite {
                span,
                source: &source,
            },
            depth,
            &mut |def, this_value, args, depth| self.call_function(def, this_value, args, depth),
        )
    }

    /// Evaluate a comparison a fused compare-and-branch could not decide by itself.
    #[inline(never)]
    fn compare_slow(&mut self, op: BinaryOp, lhs: Value, rhs: Value) -> Result<bool, RuntimeError> {
        let value = self.binary_slow(op, lhs, rhs)?;

        Ok(value.as_bool()?)
    }

    /// Look up name `index` of the innermost frame's proto for an identifier expression, or
    /// for a record shorthand (which reports failure without a location or a hint).
    #[inline(never)]
    fn get_name(&self, index: usize, shorthand: bool) -> Result<Value, RuntimeError> {
        let name = self.top().proto().names[index];

        match self.lookup(name) {
            Some(value) => Ok(value),
            None if shorthand => Err(RuntimeError::new(format!("unknown identifier: {}", name))),
            None => Err(self.unknown_identifier(name)),
        }
    }

    /// Look up name `index` of the innermost frame's proto in the boundary environment.
    #[inline(never)]
    fn get_global(&self, index: usize) -> Result<Value, RuntimeError> {
        let name = self.top().proto().names[index];

        self.env
            .get(name)
            .ok_or_else(|| self.unknown_identifier(name))
    }

    /// Resolve slot reference `index` of the innermost frame's proto, whose capture slot
    /// turned out to be unbound: through the function's own name (unless something may
    /// shadow it), then by name.
    #[inline(never)]
    fn capture_fallback(&self, index: usize, shadowable: bool) -> Result<Value, RuntimeError> {
        let frame = self.top();
        let (_, name) = frame.proto().slot_refs[index];

        if !shadowable && frame.def().is_some_and(|def| def.name == Some(name)) {
            return Ok(frame.this_value());
        }

        self.lookup_or_unknown(name)
    }

    /// Read slot reference `index` of the innermost frame's proto from the nearest call
    /// frame of the boundary environment, falling back to a lookup by name.
    #[inline(never)]
    fn env_slot(&self, index: usize, param: bool) -> Result<Value, RuntimeError> {
        let (slot, name) = self.top().proto().slot_refs[index];
        let value = if param {
            self.env.get_param_slot(slot as usize)
        } else {
            self.env.get_capture_slot(slot as usize)
        };

        match value {
            Some(value) => Ok(value),
            None => self.lookup_or_unknown(name),
        }
    }

    #[inline(never)]
    fn input(&self, index: usize) -> Result<Value, RuntimeError> {
        let field = &self.top().proto().strings[index];
        let inputs = self
            .lookup(inputs_symbol())
            .ok_or_else(|| format!("inputs not found (required for #{} syntax)", field))?;

        let Value::Record(record) = inputs else {
            return Err(
                self.span_error(format!("inputs must be a record to use #{} syntax", field))
            );
        };

        let heap = self.heap.borrow();
        let record = record.reify(&heap).as_record()?;

        Ok(record.get(field).copied().unwrap_or(Value::Null))
    }

    #[inline(never)]
    fn factorial(&self, value: Value) -> Result<Value, RuntimeError> {
        let n = value.as_number()?;

        if n >= 0.0 && n == (n as u64) as f64 {
            return Ok(Value::Number(
                (1..(n as u64) + 1).map(|x| x as f64).product::<f64>(),
            ));
        }

        Err(self.span_error("factorial only works on non-negative integers".to_string()))
    }

    #[inline(never)]
    fn load_string(&mut self, index: usize) -> Value {
        let text = self.top().proto().strings[index].clone();

        self.heap.borrow_mut().insert_string(text)
    }

    /// Create a function value from lambda template `index` of the innermost frame's proto,
    /// capturing the current values of its free variables.
    #[inline(never)]
    fn make_closure(&mut self, index: usize) -> Result<Value, RuntimeError> {
        let frame = self.top();
        let template = &frame.proto().lambdas[index];
        let mut values = Vec::with_capacity(template.captures.len());
        let mut any_bound = false;

        for (name, source) in template.captures.iter().zip(&template.capture_sources) {
            let value = match source {
                CaptureSource::Register(reg) => {
                    // SAFETY: capture sources are registers of the creating frame.
                    let value = unsafe { *frame.base.add(*reg as usize) };
                    (!is_unbound(&value)).then_some(value)
                }
                CaptureSource::Dynamic => self.lookup(*name),
            };
            any_bound |= value.is_some();
            values.push(value);
        }

        let code = match template.proto.get() {
            Some(code) => Rc::clone(code),
            None => {
                let code =
                    compile_lambda(&template.args, &template.body, self.inlines_callbacks())?;
                let _ = template.proto.set(Rc::clone(&code));
                code
            }
        };

        // When nothing was bound, an empty scope spares every call the captures layer.
        let scope = if any_bound {
            CapturedScope::compiled(Rc::clone(&template.captures), values, code)
        } else {
            CapturedScope::unbound(code)
        };

        let lambda = LambdaDef {
            name: None,
            args: Rc::clone(&template.args),
            body: Rc::clone(&template.body),
            scope,
            source: self.source_of(frame),
        };

        Ok(self.heap.borrow_mut().insert_lambda(lambda))
    }

    /// Record `name` as the name of a function value that is being assigned. A definition
    /// that is in use keeps the name it was called with: the heap gets a renamed copy.
    #[inline(never)]
    fn name_lambda(&mut self, pointer: LambdaPointer, index: usize, fresh: bool) {
        let name = self.top().proto().names[index];
        let heap = Rc::clone(&self.heap);
        let mut heap = heap.borrow_mut();
        let Some(HeapValue::Lambda(def)) = heap.get_mut(pointer.index()) else {
            return;
        };

        if !fresh {
            let current = Rc::as_ptr(def);
            let mut running = false;

            for frame in self.active_frames_mut() {
                if ptr::eq(frame.def, current) {
                    frame.meta |= DEOPT;
                    running = true;
                }
            }

            if running {
                self.retired.push(Rc::clone(def));
            }
        }

        Rc::make_mut(def).name = Some(name);
    }

    #[inline(never)]
    fn new_list(&mut self, first: *const Value, count: usize) -> Value {
        let mut items: Vec<Value> = Vec::with_capacity(count);

        // SAFETY: the elements are `count` consecutive registers of the innermost frame.
        unsafe {
            for offset in 0..count {
                copy_value(items.as_mut_ptr().add(offset), first.add(offset));
            }
            items.set_len(count);
        }

        self.heap.borrow_mut().insert_list(items)
    }

    /// Start building a list. `hint` selects the capacity: 1 = the length of the list in
    /// `source`, 2 = the element count of an inlined range in `source` (stored as the bit
    /// pattern of a number, like all loop counters).
    #[inline(never)]
    fn list_begin(&mut self, hint: u8, source: *const Value) {
        // SAFETY: with a hint, `source` is a loop state register of the innermost frame.
        let capacity = match hint {
            1 => match unsafe { *source } {
                Value::List(pointer) => match self.heap.borrow().get(pointer.index()) {
                    Some(HeapValue::List(items)) => items.len(),
                    _ => 0,
                },
                _ => 0,
            },
            2 => match unsafe { *source } {
                Value::Number(count) => count.to_bits() as usize,
                _ => 0,
            },
            _ => 0,
        };

        self.builders
            .push(Builder::List(Vec::with_capacity(capacity)));
    }

    /// Append `count` registers starting at `first` to the list being built, flattening
    /// spread values.
    #[inline(never)]
    fn list_append(&mut self, first: *const Value, count: usize) -> Result<(), RuntimeError> {
        let Some(Builder::List(list)) = self.builders.last_mut() else {
            unreachable!("no list is being built");
        };

        for offset in 0..count {
            // SAFETY: the values are `count` consecutive registers of the innermost frame.
            match unsafe { *first.add(offset) } {
                Value::Spread(pointer) => {
                    let values = flatten_spread_value(&pointer, &self.heap)?;
                    list.extend(values.iter().copied());
                }
                value => list.push(value),
            }
        }

        Ok(())
    }

    #[inline(never)]
    fn list_end(&mut self) -> Value {
        let Some(Builder::List(list)) = self.builders.pop() else {
            unreachable!("no list is being built");
        };

        self.heap.borrow_mut().insert_list(list)
    }

    /// Build a record of layout `index` of the innermost frame's proto from the consecutive
    /// registers starting at `first`.
    #[inline(never)]
    fn new_record(&mut self, index: usize, first: *const Value) -> Value {
        let layout = &self.top().proto().layouts[index];
        let count = layout.shape.len();
        let mut values: Vec<Value> = Vec::with_capacity(count);

        // SAFETY: the values are consecutive registers of the innermost frame, one per entry
        // of the layout, and every position of the shape is the slot of at least one entry.
        unsafe {
            if layout.distinct {
                for offset in 0..count {
                    copy_value(values.as_mut_ptr().add(offset), first.add(offset));
                }
            } else {
                for (offset, slot) in layout.slots.iter().enumerate() {
                    copy_value(values.as_mut_ptr().add(*slot as usize), first.add(offset));
                }
            }
            values.set_len(count);
        }

        let record = Record::new(Rc::clone(&layout.shape), values.into_boxed_slice());

        self.heap.borrow_mut().insert_record(record)
    }

    fn record_builder(&mut self) -> (&mut RecordBuilder, &mut Option<String>) {
        match self.builders.last_mut() {
            Some(Builder::Record { entries, key }) => (entries, key),
            _ => unreachable!("no record is being built"),
        }
    }

    #[inline(never)]
    fn record_begin(&mut self) {
        self.builders.push(Builder::Record {
            entries: RecordBuilder::default(),
            key: None,
        });
    }

    #[inline(never)]
    fn record_set(&mut self, index: usize, value: Value) {
        let key = self.top().proto().strings[index].clone();
        self.record_builder().0.insert(key, value);
    }

    #[inline(never)]
    fn record_key(&mut self, value: Value) -> Result<(), RuntimeError> {
        let text = value.as_string(&self.heap.borrow())?.to_string();
        *self.record_builder().1 = Some(text);

        Ok(())
    }

    #[inline(never)]
    fn record_value(&mut self, value: Value) {
        let (entries, key) = self.record_builder();
        let key = key.take().expect("no record key was staged");
        entries.insert(key, value);
    }

    #[inline(never)]
    fn record_spread(&mut self, value: Value) -> Result<(), RuntimeError> {
        let Value::Spread(iterable) = value else {
            return Ok(());
        };

        let heap = Rc::clone(&self.heap);
        spread_into_record(&heap, self.record_builder().0, iterable)
    }

    #[inline(never)]
    fn record_end(&mut self) -> Value {
        let Some(Builder::Record { entries, .. }) = self.builders.pop() else {
            unreachable!("no record is being built");
        };

        self.heap.borrow_mut().insert_record(entries.finish())
    }

    #[inline(never)]
    fn index(&self, object: Value, index: Value) -> Result<Value, RuntimeError> {
        match object {
            Value::Record(record) => {
                let heap = self.heap.borrow();
                let record = record.reify(&heap).as_record()?;
                let key = index.as_string(&heap)?;

                Ok(record.get(key).copied().unwrap_or(Value::Null))
            }
            Value::List(list) => {
                let heap = self.heap.borrow();
                let list = list.reify(&heap).as_list()?;
                let position = element_position(index.as_number()? as i64, list.len());

                Ok(position
                    .and_then(|position| list.get(position).copied())
                    .unwrap_or(Value::Null))
            }
            Value::String(string) => {
                let string = string.reify(&self.heap.borrow()).as_string()?.to_string();
                let char_count = string.chars().count();
                let position = element_position(index.as_number()? as i64, char_count);

                Ok(position
                    .and_then(|position| string.chars().nth(position))
                    .map(|character| self.heap.borrow_mut().insert_string(character.to_string()))
                    .unwrap_or(Value::Null))
            }
            _ => Err(self.span_error(format!(
                "expected a record, list, or string, but got a {}",
                object.get_type()
            ))),
        }
    }

    /// Read the field of access site `site` of the innermost frame's proto, remembering the
    /// position the field was found at for the next read.
    #[inline(never)]
    fn field(&self, object: Value, site: usize) -> Result<Value, RuntimeError> {
        let Value::Record(record) = object else {
            return Err(self.span_error(format!(
                "expected a record, but got a {}",
                object.get_type()
            )));
        };

        let proto = self.top().proto();
        let heap = self.heap.borrow();
        let record = record.reify(&heap).as_record()?;

        let Some((position, _, value)) = record.get_full(&proto.fields[site]) else {
            return Ok(Value::Null);
        };

        let cache = &proto.field_caches[site];
        let shape = record.shape();
        cache.position.set(position as u32);
        cache.shape.set(match shape.is_interned() {
            true => Rc::as_ptr(shape),
            false => ptr::null(),
        });

        Ok(*value)
    }

    /// The spread form of a value.
    #[inline(never)]
    fn spread(&self, value: Value) -> Result<Value, RuntimeError> {
        match value {
            Value::List(pointer) => Ok(Value::Spread(IterablePointer::List(pointer))),
            Value::String(pointer) => Ok(Value::Spread(IterablePointer::String(pointer))),
            Value::Record(pointer) => Ok(Value::Spread(IterablePointer::Record(pointer))),
            _ => Err(self.span_error("expected a list, record, or string".to_string())),
        }
    }

    /// Raise the static error message `index` of the innermost frame's proto.
    #[inline(never)]
    fn raise(&self, index: usize) -> RuntimeError {
        self.span_error(self.top().proto().strings[index].clone())
    }

    /// Fail if name `index` of the innermost frame's proto is bound anywhere in scope.
    #[inline(never)]
    fn check_unbound(&self, index: usize) -> Result<(), RuntimeError> {
        let name = self.top().proto().names[index];
        if self.lookup(name).is_none() {
            return Ok(());
        }

        Err(self.span_error(format!(
            "{} is already defined, and cannot be reassigned",
            name
        )))
    }

    #[inline(never)]
    fn set_global(&self, index: usize, value: Value) {
        self.env.insert(self.top().proto().names[index], value);
    }

    /// Check that an inlined iteration over `list` may start, the way the built-in it replaces
    /// would, and locate the elements: the first one and the end of the last one.
    #[inline(never)]
    fn iter_bounds(&self, list: Value) -> Result<(*const Value, *const Value), RuntimeError> {
        if self.top().depth() > MAX_CALL_DEPTH {
            return Err(RuntimeError::new(format!(
                "maximum call depth of {} exceeded",
                MAX_CALL_DEPTH
            )));
        }

        let pointer = list.as_list_pointer()?;
        let heap = self.heap.borrow();
        let items = pointer.reify(&heap).as_list()?.as_ptr_range();

        Ok((items.start, items.end))
    }

    /// Check the arguments of an inlined `range` call the way the built-in does, returning the
    /// first element and the number of elements.
    #[inline(never)]
    fn range_bounds(&self, first: *const Value, count: usize) -> Result<(f64, u64), RuntimeError> {
        if self.top().depth() > MAX_CALL_DEPTH {
            return Err(RuntimeError::new(format!(
                "maximum call depth of {} exceeded",
                MAX_CALL_DEPTH
            )));
        }

        // SAFETY: the arguments are `count` consecutive registers of the innermost frame.
        let args = unsafe { std::slice::from_raw_parts(first, count) };
        let (start, end) = match args[..] {
            [Value::Number(start)] => (0.0, start),
            [Value::Number(start), Value::Number(end)] => (start, end),
            _ => return Err(RuntimeError::from("range requires 1 or 2 numbers")),
        };

        if start > end {
            return Err(RuntimeError::from(
                "range requires start to be less than or equal to end",
            ));
        }

        if !f64::is_finite(start) || !f64::is_finite(end) {
            return Err(RuntimeError::from("range requires finite numbers"));
        }

        let first = start as i64;
        let length = (end as i64).checked_sub(first);

        match length {
            Some(length) if length <= u32::MAX as i64 => Ok((first as f64, length as u64)),
            _ => Err(RuntimeError::new(format!(
                "list would be longer than the maximum length of {}",
                u32::MAX
            ))),
        }
    }
}
