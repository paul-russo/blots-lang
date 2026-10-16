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

use crate::{
    ast::{BinaryOp, Span},
    bytecode::{CaptureSource, Instr, Op, Proto, Wrap},
    compiler::compile_lambda,
    environment::Environment,
    error::RuntimeError,
    expressions::{OpSite, apply_binary_op, flatten_spread_value},
    functions::{BuiltInFunction, FunctionDef},
    heap::{CONSTANTS, Heap, HeapPointer, HeapValue, IterablePointer, StringPointer},
    intern::{Symbol, inputs_symbol},
    values::{CapturedScope, LambdaArg, LambdaDef, Value},
};
use indexmap::IndexMap;
use std::{cell::RefCell, rc::Rc};

/// A call made from a body that is itself nested this deep fails.
const MAX_CALL_DEPTH: usize = 1000;

/// How much deeper than its caller the callback of a higher-order built-in is called: one
/// level for the built-in, one for the callback.
const CALLBACK_CALL_DEPTH: usize = 2;

/// The smallest value stack worth allocating.
const MIN_STACK: usize = 256;

/// The content of a register whose variable has not been assigned (yet). No expression can
/// produce this value, so it never escapes into the program.
const UNBOUND: Value = Value::Spread(IterablePointer::String(StringPointer::new(usize::MAX)));

#[inline(always)]
fn is_unbound(value: Value) -> bool {
    matches!(value, Value::Spread(IterablePointer::String(pointer)) if pointer.index() == usize::MAX)
}

/// The number held by a register the compiler reserved for loop state.
#[inline(always)]
fn loop_number(value: Value) -> f64 {
    match value {
        Value::Number(number) => number,
        _ => {
            debug_assert!(false, "loop state register does not hold a number");
            f64::NAN
        }
    }
}

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

/// The position of the element a (possibly negative) index refers to.
fn element_position(index: i64, len: usize) -> Option<usize> {
    if index >= 0 {
        return Some(index as usize);
    }

    let adjusted = len as i64 + index;
    (adjusted >= 0).then_some(adjusted as usize)
}

fn lambda_name(def: &LambdaDef) -> String {
    match def.name {
        Some(name) => format!("function \"{}\"", name),
        None => String::from("anonymous function"),
    }
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
    entries: &mut IndexMap<String, Value>,
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
        entries: IndexMap<String, Value>,

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

struct Frame {
    proto: Rc<Proto>,

    /// The function being run; chunks have none.
    def: Option<Rc<LambdaDef>>,

    /// The value the function was called through, which its own name is bound to inside the
    /// body.
    this_value: Value,

    /// Index of the frame's first register in the value stack.
    base: usize,

    /// The instruction being executed. Only kept up to date while the frame is suspended in a
    /// call or when something is about to inspect the frame stack.
    pc: usize,

    /// The call depth the body runs at.
    depth: usize,

    /// Number of literals under construction when the frame was entered.
    builders: usize,

    /// The caller's register that receives the result.
    ret: u16,

    /// Whether the frame was entered from native code, which gets the result instead.
    entry: bool,

    #[cfg(not(target_arch = "wasm32"))]
    profile: Option<Box<CallProfile>>,
}

pub struct Vm {
    heap: Rc<RefCell<Heap>>,

    /// Where names that no frame binds are resolved, and where top-level assignments go.
    env: Rc<Environment>,

    stack: Vec<Value>,
    frames: Vec<Frame>,
    builders: Vec<Builder>,

    /// The source text of the chunk at the bottom of the frame stack.
    chunk_source: Option<Rc<str>>,

    #[cfg(not(target_arch = "wasm32"))]
    profiling: bool,
}

impl Vm {
    pub fn new(heap: Rc<RefCell<Heap>>, bindings: Rc<Environment>) -> Self {
        Self {
            heap,
            env: bindings,
            stack: Vec::new(),
            frames: Vec::new(),
            builders: Vec::new(),
            chunk_source: None,
            #[cfg(not(target_arch = "wasm32"))]
            profiling: crate::functions::is_profiling_enabled(),
        }
    }

    /// Whether calls to higher-order built-ins may be compiled into inline loops. Profiling
    /// reports every call, so it needs the callbacks to stay real calls.
    pub fn inlines_callbacks(&self) -> bool {
        #[cfg(not(target_arch = "wasm32"))]
        let inline = !self.profiling;
        #[cfg(target_arch = "wasm32")]
        let inline = true;

        inline
    }

    /// Run a compiled top-level expression whose spans refer to `source`.
    pub fn run_chunk(
        &mut self,
        chunk: &Rc<Proto>,
        source: Rc<str>,
        call_depth: usize,
    ) -> Result<Value, RuntimeError> {
        debug_assert!(
            self.frames.is_empty(),
            "chunks run at the bottom of the stack"
        );

        let base = self.stack_top();
        self.ensure_stack(base + chunk.max_stack);
        self.stack[base..base + chunk.cache_regs].fill(UNBOUND);
        self.chunk_source = Some(source);

        let mut frame = self.new_frame(Rc::clone(chunk), None, Value::Null, base, call_depth);
        frame.entry = true;
        self.frames.push(frame);

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

        match def {
            FunctionDef::Lambda(lambda) => {
                #[cfg(not(target_arch = "wasm32"))]
                let start = self.profiling.then(std::time::Instant::now);

                let proto = self.proto_for(lambda)?;
                let base = self.stack_top();
                self.ensure_stack(base + proto.max_stack.max(args.len()));
                self.stack[base..base + args.len()].copy_from_slice(args);
                self.bind_params(&proto, base, args.len());

                let mut frame = self.new_frame(
                    proto,
                    Some(Rc::clone(lambda)),
                    this_value,
                    base,
                    call_depth + 1,
                );
                frame.entry = true;
                #[cfg(not(target_arch = "wasm32"))]
                if let Some(start) = start {
                    frame.profile = Some(Self::call_profile(start));
                }
                self.frames.push(frame);

                self.run()
            }
            FunctionDef::BuiltIn(built_in) => self
                .run_built_in(*built_in, args.to_vec(), call_depth + 1)
                .map_err(|error| error.with_function_context(&built_in_name(*built_in))),
        }
    }

    fn new_frame(
        &self,
        proto: Rc<Proto>,
        def: Option<Rc<LambdaDef>>,
        this_value: Value,
        base: usize,
        depth: usize,
    ) -> Frame {
        Frame {
            proto,
            def,
            this_value,
            base,
            pc: 0,
            depth,
            builders: self.builders.len(),
            ret: 0,
            entry: false,
            #[cfg(not(target_arch = "wasm32"))]
            profile: None,
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn call_profile(start: std::time::Instant) -> Box<CallProfile> {
        let now = std::time::Instant::now();

        Box::new(CallProfile {
            start,
            start_var_env: start,
            end_var_env: now,
        })
    }

    /// Report a finished call to the profiler, if it was being timed.
    #[inline(always)]
    fn finish_profile(&self, frame: &Frame) {
        #[cfg(not(target_arch = "wasm32"))]
        if let (Some(profile), Some(def)) = (&frame.profile, &frame.def) {
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

    #[inline(always)]
    fn top(&self) -> &Frame {
        self.frames.last().expect("the VM has no active frame")
    }

    #[inline(always)]
    fn top_mut(&mut self) -> &mut Frame {
        self.frames.last_mut().expect("the VM has no active frame")
    }

    /// The first stack slot above every active frame.
    fn stack_top(&self) -> usize {
        self.frames
            .last()
            .map_or(0, |frame| frame.base + frame.proto.max_stack)
    }

    #[inline(always)]
    fn ensure_stack(&mut self, needed: usize) {
        if needed > self.stack.len() {
            self.grow_stack(needed);
        }
    }

    #[cold]
    fn grow_stack(&mut self, needed: usize) {
        let len = needed.max(self.stack.len() * 2).max(MIN_STACK);
        self.stack.resize(len, Value::Null);
    }

    /// The register window of the innermost frame.
    #[inline(always)]
    fn regs(&mut self) -> *mut Value {
        let base = self.top().base;

        // SAFETY: the stack covers the whole window of every active frame.
        unsafe { self.stack.as_mut_ptr().add(base) }
    }

    /// The interpreter state of the innermost frame: its proto, its code, its register window
    /// and its saved pc.
    #[inline(always)]
    fn frame_state(&mut self) -> (*const Proto, *const Instr, *mut Value, usize) {
        let frame = self.top();
        let proto = Rc::as_ptr(&frame.proto);
        let code = frame.proto.code.as_ptr();
        let (base, pc) = (frame.base, frame.pc);

        // SAFETY: the stack covers the whole window of every active frame.
        let regs = unsafe { self.stack.as_mut_ptr().add(base) };

        (proto, code, regs, pc)
    }

    fn source_of(&self, frame: &Frame) -> Rc<str> {
        match (&frame.def, &self.chunk_source) {
            (Some(def), _) => Rc::clone(&def.source),
            (None, Some(source)) => Rc::clone(source),
            (None, None) => Rc::from(""),
        }
    }

    /// An error located at the instruction `pc` of the innermost frame.
    fn span_error(&self, message: String, pc: usize) -> RuntimeError {
        let frame = self.top();

        match frame.proto.span_at(pc) {
            Some(span) => RuntimeError::with_span(message, span, self.source_of(frame)),
            None => RuntimeError::new(message),
        }
    }

    fn unknown_identifier(&self, name: Symbol, pc: usize) -> RuntimeError {
        let message = if CONSTANTS.contains_key(name.as_str()) {
            format!(
                "unknown identifier: {}. Did you mean constants.{}?",
                name, name
            )
        } else {
            format!("unknown identifier: {}", name)
        };

        self.span_error(message, pc)
    }

    /// Point an error raised by the call the innermost frame is executing at that call, unless
    /// it already points somewhere more specific.
    fn at_call_site(&self, error: RuntimeError) -> RuntimeError {
        let frame = self.top();

        match frame.proto.span_at(frame.pc) {
            Some(span) => error.with_call_site(span, self.source_of(frame)),
            None => error,
        }
    }

    /// Resolve a name through the scope chain: every active frame, innermost first, then the
    /// boundary environment. Within a frame, block variables shadow parameters, which shadow
    /// the function's own name, which shadows its captures.
    fn lookup(&self, name: Symbol) -> Option<Value> {
        for frame in self.frames.iter().rev() {
            let proto = &*frame.proto;
            let pc = frame.pc as u32;

            for local in proto.locals.iter().rev() {
                if local.name != name || pc < local.start || pc >= local.end {
                    continue;
                }

                let value = self.stack[frame.base + local.reg as usize];
                if !is_unbound(value) {
                    return Some(value);
                }
            }

            if let Some(position) = proto.params.iter().position(|param| *param == name) {
                return Some(self.stack[frame.base + position]);
            }

            let Some(def) = &frame.def else {
                continue;
            };
            if def.name == Some(name) {
                return Some(frame.this_value);
            }
            if let Some(value) = def.scope.get(name) {
                return Some(value);
            }
        }

        self.env.get(name)
    }

    /// The compiled body of a function, compiling it first if this is the first time the
    /// definition is called.
    fn proto_for(&self, def: &LambdaDef) -> Result<Rc<Proto>, RuntimeError> {
        let Some(code) = def.scope.code.get() else {
            let code = compile_lambda(&def.args, &def.body, self.inlines_callbacks())?;
            let _ = def.scope.code.set(Rc::clone(&code));

            return Ok(code);
        };

        // A definition assembled from the parts of another one can carry that one's code.
        let current = code
            .body
            .as_ref()
            .is_some_and(|body| Rc::ptr_eq(body, &def.body))
            && code
                .args
                .as_ref()
                .is_some_and(|args| Rc::ptr_eq(args, &def.args));
        if !current {
            return compile_lambda(&def.args, &def.body, self.inlines_callbacks());
        }

        Ok(Rc::clone(code))
    }

    /// Turn the `argc` arguments at `base` into the parameter registers the body expects:
    /// missing optional parameters become null and a rest parameter collects what is left.
    fn bind_params(&mut self, proto: &Proto, base: usize, argc: usize) {
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
        let base = self.top().base + a + 1;
        self.ensure_stack(base + args.len());
        self.stack[base..base + args.len()].copy_from_slice(args);
    }

    /// Call `func` with the `argc` arguments that follow register `a` of the innermost frame.
    /// A built-in runs to completion and its result is returned; for a user function a frame
    /// is pushed, to be run by the caller, and `None` is returned.
    fn call(
        &mut self,
        func: Value,
        a: usize,
        argc: usize,
        ret: u16,
    ) -> Result<Option<Value>, RuntimeError> {
        let (base, depth, pc) = {
            let frame = self.top();
            (frame.base + a + 1, frame.depth, frame.pc)
        };

        match func {
            Value::Lambda(pointer) => {
                let def = match self.heap.borrow().get(pointer.index()) {
                    Some(HeapValue::Lambda(def)) => Rc::clone(def),
                    _ => panic!("Dangling pointer!"),
                };
                let proto = self
                    .proto_for(&def)
                    .map_err(|error| self.at_call_site(error))?;

                if !(proto.simple_params && argc == proto.params.len()) {
                    FunctionDef::Lambda(Rc::clone(&def))
                        .check_arity(argc)
                        .map_err(|error| self.at_call_site(error))?;
                }

                if depth > MAX_CALL_DEPTH {
                    return Err(self.at_call_site(depth_exceeded(&lambda_name(&def))));
                }

                #[cfg(not(target_arch = "wasm32"))]
                let start = self.profiling.then(std::time::Instant::now);

                self.ensure_stack(base + proto.max_stack.max(argc));
                self.bind_params(&proto, base, argc);

                let mut frame = self.new_frame(proto, Some(def), func, base, depth + 1);
                frame.ret = ret;
                #[cfg(not(target_arch = "wasm32"))]
                if let Some(start) = start {
                    frame.profile = Some(Self::call_profile(start));
                }
                self.frames.push(frame);

                Ok(None)
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
                    Ok(value) => Ok(Some(value)),
                    Err(error) => {
                        let error = error.with_function_context(&built_in_name(built_in));
                        Err(self.at_call_site(error))
                    }
                }
            }
            _ => {
                let message = format!(
                    "can't call a non-function: {}",
                    func.stringify_internal(&self.heap.borrow())
                );
                Err(self.span_error(message, pc))
            }
        }
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

    /// Apply a binary operator the instruction's fast path does not cover: broadcasting,
    /// strings, the callback operators, and every error case.
    fn binary_slow(&mut self, op: BinaryOp, lhs: Value, rhs: Value) -> Result<Value, RuntimeError> {
        let frame = self.top();
        let span = frame.proto.span_at(frame.pc).unwrap_or_else(Span::dummy);
        let source = self.source_of(frame);
        let depth = frame.depth;
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

    /// Create a function value from lambda template `index` of the innermost frame's proto,
    /// capturing the current values of its free variables.
    fn make_closure(&mut self, index: usize) -> Result<Value, RuntimeError> {
        let frame = self.top();
        let template = &frame.proto.lambdas[index];
        let mut values = Vec::with_capacity(template.captures.len());
        let mut any_bound = false;

        for (name, source) in template.captures.iter().zip(&template.capture_sources) {
            let value = match source {
                CaptureSource::Register(reg) => {
                    let value = self.stack[frame.base + *reg as usize];
                    (!is_unbound(value)).then_some(value)
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

    fn index(&self, object: Value, index: Value, pc: usize) -> Result<Value, RuntimeError> {
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
            _ => Err(self.span_error(
                format!(
                    "expected a record, list, or string, but got a {}",
                    object.get_type()
                ),
                pc,
            )),
        }
    }

    /// Read the field of access site `site`, trying the position the field was found at last
    /// time before hashing its name.
    fn field(
        &self,
        object: Value,
        proto: &Proto,
        site: usize,
        pc: usize,
    ) -> Result<Value, RuntimeError> {
        let Value::Record(record) = object else {
            return Err(self.span_error(
                format!("expected a record, but got a {}", object.get_type()),
                pc,
            ));
        };

        let heap = self.heap.borrow();
        let record = record.reify(&heap).as_record()?;
        let name = &proto.fields[site];
        let cache = &proto.field_caches[site];

        if let Some((key, value)) = record.get_index(cache.get() as usize)
            && key == name
        {
            return Ok(*value);
        }

        let Some((position, _, value)) = record.get_full(name) else {
            return Ok(Value::Null);
        };
        cache.set(position as u32);

        Ok(*value)
    }

    fn input(&self, field: &str, pc: usize) -> Result<Value, RuntimeError> {
        let inputs = self
            .lookup(inputs_symbol())
            .ok_or_else(|| format!("inputs not found (required for #{} syntax)", field))?;

        let Value::Record(record) = inputs else {
            return Err(self.span_error(
                format!("inputs must be a record to use #{} syntax", field),
                pc,
            ));
        };

        let heap = self.heap.borrow();
        let record = record.reify(&heap).as_record()?;

        Ok(record.get(field).copied().unwrap_or(Value::Null))
    }

    fn factorial(&self, value: Value, pc: usize) -> Result<Value, RuntimeError> {
        let n = value.as_number()?;

        if n >= 0.0 && n == (n as u64) as f64 {
            return Ok(Value::Number(
                (1..(n as u64) + 1).map(|x| x as f64).product::<f64>(),
            ));
        }

        Err(self.span_error(
            "factorial only works on non-negative integers".to_string(),
            pc,
        ))
    }

    /// Check the arguments of an inlined `range` call the way the built-in does, returning the
    /// first element and the number of elements.
    fn range_bounds(&self, args: &[Value]) -> Result<(f64, f64), RuntimeError> {
        if self.top().depth > MAX_CALL_DEPTH {
            return Err(RuntimeError::new(format!(
                "maximum call depth of {} exceeded",
                MAX_CALL_DEPTH
            )));
        }

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
            Some(length) if length <= u32::MAX as i64 => Ok((first as f64, length as f64)),
            _ => Err(RuntimeError::new(format!(
                "list would be longer than the maximum length of {}",
                u32::MAX
            ))),
        }
    }

    /// The element at `position` of a list being iterated, if there is one left.
    #[inline(always)]
    fn list_item(&self, list: Value, position: usize) -> Option<Value> {
        let Value::List(pointer) = list else {
            return None;
        };

        match self.heap.borrow().get(pointer.index()) {
            Some(HeapValue::List(items)) => items.get(position).copied(),
            _ => None,
        }
    }

    /// Abandon every frame down to (and including) the one the current `run` was entered
    /// through, decorating `error` (raised by instruction `pc` of the innermost frame) with
    /// the context each level adds on the way out.
    #[cold]
    fn unwind(&mut self, mut error: RuntimeError, mut pc: usize) -> RuntimeError {
        loop {
            let frame = self.frames.pop().expect("unwinding without a frame");
            let source = self.source_of(&frame);

            // Handlers are recorded innermost first.
            for handler in &frame.proto.handlers {
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

            if let Some(def) = &frame.def {
                error = error.with_function_context(&lambda_name(def));
            }

            self.builders.truncate(frame.builders);
            self.finish_profile(&frame);

            if frame.entry {
                return error;
            }

            error = self.at_call_site(error);
            pc = self.top().pc;
        }
    }

    /// Run the innermost frame, and every frame it calls, until it returns.
    fn run(&mut self) -> Result<Value, RuntimeError> {
        let (mut proto, mut code, mut regs, mut pc) = self.frame_state();
        let mut instr: Instr;

        // SAFETY (for all of the register accesses below): the compiler only emits register
        // indices below the proto's `max_stack`, and the stack covers `max_stack` registers
        // from the base of every active frame. `regs` is re-derived whenever the frame
        // changes or the stack may have been reallocated.
        macro_rules! reg {
            ($index:expr) => {
                *unsafe { &mut *regs.add($index as usize) }
            };
        }

        // SAFETY: `proto` points into the `Rc` held by the innermost frame.
        macro_rules! proto {
            () => {
                unsafe { &*proto }
            };
        }

        macro_rules! sync {
            () => {
                self.top_mut().pc = pc - 1
            };
        }

        macro_rules! throw {
            ($error:expr) => {{
                let error = RuntimeError::from($error);
                return Err(self.unwind(error, pc - 1));
            }};
        }

        macro_rules! check {
            ($result:expr) => {
                match $result {
                    Ok(value) => value,
                    Err(error) => throw!(error),
                }
            };
        }

        macro_rules! jump {
            ($offset:expr) => {
                pc = (pc as isize + $offset) as usize
            };
        }

        macro_rules! slow {
            ($op:expr, $lhs:expr, $rhs:expr) => {{
                sync!();
                let value = check!(self.binary_slow($op, $lhs, $rhs));
                regs = self.regs();
                value
            }};
        }

        macro_rules! arithmetic {
            ($op:expr, $lhs:expr, $rhs:expr, |$x:ident, $y:ident| $fast:expr) => {{
                let (lhs, rhs): (Value, Value) = ($lhs, $rhs);
                let value = match (lhs, rhs) {
                    (Value::Number($x), Value::Number($y)) => Value::Number($fast),
                    _ => slow!($op, lhs, rhs),
                };
                reg!(instr.a) = value;
            }};
        }

        macro_rules! equality {
            ($op:expr, $lhs:expr, $rhs:expr, |$x:ident, $y:ident| $fast:expr) => {{
                let (lhs, rhs): (Value, Value) = ($lhs, $rhs);
                let value = match (lhs, rhs) {
                    (Value::Number($x), Value::Number($y)) => Value::Bool($fast),
                    _ => slow!($op, lhs, rhs),
                };
                reg!(instr.a) = value;
            }};
        }

        // Ordering comparisons involving NaN fall through to the slow path for its error.
        macro_rules! ordering {
            ($op:expr, $lhs:expr, $rhs:expr, |$x:ident, $y:ident| $fast:expr) => {{
                let (lhs, rhs): (Value, Value) = ($lhs, $rhs);
                let fast = match (lhs, rhs) {
                    (Value::Number($x), Value::Number($y)) => $fast,
                    _ => None,
                };
                let value = match fast {
                    Some(holds) => Value::Bool(holds),
                    None => slow!($op, lhs, rhs),
                };
                reg!(instr.a) = value;
            }};
        }

        macro_rules! logical {
            ($op:expr, |$x:ident, $y:ident| $fast:expr) => {{
                let (lhs, rhs) = (reg!(instr.b), reg!(instr.c));
                let value = match (lhs, rhs) {
                    (Value::Bool($x), Value::Bool($y)) => Value::Bool($fast),
                    _ => slow!($op, lhs, rhs),
                };
                reg!(instr.a) = value;
            }};
        }

        // A fused comparison is followed by the jump to take when it does not hold.
        macro_rules! branch {
            ($op:expr, $lhs:expr, $rhs:expr, |$x:ident, $y:ident| $fast:expr) => {{
                let (lhs, rhs): (Value, Value) = ($lhs, $rhs);
                let fast = match (lhs, rhs) {
                    (Value::Number($x), Value::Number($y)) => $fast,
                    _ => None,
                };
                let holds = match fast {
                    Some(holds) => holds,
                    None => {
                        let value = slow!($op, lhs, rhs);
                        check!(value.as_bool())
                    }
                };

                if holds {
                    pc += 1;
                } else {
                    // SAFETY: the compiler always emits the jump right after the comparison.
                    let target = unsafe { *code.add(pc) };
                    pc = (pc as isize + 1 + target.sbx()) as usize;
                }
            }};
        }

        macro_rules! number_k {
            ($index:expr) => {
                Value::Number(proto!().nums[$index as usize])
            };
        }

        macro_rules! finish_call {
            ($called:expr, $dst:expr) => {
                match $called {
                    Some(value) => {
                        regs = self.regs();
                        reg!($dst) = value;
                    }
                    None => (proto, code, regs, pc) = self.frame_state(),
                }
            };
        }

        loop {
            // SAFETY: every path through a proto ends in a return or an error, so `pc` never
            // runs past the end of the code.
            instr = unsafe { *code.add(pc) };
            pc += 1;

            match instr.op {
                Op::Move => reg!(instr.a) = reg!(instr.b),
                Op::LoadK => reg!(instr.a) = proto!().consts[instr.bx()],
                Op::LoadStr => {
                    let text = proto!().strings[instr.bx()].clone();
                    reg!(instr.a) = self.heap.borrow_mut().insert_string(text);
                }
                Op::LoadUnbound => reg!(instr.a) = UNBOUND,

                Op::GetName => {
                    sync!();
                    let name = proto!().names[instr.bx()];

                    match self.lookup(name) {
                        Some(value) => reg!(instr.a) = value,
                        None if instr.x == 1 => throw!(format!("unknown identifier: {}", name)),
                        None => throw!(self.unknown_identifier(name, pc - 1)),
                    }
                }
                Op::EnsureGlobal => {
                    if is_unbound(reg!(instr.a)) {
                        let name = proto!().names[instr.bx()];

                        match self.env.get(name) {
                            Some(value) => reg!(instr.a) = value,
                            None => throw!(self.unknown_identifier(name, pc - 1)),
                        }
                    }
                }
                Op::GetCapture | Op::GetCaptureDyn => {
                    let (slot, name) = proto!().slot_refs[instr.bx()];
                    let frame = self.top();
                    let captured = frame.def.as_ref().and_then(|def| {
                        def.scope.get_slot(slot as usize).or_else(|| {
                            (instr.op == Op::GetCapture && def.name == Some(name))
                                .then_some(frame.this_value)
                        })
                    });

                    match captured {
                        Some(value) => reg!(instr.a) = value,
                        None => {
                            sync!();

                            match self.lookup(name) {
                                Some(value) => reg!(instr.a) = value,
                                None => throw!(self.unknown_identifier(name, pc - 1)),
                            }
                        }
                    }
                }
                Op::GetEnvParam | Op::GetEnvCapture => {
                    let (slot, name) = proto!().slot_refs[instr.bx()];
                    let slot_value = if instr.op == Op::GetEnvParam {
                        self.env.get_param_slot(slot as usize)
                    } else {
                        self.env.get_capture_slot(slot as usize)
                    };

                    match slot_value {
                        Some(value) => reg!(instr.a) = value,
                        None => {
                            sync!();

                            match self.lookup(name) {
                                Some(value) => reg!(instr.a) = value,
                                None => throw!(self.unknown_identifier(name, pc - 1)),
                            }
                        }
                    }
                }
                Op::GetInput => {
                    sync!();
                    let value = check!(self.input(&proto!().strings[instr.bx()], pc - 1));
                    reg!(instr.a) = value;
                }

                Op::Add => {
                    arithmetic!(BinaryOp::Add, reg!(instr.b), reg!(instr.c), |x, y| x + y)
                }
                Op::Sub => {
                    arithmetic!(BinaryOp::Subtract, reg!(instr.b), reg!(instr.c), |x, y| x
                        - y)
                }
                Op::Mul => {
                    arithmetic!(BinaryOp::Multiply, reg!(instr.b), reg!(instr.c), |x, y| x
                        * y)
                }
                Op::Div => {
                    arithmetic!(BinaryOp::Divide, reg!(instr.b), reg!(instr.c), |x, y| x / y)
                }
                Op::Mod => {
                    arithmetic!(BinaryOp::Modulo, reg!(instr.b), reg!(instr.c), |x, y| x % y)
                }
                Op::Pow => {
                    arithmetic!(BinaryOp::Power, reg!(instr.b), reg!(instr.c), |x, y| x
                        .powf(y))
                }
                Op::Eq => {
                    equality!(BinaryOp::Equal, reg!(instr.b), reg!(instr.c), |x, y| x == y)
                }
                Op::Ne => {
                    equality!(BinaryOp::NotEqual, reg!(instr.b), reg!(instr.c), |x, y| x
                        != y)
                }
                Op::Lt => {
                    ordering!(BinaryOp::Less, reg!(instr.b), reg!(instr.c), |x, y| less(
                        x, y
                    ))
                }
                Op::Le => {
                    ordering!(BinaryOp::LessEq, reg!(instr.b), reg!(instr.c), |x, y| {
                        less_eq(x, y)
                    })
                }
                Op::Gt => {
                    ordering!(
                        BinaryOp::Greater,
                        reg!(instr.b),
                        reg!(instr.c),
                        |x, y| less(y, x)
                    )
                }
                Op::Ge => {
                    ordering!(BinaryOp::GreaterEq, reg!(instr.b), reg!(instr.c), |x, y| {
                        less_eq(y, x)
                    })
                }
                Op::DotEq => {
                    equality!(BinaryOp::DotEqual, reg!(instr.b), reg!(instr.c), |x, y| x
                        == y)
                }
                Op::DotNe => {
                    equality!(
                        BinaryOp::DotNotEqual,
                        reg!(instr.b),
                        reg!(instr.c),
                        |x, y| x != y
                    )
                }
                Op::DotLt => {
                    ordering!(
                        BinaryOp::DotLess,
                        reg!(instr.b),
                        reg!(instr.c),
                        |x, y| less(x, y)
                    )
                }
                Op::DotLe => {
                    ordering!(BinaryOp::DotLessEq, reg!(instr.b), reg!(instr.c), |x, y| {
                        less_eq(x, y)
                    })
                }
                Op::DotGt => {
                    ordering!(
                        BinaryOp::DotGreater,
                        reg!(instr.b),
                        reg!(instr.c),
                        |x, y| less(y, x)
                    )
                }
                Op::DotGe => {
                    ordering!(
                        BinaryOp::DotGreaterEq,
                        reg!(instr.b),
                        reg!(instr.c),
                        |x, y| less_eq(y, x)
                    )
                }
                Op::And => logical!(BinaryOp::And, |x, y| x && y),
                Op::NaturalAnd => logical!(BinaryOp::NaturalAnd, |x, y| x && y),
                Op::Or => logical!(BinaryOp::Or, |x, y| x || y),
                Op::NaturalOr => logical!(BinaryOp::NaturalOr, |x, y| x || y),
                Op::Via => {
                    let value = slow!(BinaryOp::Via, reg!(instr.b), reg!(instr.c));
                    reg!(instr.a) = value;
                }
                Op::Into => {
                    let value = slow!(BinaryOp::Into, reg!(instr.b), reg!(instr.c));
                    reg!(instr.a) = value;
                }
                Op::Where => {
                    let value = slow!(BinaryOp::Where, reg!(instr.b), reg!(instr.c));
                    reg!(instr.a) = value;
                }
                Op::Coalesce => {
                    let (lhs, rhs) = (reg!(instr.b), reg!(instr.c));
                    let value = match (lhs, rhs) {
                        (Value::List(_), _) | (_, Value::List(_)) => {
                            slow!(BinaryOp::Coalesce, lhs, rhs)
                        }
                        (Value::Null, _) => rhs,
                        _ => lhs,
                    };
                    reg!(instr.a) = value;
                }

                Op::AddK => {
                    arithmetic!(BinaryOp::Add, reg!(instr.b), number_k!(instr.c), |x, y| x
                        + y)
                }
                Op::SubK => {
                    arithmetic!(
                        BinaryOp::Subtract,
                        reg!(instr.b),
                        number_k!(instr.c),
                        |x, y| x - y
                    )
                }
                Op::MulK => {
                    arithmetic!(
                        BinaryOp::Multiply,
                        reg!(instr.b),
                        number_k!(instr.c),
                        |x, y| x * y
                    )
                }
                Op::DivK => {
                    arithmetic!(
                        BinaryOp::Divide,
                        reg!(instr.b),
                        number_k!(instr.c),
                        |x, y| x / y
                    )
                }
                Op::ModK => {
                    arithmetic!(
                        BinaryOp::Modulo,
                        reg!(instr.b),
                        number_k!(instr.c),
                        |x, y| x % y
                    )
                }
                Op::PowK => {
                    arithmetic!(
                        BinaryOp::Power,
                        reg!(instr.b),
                        number_k!(instr.c),
                        |x, y| x.powf(y)
                    )
                }
                Op::EqK => {
                    equality!(
                        BinaryOp::Equal,
                        reg!(instr.b),
                        number_k!(instr.c),
                        |x, y| x == y
                    )
                }
                Op::NeK => {
                    equality!(
                        BinaryOp::NotEqual,
                        reg!(instr.b),
                        number_k!(instr.c),
                        |x, y| x != y
                    )
                }
                Op::LtK => {
                    ordering!(BinaryOp::Less, reg!(instr.b), number_k!(instr.c), |x, y| {
                        less(x, y)
                    })
                }
                Op::LeK => {
                    ordering!(
                        BinaryOp::LessEq,
                        reg!(instr.b),
                        number_k!(instr.c),
                        |x, y| less_eq(x, y)
                    )
                }
                Op::GtK => {
                    ordering!(
                        BinaryOp::Greater,
                        reg!(instr.b),
                        number_k!(instr.c),
                        |x, y| less(y, x)
                    )
                }
                Op::GeK => {
                    ordering!(
                        BinaryOp::GreaterEq,
                        reg!(instr.b),
                        number_k!(instr.c),
                        |x, y| { less_eq(y, x) }
                    )
                }

                Op::KAdd => {
                    arithmetic!(BinaryOp::Add, number_k!(instr.b), reg!(instr.c), |x, y| x
                        + y)
                }
                Op::KSub => {
                    arithmetic!(
                        BinaryOp::Subtract,
                        number_k!(instr.b),
                        reg!(instr.c),
                        |x, y| x - y
                    )
                }
                Op::KMul => {
                    arithmetic!(
                        BinaryOp::Multiply,
                        number_k!(instr.b),
                        reg!(instr.c),
                        |x, y| x * y
                    )
                }
                Op::KDiv => {
                    arithmetic!(
                        BinaryOp::Divide,
                        number_k!(instr.b),
                        reg!(instr.c),
                        |x, y| x / y
                    )
                }
                Op::KMod => {
                    arithmetic!(
                        BinaryOp::Modulo,
                        number_k!(instr.b),
                        reg!(instr.c),
                        |x, y| x % y
                    )
                }
                Op::KPow => {
                    arithmetic!(
                        BinaryOp::Power,
                        number_k!(instr.b),
                        reg!(instr.c),
                        |x, y| x.powf(y)
                    )
                }

                Op::JmpNotEq => {
                    branch!(BinaryOp::Equal, reg!(instr.b), reg!(instr.c), |x, y| Some(
                        x == y
                    ))
                }
                Op::JmpNotNe => {
                    branch!(BinaryOp::NotEqual, reg!(instr.b), reg!(instr.c), |x, y| {
                        Some(x != y)
                    })
                }
                Op::JmpNotLt => {
                    branch!(BinaryOp::Less, reg!(instr.b), reg!(instr.c), |x, y| less(
                        x, y
                    ))
                }
                Op::JmpNotLe => {
                    branch!(BinaryOp::LessEq, reg!(instr.b), reg!(instr.c), |x, y| {
                        less_eq(x, y)
                    })
                }
                Op::JmpNotGt => {
                    branch!(
                        BinaryOp::Greater,
                        reg!(instr.b),
                        reg!(instr.c),
                        |x, y| less(y, x)
                    )
                }
                Op::JmpNotGe => {
                    branch!(BinaryOp::GreaterEq, reg!(instr.b), reg!(instr.c), |x, y| {
                        less_eq(y, x)
                    })
                }
                Op::JmpNotEqK => {
                    branch!(
                        BinaryOp::Equal,
                        reg!(instr.b),
                        number_k!(instr.c),
                        |x, y| Some(x == y)
                    )
                }
                Op::JmpNotNeK => {
                    branch!(
                        BinaryOp::NotEqual,
                        reg!(instr.b),
                        number_k!(instr.c),
                        |x, y| Some(x != y)
                    )
                }
                Op::JmpNotLtK => {
                    branch!(BinaryOp::Less, reg!(instr.b), number_k!(instr.c), |x, y| {
                        less(x, y)
                    })
                }
                Op::JmpNotLeK => {
                    branch!(
                        BinaryOp::LessEq,
                        reg!(instr.b),
                        number_k!(instr.c),
                        |x, y| less_eq(x, y)
                    )
                }
                Op::JmpNotGtK => {
                    branch!(
                        BinaryOp::Greater,
                        reg!(instr.b),
                        number_k!(instr.c),
                        |x, y| less(y, x)
                    )
                }
                Op::JmpNotGeK => {
                    branch!(
                        BinaryOp::GreaterEq,
                        reg!(instr.b),
                        number_k!(instr.c),
                        |x, y| less_eq(y, x)
                    )
                }

                Op::Neg => match reg!(instr.b) {
                    Value::Number(number) => reg!(instr.a) = Value::Number(-number),
                    other => throw!(other.as_number().unwrap_err()),
                },
                Op::Not => match reg!(instr.b) {
                    Value::Bool(boolean) => reg!(instr.a) = Value::Bool(!boolean),
                    other => throw!(other.as_bool().unwrap_err()),
                },
                Op::Factorial => {
                    let value = check!(self.factorial(reg!(instr.b), pc - 1));
                    reg!(instr.a) = value;
                }

                Op::Jmp => jump!(instr.sbx()),
                Op::JmpIfFalse => match reg!(instr.a) {
                    Value::Bool(true) => {}
                    Value::Bool(false) => jump!(instr.sbx()),
                    other => throw!(other.as_bool().unwrap_err()),
                },

                Op::Call => {
                    let a = instr.a as usize;
                    let mut argc = instr.b as usize;
                    sync!();

                    let spread = instr.x == 0
                        && (1..=argc).any(|offset| matches!(reg!(a + offset), Value::Spread(_)));
                    if spread {
                        let mut args = Vec::with_capacity(argc);

                        for offset in 1..=argc {
                            match reg!(a + offset) {
                                Value::Spread(pointer) => {
                                    let values = check!(flatten_spread_value(&pointer, &self.heap));
                                    args.extend(values.iter().copied());
                                }
                                value => args.push(value),
                            }
                        }

                        self.place_args(a, &args);
                        regs = self.regs();
                        argc = args.len();
                    }

                    let called = check!(self.call(reg!(a), a, argc, instr.c));
                    finish_call!(called, instr.c);
                }
                Op::CallList => {
                    let a = instr.a as usize;
                    sync!();

                    let Some(Builder::List(args)) = self.builders.pop() else {
                        unreachable!("no argument list is being built");
                    };
                    self.place_args(a, &args);
                    regs = self.regs();

                    let called = check!(self.call(reg!(a), a, args.len(), instr.c));
                    finish_call!(called, instr.c);
                }
                Op::Return => {
                    let value = reg!(instr.a);
                    let frame = self.frames.pop().expect("returning without a frame");
                    self.finish_profile(&frame);

                    if frame.entry {
                        return Ok(value);
                    }

                    (proto, code, regs, pc) = self.frame_state();
                    pc += 1;
                    reg!(frame.ret) = value;
                }

                Op::Closure => {
                    sync!();
                    let value = check!(self.make_closure(instr.bx()));
                    reg!(instr.a) = value;
                }

                Op::NewList => {
                    // SAFETY: the elements are `c` consecutive registers of this frame.
                    let items = unsafe {
                        std::slice::from_raw_parts(regs.add(instr.b as usize), instr.c as usize)
                    };
                    reg!(instr.a) = self.heap.borrow_mut().insert_list(items.to_vec());
                }
                Op::ListBegin => {
                    let capacity = match instr.x {
                        1 => match reg!(instr.a + 1) {
                            Value::List(pointer) => match self.heap.borrow().get(pointer.index()) {
                                Some(HeapValue::List(items)) => items.len(),
                                _ => 0,
                            },
                            _ => 0,
                        },
                        2 => loop_number(reg!(instr.a + 1)) as usize,
                        _ => 0,
                    };
                    self.builders
                        .push(Builder::List(Vec::with_capacity(capacity)));
                }
                Op::ListAppend => {
                    let Some(Builder::List(list)) = self.builders.last_mut() else {
                        unreachable!("no list is being built");
                    };

                    for offset in 0..instr.b {
                        match reg!(instr.a + offset) {
                            Value::Spread(pointer) => {
                                match flatten_spread_value(&pointer, &self.heap) {
                                    Ok(values) => list.extend(values.iter().copied()),
                                    Err(error) => throw!(error),
                                }
                            }
                            value => list.push(value),
                        }
                    }
                }
                Op::ListPush => {
                    let Some(Builder::List(list)) = self.builders.last_mut() else {
                        unreachable!("no list is being built");
                    };
                    list.push(reg!(instr.a));
                }
                Op::ListEnd => {
                    let Some(Builder::List(list)) = self.builders.pop() else {
                        unreachable!("no list is being built");
                    };
                    reg!(instr.a) = self.heap.borrow_mut().insert_list(list);
                }

                Op::NewRecord => {
                    let shape = &proto!().shapes[instr.c as usize];
                    let mut entries = shape.template.clone();

                    for (offset, slot) in shape.slots.iter().enumerate() {
                        entries[*slot as usize] = reg!(instr.b as usize + offset);
                    }

                    reg!(instr.a) = self.heap.borrow_mut().insert_record(entries);
                }
                Op::RecordBegin => self.builders.push(Builder::Record {
                    entries: IndexMap::new(),
                    key: None,
                }),
                Op::RecordSet => {
                    let Some(Builder::Record { entries, .. }) = self.builders.last_mut() else {
                        unreachable!("no record is being built");
                    };
                    entries.insert(proto!().strings[instr.bx()].clone(), reg!(instr.a));
                }
                Op::RecordKey => {
                    let text = reg!(instr.a)
                        .as_string(&self.heap.borrow())
                        .map(str::to_string);
                    let text = check!(text);

                    let Some(Builder::Record { key, .. }) = self.builders.last_mut() else {
                        unreachable!("no record is being built");
                    };
                    *key = Some(text);
                }
                Op::RecordValue => {
                    let Some(Builder::Record { entries, key }) = self.builders.last_mut() else {
                        unreachable!("no record is being built");
                    };
                    let key = key.take().expect("no record key was staged");
                    entries.insert(key, reg!(instr.a));
                }
                Op::RecordSpread => {
                    let Some(Builder::Record { entries, .. }) = self.builders.last_mut() else {
                        unreachable!("no record is being built");
                    };

                    if let Value::Spread(iterable) = reg!(instr.a)
                        && let Err(error) = spread_into_record(&self.heap, entries, iterable)
                    {
                        throw!(error);
                    }
                }
                Op::RecordEnd => {
                    let Some(Builder::Record { entries, .. }) = self.builders.pop() else {
                        unreachable!("no record is being built");
                    };
                    reg!(instr.a) = self.heap.borrow_mut().insert_record(entries);
                }

                Op::Index => {
                    let value = check!(self.index(reg!(instr.b), reg!(instr.c), pc - 1));
                    reg!(instr.a) = value;
                }
                Op::GetField => {
                    let site = instr.c as usize | (instr.x as usize) << 16;
                    let value = check!(self.field(reg!(instr.b), proto!(), site, pc - 1));
                    reg!(instr.a) = value;
                }
                Op::Spread => {
                    reg!(instr.a) = match reg!(instr.b) {
                        Value::List(pointer) => Value::Spread(IterablePointer::List(pointer)),
                        Value::String(pointer) => Value::Spread(IterablePointer::String(pointer)),
                        Value::Record(pointer) => Value::Spread(IterablePointer::Record(pointer)),
                        _ => {
                            throw!(self.span_error(
                                "expected a list, record, or string".to_string(),
                                pc - 1
                            ))
                        }
                    };
                }

                Op::Raise => {
                    let message = proto!().strings[instr.bx()].clone();
                    throw!(self.span_error(message, pc - 1));
                }
                Op::CheckUnbound => {
                    sync!();
                    let name = proto!().names[instr.bx()];

                    if self.lookup(name).is_some() {
                        throw!(self.span_error(
                            format!("{} is already defined, and cannot be reassigned", name),
                            pc - 1
                        ));
                    }
                }
                Op::NameLambda => {
                    if let Value::Lambda(pointer) = reg!(instr.a) {
                        let name = proto!().names[instr.bx()];
                        let mut heap = self.heap.borrow_mut();

                        // A function that is running right now keeps the definition (and name)
                        // it was called with: `make_mut` leaves shared definitions untouched.
                        if let Some(HeapValue::Lambda(def)) = heap.get_mut(pointer.index()) {
                            Rc::make_mut(def).name = Some(name);
                        }
                    }
                }
                Op::SetGlobal => self.env.insert(proto!().names[instr.bx()], reg!(instr.a)),

                Op::AddDepth => {
                    let frame = self.top_mut();
                    frame.depth = (frame.depth as isize + instr.sbx()) as usize;
                }

                Op::RangePrep => {
                    // SAFETY: the arguments are `c` consecutive registers of this frame.
                    let args = unsafe {
                        std::slice::from_raw_parts(regs.add(instr.b as usize), instr.c as usize)
                    };
                    let (first, count) = check!(self.range_bounds(args));

                    reg!(instr.a) = Value::Number(0.0);
                    reg!(instr.a + 1) = Value::Number(count);
                    reg!(instr.a + 2) = Value::Number(first);
                }
                Op::ForRange | Op::ForRangeNext => {
                    let a = instr.a as usize;
                    let next = loop_number(reg!(a));
                    let has_next = next < loop_number(reg!(a + 1));

                    if has_next {
                        reg!(a + 3) = Value::Number(loop_number(reg!(a + 2)) + next);
                        if instr.x == 1 {
                            reg!(a + 4) = Value::Number(next);
                        }
                        reg!(a) = Value::Number(next + 1.0);
                    }

                    if has_next == (instr.op == Op::ForRangeNext) {
                        jump!(instr.sbx());
                    }
                }
                Op::IterPrep => {
                    if self.top().depth > MAX_CALL_DEPTH {
                        throw!(format!("maximum call depth of {} exceeded", MAX_CALL_DEPTH));
                    }

                    if let Err(error) = reg!(instr.a + 1).as_list_pointer() {
                        throw!(error);
                    }

                    reg!(instr.a) = Value::Number(0.0);
                }
                Op::ForList | Op::ForListNext => {
                    let a = instr.a as usize;
                    let position = loop_number(reg!(a));
                    let item = self.list_item(reg!(a + 1), position as usize);

                    if let Some(item) = item {
                        reg!(a + 2) = item;
                        if instr.x == 1 {
                            reg!(a + 3) = Value::Number(position);
                        }
                        reg!(a) = Value::Number(position + 1.0);
                    }

                    if item.is_some() == (instr.op == Op::ForListNext) {
                        jump!(instr.sbx());
                    }
                }
                Op::CallbackDepth => {
                    if self.top().depth + CALLBACK_CALL_DEPTH > MAX_CALL_DEPTH {
                        throw!(depth_exceeded("anonymous function"));
                    }
                }
            }
        }
    }
}
