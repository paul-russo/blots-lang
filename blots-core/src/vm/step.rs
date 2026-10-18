//! The complete implementation of every instruction, one instruction at a time.
//!
//! The instruction loop in [`super::run`] only covers the common case of each instruction.
//! Everything else comes through [`Vm::step`]: operands of other types (broadcasting, strings,
//! every error), calls that need more than a frame record, allocation, name lookups. Nothing
//! here assumes that the loop tried its own path first, so any instruction can be executed
//! this way.

use super::{
    Builder, CALLBACK_CALL_DEPTH, MAX_CALL_DEPTH, UNBOUND, Vm, depth_exceeded, is_unbound,
    list_next, range_next, register, set_state, store_value,
};
use crate::{
    ast::BinaryOp,
    bytecode::{Instr, Op, RECIPROCAL, RemainderTest, register_index},
    error::RuntimeError,
    values::Value,
};

/// What the instruction loop does after an instruction it handed over.
pub(super) enum Flow {
    /// Carry on with the innermost frame, whichever that is now.
    Continue,
    /// The frame the loop was entered through returned this value.
    Finished(Value),
}

/// The operator applied by a binary operator instruction, in any of its forms.
fn operator_of(op: Op) -> BinaryOp {
    match op {
        Op::Add | Op::AddK | Op::KAdd => BinaryOp::Add,
        Op::Sub | Op::SubK | Op::KSub => BinaryOp::Subtract,
        Op::Mul | Op::MulK | Op::KMul => BinaryOp::Multiply,
        Op::Div | Op::DivK | Op::KDiv => BinaryOp::Divide,
        Op::Mod | Op::ModK | Op::KMod => BinaryOp::Modulo,
        Op::Pow | Op::PowK | Op::KPow => BinaryOp::Power,
        Op::Eq | Op::EqK | Op::JmpNotEq | Op::JmpNotEqK | Op::EqNull | Op::JmpNotEqNull => {
            BinaryOp::Equal
        }
        Op::Ne | Op::NeK | Op::JmpNotNe | Op::JmpNotNeK | Op::NeNull | Op::JmpNotNeNull => {
            BinaryOp::NotEqual
        }
        Op::Lt | Op::LtK | Op::JmpNotLt | Op::JmpNotLtK => BinaryOp::Less,
        Op::Le | Op::LeK | Op::JmpNotLe | Op::JmpNotLeK => BinaryOp::LessEq,
        Op::Gt | Op::GtK | Op::JmpNotGt | Op::JmpNotGtK => BinaryOp::Greater,
        Op::Ge | Op::GeK | Op::JmpNotGe | Op::JmpNotGeK => BinaryOp::GreaterEq,
        Op::DotEq => BinaryOp::DotEqual,
        Op::DotNe => BinaryOp::DotNotEqual,
        Op::DotLt => BinaryOp::DotLess,
        Op::DotLe => BinaryOp::DotLessEq,
        Op::DotGt => BinaryOp::DotGreater,
        Op::DotGe => BinaryOp::DotGreaterEq,
        Op::And => BinaryOp::And,
        Op::NaturalAnd => BinaryOp::NaturalAnd,
        Op::Or => BinaryOp::Or,
        Op::NaturalOr => BinaryOp::NaturalOr,
        Op::Via => BinaryOp::Via,
        Op::Into => BinaryOp::Into,
        Op::Where => BinaryOp::Where,
        Op::Coalesce => BinaryOp::Coalesce,
        _ => unreachable!("not a binary operator instruction"),
    }
}

impl Vm {
    /// Execute `instr`, the instruction right before the saved `ip` of the innermost frame.
    /// An error comes back with every frame of the current `run` already abandoned.
    ///
    /// # Safety
    /// The state of the instruction loop must be published: `Vm::top` is the frame executing
    /// `instr`, and its saved `ip` points at the code word after it.
    #[cold]
    #[inline(never)]
    pub(super) unsafe fn step(&mut self, instr: Instr) -> Result<Flow, RuntimeError> {
        // SAFETY: guaranteed by the caller.
        match unsafe { self.perform(instr) } {
            Ok(flow) => Ok(flow),
            Err(error) => Err(self.unwind(error)),
        }
    }

    /// The value in the register of the innermost frame that `operand` names.
    #[inline(always)]
    fn reg(&self, operand: u16) -> Value {
        // SAFETY: the compiler only emits register operands that lie within the frame.
        unsafe { *register(self.top().base, operand) }
    }

    /// Write a register of the innermost frame, wherever the value stack is by now.
    #[inline(always)]
    fn set_reg(&mut self, operand: u16, value: Value) {
        // SAFETY: as in `reg`.
        unsafe { store_value(register(self.top().base, operand), value) }
    }

    /// The address of a register of the innermost frame. It is only good until the next call
    /// that may grow the value stack.
    #[inline(always)]
    fn reg_ptr(&self, operand: u16) -> *mut Value {
        // SAFETY: as in `reg`.
        unsafe { register(self.top().base, operand) }
    }

    /// The number operand in the code word after the instruction being executed.
    #[inline(always)]
    fn operand(&self) -> f64 {
        // SAFETY: the saved `ip` points at the operand word of such an instruction.
        f64::from_bits(unsafe { *self.top().ip })
    }

    /// Move the saved `ip` of the innermost frame by `offset` code words. This comes last in
    /// an instruction: until then the saved `ip` identifies the instruction, for error spans
    /// and for the visibility of variables.
    #[inline(always)]
    fn skip(&mut self, offset: isize) {
        // SAFETY: operand words and jump targets lie within the frame's code.
        unsafe { (*self.top).ip = (*self.top).ip.offset(offset) }
    }

    /// # Safety
    /// As for [`Vm::step`].
    unsafe fn perform(&mut self, instr: Instr) -> Result<Flow, RuntimeError> {
        let (a, b, c) = (instr.a, instr.b, instr.c);

        match instr.op {
            Op::Move => self.set_reg(a, self.reg(b)),
            Op::LoadK => {
                let value = self.top().proto().consts[instr.bx()];
                self.set_reg(a, value);
            }
            Op::LoadNum => {
                let number = self.operand();
                self.set_reg(a, Value::Number(number));
                self.skip(1);
            }
            Op::LoadStr => {
                let value = self.load_string(instr.bx());
                self.set_reg(a, value);
            }
            Op::LoadUnbound => self.set_reg(a, UNBOUND),

            Op::GetName => {
                let value = self.get_name(instr.bx(), instr.x == 1)?;
                self.set_reg(a, value);
            }
            Op::EnsureGlobal => {
                if is_unbound(&self.reg(a)) {
                    let value = self.get_global(instr.bx())?;
                    self.set_reg(a, value);
                }
            }
            Op::GetCapture => {
                let value = self.capture(b as usize, c as usize, instr.x == 1)?;
                self.set_reg(a, value);
            }
            Op::GetCaptureRef => {
                let (slot, _) = self.top().proto().slot_refs[instr.bx()];
                let value = self.capture(slot as usize, instr.bx(), instr.x == 1)?;
                self.set_reg(a, value);
            }
            Op::GetEnvParam => {
                let value = self.env_slot(instr.bx(), true)?;
                self.set_reg(a, value);
            }
            Op::GetEnvCapture => {
                let value = self.env_slot(instr.bx(), false)?;
                self.set_reg(a, value);
            }
            Op::GetInput => {
                let value = self.input(instr.bx())?;
                self.set_reg(a, value);
            }

            Op::Add
            | Op::Sub
            | Op::Mul
            | Op::Div
            | Op::Mod
            | Op::Pow
            | Op::Eq
            | Op::Ne
            | Op::Lt
            | Op::Le
            | Op::Gt
            | Op::Ge
            | Op::DotEq
            | Op::DotNe
            | Op::DotLt
            | Op::DotLe
            | Op::DotGt
            | Op::DotGe
            | Op::And
            | Op::NaturalAnd
            | Op::Or
            | Op::NaturalOr
            | Op::Via
            | Op::Into
            | Op::Where => {
                let (lhs, rhs) = (self.reg(b), self.reg(c));

                // Two strings that are added are joined right in the heap.
                if let (Op::Add, Value::String(first), Value::String(second)) = (instr.op, lhs, rhs)
                {
                    let value = self.heap.borrow_mut().join_strings(first, second)?;
                    self.set_reg(a, value);
                    return Ok(Flow::Continue);
                }

                // SAFETY: the state of the loop is published, as `broadcast` needs it.
                if (lhs.is_list() || rhs.is_list()) && unsafe { self.broadcast(instr) } {
                    return Ok(Flow::Continue);
                }

                let value = self.binary_slow(operator_of(instr.op), lhs, rhs)?;
                self.set_reg(a, value);
            }
            Op::Coalesce => {
                let (lhs, rhs) = (self.reg(b), self.reg(c));
                let value = match (lhs, rhs) {
                    (Value::List(_), _) | (_, Value::List(_)) => {
                        self.binary_slow(BinaryOp::Coalesce, lhs, rhs)?
                    }
                    (Value::Null, _) => rhs,
                    _ => lhs,
                };
                self.set_reg(a, value);
            }

            Op::AddK
            | Op::SubK
            | Op::MulK
            | Op::DivK
            | Op::ModK
            | Op::PowK
            | Op::EqK
            | Op::NeK
            | Op::LtK
            | Op::LeK
            | Op::GtK
            | Op::GeK => {
                let lhs = self.reg(b);

                // SAFETY: the state of the loop is published, as `broadcast` needs it.
                if lhs.is_list() && unsafe { self.broadcast(instr) } {
                    return Ok(Flow::Continue);
                }

                // A multiplication that stands for a division by a power of two is that
                // division for everything but numbers.
                let (operator, operand) = match instr.op {
                    Op::MulK if instr.x & RECIPROCAL != 0 => {
                        (BinaryOp::Divide, 1.0 / self.operand())
                    }
                    op => (operator_of(op), self.operand()),
                };

                let value = self.binary_slow(operator, lhs, Value::Number(operand))?;
                self.set_reg(a, value);
                self.skip(1);
            }
            Op::KAdd | Op::KSub | Op::KMul | Op::KDiv | Op::KMod | Op::KPow => {
                let rhs = self.reg(b);

                // SAFETY: the state of the loop is published, as `broadcast` needs it.
                if rhs.is_list() && unsafe { self.broadcast(instr) } {
                    return Ok(Flow::Continue);
                }

                let lhs = Value::Number(self.operand());
                let value = self.binary_slow(operator_of(instr.op), lhs, rhs)?;
                self.set_reg(a, value);
                self.skip(1);
            }

            Op::JmpNotEq
            | Op::JmpNotNe
            | Op::JmpNotLt
            | Op::JmpNotLe
            | Op::JmpNotGt
            | Op::JmpNotGe => {
                let holds = self.compare_slow(operator_of(instr.op), self.reg(b), self.reg(c))?;

                // SAFETY: the comparison is followed by the jump to take when it does not hold.
                let jump = unsafe { Instr::decode(*self.top().ip) };
                self.skip(if holds { 1 } else { 1 + jump.sbx() });
            }
            Op::JmpNotEqK
            | Op::JmpNotNeK
            | Op::JmpNotLtK
            | Op::JmpNotLeK
            | Op::JmpNotGtK
            | Op::JmpNotGeK => {
                let rhs = Value::Number(self.operand());
                let holds = self.compare_slow(operator_of(instr.op), self.reg(a), rhs)?;
                self.skip(if holds { 1 } else { instr.sbx() });
            }
            Op::JmpNotRemEq | Op::JmpNotRemNe => {
                // SAFETY: the instruction is followed by the two code words of its test.
                let test = unsafe { RemainderTest::read(self.top().ip) };
                let (divisor, remainder) = test.operands();
                let comparison = match instr.op {
                    Op::JmpNotRemEq => BinaryOp::Equal,
                    _ => BinaryOp::NotEqual,
                };

                // The remainder is taken at the instruction, and it is compared one code word
                // further on: that is where an error of each of the two is located.
                let value =
                    self.binary_slow(BinaryOp::Modulo, self.reg(a), Value::Number(divisor))?;
                self.skip(1);
                let holds = self.compare_slow(comparison, value, Value::Number(remainder))?;
                self.skip(if holds { 1 } else { instr.sbx() - 1 });
            }

            Op::EqNull | Op::NeNull => {
                let operand = self.reg(b);
                let value = match operand {
                    Value::List(_) => self.null_comparison(instr, operand)?,
                    _ => {
                        let is_null = matches!(operand, Value::Null);
                        Value::Bool(is_null == (instr.op == Op::EqNull))
                    }
                };
                self.set_reg(a, value);
            }
            Op::JmpNotEqNull | Op::JmpNotNeNull => {
                let operand = self.reg(a);
                let holds = match operand {
                    Value::List(_) => self.null_comparison(instr, operand)?.as_bool()?,
                    _ => matches!(operand, Value::Null) == (instr.op == Op::JmpNotEqNull),
                };

                if !holds {
                    self.skip(instr.sbx());
                }
            }

            Op::Neg => {
                let number = self.reg(b).as_number()?;
                self.set_reg(a, Value::Number(-number));
            }
            Op::Not => {
                let boolean = self.reg(b).as_bool()?;
                self.set_reg(a, Value::Bool(!boolean));
            }
            Op::Factorial => {
                let value = self.factorial(self.reg(b))?;
                self.set_reg(a, value);
            }

            Op::Jmp => self.skip(instr.sbx()),
            Op::JmpIfFalse => {
                if !self.reg(a).as_bool()? {
                    self.skip(instr.sbx());
                }
            }

            Op::Call => self.call_slow(register_index(a), b as usize, c)?,
            Op::CallList => {
                let argc = self.place_list_args(register_index(a));
                self.call_slow(register_index(a), argc, c)?;
            }
            Op::CallSelf => {
                // The function calls itself through its own name: the value the frame was
                // called through is the callee.
                let callee = self.top().this_value();
                self.set_reg(a, callee);
                self.call_slow(register_index(a), instr.x as usize, c)?;
            }
            Op::TailSelf | Op::TailSelf0 | Op::TailSelf1 => {
                // A tail call that cannot replace the frame is an ordinary call, whose result
                // the following instruction returns. Of its arguments, only those that were
                // waiting to be moved into their parameters are where a call wants them; the
                // parameters themselves hold the others.
                let func = register_index(c);
                let first = register_index(a) - func - 1;
                let waiting = first..first + instr.x as usize;
                let frame = *self.top();
                let argc = frame.proto().params.len();

                for index in (0..argc).filter(|index| !waiting.contains(index)) {
                    // SAFETY: the parameters and the argument registers of the call lie
                    // within the frame.
                    unsafe { *frame.base.add(func + 1 + index) = *frame.base.add(index) };
                }

                self.set_reg(c, frame.this_value());
                self.call_slow(func, argc, c)?;
            }
            Op::Return => {
                let value = self.reg(a);
                if self.finish_frame() {
                    return Ok(Flow::Finished(value));
                }

                // The result goes where the call instruction of the caller wants it.
                // SAFETY: the caller's saved `ip` points right after its call instruction.
                let target = unsafe { Instr::decode(*self.top().ip.sub(1)) }.c;
                self.set_reg(target, value);
            }

            Op::Closure => {
                let value = self.make_closure(instr.bx())?;
                self.set_reg(a, value);
            }

            Op::NewList => {
                let value = self.new_list(self.reg_ptr(b), c as usize);
                self.set_reg(a, value);
            }
            Op::NewListSpread => {
                let value = self.new_list_spread(self.reg_ptr(b), c as usize)?;
                self.set_reg(a, value);
            }
            // SAFETY: the register after a loop state's first one lies within the frame.
            Op::ListBegin => self.list_begin(instr.x, unsafe { self.reg_ptr(a).add(1) }),
            Op::ListAppend => self.list_append(self.reg_ptr(a), b as usize)?,
            Op::ListPush => {
                let value = self.reg(a);
                let Some(Builder::List(list)) = self.builders.last_mut() else {
                    unreachable!("no list is being built");
                };
                list.push(value);
            }
            Op::ListEnd => {
                let value = self.list_end();
                self.set_reg(a, value);
            }

            Op::NewRecord => {
                let value = self.new_record(c as usize, self.reg_ptr(b));
                self.set_reg(a, value);
            }
            Op::RecordBegin => self.record_begin(instr.bx()),
            Op::RecordSet => self.record_set(instr.bx(), self.reg(a)),
            Op::RecordKey => self.record_key(self.reg(a))?,
            Op::RecordValue => self.record_value(self.reg(a)),
            Op::RecordSpread => self.record_spread(self.reg(a))?,
            Op::RecordEnd => {
                let value = self.record_end();
                self.set_reg(a, value);
            }

            Op::Index => {
                let value = self.index(self.reg(b), self.reg(c))?;
                self.set_reg(a, value);
            }
            Op::GetField => {
                let site = c as usize | (instr.x as usize) << 16;
                let value = self.field(self.reg(b), site)?;
                self.set_reg(a, value);
            }
            Op::Spread => {
                let value = self.spread(self.reg(b))?;
                self.set_reg(a, value);
            }

            Op::Raise => return Err(self.raise(instr.bx())),
            Op::CheckUnbound => self.check_unbound(instr.bx())?,
            Op::NameLambda => {
                if let Value::Lambda(pointer) = self.reg(a) {
                    self.name_lambda(pointer, instr.bx(), instr.x == 1);
                }
            }
            Op::SetGlobal => self.set_global(instr.bx(), self.reg(a)),

            Op::AddDepth => {
                // SAFETY: `top` is the innermost active frame.
                let frame = unsafe { &mut *self.top };
                frame.meta = frame.meta.wrapping_add(instr.sbx() as u64);
            }
            Op::CallbackDepth => {
                if self.top().depth() + CALLBACK_CALL_DEPTH > MAX_CALL_DEPTH {
                    return Err(depth_exceeded("anonymous function"));
                }
            }

            Op::RangePrep => {
                let (first, count) = self.range_bounds(self.reg_ptr(b), c as usize)?;
                let state = self.reg_ptr(a);

                // SAFETY: the loop state registers lie within the frame.
                unsafe {
                    set_state(state, 0);
                    set_state(state.add(1), count);
                    state.add(2).write(Value::Number(first));
                }
            }
            Op::ForRange => {
                // SAFETY: the loop state was set up by `RangePrep`.
                if !unsafe { range_next(self.reg_ptr(a), instr.x == 1) } {
                    self.skip(instr.sbx());
                }
            }
            Op::ForRangeNext => {
                // SAFETY: as in `ForRange`.
                if unsafe { range_next(self.reg_ptr(a), instr.x == 1) } {
                    self.skip(instr.sbx());
                }
            }
            Op::IterPrep => {
                // SAFETY: the loop state registers lie within the frame.
                let list = unsafe { *self.reg_ptr(a).add(1) };
                let (first, end) = self.iter_bounds(list)?;
                let state = self.reg_ptr(a);

                // SAFETY: the loop state registers lie within the frame.
                unsafe {
                    set_state(state, first as u64);
                    set_state(state.add(4), end as u64);
                }
            }
            Op::ForList => {
                // SAFETY: the loop state was set up by `IterPrep`, and the list it points
                // into is kept alive by the register holding it.
                if !unsafe { list_next(self.reg_ptr(a), instr.x == 1, true) } {
                    self.skip(instr.sbx());
                }
            }
            Op::ForListNext => {
                // SAFETY: as in `ForList`.
                if unsafe { list_next(self.reg_ptr(a), instr.x == 1, false) } {
                    self.skip(instr.sbx());
                }
            }
        }

        Ok(Flow::Continue)
    }

    /// Read capture slot `slot` of the function the innermost frame is running, which is
    /// slot reference `reference` of its proto. A slot that was unbound when the closure was
    /// created is resolved late (see `Vm::capture_fallback`).
    fn capture(
        &self,
        slot: usize,
        reference: usize,
        shadowable: bool,
    ) -> Result<Value, RuntimeError> {
        let bound = self
            .top()
            .def()
            .and_then(|def| def.scope.slot_ref(slot).copied());

        match bound {
            Some(value) => Ok(value),
            None => self.capture_fallback(reference, shadowable),
        }
    }

    /// Compare the list `operand` against `null` with the (in)equality operator of `instr`,
    /// which broadcasts over the elements of the list.
    fn null_comparison(&mut self, instr: Instr, operand: Value) -> Result<Value, RuntimeError> {
        let (lhs, rhs) = match instr.x {
            1 => (Value::Null, operand),
            _ => (operand, Value::Null),
        };

        self.binary_slow(operator_of(instr.op), lhs, rhs)
    }
}
