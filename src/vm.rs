use crate::spool::Spool;
use crate::*;

#[derive(Debug)]
pub struct Wire4VM {
    pc: usize,
    code: Prog,
    stack: Stack,
    scope: ScopeStack,
    vtable: VTable,
    spool: Spool,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Cond {
    Active,
    Skip,
    Bypass,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Scope {
    Call(usize),
    Cond(Cond),
    Loop(usize),
    CondLoop(usize, i32, i32),
}

impl Default for vm::Wire4VM {
    fn default() -> Self {
        Self::new()
    }
}

impl Wire4VM {
    pub fn new() -> Wire4VM {
        Wire4VM {
            pc: 0,
            code: Prog::new(),
            stack: Stack::new(),
            vtable: VTable::new(),
            scope: ScopeStack::new(),
            spool: Spool::new(),
        }
    }

    pub fn reset(&mut self) {
        self.pc = 0;
        self.spool.clear();
        self.code.clear();
        self.stack.clear();
        self.scope.clear();
        self.vtable.clear();
    }

    pub fn load(&mut self, prog: &str) -> Result<(), VMError> {
        compile(prog, &mut self.code, &mut self.vtable, &mut self.spool)
            .map_err(VMError::CompileError)?;
        Ok(())
    }

    pub fn get_stack(&self) -> &Stack {
        &self.stack
    }

    pub fn intern_string(&mut self, s: &str) -> Result<u32, VMError> {
        self.spool.store(s).map_err(VMError::SpoolError)
    }

    pub fn get_string(&self, key: u32) -> Option<&String> {
        self.spool.load(key)
    }

    pub fn pop(&mut self) -> Result<Value, VMError> {
        self.stack.pop().ok_or(VMError::StackUnderflow)
    }

    pub fn push(&mut self, value: Value) -> Result<(), VMError> {
        self.stack.push(value).map_err(|_| VMError::StackOverflow)
    }

    pub fn spin(&mut self) -> Result<VMRequest, VMError> {
        while self.pc < self.code.len() {
            if let Some(req) = self.tick()? {
                return Ok(req);
            }
        }
        Ok(VMRequest::Idle)
    }

    #[allow(clippy::cognitive_complexity)]
    pub fn tick(&mut self) -> Result<Option<VMRequest>, VMError> {
        if self.pc >= self.code.len() {
            return Err(VMError::ProgramHalted);
        }
        let op = self.code.get(self.pc).cloned().ok_or(VMError::InvalidPtr)?;
        let scope = self.scope.get(self.scope.len() - 1).cloned();
        self.pc = self.pc.saturating_add(1);
        match (op, scope) {
            (Word::Proc(proc), _) => {
                if let Some((_, end)) = self.vtable.get(&proc) {
                    self.pc = *end;
                    return Ok(None);
                } else {
                    return Err(VMError::InvalidPtr);
                }
            }
            (Word::Call(proc), _) => {
                if let Some((addr, _)) = self.vtable.get(&proc) {
                    self.scope
                        .push(Scope::Call(self.pc))
                        .map_err(|_| VMError::StackOverflow)?;
                    self.pc = *addr;
                    return Ok(None);
                } else {
                    return Ok(Some(VMRequest::CallProc(proc)));
                }
            }
            (Word::Ret, _) => match self.scope.pop() {
                Some(Scope::Call(ra)) => {
                    self.pc = ra;
                    return Ok(None);
                }
                _ => return Err(VMError::InvalidScope),
            },
            (Word::Else, Some(Scope::Cond(Cond::Bypass))) => {
                return Ok(None);
            }
            (Word::Else, Some(Scope::Cond(Cond::Active))) => {
                self.patch_scope(Scope::Cond(Cond::Skip))?;
                return Ok(None);
            }
            (Word::Else, Some(Scope::Cond(Cond::Skip))) => {
                self.patch_scope(Scope::Cond(Cond::Active))?;
                return Ok(None);
            }
            (Word::Else, _) => return Err(VMError::InvalidScope),
            (Word::Then, Some(Scope::Cond(_))) => {
                self.scope.pop();
                return Ok(None);
            }
            (Word::Then, _) => {
                return Err(VMError::InvalidScope);
            }
            (Word::If, scope) => {
                if let Value::Num(flag) = self.pop()? {
                    let cond = match (scope, flag) {
                        (Some(Scope::Cond(Cond::Skip)), _)
                        | (Some(Scope::Cond(Cond::Bypass)), _) => Cond::Bypass,
                        (_, 0) => Cond::Skip,
                        _ => Cond::Active,
                    };
                    self.scope
                        .push(Scope::Cond(cond))
                        .map_err(|_| VMError::StackOverflow)?;
                    return Ok(None);
                }
                return Err(VMError::InvalidArguments(Word::If));
            }
            (_, Some(Scope::Cond(Cond::Skip))) | (_, Some(Scope::Cond(Cond::Bypass))) => {
                return Ok(None)
            }
            (Word::Do, _) => {
                let start = self.pop()?;
                let end = self.pop()?;
                if let (Value::Num(end), Value::Num(start)) = (end, start) {
                    if start == end {
                        return Err(VMError::InvalidArguments(Word::Do));
                    }
                    self.scope
                        .push(Scope::CondLoop(self.pc, start, end))
                        .map_err(|_| VMError::StackOverflow)?;
                    return Ok(None);
                } else {
                    return Err(VMError::InvalidArguments(Word::Do));
                }
            }
            (Word::I, scope) => {
                if let Some(Scope::CondLoop(_, i, _)) = scope {
                    self.push(Value::Num(i))?;
                    return Ok(None);
                }
                for scope in self.scope.iter().rev() {
                    match scope {
                        Scope::CondLoop(_, i, _) => {
                            #[allow(mutable_borrow_reservation_conflict)]
                            self.push(Value::Num(*i))?;
                            return Ok(None);
                        }
                        Scope::Call(_) => break,
                        _ => {}
                    }
                }
                return Err(VMError::InvalidScope);
            }
            (Word::Loop, Some(Scope::CondLoop(ra, val, end))) => {
                let next = val.saturating_add(1);
                if next < end {
                    self.patch_scope(Scope::CondLoop(ra, next, end))?;
                    self.pc = ra;
                } else {
                    self.scope.pop();
                }
                return Ok(None);
            }
            (Word::Loop, _) => return Err(VMError::InvalidScope),
            (Word::Begin, _) => {
                self.scope
                    .push(Scope::Loop(self.pc))
                    .map_err(|_| VMError::StackOverflow)?;
                return Ok(None);
            }
            (Word::Until, Some(Scope::Loop(ra))) => {
                if let Value::Num(flag) = self.pop()? {
                    if flag != 0 {
                        self.pc = ra;
                    } else {
                        self.scope.pop();
                    }
                    return Ok(None);
                }
                return Err(VMError::InvalidArguments(Word::Until));
            }
            (Word::Until, _) => return Err(VMError::InvalidScope),
            (op, _) => match op {
                Word::Drop => {
                    self.pop()?;
                }
                Word::Dup => {
                    let fst = self.pop()?;
                    self.push(fst.clone())?;
                    self.push(fst)?;
                }
                Word::Swap => {
                    let fst = self.pop()?;
                    let snd = self.pop()?;
                    self.push(fst)?;
                    self.push(snd)?;
                }
                Word::Over => {
                    let fst = self.pop()?;
                    let snd = self.pop()?;
                    self.push(snd.clone())?;
                    self.push(fst)?;
                    self.push(snd)?;
                }
                Word::Rot => {
                    let fst = self.pop()?;
                    let snd = self.pop()?;
                    let thr = self.pop()?;
                    self.push(fst)?;
                    self.push(snd)?;
                    self.push(thr)?;
                }
                Word::RRot => {
                    let fst = self.pop()?;
                    let snd = self.pop()?;
                    let thr = self.pop()?;
                    self.push(fst)?;
                    self.push(thr)?;
                    self.push(snd)?;
                }
                Word::Nip => {
                    let fst = self.pop()?;
                    self.pop()?;
                    self.push(fst)?;
                }
                Word::Tuck => {
                    let fst = self.pop()?;
                    let snd = self.pop()?;
                    self.push(fst.clone())?;
                    self.push(snd)?;
                    self.push(fst)?;
                }
                Word::Inc
                | Word::Dec
                | Word::EqZero
                | Word::LtZero
                | Word::GtZero
                | Word::NotEqZero
                | Word::Invert => {
                    if let Value::Num(num) = self.pop()? {
                        let num = match op {
                            Word::Inc => num.saturating_add(1),
                            Word::Dec => num.saturating_sub(1),
                            Word::EqZero => encode_bool(num == 0),
                            Word::LtZero => encode_bool(num < 0),
                            Word::GtZero => encode_bool(num > 0),
                            Word::NotEqZero => encode_bool(num != 0),
                            Word::Invert => encode_bool(num == 0),
                            _ => unreachable!(),
                        };
                        self.push(Value::Num(num))?;
                    } else {
                        return Err(VMError::InvalidArguments(op));
                    }
                }
                Word::Plus
                | Word::Minus
                | Word::Div
                | Word::Mul
                | Word::Mod
                | Word::And
                | Word::Or
                | Word::Xor
                | Word::Lt
                | Word::Gt
                | Word::Lte
                | Word::Gte
                | Word::Eq
                | Word::NotEq => {
                    let fst = self.pop()?;
                    let snd = self.pop()?;
                    if let (Value::Num(a), Value::Num(b)) = (snd, fst) {
                        let num = match op {
                            Word::Plus => a + b,
                            Word::Minus => a - b,
                            Word::Div => {
                                if b == 0 {
                                    return Err(VMError::DivisionByZero);
                                } else {
                                    a / b
                                }
                            }
                            Word::Mul => a * b,
                            Word::And => a & b,
                            Word::Mod => a % b,
                            Word::Or => a | b,
                            Word::Xor => a ^ b,
                            Word::Lt => encode_bool(a < b),
                            Word::Gt => encode_bool(a > b),
                            Word::Lte => encode_bool(a <= b),
                            Word::Gte => encode_bool(a >= b),
                            Word::Eq => encode_bool(a == b),
                            Word::NotEq => encode_bool(a != b),
                            _ => unreachable!(),
                        };
                        self.push(Value::Num(num))?;
                    } else {
                        return Err(VMError::InvalidArguments(op));
                    }
                }
                Word::Port(key) => self.push(Value::Port(key))?,
                Word::Net(key) => self.push(Value::Net(key))?,
                Word::Var(key) => self.push(Value::Var(key))?,
                Word::Num(num) => self.push(Value::Num(num as i32))?,
                Word::Str(key) => self.push(Value::Str(key))?,
                Word::Reset => {
                    return Ok(Some(VMRequest::Reset));
                }
                Word::Wire => {
                    return Ok(Some(VMRequest::Wire));
                }
                Word::ListWires => {
                    return Ok(Some(VMRequest::ListWires));
                }
                Word::Unwire => {
                    return Ok(Some(VMRequest::Unwire));
                }
                Word::StoreVar => {
                    if let (Value::Var(var), val) = (self.pop()?, self.pop()?) {
                        return Ok(Some(VMRequest::StoreVar(var, val)));
                    } else {
                        return Err(VMError::InvalidArguments(op));
                    }
                }
                Word::FetchVar => {
                    if let Value::Var(var) = self.pop()? {
                        #[allow(mutable_borrow_reservation_conflict)]
                        return Ok(Some(VMRequest::FetchVar(var)));
                    } else {
                        return Err(VMError::InvalidArguments(op));
                    }
                }
                Word::TestVar => {
                    if let Value::Var(var) = self.pop()? {
                        #[allow(mutable_borrow_reservation_conflict)]
                        return Ok(Some(VMRequest::TestVar(var)));
                    } else {
                        return Err(VMError::InvalidArguments(op));
                    }
                }
                Word::DeleteVar => {
                    if let Value::Var(var) = self.pop()? {
                        #[allow(mutable_borrow_reservation_conflict)]
                        return Ok(Some(VMRequest::DeleteVar(var)));
                    } else {
                        return Err(VMError::InvalidArguments(op));
                    }
                }
                Word::IO(io) => return Ok(Some(VMRequest::IO(io))),
                Word::Call(_)
                | Word::Proc(_)
                | Word::Ret
                | Word::If
                | Word::Then
                | Word::Else
                | Word::Begin
                | Word::Until
                | Word::I
                | Word::Do
                | Word::Loop => unreachable!(),
            },
        }
        Ok(None)
    }

    fn patch_scope(&mut self, scope: Scope) -> Result<(), VMError> {
        if self.scope.pop().is_none() {
            return Err(VMError::InvalidScope);
        }
        self.scope.push(scope).map_err(|_| VMError::StackOverflow)?;
        Ok(())
    }
}
