#![no_std]
use hash32::{Hasher, Murmur3Hasher};
use heapless::{self, consts, LinearMap, Vec};

mod compiler;
pub mod spool;
mod vm;

pub use compiler::*;
pub use vm::*;

pub type Prog = Vec<Word, consts::U512>;
pub type Stack = Vec<Value, consts::U32>;
pub type ScopeStack = Vec<Scope, consts::U16>;
pub type VTable = LinearMap<u32, (usize, usize), consts::U16>;
pub type Vars = LinearMap<u32, Value, consts::U16>;
pub type SpoolData = LinearMap<u32, String, consts::U32>;
pub type String = heapless::String<consts::U12>;

#[derive(Debug, PartialEq)]
pub enum VMError {
    CompileError(CompileError),
    InvalidArguments(Word),
    ProgramHalted,
    DivisionByZero,
    StackOverflow,
    StackUnderflow,
    UnknownVar,
    InvalidScope,
    InvalidPtr,
    TooManyVars,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Num(i32),
    Str(u32),
    Var(u32),
    Net(u32),
    Port(u32),
}

#[derive(Debug, PartialEq)]
pub enum VMRequest {
    Idle,
    CallProc(u32),
    IO(IO),
}

#[derive(Debug, PartialEq, Clone)]
pub enum IO {
    Clear,
    PrintChar,
    PrintStack,
    PrintTop,
    PrintVar,
    ReadChar,
    ReadVal,
    Space,
    Spaces,
    Cr,
    Nl,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Word {
    Num(i32),
    Str(u32),
    Var(u32),
    Proc(u32),
    Call(u32),
    IO(IO),
    Net(u32),
    Port(u32),
    Ret,
    Drop,
    Dup,
    Swap,
    Over,
    Nip,
    Tuck,
    Rot,
    RRot,
    Inc,
    Dec,
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Invert,
    Lt,
    Gt,
    Lte,
    Gte,
    Eq,
    NotEq,
    EqZero,
    NotEqZero,
    LtZero,
    GtZero,
    SetVar,
    GetVar,
    If,
    Then,
    Else,
    Begin,
    Until,
    Do,
    I,
    Loop,
}

pub fn hash_str(string: &str) -> u32 {
    let mut hasher = Murmur3Hasher::default();
    hasher.write(&string.as_bytes());
    hasher.finish()
}

pub fn bool_enc(val: bool) -> i32 {
    if val {
        -1
    } else {
        0
    }
}
