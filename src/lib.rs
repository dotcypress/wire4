#![no_std]
use hash32::{Hasher, Murmur3Hasher};
use heapless::{self, consts, LinearMap, Vec};

mod compiler;
pub mod spool;
mod vm;

pub use compiler::*;
pub use spool::*;
pub use vm::*;

pub type String = heapless::String<consts::U12>;
pub type Prog = Vec<Word, consts::U512>;
pub type VTable = LinearMap<u32, (usize, usize), consts::U32>;
pub type Stack = Vec<Value, consts::U32>;
pub type ScopeStack = Vec<Scope, consts::U16>;
pub type SpoolData = LinearMap<u32, String, consts::U32>;

#[derive(Debug, PartialEq)]
pub enum VMError {
    CompileError(CompileError),
    InvalidArguments(Word),
    SpoolError(SpoolError),
    ProgramHalted,
    DivisionByZero,
    StackOverflow,
    StackUnderflow,
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
    StoreVar(u32, Value),
    TestVar(u32),
    FetchVar(u32),
    DeleteVar(u32),
}

#[derive(Debug, PartialEq, Clone)]
pub enum IO {
    Clear,
    PrintChar,
    PrintStack,
    PrintTop,
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
    TestVar,
    StoreVar,
    FetchVar,
    DeleteVar,
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

pub fn encode_bool(val: bool) -> i32 {
    if val {
        -1
    } else {
        0
    }
}

pub fn decode_bool(val: i32) -> bool {
    val == -1
}
