use crate::spool::{Spool, SpoolError};
use crate::*;

#[derive(Debug, PartialEq)]
pub enum CompileError {
    SpoolError(SpoolError),
    CodeMemoryOverflow,
    InvalidProcName,
    MissingProcName,
    NestedDefine,
}

pub fn compile(
    input: &str,
    code: &mut Prog,
    vtable: &mut VTable,
    spool: &mut Spool,
) -> Result<(), CompileError> {
    let initial_offset = code.len();
    let mut proc_rec = Recording::Off;
    let mut vtable_rec = Recording::Off;
    let mut string_started = false;
    let mut stack_comment = false;
    let mut line_comment = false;
    let mut escape = false;
    let mut proc_key = 0;
    let mut proc_start = 0;

    let tokens = input
        .split(|ch: char| {
            if !escape && !stack_comment && !line_comment && ch.is_whitespace() {
                return !string_started;
            }
            match ch {
                '\\' if string_started => {
                    escape = true;
                    false
                }
                '\\' => {
                    line_comment = true;
                    false
                }
                '\n' => {
                    line_comment = false;
                    true
                }
                '(' => {
                    stack_comment = true;
                    false
                }
                ')' if stack_comment => {
                    stack_comment = false;
                    true
                }
                '"' if !stack_comment && !line_comment => {
                    if !escape {
                        string_started = !string_started;
                    }
                    escape = false;
                    !string_started
                }
                _ => {
                    escape = false;
                    false
                }
            }
        })
        .filter(|token| !token.is_empty());

    for token in tokens {
        if proc_rec == Recording::Pending {
            if token.parse::<i32>().is_ok() {
                let err = CompileError::InvalidProcName;
                return Err(err);
            }
            code.push(Word::Proc(hash_str(token)))
                .map_err(|_| CompileError::CodeMemoryOverflow)?;
            proc_rec = Recording::Started;
            continue;
        }
        match token {
            "." => code.push(Word::IO(IO::PrintTop)),
            ".s" => code.push(Word::IO(IO::PrintStack)),
            "if" if proc_rec == Recording::Started => code.push(Word::If),
            "then" if proc_rec == Recording::Started => code.push(Word::Then),
            "else" if proc_rec == Recording::Started => code.push(Word::Else),
            "do" if proc_rec == Recording::Started => code.push(Word::Do),
            "loop" if proc_rec == Recording::Started => code.push(Word::Loop),
            "i" if proc_rec == Recording::Started => code.push(Word::I),
            "begin" if proc_rec == Recording::Started => code.push(Word::Begin),
            "until" if proc_rec == Recording::Started => code.push(Word::Until),
            "dup" => code.push(Word::Dup),
            "drop" => code.push(Word::Drop),
            "swap" => code.push(Word::Swap),
            "over" => code.push(Word::Over),
            "nip" => code.push(Word::Nip),
            "rot" => code.push(Word::Rot),
            "-rot" => code.push(Word::RRot),
            "tuck" => code.push(Word::Tuck),
            "inc" | "+1" => code.push(Word::Inc),
            "dec" => code.push(Word::Dec),
            "+" => code.push(Word::Plus),
            "-" => code.push(Word::Minus),
            "*" => code.push(Word::Mul),
            "/" => code.push(Word::Div),
            "mod" => code.push(Word::Mod),
            "and" => code.push(Word::And),
            "or" => code.push(Word::Or),
            "xor" => code.push(Word::Xor),
            "invert" => code.push(Word::Invert),
            "<" => code.push(Word::Lt),
            ">" => code.push(Word::Gt),
            "<=" => code.push(Word::Lte),
            ">=" => code.push(Word::Gte),
            "=" => code.push(Word::Eq),
            "<>" => code.push(Word::NotEq),
            "0=" => code.push(Word::EqZero),
            "0<>" => code.push(Word::NotEqZero),
            "0<" => code.push(Word::LtZero),
            "0>" => code.push(Word::GtZero),
            "cr" => code.push(Word::IO(IO::Cr)),
            "nl" => code.push(Word::IO(IO::Nl)),
            "key" => code.push(Word::IO(IO::ReadChar)),
            "accept" => code.push(Word::IO(IO::ReadVal)),
            "emit" => code.push(Word::IO(IO::PrintChar)),
            "spaces" => code.push(Word::IO(IO::Spaces)),
            "cls" | "clear" => code.push(Word::IO(IO::Clear)),
            "bl" | "_" => code.push(Word::IO(IO::Space)),
            "!" | "store" => code.push(Word::StoreVar),
            "@" | "fetch" => code.push(Word::FetchVar),
            "?" | "test" => code.push(Word::TestVar),
            "del" => code.push(Word::DeleteVar),
            "reset" => code.push(Word::Reset),
            "wire" => code.push(Word::Wire),
            "wires" => code.push(Word::ListWires),
            "unwire" => code.push(Word::Unwire),
            "true" | "on" | "high" => code.push(Word::Num(-1)),
            "false" | "off" | "low" => code.push(Word::Num(0)),
            ":" => {
                if proc_rec == Recording::Pending {
                    return Err(CompileError::InvalidProcName);
                }
                if proc_rec == Recording::Started {
                    return Err(CompileError::NestedDefine);
                }
                proc_rec = Recording::Pending;
                Ok(())
            }
            ";" => {
                if proc_rec != Recording::Started {
                    return Err(CompileError::MissingProcName);
                }
                proc_rec = Recording::Off;
                code.push(Word::Ret)
            }
            token if token.starts_with('$') => code.push(Word::Var(hash_str(token))),
            token if token.starts_with('#') => code.push(Word::Net(hash_str(token))),
            token if token.starts_with('~') => code.push(Word::Port(hash_str(token))),
            token if !token.starts_with('(') && !token.starts_with('\\') => {
                let op = if token.starts_with('.') || token.starts_with('\"') {
                    let print_imm = token.starts_with(".\" ");
                    let mut string = String::default();
                    let mut last = ' ';
                    let skip = if print_imm { 3 } else { 1 };
                    for ch in token.chars().skip(skip) {
                        if ch != '\\' || last == '\\' {
                            string
                                .push(ch)
                                .map_err(|_| CompileError::SpoolError(SpoolError::LongString))?;
                        }
                        last = ch;
                    }
                    if print_imm {
                        let key = spool
                            .store(&string.as_str())
                            .map_err(CompileError::SpoolError)?;
                        code.push(Word::Str(key))
                            .map_err(|_| CompileError::CodeMemoryOverflow)?;
                        Word::IO(IO::PrintTop)
                    } else {
                        let key = spool
                            .store(&string.as_str().trim_start())
                            .map_err(CompileError::SpoolError)?;
                        Word::Str(key)
                    }
                } else if let Ok(num) = token.parse::<i32>() {
                    Word::Num(num)
                } else {
                    Word::Call(hash_str(token))
                };
                code.push(op)
            }
            _ => Ok(()),
        }
        .map_err(|_| CompileError::CodeMemoryOverflow)?;
    }

    for (offset, op) in code.iter().skip(initial_offset).enumerate() {
        match op {
            Word::Proc(_) if vtable_rec == Recording::Started => {
                return Err(CompileError::NestedDefine);
            }
            Word::Ret if vtable_rec != Recording::Started => {
                return Err(CompileError::InvalidProcName);
            }
            Word::Proc(proc) => {
                proc_key = *proc;
                proc_start = initial_offset + offset;
                vtable_rec = Recording::Started;
            }
            Word::Ret => {
                vtable_rec = Recording::Off;
                vtable
                    .insert(proc_key, (proc_start + 1, initial_offset + offset + 1))
                    .map_err(|_| CompileError::CodeMemoryOverflow)?;
            }
            _ => {}
        }
    }
    Ok(())
}

#[derive(Debug, PartialEq)]
enum Recording {
    Off,
    Pending,
    Started,
}
