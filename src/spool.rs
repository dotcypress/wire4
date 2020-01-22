use core::str::FromStr;

use crate::{hash_str, SpoolData, String};

#[derive(Debug, PartialEq)]
pub enum SpoolError {
    TooManyStrings,
    LongString,
}

#[derive(Debug, Default)]
pub struct Spool {
    data: SpoolData,
}

impl Spool {
    pub fn new() -> Spool {
        Spool {
            data: SpoolData::new(),
        }
    }

    pub fn clear(&mut self) {
        self.data.clear();
    }

    pub fn load(&self, key: u32) -> Option<&String> {
        self.data.get(&key)
    }

    pub fn store(&mut self, string: &str) -> Result<u32, SpoolError> {
        let key = hash_str(string);
        if self.data.get(&key).is_none() {
            let string = String::from_str(&string).map_err(|_| SpoolError::LongString)?;
            self.data
                .insert(key, string)
                .map_err(|_| SpoolError::TooManyStrings)?;
        }
        Ok(key)
    }
}
