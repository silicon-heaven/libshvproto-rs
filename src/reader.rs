use std::fmt::{Display, Formatter};
use std::io::Read;
use crate::{MetaMap, RpcValue};
use crate::rpcvalue::Value;

#[derive(Debug)]
pub enum ReadErrorReason {
    UnexpectedEndOfStream,
    InvalidCharacter,
    NumericValueOverflow,
}
#[derive(Debug)]
pub struct ReadError {
    pub msg: String,
    pub pos: usize,
    pub line: usize,
    pub col: usize,
    pub reason: ReadErrorReason,
}

impl Display for ReadError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ReadError: {}, pos {}, line: {}, col: {}", self.msg, self.pos, self.line, self.col)
    }
}

impl std::error::Error for ReadError {}

pub(crate) struct ByteReader<'a, R>
{
    read: &'a mut R,
    peeked: Option<u8> ,
    new_line_read: bool,
    pub pos: usize,
    line: usize,
    col: usize,
}

impl<'a, R> ByteReader<'a, R>
where R: Read
{
    pub(crate) fn new(read: &'a mut R) -> ByteReader<'a, R> {
        ByteReader {
            read,
            peeked: None,
            new_line_read: false,
            pos: 0,
            line: 0,
            col: 0,
        }
    }

    pub(crate) fn peek_byte_opt(&mut self) -> Result<Option<u8>, ReadError> {
        if let Some(b) = self.peeked {
            return Ok(Some(b));
        }
        let mut arr: [u8; 1] = [0];
        let r = self.read.read(&mut arr);
        match r {
            Ok(n) => {
                if n == 0 {
                    Ok(None)
                } else {
                    self.peeked = Some(arr[0]);
                    Ok(Some(arr[0]))
                }
            }
            Err(e) => Err(self.make_error(&e.to_string(), ReadErrorReason::InvalidCharacter))
        }
    }
    pub(crate) fn get_byte(&mut self) -> Result<u8, ReadError> {
        let ret_b;
        if let Some(b) = self.peeked {
            self.peeked = None;
            ret_b = b;
        } else {
            let mut arr: [u8; 1] = [0];
            let r = self.read.read(&mut arr);
            match r {
                Ok(n) => {
                    if n == 0 {
                        return Err(self.make_error("Unexpected end of stream.", ReadErrorReason::UnexpectedEndOfStream))
                    }
                    ret_b = arr[0];
                }
                Err(e) => return Err(self.make_error(&e.to_string(), ReadErrorReason::InvalidCharacter))
            }
        }
        self.pos += 1;
        if self.new_line_read {
            self.new_line_read = false;
            self.line += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
        if ret_b == b'\n' {
            self.new_line_read = true;
        }
        Ok(ret_b)
    }

    pub(crate) fn make_error(&self, msg: &str, reason: ReadErrorReason) -> ReadError {
        ReadError { msg: msg.to_string(), pos: self.pos, line: self.line, col: self.col, reason }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ContainerType {
    List,
    Map,
    IMap,
    MetaMap,
}

#[derive(Debug, Copy, Clone)]
pub enum ReadSchema {
    ContainerBegin(ContainerType),
    ContainerEnd,
    Scalar,
}

#[derive(Debug)]
pub enum MapKey {
    Int(i64),
    String(String),
}
impl MapKey {
    pub fn eq_str(&self, other: &str) -> bool {
        match self {
            MapKey::Int(key) => format!("{key}") == other,
            MapKey::String(key) => key == other,
        }
    }
}

pub type ReadResult = Result<RpcValue, ReadError>;

pub trait Reader {
    fn read(&mut self) -> ReadResult {
        let m = self.try_read_meta()?;
        let v = self.read_value()?;
        let rv = RpcValue::new(v, m);
        Ok(rv)
    }
    fn try_read_meta(&mut self) -> Result<Option<MetaMap>, ReadError>;
    fn read_value(&mut self) -> Result<Value, ReadError>;

    fn read_schema(&mut self) -> Result<ReadSchema, ReadError>;
    fn is_container_end(&mut self) -> Result<bool, ReadError>;
    fn read_key(&mut self) -> Result<MapKey, ReadError>;
    fn skip(&mut self) -> Result<(), ReadError>;

    fn find_path(&mut self, path: &[&str]) -> Result<(), ReadError> {
        if path.is_empty() {
            return Ok(());
        }

        fn make_error(msg: String) -> ReadError {
            ReadError { msg, pos: 0, line: 0, col: 0, reason: ReadErrorReason::InvalidCharacter }
        }

        for (dir_ix, &dir) in path.iter().enumerate() {
            self.try_read_meta()?;
            let schema = self.read_schema()?;
            match schema {
                ReadSchema::ContainerBegin(ContainerType::List) => {
                    let mut n = 0;
                    loop {
                        if self.is_container_end()? {
                            return Err(make_error(format!("Invalid List index '{dir}'")));
                        }

                        if format!("{n}") == dir {
                            if dir_ix == path.len() - 1 {
                                return Ok(());
                            }
                            break;
                        }
                        self.skip()?;
                        n += 1;
                    }
                }
                ReadSchema::ContainerBegin(ContainerType::Map | ContainerType::IMap) => {
                    loop {
                        if self.is_container_end()? {
                            // No more elements - index not found
                            return Err(make_error(format!("Invalid List index '{dir}'")));
                        }
                        if self.read_key()?.eq_str(dir) {
                            if dir_ix == path.len() - 1 {
                                return Ok(());
                            }
                            break;
                        }
                        self.skip()?;
                    }
                }
                _ => return Err(make_error("Not container".into()))
            }
        }
        Err(make_error("Path not found".into()))
    }

}
