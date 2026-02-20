use crate::{Decimal, Map, ReadError, Reader, Value, WriteResult, Writer};
use crate::reader::ReadErrorReason;

pub trait TextWriter : Writer {
    fn write_count(&self) -> usize;

    fn write_byte(&mut self, b: u8) -> WriteResult;
    fn write_bytes(&mut self, b: &[u8]) -> WriteResult;
    fn write_int(&mut self, n: i64) -> WriteResult {
        let s = n.to_string();
        let cnt = self.write_bytes(s.as_bytes())?;
        Ok(self.write_count() - cnt)
    }
    fn write_double(&mut self, n: f64) -> WriteResult {
        let s = n.to_string();
        let cnt = self.write_bytes(s.as_bytes())?;
        Ok(self.write_count() - cnt)
    }
    fn write_decimal(&mut self, decimal: Decimal) -> WriteResult {
        let s = decimal.to_cpon_string();
        let cnt = self.write_bytes(s.as_bytes())?;
        Ok(self.write_count() - cnt)
    }
}

pub struct ReadInt {
    pub value: i64,
    pub digit_cnt: i32,
    pub is_negative: bool,
    pub is_overflow: bool,
}
pub trait TextReader : Reader {
    fn peek_byte(&mut self) -> u8;
    fn get_byte(&mut self) -> Result<u8, ReadError>;
    fn make_error(&self, msg: &str, reason: ReadErrorReason) -> ReadError;
    fn read_string(&mut self) -> Result<Value, ReadError>;

    fn skip_white_or_insignificant(&mut self) -> Result<(), ReadError> {
        loop {
            let b = self.peek_byte();
            if b == 0 {
                break;
            }
            if b > b' ' {
                match b {
                    b'/' => {
                        self.get_byte()?;
                        let b = self.get_byte()?;
                        match b {
                            b'*' => {
                                // multiline_comment_entered
                                loop {
                                    let b = self.get_byte()?;
                                    if b == b'*' {
                                        let b = self.get_byte()?;
                                        if b == b'/' {
                                            break;
                                        }
                                    }
                                }
                            }
                            b'/' => {
                                // to end of line comment entered
                                loop {
                                    let b = self.get_byte()?;
                                    if b == b'\n' {
                                        break;
                                    }
                                }
                            }
                            _ => {
                                return Err(self.make_error("Malformed comment", ReadErrorReason::InvalidCharacter))
                            }
                        }
                    }
                    b':' => {
                        self.get_byte()?; // skip key delimiter
                    }
                    b',' => {
                        self.get_byte()?; // skip val delimiter
                    }
                    _ => {
                        break;
                    }
                }
            }
            else {
                self.get_byte()?;
            }
        }
        Ok(())
    }
    fn read_token(&mut self, token: &str) -> Result<(), ReadError> {
        for c in token.as_bytes() {
            let b = self.get_byte()?;
            if b != *c {
                return Err(self.make_error(&format!("Expected '{token}'."), ReadErrorReason::InvalidCharacter))
            }
        }
        Ok(())
    }
    fn read_true(&mut self) -> Result<Value, ReadError> {
        self.read_token("true")?;
        Ok(Value::from(true))
    }
    fn read_false(&mut self) -> Result<Value, ReadError> {
        self.read_token("false")?;
        Ok(Value::from(false))
    }
    fn read_null(&mut self) -> Result<Value, ReadError> {
        self.read_token("null")?;
        Ok(Value::from(()))
    }
    fn read_int(&mut self, init_val: i64, no_signum: bool) -> Result<ReadInt, ReadError> {
        let mut base = 10;
        let mut value: i64 = init_val;
        let mut is_negative = false;
        let mut n = 0;
        let mut digit_cnt = 0;
        let mut is_overflow = false;
        fn add_digit(val: i64, base: i64, digit: u8) -> Option<i64> {
            let res = val.checked_mul(base)?;
            let res = res.checked_add(i64::from(digit))?;
            Some(res)
        }
        loop {
            let b = self.peek_byte();
            let digit = match b {
                b'+' | b'-' => {
                    if n != 0 {
                        break;
                    }
                    if no_signum {
                        return Err(self.make_error("Unexpected signum", ReadErrorReason::InvalidCharacter))
                    }
                    let b = self.get_byte()?;
                    if b == b'-' {
                        is_negative = true;
                    }
                    None
                }
                b'x' | b'X' => {
                    if n == 1 && value != 0 {
                        break;
                    }
                    if n != 1 {
                        break;
                    }
                    self.get_byte()?;
                    base = 16;
                    None
                }
                b'0' ..= b'9' => {
                    self.get_byte()?;
                    Some(b - b'0')
                }
                b'A' ..= b'F' => {
                    if base != 16 {
                        break;
                    }
                    self.get_byte()?;
                    Some(10 + (b - b'A'))
                }
                b'a' ..= b'f' => {
                    if base != 16 {
                        break;
                    }
                    self.get_byte()?;
                    Some(10 + (b - b'a'))
                }
                _ => break,
            };
            if let Some(digit) = digit && !is_overflow {
                if let Some(val) = add_digit(value, base, digit) {
                    value = val;
                    digit_cnt += 1;
                } else {
                    is_overflow = true;
                }
            }
            n += 1;
        }
        Ok(ReadInt {
            value,
            digit_cnt,
            is_negative,
            is_overflow,
        })
    }
    fn read_number(&mut self) -> Result<Value, ReadError> {
        let mut mantissa;
        let mut exponent = 0;
        let mut dec_cnt = 0;
        let mut is_decimal = false;
        let mut is_uint = false;
        let mut is_negative = false;
        let mut decimal_overflow = false;

        let b = self.peek_byte();
        if b == b'+' {
            is_negative = false;
            self.get_byte()?;
        }
        else if b == b'-' {
            is_negative = true;
            self.get_byte()?;
        }

        let ReadInt { value, digit_cnt, is_overflow, .. } = self.read_int(0, false)?;
        decimal_overflow = decimal_overflow || is_overflow;
        if digit_cnt == 0 {
            return Err(self.make_error("Number should contain at least one digit.", ReadErrorReason::InvalidCharacter))
        }
        mantissa = value;
        #[derive(PartialEq)]
        enum State { Mantissa, Decimals,  }
        let mut state = State::Mantissa;
        loop {
            let b = self.peek_byte();
            match b {
                b'u' => {
                    is_uint = true;
                    self.get_byte()?;
                    break;
                }
                b'.' => {
                    if state != State::Mantissa {
                        return Err(self.make_error("Unexpected decimal point.", ReadErrorReason::InvalidCharacter))
                    }
                    state = State::Decimals;
                    is_decimal = true;
                    self.get_byte()?;
                    let ReadInt { value, digit_cnt, is_overflow, .. } = self.read_int(mantissa, true)?;
                    decimal_overflow = decimal_overflow || is_overflow;
                    mantissa = value;
                    if mantissa >= 0x80_0000_0000_0000 {
                        decimal_overflow = true;
                    }
                    dec_cnt = i64::from(digit_cnt);
                }
                b'e' | b'E' => {
                    if state != State::Mantissa && state != State::Decimals {
                        return Err(self.make_error("Unexpected exponent mark.", ReadErrorReason::InvalidCharacter))
                    }
                    //state = State::Exponent;
                    is_decimal = true;
                    self.get_byte()?;
                    let ReadInt { value, digit_cnt, is_negative, is_overflow } = self.read_int(0, false)?;
                    decimal_overflow = decimal_overflow || is_overflow;
                    exponent = value;
                    if is_negative { exponent = -exponent; }
                    if digit_cnt == 0 {
                        return Err(self.make_error("Malformed number exponential part.", ReadErrorReason::InvalidCharacter))
                    }
                    break;
                }
                _ => { break; }
            }
        }
        let mantissa = if is_negative { -mantissa } else { mantissa };
        if is_decimal {
            if decimal_overflow {
                return Err(self.make_error("Not enough precision to read the Decimal", ReadErrorReason::NumericValueOverflow))
            }
            #[expect(clippy::cast_possible_truncation, reason = "We hope that the new exponent is not big enough to truncate")]
            return Ok(Value::from(Decimal::new(mantissa, (exponent - dec_cnt) as i8)))
        }
        if is_uint {
            if decimal_overflow {
                return Ok(Value::from(i64::MAX as u64))
            }
            return Ok(Value::from(mantissa.cast_unsigned()))
        }
        if decimal_overflow {
            return Ok(Value::from(if is_negative { i64::MIN } else { i64::MAX }))
        }
        Ok(Value::from(mantissa))
    }
    fn read_list(&mut self) -> Result<Value, ReadError> {
        let mut lst = Vec::new();
        self.get_byte()?; // eat '['
        loop {
            self.skip_white_or_insignificant()?;
            let b = self.peek_byte();
            if b == b']' {
                self.get_byte()?;
                break;
            }
            let val = self.read()?;
            lst.push(val);
        }
        Ok(Value::from(lst))
    }

    fn read_map(&mut self) -> Result<Value, ReadError> {
        let mut map: Map = Map::new();
        self.get_byte()?; // eat '{'
        loop {
            self.skip_white_or_insignificant()?;
            let b = self.peek_byte();
            if b == b'}' {
                self.get_byte()?;
                break;
            }
            let key = self.read_string();
            let skey = match &key {
                Ok(b) => {
                    match b {
                        Value::String(s) => {
                            s
                        },
                        _ => return Err(self.make_error("Read MetaMap key internal error", ReadErrorReason::InvalidCharacter)),
                    }
                },
                _ => return Err(self.make_error(&format!("Invalid Map key '{b}'"), ReadErrorReason::InvalidCharacter)),
            };
            self.skip_white_or_insignificant()?;
            let val = self.read()?;
            map.insert(skey.to_string(), val);
        }
        Ok(Value::from(map))
    }
}
