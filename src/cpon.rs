use crate::textrdwr::{ReadInt, TextWriter};
use std::io::{Write, Read};
use crate::{RpcValue, MetaMap, Value, DateTime};
use std::collections::BTreeMap;
use crate::datetime::{IncludeMilliseconds, ToISOStringOptions};
use crate::writer::{WriteResult, Writer, ByteWriter};
use crate::metamap::MetaKey;
use crate::reader::{Reader, ByteReader, ReadError, ReadErrorReason};
use crate::rpcvalue::{Map};
use crate::textrdwr::{TextReader};

pub struct CponWriter<'a, W>
    where W: Write
{
    byte_writer: ByteWriter<'a, W>,
    indent: Vec<u8>,
    no_oneliners: bool,
    nest_count: usize,
}

impl<'a, W> CponWriter<'a, W>
    where W: Write
{
    pub fn new(write: &'a mut W) -> Self {
        CponWriter {
            byte_writer: ByteWriter::new(write),
            indent: "".as_bytes().to_vec(),
            no_oneliners: false,
            nest_count: 0,
        }
    }
    pub fn set_indent(&mut self, indent: &[u8]) {
        self.indent = indent.to_vec();
    }
    pub fn set_no_oneliners(&mut self, b: bool) {
        self.no_oneliners = b;
    }

    fn is_oneliner_list(&self, lst: &[RpcValue]) -> bool {
        if self.no_oneliners || lst.len() > 10 {
            return false;
        }
        for it in lst.iter() {
            if matches!(&it.value, Value::List(_) | Value::Map(_) | Value::IMap(_)) {
                return false;
            }
        }
        true
    }
    fn is_oneliner_map<K>(&self, iter: &mut dyn Iterator<Item = (K, &RpcValue)>) -> bool {
        if self.no_oneliners {
            return false;
        }
        let mut n = 0;
        loop {
            if n == 5 {
                return false
            }
            match iter.next() {
                Some(val) => {
                    match &val.1.value {
                        Value::List(_) | Value::Map(_) | Value::IMap(_) => return false,
                        _ => (),
                    }
                },
                None => break,
            }
            n += 1;
        };
        true
    }

    fn is_oneliner_meta(&self, map: &MetaMap) -> bool {
        if self.no_oneliners || map.0.len() > 5 {
            return false;
        }
        for k in map.0.iter() {
            if matches!(&k.value.value, Value::List(_) | Value::Map(_) | Value::IMap(_)) {
                return false;
            }
        }
        true
    }

    fn start_block(&mut self) {
        self.nest_count += 1;
    }
    fn end_block(&mut self, is_oneliner: bool) -> WriteResult {
        let cnt = self.byte_writer.count();
        self.nest_count -= 1;
        if !self.indent.is_empty() {
            self.indent_element(is_oneliner, true)?;
        }
        Ok(self.byte_writer.count() - cnt)
    }
    fn indent_element(&mut self, is_oneliner: bool, is_first_field: bool) -> WriteResult {
        let cnt = self.byte_writer.count();
        if !self.indent.is_empty() {
            if is_oneliner {
                if !is_first_field {
                    self.write_byte(b' ')?;
                }
            } else {
                self.write_byte(b'\n')?;
                for _ in 0 .. self.nest_count {
                    self.byte_writer.write_bytes(&self.indent)?;
                }
            }
        }
        Ok(self.byte_writer.count() - cnt)
    }

    fn write_uint(&mut self, n: u64) -> WriteResult {
        let s = n.to_string();
        let cnt = self.write_bytes(s.as_bytes())?;
                Ok(self.byte_writer.count() - cnt)
    }
    fn write_string(&mut self, s: &str) -> WriteResult {
        let cnt = self.byte_writer.count();
        self.write_byte(b'"')?;
        //let bytes = s.as_bytes();
        for c in s.chars() {
            match c {
                '\0' => {
                    self.write_byte(b'\\')?;
                    self.write_byte(b'0')?;
                }
                '\\' => {
                    self.write_byte(b'\\')?;
                    self.write_byte(b'\\')?;
                }
                '\t' => {
                    self.write_byte(b'\\')?;
                    self.write_byte(b't')?;
                }
                '\r' => {
                    self.write_byte(b'\\')?;
                    self.write_byte(b'r')?;
                }
                '\n' => {
                    self.write_byte(b'\\')?;
                    self.write_byte(b'n')?;
                }
                '"' => {
                    self.write_byte(b'\\')?;
                    self.write_byte(b'"')?;
                }
                _ => {
                    let mut b = [0; 4];
                    let bytes = c.encode_utf8(&mut b);
                    self.write_bytes(bytes.as_bytes())?;
                }
            }
        }
        self.write_byte(b'"')?;
        Ok(self.byte_writer.count() - cnt)
    }
    fn write_blob(&mut self, bytes: &[u8]) -> WriteResult {
        let cnt = self.byte_writer.count();
        self.write_bytes(b"b\"")?;
        for b in bytes {
            match b {
                b'\\' => {
                    self.write_byte(b'\\')?;
                    self.write_byte(b'\\')?;
                }
                b'\t' => {
                    self.write_byte(b'\\')?;
                    self.write_byte(b't')?;
                }
                b'\r' => {
                    self.write_byte(b'\\')?;
                    self.write_byte(b'r')?;
                }
                b'\n' => {
                    self.write_byte(b'\\')?;
                    self.write_byte(b'n')?;
                }
                b'"' => {
                    self.write_byte(b'\\')?;
                    self.write_byte(b'"')?;
                }
                _ => {
                    if *b < 0x20 || *b >= 0x7f {
                        self.write_byte(b'\\')?;
                        let (high, low) = crate::u8_to_hex(*b);
                        self.write_byte(high)?;
                        self.write_byte(low)?;
                    } else {
                        self.write_byte(*b)?;
                    }
                }
            }
        }
        self.write_byte(b'"')?;
        Ok(self.byte_writer.count() - cnt)
    }
    fn write_datetime(&mut self, dt: DateTime) -> WriteResult {
        let cnt = self.write_bytes("d\"".as_bytes())?;
        let s = dt.to_iso_string_opt(&ToISOStringOptions {
            include_millis: IncludeMilliseconds::WhenNonZero,
            include_timezone: true
        });
        self.write_bytes(s.as_bytes())?;
        self.write_byte(b'"')?;
        Ok(self.byte_writer.count() - cnt)
    }
    fn write_list(&mut self, lst: &[RpcValue]) -> WriteResult {
        let cnt = self.byte_writer.count();
        let is_oneliner = self.is_oneliner_list(lst);
        self.write_byte(b'[')?;
        self.start_block();
        for (n, v) in lst.iter().enumerate() {
            if n > 0 {
                self.write_byte(b',')?;
            }
            self.indent_element(is_oneliner, n == 0)?;
            self.write(v)?;
        }
        self.end_block(is_oneliner)?;
        self.write_byte(b']')?;
        Ok(self.byte_writer.count() - cnt)
    }
    fn write_map(&mut self, map: &Map) -> WriteResult {
        let cnt = self.byte_writer.count();
        let is_oneliner = self.is_oneliner_map(&mut map.iter());
        self.write_byte(b'{')?;
        self.start_block();
        for (n, (k, v)) in map.iter().enumerate() {
            if n > 0 {
                self.write_byte(b',')?;
            }
            self.indent_element(is_oneliner, n == 0)?;
            self.write_string(k)?;
            self.write_byte(b':')?;
            self.write(v)?;
        }
        self.end_block(is_oneliner)?;
        self.write_byte(b'}')?;
        Ok(self.byte_writer.count() - cnt)
    }
    fn write_imap(&mut self, map: &BTreeMap<i32, RpcValue>) -> WriteResult {
        let cnt = self.byte_writer.count();
        let is_oneliner = self.is_oneliner_map(&mut map.iter());
        self.write_byte(b'i')?;
        self.write_byte(b'{')?;
        self.start_block();
        for (n, (k, v)) in map.iter().enumerate() {
            if n > 0 {
                self.write_byte(b',')?;
            }
            self.indent_element(is_oneliner, n == 0)?;
            self.write_int(i64::from(*k))?;
            self.write_byte(b':')?;
            self.write(v)?;
        }
        self.end_block(is_oneliner)?;
        self.write_byte(b'}')?;
        Ok(self.byte_writer.count() - cnt)
    }
}
impl<W> TextWriter for CponWriter<'_, W>
where W: Write
{
    fn write_count(&self) -> usize {
        self.byte_writer.count()
    }

    fn write_byte(&mut self, b: u8) -> WriteResult {
        self.byte_writer.write_byte(b)
    }
    fn write_bytes(&mut self, b: &[u8]) -> WriteResult {
        self.byte_writer.write_bytes(b)
    }
}

impl<W> Writer for CponWriter<'_, W>
    where W: Write
{
    fn write(&mut self, val: &RpcValue) -> WriteResult {
        let cnt = self.byte_writer.count();
        let mm = &val.meta;
        if let Some(mm) = mm {
            self.write_meta(mm)?;
        }
        self.write_value(&val.value)?;
        Ok(self.byte_writer.count() - cnt)
    }
    fn write_meta(&mut self, map: &MetaMap) -> WriteResult {
        let cnt: usize = self.byte_writer.count();
        let is_oneliner = self.is_oneliner_meta(map);
        self.write_byte(b'<')?;
        self.start_block();
        for (n, k) in map.0.iter().enumerate() {
            if n > 0 {
                self.write_byte(b',')?;
            }
            self.indent_element(is_oneliner, n == 0)?;
            match &k.key {
                MetaKey::Str(s) => {
                    self.write_string(s)?;
                },
                MetaKey::Int(i) => {
                    self.write_bytes(i.to_string().as_bytes())?;
                },
            }
            self.write_byte(b':')?;
            self.write(&k.value)?;
        }
        self.end_block(is_oneliner)?;
        self.write_byte(b'>')?;
        Ok(self.byte_writer.count() - cnt)
    }
    fn write_value(&mut self, val: &Value) -> WriteResult {
        let cnt: usize = self.byte_writer.count();
        match val {
            Value::Null => self.write_bytes("null".as_bytes()),
            Value::Bool(b) => if *b {
                self.write_bytes("true".as_bytes())
            } else {
                self.write_bytes("false".as_bytes())
            },
            Value::Int(n) => self.write_int(*n),
            Value::UInt(n) => {
                self.write_uint(*n)?;
                self.write_byte(b'u')
            },
            Value::String(s) => self.write_string(s),
            Value::Blob(b) => self.write_blob(b),
            Value::Double(n) => self.write_double(*n),
            Value::Decimal(d) => self.write_decimal(*d),
            Value::DateTime(d) => self.write_datetime(*d),
            Value::List(lst) => self.write_list(lst),
            Value::Map(map) => self.write_map(map),
            Value::IMap(map) => self.write_imap(map),
        }?;
        Ok(self.byte_writer.count() - cnt)
    }
}

pub struct CponReader<'a, R>
    where R: Read
{
    byte_reader: ByteReader<'a, R>,
}
impl<'a, R> CponReader<'a, R>
    where R: Read
{
    pub fn new(read: &'a mut R) -> Self {
        CponReader { byte_reader: ByteReader::new(read) }
    }

    fn decode_hex_byte(&self, b: u8) -> Result<u8, ReadError> {
        match b {
            b'A' ..= b'F' => Ok(b - b'A' + 10),
            b'a' ..= b'f' => Ok(b - b'a' + 10),
            b'0' ..= b'9' => Ok(b - b'0'),
            c => Err(self.make_error(&format!("Illegal hex encoding character: {c}"), ReadErrorReason::InvalidCharacter)),
        }
    }
    fn read_blob_esc(&mut self) -> Result<Value, ReadError> {
        let mut buff: Vec<u8> = Vec::new();
        self.get_byte()?; // eat b
        self.get_byte()?; // eat "
        loop {
            let b = self.get_byte()?;
            match &b {
                b'\\' => {
                    let b = self.get_byte()?;
                    match &b {
                        b'\\' => buff.push(b'\\'),
                        b'"' => buff.push(b'"'),
                        b'n' => buff.push(b'\n'),
                        b'r' => buff.push(b'\r'),
                        b't' => buff.push(b'\t'),
                        _ => {
                            let hi = b;
                            let lo = self.get_byte()?;
                            let b = self.decode_hex_byte(hi)? * 16 + self.decode_hex_byte(lo)?;
                            buff.push(b);
                        },
                    }
                }
                b'"' => {
                    // end of string
                    break;
                }
                _ => {
                    buff.push(b);
                }
            }
        }
        Ok(Value::from(buff))
    }
    fn read_blob_hex(&mut self) -> Result<Value, ReadError> {
        let mut buff: Vec<u8> = Vec::new();
        self.get_byte()?; // eat x
        self.get_byte()?; // eat "
        loop {
            let mut b1;
            loop {
                b1 = self.get_byte()?;
                if b1 > b' ' {
                    break;
                }
            }
            if b1 == b'"' {
                // end of blob
                break;
            }
            let b2 = self.get_byte()?;
            let b = self.decode_hex_byte(b1)? * 16 + self.decode_hex_byte(b2)?;
            buff.push(b);
        }
        Ok(Value::from(buff))
    }
    fn read_imap(&mut self) -> Result<Value, ReadError> {
        self.get_byte()?; // eat 'i'
        let b = self.get_byte()?; // eat '{'
        if b != b'{' {
            return Err(self.make_error("Wrong IMap prefix, '{' expected.", ReadErrorReason::InvalidCharacter))
        }
        let mut map: BTreeMap<i32, RpcValue> = BTreeMap::new();
        loop {
            self.skip_white_insignificant()?;
            let b = self.peek_byte();
            if b == b'}' {
                self.get_byte()?;
                break;
            }
            let ReadInt{ value, is_negative, .. } = self.read_int(0, false)?;
            let key = if is_negative { -value } else { value };
            self.skip_white_insignificant()?;
            let val = self.read()?;
            #[expect(clippy::cast_possible_truncation, reason = "We hope that the key is small enough to fit")]
            map.insert(key as i32, val);
        }
        Ok(Value::from(map))
    }
    fn read_datetime(&mut self) -> Result<Value, ReadError> {
        self.get_byte()?; // eat 'd'
        let v = self.read_string()?;
        if let Value::String(sdata) = v {
            match DateTime::from_iso_str(&sdata) {
                Ok(dt) => {
                    return Ok(Value::from(dt));
                }
                Err(err) => {
                    return Err(self.make_error(&err, ReadErrorReason::InvalidCharacter))
                }
            }
        }
        Err(self.make_error("Invalid DateTime", ReadErrorReason::InvalidCharacter))
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

}
impl<R> TextReader for CponReader<'_, R>
where R: Read
{
    fn peek_byte(&mut self) -> u8 {
        self.byte_reader.peek_byte()
    }
    fn get_byte(&mut self) -> Result<u8, ReadError> {
        self.byte_reader.get_byte()
    }
    fn make_error(&self, msg: &str, reason: ReadErrorReason) -> ReadError {
        self.byte_reader.make_error(&format!("Cpon read error - {msg}"), reason)
    }
    fn read_string(&mut self) -> Result<Value, ReadError> {
        let mut buff: Vec<u8> = Vec::new();
        self.get_byte()?; // eat "
        loop {
            let b = self.get_byte()?;
            match &b {
                b'\\' => {
                    let b = self.get_byte()?;
                    match &b {
                        b'\\' => buff.push(b'\\'),
                        b'"' => buff.push(b'"'),
                        b'n' => buff.push(b'\n'),
                        b'r' => buff.push(b'\r'),
                        b't' => buff.push(b'\t'),
                        b'0' => buff.push(b'\0'),
                        _ => buff.push(b),
                    }
                }
                b'"' => {
                    // end of string
                    break;
                }
                _ => {
                    buff.push(b);
                }
            }
        }
        let s = std::str::from_utf8(&buff);
        match s {
            Ok(s) => Ok(Value::from(s)),
            Err(e) => Err(self.make_error(&format!("Invalid String, Utf8 error: {e}"), ReadErrorReason::InvalidCharacter)),
        }
    }
}

impl<R> Reader for CponReader<'_, R>
    where R: Read
{
    fn try_read_meta(&mut self) -> Result<Option<MetaMap>, ReadError> {
        self.skip_white_insignificant()?;
        let b = self.peek_byte();
        if b != b'<' {
            return Ok(None)
        }
        self.get_byte()?;
        let mut map = MetaMap::new();
        loop {
            self.skip_white_insignificant()?;
            let b = self.peek_byte();
            if b == b'>' {
                self.get_byte()?;
                break;
            }
            let key = self.read()?;
            self.skip_white_insignificant()?;
            let val = self.read()?;
            if key.is_int() {
                map.insert(key.as_i32(), val);
            }
            else {
                map.insert(key.as_str(), val);
            }
        }
        Ok(Some(map))
    }
    fn read_value(&mut self) -> Result<Value, ReadError> {
        self.skip_white_insignificant()?;
        let b = self.peek_byte();
        let v = match &b {
            b'0' ..= b'9' | b'+' | b'-' => self.read_number(),
            b'"' => self.read_string(),
            b'b' => self.read_blob_esc(),
            b'x' => self.read_blob_hex(),
            b'[' => self.read_list(),
            b'{' => self.read_map(),
            b'i' => self.read_imap(),
            b'd' => self.read_datetime(),
            b't' => self.read_true(),
            b'f' => self.read_false(),
            b'n' => self.read_null(),
            _ => Err(self.make_error(&format!("Invalid char {}, code: {}", char::from(b), b), ReadErrorReason::InvalidCharacter)),
        }?;
        Ok(v)
    }
}

#[cfg(test)]
mod test
{
    use crate::{DateTime, MetaMap, RpcValue};
    use crate::Decimal;
    use std::collections::BTreeMap;
    use chrono::{Duration, FixedOffset, LocalResult, NaiveDate, NaiveDateTime, NaiveTime, TimeZone, Utc};
    use crate::cpon::CponReader;
    use crate::reader::Reader;
    use crate::rpcvalue::Map;
    #[test]
    fn test_read() {
        fn test_cpon_round_trip<T>(cpon: &str, val: T) where RpcValue: From<T> {
            let rv1 = RpcValue::from_cpon(cpon).unwrap();
            let rv2 = RpcValue::from(val);
            assert_eq!(rv1, rv2);
            let cpon2 = rv1.to_cpon();
            assert_eq!(cpon, &cpon2);
        }
        test_cpon_round_trip("null", RpcValue::null());
        test_cpon_round_trip("false", false);
        assert!(RpcValue::from_cpon("faldwa").is_err());
        test_cpon_round_trip("true", true);
        assert_eq!(RpcValue::from_cpon("0").unwrap().as_i32(), 0);
        assert_eq!(RpcValue::from_cpon("123").unwrap().as_i32(), 123);
        test_cpon_round_trip("-123", -123);
        assert_eq!(RpcValue::from_cpon("+123").unwrap().as_i32(), 123);
        test_cpon_round_trip("123u", 123u32);
        assert_eq!(RpcValue::from_cpon("0xFF").unwrap().as_i32(), 255);
        assert_eq!(RpcValue::from_cpon("-0x1000").unwrap().as_i32(), -4096);
        test_cpon_round_trip("123.4", Decimal::new(1234, -1));
        test_cpon_round_trip("0.123", Decimal::new(123, -3));
        test_cpon_round_trip("-0.123", Decimal::new(-123, -3));
        test_cpon_round_trip("123000000e2", Decimal::new(123_000_000, 2));
        assert_eq!(Decimal::new(10000, 3).to_cpon_string(), "10000000.");
        assert_eq!(RpcValue::from_cpon("0e0").unwrap().as_decimal(), Decimal::new(0, 0));
        assert_eq!(RpcValue::from_cpon("0.123e3").unwrap().as_decimal(), Decimal::new(123, 0));
        test_cpon_round_trip("1000000.", Decimal::new(1_000_000, 0));
        test_cpon_round_trip("50.03138741402532", Decimal::new(5_003_138_741_402_532, -14));
        // We do not support such high precision.
        assert!(RpcValue::from_cpon("36.028797018963968").is_err());
        assert_eq!(RpcValue::from_cpon(r#""foo""#).unwrap().as_str(), "foo");
        assert_eq!(RpcValue::from_cpon(r#""ěščřžýáí""#).unwrap().as_str(), "ěščřžýáí");
        assert_eq!(RpcValue::from_cpon("b\"foo\tbar\nbaz\"").unwrap().as_blob(), b"foo\tbar\nbaz");
        assert_eq!(RpcValue::from_cpon(r#""foo\"bar""#).unwrap().as_str(), r#"foo"bar"#);

        assert_eq!(RpcValue::from_cpon("[]").unwrap().to_cpon(), "[]");
        assert_eq!(RpcValue::from_cpon("[1,2,3]").unwrap().to_cpon(), "[1,2,3]");
        assert!(RpcValue::from_cpon("[").is_err());

        assert_eq!(RpcValue::from_cpon("{}").unwrap().to_cpon(), "{}");
        assert_eq!(RpcValue::from_cpon(r#"{"foo": 1, "bar":"baz", }"#).unwrap().to_cpon(), r#"{"bar":"baz","foo":1}"#);
        assert!(RpcValue::from_cpon("{").is_err());

        assert_eq!(RpcValue::from_cpon("i{}").unwrap().to_cpon(), "i{}");
        assert_eq!(RpcValue::from_cpon(r#"i{1: "foo", -1:"bar", 0:"baz", }"#).unwrap().to_cpon(), r#"i{-1:"bar",0:"baz",1:"foo"}"#);
        assert!(RpcValue::from_cpon("i{").is_err());

        let ndt = NaiveDateTime::new(NaiveDate::from_ymd_opt(2022, 1, 2).unwrap(), NaiveTime::from_hms_milli_opt(12, 59, 6, 0).unwrap());
        assert_eq!(RpcValue::from_cpon(r#"d"2022-01-02T12:59:06Z""#).unwrap().as_datetime(), DateTime::from_naive_datetime(&ndt));
        let dt = chrono::DateTime::<Utc>::from_naive_utc_and_offset(ndt, Utc);
        assert_eq!(RpcValue::from_cpon(r#"d"2022-01-02T12:59:06Z""#).unwrap().as_datetime(), DateTime::from_datetime(&dt));


        #[expect(clippy::too_many_arguments, reason = "Allow in tests")]
        fn dt_from_ymd_hms_milli_tz_offset(year: i32, month: u32, day: u32, hour: u32, min: u32, sec: u32, milli: i64, tz_offset: i32) -> chrono::DateTime<FixedOffset> {
            if let LocalResult::Single(dt) = FixedOffset::east_opt(tz_offset).unwrap()
                .with_ymd_and_hms(year, month, day, hour, min, sec) {
                dt.checked_add_signed(Duration::milliseconds(milli)).unwrap()
            } else {
                panic!("Invalid date time");
            }
        }

        const MINUTE: i32 = 60;
        const HOUR: i32 = 60 * MINUTE;

        let dt_str = r#"d"2021-11-08T01:02:03+05""#;
        let dt = dt_from_ymd_hms_milli_tz_offset(2021, 11, 8, 1, 2, 3, 0, 5 * HOUR);
        assert_eq!(RpcValue::from_cpon(dt_str).unwrap().as_datetime(), DateTime::from_datetime(&dt));
        assert_eq!(RpcValue::from_cpon(dt_str).unwrap().to_cpon(), dt_str.to_string());

        let dt_str = r#"d"2021-11-08T01:02:03-0815""#;
        let dt = dt_from_ymd_hms_milli_tz_offset(2021, 11, 8, 1, 2, 3, 0, -8 * HOUR - 15 * MINUTE);
        assert_eq!(RpcValue::from_cpon(dt_str).unwrap().as_datetime(), DateTime::from_datetime(&dt));
        assert_eq!(RpcValue::from_cpon(dt_str).unwrap().to_cpon(), dt_str.to_string());

        let dt_str = r#"d"2021-11-08T01:02:03.456-0815""#;
        let dt = dt_from_ymd_hms_milli_tz_offset(2021, 11, 8, 1, 2, 3, 456, -8 * HOUR - 15 * MINUTE);
        assert_eq!(RpcValue::from_cpon(dt_str).unwrap().as_datetime(), DateTime::from_datetime(&dt));
        assert_eq!(RpcValue::from_cpon(dt_str).unwrap().to_cpon(), dt_str.to_string());

        let lst1 = vec![RpcValue::from(123), RpcValue::from("foo")];
        let cpon = r#"[123 , "foo"]"#;
        let rv = RpcValue::from_cpon(cpon).unwrap();
        let lst2 = rv.as_list();
        assert_eq!(lst2, &lst1);

        let mut map: Map = Map::new();
        map.insert("foo".to_string(), RpcValue::from(123));
        map.insert("bar".to_string(), RpcValue::from("baz"));
        let cpon = r#"{"foo": 123,"bar":"baz"}"#;
        assert_eq!(RpcValue::from_cpon(cpon).unwrap().as_map(), &map);

        let mut map: BTreeMap<i32, RpcValue> = BTreeMap::new();
        map.insert(1, RpcValue::from(123));
        map.insert(2, RpcValue::from("baz"));
        let cpon = r#"i{1: 123,2:"baz"}"#;
        assert_eq!(RpcValue::from_cpon(cpon).unwrap().as_imap(), &map);

        let cpon = r#"<1: 123,2:"baz">"#;
        let mut b = cpon.as_bytes();
        let mut rd = CponReader::new(&mut b);
        let mm1 = rd.try_read_meta().unwrap().unwrap();
        let mut mm2 = MetaMap::new();
        mm2.insert(1, RpcValue::from(123));
        mm2.insert(2, RpcValue::from("baz"));
        assert_eq!(mm1, mm2);
    }
    #[test]
    fn test_read_too_long_numbers() {
        // read very long decimal without overflow error, value is capped
        assert_eq!(RpcValue::from_cpon("123456789012345678901234567890123456789012345678901234567890").unwrap().as_int(), i64::MAX);

        assert_eq!(RpcValue::from_cpon("9223372036854775806").unwrap().as_int(), 9_223_372_036_854_775_806_i64);
        assert_eq!(RpcValue::from_cpon("9223372036854775807").unwrap().as_int(), i64::MAX);
        assert_eq!(RpcValue::from_cpon("9223372036854775808").unwrap().as_int(), i64::MAX);

        assert_eq!(RpcValue::from_cpon("0x7FFFFFFFFFFFFFFE").unwrap().as_int(), 0x7FFF_FFFF_FFFF_FFFE_i64);
        assert_eq!(RpcValue::from_cpon("0x7FFFFFFFFFFFFFFF").unwrap().as_int(), i64::MAX);
        assert_eq!(RpcValue::from_cpon("0x8000000000000000").unwrap().as_int(), i64::MAX);

        assert_eq!(RpcValue::from_cpon("-123456789012345678901234567890123456789012345678901234567890").unwrap().as_int(), i64::MIN);

        assert_eq!(RpcValue::from_cpon("-9223372036854775807").unwrap().as_int(), -9_223_372_036_854_775_807_i64);
        assert_eq!(RpcValue::from_cpon("-9223372036854775808").unwrap().as_int(), i64::MIN);
        assert_eq!(RpcValue::from_cpon("-9223372036854775809").unwrap().as_int(), i64::MIN);

        assert_eq!(RpcValue::from_cpon("-0x7FFFFFFFFFFFFFFF").unwrap().as_int(), -0x7FFF_FFFF_FFFF_FFFF_i64);
        assert_eq!(RpcValue::from_cpon("-0x8000000000000000").unwrap().as_int(), i64::MIN);
        assert_eq!(RpcValue::from_cpon("-0x8000000000000001").unwrap().as_int(), i64::MIN);

        assert!(RpcValue::from_cpon("1.23456789012345678901234567890123456789012345678901234567890").is_err());
        assert!(RpcValue::from_cpon("12345678901234567890123456789012345678901234567890123456.7890").is_err());
        assert!(RpcValue::from_cpon("123456789012345678901234567890123456789012345678901234567890.").is_err());
    }
}
