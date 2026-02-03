use std::io::{Write, Read};
use crate::{RpcValue, MetaMap, Value, DateTime, ReadResult};
use std::collections::BTreeMap;
use crate::datetime::{IncludeMilliseconds, ToISOStringOptions};
use crate::writer::{WriteResult, Writer, ByteWriter};
use crate::metamap::MetaKey;
use crate::reader::{Reader, ByteReader, ReadError, ReadErrorReason};
use crate::rpcvalue::{Map};
use crate::textrdwr::{TextReader, TextWriter};

pub struct JsonWriter<'a, W>
    where W: Write
{
    byte_writer: ByteWriter<'a, W>,
    wrappers_stack: Vec<bool>,
}

impl<'a, W> JsonWriter<'a, W>
    where W: Write
{
    pub fn new(write: &'a mut W) -> Self {
        JsonWriter {
            byte_writer: ByteWriter::new(write),
            wrappers_stack: vec![],
        }
    }
    fn write_string(&mut self, s: &str) -> WriteResult {
        let cnt = self.byte_writer.count();
        self.write_byte(b'"')?;
        for ch in s.chars() {
            match ch {
                '"'  => self.write_bytes(b"\\\"")?,  // Escape double quotes
                '\\' => self.write_bytes(b"\\\\")?,  // Escape backslashes
                // '\x08' => self.write_bytes(b"\\b")?,   // Escape newline
                // '\x0c' => self.write_bytes(b"\\f")?,   // Escape newline
                '\n' => self.write_bytes(b"\\n")?,   // Escape newline
                '\r' => self.write_bytes(b"\\r")?,   // Escape carriage return
                '\t' => self.write_bytes(b"\\t")?,   // Escape tab
                // '\u{2028}' => escaped.push_str("\\u2028"), // Escape line separator (U+2028)
                // '\u{2029}' => escaped.push_str("\\u2029"), // Escape paragraph separator (U+2029)
                // ch if ch > '\u{7F}' => {
                //     // Escape Unicode characters beyond ASCII (i.e., non-ASCII characters)
                //     self.write_bytes(format!("\\u{:04X}", ch as u32).as_bytes())?
                // },
                ch => {
                    let mut b = [0; 4];
                    let s = ch.encode_utf8(&mut b);
                    self.write_bytes(s.as_bytes())?
                },
            };
        }
        self.write_byte(b'"')?;
        Ok(self.byte_writer.count() - cnt)
    }
    fn write_blob(&mut self, bytes: &[u8]) -> WriteResult {
        let cnt = self.byte_writer.count();
        self.add_wrap_type_tag("Blob")?;
        self.write_byte(b'"')?;
        for b in bytes {
            let (high, low) = crate::u8_to_hex(*b);
            self.write_byte(high)?;
            self.write_byte(low)?;
        }
        self.write_byte(b'"')?;
        Ok(self.byte_writer.count() - cnt)
    }
    fn write_datetime(&mut self, dt: DateTime) -> WriteResult {
        let cnt = self.byte_writer.count();
        self.add_wrap_type_tag("DateTime")?;
        self.write_byte(b'"')?;
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
        self.write_byte(b'[')?;
        for (n, v) in lst.iter().enumerate() {
            if n > 0 {
                self.write_byte(b',')?;
            }
            self.write(v)?;
        }
        self.write_byte(b']')?;
        Ok(self.byte_writer.count() - cnt)
    }
    fn write_map(&mut self, map: &Map) -> WriteResult {
        let cnt = self.byte_writer.count();
        self.write_byte(b'{')?;
        for (n, (k, v)) in map.iter().enumerate() {
            if n > 0 {
                self.write_byte(b',')?;
            }
            self.write_string(k)?;
            self.write_byte(b':')?;
            self.write(v)?;
        }
        self.write_byte(b'}')?;
        Ok(self.byte_writer.count() - cnt)
    }
    fn write_imap(&mut self, map: &BTreeMap<i32, RpcValue>) -> WriteResult {
        let cnt = self.byte_writer.count();
        self.add_wrap_type_tag("IMap")?;
        self.write_byte(b'{')?;
        for (n, (k, v)) in map.iter().enumerate() {
            if n > 0 {
                self.write_byte(b',')?;
            }
            self.write_byte(b'"')?;
            self.write_int(i64::from(*k))?;
            self.write_byte(b'"')?;
            self.write_byte(b':')?;
            self.write(v)?;
        }
        self.write_byte(b'}')?;
        Ok(self.byte_writer.count() - cnt)
    }
    fn push_wrap_state(&mut self) {
        self.wrappers_stack.push(false);
    }
    fn add_wrap_tag(&mut self, tag: &str, val: Option<&str>) -> WriteResult {
        if let Some(b) = self.wrappers_stack.last_mut() && !*b {
            *b = true;
            self.write_byte(b'[')?;
        }
        self.write_string(tag)?;
        self.write_byte(b',')?;
        if let Some(val) = val {
            self.write_string(val)?;
            self.write_byte(b',')?;
        }
        Ok(tag.len() + 1)
    }
    fn add_wrap_meta_tag(&mut self) -> WriteResult { self.add_wrap_tag(TAG_META, None) }
    fn add_wrap_type_tag(&mut self, type_str: &str) -> WriteResult { self.add_wrap_tag(TAG_TYPE, Some(type_str)) }
    fn pop_wrap_state(&mut self) -> WriteResult {
        match self.wrappers_stack.pop() {
            Some(true) => self.write_byte(b']'),
            _ => Ok(0),
        }
    }
}

const TAG_META: &str = "!shvMeta";
const TAG_TYPE: &str = "!shvType";

impl<W> TextWriter for JsonWriter<'_, W>
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

impl<W> Writer for JsonWriter<'_, W>
where W: Write
{
    fn write(&mut self, val: &RpcValue) -> WriteResult {
        self.push_wrap_state();
        let cnt = self.byte_writer.count();
        if let Some(mm) = &val.meta {
            self.write_meta(mm)?;
        }
        self.write_value(&val.value)?;
        self.pop_wrap_state()?;
        Ok(self.byte_writer.count() - cnt)
    }
    fn write_meta(&mut self, map: &MetaMap) -> WriteResult {
        let cnt: usize = self.byte_writer.count();
        self.add_wrap_meta_tag()?;
        self.write_byte(b'{')?;
        for (n, k) in map.0.iter().enumerate() {
            if n > 0 {
                self.write_byte(b',')?;
            }
            match &k.key {
                MetaKey::Str(s) => {
                    self.write_string(s)?;
                },
                MetaKey::Int(i) => {
                    self.write_string(&i.to_string())?;
                },
            }
            self.write_byte(b':')?;
            self.write(&k.value)?;
        }
        self.write_byte(b'}')?;
        self.write_byte(b',')?;
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
            Value::UInt(n) => self.write_int(n.cast_signed()),
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
pub struct JsonReader<'a, R>
    where R: Read
{
    byte_reader: ByteReader<'a, R>,
}
impl<'a, R> JsonReader<'a, R>
    where R: Read
{
    pub fn new(read: &'a mut R) -> Self {
        JsonReader { byte_reader: ByteReader::new(read) }
    }
    fn retype_value(&self, m: Option<&RpcValue>, t: Option<&str>, v: &Value) -> Result<RpcValue, ReadError> {
        let meta = 'a: {
            if let Some(rv) = m && let Value::Map(m) = &rv.value {
                let mut mm = MetaMap::default();
                for (k, v) in m.iter() {
                    if let Ok(i) = k.parse::<i32>() {
                        mm.insert(i, v.clone());
                    } else {
                        mm.insert(k.as_str(), v.clone());
                    }
                }
                break 'a Some(mm)
            }
            None
        };
        let val = match t {
            Some("Blob") => {
                if let Value::String(hex) = v {
                    let data = hex::decode(hex.as_str()).map_err(|e| self.make_error(&format!("Hex blob decode error: {e}."), ReadErrorReason::InvalidCharacter))?;
                    Value::from(data)
                } else {
                    return Err(self.make_error("Blob must be encoded as hex string.", ReadErrorReason::InvalidCharacter))
                }
            }
            Some("DateTime") => {
                if let Value::String(dt) = v {
                    let dt = DateTime::from_iso_str(dt).map_err(|e| self.make_error(&format!("DateTime decode error: {e}."), ReadErrorReason::InvalidCharacter))?;
                    Value::from(dt)
                } else {
                    return Err(self.make_error("DateTime must be encoded as ISO string.", ReadErrorReason::InvalidCharacter))
                }
            }
            Some("IMap") => {
                if let Value::Map(im) = v {
                    let mut imap = crate::IMap::default();
                    for (k, v) in im.iter() {
                        let ik = k.parse::<i32>().map_err(|e| self.make_error(&format!("IMap key decode error: {e}."), ReadErrorReason::InvalidCharacter))?;
                        imap.insert(ik, v.clone());
                    }
                    Value::from(imap)
                } else {
                    // let s = RpcValue::new(v.clone(), None).to_cpon();
                    return Err(self.make_error("IMap must be encoded as map with indexes converted to string keys.", ReadErrorReason::InvalidCharacter))
                }
            }
            _ => { v.clone() }
        };
        Ok(RpcValue::new(val, meta))
    }
}
impl<R> TextReader for JsonReader<'_, R>
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
            // println!(".....{}", b as char);
            match &b {
                b'\\' => {
                    let b = self.get_byte()?;
                    // println!("\\{}", b as char);
                    match &b {
                        b'"' => buff.push(b'"'),
                        b'\\' => buff.push(b'\\'),
                        // b'b' => buff.push(b'\x08'),
                        // b'f' => buff.push(b'\x0c'),
                        b'n' => buff.push(b'\n'),
                        b'r' => buff.push(b'\r'),
                        b't' => buff.push(b'\t'),
                        b'u' => {
                            // \uXXXX
                            let mut hex = "".to_string();
                            hex.push(self.get_byte()? as char);
                            hex.push(self.get_byte()? as char);
                            hex.push(self.get_byte()? as char);
                            hex.push(self.get_byte()? as char);
                            let code_point = u32::from_str_radix(&hex, 16).map_err(|e| self.make_error(&format!("Invalid unicode escape sequence: {hex:?} - {e}"), ReadErrorReason::InvalidCharacter))?;
                            let ch = char::from_u32(code_point).ok_or_else(|| self.make_error(&format!("Invalid code point: {code_point}"), ReadErrorReason::InvalidCharacter))?;
                            let mut utf8 = [0; 4];
                            let s = ch.encode_utf8(&mut utf8);
                            for b in s.as_bytes() {
                                buff.push(*b);
                            }
                        }
                        _ => {
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
        let s = std::str::from_utf8(&buff);
        match s {
            Ok(s) => Ok(Value::from(s)),
            Err(e) => Err(self.make_error(&format!("Invalid String, Utf8 error: {e}"), ReadErrorReason::InvalidCharacter)),
        }
    }
}

impl<R> Reader for JsonReader<'_, R>
    where R: Read
{
    fn read(&mut self) -> ReadResult {
        let val = self.read_value()?;
        let rv = 'a: {
            if let Value::List(list1) = &val {
                match &list1[..] {
                    [meta_tag, meta, type_tag, typestr, val] if meta_tag.as_str() == TAG_META && type_tag.as_str() == TAG_TYPE => {
                        break 'a self.retype_value(Some(meta), Some(typestr.as_str()), &val.value)?;
                    }
                    [meta_tag, _meta, type_tag, _typestr] if meta_tag.as_str() == TAG_META && type_tag.as_str() == TAG_TYPE => {
                        return Err(self.make_error("Value part of encoded meta with type missing.", ReadErrorReason::InvalidCharacter));
                    }
                    [type_tag, _typestr, meta_tag, _meta, _val @ ..] if meta_tag.as_str() == TAG_META && type_tag.as_str() == TAG_TYPE => {
                        return Err(self.make_error("Meta tag must be first.", ReadErrorReason::InvalidCharacter));
                    }

                    [meta_tag, meta, val] if meta_tag.as_str() == TAG_META => {
                        break 'a self.retype_value(Some(meta), None, &val.value)?;
                    }
                    [meta_tag, _meta] if meta_tag.as_str() == TAG_META => {
                        return Err(self.make_error("Value part of encoded meta missing.", ReadErrorReason::InvalidCharacter));
                    }

                    [type_tag, typestr, val] if type_tag.as_str() == TAG_TYPE => {
                        break 'a self.retype_value(None, Some(typestr.as_str()), &val.value)?;
                    }
                    [type_tag, _typestr] if type_tag.as_str() == TAG_TYPE => {
                        return Err(self.make_error("Value part of encoded type missing.", ReadErrorReason::InvalidCharacter));
                    }
                    _ => {
                        break 'a RpcValue::new(val, None);
                    }
                }
            }
            break 'a RpcValue::new(val, None);
        };
        Ok(rv)
    }
    fn try_read_meta(&mut self) -> Result<Option<MetaMap>, ReadError> {
        Ok(None)
    }
    fn read_value(&mut self) -> Result<Value, ReadError> {
        self.skip_white_insignificant()?;
        let b = self.peek_byte();
        let v = match &b {
            b'0' ..= b'9' | b'+' | b'-' => self.read_number(),
            b'"' => self.read_string(),
            b'[' => self.read_list(),
            b'{' => self.read_map(),
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
    use crate::Map;
    use chrono::{Duration, FixedOffset, LocalResult};
    use crate::json::{TAG_META, TAG_TYPE};
    use crate::{Decimal, IMap, RpcValue};

    fn fix_tags(json: &str) -> String {
        json.replace("!shvM", TAG_META)
            .replace("!shvT", TAG_TYPE)
    }
    fn test_json_round_trip<T>(json: &str, val: T) where RpcValue: From<T> {
        let json = fix_tags(json);
        let rv1 = RpcValue::from_json(&json).unwrap();
        let rv2 = RpcValue::from(val);
        assert_eq!(rv1, rv2);
        let json2 = rv1.to_json();
        assert_eq!(&json.replace(' ', ""), &json2);
    }
    fn test_cpon_cross_check(json: &str, cpon: &str) {
        let json1 = fix_tags(json);
        let rv1 = RpcValue::from_json(&json1).unwrap();
        let rv2 = RpcValue::from_cpon(cpon).unwrap();
        assert_eq!(rv1, rv2);
        let json2 = rv1.to_json();
        // println!("{json2} <=> {json2}");
        assert_eq!(&json1.replace(' ', ""), &json2);
    }
    #[test]
    fn test_string() {
        for (text, json) in [
            // (r#"["!shvT", "IMap"]"#, None),
            ("", r#""""#),
            ("foo", r#""foo""#),
            ("ěščřžýáí", r#""ěščřžýáí""#),
            ("foo\tbar\nbaz\"single\\quote\r", r#""foo\tbar\nbaz\"single\\quote\r""#),
            ("😀👾🤖", r#""😀👾🤖""#),
        ] {
            let rv1 = RpcValue::from(text);
            let rv2 = RpcValue::from_json(json).unwrap();
            assert_eq!(rv1, rv2);
            assert_eq!(rv1.to_json(), json);
        }
    }
    #[test]
    fn test_blob() {
        for (json, cpon) in [
            (r#"["!shvT", "Blob", ""]"#, r#"b"""#),
            (r#"["!shvT", "Blob", "6162a1"]"#, r#"x"6162a1""#),
            (r#"["!shvT", "Blob", "6162a1"]"#, r#"b"ab\a1""#),
        ] {
            test_cpon_cross_check(json, cpon);
        }
    }
    #[test]
    fn test_primitive_types() {
        test_json_round_trip("null", ());
        test_json_round_trip("true", true);
        test_json_round_trip("false", false);
        test_json_round_trip("42", 42);
    }
    #[test]
    fn test_implicit_conversions() {
        assert_eq!(RpcValue::from(42_u32).to_json(), "42");
        assert_eq!(RpcValue::from(4.2).to_json(), "4.2");
        assert_eq!(RpcValue::from(Decimal::new(42, -1)).to_json(), "4.2");
    }
    #[test]
    fn test_imap() {
        assert!(RpcValue::from_json(fix_tags(r#"["!shvT", "IMap"]"#)).is_err());
        assert!(RpcValue::from_json(fix_tags(r#"["!shvT", "IMap", {"foo": "bar"}]"#)).is_err());
        for (json, imap) in [
            (r#"["!shvT", "IMap", {}]"#, IMap::default()),
            (r#"["!shvT", "IMap", {"1": 2}]"#, IMap::from([(1, 2.into())])),
            (r#"["!shvT", "IMap", {"1": 2, "2": "foo"}]"#, IMap::from([(1, 2.into()), (2, "foo".into())])),
        ] {
            test_json_round_trip(json, imap);
        }
    }
    #[test]
    fn test_map() {
        for (json, imap) in [
            // (r#"["!shvT", "IMap"]"#, None),
            (r#"{}"#, Map::default()),
            (r#"{"foo": 2}"#, Map::from([("foo".into(), 2.into())])),
            (r#"{"bar": "baz", "foo": 2}"#, Map::from([("foo".into(), 2.into()), ("bar".into(), "baz".into())])),
        ] {
            test_json_round_trip(json, imap);
        }
    }

    #[test]
    fn test_datetime() {
        use chrono::TimeZone;
        const MINUTE: i32 = 60;
        const HOUR: i32 = 60 * MINUTE;

        #[expect(clippy::too_many_arguments, reason = "Allow in tests")]
        fn dt_from_ymd_hms_milli_tz_offset(year: i32, month: u32, day: u32, hour: u32, min: u32, sec: u32, milli: i64, tz_offset: i32) -> chrono::DateTime<FixedOffset> {
            if let LocalResult::Single(dt) = FixedOffset::east_opt(tz_offset).unwrap()
                .with_ymd_and_hms(year, month, day, hour, min, sec) {
                dt.checked_add_signed(Duration::milliseconds(milli)).unwrap()
            } else {
                panic!("Invalid date time");
            }
        }
        fn make_json_dt(dtstr: &str) -> String {
            format!(r#"["!shvT","DateTime","{dtstr}"]"#)
        }
        for (dt_str, dt) in &[
            ("2021-11-08T01:02:03+05", dt_from_ymd_hms_milli_tz_offset(2021, 11, 8, 1, 2, 3, 0, 5 * HOUR)),
            ("2021-11-08T01:02:03-0815", dt_from_ymd_hms_milli_tz_offset(2021, 11, 8, 1, 2, 3, 0, -8 * HOUR - 15 * MINUTE)),
            ("2021-11-08T01:02:03.456-0815", dt_from_ymd_hms_milli_tz_offset(2021, 11, 8, 1, 2, 3, 456, -8 * HOUR - 15 * MINUTE)),
        ] {
            let json = make_json_dt(dt_str);
            test_json_round_trip(&json, dt);
        }
    }
    #[test]
    fn test_meta() {
        assert!(RpcValue::from_json(fix_tags(r#"["!shvM", {}]"#)).is_err());
        assert!(RpcValue::from_json(fix_tags(r#"["!shvM", {"1":2}, "!shvT", "IMap"]"#)).is_err());
        assert!(RpcValue::from_json(fix_tags(r#"["!shvT", "IMap", "!shvM", {"1":2}, {"42": 7}]"#)).is_err());
        for (json, cpon) in [
            (r#"["!shvM", {"1":2}, 42]"#, r#"<1:2>42"#),
            (r#"["!shvM", {"1":2}, "!shvT", "IMap", {"42": 7}]"#, r#"<1:2>i{42:7}"#),
            (r#"["!shvM", {"1": 2, "foo": "bar"}, [1,2,3]]"#, r#"<1:2, "foo":"bar">[1,2,3]"#),
        ] {
            test_cpon_cross_check(json, cpon);
        }
    }

    #[test]
    fn test_read_too_long_numbers() {
        // read very long decimal without overflow error, value is capped
        assert_eq!(RpcValue::from_json("123456789012345678901234567890123456789012345678901234567890").unwrap().as_int(), i64::MAX);

        assert_eq!(RpcValue::from_json("9223372036854775806").unwrap().as_int(), 9_223_372_036_854_775_806_i64);
        assert_eq!(RpcValue::from_json("9223372036854775807").unwrap().as_int(), i64::MAX);
        assert_eq!(RpcValue::from_json("9223372036854775808").unwrap().as_int(), i64::MAX);

        assert_eq!(RpcValue::from_json("0x7FFFFFFFFFFFFFFE").unwrap().as_int(), 0x7FFF_FFFF_FFFF_FFFE_i64);
        assert_eq!(RpcValue::from_json("0x7FFFFFFFFFFFFFFF").unwrap().as_int(), i64::MAX);
        assert_eq!(RpcValue::from_json("0x8000000000000000").unwrap().as_int(), i64::MAX);

        assert_eq!(RpcValue::from_json("-123456789012345678901234567890123456789012345678901234567890").unwrap().as_int(), i64::MIN);

        assert_eq!(RpcValue::from_json("-9223372036854775807").unwrap().as_int(), -9_223_372_036_854_775_807_i64);
        assert_eq!(RpcValue::from_json("-9223372036854775808").unwrap().as_int(), i64::MIN);
        assert_eq!(RpcValue::from_json("-9223372036854775809").unwrap().as_int(), i64::MIN);

        assert_eq!(RpcValue::from_json("-0x7FFFFFFFFFFFFFFF").unwrap().as_int(), -0x7FFF_FFFF_FFFF_FFFF_i64);
        assert_eq!(RpcValue::from_json("-0x8000000000000000").unwrap().as_int(), i64::MIN);
        assert_eq!(RpcValue::from_json("-0x8000000000000001").unwrap().as_int(), i64::MIN);

        assert!(RpcValue::from_json("1.23456789012345678901234567890123456789012345678901234567890").is_err());
        assert!(RpcValue::from_json("12345678901234567890123456789012345678901234567890123456.7890").is_err());
        assert!(RpcValue::from_json("123456789012345678901234567890123456789012345678901234567890.").is_err());
    }
}
