use std::collections::BTreeMap;

use serde::ser::Error as SerdeError;

use crate::{RpcValue, Value};

#[derive(Debug)]
pub struct Error {
    msg: String,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Serialization error: {msg}", msg = self.msg)
    }
}

impl std::error::Error for Error { }

impl SerdeError for Error {
    fn custom<T: std::fmt::Display>(msg: T) -> Self {
        Error {
            msg: msg.to_string(),
        }
    }
}

pub struct ValueSerializer;

impl serde::Serializer for ValueSerializer {
    type Ok = Value;

    type Error = Error;

    type SerializeSeq = ValueSerializeSeq;

    type SerializeTuple = ValueSerializeSeq;

    type SerializeTupleStruct = ValueSerializeSeq;

    type SerializeTupleVariant = ValueSerializeTupleVariant;

    type SerializeMap = ValueSerializeMap;

    type SerializeStruct = ValueSerializeMap;

    type SerializeStructVariant = ValueSerializeStructVariant;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Bool(v))
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Int(v as _))
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Int(v as _))
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Int(v as _))
    }

    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Int(v as _))
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        Ok(Value::UInt(v as _))
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        Ok(Value::UInt(v as _))
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        Ok(Value::UInt(v as _))
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        Ok(Value::UInt(v as _))
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Double(v as _))
    }

    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Double(v as _))
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        Ok(Value::String(Box::new(v.to_string())))
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Blob(Box::new(v.into())))
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Null)
    }

    fn serialize_some<T>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + serde::Serialize
    {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        self.serialize_none()
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok, Self::Error> {
        self.serialize_unit() // TODO: or empty map
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        self.serialize_str(variant)
    }

    fn serialize_newtype_struct<T>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + serde::Serialize
    {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + serde::Serialize
    {
        Ok(Value::Map(Box::new(BTreeMap::from([(
                            variant.into(),
                            RpcValue::new(value.serialize(self)?, None))])
        )))
    }

    fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Ok(ValueSerializeSeq { elements: Vec::with_capacity(len.unwrap_or(0)) })
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Ok(ValueSerializeTupleVariant {
            name: variant.into(),
            elements: Vec::with_capacity(len),
        })
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Ok(ValueSerializeMap {
            map: BTreeMap::new(),
            next_key: None,
        })
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        self.serialize_map(Some(len))
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Ok(ValueSerializeStructVariant {
            name: variant.into(),
            map: BTreeMap::new(),
        })
    }
}

pub struct ValueSerializeSeq {
    elements: Vec<RpcValue>,
}

impl serde::ser::SerializeSeq for ValueSerializeSeq {
    type Ok = Value;
    type Error = Error;

    fn serialize_element<T: ?Sized + serde::Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        self.elements.push(RpcValue::new(value.serialize(ValueSerializer)?, None));
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Value::List(Box::new(self.elements)))
    }
}

impl serde::ser::SerializeTuple for ValueSerializeSeq {
    type Ok = Value;
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize
    {
        serde::ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        serde::ser::SerializeSeq::end(self)
    }
}

impl serde::ser::SerializeTupleStruct for ValueSerializeSeq {
    type Ok = Value;
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize
    {
        serde::ser::SerializeTuple::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        serde::ser::SerializeTuple::end(self)
    }
}

pub struct ValueSerializeTupleVariant {
    name: String,
    elements: Vec<RpcValue>,
}

impl serde::ser::SerializeTupleVariant for ValueSerializeTupleVariant {
    type Ok = Value;
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize
    {
        self.elements.push(RpcValue::new(value.serialize(ValueSerializer)?, None));
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Map(Box::new(BTreeMap::from([(
                            self.name,
                            RpcValue::new(Value::List(Box::new(self.elements)), None))])
        )))
    }
}

pub struct ValueSerializeMap {
    map: BTreeMap<String, RpcValue>,
    next_key: Option<String>,
}

impl serde::ser::SerializeMap for ValueSerializeMap {
    type Ok = Value;
    type Error = Error;

    fn serialize_key<T: ?Sized + serde::Serialize>(&mut self, key: &T) -> Result<(), Self::Error> {
        if let Value::String(s) = key.serialize(ValueSerializer)? {
            self.next_key = Some(*s);
            Ok(())
        } else {
            Err(Error::custom("Map key must be a string"))
        }
    }

    fn serialize_value<T: ?Sized + serde::Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        let key = self.next_key.take().ok_or_else(|| Error::custom("Value without key"))?;
        self.map.insert(key, RpcValue::new(value.serialize(ValueSerializer)?, None));
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Map(Box::new(self.map)))
    }
}

impl serde::ser::SerializeStruct for ValueSerializeMap {
    type Ok = Value;

    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize
    {
        serde::ser::SerializeMap::serialize_entry(self, key, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        serde::ser::SerializeMap::end(self)
    }
}

pub struct ValueSerializeStructVariant {
    name: String,
    map: BTreeMap<String, RpcValue>,
}

impl serde::ser::SerializeStructVariant for ValueSerializeStructVariant {
    type Ok = Value;
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize
    {
        self.map.insert(key.into(), RpcValue::new(value.serialize(ValueSerializer)?, None));
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Map(Box::new(BTreeMap::from([(
                            self.name,
                            RpcValue::new(Value::Map(Box::new(self.map)), None))])
        )))
    }
}

pub fn to_rpcvalue<T: serde::Serialize>(v: &T) -> Result<RpcValue, Error> {
    Ok(RpcValue::new(v.serialize(ValueSerializer)?, None))
}


#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;


    #[derive(Debug, serde::Serialize)]
    struct UserStruct {
        string: String,
        num: i8,
        flag: bool,
        map: BTreeMap<String, i32>,
        // imap: BTreeMap<i64, String>,
    }

    #[test]
    fn serialize_value() {
        let user = UserStruct {
            string: "test".into(),
            num: 42,
            flag: true,
            map: BTreeMap::from([("abc".into(), 123)]),
            // imap: BTreeMap::from([(1, "xyz".into())]),
        };
        let rv = super::to_rpcvalue(&user).unwrap();
        assert!(rv.is_map());
        assert_eq!(rv.as_map().get("string").unwrap(), &"test".into());
        assert_eq!(rv.as_map().get("num").unwrap(), &42.into());
        assert_eq!(rv.as_map().get("flag").unwrap(), &true.into());
        assert_eq!(rv.as_map().get("map").unwrap().value, crate::Value::Map(Box::new(crate::make_map!("abc" => 123))));
        // assert_eq!(rv.as_map().get("imap").unwrap().value, crate::Value::IMap(Box::new(crate::make_imap!(1 => "xyz"))));
    }
}
