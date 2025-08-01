use serde::de::{self, Deserialize, Deserializer, IntoDeserializer, Visitor};
use serde::forward_to_deserialize_any;
use std::fmt;

use crate::{RpcValue, Value};

pub struct ValueDeserializer<'a> {
    pub value: &'a Value,
}

impl<'de, 'a> IntoDeserializer<'de> for ValueDeserializer<'a> {
    type Deserializer = Self;

    fn into_deserializer(self) -> Self::Deserializer {
        self
    }
}

impl<'de, 'a> serde::Deserializer<'de> for ValueDeserializer<'a> {
    type Error = de::value::Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Value::Null => visitor.visit_unit(),
            Value::Bool(b) => visitor.visit_bool(*b),
            Value::Int(i) => visitor.visit_i64(*i),
            Value::UInt(u) => visitor.visit_u64(*u),
            Value::Double(f) => visitor.visit_f64(*f),
            Value::String(s) => visitor.visit_str(s),
            Value::DateTime(dt) => visitor.visit_str(&dt.to_iso_string()),
            Value::Blob(bytes) => visitor.visit_byte_buf(*bytes.clone()),
            Value::List(list) => {
                        let iter = list.iter().map(|v| ValueDeserializer { value: &v.value });
                        let mut seq = de::value::SeqDeserializer::new(iter);
                        visitor.visit_seq(&mut seq)
                    }
            Value::Map(map) => {
                        let iter = map
                            .iter()
                            .map(|(k, v)| (k.as_str(), ValueDeserializer { value: &v.value }));
                        let mut map = de::value::MapDeserializer::new(iter);
                        visitor.visit_map(&mut map)
                    }
            Value::Decimal(_decimal) => todo!(),
            Value::IMap(_btree_map) => todo!(),
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Value::Int(i) => visitor.visit_i64(*i),
            // Value::UInt(u) => visitor.visit_i64(*u as i64),
            // Value::Double(f) => visitor.visit_i64(*f as i64),
            _ => Err(de::Error::custom("expected integer-compatible value")),
        }
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where
            V: Visitor<'de>
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where
            V: Visitor<'de>
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where
            V: Visitor<'de>
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Value::UInt(u) => visitor.visit_u64(*u),
            // Value::Int(i) if *i >= 0 => visitor.visit_u64(*i as u64),
            // Value::Double(f) if *f >= 0.0 => visitor.visit_u64(*f as u64),
            _ => Err(de::Error::custom("expected unsigned integer-compatible value")),
        }
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where
            V: Visitor<'de>
    {
        self.deserialize_u64(visitor)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where
            V: Visitor<'de>
    {
        self.deserialize_u64(visitor)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where
            V: Visitor<'de>
    {
        self.deserialize_u64(visitor)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Value::Double(f) => visitor.visit_f64(*f),
            // Value::Int(i) => visitor.visit_f64(*i as f64),
            // Value::UInt(u) => visitor.visit_f64(*u as f64),
            _ => Err(de::Error::custom("expected float-compatible value")),
        }
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where
            V: Visitor<'de>
    {
        self.deserialize_f64(visitor)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Value::String(s) => visitor.visit_str(s),
            Value::DateTime(dt) => visitor.visit_str(&dt.to_iso_string()),
            _ => Err(de::Error::custom("expected string")),
        }
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Value::Blob(bytes) => visitor.visit_bytes(bytes),
            _ => Err(de::Error::custom("expected blob/bytes")),
        }
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Value::Blob(bytes) => visitor.visit_byte_buf(*bytes.clone()),
            _ => Err(de::Error::custom("expected blob/byte_buf")),
        }
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Value::Null => visitor.visit_none(),
            _ => visitor.visit_some(self),
        }
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Value::Null => visitor.visit_unit(),
            _ => Err(de::Error::custom("expected unit")),
        }
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }

    fn deserialize_newtype_struct<V>(
            self,
            name: &'static str,
            visitor: V,
        ) -> Result<V::Value, Self::Error>
        where
            V: Visitor<'de>
    {
        match name {
            "DateTime" => match self.value {
                Value::DateTime(date_time) => visitor.visit_i64(date_time.to_inner()),
                _ => self.deserialize_any(visitor),
            }
            _ => self.deserialize_any(visitor),
        }
    }

    forward_to_deserialize_any! {
        // i8 i16 i32 u8 u16 u32 f32
        char unit_struct tuple tuple_struct enum identifier ignored_any
    }
}

impl<'de> serde::Deserialize<'de> for crate::DateTime {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>
    {
        struct DateTimeVisitor;

        impl<'de> Visitor<'de> for DateTimeVisitor {
            type Value = crate::DateTime;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a custom DateTime value")
            }

            fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
                where
                    E: de::Error,
            {
                Ok(crate::DateTime::from_inner(v))
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
                where
                    E: de::Error,
            {
                crate::DateTime::from_iso_str(v)
                    .map_err(E::custom)
            }
        }

        deserializer.deserialize_newtype_struct("DateTime", DateTimeVisitor)
    }
}

pub fn from_value<'a, T: Deserialize<'a>>(value: &Value) -> Result<T, serde::de::value::Error> {
    T::deserialize(ValueDeserializer { value })
}

pub fn from_rpcvalue<'a, T: Deserialize<'a>>(rpc_value: &RpcValue) -> Result<T, serde::de::value::Error> {
    from_value(&rpc_value.value)
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use crate::Value;

    use super::from_value;

    #[derive(Debug,serde::Deserialize)]
    struct Person {
        name: String,
        age: i8,
        date_time: crate::DateTime,
        date_time_str: String,
        map: BTreeMap<String, i32>,
        list: Vec<u32>,
    }

    #[test]
    fn deserialize_value() {
        let date_time = crate::DateTime::from_iso_str("2025-08-01T12:30:40+02").unwrap();
        let person: Person = from_value(&Value::Map(Box::new(crate::make_map!(
                        "name" => "john",
                        "age" => 42,
                        "date_time" => date_time,
                        "date_time_str" => date_time,
                        "map" => crate::make_map!(
                            "a" => 1,
                            "b" => 2,
                        ),
                        "list" => crate::make_list!(10_u32, 20_u32, 30_u32),
        )))).unwrap();
        assert_eq!(&person.name, "john");
        assert_eq!(person.age, 42);
        assert_eq!(person.date_time, date_time);
        assert_eq!(person.date_time_str, date_time.to_iso_string());
        assert_eq!(person.map, BTreeMap::from([
                ("a".into(), 1),
                ("b".into(), 2)
        ]));
        assert_eq!(person.list, Vec::from([10, 20, 30]));
    }
}
