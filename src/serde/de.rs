use serde::de::{self, Deserialize, DeserializeSeed, Deserializer, EnumAccess, IntoDeserializer, VariantAccess, Visitor};
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

    fn deserialize_enum<V>(
        self,
        _name: &str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            // Externally tagged (e.g. { "Variant": {...} })
            Value::Map(map) if map.len() == 1 => {
                let (variant, value) = map.iter().next().unwrap();
                let enum_access = SimpleEnumAccess { variant: variant.clone(), value: Some(value.value.clone()) };
                visitor.visit_enum(enum_access)
            }

            // Internally tagged (e.g. { "type": "Variant", ... })
            Value::Map(map) => {
                if let Some(Value::String(variant)) = map.get("type").map(|rv| &rv.value) {
                    let variant = *variant.clone();
                    let content = Value::Map(map.clone()); // reuse the whole map
                    let enum_access = SimpleEnumAccess {
                        variant,
                        value: Some(content),
                    };
                    return visitor.visit_enum(enum_access);
                }

                // Adjacently tagged (e.g. { "tag": "Variant", "content": {...} })
                if let (Some(Value::String(tag)), Some(content)) =
                    (map.get("tag").map(|rv| &rv.value), map.get("content").map(|rv| &rv.value))
                {
                    let variant = *tag.clone();
                    let enum_access = SimpleEnumAccess {
                        variant,
                        value: Some(content.clone()),
                    };
                    return visitor.visit_enum(enum_access);
                }

                // Fallback to untagged
                visitor.visit_enum(UntaggedEnumAccess(&Value::Map(map.clone())))
            }

            // Unit variant as a string
            Value::String(s) => {
                let enum_access = SimpleEnumAccess { variant: *s.clone(), value: None };
                visitor.visit_enum(enum_access)
            }

            // Fallback to untagged for other types
            other => visitor.visit_enum(UntaggedEnumAccess(other)),
        }
    }

    forward_to_deserialize_any! {
        // i8 i16 i32 u8 u16 u32 f32
        char unit_struct tuple tuple_struct identifier ignored_any
    }
}

struct SimpleEnumAccess {
    variant: String,
    value: Option<Value>,
}

impl<'de> EnumAccess<'de> for SimpleEnumAccess {
    type Error = serde::de::value::Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self), Self::Error>
    where V: DeserializeSeed<'de> {
        let de = self.variant.as_str().into_deserializer();
        seed.deserialize(de).map(|v| (v, self))
    }
}

impl<'de> VariantAccess<'de> for SimpleEnumAccess {
    type Error = serde::de::value::Error;

    fn unit_variant(self) -> Result<(), Self::Error> {
        match self.value {
            Some(v) => serde::Deserialize::deserialize(ValueDeserializer { value: &v }),
            None => Ok(()),
        }
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where T: DeserializeSeed<'de> {
        match self.value {
            Some(v) => seed.deserialize(ValueDeserializer { value: &v }),
            None => Err(de::Error::custom("Expected value for newtype variant")),
        }
    }

    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where V: Visitor<'de> {
        serde::Deserializer::deserialize_any(ValueDeserializer { value: &self.value.unwrap_or(Value::Null) }, visitor)
    }

    fn struct_variant<V>(self, _fields: &'static [&'static str], visitor: V) -> Result<V::Value, Self::Error>
    where V: Visitor<'de> {
        serde::Deserializer::deserialize_any(ValueDeserializer { value: &self.value.unwrap_or(Value::Null) }, visitor)
    }
}

struct UntaggedEnumAccess<'a>(&'a Value);

impl<'a, 'de> EnumAccess<'de> for UntaggedEnumAccess<'a> {
    type Error = serde::de::value::Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self), Self::Error>
    where
        V: serde::de::DeserializeSeed<'de>,
    {
        // Pretend the variant name is "", since Serde doesn’t use it
        let de = "".into_deserializer();
        seed.deserialize(de).map(|v| (v, self))
    }
}

impl<'a, 'de> VariantAccess<'de> for UntaggedEnumAccess<'a> {
    type Error = serde::de::value::Error;

    fn unit_variant(self) -> Result<(), Self::Error> {
        serde::Deserialize::deserialize(ValueDeserializer { value: self.0 })
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where T: serde::de::DeserializeSeed<'de> {
        seed.deserialize(ValueDeserializer { value: &self.0 })
    }

    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where V: Visitor<'de> {
        serde::Deserializer::deserialize_any(ValueDeserializer { value: &self.0 }, visitor)
    }

    fn struct_variant<V>(self, _fields: &'static [&'static str], visitor: V) -> Result<V::Value, Self::Error>
    where V: Visitor<'de> {
        serde::Deserializer::deserialize_any(ValueDeserializer { value: &self.0 }, visitor)
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

    use crate::serde::IMap;
    use crate::Value;

    use super::from_value;

    #[derive(Debug, serde::Deserialize,PartialEq)]
    #[serde(tag = "type", content = "c")]
    enum Command {
        Quit,
        Move { x: i32, y: i32 },
        Shoot(f64, f64),
    }

    #[derive(Debug, serde::Deserialize,PartialEq)]
    #[serde(tag = "type")]
    enum CustomTagEnum {
        Unnamed,
        StructVariant { x: i32, y: i32 },
    }

    #[derive(Debug, serde::Deserialize,PartialEq)]
    enum ExternallyTaggedEnum {
        Variant(i32, i32),
        Unnamed,
    }

    #[derive(Debug,serde::Deserialize)]
    struct Person {
        name: String,
        age: i8,
        date_time: crate::DateTime,
        date_time_str: String,
        map: BTreeMap<String, i32>,
        list: Vec<u32>,
        command: Command,
        command2: Command,
        command3: Command,
        custom_tag_enum_1: CustomTagEnum,
        custom_tag_enum_2: CustomTagEnum,
        ext_tag_enum: ExternallyTaggedEnum,
        ext_tag_enum_unnamed: ExternallyTaggedEnum,
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
                        "command" => crate::make_map!(
                            "type" => "Move",
                            "c" => crate::make_map!(
                                "x" => 5,
                                "y" => 7,
                            ),
                        ),
                        "command2" => crate::make_map!("type" => "Quit"),
                        "command3" => crate::make_map!(
                            "type" => "Shoot",
                            "c" => crate::make_list!(10.5, 9.75),
                        ),
                        "custom_tag_enum_1" => crate::make_map!("type" => "Unnamed"),
                        "custom_tag_enum_2" => crate::make_map!(
                            "type" => "StructVariant",
                            "x" => 1,
                            "y" => 2,
                        ),
                        "ext_tag_enum" => crate::make_map!(
                            "Variant" => crate::make_list!(10, 20),
                        ),
                        "ext_tag_enum_unnamed" => "Unnamed",
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
        assert_eq!(person.command, Command::Move { x: 5, y: 7});
        assert_eq!(person.command2, Command::Quit);
        assert_eq!(person.command3, Command::Shoot(10.5, 9.75));
        assert_eq!(person.custom_tag_enum_1, CustomTagEnum::Unnamed);
        assert_eq!(person.custom_tag_enum_2, CustomTagEnum::StructVariant { x: 1, y: 2 });
        assert_eq!(person.ext_tag_enum, ExternallyTaggedEnum::Variant(10, 20));
        assert_eq!(person.ext_tag_enum_unnamed, ExternallyTaggedEnum::Unnamed);
    }
}
