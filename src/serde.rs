mod ser;
mod de;

use std::collections::BTreeMap;
use std::ops::Deref;
use std::ops::DerefMut;

pub use ser::ValueSerializer;
pub use ser::to_rpcvalue;
pub use de::ValueDeserializer;
pub use de::from_rpcvalue;

use crate::Value;

#[derive(Clone,Debug,serde::Serialize,PartialEq)]
pub struct IMap<T: Into<Value>>(BTreeMap<i32, T>);

impl<T: Into<Value>> Deref for IMap<T> {
    type Target = BTreeMap<i32, T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Into<Value>> DerefMut for IMap<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: Into<Value>> From<BTreeMap<i32, T>> for IMap<T> {
    fn from(value: BTreeMap<i32, T>) -> Self {
        Self(value)
    }
}

impl<T: Into<Value>> From<IMap<T>> for BTreeMap<i32, T> {
    fn from(value: IMap<T>) -> Self {
        value.0
    }
}

#[derive(Clone,Debug,PartialEq)]
pub struct Blob(Vec<u8>);

impl Deref for Blob {
    type Target = Vec<u8>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Blob {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<Vec<u8>> for Blob {
    fn from(value: Vec<u8>) -> Self {
        Self(value)
    }
}

impl<const N: usize> From<[u8; N]> for Blob {
    fn from(value: [u8; N]) -> Self {
        Self(value.into())
    }
}

impl From<&[u8]> for Blob {
    fn from(value: &[u8]) -> Self {
        Self(value.into())
    }
}

impl From<Blob> for Vec<u8> {
    fn from(value: Blob) -> Self {
        value.0
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use super::{Blob, IMap};


    #[derive(Debug, serde::Serialize, serde::Deserialize, PartialEq)]
    #[serde(untagged)]
    enum UntaggedEnum {
        Color { r: u8, g: u8, b: u8 },
    }

    #[derive(Debug, serde::Serialize, serde::Deserialize, PartialEq)]
    #[serde(tag = "t")]
    enum InternallyTaggedEnum {
        Color { r: u8, g: u8, b: u8 },
    }

    #[derive(Debug, serde::Serialize, serde::Deserialize, PartialEq)]
    #[serde(tag = "t", content = "c")]
    enum InternallyTaggedExtraContentEnum {
        Color { r: u8, g: u8, b: u8 },
    }

    #[derive(Debug, serde::Serialize, serde::Deserialize, PartialEq)]
    enum ExternallyTaggedEnum {
        Color { r: u8, g: u8, b: u8 },
        Point2D(f64, f64),
        Unit,
    }

    #[derive(Debug, serde::Serialize, serde::Deserialize, PartialEq)]
    struct UserStruct {
        string: String,
        num: i8,
        flag: bool,
        map: BTreeMap<String, i32>,
        imap: IMap<String>,
        date_time: crate::DateTime,
        list: Vec<i32>,
        blob: Blob,
        untagged: UntaggedEnum,
        internally_tagged: InternallyTaggedEnum,
        internally_tagged_extra_content: InternallyTaggedExtraContentEnum,
        externally_tagged: ExternallyTaggedEnum,
        externally_tagged_tuple: ExternallyTaggedEnum,
        externally_tagged_unit: ExternallyTaggedEnum,
    }

    #[test]
    fn serialize_and_deserialize() {
        let date_time = crate::DateTime::from_iso_str("2025-07-31T18:51:00.220+02").unwrap();
        let blob = [1_u8, 2_u8, 3_u8];
        let user_from = UserStruct {
            string: "test".into(),
            num: 42,
            flag: true,
            map: BTreeMap::from([("abc".into(), 123)]),
            imap: IMap(BTreeMap::from([(1, "xyz".into())])),
            date_time,
            list: [10, 20, 30].into(),
            blob: blob.into(),
            untagged: UntaggedEnum::Color { r: 100, g: 200, b: 250 },
            internally_tagged: InternallyTaggedEnum::Color { r: 100, g: 200, b: 250 },
            internally_tagged_extra_content: InternallyTaggedExtraContentEnum::Color { r: 100, g: 200, b: 250 },
            externally_tagged: ExternallyTaggedEnum::Color { r: 100, g: 200, b: 250 },
            externally_tagged_tuple: ExternallyTaggedEnum::Point2D(0.5, 0.75),
            externally_tagged_unit: ExternallyTaggedEnum::Unit,
        };
        let user_to: UserStruct = super::from_rpcvalue(&super::to_rpcvalue(&user_from).unwrap()).unwrap();
        assert_eq!(user_from, user_to);

    }
}
