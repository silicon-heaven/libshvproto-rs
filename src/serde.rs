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

#[derive(Clone,Debug)]
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
