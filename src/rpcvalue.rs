use std::collections::BTreeMap;
use std::fmt;

use crate::chainpack::ChainPackReader;
use crate::chainpack::ChainPackWriter;
use crate::decimal;
use crate::metamap::MetaMap;
use crate::reader::Reader;
use crate::writer::Writer;
use crate::CponWriter;
use crate::{datetime, DateTime, Decimal};
use crate::{CponReader, ReadResult};
use std::sync::OnceLock;

// see https://github.com/rhysd/tinyjson/blob/master/src/json_value.rs

const EMPTY_STR_REF: &str = "";
const EMPTY_BYTES_REF: &[u8] = EMPTY_STR_REF.as_bytes();
static EMPTY_LIST: OnceLock<List> = OnceLock::new();
static EMPTY_MAP: OnceLock<Map> = OnceLock::new();
static EMPTY_IMAP: OnceLock<IMap> = OnceLock::new();
static EMPTY_METAMAP: OnceLock<MetaMap> = OnceLock::new();

#[macro_export(local_inner_macros)]
macro_rules! make_map {
	($( $key: expr => $val: expr ),* $(,)?) => {{
		 let mut map = $crate::rpcvalue::Map::new();
		 $( map.insert($key.to_string(), $crate::RpcValue::from($val)); )*
		 map
	}}
}

pub type Blob = Vec<u8>;
pub type List = Vec<RpcValue>;
pub type Map = BTreeMap<String, RpcValue>;
pub type IMap = BTreeMap<i32, RpcValue>;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    Int(i64),
    UInt(u64),
    Double(f64),
    Bool(bool),
    DateTime(datetime::DateTime),
    Decimal(decimal::Decimal),
    String(Box<String>),
    Blob(Box<Blob>),
    List(Box<List>),
    Map(Box<Map>),
    IMap(Box<IMap>),
}

impl Value {
    pub fn type_name(&self) -> &'static str {
        match &self {
            Value::Null => "Null",
            Value::Int(_) => "Int",
            Value::UInt(_) => "UInt",
            Value::Double(_) => "Double",
            Value::Bool(_) => "Bool",
            Value::DateTime(_) => "DateTime",
            Value::Decimal(_) => "Decimal",
            Value::String(_) => "String",
            Value::Blob(_) => "Blob",
            Value::List(_) => "List",
            Value::Map(_) => "Map",
            Value::IMap(_) => "IMap",
        }
    }
    pub fn is_default_value(&self) -> bool {
        match &self {
            Value::Null => true,
            Value::Int(i) => *i == 0,
            Value::UInt(u) => *u == 0,
            Value::Double(d) => *d == 0.0,
            Value::Bool(b) => !(*b),
            Value::DateTime(dt) => dt.epoch_msec() == 0,
            Value::Decimal(d) => d.mantissa() == 0,
            Value::String(s) => s.is_empty(),
            Value::Blob(b) => b.is_empty(),
            Value::List(l) => l.is_empty(),
            Value::Map(m) => m.is_empty(),
            Value::IMap(m) => m.is_empty(),
        }
    }
}

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Value::Null
    }
}
impl From<bool> for Value {
    fn from(val: bool) -> Self {
        Value::Bool(val)
    }
}
impl From<&str> for Value {
    fn from(val: &str) -> Self {
        Value::String(Box::new(val.to_string()))
    }
}
impl From<String> for Value {
    fn from(val: String) -> Self {
        Value::String(Box::new(val))
    }
}
impl From<&String> for Value {
    fn from(val: &String) -> Self {
        Value::String(Box::new(val.clone()))
    }
}
impl From<Vec<u8>> for Value {
    fn from(val: Vec<u8>) -> Self {
        Value::Blob(Box::new(val))
    }
}
impl From<&[u8]> for Value {
    fn from(val: &[u8]) -> Self {
        Value::Blob(Box::new(val.to_vec()))
    }
}
impl From<i32> for Value {
    fn from(val: i32) -> Self {
        Value::Int(val.into())
    }
}
impl From<i64> for Value {
    fn from(val: i64) -> Self {
        Value::Int(val)
    }
}
impl From<isize> for Value {
    fn from(val: isize) -> Self {
        Value::Int(val as i64)
    }
}
impl From<u32> for Value {
    fn from(val: u32) -> Self {
        Value::UInt(val.into())
    }
}
impl From<u64> for Value {
    fn from(val: u64) -> Self {
        Value::UInt(val)
    }
}
impl From<usize> for Value {
    fn from(val: usize) -> Self {
        Value::UInt(val as u64)
    }
}
impl From<f64> for Value {
    fn from(val: f64) -> Self {
        Value::Double(val)
    }
}
impl From<Decimal> for Value {
    fn from(val: Decimal) -> Self {
        Value::Decimal(val)
    }
}
impl From<List> for Value {
    fn from(val: List) -> Self {
        Value::List(Box::new(val))
    }
}
impl From<Map> for Value {
    fn from(val: Map) -> Self {
        Value::Map(Box::new(val))
    }
}
impl From<IMap> for Value {
    fn from(val: IMap) -> Self {
        Value::IMap(Box::new(val))
    }
}
impl From<datetime::DateTime> for Value {
    fn from(val: datetime::DateTime) -> Self {
        Value::DateTime(val)
    }
}
impl From<chrono::NaiveDateTime> for Value {
    fn from(val: chrono::NaiveDateTime) -> Self {
        Value::DateTime(DateTime::from_naive_datetime(&val))
    }
}
impl<Tz: chrono::TimeZone> From<chrono::DateTime<Tz>> for Value {
    fn from(item: chrono::DateTime<Tz>) -> Self {
        Value::DateTime(datetime::DateTime::from_datetime(&item))
    }
}

macro_rules! impl_from_type_for_rpcvalue {
    ($from:ty) => {
        impl From<$from> for RpcValue {
            fn from(value: $from) -> Self {
                $crate::RpcValue {
                    meta: None,
                    value: value.into(),
                }
            }
        }
    };
}

impl_from_type_for_rpcvalue!(());
impl_from_type_for_rpcvalue!(bool);
impl_from_type_for_rpcvalue!(&str);
impl_from_type_for_rpcvalue!(String);
impl_from_type_for_rpcvalue!(&String);
impl_from_type_for_rpcvalue!(&[u8]);
impl_from_type_for_rpcvalue!(i32);
impl_from_type_for_rpcvalue!(i64);
impl_from_type_for_rpcvalue!(isize);
impl_from_type_for_rpcvalue!(u32);
impl_from_type_for_rpcvalue!(u64);
impl_from_type_for_rpcvalue!(f64);
impl_from_type_for_rpcvalue!(Decimal);
impl_from_type_for_rpcvalue!(DateTime);
impl_from_type_for_rpcvalue!(chrono::NaiveDateTime);

impl<Tz: chrono::TimeZone> From<chrono::DateTime<Tz>> for RpcValue {
    fn from(value: chrono::DateTime<Tz>) -> Self {
        RpcValue {
            meta: None,
            value: value.into(),
        }
    }
}

impl From<Vec<u8>> for RpcValue {
    fn from(val: Vec<u8>) -> Self {
        RpcValue {
            meta: None,
            value: Value::Blob(Box::new(val))
        }
    }
}

impl<T> From<Option<T>> for RpcValue
where
    RpcValue: From<T>,
{
    fn from(value: Option<T>) -> Self {
        value.map_or_else(RpcValue::null, RpcValue::from)
    }
}

#[cfg(feature = "specialization")]
mod with_specialization {
    use super::{
        from_vec_rpcvalue_for_rpcvalue,
        from_map_rpcvalue_for_rpcvalue,
        from_imap_rpcvalue_for_rpcvalue
    };
    use crate::RpcValue;
    use std::collections::BTreeMap;
    use super::{List,Map,IMap};

    impl<T: Into<RpcValue>> From<Vec<T>> for RpcValue {
        default fn from(value: Vec<T>) -> Self {
            from_vec_rpcvalue_for_rpcvalue(value)
        }
    }
    impl<T: Into<RpcValue>> From<BTreeMap<String, T>> for RpcValue {
        default fn from(value: BTreeMap<String, T>) -> Self {
            from_map_rpcvalue_for_rpcvalue(value)
        }
    }
    impl<T: Into<RpcValue>> From<BTreeMap<i32, T>> for RpcValue {
        default fn from(value: BTreeMap<i32, T>) -> Self {
            from_imap_rpcvalue_for_rpcvalue(value)
        }
    }

    // Specializations of `impl From<Collection<RpcValue>> for RpcValue`
    // for List, Map and IMap to just move the value instead of iterating
    // through the collections.
    impl_from_type_for_rpcvalue!(List);
    impl_from_type_for_rpcvalue!(Map);
    impl_from_type_for_rpcvalue!(IMap);
}


#[cfg(not(feature = "specialization"))]
mod without_specialization {
    use super::{
        from_vec_rpcvalue_for_rpcvalue,
        from_map_rpcvalue_for_rpcvalue,
        from_imap_rpcvalue_for_rpcvalue}
    ;
    use crate::RpcValue;
    use std::collections::BTreeMap;


    impl<T: Into<RpcValue>> From<Vec<T>> for RpcValue {
        fn from(value: Vec<T>) -> Self {
            from_vec_rpcvalue_for_rpcvalue(value)
        }
    }

    impl<T: Into<RpcValue>> From<BTreeMap<String, T>> for RpcValue {
        fn from(value: BTreeMap<String, T>) -> Self {
            from_map_rpcvalue_for_rpcvalue(value)
        }
    }

    impl<T: Into<RpcValue>> From<BTreeMap<i32, T>> for RpcValue {
        fn from(value: BTreeMap<i32, T>) -> Self {
            from_imap_rpcvalue_for_rpcvalue(value)
        }
    }
}

fn from_vec_rpcvalue_for_rpcvalue<T: Into<RpcValue>>(value: Vec<T>) -> RpcValue {
    RpcValue {
        meta: None,
        value: Value::List(Box::new(
                value
                .into_iter()
                .map(Into::into)
                .collect::<Vec<_>>()
        ))
    }
}

fn from_map_rpcvalue_for_rpcvalue<T: Into<RpcValue>>(value: BTreeMap<String, T>) -> RpcValue {
    RpcValue {
        meta: None,
        value: Value::Map(Box::new(
                value
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .collect::<BTreeMap<_,_>>()
        ))
    }
}

fn from_imap_rpcvalue_for_rpcvalue<T: Into<RpcValue>>(value: BTreeMap<i32, T>) -> RpcValue {
    RpcValue {
        meta: None,
        value: Value::IMap(Box::new(
                value
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .collect::<BTreeMap<_,_>>()
        ))
    }
}



fn format_err_try_from(expected_type: &str, actual_type: &str) -> String {
    format!("Expected type `{expected_type}`, got `{actual_type}`")
}

impl TryFrom<&Value> for () {
    type Error = String;
    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Null => Ok(()),
            _ => Err(format_err_try_from("Null",value.type_name()))
        }
    }
}

impl TryFrom<Value> for () {
    type Error = String;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        Self::try_from(&value)
    }
}

impl TryFrom<&Value> for bool {
    type Error = String;
    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Bool(val) => Ok(*val),
            _ => Err(format_err_try_from("Bool", value.type_name()))
        }
    }
}

impl TryFrom<Value> for bool {
    type Error = String;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        Self::try_from(&value)
    }
}

impl<'a> TryFrom<&'a Value> for &'a str {
    type Error = String;
    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(val) => Ok(val.as_str()),
            _ => Err(format_err_try_from("String", value.type_name()))
        }
    }
}

impl<'a> TryFrom<&'a Value> for &'a String {
    type Error = String;
    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(val) => Ok(val.as_ref()),
            _ => Err(format_err_try_from("String", value.type_name()))
        }
    }
}

impl TryFrom<&Value> for String {
    type Error = String;
    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(val) => Ok(*val.clone()),
            _ => Err(format_err_try_from("String", value.type_name()))
        }
    }
}

impl TryFrom<Value> for String {
    type Error = String;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(val) => Ok(*val),
            _ => Err(format_err_try_from("String", value.type_name()))
        }
    }
}

impl<'a> TryFrom<&'a Value> for &'a Vec<u8> {
    type Error = String;
    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        match value {
            Value::Blob(val) => Ok(val),
            _ => Err(format_err_try_from("Blob", value.type_name()))
        }
    }
}

impl TryFrom<&Value> for Vec<u8> {
    type Error = String;
    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Blob(val) => Ok(*val.clone()),
            _ => Err(format_err_try_from("Blob", value.type_name()))
        }
    }
}

impl TryFrom<Value> for Vec<u8> {
    type Error = String;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Blob(val) => Ok(*val),
            _ => Err(format_err_try_from("Blob", value.type_name()))
        }
    }
}

impl<'a> TryFrom<&'a Value> for &'a [u8] {
    type Error = String;
    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        match value {
            Value::Blob(val) => Ok(val.as_slice()),
            _ => Err(format_err_try_from("Blob", value.type_name()))
        }
    }
}

macro_rules! try_from_integral_value {
    ($target:ty) => {
        impl TryFrom<&Value> for $target {
            type Error = String;
            fn try_from(value: &Value) -> Result<Self, Self::Error> {
                match value {
                    Value::Int(val) => <$target>::try_from(*val).map_err(|e| e.to_string()),
                    Value::UInt(val) => <$target>::try_from(*val).map_err(|e| e.to_string()),
                    _ => Err(format_err_try_from("Int or UInt", value.type_name()))
                }
            }
        }

        impl TryFrom<Value> for $target {
            type Error = String;
            fn try_from(value: Value) -> Result<Self, Self::Error> {
                Self::try_from(&value)
            }
        }
    };
}

try_from_integral_value!(i32);
try_from_integral_value!(i64);
try_from_integral_value!(isize);
try_from_integral_value!(usize);
try_from_integral_value!(u32);
try_from_integral_value!(u64);

impl TryFrom<&Value> for f64 {
    type Error = String;
    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Double(val) => Ok(*val),
            _ => Err(format_err_try_from("Double", value.type_name()))
        }
    }
}

impl TryFrom<Value> for f64 {
    type Error = String;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        Self::try_from(&value)
    }
}

impl TryFrom<&Value> for Decimal {
    type Error = String;
    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Decimal(val) => Ok(*val),
            _ => Err(format_err_try_from("Decimal", value.type_name()))
        }
    }
}

impl TryFrom<Value> for Decimal {
    type Error = String;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        Self::try_from(&value)
    }
}

impl<'a> TryFrom<&'a Value> for &'a List {
    type Error = String;
    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        match value {
            Value::List(val) => Ok(val),
            _ => Err(format_err_try_from("List", value.type_name()))
        }
    }
}

impl TryFrom<&Value> for List {
    type Error = String;
    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::List(val) => Ok(*val.clone()),
            _ => Err(format_err_try_from("List", value.type_name()))
        }
    }
}

impl TryFrom<Value> for List {
    type Error = String;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::List(val) => Ok(*val),
            _ => Err(format_err_try_from("List", value.type_name()))
        }
    }
}

impl<'a> TryFrom<&'a Value> for &'a Map {
    type Error = String;
    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        match value {
            Value::Map(val) => Ok(val),
            _ => Err(format_err_try_from("Map", value.type_name()))
        }
    }
}

impl TryFrom<&Value> for Map {
    type Error = String;
    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Map(val) => Ok(*val.clone()),
            _ => Err(format_err_try_from("Map", value.type_name()))
        }
    }
}

impl TryFrom<Value> for Map {
    type Error = String;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Map(val) => Ok(*val),
            _ => Err(format_err_try_from("Map", value.type_name()))
        }
    }
}

impl<'a> TryFrom<&'a Value> for &'a IMap {
    type Error = String;
    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        match value {
            Value::IMap(val) => Ok(val),
            _ => Err(format_err_try_from("IMap", value.type_name()))
        }
    }
}

impl TryFrom<&Value> for IMap {
    type Error = String;
    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::IMap(val) => Ok(*val.clone()),
            _ => Err(format_err_try_from("IMap", value.type_name()))
        }
    }
}

impl TryFrom<Value> for IMap {
    type Error = String;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::IMap(val) => Ok(*val),
            _ => Err(format_err_try_from("IMap", value.type_name()))
        }
    }
}

impl TryFrom<&Value> for datetime::DateTime {
    type Error = String;
    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::DateTime(val) => Ok(*val),
            _ => Err(format_err_try_from("DateTime", value.type_name()))
        }
    }
}

impl TryFrom<Value> for datetime::DateTime {
    type Error = String;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        Self::try_from(&value)
    }
}

impl TryFrom<&Value> for chrono::NaiveDateTime {
    type Error = String;
    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::DateTime(val) => Ok(val.to_chrono_naivedatetime()),
            _ => Err(format_err_try_from("DateTime", value.type_name()))
        }
    }
}

impl TryFrom<Value> for chrono::NaiveDateTime {
    type Error = String;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        Self::try_from(&value)
    }
}

impl TryFrom<&Value> for chrono::DateTime<chrono::FixedOffset> {
    type Error = String;
    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::DateTime(val) => Ok(val.to_chrono_datetime()),
            _ => Err(format_err_try_from("DateTime", value.type_name()))
        }
    }
}

impl TryFrom<Value> for chrono::DateTime<chrono::FixedOffset> {
    type Error = String;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        Self::try_from(&value)
    }
}

// Cannot use generic trait implementation here.
// See `rustc --explain E0210`
macro_rules! try_from_rpc_value_ref {
    ($target:ty) => {
        impl<'a> TryFrom<&'a RpcValue> for $target {
            type Error = String;
            fn try_from(value: &'a RpcValue) -> Result<Self, Self::Error> {
                let inner = &value.value;
                inner.try_into()
            }
        }
    };
}

macro_rules! try_from_rpc_value {
    ($target:ty) => {
        impl TryFrom<RpcValue> for $target {
            type Error = String;
            fn try_from(value: RpcValue) -> Result<Self, Self::Error> {
                value.value.try_into()
            }
        }
    };
}

try_from_rpc_value_ref!(());
try_from_rpc_value_ref!(bool);
try_from_rpc_value!(bool);
try_from_rpc_value_ref!(&'a str);
try_from_rpc_value_ref!(&'a String);
try_from_rpc_value_ref!(String);
try_from_rpc_value!(String);
try_from_rpc_value_ref!(&'a Vec<u8>);
try_from_rpc_value_ref!(Vec<u8>);
try_from_rpc_value!(Vec<u8>);
try_from_rpc_value_ref!(&'a [u8]);
try_from_rpc_value_ref!(i32);
try_from_rpc_value!(i32);
try_from_rpc_value_ref!(i64);
try_from_rpc_value!(i64);
try_from_rpc_value_ref!(isize);
try_from_rpc_value!(isize);
try_from_rpc_value_ref!(usize);
try_from_rpc_value!(usize);
try_from_rpc_value_ref!(u32);
try_from_rpc_value!(u32);
try_from_rpc_value_ref!(u64);
try_from_rpc_value!(u64);
try_from_rpc_value_ref!(f64);
try_from_rpc_value!(f64);
try_from_rpc_value_ref!(Decimal);
try_from_rpc_value!(Decimal);
try_from_rpc_value_ref!(&'a List);
try_from_rpc_value_ref!(List);
try_from_rpc_value!(List);
try_from_rpc_value_ref!(&'a Map);
try_from_rpc_value_ref!(Map);
try_from_rpc_value!(Map);
try_from_rpc_value_ref!(&'a IMap);
try_from_rpc_value_ref!(IMap);
try_from_rpc_value!(IMap);
try_from_rpc_value_ref!(datetime::DateTime);
try_from_rpc_value!(datetime::DateTime);
try_from_rpc_value_ref!(chrono::NaiveDateTime);
try_from_rpc_value!(chrono::NaiveDateTime);
try_from_rpc_value_ref!(chrono::DateTime<chrono::FixedOffset>);
try_from_rpc_value!(chrono::DateTime<chrono::FixedOffset>);

impl<T> TryFrom<&RpcValue> for Vec<T>
where
    T: for<'a> TryFrom<&'a RpcValue, Error = String>,
{
    type Error = String;

    fn try_from(value: &RpcValue) -> Result<Self, Self::Error> {
        let mut res = Self::new();
        for (idx,val) in List::try_from(value)?.iter().enumerate() {
            res.push(val
                .try_into()
                .map_err(|e| format!("Wrong item at index {idx}: {e}"))?
            );
        }
        Ok(res)
    }
}

impl<T> TryFrom<RpcValue> for Vec<T>
where
    T: TryFrom<RpcValue, Error = String>,
{
    type Error = String;

    fn try_from(value: RpcValue) -> Result<Self, Self::Error> {
        let mut res = Self::new();
        for (idx, val) in List::try_from(value)?.into_iter().enumerate() {
            res.push(val
                .try_into()
                .map_err(|e| format!("Wrong item at index {idx}: {e}"))?
            );
        }
        Ok(res)
    }
}

impl<T> TryFrom<&RpcValue> for BTreeMap<String, T>
where
    T: for<'a> TryFrom<&'a RpcValue, Error = String>,
{
    type Error = String;

    fn try_from(value: &RpcValue) -> Result<Self, Self::Error> {
        let mut res = Self::new();
        for (key, val) in Map::try_from(value)?.iter() {
            res.insert(
                key.to_owned(),
                val.try_into()
                .map_err(|e| format!("Wrong item at key `{key}`: {e}"))?
            );
        }
        Ok(res)
    }
}

impl<T> TryFrom<RpcValue> for BTreeMap<String, T>
where
    T: TryFrom<RpcValue, Error = String>,
{
    type Error = String;

    fn try_from(value: RpcValue) -> Result<Self, Self::Error> {
        let mut res = Self::new();
        for (key, val) in Map::try_from(value)?.into_iter() {
            let val = val.try_into().map_err(|e| format!("Wrong item at key `{key}`: {e}"))?;
            res.insert(key,val);
        }
        Ok(res)
    }
}

impl<T> TryFrom<&RpcValue> for BTreeMap<i32, T>
where
    T: for<'a> TryFrom<&'a RpcValue, Error = String>,
{
    type Error = String;

    fn try_from(value: &RpcValue) -> Result<Self, Self::Error> {
        let mut res = Self::new();
        for (key, val) in IMap::try_from(value)?.iter() {
            res.insert(
                *key,
                val.try_into()
                .map_err(|e| format!("Wrong item at key `{key}`: {e}"))?
            );
        }
        Ok(res)
    }
}

impl<T> TryFrom<RpcValue> for BTreeMap<i32, T>
where
    T: TryFrom<RpcValue, Error = String>,
{
    type Error = String;

    fn try_from(value: RpcValue) -> Result<Self, Self::Error> {
        let mut res = Self::new();
        for (key, val) in IMap::try_from(value)?.into_iter() {
            res.insert(
                key,
                val.try_into()
                .map_err(|e| format!("Wrong item at key `{key}`: {e}"))?
            );
        }
        Ok(res)
    }
}

macro_rules! is_xxx {
    ($name:ident, $variant:pat) => {
        pub fn $name(&self) -> bool {
            match self.value() {
                $variant => true,
                _ => false,
            }
        }
    };
}

pub enum GetKey<'a> {
    Int(i32),
    Str(&'a str),
}
pub trait GetIndex {
    fn make_key(&self) -> GetKey;
}
impl GetIndex for &str {
    fn make_key(&self) -> GetKey {
        GetKey::Str(self)
    }
}
impl GetIndex for i32 {
    fn make_key(&self) -> GetKey {
        GetKey::Int(*self)
    }
}
impl GetIndex for u32 {
    fn make_key(&self) -> GetKey {
        GetKey::Int(*self as i32)
    }
}
impl GetIndex for usize {
    fn make_key(&self) -> GetKey {
        GetKey::Int(*self as i32)
    }
}

#[derive(PartialEq, Clone)]
pub struct RpcValue {
    meta: Option<Box<MetaMap>>,
    value: Value,
}

impl RpcValue {
    pub fn null() -> RpcValue {
        RpcValue {
            meta: None,
            value: Value::Null,
        }
    }
    pub fn new(v: Value, m: Option<MetaMap>) -> Self {
        RpcValue {
            meta: m.map(Box::new),
            value: v,
        }
    }
    /*
    pub fn new<I>(val: I) -> RpcValue
        where I: FromValue
    {
        RpcValue {
            meta: None,
            value: val.chainpack_make_value(),
        }
    }
    pub fn new_with_meta<I>(val: I, meta: Option<MetaMap>) -> RpcValue
        where I: FromValue
    {
        let mm = match meta {
            None => None,
            Some(m) => Some(Box::new(m)),
        };
        RpcValue {
            meta: mm,
            value: val.chainpack_make_value(),
        }
    }
        pub fn set_meta(&mut self, m: MetaMap) {
        if m.is_empty() {
            self.meta = None;
        }
        else {
            self.meta = Some(Box::new(m));
        }
    }
    */
    pub fn set_meta(mut self, meta: Option<MetaMap>) -> Self {
        self.meta = meta.map(Box::new);
        self
    }
    pub fn has_meta(&self) -> bool {
        self.meta.is_some()
    }
    pub fn meta(&self) -> &MetaMap {
        self.meta.as_ref().map_or_else(
            || EMPTY_METAMAP.get_or_init(MetaMap::new),
            <Box<MetaMap>>::as_ref,
        )
    }
    pub fn meta_mut(&mut self) -> Option<&mut MetaMap> {
        self.meta.as_mut().map(<Box<MetaMap>>::as_mut)
    }
    pub fn clear_meta(&mut self) {
        self.meta = None;
    }

    pub fn value(&self) -> &Value {
        &self.value
    }
    pub fn value_mut(&mut self) -> &mut Value {
        &mut self.value
    }

    pub fn type_name(&self) -> &'static str {
        self.value.type_name()
    }

    is_xxx!(is_null, Value::Null);
    is_xxx!(is_bool, Value::Bool(_));
    is_xxx!(is_int, Value::Int(_));
    is_xxx!(is_string, Value::String(_));
    is_xxx!(is_blob, Value::Blob(_));
    is_xxx!(is_list, Value::List(_));
    is_xxx!(is_map, Value::Map(_));
    is_xxx!(is_imap, Value::IMap(_));

    pub fn is_default_value(&self) -> bool {
        self.value.is_default_value()
    }
    pub fn as_bool(&self) -> bool {
        match &self.value {
            Value::Bool(d) => *d,
            _ => false,
        }
    }
    pub fn as_int(&self) -> i64 {
        self.as_i64()
    }
    pub fn as_i64(&self) -> i64 {
        match &self.value {
            Value::Int(d) => *d,
            Value::UInt(d) => *d as i64,
            _ => 0,
        }
    }
    pub fn as_i32(&self) -> i32 {
        self.as_i64() as i32
    }
    pub fn as_u64(&self) -> u64 {
        match &self.value {
            Value::Int(d) => *d as u64,
            Value::UInt(d) => *d,
            _ => 0,
        }
    }
    pub fn as_u32(&self) -> u32 {
        self.as_u64() as u32
    }
    pub fn as_f64(&self) -> f64 {
        match &self.value {
            Value::Double(d) => *d,
            _ => 0.,
        }
    }
    pub fn as_usize(&self) -> usize {
        match &self.value {
            Value::Int(d) => *d as usize,
            Value::UInt(d) => *d as usize,
            _ => 0_usize,
        }
    }
    pub fn as_datetime(&self) -> datetime::DateTime {
        match &self.value {
            Value::DateTime(d) => *d,
            _ => datetime::DateTime::from_epoch_msec(0),
        }
    }
    pub fn to_datetime(&self) -> Option<datetime::DateTime> {
        match &self.value {
            Value::DateTime(d) => Some(*d),
            _ => None,
        }
    }
    pub fn as_decimal(&self) -> decimal::Decimal {
        match &self.value {
            Value::Decimal(d) => *d,
            _ => decimal::Decimal::new(0, 0),
        }
    }
    // pub fn as_str(&self) -> Result<&str, Utf8Error> {
    // 	match &self.value {
    // 		Value::String(b) => std::str::from_utf8(b),
    // 		_ => std::str::from_utf8(EMPTY_BYTES_REF),
    // 	}
    // }
    pub fn as_blob(&self) -> &[u8] {
        match &self.value {
            Value::Blob(b) => b,
            _ => EMPTY_BYTES_REF,
        }
    }
    pub fn as_str(&self) -> &str {
        match &self.value {
            Value::String(b) => b,
            _ => EMPTY_STR_REF,
        }
    }
    pub fn as_list(&self) -> &Vec<RpcValue> {
        match &self.value {
            Value::List(b) => b,
            _ => EMPTY_LIST.get_or_init(List::new),
        }
    }
    pub fn as_map(&self) -> &Map {
        match &self.value {
            Value::Map(b) => b,
            _ => EMPTY_MAP.get_or_init(Map::new),
        }
    }
    pub fn as_imap(&self) -> &BTreeMap<i32, RpcValue> {
        match &self.value {
            Value::IMap(b) => b,
            _ => EMPTY_IMAP.get_or_init(IMap::new),
        }
    }
    pub fn get<I>(&self, key: I) -> Option<&RpcValue>
    where
        I: GetIndex,
    {
        match key.make_key() {
            GetKey::Int(ix) => match &self.value {
                Value::List(lst) => lst.get(ix as usize),
                Value::IMap(map) => map.get(&ix),
                _ => None,
            },
            GetKey::Str(ix) => match &self.value {
                Value::Map(map) => map.get(ix),
                _ => None,
            },
        }
    }
    pub fn to_cpon(&self) -> String {
        self.to_cpon_indented("")
    }
    pub fn to_cpon_indented(&self, indent: &str) -> String {
        let buff = self.to_cpon_bytes_indented(indent.as_bytes());
        String::from_utf8(buff).unwrap_or_else(|_| "".to_string())
    }
    pub fn to_cpon_bytes_indented(&self, indent: &[u8]) -> Vec<u8> {
        let mut buff: Vec<u8> = Vec::new();
        let mut wr = CponWriter::new(&mut buff);
        wr.set_indent(indent);
        let r = wr.write(self);
        r.map_or_else(|_| Vec::new(), |_| buff)
    }
    pub fn to_chainpack(&self) -> Vec<u8> {
        let mut buff: Vec<u8> = Vec::new();
        let mut wr = ChainPackWriter::new(&mut buff);
        let r = wr.write(self);
        r.map_or_else(|_| Vec::new(), |_| buff)
    }

    pub fn from_cpon(s: &str) -> ReadResult {
        let mut buff = s.as_bytes();
        let mut rd = CponReader::new(&mut buff);
        rd.read()
    }
    pub fn from_chainpack(b: &[u8]) -> ReadResult {
        let mut buff = b;
        let mut rd = ChainPackReader::new(&mut buff);
        rd.read()
    }
}
static NULL_RPCVALUE: OnceLock<RpcValue> = OnceLock::new();

impl Default for &RpcValue {
    fn default() -> Self {
        NULL_RPCVALUE.get_or_init(RpcValue::null)
    }
}
impl fmt::Debug for RpcValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        //write!(f, "RpcValue {{meta: {:?} value: {:?}}}", self.meta, self.value)
        write!(f, "{}", self.to_cpon())
    }
}
impl fmt::Display for RpcValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_cpon())
    }
}

#[cfg(test)]
mod test {
    use std::collections::BTreeMap;
    use std::mem::size_of;

    use chrono::Offset;

    use crate::metamap::MetaMap;
    use crate::rpcvalue::{IMap, Map, RpcValue, Value};
    use crate::DateTime;
    use crate::Decimal;

    macro_rules! show_size {
        (header) => {
            log::debug!("{:<22} {:>4}    ", "Type", "T");
            log::debug!("------------------------------");
        };
        ($t:ty) => {
            log::debug!("{:<22} {:4}", stringify!($t), size_of::<$t>())
        };
    }

    #[test]
    fn size() {
        show_size!(header);
        show_size!(usize);
        show_size!(MetaMap);
        show_size!(Box<MetaMap>);
        show_size!(Option<MetaMap>);
        show_size!(Option<Box<MetaMap>>);
        show_size!(Value);
        show_size!(Option<Value>);
        show_size!(RpcValue);
    }

    #[test]
    fn rpcval_new() {
        let rv = RpcValue::from(true);
        assert!(rv.as_bool());
        let rrv = &rv;
        assert_eq!(rrv.try_into(), Ok(true));
        assert_eq!(rv.try_into(), Ok(true));
        let rv = RpcValue::from("foo");
        assert_eq!(rv.as_str(), "foo");
        let rrv = &rv;
        assert_eq!(rrv.try_into(), Ok("foo"));
        let rv = RpcValue::from(&b"bar"[..]);
        assert_eq!(rv.as_blob(), b"bar");
        let rrv = &rv;
        assert_eq!(rrv.try_into(), Ok(b"bar".as_slice()));
        let rv = RpcValue::from(123);
        assert_eq!(rv.as_i32(), 123);
        let rrv = &rv;
        assert_eq!(rrv.try_into(), Ok(123));
        assert_eq!(rv.try_into(), Ok(123));
        let rv = RpcValue::from(12.3);
        assert_eq!(rv.as_f64(), 12.3);
        let rrv = &rv;
        assert_eq!(rrv.try_into(), Ok(12.3));
        assert_eq!(rv.try_into(), Ok(12.3));

        let dt = DateTime::now();
        let rv = RpcValue::from(dt);
        assert_eq!(rv.as_datetime(), dt);
        let rrv = &rv;
        assert_eq!(rrv.try_into(), Ok(dt));
        assert_eq!(rv.try_into(), Ok(dt));

        let dc = Decimal::new(123, -1);
        let rv = RpcValue::from(dc);
        assert_eq!(rv.as_decimal(), dc);
        let rrv = &rv;
        assert_eq!(rrv.try_into(), Ok(dc));
        assert_eq!(rv.try_into(), Ok(dc));

        let dt = chrono::offset::Utc::now();
        let rv = RpcValue::from(dt);
        assert_eq!(rv.as_datetime().epoch_msec(), dt.timestamp_millis());
        let rrv = &rv;
        assert_eq!(rrv.try_into(), Ok(DateTime::from_datetime(&dt)));
        assert_eq!(rv.try_into(), Ok(DateTime::from_datetime(&dt)));

        let dt = chrono::offset::Local::now();
        let rv = RpcValue::from(dt);
        assert_eq!(
            rv.as_datetime().epoch_msec() + rv.as_datetime().utc_offset() as i64 * 1000,
            dt.timestamp_millis() + dt.offset().fix().local_minus_utc() as i64 * 1000
        );

        let vec1 = vec![RpcValue::from(123), RpcValue::from("foo")];
        let rv = RpcValue::from(vec1.clone());
        assert_eq!(rv.as_list(), &vec1);
        let rrv = &rv;
        assert_eq!(rrv.try_into(), Ok(&vec1));
        assert_eq!(rv.try_into(), Ok(vec1));

        let m = [
            ("foo".to_string(), RpcValue::from(123)),
            ("bar".to_string(), RpcValue::from("foo"))
        ].into_iter().collect::<Map>();
        let rv = RpcValue::from(m.clone());
        assert_eq!(rv.as_map(), &m);
        let rrv = &rv;
        assert_eq!(rrv.try_into(), Ok(&m));
        assert_eq!(rv.try_into(), Ok(m));

        let m = [
            (1, RpcValue::from(123)),
            (2, RpcValue::from("foo"))
        ].into_iter().collect::<IMap>();
        let rv = RpcValue::from(m.clone());
        assert_eq!(rv.as_imap(), &m);
        let rrv = &rv;
        assert_eq!(rrv.try_into(), Ok(&m));
        assert_eq!(rv.try_into(), Ok(m));

        let vec1 = vec![123_i32, 456_i32];
        let rv = RpcValue::from(vec1.iter().copied().map(RpcValue::from).collect::<Vec<RpcValue>>());
        let rrv = &rv;
        assert_eq!(rrv.try_into(), Ok(vec1.clone()));
        assert_eq!(rv.try_into(), Ok(vec1));

        let m = [
            ("foo".to_owned(), 123_i32),
            ("bar".to_owned(), 456_i32)
        ].into_iter().collect::<BTreeMap<_,_>>();
        let rv = RpcValue::from(
            m.iter().map(|(k, &v)| (k.clone(), v.into())).collect::<Map>());
        let rrv = &rv;
        assert_eq!(rrv.try_into(), Ok(m.clone()));
        assert_eq!(rv.try_into(), Ok(m));

        let m = [
            (1, std::f64::consts::E),
            (7, std::f64::consts::PI)
        ].into_iter().collect::<BTreeMap<_,_>>();
        let rv = RpcValue::from(
            m.iter().map(|(&k, &v)| (k, v.into())).collect::<IMap>());
        let rrv = &rv;
        assert_eq!(rrv.try_into(), Ok(m.clone()));
        assert_eq!(rv.try_into(), Ok(m));


        // Collection -> RpcValue -> Collection

        let v = vec![RpcValue::from(1i32), RpcValue::from(2i32)];
        assert_eq!(Ok(v.clone()), RpcValue::from(v).try_into());

        let v = vec![1i32, 2i32];
        assert_eq!(Ok(v.clone()), RpcValue::from(v).try_into());

        let v = [
            "x",
            "yy",
            "zzz",
        ].into_iter().map(String::from).collect::<Vec<_>>();
        assert_eq!(Ok(v.clone()), RpcValue::from(v).try_into());

        let v = [
            ("xxx".to_owned(), RpcValue::from(1_i32)),
            ("yyy".to_owned(), RpcValue::from(2_i32)),
        ].into_iter().collect::<BTreeMap<_,_>>();
        assert_eq!(Ok(v.clone()), RpcValue::from(v).try_into());

        let v = [
            ("xxx".to_owned(), 1_i32),
            ("yyy".to_owned(), 2_i32),
        ].into_iter().collect::<BTreeMap<_,_>>();
        assert_eq!(Ok(v.clone()), RpcValue::from(v).try_into());

        let v = [
            (1, RpcValue::from(1_i32)),
            (2, RpcValue::from(2_i32)),
        ].into_iter().collect::<BTreeMap<_,_>>();
        assert_eq!(Ok(v.clone()), RpcValue::from(v).try_into());

        let v = [
            (1, 1_i32),
            (2, 2_i32),
        ].into_iter().collect::<BTreeMap<_,_>>();
        assert_eq!(Ok(v.clone()), RpcValue::from(v).try_into());
    }
}
