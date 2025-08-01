#![cfg_attr(feature = "specialization", feature(min_specialization))]

pub mod chainpack;
pub mod cpon;
pub mod json;
pub mod datetime;
pub mod decimal;
pub mod metamap;
pub mod reader;
pub mod rpcvalue;
pub mod util;
pub mod writer;
mod textrdwr;
#[cfg(feature = "serde")]
mod serde;

pub use datetime::DateTime;
pub use decimal::Decimal;
pub use metamap::MetaMap;
pub use reader::{ReadError, ReadResult, Reader};
pub use rpcvalue::Value;
pub use rpcvalue::{Blob, List, Map, IMap, RpcValue};
pub use writer::{WriteResult, Writer};

pub use chainpack::{ChainPackReader, ChainPackWriter};
pub use cpon::{CponReader, CponWriter};
pub use json::{JsonReader, JsonWriter};

pub use libshvproto_macros::{FromRpcValue, ToRpcValue};

#[cfg(feature = "serde")]
pub use crate::serde::{ValueSerializer, to_rpcvalue, ValueDeserializer, from_rpcvalue};

fn u8_to_hex(num: u8) -> (u8, u8) {
    let nibble_to_hex = |b| if b < 10 {
        b'0' + b
    }
    else {
        b'a' + (b - 10)
    };
    let high = num / 16;
    let low = num % 16;
    (nibble_to_hex(high), nibble_to_hex(low))
}

