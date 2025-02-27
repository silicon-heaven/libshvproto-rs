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

pub use libshvproto_macros::TryFromRpcValue;

fn to_hex(b: u8) -> u8 {
    if b < 10 {
        b'0' + b
    }
    else {
        b'a' + (b - 10)
    }
}

