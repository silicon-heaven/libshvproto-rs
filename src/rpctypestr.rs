//! SHV RPC type description, see <https://silicon-heaven.github.io/shv-doc/rpctypes.html>

/// SHV RPC type description, see <https://silicon-heaven.github.io/shv-doc/rpctypes.html>
///
/// Use `#[derive(RpcTypeStr)]` to generate it for parameter structs and enums.
pub trait RpcTypeStr {
    const RPC_TYPE_STR: &'static str;
}

pub use libshvproto_macros::RpcTypeStr;

macro_rules! impl_rpc_type {
    ($rpc_type:literal: $($ty:ty),+) => {
        $(impl RpcTypeStr for $ty {
            const RPC_TYPE_STR: &'static str = $rpc_type;
        })+
    };
}

impl_rpc_type!("b": bool);
impl_rpc_type!("i": i8, i16, i32, i64, isize);
impl_rpc_type!("u": u16, u32, u64, usize);
impl_rpc_type!("f": f32, f64);
impl_rpc_type!("s": String, &str);
impl_rpc_type!("x": Vec<u8>);
// chrono serializes itself as an RFC 3339 string, fields sent over RPC have to use
// `#[serde(with = "shvproto::datetime::chrono_datetime")]` or `chrono_datetime_opt`
// to be SHV DateTime
impl_rpc_type!("t": crate::DateTime, chrono::DateTime<chrono::FixedOffset>);
impl_rpc_type!("?": crate::RpcValue);
