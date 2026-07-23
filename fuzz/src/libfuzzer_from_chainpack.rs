#![no_main]

use libfuzzer_sys::fuzz_target;
use shvproto::RpcValue;

fn fuzz_fn(data: &[u8]) {
    let _x = RpcValue::from_chainpack(data);
}

fuzz_target!(|data: &[u8]| {
    fuzz_fn(data);
});
