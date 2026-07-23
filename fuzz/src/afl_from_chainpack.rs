#[macro_use]
extern crate afl;

fn fuzz_fn(data: &[u8]) {
    let _x = shvproto::RpcValue::from_chainpack(data);
}
fn main() {
    fuzz!(|data: &[u8]| {
        fuzz_fn(data);
    });
}

