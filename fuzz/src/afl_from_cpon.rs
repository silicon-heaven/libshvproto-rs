#[macro_use]
extern crate afl;

fn fuzz_fn(data: &[u8]) {
    if let Ok(s) = std::str::from_utf8(data) {
        let _x = shvproto::RpcValue::from_cpon(s);
    }
}
fn main() {
    fuzz!(|data: &[u8]| {
        fuzz_fn(data);
    });
}

