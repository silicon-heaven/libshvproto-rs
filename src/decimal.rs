//use crate::rpcvalue::RpcValue;

/// mantisa: 56, exponent: 8;
/// I'm storing whole Decimal in one i64 to keep size_of RpcValue == 24
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Decimal(i64);

impl Decimal {

    pub fn new(mantissa: i64, exponent: i8) -> Decimal {
        let mut n = mantissa << 8;
        n |= (exponent as i64) & 0xff;
        Decimal(n)
    }
    pub fn decode(&self) -> (i64, i8) {
        let m = self.0 >> 8;
        let e = self.0 as i8;
        (m, e)
    }
    pub fn mantissa(&self) -> i64 {
        self.decode().0
    }
    pub fn exponent(&self) -> i8 {
        self.decode().1
    }
    pub fn to_cpon_string(&self) -> String {
        let mut neg = false;
        let (mut mantissa, exponent) = self.decode();
        if mantissa < 0 {
            mantissa = -mantissa;
            neg = true;
        }
        //let buff: Vec<u8> = Vec::new();
        let mut s = mantissa.to_string();

        let n = s.len() as i8;
        let dec_places = -exponent;
        if dec_places > 0 && dec_places < n {
            // insert decimal point
            let dot_ix = n - dec_places;
            s.insert(dot_ix as usize, '.');
        }
        else if dec_places > 0 && dec_places <= 3 {
            // prepend 0.00000..
            let extra_0_cnt = dec_places - n;
            s = "0.".to_string()
                + &*"0".repeat(extra_0_cnt as usize)
                + &*s;
        }
        else if dec_places < 0 && n + exponent <= 9 {
            // append ..000000.
            s += &*"0".repeat(exponent as usize);
            s.push('.');
        }
        else if dec_places == 0 {
            // just append decimal point
            s.push('.');
        }
        else {
            // exponential notation
            s.push('e');
            s += &*exponent.to_string();
        }
        if neg {
            s.insert(0, '-');
        }
        s
    }
    pub fn to_f64(&self) -> f64 {
        let decoded = self.decode();
        let mut d = decoded.0 as f64;
        let exp = decoded.1;
        // We probably don't want to call .cmp() because of performance loss
        #[allow(clippy::comparison_chain)]
        if exp < 0 {
            for _ in exp .. 0 {
                d /= 10.;
            }
        }
        else if exp > 0 {
            for _ in 0 .. exp {
                d *= 10.;
            }
        }
        d
    }
}


