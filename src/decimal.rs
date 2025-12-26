/// mantisa: 56, exponent: 8;
/// I'm storing whole Decimal in one i64 to keep size_of RpcValue == 24
#[derive(Debug, Copy, Clone)]
pub struct Decimal(i64);

impl Decimal {

    pub fn new(mantissa: i64, exponent: i8) -> Decimal {
        let mut n = mantissa << 8;
        n |= (exponent as i64) & 0xff;
        Decimal(n)
    }
    pub fn normalize(&self) -> Decimal {
        let (mut mantissa, mut exponent) = self.decode();
        if mantissa == 0 {
            return Decimal::new(0, 0);
        }


        while mantissa != 0 && mantissa % 10 == 0 {
            mantissa /= 10;
            exponent += 1;
        }

        Decimal::new(mantissa, exponent)
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


use std::cmp::Ordering;

impl Ord for Decimal {
    fn cmp(&self, other: &Self) -> Ordering {
        let mut a = self.normalize();
        let mut b = other.normalize();

        if a.exponent() == b.exponent() {
            return a.mantissa().cmp(&b.mantissa());
        }

        let (to_scale, not_to_scale) = if a.exponent() > b.exponent() {
            (&mut a, &mut b)
        } else {
            (&mut b, &mut a)
        };
        let exponent_diff = (not_to_scale.exponent() - to_scale.exponent()).abs();
        if let Some(scaled) = 10_i64.checked_pow(exponent_diff as u32).and_then(|multiply_by| to_scale.mantissa().checked_mul(multiply_by))  {
            *to_scale = Decimal::new(scaled, to_scale.exponent());
            return a.mantissa().cmp(&b.mantissa());
        }

        let da = a.to_f64();
        let db = b.to_f64();

        if da < db {
            return Ordering::Less;
        }
        if da > db {
            return Ordering::Greater;
        }

        // We can't actually be sure whether the Decimals are equal here, because double does not have strong ordering.
        Ordering::Equal
    }
}

impl PartialOrd for Decimal {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Decimal {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for Decimal {}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decimal_normalization_removes_trailing_zeros() {
        let d1 = Decimal::new(1000, -3);
        let n1 = Decimal::normalize(&d1);
        assert_eq!(n1.mantissa(), 1);
        assert_eq!(n1.exponent(), 0);

        let d2 = Decimal::new(1200, -3);
        let n2 = Decimal::normalize(&d2);
        assert_eq!(n2.mantissa(), 12);
        assert_eq!(n2.exponent(), -1);

        let d3 = Decimal::new(500, -1);
        let n3 = Decimal::normalize(&d3);
        assert_eq!(n3.mantissa(), 5);
        assert_eq!(n3.exponent(), 1);
    }

    #[test]
    fn decimal_normalization_zero() {
        let zero = Decimal::new(0, -10);
        let n = Decimal::normalize(&zero);
        assert_eq!(n.mantissa(), 0);
        assert_eq!(n.exponent(), 0);
    }

    #[test]
    fn decimal_normalization_negative() {
        let d = Decimal::new(-5000, -3);
        let n = Decimal::normalize(&d);
        assert_eq!(n.mantissa(), -5);
        assert_eq!(n.exponent(), 0);
    }

    #[test]
    fn decimal_normalization_preserves_value() {
        let d = Decimal::new(1200, -3);
        let n = Decimal::normalize(&d);
        let diff = (d.to_f64() - n.to_f64()).abs();
        assert!(diff < 1e-12);
    }

    #[test]
    fn decimal_equality_and_normalization() {
        let d1 = Decimal::new(100, -2);
        let d2 = Decimal::new(1, 0);
        let d3 = Decimal::new(10, -1);

        assert_eq!(d1, d2);
        assert_eq!(d2, d3);
        assert!(d1 <= d3);
        assert!(d1 >= d3);
    }

    #[test]
    fn decimal_less_and_greater() {
        let d1 = Decimal::new(100, -2);
        let smaller = Decimal::new(5, -1);
        let larger = Decimal::new(2, 0);

        assert!(smaller < d1);
        assert!(d1 > smaller);
        assert!(d1 < larger);
        assert!(larger > d1);
        assert!(d1 <= larger);
        assert!(larger >= d1);
    }

    #[test]
    fn decimal_negative_numbers() {
        let neg1 = Decimal::new(-5, 0);
        let neg2 = Decimal::new(-50, -1);
        let pos = Decimal::new(5, 0);

        assert_eq!(neg1, neg2);
        assert!(neg1 < pos);
        assert!(pos > neg2);
    }

    #[test]
    fn decimal_zero_edge_cases() {
        let zero1 = Decimal::new(0, 0);
        let zero2 = Decimal::new(0, 5);

        assert_eq!(zero1, zero2);
        assert!(!(zero1 < zero2));
        assert!(!(zero1 > zero2));
        assert!(zero1 <= zero2);
        assert!(zero1 >= zero2);
    }

    #[test]
    fn decimal_large_exponents() {
        let big1 = Decimal::new(1, 3);
        let big2 = Decimal::new(1000, 0);
        let big3 = Decimal::new(1, 4);

        assert_eq!(big1, big2);
        assert!(big1 < big3);
        assert!(big3 > big2);
    }
}
