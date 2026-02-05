use std::{cmp::Ordering, collections::{BTreeMap, btree_map::Entry}, ops::{Add, Div, Mul, Neg, Rem, Sub}};

use jaq_all::jaq_core::{Error, Exn, ops};

use crate::{RpcValue, Value};

pub type ValR = jaq_all::jaq_core::ValR<RpcValue>;
pub type ValX = jaq_all::jaq_core::ValX<RpcValue>;
impl Neg for RpcValue {
    type Output = ValR;
    fn neg(self) -> Self::Output {
        match &self.value {
            Value::Int(num) => Ok((-num).into()),
            _ => Err(jaq_all::jaq_core::Error::typ(self, "")),
        }
    }
}

impl Add for RpcValue {
    type Output = ValR;
    fn add(self, rhs: Self) -> Self::Output {
        match (&self.value, &rhs.value) {
            (Value::Int(x), Value::Int(y)) => Ok((x + y).into()),
            (Value::UInt(x), Value::UInt(y)) => Ok((x + y).into()),
            _=> Err(Error::math(self, ops::Math::Add, rhs))
        }
    }
}

impl Sub for RpcValue {
    type Output = ValR;
    fn sub(self, rhs: Self) -> Self::Output {
        match (&self.value, &rhs.value) {
            (Value::Int(x), Value::Int(y)) => Ok((x - y).into()),
            (Value::UInt(x), Value::UInt(y)) => Ok((x - y).into()),
            _=> Err(Error::math(self, ops::Math::Sub, rhs))
        }
    }
}

impl Mul for RpcValue {
    type Output = ValR;
    fn mul(self, rhs: Self) -> Self::Output {
        match (&self.value, &rhs.value) {
            (Value::Int(x), Value::Int(y)) => Ok((x * y).into()),
            _=> Err(Error::math(self, ops::Math::Mul, rhs))
        }
    }
}

impl Div for RpcValue {
    type Output = ValR;
    fn div(self, rhs: Self) -> Self::Output {
        match (&self.value, &rhs.value) {
            (Value::Int(x), Value::Int(y)) => Ok((x / y).into()),
            _=> Err(Error::math(self, ops::Math::Div, rhs))
        }
    }
}

impl Rem for RpcValue {
    type Output = ValR;
    fn rem(self, rhs: Self) -> Self::Output {
        match (&self.value, &rhs.value) {
            (Value::Int(x), Value::Int(y)) => Ok((x % y).into()),
            _=> Err(Error::math(self, ops::Math::Rem, rhs))
        }
    }
}

impl PartialOrd for RpcValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Eq for RpcValue {}

impl Ord for RpcValue {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use Ordering::{Equal, Greater, Less};
        match (&self.value, &other.value) {
            (Value::Bool(x), Value::Bool(y)) => x.cmp(y),
            (Value::Int(x), Value::Int(y)) => x.cmp(y),
            (Value::UInt(x), Value::UInt(y)) => x.cmp(y),
            (Value::DateTime(x), Value::DateTime(y)) => x.cmp(y),
            (Value::Decimal(x), Value::Decimal(y)) => x.cmp(y),
            (Value::String(x), Value::String(y)) => x.cmp(y),
            (Value::Blob(x), Value::Blob(y)) => x.cmp(y),
            (Value::List(x), Value::List(y)) => x.cmp(y),
            (Value::Map(x), Value::Map(y)) => x.cmp(y),
            (Value::IMap(x), Value::IMap(y)) => x.cmp(y),
            (Value::Null, Value::Null) => Equal,
            (Value::Null, _) => Less,
            (_, Value::Null) => Greater,
            (Value::Bool(_), _) => Less,
            (_, Value::Bool(_)) => Greater,
            (Value::Int(_), _) => Less,
            (_, Value::Int(_)) => Greater,
            (Value::UInt(_), _) => Less,
            (_, Value::UInt(_)) => Greater,
            (Value::Double(_), _) => Less,
            (_, Value::Double(_)) => Greater,
            (Value::Decimal(_), _) => Less,
            (_, Value::Decimal(_)) => Greater,
            (Value::DateTime(_), _) => Less,
            (_, Value::DateTime(_)) => Greater,
            (Value::String(_), _) => Less,
            (_, Value::String(_)) => Greater,
            (Value::Blob(_), _) => Less,
            (_, Value::Blob(_)) => Greater,
            (Value::List(_), _) => Less,
            (_, Value::List(_)) => Greater,
            (Value::IMap(_), _) => Less,
            (_, Value::IMap(_)) => Greater,
        }
    }
}

impl FromIterator<RpcValue> for RpcValue {
    fn from_iter<T: IntoIterator<Item = RpcValue>>(iter: T) -> Self {
        iter.into_iter().collect::<Vec<_>>().into()
    }
}

impl From<core::ops::Range<Option<RpcValue>>> for RpcValue {
    fn from(value: core::ops::Range<Option<RpcValue>>) -> Self {
        let kv = |(k, v): (&str, Option<_>)| v.map(|v| (k.to_owned(), v));
        let kvs = [("start", value.start), ("end", value.end)];
        kvs.into_iter().filter_map(kv).collect::<BTreeMap<String,_>>().into()
    }
}

impl From<usize> for RpcValue {
    fn from(value: usize) -> Self {
        (value as u64).into()
    }
}

impl jaq_all::jaq_std::ValT for RpcValue {
    fn into_seq<S: FromIterator<Self>>(self) -> Result<S, Self> {
        match self.value {
            Value::List(list) => Ok(list.into_iter().collect()),
            _ => Err(self)
        }
    }

    fn is_int(&self) -> bool {
        self.is_int()
    }

    fn as_isize(&self) -> Option<isize> {
        match self.value {
            #[expect(clippy::cast_possible_truncation, reason = "We assume pointer size is 64-bit")]
            Value::Int(num) => Some(num as isize),
            _ => None,
        }
    }

    fn as_f64(&self) -> Option<f64> {
        if let Value::Double(double) = self.value {
            Some(double)
        } else {
            None
        }
    }

    fn is_utf8_str(&self) -> bool {
        self.is_string()
    }

    fn as_bytes(&self) -> Option<&[u8]> {
        if let Value::String(str) = &self.value {
            Some(str.as_bytes())
        } else {
            None
        }
    }

    fn as_sub_str(&self, sub: &[u8]) -> Self {
        if matches!(&self.value, Value::String(_)) {
            // We do not have any fancy bytes handling, so we will just believe that the sub range
            // is a substring of self, and create a string out of it.
            String::from_utf8_lossy(sub).to_string().into()
        } else {
            // jaq-json panics here, but I don't really like that, so if self is not a String, let's just give null.
            RpcValue::null()
        }
    }

    fn from_utf8_bytes(b: impl AsRef<[u8]> + Send + 'static) -> Self {
        String::from_utf8_lossy(b.as_ref()).to_string().into()
    }
}

impl jaq_all::jaq_core::ValT for RpcValue {
    fn from_num(n: &str) -> ValR {
        Ok(n.parse::<i64>().map_or_else(|_| 0_i64.into(), RpcValue::from))
    }

    fn from_map<I: IntoIterator<Item = (Self, Self)>>(iter: I) -> ValR {
        Ok(iter.into_iter().map(|(k, v)| (k.as_str().to_owned(), v)).collect::<BTreeMap<String, _>>().into())
    }

    fn key_values(self) -> jaq_all::jaq_core::box_iter::BoxIter<'static, jaq_all::jaq_core::ValR<(Self, Self), Self>> {
        match self.value {
            Value::List(list) => {
                Box::new(list
                    .into_iter()
                    .enumerate()
                    .map(|(idx, val)| Ok((RpcValue::from(idx.cast_signed()), val))))
            }
            Value::Map(map) => {
                Box::new(map
                    .into_iter()
                    .map(|(key, val)| Ok((RpcValue::from(key), val))))
            }
            Value::IMap(map) => {
                Box::new(map
                    .into_iter()
                    .map(|(key, val)| Ok((RpcValue::from(key), val))))
            }
            _ => Box::new(Err(Error::typ(self, "iterable (List or Map or IMap)")).into_iter())
        }
    }

    fn values(self) -> Box<dyn Iterator<Item = ValR>> {
        match self.value {
            Value::List(list) => {
                Box::new(list
                    .into_iter()
                    .map(Ok))
            }
            Value::Map(map) => {
                Box::new(map
                    .into_values()
                    .map(Ok))
            }
            Value::IMap(map) => {
                Box::new(map
                    .into_values()
                    .map(Ok))
            }
            _ => Box::new(Err(Error::typ(self, "iterable (List or Map or IMap)")).into_iter())
        }
    }

    fn index(self, index: &Self) -> ValR {
        match (&self.value, &index.value) {
            (Value::Null, _) => Ok(RpcValue::null()),
            #[expect(clippy::cast_possible_truncation, clippy::cast_sign_loss, reason = "We assume pointer size is 64-bit")]
            (Value::String(rv), Value::Int(i)) => Ok(rv.chars().nth(*i as usize).map(|cha| cha.to_string()).into()),
            #[expect(clippy::cast_possible_truncation, clippy::cast_sign_loss, reason = "We assume pointer size is 64-bit")]
            (Value::List(list), Value::Int(i)) => Ok((*list).get(*i as usize).cloned().unwrap_or_else(RpcValue::null)),
            (Value::Map(o), Value::String(key)) => Ok(o.get(key.as_str()).cloned().unwrap_or_else(RpcValue::null)),
            (_s, _) => Err(Error::typ(self, "")),
        }
    }

    fn range(self, range: jaq_all::jaq_core::val::Range<&Self>) -> ValR {
        let start = range.start.map_or(0, RpcValue::as_usize);
        let end = range.end.map_or(0, RpcValue::as_usize);
        match &self.value {
            Value::String(str) => {
                let bytes = str.as_bytes();
                bytes.get(start..end).map(|bytes| String::from_utf8_lossy(bytes).to_string().into()).ok_or_else(|| Error::typ(self, ".."))
            }
            Value::List(lst) => {
                lst
                    .get(start..end)
                    .map(|new_range| new_range.to_vec().into())
                    .ok_or_else(|| Error::typ(self, ".."))
            }
            _ => Err(Error::typ(self, "")),
        }
    }

    fn map_values<I: Iterator<Item = ValX>>(self, opt: jaq_all::jaq_core::path::Opt, f: impl Fn(Self) -> I,
    ) -> ValX {
        match self.value {
            Value::List(lst) => {
                lst
                    .into_iter()
                    .flat_map(f)
                    .collect::<Result<Vec<_>,_>>()
                    .map(Into::into)
            }
            Value::Map(map) => {
                map
                    .into_iter()
                    .filter_map(|(k, v)| f(v)
                        .next()
                        .map(|v| Ok((k, v?))))
                    .collect::<Result<BTreeMap<_,_>,_>>()
                    .map(Into::into)
            }
            v => opt.fail(RpcValue { meta: None, value: v }, |v| jaq_all::jaq_core::Exn::from(Error::typ(v, ""))),
        }
    }

    fn map_index<I: Iterator<Item = ValX>>(
        self,
        index: &Self,
        opt: jaq_all::jaq_core::path::Opt,
        f: impl Fn(Self) -> I,
    ) -> ValX {
        if let (Value::String(..) | Value::List(..), Value::Map(o)) = (&self.value, &index.value) {
            let range = o.get("start")..o.get("end");
            return self.map_range(range, opt, f);
        }
        match self.value {
            Value::Map(map) => {
                let mut map = *map;
                let Value::String(index) = &index.value else {
                    return opt.fail(RpcValue { meta: None, value: index.value.clone() }, |v| jaq_all::jaq_core::Exn::from(Error::typ(v, "")))
                };
                match map.entry(index.to_string()) {
                    Entry::Occupied(mut e) => {
                        let v = e.get_mut();
                        match f(v.clone()).next().transpose()? {
                            Some(y) => e.insert(y),
                            None => e.remove(),
                        };
                    },
                    Entry::Vacant(e) => {
                        if let Some(y) = f(RpcValue::null()).next().transpose()? {
                            e.insert(y);
                        }
                    },
                }
                Ok(map.into())
            },
            #[expect(clippy::cast_possible_truncation, reason = "For now, we hope that usizes are 64-bit")]
            Value::List(lst) => {
                let mut lst = *lst;
                let Value::UInt(index) = &index.value else {
                    return opt.fail(RpcValue { meta: None, value: index.value.clone() }, |v| jaq_all::jaq_core::Exn::from(Error::typ(v, "")))
                };
                let Some(x) = lst.get(*index as usize) else {
                    return opt.fail(lst.into(), |oof| Exn::from(Error::typ(oof, "")));
                };

                if let Some(y) = f(x.clone()).next().transpose()? {
                    lst.insert(*index as usize, y);
                } else {
                    lst.remove(*index as usize);
                }

                Ok(lst.into())
            },
            v => opt.fail(RpcValue { meta: None, value: v }, |v| jaq_all::jaq_core::Exn::from(Error::typ(v, ""))),
        }
    }

    fn map_range<I: Iterator<Item = ValX>>(
        self,
        _range: jaq_all::jaq_core::val::Range<&Self>,
        _opt: jaq_all::jaq_core::path::Opt,
        _f: impl Fn(Self) -> I,
    ) -> ValX {
        todo!()
    }

    fn as_bool(&self) -> bool {
        self.as_bool()
    }

    fn into_string(self) -> Self {
        self.to_cpon().into()
    }
}
