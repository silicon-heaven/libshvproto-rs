use std::{cmp::Ordering, collections::{BTreeMap, btree_map::Entry}, ops::{Add, Div, Mul, Neg, Rem, Sub}};

use jaq_all::jaq_core::{Error, Exn, Native, RunPtr, data::JustLut, native::Fun, ops};

use crate::{RpcValue, Value};

pub type ValR = jaq_all::jaq_core::ValR<RpcValue>;
pub type ValX<'a> = jaq_all::jaq_core::ValX<'a, RpcValue>;
impl Neg for RpcValue {
    type Output = ValR;
    fn neg(mut self) -> Self::Output {
        match &mut self.value {
            Value::Bool(bool) => *bool = !*bool,
            Value::Int(num) => *num = -*num,
            _ => return Err(jaq_all::jaq_core::Error::typ(self, "")),
        }
        Ok(self)
    }
}

impl Add for RpcValue {
    type Output = ValR;
    fn add(mut self, mut rhs: Self) -> Self::Output {
        match (&mut self.value, &mut rhs.value) {
            (Value::Int(x), Value::Int(y)) => *x += *y,
            (Value::UInt(x), Value::UInt(y)) => *x += *y,
            (Value::UInt(x), Value::Int(y)) => *x = (x.cast_signed() + *y).cast_unsigned(),
            (Value::Map(x), Value::Map(y)) => x.append(y),
            _=> return Err(Error::math(self, ops::Math::Add, rhs))
        }

        Ok(self)
    }
}

impl Sub for RpcValue {
    type Output = ValR;
    fn sub(mut self, rhs: Self) -> Self::Output {
        match (&mut self.value, &rhs.value) {
            (Value::Int(x), Value::Int(y)) => *x -= y,
            (Value::UInt(x), Value::UInt(y)) => *x -= y,
            (Value::UInt(x), Value::Int(y)) => *x = (x.cast_signed() - *y).cast_unsigned(),
            _=> return Err(Error::math(self, ops::Math::Sub, rhs))
        }

        Ok(self)
    }
}

impl Mul for RpcValue {
    type Output = ValR;
    fn mul(mut self, rhs: Self) -> Self::Output {
        match (&mut self.value, &rhs.value) {
            (Value::Int(x), Value::Int(y)) => *x *= y,
            (Value::UInt(x), Value::UInt(y)) => *x *= y,
            (Value::UInt(x), Value::Int(y)) => *x = (x.cast_signed() * *y).cast_unsigned(),
            _=> return Err(Error::math(self, ops::Math::Mul, rhs))
        }

        Ok(self)
    }
}

impl Div for RpcValue {
    type Output = ValR;
    fn div(mut self, rhs: Self) -> Self::Output {
        match (&mut self.value, &rhs.value) {
            (Value::Int(x), Value::Int(y)) => *x /=y,
            (Value::UInt(x), Value::UInt(y)) => *x /= y,
            (Value::UInt(x), Value::Int(y)) => *x = (x.cast_signed() / *y).cast_unsigned(),
            _=> return Err(Error::math(self, ops::Math::Div, rhs))
        }

        Ok(self)
    }
}

impl Rem for RpcValue {
    type Output = ValR;
    fn rem(mut self, rhs: Self) -> Self::Output {
        match (&mut self.value, &rhs.value) {
            (Value::Int(x), Value::Int(y)) => *x %= y,
            _=> return Err(Error::math(self, ops::Math::Rem, rhs))
        }

        Ok(self)
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
        match self.value {
            Value::Double(double) => Some(double),
            Value::Decimal(decimal) => Some(decimal.to_f64()),
            _ => None,
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
            (_s, _) => Err(Error::typ(self, "index")),
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

    fn map_values<'a, I: Iterator<Item = ValX<'a>>>(mut self, opt: jaq_all::jaq_core::path::Opt, f: impl Fn(Self) -> I,
    ) -> ValX<'a> {
        match self.value {
            Value::List(lst) => {
                self.value = (*lst)
                    .into_iter()
                    .flat_map(f)
                    .collect::<Result<Vec<_>,_>>()
                    .map(Into::into)?;
            }
            Value::Map(map) => {
                self.value = (*map)
                    .into_iter()
                    .filter_map(|(k, v)| f(v)
                        .next()
                        .map(|v| v.map(|v| (k, v))))
                    .collect::<Result<BTreeMap<_,_>,_>>()
                    .map(Into::into)?;
            }
            Value::IMap(map) => {
                self.value = (*map)
                    .into_iter()
                    .filter_map(|(k, v)| f(v)
                        .next()
                        .map(|v| v.map(|v| (k, v))))
                    .collect::<Result<BTreeMap<_,_>,_>>()
                    .map(Into::into)?;
            }
            _ => return opt.fail(self, |v| jaq_all::jaq_core::Exn::from(Error::typ(v, "map_values"))),
        }

        Ok(self)
    }

    fn map_index<'a, I: Iterator<Item = ValX<'a>>>(
        mut self,
        index: &Self,
        opt: jaq_all::jaq_core::path::Opt,
        f: impl Fn(Self) -> I,
    ) -> ValX<'a> {
        if let (Value::String(..) | Value::List(..), Value::Map(o)) = (&self.value, &index.value) {
            let range = o.get("start")..o.get("end");
            return self.map_range(range, opt, f);
        }
        match &mut self.value {
            Value::Map(map) => {
                let Value::String(index) = &index.value else {
                    return opt.fail(self, |v| jaq_all::jaq_core::Exn::from(Error::typ(v, "map_index")))
                };
                match map.as_mut().entry(index.to_string()) {
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
            },
            #[expect(clippy::cast_possible_truncation, reason = "For now, we hope that usizes are 64-bit")]
            Value::List(lst) => {
                let Value::Int(index) = &index.value else {
                    return opt.fail(self, |v| jaq_all::jaq_core::Exn::from(Error::typ(v, "map_index")))
                };
                // FIXME: Add support for negative keys.
                #[expect(clippy::cast_sign_loss, reason = "Indexing is signed, but we epxect insigned keys")]
                let index = *index as usize;
                let Some(x) = lst.get_mut(index) else {
                    return opt.fail(self, |oof| Exn::from(Error::typ(oof, "")));
                };

                if let Some(y) = f(x.clone()).next().transpose()? {
                    *x = y;
                } else {
                    lst.remove(index);
                }

            },
            _ => return opt.fail(self, |v| jaq_all::jaq_core::Exn::from(Error::typ(v, "map_index"))),
        }

        Ok(self)
    }

    fn map_range<'a, I: Iterator<Item = ValX<'a>>>(
        mut self,
        range: jaq_all::jaq_core::val::Range<&Self>,
        opt: jaq_all::jaq_core::path::Opt,
        f: impl Fn(Self) -> I,
    ) -> ValX<'a> {
        let start = range.start.map_or(0, RpcValue::as_usize);
        let end = range.end.map_or(0, RpcValue::as_usize);
        match &mut self.value {
            Value::List(lst) => {
                let Some(elems) = lst.get_mut(start..end) else {
                    return opt.fail(self, |oof| Exn::from(Error::index(oof, crate::make_list!(start, end).into())));
                };

                for elem in elems {
                    if let Some(y) = f(elem.clone()).next().transpose()? {
                        *elem = y;
                    } else {
                        return opt.fail(self, |_oof| Exn::from(Error::new("map_range doesn't support removing elements".into())));
                    }
                }
            },
            _ => return opt.fail(self, |v| jaq_all::jaq_core::Exn::from(Error::typ(v, "map_range"))),
        }

        Ok(self)
    }

    fn as_bool(&self) -> bool {
        self.as_bool()
    }

    fn into_string(self) -> Self {
        self.to_cpon().into()
    }
}

pub fn base() -> Box<[jaq_all::jaq_core::native::Filter<RunPtr<JustLut<RpcValue>>>]> {
    Box::new([
        ("typename", jaq_all::jaq_core::native::v(0), |(_, val)| {
            Box::new(core::iter::once(Ok(val.type_name().into())))
        })
    ])
}

pub fn funs() -> impl Iterator<Item = Fun<JustLut<RpcValue>>> {
    base().into_vec().into_iter().map(|(name, arity, fun)| (name, arity, Native::<JustLut<RpcValue>>::new(fun)))
}
