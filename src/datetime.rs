#![allow(clippy::string_slice, reason = "strings are not utf8")]

use std::cmp::Ordering;
use std::fmt;
use chrono::{FixedOffset, NaiveDate, NaiveDateTime, NaiveTime, Offset, TimeZone};

/// msec: 57, tz: 7;
/// tz is stored as signed count of quarters of hour (15 min)
/// I'm storing whole DateTime in one i64 to keep size_of RpcValue == 24
const TZ_MASK: i64 = 127;
pub enum IncludeMilliseconds {
    Never,
    Always,
    WhenNonZero,
}
pub struct ToISOStringOptions {
    pub(crate) include_millis: IncludeMilliseconds,
    pub(crate) include_timezone: bool,
}
impl Default for ToISOStringOptions {
    fn default() -> Self {
        ToISOStringOptions {
            include_millis: IncludeMilliseconds::Always,
            include_timezone: true
        }
    }
}
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct DateTime(i64);
impl DateTime {

#[cfg(feature = "serde")]
    pub(crate) fn from_inner(inner: i64) -> Self {
        Self(inner)
    }

#[cfg(feature = "serde")]
    pub(crate) fn to_inner(self) -> i64 {
        self.0
    }

    pub fn now() -> DateTime {
        let dt = chrono::offset::Local::now();
        let msec = dt.naive_utc().and_utc().timestamp_millis();
        let offset = dt.offset().local_minus_utc() / 60 / 15;
        DateTime::from_epoch_msec_tz(msec, offset)
    }

    pub fn from_datetime<Tz: chrono::TimeZone>(dt: &chrono::DateTime<Tz>) -> DateTime {
        let msec = dt.naive_utc().and_utc().timestamp_millis();
        let offset = dt.offset().fix().local_minus_utc();
        DateTime::from_epoch_msec_tz(msec, offset)
    }
    pub fn from_naive_datetime(dt: &chrono::NaiveDateTime) -> DateTime {
        let msec = dt.and_utc().timestamp_millis();
        DateTime::from_epoch_msec(msec)
    }
    pub fn from_epoch_msec_tz(epoch_msec: i64, utc_offset_sec: i32) -> DateTime {
        let mut msec = epoch_msec;
        // offset in quarters of hour
        msec *= TZ_MASK + 1;
        let offset = i64::from(utc_offset_sec / 60 / 15);
        msec |= offset & TZ_MASK;
        DateTime(msec)
    }
    pub fn from_epoch_msec(epoch_msec: i64) -> DateTime {
        Self::from_epoch_msec_tz(epoch_msec, 0)
    }
    pub fn from_iso_str(iso_str: &str) -> Result<DateTime, String> {
        const BASE_LEN: usize = 19;
        let b = iso_str.as_bytes();
        let invalid_datetime = || format!("Invalid DateTime: '{iso_str}'");

        // Check if we have at least enough characters for a timestamp without a ms or timezone.
        let Some((base, mut rest)) = b.split_at_checked(BASE_LEN) else {
            return Err(invalid_datetime());
        };

        fn parse_u32(slice: &[u8]) -> Option<u32> {
            let mut value = 0;
            for &byte in slice {
                if !byte.is_ascii_digit() {
                    return None;
                }
                value = value * 10 + u32::from(byte - b'0');
            }
            Some(value)
        }

        fn take_u32(slice: &mut &[u8], len: usize) -> Option<u32> {
            let (head, tail) = slice.split_at_checked(len)?;
            *slice = tail;
            parse_u32(head)
        }

        let &[
            y0, y1, y2, y3, b'-',
            mo0, mo1, b'-',
            d0, d1, b'T',
            h0, h1, b':',
            mi0, mi1, b':',
            s0, s1,
        ] = base else {
            return Err(invalid_datetime());
        };

        let year = parse_u32(&[y0, y1, y2, y3]).ok_or_else(invalid_datetime)?;
        let month = parse_u32(&[mo0, mo1]).ok_or_else(invalid_datetime)?;
        let day = parse_u32(&[d0, d1]).ok_or_else(invalid_datetime)?;

        let naive_date = NaiveDate::from_ymd_opt(year.cast_signed(), month, day).ok_or_else(invalid_datetime)?;

        let hour = parse_u32(&[h0, h1]).ok_or_else(invalid_datetime)?;
        let minute = parse_u32(&[mi0, mi1]).ok_or_else(invalid_datetime)?;
        let second = parse_u32(&[s0, s1]).ok_or_else(invalid_datetime)?;

        let invalid_datetime_msec = || format!("Parsing DateTime msec part error, in '{iso_str}'");

        let msec = if let Some((&b'.', tail)) = rest.split_first() {
            rest = tail;
            let digits = rest.iter().take(3).take_while(|&&c| c.is_ascii_digit()).count();
            if digits == 0 {
                return Err(invalid_datetime_msec());
            }
            let val = take_u32(&mut rest, digits).ok_or_else(invalid_datetime_msec)?;

            // The fractional part can have more than 3 digits of precision, we cut the rest.
            let skip = rest.iter().take_while(|b| b.is_ascii_digit()).count();
            rest = rest
                .get(skip..)
                .expect("skip comes from counting rest digits");

            match digits {
                1 => val * 100,
                2 => val * 10,
                3 => val,
                _ => unreachable!("digits capped at 3 by .take(3)"),
            }
        } else {
            0
        };

        let naive_time = NaiveTime::from_hms_milli_opt(hour, minute, second, msec).ok_or_else(invalid_datetime)?;

        let invalid_datetime_part = |part| format!("Invalid DateTime TZ part: '{part:?}', date time: '{iso_str}'");

        let offset_seconds = match rest.split_first() {
            Some((b'Z', tail)) => {
                if !tail.is_empty() {
                    return Err(invalid_datetime_part(tail));
                }
                0
            },
            Some((&sign @ (b'+' | b'-'), tail)) => {
                let sign: i32 = if sign == b'-' { -1 } else { 1 };
                match *tail {
                    [hh_0, hh_1, b':', mm_0, mm_1] | [hh_0, hh_1, mm_0, mm_1] => {
                        let hh = parse_u32(&[hh_0, hh_1]).ok_or_else(|| invalid_datetime_part(tail))?;
                        let mm = parse_u32(&[mm_0, mm_1]).ok_or_else(|| invalid_datetime_part(tail))?;
                        sign * (hh * 3600 + mm * 60).cast_signed()
                    }
                    [hh_0, hh_1] => {
                        let hh = parse_u32(&[hh_0, hh_1]).ok_or_else(|| invalid_datetime_part(tail))?;
                        sign * (hh * 3600).cast_signed()
                    }
                    _ => return Err(invalid_datetime_part(tail)),
                }
            },
            None => {
                0
            }
            _ => {
                return Err(invalid_datetime_part(rest));
            }
        };

        let tz = FixedOffset::east_opt(offset_seconds).ok_or_else(|| format!("Invalid timezone offset seconds: {offset_seconds}"))?;

        let naive_datetime = naive_date.and_time(naive_time);
        let chrono_dt = tz.from_local_datetime(&naive_datetime).single().ok_or_else(invalid_datetime)?;
        let epoch_msec = chrono_dt.timestamp_millis();

        let dt = DateTime::from_epoch_msec_tz(epoch_msec, offset_seconds);
        Ok(dt)
    }

    pub fn epoc_msec_utc_offset(self) -> (i64, i32) {
        let msec= self.0 / (TZ_MASK + 1);
        let mut offset = self.0 & TZ_MASK;
        if (offset & ((TZ_MASK + 1) / 2)) != 0 {
            // sign extension
            offset |= !TZ_MASK;
        }
        #[expect(clippy::cast_possible_truncation, reason = "We hope that the offset is small enough to fit")]
        let offset = (offset * 15 * 60) as i32;
        (msec, offset)
    }
    pub fn epoch_msec(self) -> i64 { self.epoc_msec_utc_offset().0 }
    pub fn utc_offset(self) -> i32 { self.epoc_msec_utc_offset().1 }

    pub fn to_chrono_naivedatetime(self) -> chrono::NaiveDateTime {
        let msec = self.epoch_msec();
        chrono::DateTime::from_timestamp_millis(msec).unwrap_or_default().naive_utc()
    }
    pub fn to_chrono_datetime(self) -> chrono::DateTime<chrono::offset::FixedOffset> {
        let offset = FixedOffset::east_opt(self.utc_offset())
            .unwrap_or_else(|| FixedOffset::east_opt(0).expect("Zero is within the range"));
        chrono::DateTime::from_naive_utc_and_offset(self.to_chrono_naivedatetime(), offset)
    }
    pub fn to_iso_string(self) -> String {
        self.to_iso_string_opt(&ToISOStringOptions::default())
    }
    pub fn to_iso_string_opt(self, opts: &ToISOStringOptions) -> String {
        let dt = self.to_chrono_datetime();
        let mut s = format!("{}", dt.format("%Y-%m-%dT%H:%M:%S"));
        let ms = self.epoch_msec() % 1000;
        match opts.include_millis {
            IncludeMilliseconds::Never => {}
            IncludeMilliseconds::Always => { s.push_str(&format!(".{ms:03}")); }
            IncludeMilliseconds::WhenNonZero => {
                if ms > 0 {
                    s.push_str(&format!(".{ms:03}"));
                }
            }
        }
        if opts.include_timezone {
            let mut offset = self.utc_offset();
            if offset == 0 {
                s.push('Z');
            }
            else {
                if offset < 0 {
                    s.push('-');
                    offset = -offset;
                } else {
                    s.push('+');
                }
                let offset_hr = offset / 60 / 60;
                let offset_min = offset / 60 % 60;
                s += &format!("{offset_hr:02}");
                if offset_min > 0 {
                    s += &format!("{offset_min:02}");
                }
            }
        }
        s
    }

    #[must_use]
    pub fn add_days(self, days: i64) -> Self {
        let (msec, offset) = self.epoc_msec_utc_offset();
        Self::from_epoch_msec_tz(msec + (days * 24 * 60 * 60 * 1000), offset)
    }
    #[must_use]
    pub fn add_hours(self, hours: i64) -> Self {
        let (msec, offset) = self.epoc_msec_utc_offset();
        Self::from_epoch_msec_tz(msec + (hours * 60 * 60 * 1000), offset)
    }
    #[must_use]
    pub fn add_minutes(self, minutes: i64) -> Self {
        let (msec, offset) = self.epoc_msec_utc_offset();
        Self::from_epoch_msec_tz(msec + (minutes * 60 * 1000), offset)
    }
    #[must_use]
    pub fn add_seconds(self, seconds: i64) -> Self {
        let (msec, offset) = self.epoc_msec_utc_offset();
        Self::from_epoch_msec_tz(msec + (seconds * 1000), offset)
    }
    #[must_use]
    pub fn add_millis(self, millis: i64) -> Self {
        let (msec, offset) = self.epoc_msec_utc_offset();
        Self::from_epoch_msec_tz(msec + millis, offset)
    }
}

impl PartialOrd for DateTime {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Eq for DateTime {}

impl Ord for DateTime {
    fn cmp(&self, other: &Self) -> Ordering {
        let e1 = self.epoch_msec();
        let e2 = other.epoch_msec();
        e1.cmp(&e2)
    }
}

impl fmt::Display for DateTime {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_iso_string())
    }
}

impl From<NaiveDateTime> for DateTime {
    fn from(ndt: NaiveDateTime) -> Self {
        DateTime::from_naive_datetime(&ndt)
    }
}

#[cfg(test)]
mod test {
    use super::DateTime;

    const MINUTE: i32 = 60;
    const HOUR: i32 = 60 * MINUTE;

    #[test]
    fn from_iso_str_parses_timezone_forms() {
        for (input, expected) in [
            ("2021-11-08T01:02:03", DateTime::from_epoch_msec_tz(1_636_333_323_000, 0)),
            ("2021-11-08T01:02:03Z", DateTime::from_epoch_msec_tz(1_636_333_323_000, 0)),
            ("2021-11-08T01:02:03+05", DateTime::from_epoch_msec_tz(1_636_315_323_000, 5 * HOUR)),
            ("2021-11-08T01:02:03+05:30", DateTime::from_epoch_msec_tz(1_636_313_523_000, 5 * HOUR + 30 * MINUTE)),
            ("2021-11-08T01:02:03-0815", DateTime::from_epoch_msec_tz(1_636_363_023_000, -8 * HOUR - 15 * MINUTE)),
        ] {
            assert_eq!(DateTime::from_iso_str(input), Ok(expected));
        }
    }

    #[test]
    fn from_iso_str_parses_fractional_milliseconds() {
        for (input, expected) in [
            ("2021-11-08T01:02:03.1Z", DateTime::from_epoch_msec_tz(1_636_333_323_100, 0)),
            ("2021-11-08T01:02:03.12Z", DateTime::from_epoch_msec_tz(1_636_333_323_120, 0)),
            ("2021-11-08T01:02:03.123Z", DateTime::from_epoch_msec_tz(1_636_333_323_123, 0)),
            ("2021-11-08T01:02:03.1234Z", DateTime::from_epoch_msec_tz(1_636_333_323_123, 0)),
        ] {
            assert_eq!(DateTime::from_iso_str(input), Ok(expected));
        }
    }

    #[test]
    fn from_iso_str_rejects_invalid_inputs() {
        for input in [
            "2021-11-08T01:02:03.",
            "2021-11-08T01:02:03+",
            "2021-11-08T01:02:03+0",
            "2021-11-08T01:02:03+050",
            "2021-11-08T01:02:03+05:0",
            "2021-11-08T01:02:03+05:00x",
            "2021-11-08T01:02:03X",
            "2021/11/08T01:02:03Z",
            "2021-02-29T01:02:03Z",
            "2021-11-08T24:00:00Z",
        ] {
            assert!(DateTime::from_iso_str(input).is_err());
        }
    }
}
