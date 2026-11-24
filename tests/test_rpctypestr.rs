#![cfg(feature = "serde")]

mod tests {
    use shvproto::RpcTypeStr;
    use shvproto::datetime::{chrono_datetime, chrono_datetime_opt};
    use serde::{Deserialize, Serialize};
    use shvproto::{DateTime, RpcValue};
    use std::collections::{BTreeMap, HashMap};

    #[derive(Serialize, Deserialize, RpcTypeStr)]
    struct AllScalars {
        flag: bool,
        count: i32,
        size: u64,
        ratio: f64,
        name: String,
        data: Vec<u8>,
        created: DateTime,
        #[serde(with = "chrono_datetime")]
        modified: chrono::DateTime<chrono::FixedOffset>,
    }

    #[derive(Serialize, Deserialize, RpcTypeStr)]
    #[serde(transparent)]
    struct RunId {
        run_id: i64,
    }

    #[derive(Serialize, Deserialize, RpcTypeStr)]
    struct Renamed {
        #[serde(rename = "partId")]
        part_id: i64,
        #[serde(default)]
        note: String,
    }

    #[derive(Serialize, Deserialize, RpcTypeStr)]
    struct Nested {
        run: RunId,
        renamed: Renamed,
    }

    #[derive(Serialize, Deserialize, RpcTypeStr)]
    struct Empty {}

    #[derive(Serialize, Deserialize, RpcTypeStr)]
    struct OptionalParam {
        run_id: RunId,
        user_id: Option<String>
    }

    #[derive(Serialize, Deserialize, RpcTypeStr)]
    struct Maps {
        names: BTreeMap<String, String>,
        counts: Option<HashMap<String, i64>>,
        nested: BTreeMap<String, Option<RunId>>,
    }

    #[derive(Serialize, Deserialize, RpcTypeStr)]
    struct Lists {
        ids: Vec<i64>,
        data: Vec<u8>,
        runs: Option<Vec<Option<RunId>>>,
        matrix: Vec<Vec<String>>,
    }

    #[derive(Serialize, Deserialize, RpcTypeStr)]
    #[serde(transparent)]
    struct RenamedList {
        items: Vec<Renamed>,
    }

    #[test]
    fn scalars() {
        assert_eq!(
            AllScalars::RPC_TYPE_STR,
            "{b:flag,i:count,u:size,f:ratio,s:name,x:data,t:created,t:modified}"
        );
    }

    #[test]
    fn rpc_value_is_any() {
        assert_eq!(RpcValue::RPC_TYPE_STR, "?");
    }

    #[test]
    fn transparent() {
        assert_eq!(RunId::RPC_TYPE_STR, "i");
    }

    #[test]
    fn rename_and_other_serde_attrs() {
        assert_eq!(Renamed::RPC_TYPE_STR, "{i:partId,s:note}");
    }

    #[test]
    fn nested() {
        assert_eq!(Nested::RPC_TYPE_STR, "{i:run,{i:partId,s:note}:renamed}");
    }

    #[test]
    fn empty() {
        assert_eq!(Empty::RPC_TYPE_STR, "{}");
    }

    #[test]
    fn usable_in_const_context() {
        const PARAM: &str = Renamed::RPC_TYPE_STR;
        assert_eq!(PARAM, "{i:partId,s:note}");
    }

    #[test]
    fn optional_param() {
        assert_eq!(OptionalParam::RPC_TYPE_STR, "{i:run_id,s|n:user_id}");
    }

    #[test]
    fn lists() {
        assert_eq!(Lists::RPC_TYPE_STR, "{[i]:ids,x:data,[i|n]|n:runs,[[s]]:matrix}");
        assert_eq!(RenamedList::RPC_TYPE_STR, "[{i:partId,s:note}]");
    }

    #[test]
    fn maps() {
        assert_eq!(Maps::RPC_TYPE_STR, "{{s}:names,{i}|n:counts,{i|n}:nested}");
    }

    #[derive(Serialize, Deserialize, RpcTypeStr)]
    enum Mode {
        Archived,
        #[serde(rename = "readOnly")]
        ReadOnly,
    }

    #[derive(Serialize, Deserialize, RpcTypeStr)]
    struct WithEnum {
        mode: Mode,
        opt_mode: Option<Mode>,
    }

    #[test]
    fn unit_enum_is_int_enum() {
        assert_eq!(Mode::RPC_TYPE_STR, "s[Archived,readOnly]");
        assert_eq!(WithEnum::RPC_TYPE_STR, "{s[Archived,readOnly]:mode,s[Archived,readOnly]|n:opt_mode}");
        assert_eq!(shvproto::to_rpcvalue(&Mode::ReadOnly).unwrap().as_str(), "readOnly");
    }

    #[derive(Serialize, Deserialize, RpcTypeStr)]
    #[repr(i32)]
    enum Mixed {
        None,
        Id(i64),
        #[serde(rename = "pos")]
        Position { x: i32, #[serde(rename = "Y")] y: Option<i32> },
        Other = 123,
    }

    #[derive(Serialize, Deserialize, RpcTypeStr)]
    enum Indexed {
        Fail = -1,
        Success,
    }

    #[test]
    fn data_enum_is_externally_tagged() {
        assert_eq!(Indexed::RPC_TYPE_STR, "s[Fail,Success]");
        assert_eq!(Mixed::RPC_TYPE_STR, "s[None,Id:i,pos:{i:x,i|n:Y},Other]");
        assert_eq!(shvproto::to_rpcvalue(&Mixed::None).unwrap().to_cpon(), r#""None""#);
        assert_eq!(shvproto::to_rpcvalue(&Mixed::Id(3)).unwrap().to_cpon(), r#"{"Id":3}"#);
        assert_eq!(
            shvproto::to_rpcvalue(&Mixed::Position { x: 1, y: None }).unwrap().to_cpon(),
            r#"{"pos":{"Y":null,"x":1}}"#
        );
    }

    #[derive(Debug, PartialEq, Serialize, Deserialize, RpcTypeStr)]
    struct ChronoDates {
        #[serde(with = "chrono_datetime")]
        date: chrono::DateTime<chrono::FixedOffset>,
        #[serde(default, with = "chrono_datetime_opt")]
        opt_date: Option<chrono::DateTime<chrono::FixedOffset>>,
    }

    #[test]
    fn chrono_datetime_is_shv_datetime() {
        assert_eq!(ChronoDates::RPC_TYPE_STR, "{t:date,t|n:opt_date}");

        let date = chrono::DateTime::parse_from_rfc3339("2026-10-31T10:00:00.123+01:00").unwrap();
        let dates = ChronoDates { date, opt_date: Some(date) };
        let value = shvproto::to_rpcvalue(&dates).unwrap();
        assert_eq!(value.get("date").unwrap().type_name(), "DateTime");
        assert_eq!(value.get("opt_date").unwrap().type_name(), "DateTime");
        assert_eq!(shvproto::from_rpcvalue::<ChronoDates>(&value).unwrap(), dates);

        let none = ChronoDates { date, opt_date: None };
        let value = shvproto::to_rpcvalue(&none).unwrap();
        assert_eq!(shvproto::from_rpcvalue::<ChronoDates>(&value).unwrap(), none);

        let missing = RpcValue::from_cpon(r#"{"date":d"2026-10-31T10:00:00.123+01"}"#).unwrap();
        assert_eq!(shvproto::from_rpcvalue::<ChronoDates>(&missing).unwrap(), none);
    }

    #[test]
    fn chrono_datetime_accepts_rfc3339_string() {
        let value = RpcValue::from_cpon(r#"{"date":"2026-10-31T10:00:00+01:00","opt_date":"2026-10-31T09:00:00Z"}"#).unwrap();
        let dates = shvproto::from_rpcvalue::<ChronoDates>(&value).unwrap();
        assert_eq!(dates.date, chrono::DateTime::parse_from_rfc3339("2026-10-31T10:00:00+01:00").unwrap());
        assert_eq!(dates.opt_date, Some(chrono::DateTime::parse_from_rfc3339("2026-10-31T09:00:00Z").unwrap()));
    }
}
