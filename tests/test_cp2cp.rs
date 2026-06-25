#[cfg(test)]
mod test {
    use assert_cmd::cargo_bin;
    use shvproto::RpcValue;
    use std::io::Write;
    use std::process::{Command, Output, Stdio};
    use std::thread;

    fn run_cp2cp(data: &str) -> Result<Output, String> {
        let block = hex::decode(data).expect("HEX decoding failed");
        let mut cmd = Command::new(cargo_bin!("cp2cp"));
        cmd.stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .arg("--chainpack-rpc-block");
        let mut child = cmd.spawn().map_err(|e| e.to_string())?;
        let mut stdin = child.stdin.take().expect("cp2cp should be running");
        thread::spawn(move || {
            stdin.write_all(&block).expect("Failed to write to stdin");
        });
        child.wait_with_output().map_err(|e| e.to_string())
    }
    #[test]
    fn chainpack_rpc_block_valid() -> Result<(), String> {
        // <T:RpcMessage,id:4,method:"ls">i{}
        let data = "0E018B414148444A86026C73FF8AFF";
        let output = run_cp2cp(data)?;
        let exit_code = output.status.code().unwrap();
        let output = String::from_utf8(output.stdout).map_err(|e| e.to_string())?;
        let mut lines = output.split('\n');
        let block_length = lines.next().unwrap().parse::<usize>().unwrap();
        let frame_length = lines.next().unwrap().parse::<usize>().unwrap();
        let proto = lines.next().unwrap().parse::<i32>().unwrap();
        let cpon = lines.next().unwrap().to_owned();

        assert_eq!(exit_code, 0);
        assert_eq!(block_length, 15);
        assert_eq!(frame_length, 14);
        assert_eq!(proto, 1);
        let rv = RpcValue::from_cpon(&cpon).unwrap();
        assert!(rv.is_imap());
        // println!("{}", rv.to_cpon());
        assert_eq!(rv.meta.as_ref().unwrap().get(8).unwrap().as_int(), 4);
        assert_eq!(rv.meta.as_ref().unwrap().get(10).unwrap().as_str(), "ls");
        Ok(())
    }
    #[test]
    fn chainpack_rpc_block_error() -> Result<(), String> {
        // <T:RpcMessage,id:4,method:"ls">i{}
        let data = "0E0190414148444A86026C73FF8AFF";
        let output = run_cp2cp(data)?;
        let exit_code = output.status.code().unwrap();
        assert_eq!(exit_code, 1);
        Ok(())
    }
    #[test]
    fn chainpack_rpc_block_not_enough_data() -> Result<(), String> {
        // <T:RpcMessage,id:4,method:"ls">i{}
        let data = "0E018B414148444A86026C73FF8A";
        let output = run_cp2cp(data)?;
        let exit_code = output.status.code().unwrap();
        assert_eq!(exit_code, 2);
        Ok(())
    }
    #[test]
    fn chainpack_rpc_block_invalid_length() -> Result<(), String> {
        // <T:RpcMessage,id:4,method:"ls">i{}
        let data = "FFFF";
        let output = run_cp2cp(data)?;
        let exit_code = output.status.code().unwrap();
        assert_eq!(exit_code, 1);
        Ok(())
    }
    #[test]
    fn chainpack_rpc_block_invalid_protocol() -> Result<(), String> {
        // <T:RpcMessage,id:4,method:"ls">i{}
        let data = "0103";
        let output = run_cp2cp(data)?;
        let exit_code = output.status.code().unwrap();
        assert_eq!(exit_code, 1);
        Ok(())
    }

    #[cfg(feature = "cq")]
    mod cq {
        use super::*;

        fn run_cq(data: &RpcValue, filter: &str) -> Result<RpcValue, String> {
            let block = data.to_chainpack();
            let mut cmd = Command::new(cargo_bin!("cp2cp"));
            cmd.stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .arg("--oc")
                .arg("--cq").arg(filter);
            let mut child = cmd.spawn().map_err(|e| e.to_string())?;
            let mut stdin = child.stdin.take().expect("cp2cp should be running");
            thread::spawn(move || {
                stdin.write_all(&block).expect("Failed to write to stdin");
            });
            let output = child.wait_with_output().map_err(|err| err.to_string())?;
            RpcValue::from_chainpack(output.stdout).map_err(|err| format!("{}, stderr: {}", err, String::from_utf8_lossy(output.stderr.as_slice())))
        }

        #[test]
        fn dot_filter() -> Result<(), String> {
            let input = RpcValue::from_cpon(r#"{"foo": true}"#).unwrap();
            let output = run_cq(&input, ".")?;
            assert_eq!(input, output);
            Ok(())
        }

        fn impl_cq_test(tests: impl IntoIterator<Item = (&'static str, &'static str, &'static str)>) -> Result<(), String> {
            for (input, filter, expected_output) in tests {
                let input = RpcValue::from_cpon(input).expect("valid cpon expected");
                let expected_output = RpcValue::from_cpon(expected_output).expect("valid cpon expected");
                let output = run_cq(&input, filter)?;
                assert_eq!(output, expected_output);
            }

            Ok(())
        }

        #[test]
        fn key_lookup() -> Result<(), String> {
            impl_cq_test([
                (r#"null"#, ".[0]", r#"null"#),
                (r#"null"#, ".foo", r#"null"#),
                (r#"{"foo": true}"#, ".foo", "true"),
                (r#""some_string""#, ".[0]", r#""s""#),
                (r#""some_string""#, ".[1]", r#""o""#),
                (r#"["first_elem", "second_elem"]"#, ".[0]", r#""first_elem""#),
                (r#"["first_elem", "second_elem"]"#, ".[1]", r#""second_elem""#),
                // (r#"i{1: "one", 2: "two"}"#, ".asd", "\"two\"")
            ])
        }

        #[test]
        fn arithmetics() -> Result<(), String> {
            // FIXME: Add a way to specify UInt literals, and then use it here in the tests.
            impl_cq_test([
                ("null", "(-false)", r"true"),
                ("null", "(-true)", r"false"),
                ("null", "(-1)", r"-1"),

                ("null", "1 + 1", r"2"),
                ("1", ". + 1", r"2"),
                ("1u", ". + (-1)", r"0u"),
                ("-1", ". + .", r"-2"),
                ("1u", ". + 1", r"2u"),
                ("1", ". + .", r"2"),
                ("1u", ". + .", r"2u"),

                ("1u", ". - .", r"0u"),
                ("1u", ". - 1", r"0u"),
                ("null", "1 - 1", r"0"),
                ("1", ". - 1", r"0"),

                ("null", "(10 * 10)", r"100"),
                ("10u", ". * .", r"100u"),
                ("10u", ". * 10", r"100u"),

                ("null", "(10 / 10)", r"1"),
                ("10u", ". / .", r"1u"),
                ("10u", ". / 10", r"1u"),

                ("null", "(10 % 10)", r"0"),
                ("null", "(10 % 11)", r"10"),
                ("null", "(10 % 9)", r"1"),
            ])
        }

        #[test]
        fn comparisons() -> Result<(), String> {
            impl_cq_test([
                ("null", "null == null", r"true"),
                ("null", "true == true", r"true"),
                ("null", "{a: false} == {a: false}", r"true"),
                ("null", "{a: false} == {a: true}", r"false"),
                ("null", "{a: false} == {b: false}", r"false"),

                ("null", "null < null", r"false"),
                ("null", "null < 1", r"true"),
                ("null", "1 > null", r"true"),
                ("null", "false > 1", r"false"),
                ("null", "1 < false", r"false"),
                ("null", "1 < {}", r"true"),
                ("null", "{} < 1", r"false"),
                ("1u", ". < {}", r"true"),
                ("1u", "{} < .", r"false"),
                ("1.25p-2", ". < {}", r"true"),
                ("1.25p-2", "{} < .", r"false"),
                ("1e1", ". < {}", r"true"),
                ("1e1", "{} < .", r"false"),
                (r#"d"2017-05-03T15:52:31.123""#, ". < {}", r"true"),
                (r#"d"2017-05-03T15:52:31.123""#, "{} < .", r"false"),
                ("null", "\"asdf\" < {}", r"true"),
                ("null", r#"{} < "asdf" "#, r"false"),
                (r#"b"""#, ". < {}", r"true"),
                (r#"b"""#, "{} < .", r"false"),
                ("null", "[] < {}", r"true"),
                ("null", "{} < []", r"false"),
                ("i{}", ". < {}", r"true"),
                ("i{}", "{} < .", r"false"),

                ("null", "false < true", r"true"),
                ("null", "1 < 2", r"true"),
                ("null", r#" "asdf" < "xxxx" "#, r"true"),
                ("null", "[1] < [1, 1]", r"true"),
                ("null", "{} < {a: 1}", r"true"),

                // FIXME: No way of creating these values in cq yet.
                ("1u", ". < (. + (1))", r"true"),
                (r#"d"2017-05-03T15:52:31.123""#, ". < .", r"false"),
                ("1e1", ". < .", r"false"),
                (r#" b"" "#, ". < .", r"false"),
                ("i{}", ". < .", r"false"),
            ])
        }

        #[test]
        fn slicing() -> Result<(), String> {
            impl_cq_test([
                (r#" [1, 2, 3, 4] "#, ".[2:4]", r#" [3, 4] "#),

                (r#" "asdfasdf" "#, ".[4:7]", r#" "asd" "#),
            ])
        }

        #[test]
        fn extra_funs() -> Result<(), String> {
            impl_cq_test([
                ("null", "typename", r#" "Null" "#),
                ("1u", "typename", r#" "UInt" "#),
            ])
        }

        #[test]
        fn misc() -> Result<(), String> {
            impl_cq_test([
                (r#" [1, 2, 3] "#, "reverse", r#" [3, 2, 1] "#),
                (r#" 12 "#, "round", r#" 12 "#),
                (r#" 1.2 "#, "round", r#" 1 "#),
                (r#" 1.25p-2 "#, "round", r#" 1 "#),
                (r#" "asdf" "#, "explode", r#" [97,115,100,102] "#),
                (r#" [97,115,100,102] "#, "implode", r#" "asdf" "#),
                (r#" "   asdf    " "#, "trim", r#" "asdf" "#),
            ])
        }
    }
}
