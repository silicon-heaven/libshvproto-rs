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
        let mut lines = output.split("\n");
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
            child.wait_with_output().map_err(|err| err.to_string()).and_then(|output| {
                RpcValue::from_chainpack(output.stdout).map_err(|err| err.to_string())
            })
        }

        #[test]
        fn dot_filter() -> Result<(), String> {
            let input = RpcValue::from_cpon(r#"{"foo": true}"#).unwrap();
            let output = run_cq(&input, ".")?;
            assert_eq!(input, output);
            Ok(())
        }

        #[test]
        fn key_lookup() -> Result<(), String> {
            for (input, filter, expected_output) in [
                (r#"null"#, ".[0]", r#"null"#),
                (r#"null"#, ".foo", r#"null"#),
                (r#"{"foo": true}"#, ".foo", "true"),
                (r#""some_string""#, ".[0]", r#""s""#),
                (r#""some_string""#, ".[1]", r#""o""#),
                (r#"["first_elem", "second_elem"]"#, ".[0]", r#""first_elem""#),
                (r#"["first_elem", "second_elem"]"#, ".[1]", r#""second_elem""#),
                // (r#"i{1: "one", 2: "two"}"#, ".asd", "\"two\"")
            ] {
                let input = RpcValue::from_cpon(input).expect("valid cpon expected");
                let expected_output = RpcValue::from_cpon(expected_output).expect("valid cpon expected");
                let output = run_cq(&input, filter)?;
                assert_eq!(output, expected_output);
            }
            Ok(())
        }
    }
}
