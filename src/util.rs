use log::LevelFilter;

pub fn parse_log_verbosity<'a>(verbosity: &'a str, module_path: &'a str) -> Vec<(Option<&'a str>, LevelFilter)> {
    verbosity
        .split(',')
        .map(|module_level_str| {
            let (name, level) = module_level_str
                .split_once('=')
                .unwrap_or((module_level_str, "D"));
            let level = if level == "" { "D" } else { level };
            let module = if name == "" {
                None
            } else if name == "." {
                Some(module_path)
            } else {
                Some(name)
            };
            let level = match level {
                "E" => LevelFilter::Error,
                "W" => LevelFilter::Warn,
                "I" => LevelFilter::Info,
                "D" => LevelFilter::Debug,
                _ => LevelFilter::Trace,
            };
            (module, level)
        })
        .collect::<Vec<_>>()
}

pub fn hex_array(data: &[u8]) -> String {
    let mut ret = "[".to_string();
    for b in data {
        if ret.len() > 1 {
            ret += ",";
        }
        ret += &format!("0x{:02x}", b);
    }
    ret += "]";
    ret
}
pub fn hex_dump(data: &[u8]) -> String {
    let mut ret: String = Default::default();
    let mut hex_line: String = Default::default();
    let mut char_line: String = Default::default();
    let box_size = (data.len() / 16 + 1) * 16 + 1;
    for i in 0..box_size {
        let byte = if i < data.len() { Some(data[i]) } else { None };
        if i % 16 == 0 {
            ret += &hex_line;
            ret += &char_line;
            if byte.is_some() {
                if i > 0 {
                    ret += "\n";
                }
                ret += &format!("{:04x} ", i);
            }
            hex_line.clear();
            char_line.clear();
        }
        let hex_str = match byte {
            None => { "   ".to_string() }
            Some(b) => { format!("{:02x} ", b) }
        };
        let c_str = match byte {
            None => { " ".to_string() }
            Some(b) => {
                let c = b as char;
                let c = if c >= ' ' && c < (127 as char) { c } else { '.' };
                format!("{}", c)
            }
        };
        hex_line += &hex_str;
        char_line += &c_str;
    }
    ret
}

#[cfg(test)]
mod tests {
    use std::iter::zip;

    use log::LevelFilter;

    use super::parse_log_verbosity;

    #[test]
    fn log_verbosity() {
        let current_module = module_path!();
        let data = vec![
            ("", vec![(None, LevelFilter::Debug)]),
            (".", vec![(Some(current_module), LevelFilter::Debug)]),
            (".=I", vec![(Some(current_module), LevelFilter::Info)]),
            ("mod", vec![(Some("mod"), LevelFilter::Debug)]),
            ("foo,bar", vec![(Some("foo"), LevelFilter::Debug), (Some("bar"), LevelFilter::Debug)]),
            ("foo::bar=I", vec![(Some("foo::bar"), LevelFilter::Info)]),
            ("=W,foo", vec![(None, LevelFilter::Warn), (Some("foo"), LevelFilter::Debug)]),
            ("foo=I,bar=E", vec![(Some("foo"), LevelFilter::Info), (Some("bar"), LevelFilter::Error)]),
        ];
        for (verbosity, expected_result) in data {
            let result = parse_log_verbosity(verbosity, current_module);
            assert_eq!(result.len(), expected_result.len(), "verbosity: `{verbosity}`");
            for ((module, filter), (module_expected, filter_expected)) in zip(result, expected_result) {
                assert!(module == module_expected && filter == filter_expected, "verbosity: `{verbosity}`");
            }
        }
    }
}
