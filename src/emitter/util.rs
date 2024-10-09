use regex::Regex;

pub fn escape(s: &str) -> String {
    let re = Regex::new(r#"[\x07\x08\t\n\x0B\x0C\r\\'\"?]"#).unwrap();

    re.replace_all(s, |caps: &regex::Captures| {
        let c = &caps[0];
        match c.chars().next().unwrap() {
            '\x07' => format!("\\{:03o}", 0x07),
            '\x08' => format!("\\{:03o}", 0x08),
            '\t' => format!("\\{:03o}", 0x09),
            '\n' => format!("\\{:03o}", 0x0A),
            '\x0B' => format!("\\{:03o}", 0x0B),
            '\x0C' => format!("\\{:03o}", 0x0C),
            '\r' => format!("\\{:03o}", 0x0D),
            '\\' => format!("\\{:03o}", 0x5C),
            '\'' => format!("\\{:03o}", 0x27),
            '"' => format!("\\{:03o}", 0x22),
            '?' => format!("\\{:03o}", 0x3F),
            _ => c.to_string(),
        }
    })
    .to_string()
}
