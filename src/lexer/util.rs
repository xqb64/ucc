use crate::lexer::lex::Const;
use anyhow::{bail, Result};

pub fn parse_integer(suffix: &str, just_number: &str) -> Result<Const> {
    let (digits, radix, is_hex) = if just_number.starts_with("0x") || just_number.starts_with("0X") {
        (&just_number[2..], 16, true)
    } else {
        (just_number, 10, false)
    };

    let value = u64::from_str_radix(digits, radix)?;
    let konst = match suffix {
        "ull" | "llu" | "ul" | "lu" => Const::ULong(value),
        "ll" | "l" => {
            if value <= i64::MAX as u64 {
                Const::Long(value as i64)
            } else {
                Const::ULong(value)
            }
        }
        "u" => u32::try_from(value)
            .map(Const::UInt)
            .unwrap_or_else(|_| Const::ULong(value)),
        "" => {
            if !is_hex {
                i64::try_from(value)
                    .map(|i_wide| {
                        i32::try_from(i_wide)
                            .map(Const::Int)
                            .unwrap_or_else(|_| Const::Long(i_wide))
                    })
                    .unwrap_or(Const::ULong(value))
            } else if value <= i32::MAX as u64 {
                Const::Int(value as i32)
            } else if value <= u32::MAX as u64 {
                Const::UInt(value as u32)
            } else if value <= i64::MAX as u64 {
                Const::Long(value as i64)
            } else {
                Const::ULong(value)
            }
        }
        actual => bail!("Unknown suffix: {}", actual),
    };

    Ok(konst)
}
