use crate::lexer::lex::Const;
use anyhow::{bail, Result};

pub fn parse_integer(suffix: &str, just_number: &str) -> Result<Const> {
    let konst = match suffix {
        "ul" | "lu" => just_number.parse::<u64>().map(Const::ULong)?,
        "l" => just_number.parse::<i64>().map(Const::Long)?,
        "u" => {
            let i_wide = just_number.parse::<u64>()?;
            u32::try_from(i_wide)
                .map(Const::UInt)
                .unwrap_or_else(|_| Const::ULong(i_wide))
        }
        "" => {
            let i_wide = just_number.parse::<i64>()?;
            i32::try_from(i_wide)
                .map(Const::Int)
                .unwrap_or_else(|_| Const::Long(i_wide))
        }
        actual => bail!("Unknown suffix: {}", actual),
    };

    Ok(konst)
}
