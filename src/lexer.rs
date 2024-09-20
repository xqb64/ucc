use anyhow::{bail, Result};
use regex::Regex;

pub struct Lexer {
    src: String,
    pos: usize,
    punctuation_re: Regex,
    punctuation_double_re: Regex,
    keyword_re: Regex,
    identifier_re: Regex,
    constant_re: Regex,
    double_constant_re: Regex,
}

impl Lexer {
    pub fn new(src: String) -> Lexer {
        Lexer { src, pos: 0,
            punctuation_re: Regex::new(r"^[-+*/%~(){};!<>=?:,&\[\]]").unwrap(),
            punctuation_double_re: Regex::new(r"^--|^==|^!=|^>=|^<=|^&&|^\|\|").unwrap(),
            keyword_re: Regex::new(
                r"^int\b|^long\b|^signed\b|^unsigned\b|^double\b|^void\b|^return\b|^if\b|^else\b|^do\b|^while\b|^for\b|^break\b|^continue\b|^static\b|^extern\b",
            )
            .unwrap(),
            constant_re: Regex::new(r"^[0-9]+(?P<suffix>[lL]?[uU]?|[uU]?[lL]?)\b").unwrap(),
            double_constant_re: Regex::new(
                r"^(([0-9]*\.[0-9]+|[0-9]+\.?)[Ee][+-]?[0-9]+|[0-9]*\.[0-9]+|[0-9]+\.)[^\w.]",
            )
            .unwrap(),
            identifier_re: Regex::new(r"^[a-zA-Z_]\w*\b").unwrap(),
         }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self
                .src
                .chars()
                .nth(self.pos)
                .is_some_and(|ch| ch.is_whitespace())
            {
                self.pos += 1;
            } else {
                break;
            }
        }

        let src = &self.src[self.pos..];

        let token = if let Some(m) = self.punctuation_double_re.find(src) {
            self.pos += m.as_str().len();
            match m.as_str() {
                "--" => Token::DoubleHyphen,
                "==" => Token::DoubleEqual,
                "!=" => Token::BangEqual,
                ">=" => Token::GreaterEqual,
                "<=" => Token::LessEqual,
                "&&" => Token::DoubleAmpersand,
                "||" => Token::DoublePipe,
                _ => unreachable!(),
            }
        } else if let Some(m) = self.punctuation_re.find(src) {
            self.pos += m.as_str().len();
            match m.as_str() {
                "+" => Token::Plus,
                "-" => Token::Hyphen,
                "*" => Token::Star,
                "/" => Token::Slash,
                "%" => Token::Percent,
                "~" => Token::Tilde,
                "!" => Token::Bang,
                "?" => Token::QuestionMark,
                ":" => Token::Colon,
                "<" => Token::Less,
                ">" => Token::Greater,
                "(" => Token::LParen,
                ")" => Token::RParen,
                "{" => Token::LBrace,
                "}" => Token::RBrace,
                "[" => Token::LBracket,
                "]" => Token::RBracket,
                "=" => Token::Equal,
                "," => Token::Comma,
                "&" => Token::Ampersand,
                ";" => Token::Semicolon,
                _ => unreachable!(),
            }
        } else if let Some(m) = self.keyword_re.find(src) {
            self.pos += m.as_str().len();
            match m.as_str() {
                "int" => Token::Int,
                "long" => Token::Long,
                "signed" => Token::Signed,
                "unsigned" => Token::Unsigned,
                "double" => Token::Double,
                "void" => Token::Void,
                "return" => Token::Return,
                "if" => Token::If,
                "else" => Token::Else,
                "do" => Token::Do,
                "while" => Token::While,
                "for" => Token::For,
                "break" => Token::Break,
                "continue" => Token::Continue,
                "static" => Token::Static,
                "extern" => Token::Extern,
                _ => unreachable!(),
            }
        } else if let Some(m) = self.double_constant_re.find(src) {
            self.pos += m.as_str().len() - 1;
            Token::Constant(Const::Double(
                m.as_str()[..m.as_str().len() - 1].parse::<f64>().unwrap(),
            ))
        } else if let Some(m) = self.constant_re.find(src) {
            self.pos += m.as_str().len();

            let suffix = self
                .constant_re
                .captures(src)
                .unwrap()
                .name("suffix")
                .unwrap()
                .as_str();
            let just_number = m.as_str().trim_end_matches(suffix);

            let normalized_suffix = suffix
                .chars()
                .map(|ch| ch.to_ascii_lowercase())
                .collect::<String>();

            match parse_integer(&normalized_suffix, just_number) {
                Ok(konst) => Token::Constant(konst),
                Err(_) => Token::Error,
            }
        } else if let Some(m) = self.identifier_re.find(src) {
            self.pos += m.as_str().len();
            Token::Identifier(m.as_str().to_string())
        } else {
            if src.is_empty() {
                return None;
            }
            Token::Error
        };

        Some(token)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Int,
    Long,
    Signed,
    Unsigned,
    Double,
    Void,
    Return,
    If,
    Else,
    Do,
    While,
    For,
    Break,
    Continue,
    Static,
    Extern,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Plus,
    Hyphen,
    Star,
    Slash,
    Percent,
    Tilde,
    Bang,
    QuestionMark,
    Colon,
    DoubleHyphen,
    DoubleAmpersand,
    DoublePipe,
    DoubleEqual,
    BangEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Equal,
    Comma,
    Ampersand,
    Semicolon,
    Identifier(String),
    Constant(Const),
    Error,
}

impl Token {
    pub fn as_const(&self) -> Const {
        match self {
            Token::Constant(n) => *n,
            _ => unreachable!(),
        }
    }

    pub fn as_string(&self) -> String {
        match self {
            Token::Identifier(s) => s.to_owned(),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Const {
    Int(i32),
    Long(i64),
    UInt(u32),
    ULong(u64),
    Double(f64),
}

fn parse_integer(suffix: &str, just_number: &str) -> Result<Const> {
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
