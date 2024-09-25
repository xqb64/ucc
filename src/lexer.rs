use std::hash::Hash;

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
    char_const_re: Regex,
    string_re: Regex,
}

impl Lexer {
    pub fn new(src: String) -> Lexer {
        Lexer { src, pos: 0,
            punctuation_re: Regex::new(r"^[-+*/%~(){};!<>=?:,&\[\].]").unwrap(),
            punctuation_double_re: Regex::new(r"^--|^==|^!=|^>=|^<=|^&&|^\|\||^->").unwrap(),
            keyword_re: Regex::new(
                r"^int\b|^long\b|^char\b|^signed\b|^unsigned\b|^double\b|^void\b|^return\b|^if\b|^else\b|^do\b|^while\b|^for\b|^break\b|^continue\b|^static\b|^extern\b|^sizeof\b|^struct\b|^union\b|^enum\b|^typedef\b|^const\b|^volatile\b|^register\b|^auto\b|^restrict\b|^inline\b|^_Bool\b|^_Complex\b|^_Imaginary\b",
            )
            .unwrap(),
            constant_re: Regex::new(r"^[0-9]+(?P<suffix>[lL]?[uU]?|[uU]?[lL]?)\b").unwrap(),
            double_constant_re: Regex::new(
                r"^(([0-9]*\.[0-9]+|[0-9]+\.?)[Ee][+-]?[0-9]+|[0-9]*\.[0-9]+|[0-9]+\.)[^\w.]",
            )
            .unwrap(),
            identifier_re: Regex::new(r"^[a-zA-Z_]\w*\b").unwrap(),
            char_const_re: Regex::new(r#"^'([^'\\\n]|\\['"?\\abfnrtv])'"#).unwrap(),
            string_re: Regex::new(r#"^"([^"\\\n]|\\['"\\?abfnrtv])*""#).unwrap(),
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

        let token = if let Some(m) = self.keyword_re.find(src) {
            self.pos += m.as_str().len();
            match m.as_str() {
                "int" => Token::Int,
                "long" => Token::Long,
                "char" => Token::Char,
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
                "sizeof" => Token::Sizeof,
                "struct" => Token::Struct,
                _ => unreachable!(),
            }
        } else if let Some(m) = self.double_constant_re.find(src) {
            self.pos += m.as_str().len() - 1;              
            Token::Constant(Const::Double(
                m.as_str()[..m.as_str().len() - 1].parse::<f64>().unwrap(),
            ))
        } else if let Some(m) = self.constant_re.find(src) {
            self.pos += m.as_str().len();

            if self.src.chars().nth(self.pos).is_some_and(|ch| ch == '.') {
                return Some(Token::Error);
            }

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
        } else if let Some(m) = self.punctuation_double_re.find(src) {
            self.pos += m.as_str().len();
            match m.as_str() {
                "--" => Token::DoubleHyphen,
                "==" => Token::DoubleEqual,
                "!=" => Token::BangEqual,
                ">=" => Token::GreaterEqual,
                "<=" => Token::LessEqual,
                "&&" => Token::DoubleAmpersand,
                "||" => Token::DoublePipe,
                "->" => Token::Arrow,
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
                "." => { 
                    // if the next character is a constant followed by a suffix, it's an error
                    if self.constant_re.is_match(&self.src[self.pos..]) {
                        return Some(Token::Error);
                    }

                    Token::Dot
                }
                _ => unreachable!(),
            }
        } else if let Some(m) = self.identifier_re.find(src) {
            self.pos += m.as_str().len();
            Token::Identifier(m.as_str().to_string())
        } else if let Some(m) = self.string_re.find(src) {
            self.pos += m.as_str().len();
            let s = m.as_str().trim_start_matches("\"").trim_end_matches("\"");

            let mut result = String::new();

            s.replace(r"\a", "\x07")
                .replace(r"\b", "\x08")
                .replace(r"\f", "\x0c")
                .replace(r"\n", "\x0a")
                .replace(r"\r", "\x0d")
                .replace(r"\t", "\x09")
                .replace(r"\v", "\x0b")
                .replace(r#"\'"#, "\x27")
                .replace(r#"\""#, "\x22")
                .replace(r"\\", "\x5c")
                .replace(r"\?", "\x3f")
                .chars()
                .for_each(|ch| result.push(ch));

            Token::StringLiteral(result.to_string())
        } else if let Some(m) = self.char_const_re.find(src) {
            self.pos += m.as_str().len();
            let ch = &m.as_str()[1..m.as_str().len() - 1];
            let ch = match ch {
                r"\a" => '\x07',
                r"\b" => '\x08',
                r"\f" => '\x0c',
                r"\n" => '\x0a',
                r"\r" => '\x0d',
                r"\t" => '\x09',
                r"\v" => '\x0b',
                r"\'" => '\x27',
                r#"\""# => '\x22',
                r"\\" => '\x5c',
                r"\?" => '\x3f',
                _ => ch.parse().unwrap(),
            };
            Token::CharLiteral(ch)
        } else {
            if src.is_empty() {
                return None;
            }
            Token::Error
        };

        Some(token)
    }
}

#[derive(Debug, PartialEq, Clone, PartialOrd, Eq, Ord, Hash)]
pub enum Token {
    Int,
    Long,
    Char,
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
    Sizeof,
    Struct,
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
    Dot,
    Arrow,
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
    CharLiteral(char),
    StringLiteral(String),
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

#[derive(Debug, Clone, Copy, PartialOrd)]
pub enum Const {
    Int(i32),
    Long(i64),
    UInt(u32),
    ULong(u64),
    Double(f64),
    Char(i8),
    UChar(u8),
}

impl Ord for Const {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Const::Double(d1), Const::Double(d2)) => d1.partial_cmp(d2).unwrap(),
            _ => self.cmp(other),
        }
    }
}

impl PartialEq for Const {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Const::Double(d1), Const::Double(d2)) => d1 == d2,
            _ => self.eq(other),
        }
    }
}

impl Eq for Const {}

impl Hash for Const {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Const::Int(i) => i.hash(state),
            Const::Long(i) => i.hash(state),
            Const::UInt(i) => i.hash(state),
            Const::ULong(i) => i.hash(state),
            Const::Double(d) => d.to_bits().hash(state),
            Const::Char(c) => c.hash(state),
            Const::UChar(c) => c.hash(state),
        }
    }
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
