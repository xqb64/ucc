use crate::lexer::util::parse_integer;
use regex::Regex;
use std::hash::Hash;

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

impl std::ops::Add for Const {
    type Output = Const;

    fn add(self, rhs: Const) -> Self::Output {
        match (self, rhs) {
            (Const::Int(lhs), Const::Int(rhs)) => Const::Int(lhs + rhs),
            (Const::Long(lhs), Const::Long(rhs)) => Const::Long(lhs + rhs),
            (Const::UInt(lhs), Const::UInt(rhs)) => Const::UInt(lhs + rhs),
            (Const::ULong(lhs), Const::ULong(rhs)) => Const::ULong(lhs + rhs),
            (Const::Double(lhs), Const::Double(rhs)) => Const::Double(lhs + rhs),
            (Const::Char(lhs), Const::Char(rhs)) => Const::Char(lhs + rhs),
            (Const::UChar(lhs), Const::UChar(rhs)) => Const::UChar(lhs + rhs),
            _ => unreachable!(),
        }
    }
}

impl std::ops::Sub for Const {
    type Output = Const;

    fn sub(self, rhs: Const) -> Self::Output {
        match (self, rhs) {
            (Const::Int(lhs), Const::Int(rhs)) => Const::Int(lhs - rhs),
            (Const::Long(lhs), Const::Long(rhs)) => Const::Long(lhs - rhs),
            (Const::UInt(lhs), Const::UInt(rhs)) => Const::UInt(lhs - rhs),
            (Const::ULong(lhs), Const::ULong(rhs)) => Const::ULong(lhs - rhs),
            (Const::Double(lhs), Const::Double(rhs)) => Const::Double(lhs - rhs),
            (Const::Char(lhs), Const::Char(rhs)) => Const::Char(lhs - rhs),
            (Const::UChar(lhs), Const::UChar(rhs)) => Const::UChar(lhs - rhs),
            _ => unreachable!(),
        }
    }
}

impl std::ops::Mul for Const {
    type Output = Const;

    fn mul(self, rhs: Const) -> Self::Output {
        match (self, rhs) {
            (Const::Int(lhs), Const::Int(rhs)) => Const::Int(lhs * rhs),
            (Const::Long(lhs), Const::Long(rhs)) => Const::Long(lhs * rhs),
            (Const::UInt(lhs), Const::UInt(rhs)) => Const::UInt(lhs * rhs),
            (Const::ULong(lhs), Const::ULong(rhs)) => Const::ULong(lhs * rhs),
            (Const::Double(lhs), Const::Double(rhs)) => Const::Double(lhs * rhs),
            (Const::Char(lhs), Const::Char(rhs)) => Const::Char(lhs * rhs),
            (Const::UChar(lhs), Const::UChar(rhs)) => Const::UChar(lhs * rhs),
            _ => unreachable!(),
        }
    }
}

impl std::ops::Div for Const {
    type Output = Const;

    fn div(self, rhs: Const) -> Self::Output {
        match (self, rhs) {
            (Const::Int(lhs), Const::Int(rhs)) => Const::Int(lhs.checked_div(rhs).unwrap_or(0)),
            (Const::Long(lhs), Const::Long(rhs)) => Const::Long(lhs.checked_div(rhs).unwrap_or(0)),
            (Const::UInt(lhs), Const::UInt(rhs)) => Const::UInt(lhs.checked_div(rhs).unwrap_or(0)),
            (Const::ULong(lhs), Const::ULong(rhs)) => {
                Const::ULong(lhs.checked_div(rhs).unwrap_or(0))
            }
            (Const::Char(lhs), Const::Char(rhs)) => Const::Char(lhs.checked_div(rhs).unwrap_or(0)),
            (Const::UChar(lhs), Const::UChar(rhs)) => {
                Const::UChar(lhs.checked_div(rhs).unwrap_or(0))
            }
            (Const::Double(lhs), Const::Double(rhs)) => Const::Double(lhs / rhs),
            _ => {
                unreachable!()
            }
        }
    }
}

impl std::ops::Rem for Const {
    type Output = Const;

    fn rem(self, rhs: Const) -> Self::Output {
        match (self, rhs) {
            (Const::Int(lhs), Const::Int(rhs)) => Const::Int(lhs.checked_rem(rhs).unwrap_or(0)),
            (Const::Long(lhs), Const::Long(rhs)) => Const::Long(lhs.checked_rem(rhs).unwrap_or(0)),
            (Const::UInt(lhs), Const::UInt(rhs)) => Const::UInt(lhs.checked_rem(rhs).unwrap_or(0)),
            (Const::ULong(lhs), Const::ULong(rhs)) => {
                Const::ULong(lhs.checked_rem(rhs).unwrap_or(0))
            }
            (Const::Char(lhs), Const::Char(rhs)) => Const::Char(lhs.checked_rem(rhs).unwrap_or(0)),
            (Const::UChar(lhs), Const::UChar(rhs)) => {
                Const::UChar(lhs.checked_rem(rhs).unwrap_or(0))
            }
            _ => unreachable!(),
        }
    }
}

impl std::ops::Neg for Const {
    type Output = Const;

    fn neg(self) -> Self::Output {
        match self {
            Const::Int(val) => Const::Int(-val),
            Const::Long(val) => Const::Long(-val),
            Const::UInt(val) => Const::Int(-(val as i32)),
            Const::ULong(val) => Const::Long(-(val as i64)),
            Const::Double(val) => Const::Double(-val),
            _ => unreachable!(),
        }
    }
}

impl std::ops::Not for Const {
    type Output = Const;

    fn not(self) -> Self::Output {
        match self {
            Const::Int(val) => Const::Int(!val),
            Const::Long(val) => Const::Long(!val),
            Const::UInt(val) => Const::UInt(!val),
            Const::ULong(val) => Const::ULong(!val),
            _ => unreachable!(),
        }
    }
}

impl Ord for Const {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::any::Any;
        use Const::*;
        match (self, other) {
            (Double(d1), Double(d2)) => d1.total_cmp(d2),
            (Int(i1), Int(i2)) => i1.cmp(i2),
            (Long(l1), Long(l2)) => l1.cmp(l2),
            (UInt(u1), UInt(u2)) => u1.cmp(u2),
            (ULong(ul1), ULong(ul2)) => ul1.cmp(ul2),
            (Char(c1), Char(c2)) => c1.cmp(c2),
            (UChar(uc1), UChar(uc2)) => uc1.cmp(uc2),

            (a, b) => std::mem::discriminant(a)
                .type_id()
                .cmp(&std::mem::discriminant(b).type_id()),
        }
    }
}

impl PartialEq for Const {
    fn eq(&self, other: &Self) -> bool {
        use Const::*;
        match (self, other) {
            (Double(d1), Double(d2)) => d1 == d2 || d1.total_cmp(d2) == std::cmp::Ordering::Equal,
            (Int(i1), Int(i2)) => i1 == i2,
            (Long(l1), Long(l2)) => l1 == l2,
            (UInt(u1), UInt(u2)) => u1 == u2,
            (ULong(ul1), ULong(ul2)) => ul1 == ul2,
            (Char(c1), Char(c2)) => c1 == c2,
            (UChar(uc1), UChar(uc2)) => uc1 == uc2,
            _ => false,
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

impl ToString for Const {
    fn to_string(&self) -> String {
        match self {
            Const::Int(i) => i.to_string(),
            Const::Long(l) => l.to_string(),
            Const::UInt(u) => u.to_string(),
            Const::ULong(ul) => ul.to_string(),
            Const::Double(d) => d.to_string(),
            Const::Char(c) => c.to_string(),
            Const::UChar(uc) => uc.to_string(),
        }
    }
}
