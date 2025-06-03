use crate::lexer::util::parse_integer;
use regex::Regex;
use std::{collections::HashMap, hash::Hash};

pub struct Lexer {
    src: String,
    pos: usize,
    regexes: HashMap<&'static str, Regex>,
}

impl Lexer {
    pub fn new(src: String) -> Lexer {
        let keywords = vec![
            "int", "long", "char", "signed", "unsigned", "double", "void", "return", "if", "else",
            "do", "while", "for", "break", "continue", "static", "extern", "sizeof", "struct",
            "goto", "switch", "case", "default",
        ];

        let mut regexes = HashMap::new();

        regexes.insert(
            "punctuation",
            Regex::new(r"^[-+*/%~(){};!<>=?:,&\[\].^|]").unwrap(),
        );
        regexes.insert("punctuation_triple", Regex::new(r"^(<<=|>>=)").unwrap());
        regexes.insert(
            "punctuation_double",
            Regex::new(r"^(--|==|!=|>=|<=|&&|\|\||->|>>|<<|\+\+|\+=|-=|\*=|/=|\|=|\^=|&=|%=)")
                .unwrap(),
        );
        regexes.insert(
            "keyword",
            Regex::new(&format!(r"^{}\b", keywords.join(r"\b|^"))).unwrap(),
        );
        regexes.insert(
            "constant",
            Regex::new(r"^[0-9]+(?P<suffix>[lL]?[uU]?|[uU]?[lL]?)\b").unwrap(),
        );
        regexes.insert(
            "double_constant",
            Regex::new(
                r"^(([0-9]*\.[0-9]+|[0-9]+\.?)[Ee][+-]?[0-9]+|[0-9]*\.[0-9]+|[0-9]+\.)[^\w.]",
            )
            .unwrap(),
        );
        regexes.insert("identifier", Regex::new(r"^[a-zA-Z_]\w*\b").unwrap());
        regexes.insert(
            "char_const",
            Regex::new(r#"^'([^'\\\n]|\\['"?\\abfnrtv])'"#).unwrap(),
        );
        regexes.insert(
            "string",
            Regex::new(r#"^"([^"\\\n]|\\['"\\?abfnrtv])*""#).unwrap(),
        );

        Lexer {
            src,
            pos: 0,
            regexes,
        }
    }

    fn make_token(&self, kind: TokenKind, len: usize) -> Token {
        Token::new(
            kind,
            Span {
                start: self.pos - len,
                end: self.pos,
            },
        )
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

        let src = self.src.chars().skip(self.pos).collect::<String>();
        let src = src.as_str();

        let token = if let Some(m) = self.regexes["keyword"].find(src) {
            let len = m.as_str().chars().count();
            self.pos += len;
            match m.as_str() {
                "int" => self.make_token(TokenKind::Int, len),
                "long" => self.make_token(TokenKind::Long, len),
                "char" => self.make_token(TokenKind::Char, len),
                "signed" => self.make_token(TokenKind::Signed, len),
                "unsigned" => self.make_token(TokenKind::Unsigned, len),
                "double" => self.make_token(TokenKind::Double, len),
                "void" => self.make_token(TokenKind::Void, len),
                "return" => self.make_token(TokenKind::Return, len),
                "if" => self.make_token(TokenKind::If, len),
                "else" => self.make_token(TokenKind::Else, len),
                "do" => self.make_token(TokenKind::Do, len),
                "while" => self.make_token(TokenKind::While, len),
                "for" => self.make_token(TokenKind::For, len),
                "break" => self.make_token(TokenKind::Break, len),
                "continue" => self.make_token(TokenKind::Continue, len),
                "static" => self.make_token(TokenKind::Static, len),
                "extern" => self.make_token(TokenKind::Extern, len),
                "sizeof" => self.make_token(TokenKind::Sizeof, len),
                "struct" => self.make_token(TokenKind::Struct, len),
                "goto" => self.make_token(TokenKind::Goto, len),
                "switch" => self.make_token(TokenKind::Switch, len),
                "case" => self.make_token(TokenKind::Case, len),
                "default" => self.make_token(TokenKind::Default, len),
                _ => unreachable!(),
            }
        } else if let Some(m) = self.regexes["double_constant"].find(src) {
            let len = m.as_str().chars().count() - 1;
            self.pos += len;
            self.make_token(
                TokenKind::Constant(Const::Double(
                    m.as_str()
                        .chars()
                        .take(len)
                        .collect::<String>()
                        .parse::<f64>()
                        .unwrap(),
                )),
                len,
            )
        } else if let Some(m) = self.regexes["constant"].find(src) {
            let len = m.as_str().chars().count();
            self.pos += len;

            if self.src.chars().nth(self.pos).is_some_and(|ch| ch == '.') {
                return Some(self.make_token(TokenKind::Error, len));
            }

            let suffix = self.regexes["constant"]
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
                Ok(konst) => self.make_token(TokenKind::Constant(konst), len),
                Err(_) => self.make_token(TokenKind::Error, len),
            }
        } else if let Some(m) = self.regexes["punctuation_triple"].find(src) {
            let len = m.as_str().chars().count();
            self.pos += len;

            match m.as_str() {
                ">>=" => self.make_token(TokenKind::GreaterGreaterEqual, len),
                "<<=" => self.make_token(TokenKind::LessLessEqual, len),
                _ => unreachable!(),
            }
        } else if let Some(m) = self.regexes["punctuation_double"].find(src) {
            let len = m.as_str().chars().count();
            self.pos += len;

            match m.as_str() {
                "++" => self.make_token(TokenKind::DoublePlus, len),
                "--" => self.make_token(TokenKind::DoubleHyphen, len),
                "==" => self.make_token(TokenKind::DoubleEqual, len),
                "!=" => self.make_token(TokenKind::BangEqual, len),
                ">=" => self.make_token(TokenKind::GreaterEqual, len),
                "<=" => self.make_token(TokenKind::LessEqual, len),
                "&&" => self.make_token(TokenKind::DoubleAmpersand, len),
                "||" => self.make_token(TokenKind::DoublePipe, len),
                "->" => self.make_token(TokenKind::Arrow, len),
                ">>" => self.make_token(TokenKind::GreaterGreater, len),
                "<<" => self.make_token(TokenKind::LessLess, len),
                "+=" => self.make_token(TokenKind::PlusEqual, len),
                "-=" => self.make_token(TokenKind::MinusEqual, len),
                "*=" => self.make_token(TokenKind::StarEqual, len),
                "/=" => self.make_token(TokenKind::SlashEqual, len),
                "^=" => self.make_token(TokenKind::CaretEqual, len),
                "%=" => self.make_token(TokenKind::ModEqual, len),
                "&=" => self.make_token(TokenKind::AmpersandEqual, len),
                "|=" => self.make_token(TokenKind::PipeEqual, len),
                _ => unreachable!(),
            }
        } else if let Some(m) = self.regexes["punctuation"].find(src) {
            let len = m.as_str().chars().count();
            self.pos += len;

            match m.as_str() {
                "+" => self.make_token(TokenKind::Plus, len),
                "-" => self.make_token(TokenKind::Hyphen, len),
                "*" => self.make_token(TokenKind::Star, len),
                "/" => self.make_token(TokenKind::Slash, len),
                "%" => self.make_token(TokenKind::Percent, len),
                "~" => self.make_token(TokenKind::Tilde, len),
                "!" => self.make_token(TokenKind::Bang, len),
                "?" => self.make_token(TokenKind::QuestionMark, len),
                ":" => self.make_token(TokenKind::Colon, len),
                "<" => self.make_token(TokenKind::Less, len),
                ">" => self.make_token(TokenKind::Greater, len),
                "(" => self.make_token(TokenKind::LParen, len),
                ")" => self.make_token(TokenKind::RParen, len),
                "{" => self.make_token(TokenKind::LBrace, len),
                "}" => self.make_token(TokenKind::RBrace, len),
                "[" => self.make_token(TokenKind::LBracket, len),
                "]" => self.make_token(TokenKind::RBracket, len),
                "=" => self.make_token(TokenKind::Equal, len),
                "," => self.make_token(TokenKind::Comma, len),
                "&" => self.make_token(TokenKind::Ampersand, len),
                ";" => self.make_token(TokenKind::Semicolon, len),
                "^" => self.make_token(TokenKind::Caret, len),
                "|" => self.make_token(TokenKind::Pipe, len),
                "." => {
                    if self.regexes["constant"]
                        .is_match(self.src.chars().skip(self.pos).collect::<String>().as_str())
                    {
                        return Some(self.make_token(TokenKind::Error, len));
                    }

                    self.make_token(TokenKind::Dot, len)
                }
                _ => unreachable!(),
            }
        } else if let Some(m) = self.regexes["identifier"].find(src) {
            let len = m.as_str().chars().count();
            self.pos += len;

            self.make_token(TokenKind::Identifier(m.as_str().to_string()), len)
        } else if let Some(m) = self.regexes["string"].find(src) {
            let len = m.as_str().chars().count();
            self.pos += len;

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

            self.make_token(TokenKind::StringLiteral(result.to_string()), len)
        } else if let Some(m) = self.regexes["char_const"].find(src) {
            let len = m.as_str().chars().count();
            self.pos += len;

            let ch = &m.as_str().chars().skip(1).take(len - 2).collect::<String>();
            let ch = match ch.as_str() {
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
            self.make_token(TokenKind::CharLiteral(ch), len)
        } else {
            if src.is_empty() {
                return None;
            }
            self.make_token(TokenKind::Error, 0)
        };

        Some(token)
    }
}

#[derive(Debug, PartialEq, Clone, PartialOrd, Eq, Ord, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone, Copy, PartialOrd, Eq, Ord, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl std::ops::Add for Span {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Span {
            start: self.start.min(rhs.start),
            end: self.end.max(rhs.end),
        }
    }
}

#[derive(Debug, PartialEq, Clone, PartialOrd, Eq, Ord, Hash)]
pub enum TokenKind {
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
    Goto,
    Switch,
    Case,
    Default,
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
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    CaretEqual,
    ModEqual,
    AmpersandEqual,
    PipeEqual,
    GreaterGreaterEqual,
    LessLessEqual,
    DoublePlus,
    DoubleHyphen,
    DoubleAmpersand,
    DoublePipe,
    DoubleEqual,
    BangEqual,
    GreaterGreater,
    LessLess,
    Caret,
    Pipe,
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
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn as_const(&self) -> Const {
        match self.kind {
            TokenKind::Constant(n) => n.to_owned(),
            _ => unreachable!(),
        }
    }

    pub fn as_string(&self) -> String {
        match self.kind {
            TokenKind::Identifier(ref s) => s.to_owned(),
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

use std::ops::{BitAnd, BitOr, BitXor};

impl BitAnd for Const {
    type Output = Const;

    fn bitand(self, rhs: Const) -> Self::Output {
        match (self, rhs) {
            (Const::Int(lhs), Const::Int(rhs)) => Const::Int(lhs & rhs),
            (Const::Long(lhs), Const::Long(rhs)) => Const::Long(lhs & rhs),
            (Const::UInt(lhs), Const::UInt(rhs)) => Const::UInt(lhs & rhs),
            (Const::ULong(lhs), Const::ULong(rhs)) => Const::ULong(lhs & rhs),
            (Const::Char(lhs), Const::Char(rhs)) => Const::Char(lhs & rhs),
            (Const::UChar(lhs), Const::UChar(rhs)) => Const::UChar(lhs & rhs),
            _ => unreachable!(),
        }
    }
}

impl BitOr for Const {
    type Output = Const;

    fn bitor(self, rhs: Const) -> Self::Output {
        match (self, rhs) {
            (Const::Int(lhs), Const::Int(rhs)) => Const::Int(lhs | rhs),
            (Const::Long(lhs), Const::Long(rhs)) => Const::Long(lhs | rhs),
            (Const::UInt(lhs), Const::UInt(rhs)) => Const::UInt(lhs | rhs),
            (Const::ULong(lhs), Const::ULong(rhs)) => Const::ULong(lhs | rhs),
            (Const::Char(lhs), Const::Char(rhs)) => Const::Char(lhs | rhs),
            (Const::UChar(lhs), Const::UChar(rhs)) => Const::UChar(lhs | rhs),
            _ => unreachable!(),
        }
    }
}

impl BitXor for Const {
    type Output = Const;

    fn bitxor(self, rhs: Const) -> Self::Output {
        match (self, rhs) {
            (Const::Int(lhs), Const::Int(rhs)) => Const::Int(lhs ^ rhs),
            (Const::Long(lhs), Const::Long(rhs)) => Const::Long(lhs ^ rhs),
            (Const::UInt(lhs), Const::UInt(rhs)) => Const::UInt(lhs ^ rhs),
            (Const::ULong(lhs), Const::ULong(rhs)) => Const::ULong(lhs ^ rhs),
            (Const::Char(lhs), Const::Char(rhs)) => Const::Char(lhs ^ rhs),
            (Const::UChar(lhs), Const::UChar(rhs)) => Const::UChar(lhs ^ rhs),
            _ => unreachable!(),
        }
    }
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
            (Double(d1), Double(d2)) => d1.partial_cmp(d2) == Some(std::cmp::Ordering::Equal),
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

impl From<bool> for Const {
    fn from(b: bool) -> Self {
        match b {
            true => Const::Int(1),
            false => Const::Int(0),
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

