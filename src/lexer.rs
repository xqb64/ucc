pub struct Lexer {
    src: String,
    pos: usize,
}

impl Lexer {
    pub fn new(src: String) -> Lexer {
        Lexer { src, pos: 0 }
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

        let punctuation_re = regex::Regex::new(r"^[-+*/%~(){};!<>=?:,]").unwrap();
        let punctuation_double_re = regex::Regex::new(r"^--|^==|^!=|^>=|^<=|^&&|^\|\|").unwrap();
        let keyword_re = regex::Regex::new(
            r"^int\b|^long\b|^void\b|^return\b|^if\b|^else\b|^do\b|^while\b|^for\b|^break\b|^continue\b|^static\b|^extern\b",
        )
        .unwrap();
        let constant_re = regex::Regex::new(r"^[0-9]+[lL]?\b").unwrap();
        let identifier_re = regex::Regex::new(r"^[a-zA-Z_]\w*\b").unwrap();

        let token = if let Some(m) = punctuation_double_re.find(src) {
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
        } else if let Some(m) = punctuation_re.find(src) {
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
                "=" => Token::Equal,
                "," => Token::Comma,
                ";" => Token::Semicolon,
                _ => unreachable!(),
            }
        } else if let Some(m) = keyword_re.find(src) {
            self.pos += m.as_str().len();
            match m.as_str() {
                "int" => Token::Int,
                "long" => Token::Long,
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
        } else if let Some(m) = constant_re.find(src) {
            self.pos += m.as_str().len();
            
            if m.as_str().ends_with('l') || m.as_str().ends_with('L') {
                Token::Constant(Const::Long(m.as_str().trim_end_matches(|c| c == 'l' || c == 'L').parse().unwrap()))
            } else {
                let as_i64 = m.as_str().parse::<i64>().unwrap();
                let konst = i32::try_from(as_i64)
                    .map(Const::Int)
                    .unwrap_or_else(|_| Const::Long(as_i64));

                Token::Constant(konst)
            }
        } else if let Some(m) = identifier_re.find(src) {
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
}