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

        let punctuation_re = regex::Regex::new(r"^[-+*/%~(){};!<>=?:]").unwrap();
        let punctuation_double_re = regex::Regex::new(r"^--|^==|^!=|^>=|^<=|^&&|^\|\|").unwrap();
        let keyword_re = regex::Regex::new(r"^int\b|^void\b|^return\b|^if\b|^else\b|^do\b|^while\b|^for\b|^break\b|^continue\b").unwrap();
        let constant_re = regex::Regex::new(r"^[0-9]+\b").unwrap();
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
                ";" => Token::Semicolon,
                _ => unreachable!(),
            }
        } else if let Some(m) = keyword_re.find(src) {
            self.pos += m.as_str().len();
            match m.as_str() {
                "int" => Token::Int,
                "void" => Token::Void,
                "return" => Token::Return,
                "if" => Token::If,
                "else" => Token::Else,
                "do" => Token::Do,
                "while" => Token::While,
                "for" => Token::For,
                "break" => Token::Break,
                "continue" => Token::Continue,
                _ => unreachable!(),
            }
        } else if let Some(m) = constant_re.find(src) {
            self.pos += m.as_str().len();
            Token::Constant(m.as_str().parse().unwrap())
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
    Void,
    Return,
    If,
    Else,
    Do,
    While,
    For,
    Break,
    Continue,
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
    Semicolon,
    Identifier(String),
    Constant(i32),
    Error,
}

impl Token {
    pub fn as_const(&self) -> i32 {
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
