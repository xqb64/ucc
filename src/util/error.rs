use crate::lexer::lex::Span;

#[derive(Debug, Clone, Copy)]
pub enum ErrorKind {
    Lex,
    Parse,
    Resolve,
    LoopLabel,
    LabelCheck,
    Typecheck,
    CaseCollect,
    IrGen,
    Io,
}

#[derive(Debug, Clone)]
pub struct UccError {
    pub kind: ErrorKind,
    pub msg: String,
    pub span: Span,
}

impl std::fmt::Display for UccError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            ErrorKind::Lex => write!(f, "Lex error"),
            ErrorKind::Parse => write!(f, "Parse error"),
            ErrorKind::LoopLabel => write!(f, "LoopLabel error"),
            ErrorKind::LabelCheck => write!(f, "LabelCheck error"),
            ErrorKind::CaseCollect => write!(f, "CaseCollect error"),
            ErrorKind::Resolve => write!(f, "Resolve error"),
            ErrorKind::Typecheck => write!(f, "Typecheck error"),
            ErrorKind::IrGen => write!(f, "IrGen error"),
            ErrorKind::Io => write!(f, "Io error"),
        }
    }
}

impl std::error::Error for UccError {}

impl From<std::io::Error> for UccError {
    fn from(e: std::io::Error) -> Self {
        UccError {
            kind: ErrorKind::Io,
            msg: e.to_string(),
            span: Span { start: 0, end: 0 },
        }
    }
}

pub type Result<T> = std::result::Result<T, UccError>;
