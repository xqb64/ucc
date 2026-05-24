use crate::{
    lexer::lex::{Const, Span, TokenKind},
    semantics::typechecker::TYPE_TABLE,
};

#[derive(Debug, Clone, PartialEq)]
pub enum BlockItem {
    Statement(Statement),
    Declaration(Declaration),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Variable(VariableDeclaration),
    Function(FunctionDeclaration),
    Struct(StructDeclaration),
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDeclaration {
    pub tag: String,
    pub members: Vec<MemberDeclaration>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberDeclaration {
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub name: String,
    pub ty: Type,
    pub init: Option<Initializer>,
    pub storage_class: Option<StorageClass>,
    pub is_global: bool,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Short,
    Int,
    Long,
    UShort,
    UInt,
    ULong,
    Float,
    Double,
    Func { params: Vec<Type>, ret: Box<Type> },
    Pointer(Box<Type>),
    Array { element: Box<Type>, size: usize },
    Char,
    SChar,
    UChar,
    Void,
    Struct { tag: String },
    Dummy,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    If(IfStatement),
    Compound(BlockStatement),
    DoWhile(DoWhileStatement),
    While(WhileStatement),
    For(ForStatement),
    Break(BreakStatement),
    Continue(ContinueStatement),
    Goto(GotoStatement),
    Labeled(LabeledStatement),
    Switch(SwitchStatement),
    Case(CaseStatement),
    Default(DefaultStatement),
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub block_items: Vec<BlockItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    pub name: String,
    pub ty: Type,
    pub params: Vec<String>,
    pub body: Box<Option<BlockItem>>,
    pub is_global: bool,
    pub storage_class: Option<StorageClass>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum StorageClass {
    Static,
    Extern,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStatement {
    pub expr: Option<Expression>,
    pub target_type: Option<Type>,
    pub belongs_to: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub condition: Expression,
    pub then_branch: Box<BlockItem>,
    pub else_branch: Box<Option<BlockItem>>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SwitchStatement {
    pub condition: Expression,
    pub body: Box<Statement>,
    pub label: String,
    pub cases: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CaseStatement {
    pub value: Expression,
    pub body: Box<Statement>,
    pub label: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefaultStatement {
    pub body: Box<Statement>,
    pub label: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStatement {
    pub expr: Expression,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement {
    pub stmts: Vec<BlockItem>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DoWhileStatement {
    pub condition: Expression,
    pub body: Box<BlockItem>,
    pub label: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Box<BlockItem>,
    pub label: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStatement {
    pub init: ForInit,
    pub condition: Option<Expression>,
    pub post: Option<Expression>,
    pub body: Box<BlockItem>,
    pub label: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ForInit {
    Declaration(VariableDeclaration),
    Expression(Option<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BreakStatement {
    pub label: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ContinueStatement {
    pub label: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GotoStatement {
    pub label: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LabeledStatement {
    pub label: String,
    pub body: Box<Statement>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expression {
    Constant(ConstantExpression),
    String(StringExpression),
    Literal(LiteralExpression),
    Variable(VariableExpression),
    Unary(UnaryExpression),
    Binary(BinaryExpression),
    Assign(AssignExpression),
    Conditional(ConditionalExpression),
    Call(CallExpression),
    Cast(CastExpression),
    Deref(DerefExpression),
    AddrOf(AddrOfExpression),
    Subscript(SubscriptExpression),
    Sizeof(SizeofExpression),
    SizeofT(SizeofTExpression),
    Dot(DotExpression),
    Arrow(ArrowExpression),
    Postfix(PostfixExpression),
    Compound(CompoundExpression),
}

pub fn expr_eq_ignoring_span(a: &Expression, b: &Expression) -> bool {
    use Expression::*;

    match (a, b) {
        (Constant(a), Constant(b)) => a.value == b.value && a.ty == b.ty,
        (String(a), String(b)) => a.value == b.value && a.ty == b.ty,
        (Variable(a), Variable(b)) => a.value == b.value && a.ty == b.ty,
        (Cast(a), Cast(b)) => {
            a.target_type == b.target_type && expr_eq_ignoring_span(&a.expr, &b.expr)
        }
        _ => unimplemented!(),
    }
}

pub fn spanof(e: &Expression) -> Span {
    match e {
        Expression::Constant(ConstantExpression { span, .. }) => span.to_owned(),
        Expression::Dot(DotExpression { span, .. }) => span.to_owned(),
        Expression::Call(CallExpression { span, .. }) => span.to_owned(),
        Expression::Cast(CastExpression { span, .. }) => span.to_owned(),
        Expression::Unary(UnaryExpression { span, .. }) => span.to_owned(),
        Expression::Deref(DerefExpression { span, .. }) => span.to_owned(),
        Expression::Arrow(ArrowExpression { span, .. }) => span.to_owned(),
        Expression::String(StringExpression { span, .. }) => span.to_owned(),
        Expression::Binary(BinaryExpression { span, .. }) => span.to_owned(),
        Expression::Assign(AssignExpression { span, .. }) => span.to_owned(),
        Expression::AddrOf(AddrOfExpression { span, .. }) => span.to_owned(),
        Expression::Sizeof(SizeofExpression { span, .. }) => span.to_owned(),
        Expression::Literal(LiteralExpression { span, .. }) => span.to_owned(),
        Expression::SizeofT(SizeofTExpression { span, .. }) => span.to_owned(),
        Expression::Postfix(PostfixExpression { span, .. }) => span.to_owned(),
        Expression::Variable(VariableExpression { span, .. }) => span.to_owned(),
        Expression::Compound(CompoundExpression { span, .. }) => span.to_owned(),
        Expression::Subscript(SubscriptExpression { span, .. }) => span.to_owned(),
        Expression::Conditional(ConditionalExpression { span, .. }) => span.to_owned(),
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CompoundExpression {
    pub kind: CompoundExpressionKind,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub result_t: Type,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CompoundExpressionKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseShl,
    BitwiseShr,
}

impl From<BinaryExpressionKind> for CompoundExpressionKind {
    fn from(value: BinaryExpressionKind) -> Self {
        match value {
            BinaryExpressionKind::Add => CompoundExpressionKind::Add,
            BinaryExpressionKind::Sub => CompoundExpressionKind::Sub,
            BinaryExpressionKind::Mul => CompoundExpressionKind::Mul,
            BinaryExpressionKind::Div => CompoundExpressionKind::Div,
            BinaryExpressionKind::Rem => CompoundExpressionKind::Mod,
            BinaryExpressionKind::BitwiseOr => CompoundExpressionKind::BitwiseOr,
            BinaryExpressionKind::BitwiseAnd => CompoundExpressionKind::BitwiseAnd,
            BinaryExpressionKind::BitwiseXor => CompoundExpressionKind::BitwiseXor,
            BinaryExpressionKind::BitwiseShl => CompoundExpressionKind::BitwiseShl,
            BinaryExpressionKind::BitwiseShr => CompoundExpressionKind::BitwiseShr,
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PostfixExpression {
    pub expr: Box<Expression>,
    pub kind: PostfixExpressionKind,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PostfixExpressionKind {
    Inc,
    Dec,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrowExpression {
    pub pointer: Box<Expression>,
    pub member: String,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DotExpression {
    pub structure: Box<Expression>,
    pub member: String,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SizeofExpression {
    pub expr: Box<Expression>,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SizeofTExpression {
    pub t: Type,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringExpression {
    pub value: String,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LiteralExpression {
    pub name: String,
    pub value: Box<Initializer>,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Initializer {
    Single(String, Expression),
    Compound(String, Type, Vec<Initializer>),
}

impl Initializer {
    pub fn zero(ty: &Type) -> Self {
        match ty {
            Type::Short => Initializer::Single(
                String::new(),
                Expression::Constant(ConstantExpression {
                    value: Const::Short(0),
                    ty: Type::Short,
                    span: Span { start: 0, end: 0 },
                }),
            ),
            Type::Int => Initializer::Single(
                String::new(),
                Expression::Constant(ConstantExpression {
                    value: Const::Int(0),
                    ty: Type::Int,
                    span: Span { start: 0, end: 0 },
                }),
            ),
            Type::UShort => Initializer::Single(
                String::new(),
                Expression::Constant(ConstantExpression {
                    value: Const::UShort(0),
                    ty: Type::UShort,
                    span: Span { start: 0, end: 0 },
                }),
            ),
            Type::UInt => Initializer::Single(
                String::new(),
                Expression::Constant(ConstantExpression {
                    value: Const::UInt(0),
                    ty: Type::UInt,
                    span: Span { start: 0, end: 0 },
                }),
            ),
            Type::Long => Initializer::Single(
                String::new(),
                Expression::Constant(ConstantExpression {
                    value: Const::Long(0),
                    ty: Type::Long,
                    span: Span { start: 0, end: 0 },
                }),
            ),
            Type::ULong => Initializer::Single(
                String::new(),
                Expression::Constant(ConstantExpression {
                    value: Const::ULong(0),
                    ty: Type::ULong,
                    span: Span { start: 0, end: 0 },
                }),
            ),
            Type::Float => Initializer::Single(
                String::new(),
                Expression::Constant(ConstantExpression {
                    value: Const::Float(0.0),
                    ty: Type::Float,
                    span: Span { start: 0, end: 0 },
                }),
            ),
            Type::Double => Initializer::Single(
                String::new(),
                Expression::Constant(ConstantExpression {
                    value: Const::Double(0.0),
                    ty: Type::Double,
                    span: Span { start: 0, end: 0 },
                }),
            ),
            Type::Char | Type::SChar => Initializer::Single(
                String::new(),
                Expression::Constant(ConstantExpression {
                    value: Const::Char(0),
                    ty: Type::Char,
                    span: Span { start: 0, end: 0 },
                }),
            ),
            Type::UChar => Initializer::Single(
                String::new(),
                Expression::Constant(ConstantExpression {
                    value: Const::UChar(0),
                    ty: Type::UChar,
                    span: Span { start: 0, end: 0 },
                }),
            ),
            Type::Pointer(_) => Initializer::Single(
                String::new(),
                Expression::Constant(ConstantExpression {
                    value: Const::ULong(0),
                    ty: Type::Int,
                    span: Span { start: 0, end: 0 },
                }),
            ),
            Type::Array { element, size } => {
                let mut inits = vec![];

                for _ in 0..*size {
                    inits.push(Self::zero(element));
                }

                Initializer::Compound(String::new(), *element.clone(), inits)
            }
            Type::Struct { tag } => {
                let struct_def = TYPE_TABLE.lock().unwrap().get(tag).unwrap().clone();
                let mut inits = vec![];

                for member in struct_def.members.iter() {
                    inits.push(Self::zero(&member.ty));
                }

                Initializer::Compound(String::new(), Type::Struct { tag: tag.clone() }, inits)
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SubscriptExpression {
    pub expr: Box<Expression>,
    pub index: Box<Expression>,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AddrOfExpression {
    pub expr: Box<Expression>,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DerefExpression {
    pub expr: Box<Expression>,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstantExpression {
    pub value: Const,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariableExpression {
    pub value: String,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnaryExpression {
    pub kind: UnaryExpressionKind,
    pub expr: Box<Expression>,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnaryExpressionKind {
    Negate,
    Complement,
    Not,
    Inc,
    Dec,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BinaryExpression {
    pub kind: BinaryExpressionKind,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryExpressionKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    And,
    Or,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    BitwiseShl,
    BitwiseShr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AssignExpression {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub op: TokenKind,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConditionalExpression {
    pub condition: Box<Expression>,
    pub then_expr: Box<Expression>,
    pub else_expr: Box<Expression>,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CallExpression {
    pub name: String,
    pub args: Vec<Expression>,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CastExpression {
    pub target_type: Type,
    pub expr: Box<Expression>,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declarator {
    Ident(String),
    Pointer(Box<Declarator>),
    Func(Vec<ParamInfo>, Box<Declarator>),
    Array(Box<Declarator>, usize),
}

pub type ParamInfo = (Type, Box<Declarator>);

#[derive(Debug, Clone, PartialEq)]
pub enum AbstractDeclarator {
    Pointer(Box<AbstractDeclarator>),
    Array(Box<AbstractDeclarator>, usize),
    Base,
}
