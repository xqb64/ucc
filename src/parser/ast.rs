use crate::lexer::lex::{Const, Token};

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
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberDeclaration {
    pub name: String,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub name: String,
    pub _type: Type,
    pub init: Option<Initializer>,
    pub storage_class: Option<StorageClass>,
    pub is_global: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Long,
    UInt,
    ULong,
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
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub block_items: Vec<BlockItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    pub name: String,
    pub _type: Type,
    pub params: Vec<String>,
    pub body: Box<Option<BlockItem>>,
    pub is_global: bool,
    pub storage_class: Option<StorageClass>,
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub condition: Expression,
    pub then_branch: Box<BlockItem>,
    pub else_branch: Box<Option<BlockItem>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStatement {
    pub expr: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement {
    pub stmts: Vec<BlockItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DoWhileStatement {
    pub condition: Expression,
    pub body: Box<BlockItem>,
    pub label: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Box<BlockItem>,
    pub label: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStatement {
    pub init: ForInit,
    pub condition: Option<Expression>,
    pub post: Option<Expression>,
    pub body: Box<BlockItem>,
    pub label: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ForInit {
    Declaration(VariableDeclaration),
    Expression(Option<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BreakStatement {
    pub label: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ContinueStatement {
    pub label: String,
}

#[derive(Debug, Clone, PartialEq)]
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrowExpression {
    pub pointer: Box<Expression>,
    pub member: String,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DotExpression {
    pub structure: Box<Expression>,
    pub member: String,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SizeofExpression {
    pub expr: Box<Expression>,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SizeofTExpression {
    pub t: Type,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StringExpression {
    pub value: String,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralExpression {
    pub name: String,
    pub value: Box<Initializer>,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Initializer {
    Single(String, Expression),
    Compound(String, Type, Vec<Initializer>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SubscriptExpression {
    pub expr: Box<Expression>,
    pub index: Box<Expression>,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AddrOfExpression {
    pub expr: Box<Expression>,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DerefExpression {
    pub expr: Box<Expression>,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstantExpression {
    pub value: Const,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableExpression {
    pub value: String,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    pub kind: UnaryExpressionKind,
    pub expr: Box<Expression>,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryExpressionKind {
    Negate,
    Complement,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub kind: BinaryExpressionKind,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignExpression {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub op: Token,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConditionalExpression {
    pub condition: Box<Expression>,
    pub then_expr: Box<Expression>,
    pub else_expr: Box<Expression>,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub name: String,
    pub args: Vec<Expression>,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CastExpression {
    pub target_type: Type,
    pub expr: Box<Expression>,
    pub _type: Type,
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
