use std::sync::atomic::AtomicUsize;

use crate::{
    lexer::lex::{Const, Span},
    parser::ast::*,
    semantics::typechecker::{
        get_signedness, get_size_of_type, get_type, is_integer_type, is_pointer_type,
        IdentifierAttrs, InitialValue, StaticInit, Symbol, SYMBOL_TABLE, TYPE_TABLE,
    },
    util::cfg::{self, Instr, SimpleInstr},
};

#[derive(Debug, Clone, PartialEq)]
pub enum IRNode {
    Program(IRProgram),
    Function(IRFunction),
    Instructions(Vec<IRInstruction>),
    Value(IRValue),
    StaticVariable(IRStaticVariable),
    StaticConstant(IRStaticConstant),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IRProgram {
    pub functions: Vec<IRFunction>,
    pub static_vars: Vec<IRStaticVariable>,
    pub static_constants: Vec<IRStaticConstant>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IRFunction {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<IRInstruction>,
    pub global: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IRStaticConstant {
    pub name: String,
    pub ty: Type,
    pub init: StaticInit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IRStaticVariable {
    pub name: String,
    pub ty: Type,
    pub init: Vec<StaticInit>,
    pub global: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IRInstruction {
    Unary {
        op: UnaryOp,
        src: IRValue,
        dst: IRValue,
    },
    Binary {
        op: BinaryOp,
        lhs: IRValue,
        rhs: IRValue,
        dst: IRValue,
    },
    Copy {
        src: IRValue,
        dst: IRValue,
    },
    GetAddress {
        src: IRValue,
        dst: IRValue,
    },
    Load {
        src_ptr: IRValue,
        dst: IRValue,
    },
    Store {
        src: IRValue,
        dst_ptr: IRValue,
    },
    Jump(String),
    JumpIfZero {
        condition: IRValue,
        target: String,
    },
    JumpIfNotZero {
        condition: IRValue,
        target: String,
    },
    Label(String),
    Call {
        target: String,
        args: Vec<IRValue>,
        dst: Option<IRValue>,
    },
    SignExtend {
        src: IRValue,
        dst: IRValue,
    },
    ZeroExtend {
        src: IRValue,
        dst: IRValue,
    },
    Truncate {
        src: IRValue,
        dst: IRValue,
    },
    DoubleToInt {
        src: IRValue,
        dst: IRValue,
    },
    IntToDouble {
        src: IRValue,
        dst: IRValue,
    },
    DoubletoUInt {
        src: IRValue,
        dst: IRValue,
    },
    UIntToDouble {
        src: IRValue,
        dst: IRValue,
    },
    DoubleToFloat {
        src: IRValue,
        dst: IRValue,
    },
    FloatToDouble {
        src: IRValue,
        dst: IRValue,
    },
    FloatToInt {
        src: IRValue,
        dst: IRValue,
    },
    IntToFloat {
        src: IRValue,
        dst: IRValue,
    },
    FloattoUInt {
        src: IRValue,
        dst: IRValue,
    },
    UIntToFloat {
        src: IRValue,
        dst: IRValue,
    },
    AddPtr {
        ptr: IRValue,
        index: IRValue,
        scale: usize,
        dst: IRValue,
    },
    CopyToOffset {
        src: IRValue,
        dst: String,
        offset: usize,
    },
    CopyFromOffset {
        src: String,
        offset: usize,
        dst: IRValue,
    },
    Ret(Option<IRValue>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IRValue {
    Constant(Const),
    Var(String),
}

#[derive(Debug, Clone, PartialEq, Copy, Eq, Hash)]
pub enum UnaryOp {
    Negate,
    Complement,
    Not,
    Inc,
    Dec,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Less,
    Greater,
    Equal,
    NotEqual,
    LessEqual,
    GreaterEqual,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    BitwiseShl,
    BitwiseShr,
}

impl From<BinaryExpressionKind> for BinaryOp {
    fn from(value: BinaryExpressionKind) -> Self {
        match value {
            BinaryExpressionKind::Add => BinaryOp::Add,
            BinaryExpressionKind::Sub => BinaryOp::Sub,
            BinaryExpressionKind::Mul => BinaryOp::Mul,
            BinaryExpressionKind::Div => BinaryOp::Div,
            BinaryExpressionKind::Rem => BinaryOp::Rem,
            BinaryExpressionKind::Less => BinaryOp::Less,
            BinaryExpressionKind::Greater => BinaryOp::Greater,
            BinaryExpressionKind::Equal => BinaryOp::Equal,
            BinaryExpressionKind::NotEqual => BinaryOp::NotEqual,
            BinaryExpressionKind::GreaterEqual => BinaryOp::GreaterEqual,
            BinaryExpressionKind::LessEqual => BinaryOp::LessEqual,
            BinaryExpressionKind::BitwiseOr => BinaryOp::BitwiseOr,
            BinaryExpressionKind::BitwiseXor => BinaryOp::BitwiseXor,
            BinaryExpressionKind::BitwiseAnd => BinaryOp::BitwiseAnd,
            BinaryExpressionKind::BitwiseShl => BinaryOp::BitwiseShl,
            BinaryExpressionKind::BitwiseShr => BinaryOp::BitwiseShr,
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum ExpResult {
    PlainOperand(IRValue),
    DereferencedPointer(IRValue),
    SubObject { base: String, offset: usize },
}

pub trait Irfy {
    fn irfy(&self, funcname: &str) -> Option<IRNode>;
}

impl Irfy for Statement {
    fn irfy(&self, funcname: &str) -> Option<IRNode> {
        match self {
            Statement::Return(ret_stmt) => ret_stmt.irfy(funcname),
            Statement::Expression(expr_stmt) => expr_stmt.irfy(funcname),
            Statement::If(if_stmt) => if_stmt.irfy(funcname),
            Statement::Compound(block_stmt) => block_stmt.irfy(funcname),
            Statement::Break(break_stmt) => break_stmt.irfy(funcname),
            Statement::Continue(continue_stmt) => continue_stmt.irfy(funcname),
            Statement::While(while_stmt) => while_stmt.irfy(funcname),
            Statement::DoWhile(do_while_stmt) => do_while_stmt.irfy(funcname),
            Statement::For(for_stmt) => for_stmt.irfy(funcname),
            Statement::Goto(goto_stmt) => goto_stmt.irfy(funcname),
            Statement::Labeled(labeled_stmt) => labeled_stmt.irfy(funcname),
            Statement::Switch(switch_stmt) => switch_stmt.irfy(funcname),
            Statement::Case(case_stmt) => case_stmt.irfy(funcname),
            Statement::Default(default_stmt) => default_stmt.irfy(funcname),
            Statement::Null => None,
        }
    }
}

impl Irfy for Program {
    fn irfy(&self, funcname: &str) -> Option<IRNode> {
        let mut instructions = vec![];

        for block_item in &self.block_items {
            match block_item {
                BlockItem::Declaration(decl) => match decl {
                    Declaration::Function(func_decl) => {
                        if let Some(ir_func) = func_decl.irfy(&func_decl.name) {
                            instructions.push(ir_func.into());
                        }
                    }

                    Declaration::Variable(var_decl) => {
                        if var_decl.is_global {
                            continue;
                        }

                        if !var_decl.is_global && var_decl.storage_class.is_some() {
                            continue;
                        }
                    }

                    _ => {}
                },

                BlockItem::Statement(stmt) => {
                    if let Some(ir_stmt) = stmt.irfy(funcname) {
                        instructions.push(ir_stmt.into());
                    }
                }
            }
        }

        Some(IRNode::Program(IRProgram {
            functions: instructions,
            static_vars: vec![],
            static_constants: vec![],
        }))
    }
}

impl Irfy for BlockStatement {
    fn irfy(&self, funcname: &str) -> Option<IRNode> {
        let mut instructions = vec![];

        for stmt in &self.stmts {
            match stmt {
                BlockItem::Declaration(decl) => match decl {
                    Declaration::Variable(var_decl) => {
                        if !var_decl.is_global && var_decl.storage_class.is_some() {
                            continue;
                        } else {
                            instructions.extend::<Vec<IRInstruction>>(
                                var_decl.irfy(funcname).unwrap().into(),
                            );
                        }
                    }

                    Declaration::Function(_func_decl) => {}

                    Declaration::Struct(_) => {}
                },

                BlockItem::Statement(stmt) => {
                    if let Some(ir_stmt) = stmt.irfy(funcname) {
                        instructions.extend::<Vec<IRInstruction>>(ir_stmt.into());
                    }
                }
            }
        }

        Some(IRNode::Instructions(instructions))
    }
}

impl Irfy for IfStatement {
    fn irfy(&self, funcname: &str) -> Option<IRNode> {
        let mut instructions = vec![];

        let tmp = make_temporary();

        let else_label = format!("Else.{}", tmp);
        let end_label = format!("End.{}", tmp);

        let condition = emit_tacky_and_convert(&self.condition, &mut instructions);

        if self.else_branch.is_none() {
            instructions.push(IRInstruction::JumpIfZero {
                condition,
                target: end_label.clone(),
            });

            if let Some(then_branch) = self.then_branch.irfy(funcname) {
                instructions.extend::<Vec<IRInstruction>>(then_branch.into());
            }

            instructions.push(IRInstruction::Label(end_label));
        } else {
            instructions.push(IRInstruction::JumpIfZero {
                condition,
                target: else_label.clone(),
            });

            if let Some(then_branch) = self.then_branch.irfy(funcname) {
                instructions.extend::<Vec<IRInstruction>>(then_branch.into());
            }

            instructions.push(IRInstruction::Jump(end_label.clone()));

            instructions.push(IRInstruction::Label(else_label.clone()));

            if let Some(else_branch) = &*self.else_branch {
                if let Some(else_instrs) = else_branch.irfy(funcname) {
                    instructions.extend::<Vec<IRInstruction>>(else_instrs.into());
                }
            }

            instructions.push(IRInstruction::Label(end_label));
        }

        Some(IRNode::Instructions(instructions))
    }
}

pub fn expr2const(e: &Expression) -> Const {
    match e {
        Expression::Constant(ConstantExpression { value, .. }) => *value,
        Expression::Cast(CastExpression {
            target_type, expr, ..
        }) => {
            let c = expr2const(expr);
            cast_const_to(&c, target_type)
        }
        _ => unreachable!(),
    }
}

fn cast_const_to(c: &Const, target: &Type) -> Const {
    match target {
        Type::Short => Const::Short(c.to_i16()),
        Type::Int => Const::Int(c.to_i32()),
        Type::Long => Const::Long(c.to_i64()),
        Type::UShort => Const::UShort(c.to_u16()),
        Type::UInt => Const::UInt(c.to_u32()),
        Type::ULong => Const::ULong(c.to_u64()),
        Type::Float => Const::Float(c.to_f32()),
        Type::Double => Const::Double(c.to_f64()),
        Type::Char | Type::SChar => Const::Char(c.to_i8()),
        Type::UChar => Const::UChar(c.to_u8()),
        _ => unreachable!(),
    }
}

pub trait ConstCast {
    fn to_i16(&self) -> i16;
    fn to_i32(&self) -> i32;
    fn to_i64(&self) -> i64;
    fn to_u16(&self) -> u16;
    fn to_u32(&self) -> u32;
    fn to_u64(&self) -> u64;
    fn to_f32(&self) -> f32;
    fn to_f64(&self) -> f64;
    fn to_i8(&self) -> i8;
    fn to_u8(&self) -> u8;
}

impl ConstCast for Const {
    fn to_i16(&self) -> i16 {
        match *self {
            Const::Short(v) => v,
            Const::Int(v) => v as i16,
            Const::Long(v) => v as i16,
            Const::UShort(v) => v as i16,
            Const::UInt(v) => v as i16,
            Const::ULong(v) => v as i16,
            Const::Float(v) => v as i16,
            Const::Double(v) => v as i16,
            Const::Char(v) => v as i16,
            Const::UChar(v) => v as i16,
        }
    }

    fn to_i32(&self) -> i32 {
        match *self {
            Const::Short(v) => v as i32,
            Const::Int(v) => v,
            Const::Long(v) => v as i32,
            Const::UShort(v) => v as i32,
            Const::UInt(v) => v as i32,
            Const::ULong(v) => v as i32,
            Const::Float(v) => v as i32,
            Const::Double(v) => v as i32,
            Const::Char(v) => v as i32,
            Const::UChar(v) => v as i32,
        }
    }

    fn to_i64(&self) -> i64 {
        match *self {
            Const::Short(v) => v as i64,
            Const::Int(v) => v as i64,
            Const::Long(v) => v,
            Const::UShort(v) => v as i64,
            Const::UInt(v) => v as i64,
            Const::ULong(v) => v as i64,
            Const::Float(v) => v as i64,
            Const::Double(v) => v as i64,
            Const::Char(v) => v as i64,
            Const::UChar(v) => v as i64,
        }
    }

    fn to_u16(&self) -> u16 {
        match *self {
            Const::Short(v) => v as u16,
            Const::Int(v) => v as u16,
            Const::Long(v) => v as u16,
            Const::UShort(v) => v,
            Const::UInt(v) => v as u16,
            Const::ULong(v) => v as u16,
            Const::Float(v) => v as u16,
            Const::Double(v) => v as u16,
            Const::Char(v) => v as u16,
            Const::UChar(v) => v as u16,
        }
    }

    fn to_u32(&self) -> u32 {
        match *self {
            Const::Short(v) => v as u32,
            Const::Int(v) => v as u32,
            Const::Long(v) => v as u32,
            Const::UShort(v) => v as u32,
            Const::UInt(v) => v,
            Const::ULong(v) => v as u32,
            Const::Float(v) => v as u32,
            Const::Double(v) => v as u32,
            Const::Char(v) => v as u32,
            Const::UChar(v) => v as u32,
        }
    }

    fn to_u64(&self) -> u64 {
        match *self {
            Const::Short(v) => v as u64,
            Const::Int(v) => v as u64,
            Const::Long(v) => v as u64,
            Const::UShort(v) => v as u64,
            Const::UInt(v) => v as u64,
            Const::ULong(v) => v,
            Const::Float(v) => v as u64,
            Const::Double(v) => v as u64,
            Const::Char(v) => v as u64,
            Const::UChar(v) => v as u64,
        }
    }

    fn to_f32(&self) -> f32 {
        match *self {
            Const::Short(v) => v as f32,
            Const::Int(v) => v as f32,
            Const::Long(v) => v as f32,
            Const::UShort(v) => v as f32,
            Const::UInt(v) => v as f32,
            Const::ULong(v) => v as f32,
            Const::Float(v) => v,
            Const::Double(v) => v as f32,
            Const::Char(v) => v as f32,
            Const::UChar(v) => v as f32,
        }
    }

    fn to_f64(&self) -> f64 {
        match *self {
            Const::Short(v) => v as f64,
            Const::Int(v) => v as f64,
            Const::Long(v) => v as f64,
            Const::UShort(v) => v as f64,
            Const::UInt(v) => v as f64,
            Const::ULong(v) => v as f64,
            Const::Float(v) => v as f64,
            Const::Double(v) => v,
            Const::Char(v) => v as f64,
            Const::UChar(v) => v as f64,
        }
    }

    fn to_i8(&self) -> i8 {
        match *self {
            Const::Short(v) => v as i8,
            Const::Int(v) => v as i8,
            Const::Long(v) => v as i8,
            Const::UShort(v) => v as i8,
            Const::UInt(v) => v as i8,
            Const::ULong(v) => v as i8,
            Const::Float(v) => v as i8,
            Const::Double(v) => v as i8,
            Const::Char(v) => v,
            Const::UChar(v) => v as i8,
        }
    }

    fn to_u8(&self) -> u8 {
        match *self {
            Const::Short(v) => v as u8,
            Const::Int(v) => v as u8,
            Const::Long(v) => v as u8,
            Const::UShort(v) => v as u8,
            Const::UInt(v) => v as u8,
            Const::ULong(v) => v as u8,
            Const::Float(v) => v as u8,
            Const::Double(v) => v as u8,
            Const::Char(v) => v as u8,
            Const::UChar(v) => v,
        }
    }
}

impl Irfy for SwitchStatement {
    fn irfy(&self, funcname: &str) -> Option<IRNode> {
        let mut instructions = vec![];

        let condition = emit_tacky_and_convert(&self.condition, &mut instructions);

        for case in self.cases.iter() {
            if let Statement::Case(CaseStatement {
                value,
                body: _,
                label: case_label,
                ..
            }) = case
            {
                let result = make_tacky_variable(get_type(&self.condition));

                let konst = expr2const(value);

                instructions.push(IRInstruction::Binary {
                    op: BinaryOp::Equal,
                    lhs: condition.clone(),
                    rhs: IRValue::Constant(konst),
                    dst: result.clone(),
                });

                instructions.push(IRInstruction::JumpIfNotZero {
                    condition: result.clone(),
                    target: case_label.clone(),
                });
            }
        }

        if let Some(Statement::Default(DefaultStatement { label, .. })) = self
            .cases
            .iter()
            .find(|stmt| matches!(stmt, Statement::Default(_)))
        {
            instructions.push(IRInstruction::Jump(label.clone()));
        }

        let break_label = format!("{}.break", self.label.clone());
        instructions.push(IRInstruction::Jump(break_label.clone()));

        if let Some(instrs) = self.body.irfy(funcname) {
            instructions.extend::<Vec<IRInstruction>>(instrs.into());
        }

        instructions.push(IRInstruction::Label(break_label));

        Some(IRNode::Instructions(instructions))
    }
}

impl Irfy for CaseStatement {
    fn irfy(&self, funcname: &str) -> Option<IRNode> {
        let mut instructions = vec![];

        instructions.push(IRInstruction::Label(self.label.clone()));

        if let Some(instrs) = self.body.irfy(funcname) {
            instructions.extend::<Vec<IRInstruction>>(instrs.into());
        }

        Some(IRNode::Instructions(instructions))
    }
}

impl Irfy for DefaultStatement {
    fn irfy(&self, funcname: &str) -> Option<IRNode> {
        let mut instructions = vec![];
        instructions.push(IRInstruction::Label(self.label.clone()));
        if let Some(instrs) = self.body.irfy(funcname) {
            instructions.extend::<Vec<IRInstruction>>(instrs.into());
        }

        Some(IRNode::Instructions(instructions))
    }
}

impl Irfy for BreakStatement {
    fn irfy(&self, _funcname: &str) -> Option<IRNode> {
        Some(IRNode::Instructions(vec![IRInstruction::Jump(format!(
            "{}.break",
            self.label.clone()
        ))]))
    }
}

impl Irfy for ContinueStatement {
    fn irfy(&self, _funcname: &str) -> Option<IRNode> {
        Some(IRNode::Instructions(vec![IRInstruction::Jump(format!(
            "{}.continue",
            self.label.clone()
        ))]))
    }
}

impl Irfy for GotoStatement {
    fn irfy(&self, funcname: &str) -> Option<IRNode> {
        Some(IRNode::Instructions(vec![IRInstruction::Jump(format!(
            "{}.{}",
            self.label.clone(),
            funcname
        ))]))
    }
}

impl Irfy for LabeledStatement {
    fn irfy(&self, funcname: &str) -> Option<IRNode> {
        let mut instructions = vec![];

        let label = format!("{}.{}", self.label.clone(), funcname);

        instructions.push(IRInstruction::Label(label.clone()));

        if let Some(body) = self.body.irfy(funcname) {
            instructions.extend::<Vec<IRInstruction>>(body.into());
        }

        Some(IRNode::Instructions(instructions))
    }
}

impl Irfy for DoWhileStatement {
    fn irfy(&self, funcname: &str) -> Option<IRNode> {
        let mut instructions = vec![];

        let start_label = self.label.clone();

        let continue_label = format!("{}.continue", self.label);
        let break_label = format!("{}.break", self.label);

        instructions.push(IRInstruction::Label(start_label.clone()));

        if let Some(body) = self.body.irfy(funcname) {
            instructions.extend::<Vec<IRInstruction>>(body.into());
        }

        instructions.push(IRInstruction::Label(continue_label.clone()));

        let condition = emit_tacky_and_convert(&self.condition, &mut instructions);

        instructions.push(IRInstruction::JumpIfNotZero {
            condition,
            target: start_label.clone(),
        });

        instructions.push(IRInstruction::Label(break_label.clone()));

        Some(IRNode::Instructions(instructions))
    }
}

impl Irfy for WhileStatement {
    fn irfy(&self, funcname: &str) -> Option<IRNode> {
        let mut instructions = vec![];

        let continue_label = format!("{}.continue", self.label);
        let break_label = format!("{}.break", self.label);

        instructions.push(IRInstruction::Label(continue_label.clone()));

        let condition = emit_tacky_and_convert(&self.condition, &mut instructions);

        instructions.push(IRInstruction::JumpIfZero {
            condition,
            target: break_label.clone(),
        });

        instructions.extend::<Vec<IRInstruction>>(self.body.irfy(funcname).unwrap().into());

        instructions.push(IRInstruction::Jump(continue_label.clone()));

        instructions.push(IRInstruction::Label(break_label.clone()));

        Some(IRNode::Instructions(instructions))
    }
}

impl Irfy for ForStatement {
    fn irfy(&self, funcname: &str) -> Option<IRNode> {
        let mut instructions = vec![];

        let start_label = format!("{}.start", self.label);
        let break_label = format!("{}.break", self.label);
        let continue_label = format!("{}.continue", self.label);

        instructions.extend::<Vec<IRInstruction>>(self.init.irfy(funcname).unwrap().into());

        instructions.push(IRInstruction::Label(start_label.clone()));

        if self.condition.is_some() {
            let condition =
                emit_tacky_and_convert(self.condition.as_ref().unwrap(), &mut instructions);
            instructions.push(IRInstruction::JumpIfZero {
                condition,
                target: break_label.clone(),
            });
        }

        if let Some(instrs) = self.body.irfy(funcname) {
            instructions.extend::<Vec<IRInstruction>>(instrs.into());
        }

        instructions.push(IRInstruction::Label(continue_label.clone()));

        if self.post.is_some() {
            emit_tacky(self.post.as_ref().unwrap(), &mut instructions);
        }

        instructions.push(IRInstruction::Jump(start_label.clone()));

        instructions.push(IRInstruction::Label(break_label.clone()));

        Some(IRNode::Instructions(instructions))
    }
}

impl Irfy for ForInit {
    fn irfy(&self, funcname: &str) -> Option<IRNode> {
        match self {
            ForInit::Expression(expr) => {
                let mut instructions = vec![];
                if expr.is_some() {
                    let _ = emit_tacky(expr.as_ref().unwrap(), &mut instructions);
                }
                Some(IRNode::Instructions(instructions))
            }
            ForInit::Declaration(decl) => decl.irfy(funcname),
        }
    }
}

impl Irfy for ReturnStatement {
    fn irfy(&self, _funcname: &str) -> Option<IRNode> {
        let mut instructions = vec![];
        let result = optionally_emit_tacky_and_convert(&self.expr, &mut instructions);
        instructions.push(IRInstruction::Ret(result));
        Some(IRNode::Instructions(instructions))
    }
}

impl Irfy for ExpressionStatement {
    fn irfy(&self, _funcname: &str) -> Option<IRNode> {
        let mut instructions = vec![];
        let _ = emit_tacky(&self.expr, &mut instructions);
        Some(IRNode::Instructions(instructions))
    }
}

impl Irfy for BlockItem {
    fn irfy(&self, funcname: &str) -> Option<IRNode> {
        match self {
            BlockItem::Declaration(decl) => match decl {
                Declaration::Function(func) => func.irfy(funcname),
                Declaration::Variable(var) => var.irfy(funcname),
                _ => todo!(),
            },
            BlockItem::Statement(stmt) => stmt.irfy(funcname),
        }
    }
}

impl Irfy for FunctionDeclaration {
    fn irfy(&self, funcname: &str) -> Option<IRNode> {
        let mut instructions = vec![];

        if self.body.is_none() {
            return None;
        }

        for stmt in self.body.iter() {
            instructions.extend::<Vec<IRInstruction>>(stmt.irfy(funcname).unwrap().into());
        }

        instructions.push(IRInstruction::Ret(Some(IRValue::Constant(Const::Int(0)))));

        Some(IRNode::Function(IRFunction {
            name: self.name.clone(),
            params: self.params.clone(),
            body: instructions,
            global: self.is_global,
        }))
    }
}

impl Irfy for VariableDeclaration {
    fn irfy(&self, _funcname: &str) -> Option<IRNode> {
        let mut instructions = vec![];

        if let Some(Initializer::Single(_, Expression::String(_))) = &self.init {
            emit_compound_init(
                &self.name,
                self.init.as_ref().unwrap(),
                &mut instructions,
                0,
                &self.ty,
            );
        } else if let Some(Initializer::Single(_, init)) = &self.init {
            let result = emit_tacky_and_convert(init, &mut instructions);
            instructions.push(IRInstruction::Copy {
                src: result,
                dst: IRValue::Var(self.name.clone()),
            });
        } else if let Some(Initializer::Compound(_, _, _)) = &self.init {
            emit_compound_init(
                &self.name,
                self.init.as_ref().unwrap(),
                &mut instructions,
                0,
                &self.ty,
            );
        }

        Some(IRNode::Instructions(instructions))
    }
}

fn emit_tacky_and_convert(e: &Expression, instructions: &mut Vec<IRInstruction>) -> IRValue {
    let result = emit_tacky(e, instructions);
    match result {
        ExpResult::SubObject { base, offset } => {
            let dst = make_tacky_variable(get_type(e));
            instructions.push(IRInstruction::CopyFromOffset {
                src: base,
                offset,
                dst: dst.clone(),
            });
            dst
        }
        ExpResult::PlainOperand(val) => val,
        ExpResult::DereferencedPointer(ptr) => {
            let dst = make_tacky_variable(get_type(e));
            instructions.push(IRInstruction::Load {
                src_ptr: ptr,
                dst: dst.clone(),
            });

            dst
        }
    }
}

fn optionally_emit_tacky_and_convert(
    e: &Option<Expression>,
    instructions: &mut Vec<IRInstruction>,
) -> Option<IRValue> {
    e.as_ref().map(|e| emit_tacky_and_convert(e, instructions))
}

fn make_constexpr(konst: isize, ty: Type) -> Expression {
    match ty {
        Type::Char | Type::SChar => Expression::Constant(ConstantExpression {
            value: Const::Char(konst as i8),
            ty: ty.clone(),
            span: Span { start: 0, end: 0 },
        }),
        Type::UChar => Expression::Constant(ConstantExpression {
            value: Const::UChar(konst as u8),
            ty: ty.clone(),
            span: Span { start: 0, end: 0 },
        }),
        Type::Short => Expression::Constant(ConstantExpression {
            value: Const::Short(konst as i16),
            ty: ty.clone(),
            span: Span { start: 0, end: 0 },
        }),
        Type::UShort => Expression::Constant(ConstantExpression {
            value: Const::UShort(konst as u16),
            ty: ty.clone(),
            span: Span { start: 0, end: 0 },
        }),
        Type::Int => Expression::Constant(ConstantExpression {
            value: Const::Int(konst as i32),
            ty: ty.clone(),
            span: Span { start: 0, end: 0 },
        }),
        Type::UInt => Expression::Constant(ConstantExpression {
            value: Const::UInt(konst as u32),
            ty: ty.clone(),
            span: Span { start: 0, end: 0 },
        }),
        Type::Long => Expression::Constant(ConstantExpression {
            value: Const::Long(konst as i64),
            ty: ty.clone(),
            span: Span { start: 0, end: 0 },
        }),
        Type::ULong => Expression::Constant(ConstantExpression {
            value: Const::ULong(konst as u64),
            ty: ty.clone(),
            span: Span { start: 0, end: 0 },
        }),
        Type::Float => Expression::Constant(ConstantExpression {
            value: Const::Float(konst as f32),
            ty: ty.clone(),
            span: Span { start: 0, end: 0 },
        }),
        Type::Double => Expression::Constant(ConstantExpression {
            value: Const::Double(konst as f64),
            ty: ty.clone(),
            span: Span { start: 0, end: 0 },
        }),
        Type::Pointer(_) => Expression::Constant(ConstantExpression {
            value: Const::ULong(konst as u64),
            ty: ty.clone(),
            span: Span { start: 0, end: 0 },
        }),
        _ => unreachable!(),
    }
}

fn make_const(konst: isize, ty: Type) -> Const {
    match ty {
        Type::Char | Type::SChar => Const::Char(konst as i8),
        Type::UChar => Const::UChar(konst as u8),
        Type::Short => Const::Short(konst as i16),
        Type::UShort => Const::UShort(konst as u16),
        Type::Int => Const::Int(konst as i32),
        Type::UInt => Const::UInt(konst as u32),
        Type::Long => Const::Long(konst as i64),
        Type::ULong => Const::ULong(konst as u64),
        Type::Float => Const::Float(konst as f32),
        Type::Double => Const::Double(konst as f64),
        Type::Pointer(_) => Const::ULong(konst as i64 as u64),
        _ => {
            println!("ty: {:?}", ty);
            unreachable!()
        }
    }
}

fn make_cast_instruction(src: &IRValue, dst: &IRValue, src_t: &Type, dst_t: &Type) -> IRInstruction {
    match (dst_t, src_t) {
        (Type::Float, Type::Double) => IRInstruction::DoubleToFloat {
            src: src.clone(),
            dst: dst.clone(),
        },
        (Type::Double, Type::Float) => IRInstruction::FloatToDouble {
            src: src.clone(),
            dst: dst.clone(),
        },
        (Type::Float, _) => {
            if get_signedness(src_t) {
                IRInstruction::IntToFloat {
                    src: src.clone(),
                    dst: dst.clone(),
                }
            } else {
                IRInstruction::UIntToFloat {
                    src: src.clone(),
                    dst: dst.clone(),
                }
            }
        }
        (Type::Double, _) => {
            if get_signedness(src_t) {
                IRInstruction::IntToDouble {
                    src: src.clone(),
                    dst: dst.clone(),
                }
            } else {
                IRInstruction::UIntToDouble {
                    src: src.clone(),
                    dst: dst.clone(),
                }
            }
        }
        (_, Type::Float) => {
            if get_signedness(dst_t) {
                IRInstruction::FloatToInt {
                    src: src.clone(),
                    dst: dst.clone(),
                }
            } else {
                IRInstruction::FloattoUInt {
                    src: src.clone(),
                    dst: dst.clone(),
                }
            }
        }
        (_, Type::Double) => {
            if get_signedness(dst_t) {
                IRInstruction::DoubleToInt {
                    src: src.clone(),
                    dst: dst.clone(),
                }
            } else {
                IRInstruction::DoubletoUInt {
                    src: src.clone(),
                    dst: dst.clone(),
                }
            }
        }
        _ => {
            if get_size_of_type(dst_t) == get_size_of_type(src_t) {
                IRInstruction::Copy {
                    src: src.clone(),
                    dst: dst.clone(),
                }
            } else if get_size_of_type(dst_t) < get_size_of_type(src_t) {
                IRInstruction::Truncate {
                    src: src.clone(),
                    dst: dst.clone(),
                }
            } else if get_signedness(src_t) {
                IRInstruction::SignExtend {
                    src: src.clone(),
                    dst: dst.clone(),
                }
            } else {
                IRInstruction::ZeroExtend {
                    src: src.clone(),
                    dst: dst.clone(),
                }
            }
        }
    }
}

fn emit_compound_expression(
    kind: BinaryExpressionKind,
    lhs: &Expression,
    rhs: &Expression,
    result_t: &Type,
    instructions: &mut Vec<IRInstruction>,
) -> ExpResult {
    let lhs_ty = get_type(lhs).to_owned();

    let eval_lhs = emit_tacky(lhs, instructions);
    let eval_rhs = emit_tacky_and_convert(rhs, instructions);

    let (dst, load_instr, store_instr) = match eval_lhs {
        ExpResult::PlainOperand(dst) => (dst.clone(), None, None),
        ExpResult::DereferencedPointer(ptr) => {
            let dst = make_tacky_variable(&lhs_ty);

            (
                dst.clone(),
                Some(IRInstruction::Load {
                    src_ptr: ptr.clone(),
                    dst: dst.clone(),
                }),
                Some(IRInstruction::Store {
                    src: dst.clone(),
                    dst_ptr: ptr.clone(),
                }),
            )
        }
        ExpResult::SubObject { base, offset } => {
            let dst = make_tacky_variable(&lhs_ty);

            (
                dst.clone(),
                Some(IRInstruction::CopyFromOffset {
                    src: base.clone(),
                    offset,
                    dst: dst.clone(),
                }),
                Some(IRInstruction::CopyToOffset {
                    src: dst.clone(),
                    dst: base.clone(),
                    offset,
                }),
            )
        }
    };

    fn get_cast_instruction(
        src: &IRValue,
        dst: &IRValue,
        src_t: &Type,
        dst_t: &Type,
    ) -> IRInstruction {
        make_cast_instruction(src, dst, src_t, dst_t)
    }


    let (result_var, cast_to, cast_from) = if &lhs_ty == result_t {
        (dst.clone(), None, None)
    } else {
        let tmp = make_tacky_variable(result_t);
        let cast_lhs_to_tmp = get_cast_instruction(&dst, &tmp, &lhs_ty, result_t);
        let cast_tmp_to_lhs = get_cast_instruction(&tmp, &dst, result_t, &lhs_ty);

        (tmp.clone(), Some(cast_lhs_to_tmp), Some(cast_tmp_to_lhs))
    };

    let do_operation = if is_pointer_type(result_t) {
        let scale = get_ptr_scale(result_t);

        match kind {
            BinaryExpressionKind::Add => {
                vec![IRInstruction::AddPtr {
                    ptr: result_var.clone(),
                    index: eval_rhs.clone(),
                    scale,
                    dst: result_var.clone(),
                }]
            }
            BinaryExpressionKind::Sub => {
                let negated_index = make_tacky_variable(&Type::Long);
                vec![
                    IRInstruction::Unary {
                        op: UnaryOp::Negate,
                        src: eval_rhs.clone(),
                        dst: negated_index.clone(),
                    },
                    IRInstruction::AddPtr {
                        ptr: result_var.clone(),
                        index: negated_index.clone(),
                        scale,
                        dst: result_var.clone(),
                    },
                ]
            }
            _ => unreachable!(),
        }
    } else {
        vec![IRInstruction::Binary {
            op: kind.into(),
            lhs: result_var.clone(),
            rhs: eval_rhs.clone(),
            dst: result_var.clone(),
        }]
    };

    instructions.extend(load_instr);
    instructions.extend(cast_to);
    instructions.extend(do_operation);
    instructions.extend(cast_from);
    instructions.extend(store_instr);

    ExpResult::PlainOperand(dst)
}

impl From<CompoundExpressionKind> for BinaryExpressionKind {
    fn from(value: CompoundExpressionKind) -> Self {
        match value {
            CompoundExpressionKind::Add => BinaryExpressionKind::Add,
            CompoundExpressionKind::Sub => BinaryExpressionKind::Sub,
            CompoundExpressionKind::Mul => BinaryExpressionKind::Mul,
            CompoundExpressionKind::Div => BinaryExpressionKind::Div,
            CompoundExpressionKind::Mod => BinaryExpressionKind::Rem,
            CompoundExpressionKind::BitwiseOr => BinaryExpressionKind::BitwiseOr,
            CompoundExpressionKind::BitwiseAnd => BinaryExpressionKind::BitwiseAnd,
            CompoundExpressionKind::BitwiseXor => BinaryExpressionKind::BitwiseXor,
            CompoundExpressionKind::BitwiseShl => BinaryExpressionKind::BitwiseShl,
            CompoundExpressionKind::BitwiseShr => BinaryExpressionKind::BitwiseShr,
        }
    }
}

fn emit_tacky(e: &Expression, instructions: &mut Vec<IRInstruction>) -> ExpResult {
    let t = get_type(e);
    match e {
        Expression::Compound(CompoundExpression {
            kind,
            lhs,
            rhs,
            result_t,
            ..
        }) => emit_compound_expression((*kind).into(), lhs, rhs, result_t, instructions),

        Expression::Constant(const_expr) => {
            ExpResult::PlainOperand(IRValue::Constant(const_expr.value))
        }

        Expression::Unary(UnaryExpression {
            kind: UnaryExpressionKind::Inc,
            expr,
            ty,
            ..
        }) => {
            let const_t = if is_pointer_type(ty) {
                Type::Long
            } else {
                ty.clone()
            };
            emit_compound_expression(
                BinaryExpressionKind::Add,
                expr,
                &make_constexpr(1, const_t),
                ty,
                instructions,
            )
        }

        Expression::Unary(UnaryExpression {
            kind: UnaryExpressionKind::Dec,
            expr,
            ty,
            ..
        }) => {
            let const_t = if is_pointer_type(ty) {
                Type::Long
            } else {
                ty.clone()
            };
            emit_compound_expression(
                BinaryExpressionKind::Sub,
                expr,
                &make_constexpr(1, const_t),
                ty,
                instructions,
            )
        }

        Expression::Unary(UnaryExpression { kind, expr, ty, .. }) => {
            let src = emit_tacky_and_convert(expr, instructions);
            let dst = make_tacky_variable(ty);
            let op = match kind {
                UnaryExpressionKind::Negate => UnaryOp::Negate,
                UnaryExpressionKind::Complement => UnaryOp::Complement,
                UnaryExpressionKind::Not => UnaryOp::Not,
                _ => unreachable!(),
            };

            instructions.push(IRInstruction::Unary {
                op,
                src: src.clone(),
                dst: dst.clone(),
            });

            ExpResult::PlainOperand(dst)
        }

        Expression::Postfix(PostfixExpression { expr, kind, ty, .. }) => {
            let dst = make_tacky_variable(ty);
            let lval = emit_tacky(expr, instructions);

            let do_op = |op, src: IRValue, dst: IRValue| {
                let index = match op {
                    PostfixExpressionKind::Inc => IRValue::Constant(make_const(1, Type::Long)),
                    PostfixExpressionKind::Dec => IRValue::Constant(make_const(-1, Type::Long)),
                };

                if is_pointer_type(get_type(expr)) {
                    IRInstruction::AddPtr {
                        ptr: src.clone(),
                        index: index.clone(),
                        scale: get_ptr_scale(get_type(expr)),
                        dst: dst.clone(),
                    }
                } else {
                    let one = IRValue::Constant(make_const(1, get_type(expr).to_owned()));
                    IRInstruction::Binary {
                        op: match op {
                            PostfixExpressionKind::Inc => BinaryOp::Add,
                            PostfixExpressionKind::Dec => BinaryOp::Sub,
                        },
                        lhs: src.clone(),
                        rhs: one,
                        dst: dst.clone(),
                    }
                }
            };

            let oper_instrs = match &lval {
                ExpResult::PlainOperand(IRValue::Var(var)) => {
                    vec![
                        IRInstruction::Copy {
                            src: IRValue::Var(var.clone()),
                            dst: dst.clone(),
                        },
                        do_op(
                            *kind,
                            IRValue::Var(var.to_owned()),
                            IRValue::Var(var.to_owned()),
                        ),
                    ]
                }

                ExpResult::DereferencedPointer(ptr) => {
                    let tmp = make_tacky_variable(ty);
                    vec![
                        IRInstruction::Load {
                            src_ptr: ptr.clone(),
                            dst: dst.clone(),
                        },
                        do_op(*kind, dst.clone(), tmp.clone()),
                        IRInstruction::Store {
                            src: tmp,
                            dst_ptr: ptr.clone(),
                        },
                    ]
                }

                ExpResult::SubObject { base, offset } => {
                    let tmp = make_tacky_variable(ty);
                    vec![
                        IRInstruction::CopyFromOffset {
                            src: base.clone(),
                            offset: *offset,
                            dst: tmp.clone(),
                        },
                        do_op(*kind, dst.clone(), tmp.clone()),
                        IRInstruction::CopyToOffset {
                            src: tmp,
                            dst: base.clone(),
                            offset: *offset,
                        },
                    ]
                }

                _ => panic!("Invalid lvalue in postfix increment/decrement"),
            };

            instructions.extend(oper_instrs);
            ExpResult::PlainOperand(dst)
        }

        Expression::Binary(BinaryExpression {
            kind, lhs, rhs, ty, ..
        }) => match kind {
            BinaryExpressionKind::And => {
                let tmp = make_temporary();

                let false_label = format!("And.{}.shortcircuit", tmp);
                let end_label = format!("And.{}.end", tmp);

                let result = make_tacky_variable(ty);

                let lhs = emit_tacky_and_convert(lhs, instructions);
                instructions.push(IRInstruction::JumpIfZero {
                    condition: lhs.clone(),
                    target: false_label.clone(),
                });

                let rhs = emit_tacky_and_convert(rhs, instructions);
                instructions.push(IRInstruction::JumpIfZero {
                    condition: rhs.clone(),
                    target: false_label.clone(),
                });

                instructions.push(IRInstruction::Copy {
                    src: IRValue::Constant(Const::Int(1)),
                    dst: result.clone(),
                });

                instructions.push(IRInstruction::Jump(end_label.clone()));

                instructions.push(IRInstruction::Label(false_label.clone()));

                instructions.push(IRInstruction::Copy {
                    src: IRValue::Constant(Const::Int(0)),
                    dst: result.clone(),
                });

                instructions.push(IRInstruction::Label(end_label.clone()));

                ExpResult::PlainOperand(result)
            }

            BinaryExpressionKind::Or => {
                let tmp = make_temporary();

                let true_label = format!("Or.{}.shortcircuit", tmp);
                let end_label = format!("Or.{}.end", tmp);

                let result = make_tacky_variable(ty);

                let lhs = emit_tacky_and_convert(lhs, instructions);
                instructions.push(IRInstruction::JumpIfNotZero {
                    condition: lhs.clone(),
                    target: true_label.clone(),
                });

                let rhs = emit_tacky_and_convert(rhs, instructions);
                instructions.push(IRInstruction::JumpIfNotZero {
                    condition: rhs.clone(),
                    target: true_label.clone(),
                });

                instructions.push(IRInstruction::Copy {
                    src: IRValue::Constant(Const::Int(0)),
                    dst: result.clone(),
                });

                instructions.push(IRInstruction::Jump(end_label.clone()));

                instructions.push(IRInstruction::Label(true_label.clone()));

                instructions.push(IRInstruction::Copy {
                    src: IRValue::Constant(Const::Int(1)),
                    dst: result.clone(),
                });

                instructions.push(IRInstruction::Label(end_label.clone()));

                ExpResult::PlainOperand(result)
            }

            BinaryExpressionKind::Add => emit_ptr_addition(lhs, rhs, t, instructions),

            BinaryExpressionKind::Sub => {
                let lhs_type = get_type(lhs);
                let rhs_type = get_type(rhs);

                if is_pointer_type(lhs_type) && is_integer_type(rhs_type) {
                    let ptr = emit_tacky_and_convert(lhs, instructions);
                    let index = emit_tacky_and_convert(rhs, instructions);
                    let scale = get_ptr_scale(lhs_type);
                    let dst = make_tacky_variable(lhs_type);

                    let negated_index = make_tacky_variable(rhs_type);

                    instructions.push(IRInstruction::Unary {
                        op: UnaryOp::Negate,
                        src: index.clone(),
                        dst: negated_index.clone(),
                    });

                    instructions.push(IRInstruction::AddPtr {
                        ptr,
                        index: negated_index,
                        scale,
                        dst: dst.clone(),
                    });

                    ExpResult::PlainOperand(dst)
                } else if is_pointer_type(lhs_type) && is_pointer_type(rhs_type) {
                    let ptr1 = emit_tacky_and_convert(lhs, instructions);
                    let ptr2 = emit_tacky_and_convert(rhs, instructions);

                    let ptr_diff: IRValue = make_tacky_variable(lhs_type);

                    let scale = get_ptr_scale(lhs_type);
                    let scale_var = IRValue::Constant(Const::Long(scale as i64));

                    let dst = make_tacky_variable(t);

                    instructions.push(IRInstruction::Binary {
                        op: BinaryOp::Sub,
                        lhs: ptr1,
                        rhs: ptr2,
                        dst: ptr_diff.clone(),
                    });

                    instructions.push(IRInstruction::Binary {
                        op: BinaryOp::Div,
                        lhs: ptr_diff,
                        rhs: scale_var,
                        dst: dst.clone(),
                    });

                    ExpResult::PlainOperand(dst)
                } else if is_pointer_type(lhs_type) && is_pointer_type(rhs_type) {
                    let ptr1 = emit_tacky_and_convert(lhs, instructions);
                    let ptr2 = emit_tacky_and_convert(rhs, instructions);

                    let ptr_diff = make_tacky_variable(&Type::Long);

                    let scale = get_ptr_scale(lhs_type);

                    let dst = make_tacky_variable(ty);

                    instructions.push(IRInstruction::Binary {
                        op: BinaryOp::Sub,
                        lhs: ptr1,
                        rhs: ptr2,
                        dst: ptr_diff.clone(),
                    });

                    instructions.push(IRInstruction::Binary {
                        op: BinaryOp::Div,
                        lhs: ptr_diff,
                        rhs: IRValue::Constant(Const::Long(scale as i64)),
                        dst: dst.clone(),
                    });

                    ExpResult::PlainOperand(dst)
                } else {
                    let lhs = emit_tacky_and_convert(lhs, instructions);
                    let rhs = emit_tacky_and_convert(rhs, instructions);

                    let dst = make_tacky_variable(t);

                    instructions.push(IRInstruction::Binary {
                        op: BinaryOp::Sub,
                        lhs,
                        rhs,
                        dst: dst.clone(),
                    });

                    ExpResult::PlainOperand(dst)
                }
            }

            _ => {
                let lhs = emit_tacky_and_convert(lhs, instructions);
                let rhs = emit_tacky_and_convert(rhs, instructions);

                let dst = make_tacky_variable(t);

                instructions.push(IRInstruction::Binary {
                    op: (*kind).into(),
                    lhs,
                    rhs,
                    dst: dst.clone(),
                });

                ExpResult::PlainOperand(dst)
            }
        },

        Expression::Variable(var) => ExpResult::PlainOperand(IRValue::Var(var.value.to_owned())),

        Expression::Assign(AssignExpression {
            op: _,
            lhs,
            rhs,
            ty: _,
            ..
        }) => emit_assignment(lhs, rhs, instructions),

        Expression::Conditional(ConditionalExpression {
            condition,
            then_expr,
            else_expr,
            ty,
            ..
        }) => {
            let tmp = make_temporary();

            let e2_label = format!("Cond.{}.else", tmp);
            let end_label = format!("Cond.{}.end", tmp);
            let condition = emit_tacky_and_convert(condition, instructions);

            instructions.push(IRInstruction::JumpIfZero {
                condition: condition.clone(),
                target: e2_label.clone(),
            });

            if get_type(e) == &Type::Void {
                emit_tacky_and_convert(then_expr, instructions);
                instructions.extend(vec![
                    IRInstruction::Jump(end_label.clone()),
                    IRInstruction::Label(e2_label.clone()),
                ]);
                emit_tacky_and_convert(else_expr, instructions);
                instructions.push(IRInstruction::Label(end_label));
                ExpResult::PlainOperand(IRValue::Var("DUMMY".to_owned()))
            } else {
                let result = make_tacky_variable(ty);

                let e1 = emit_tacky_and_convert(then_expr, instructions);

                instructions.push(IRInstruction::Copy {
                    src: e1,
                    dst: result.clone(),
                });

                instructions.push(IRInstruction::Jump(end_label.clone()));

                instructions.push(IRInstruction::Label(e2_label));

                let e2 = emit_tacky_and_convert(else_expr, instructions);

                instructions.push(IRInstruction::Copy {
                    src: e2,
                    dst: result.clone(),
                });

                instructions.push(IRInstruction::Label(end_label));

                ExpResult::PlainOperand(result)
            }
        }

        Expression::Call(CallExpression { name, args, ty, .. }) => {
            let result = if ty == &Type::Void {
                None
            } else {
                Some(make_tacky_variable(ty))
            };

            let mut arg_values = vec![];

            for arg in args {
                arg_values.push(emit_tacky_and_convert(arg, instructions));
            }

            instructions.push(IRInstruction::Call {
                target: name.to_owned(),
                args: arg_values,
                dst: result.clone(),
            });

            ExpResult::PlainOperand(result.unwrap_or(IRValue::Var("DUMMY".to_owned())))
        }

        Expression::Cast(CastExpression {
            target_type,
            expr,
            ty: _,
            ..
        }) => {
            let result = emit_tacky_and_convert(expr, instructions);
            let inner_type = get_type(expr);

            if target_type == inner_type || target_type == &Type::Void {
                return ExpResult::PlainOperand(result);
            }

            let dst = make_tacky_variable(target_type);
            instructions.push(make_cast_instruction(&result, &dst, inner_type, target_type));

            ExpResult::PlainOperand(dst)
        }

        Expression::Deref(DerefExpression { expr, ty: _, .. }) => {
            let ptr = emit_tacky_and_convert(expr, instructions);
            ExpResult::DereferencedPointer(ptr)
        }

        Expression::AddrOf(AddrOfExpression { expr, ty, .. }) => {
            let result = emit_tacky(expr, instructions);

            match result {
                ExpResult::PlainOperand(val) => {
                    let dst = make_tacky_variable(ty);
                    instructions.push(IRInstruction::GetAddress {
                        src: val,
                        dst: dst.clone(),
                    });
                    ExpResult::PlainOperand(dst)
                }
                ExpResult::DereferencedPointer(ptr) => ExpResult::PlainOperand(ptr),
                ExpResult::SubObject { base, offset } => {
                    let dst = make_tacky_variable(get_type(e));
                    instructions.push(IRInstruction::GetAddress {
                        src: IRValue::Var(base.clone()),
                        dst: dst.clone(),
                    });
                    instructions.push(IRInstruction::AddPtr {
                        ptr: dst.clone(),
                        index: IRValue::Constant(Const::Long(offset as i64)),
                        scale: 1,
                        dst: dst.clone(),
                    });
                    ExpResult::PlainOperand(dst)
                }
            }
        }

        Expression::Subscript(SubscriptExpression {
            expr, index, ty, ..
        }) => {
            let result = emit_ptr_addition(expr, index, ty, instructions);
            match result {
                ExpResult::PlainOperand(val) => ExpResult::DereferencedPointer(val),
                _ => unreachable!(),
            }
        }

        Expression::Literal(_) => todo!(),

        Expression::String(StringExpression { value, ty: _, .. }) => {
            let var_name = format!("str.{}", make_temporary());
            let t = Type::Array {
                element: Box::new(Type::Char),
                size: value.len() + 1,
            };
            let symbol = Symbol {
                attrs: IdentifierAttrs::ConstantAttr(StaticInit::String(value.to_owned(), true)),
                ty: t,
            };
            SYMBOL_TABLE
                .lock()
                .unwrap()
                .insert(var_name.clone(), symbol);

            ExpResult::PlainOperand(IRValue::Var(var_name))
        }

        Expression::Sizeof(SizeofExpression { expr, ty: _, .. }) => ExpResult::PlainOperand(
            IRValue::Constant(Const::ULong(get_size_of_type(get_type(expr)) as u64)),
        ),

        Expression::SizeofT(SizeofTExpression { t, ty: _, .. }) => {
            ExpResult::PlainOperand(IRValue::Constant(Const::ULong(get_size_of_type(t) as u64)))
        }

        Expression::Dot(DotExpression {
            structure,
            member,
            ty: _,
            ..
        }) => {
            let struct_tag = match get_type(structure) {
                Type::Struct { tag } => tag,
                _ => unreachable!(),
            };
            let struct_def = TYPE_TABLE.lock().unwrap().get(struct_tag).cloned().unwrap();
            let member_offset = struct_def
                .members
                .iter()
                .find(|m| m.name == *member)
                .unwrap()
                .offset;

            let inner_object = emit_tacky(structure, instructions);

            match inner_object {
                ExpResult::PlainOperand(IRValue::Var(v)) => ExpResult::SubObject {
                    base: v,
                    offset: member_offset,
                },
                ExpResult::SubObject { base, offset } => ExpResult::SubObject {
                    base,
                    offset: offset + member_offset,
                },
                ExpResult::DereferencedPointer(ptr) => {
                    let type_of_e = get_type(e);
                    let dst_ptr = make_tacky_variable(&Type::Pointer(type_of_e.to_owned().into()));
                    let instr = IRInstruction::AddPtr {
                        ptr,
                        index: IRValue::Constant(Const::Long(member_offset as i64)),
                        scale: 1,
                        dst: dst_ptr.clone(),
                    };
                    instructions.push(instr);
                    ExpResult::DereferencedPointer(dst_ptr)
                }
                _ => unreachable!(),
            }
        }

        Expression::Arrow(ArrowExpression {
            pointer,
            member,
            ty,
            ..
        }) => {
            let struct_tag = match get_type(pointer) {
                Type::Pointer(referenced) => match &**referenced {
                    Type::Struct { tag } => tag,
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            };
            let struct_def = TYPE_TABLE.lock().unwrap().get(struct_tag).cloned().unwrap();
            let member_offset = struct_def
                .members
                .iter()
                .find(|m| m.name == *member)
                .unwrap()
                .offset;

            let ptr = emit_tacky_and_convert(pointer, instructions);

            if member_offset == 0 {
                return ExpResult::DereferencedPointer(ptr);
            }

            let dst = make_tacky_variable(&Type::Pointer(ty.to_owned().into()));

            let index = IRValue::Constant(Const::Long(member_offset as i64));

            let add_ptr_instr = IRInstruction::AddPtr {
                ptr,
                index,
                scale: 1,
                dst: dst.clone(),
            };

            instructions.push(add_ptr_instr);

            ExpResult::DereferencedPointer(dst)
        }
    }
}

fn emit_assignment(
    lhs: &Expression,
    rhs: &Expression,
    instructions: &mut Vec<IRInstruction>,
) -> ExpResult {
    let lval = emit_tacky(lhs, instructions);
    let rval = emit_tacky_and_convert(rhs, instructions);

    match &lval {
        ExpResult::SubObject { base, offset } => {
            instructions.push(IRInstruction::CopyToOffset {
                src: rval.clone(),
                dst: base.to_owned(),
                offset: *offset,
            });
            ExpResult::PlainOperand(rval)
        }

        ExpResult::PlainOperand(val) => {
            instructions.push(IRInstruction::Copy {
                src: rval,
                dst: val.to_owned(),
            });
            lval
        }

        ExpResult::DereferencedPointer(ptr) => {
            instructions.push(IRInstruction::Store {
                src: rval.clone(),
                dst_ptr: ptr.to_owned(),
            });
            ExpResult::PlainOperand(rval)
        }
    }
}

fn emit_string_init(dst: String, offset: usize, s: &[u8]) -> Vec<IRInstruction> {
    let len = s.len();

    if len == 0 {
        vec![]
    } else if len >= 8 {
        let l = i64::from_le_bytes(s[0..8].try_into().unwrap());
        let instr = IRInstruction::CopyToOffset {
            src: IRValue::Constant(Const::Long(l)),
            dst: dst.clone(),
            offset,
        };
        let rest = &s[8..];
        let mut instructions = vec![instr];
        instructions.extend(emit_string_init(dst, offset + 8, rest));
        return instructions;
    } else if len >= 4 {
        let i = i32::from_le_bytes(s[0..4].try_into().unwrap());
        let instr = IRInstruction::CopyToOffset {
            src: IRValue::Constant(Const::Int(i)),
            dst: dst.clone(),
            offset,
        };
        let rest = &s[4..];
        let mut instructions = vec![instr];
        instructions.extend(emit_string_init(dst, offset + 4, rest));
        return instructions;
    } else {
        let c = s[0] as i8;
        let instr = IRInstruction::CopyToOffset {
            src: IRValue::Constant(Const::Char(c)),
            dst: dst.clone(),
            offset,
        };
        let rest = &s[1..];
        let mut instructions = vec![instr];
        instructions.extend(emit_string_init(dst, offset + 1, rest));
        return instructions;
    }
}

fn emit_compound_init(
    name: &str,
    value: &Initializer,
    instructions: &mut Vec<IRInstruction>,
    offset: usize,
    inited_type: &Type,
) {
    match value {
        Initializer::Single(_, Expression::String(string_expr)) => {
            if let Type::Array { element: _, size } = inited_type {
                let str_bytes = string_expr.value.as_bytes();
                let padding_sz = size.saturating_sub(str_bytes.len());

                let mut combined_bytes = vec![];
                combined_bytes.extend_from_slice(str_bytes);

                for _ in 0..padding_sz {
                    combined_bytes.push(0);
                }

                instructions.extend(emit_string_init(name.to_owned(), offset, &combined_bytes));
            }
        }

        Initializer::Single(_, single_init) => {
            let v = emit_tacky_and_convert(single_init, instructions);
            instructions.push(IRInstruction::CopyToOffset {
                src: v,
                dst: name.to_string(),
                offset,
            });
        }

        Initializer::Compound(_, ty, compound_init) => {
            if let Type::Array { element, size: _ } = inited_type {
                for (idx, elem_init) in compound_init.iter().enumerate() {
                    let new_offset = offset + idx * get_size_of_type(ty);
                    emit_compound_init(name, elem_init, instructions, new_offset, element);
                }
            } else if let Type::Struct { tag } = inited_type {
                let members = TYPE_TABLE.lock().unwrap().get(tag).unwrap().members.clone();

                for (member, mem_init) in members.iter().zip(compound_init) {
                    let mem_offset = offset + member.offset;
                    emit_compound_init(name, mem_init, instructions, mem_offset, &member.ty);
                }
            }
        }
    }
}

fn emit_ptr_addition(
    lhs: &Expression,
    rhs: &Expression,
    t: &Type,
    instructions: &mut Vec<IRInstruction>,
) -> ExpResult {
    let lhs_type = get_type(lhs);
    let rhs_type = get_type(rhs);

    if is_pointer_type(lhs_type) && is_integer_type(rhs_type) {
        let ptr = emit_tacky_and_convert(lhs, instructions);
        let index = emit_tacky_and_convert(rhs, instructions);
        let scale = get_ptr_scale(lhs_type);
        let dst = make_tacky_variable(lhs_type);
        instructions.push(IRInstruction::AddPtr {
            ptr,
            index,
            scale,
            dst: dst.clone(),
        });
        ExpResult::PlainOperand(dst)
    } else if is_integer_type(lhs_type) && is_pointer_type(rhs_type) {
        let ptr = emit_tacky_and_convert(rhs, instructions);
        let index = emit_tacky_and_convert(lhs, instructions);
        let scale = get_ptr_scale(rhs_type);
        let dst = make_tacky_variable(rhs_type);
        instructions.push(IRInstruction::AddPtr {
            ptr,
            index,
            scale,
            dst: dst.clone(),
        });
        ExpResult::PlainOperand(dst)
    } else {
        let lhs = emit_tacky_and_convert(lhs, instructions);
        let rhs = emit_tacky_and_convert(rhs, instructions);

        let dst = make_tacky_variable(t);

        instructions.push(IRInstruction::Binary {
            op: BinaryOp::Add,
            lhs,
            rhs,
            dst: dst.clone(),
        });

        ExpResult::PlainOperand(dst)
    }
}

pub fn convert_symbols_to_tacky() -> (Vec<IRStaticVariable>, Vec<IRStaticConstant>) {
    let mut static_variables = vec![];
    let mut static_constants = vec![];
    for (name, entry) in SYMBOL_TABLE.lock().unwrap().iter() {
        if let IdentifierAttrs::StaticAttr {
            initial_value,
            global,
        } = entry.attrs.clone()
        {
            match initial_value {
                InitialValue::Initial(init) => static_variables.push(IRStaticVariable {
                    name: name.clone(),
                    global,
                    init: init.clone(),
                    ty: entry.ty.clone(),
                }),
                InitialValue::Tentative => static_variables.push(IRStaticVariable {
                    name: name.clone(),
                    ty: entry.ty.clone(),
                    global,
                    init: vec![match &entry.ty {
                        Type::Char => StaticInit::Char(0),
                        Type::SChar => StaticInit::Char(0),
                        Type::UChar => StaticInit::UChar(0),
                        Type::Short => StaticInit::Short(0),
                        Type::UShort => StaticInit::UShort(0),
                        Type::Int => StaticInit::Int(0),
                        Type::Long => StaticInit::Long(0),
                        Type::ULong => StaticInit::ULong(0),
                        Type::UInt => StaticInit::UInt(0),
                        Type::Float => StaticInit::Float(0.0),
                        Type::Double => StaticInit::Double(0.0),
                        Type::Pointer(_) => StaticInit::ULong(0),
                        Type::Array { element, size } => {
                            StaticInit::Zero(get_size_of_type(element) * size)
                        }
                        Type::Struct { tag } => {
                            StaticInit::Zero(TYPE_TABLE.lock().unwrap().get(tag).unwrap().size)
                        }
                        _ => {
                            unimplemented!()
                        }
                    }],
                }),
                _ => continue,
            }
        }
        if let IdentifierAttrs::ConstantAttr(init) = entry.attrs.clone() {
            static_constants.push(IRStaticConstant {
                name: name.clone(),
                init,
                ty: entry.ty.clone(),
            })
        }
    }

    (static_variables, static_constants)
}

pub fn make_tacky_variable(t: &Type) -> IRValue {
    let var_name = format!("var.{}", make_temporary());
    let symbol = Symbol {
        attrs: IdentifierAttrs::LocalAttr,
        ty: t.to_owned(),
    };
    SYMBOL_TABLE
        .lock()
        .unwrap()
        .insert(var_name.clone(), symbol);
    IRValue::Var(var_name)
}

static TEMPORARY: AtomicUsize = AtomicUsize::new(0);

pub fn make_temporary() -> usize {
    TEMPORARY.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}

fn get_ptr_scale(t: &Type) -> usize {
    match t {
        Type::Pointer(referenced) => get_size_of_type(referenced),
        _ => unimplemented!(),
    }
}

impl From<IRNode> for IRProgram {
    fn from(node: IRNode) -> Self {
        match node {
            IRNode::Program(prog) => prog,
            _ => unreachable!(),
        }
    }
}

impl From<IRNode> for IRFunction {
    fn from(node: IRNode) -> Self {
        match node {
            IRNode::Function(func) => func,
            _ => unreachable!(),
        }
    }
}

impl From<IRNode> for Vec<IRInstruction> {
    fn from(node: IRNode) -> Self {
        match node {
            IRNode::Instructions(instrs) => instrs,
            IRNode::StaticVariable(_) => vec![],
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Optimization {
    ConstantFolding,
    UnreachableCodeElimination,
    CopyPropagation,
    DeadStoreElimination,
}

pub trait Optimize {
    fn optimize(&mut self, enabled_optimizations: Vec<Optimization>) -> Self;
}

impl Optimize for IRProgram {
    fn optimize(&mut self, enabled_optimizations: Vec<Optimization>) -> Self {
        let mut functions = vec![];

        for func in &mut self.functions {
            functions.push(func.optimize(enabled_optimizations.clone()));
        }

        IRProgram {
            functions,
            static_vars: self.static_vars.clone(),
            static_constants: self.static_constants.clone(),
        }
    }
}

impl Optimize for IRFunction {
    fn optimize(&mut self, enabled_optimizations: Vec<Optimization>) -> Self {
        use crate::optimizer::{
            constant_folding::constant_folding, copy_propagation::copy_propagation,
            dead_store_elimination::dead_store_elimination,
            unreachable_code_elimination::unreachable_code_elimination,
        };

        if self.body.is_empty() {
            return self.clone();
        }

        loop {
            let aliased_vars = address_taken_analysis(&self.body);

            let post_constant_folding =
                if enabled_optimizations.contains(&Optimization::ConstantFolding) {
                    constant_folding(&self.body)
                } else {
                    self.body.clone()
                };

            let mut cfg: cfg::CFG<(), IRInstruction> =
                cfg::CFG::<(), IRInstruction>::instructions_to_cfg(
                    "spam".to_string(),
                    post_constant_folding,
                );

            if enabled_optimizations.contains(&Optimization::UnreachableCodeElimination) {
                cfg = unreachable_code_elimination(&mut cfg).to_owned();
            }

            if enabled_optimizations.contains(&Optimization::CopyPropagation) {
                cfg = copy_propagation(&aliased_vars, cfg);
            }

            if enabled_optimizations.contains(&Optimization::DeadStoreElimination) {
                cfg = dead_store_elimination(&aliased_vars, cfg);
            }

            let optimized_function_body = cfg.cfg_to_instructions();

            if optimized_function_body == self.body || optimized_function_body.is_empty() {
                return IRFunction {
                    name: self.name.clone(),
                    params: self.params.clone(),
                    body: optimized_function_body,
                    global: self.global,
                };
            }

            self.body = optimized_function_body;
        }
    }
}

fn address_taken_analysis(instrs: &[IRInstruction]) -> std::collections::BTreeSet<String> {
    let addr_taken = |instr: &IRInstruction| -> Option<String> {
        match instr {
            IRInstruction::GetAddress { src, dst: _ } => {
                if let IRValue::Var(v) = src {
                    Some(v.clone())
                } else {
                    None
                }
            }
            _ => None,
        }
    };

    instrs.iter().filter_map(addr_taken).collect()
}

impl Instr for IRInstruction {
    fn simplify(&self) -> SimpleInstr {
        match self {
            IRInstruction::Label(lbl) => SimpleInstr::Label(lbl.clone()),
            IRInstruction::JumpIfZero { target, .. }
            | IRInstruction::JumpIfNotZero { target, .. } => {
                SimpleInstr::ConditionalJump(target.clone())
            }
            IRInstruction::Jump(target) => SimpleInstr::UnconditionalJump(target.clone()),
            IRInstruction::Ret(_) => SimpleInstr::Return,
            _ => SimpleInstr::Other,
        }
    }

    fn pp_instr(&self) -> String {
        match self {
            IRInstruction::Label(lbl) => format!("Label({})", lbl),
            IRInstruction::JumpIfZero { target, .. }
            | IRInstruction::JumpIfNotZero { target, .. } => format!("ConditionalJump({})", target),
            IRInstruction::Jump(target) => format!("UnconditionalJump({})", target),
            IRInstruction::Ret(val) => format!("Return {:?}", val),
            _ => format!("{:?}", self),
        }
    }

    fn is_jump(&self) -> bool {
        matches!(
            self,
            IRInstruction::Jump(_)
                | IRInstruction::JumpIfZero { .. }
                | IRInstruction::JumpIfNotZero { .. }
        )
    }

    fn is_label(&self) -> bool {
        matches!(self, IRInstruction::Label(_))
    }
}

impl ToString for IRValue {
    fn to_string(&self) -> String {
        match self {
            IRValue::Constant(c) => c.to_string(),
            IRValue::Var(v) => v.to_string(),
        }
    }
}

pub fn get_dst(instr: &IRInstruction) -> Option<IRValue> {
    match instr {
        IRInstruction::Copy { dst, .. } => Some(dst.clone()),
        IRInstruction::Unary { dst, .. } => Some(dst.clone()),
        IRInstruction::Binary { dst, .. } => Some(dst.clone()),
        IRInstruction::Call { dst, .. } => dst.clone(),
        IRInstruction::SignExtend { dst, .. } => Some(dst.clone()),
        IRInstruction::ZeroExtend { dst, .. } => Some(dst.clone()),
        IRInstruction::Truncate { dst, .. } => Some(dst.clone()),
        IRInstruction::IntToDouble { dst, .. } => Some(dst.clone()),
        IRInstruction::DoubleToInt { dst, .. } => Some(dst.clone()),
        IRInstruction::DoubletoUInt { dst, .. } => Some(dst.clone()),
        IRInstruction::UIntToDouble { dst, .. } => Some(dst.clone()),
        IRInstruction::DoubleToFloat { dst, .. } => Some(dst.clone()),
        IRInstruction::FloatToDouble { dst, .. } => Some(dst.clone()),
        IRInstruction::FloatToInt { dst, .. } => Some(dst.clone()),
        IRInstruction::IntToFloat { dst, .. } => Some(dst.clone()),
        IRInstruction::FloattoUInt { dst, .. } => Some(dst.clone()),
        IRInstruction::UIntToFloat { dst, .. } => Some(dst.clone()),
        IRInstruction::CopyFromOffset { dst, .. } => Some(dst.clone()),
        IRInstruction::CopyToOffset { dst, .. } => Some(IRValue::Var(dst.to_owned())),
        IRInstruction::GetAddress { dst, .. } => Some(dst.clone()),
        IRInstruction::Load { dst, .. } => Some(dst.clone()),
        IRInstruction::AddPtr { dst, .. } => Some(dst.clone()),
        IRInstruction::Store { .. } => None,
        _ => None,
    }
}
