use crate::parser::{
    BinaryExpression, BinaryExpressionKind, Expression, FunctionDeclaration, ProgramStatement, Statement, UnaryExpression, UnaryExpressionKind
};

#[derive(Debug, Clone, PartialEq)]
pub struct IRProgram {
    pub functions: Vec<IRFunction>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IRFunction {
    pub name: String,
    pub body: Vec<IRInstruction>,
}

#[derive(Debug, Clone, PartialEq)]
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
    Ret(IRValue),
}

#[derive(Debug, Clone, PartialEq)]
pub enum IRValue {
    Constant(i32),
    Var(String),
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum UnaryOp {
    Negate,
    Complement,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

fn emit_tacky(e: Expression, instructions: &mut Vec<IRInstruction>) -> IRValue {
    match e {
        Expression::Constant(konst) => IRValue::Constant(konst),
        Expression::Unary(UnaryExpression { kind, expr }) => {
            let src = emit_tacky(*expr, instructions);
            let dst_name = format!("var.{}", make_temporary());
            let dst = IRValue::Var(dst_name.clone());
            let op = match kind {
                UnaryExpressionKind::Negate => UnaryOp::Negate,
                UnaryExpressionKind::Complement => UnaryOp::Complement,
            };
            instructions.push(IRInstruction::Unary {
                op,
                src,
                dst: dst.clone(),
            });

            dst
        }
        Expression::Binary(BinaryExpression { kind, lhs, rhs }) => {
            let lhs = emit_tacky(*lhs, instructions);
            let rhs = emit_tacky(*rhs, instructions);
            let dst_name = format!("var.{}", make_temporary());
            let dst = IRValue::Var(dst_name.clone());
            let op = match kind {
                BinaryExpressionKind::Add => BinaryOp::Add,
                BinaryExpressionKind::Sub => BinaryOp::Sub,
                BinaryExpressionKind::Mul => BinaryOp::Mul,
                BinaryExpressionKind::Div => BinaryOp::Div,
                BinaryExpressionKind::Rem => BinaryOp::Mod,
            };
            instructions.push(IRInstruction::Binary { op, lhs, rhs, dst: dst.clone() });

            dst
        }
    }
}

fn make_temporary() -> usize {
    static mut TEMPORARY: usize = 0;
    unsafe {
        let temporary = TEMPORARY;
        TEMPORARY += 1;
        temporary
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IRNode {
    Program(IRProgram),
    Function(IRFunction),
    Instructions(Vec<IRInstruction>),
    Value(IRValue),
}

pub trait Irfy {
    fn irfy(&self) -> IRNode;
}

impl Irfy for Statement {
    fn irfy(&self) -> IRNode {
        match self {
            Statement::Program(prog) => prog.irfy(),
            Statement::Function(func) => func.irfy(),
            Statement::Return(expr) => {
                let mut instructions = vec![];

                let expr = emit_tacky(expr.clone(), &mut instructions);
                instructions.push(IRInstruction::Ret(expr));

                IRNode::Instructions(instructions)
            }
        }
    }
}

impl Irfy for ProgramStatement {
    fn irfy(&self) -> IRNode {
        let mut functions = vec![];

        for func in &self.stmts {
            functions.push(func.irfy().into());
        }

        IRNode::Program(IRProgram { functions })
    }
}

impl Irfy for FunctionDeclaration {
    fn irfy(&self) -> IRNode {
        let mut instructions = vec![];

        for stmt in &self.stmts {
            instructions.extend::<Vec<IRInstruction>>(stmt.irfy().into());
        }

        IRNode::Function(IRFunction {
            name: self.name.clone(),
            body: instructions,
        })
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
            _ => unreachable!(),
        }
    }
}
