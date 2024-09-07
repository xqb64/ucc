use crate::parser::{Expression, UnaryExpression, UnaryExpressoinKind, Statement, ProgramStatement, FunctionDeclaration};

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
    Ret(IRValue),
}

#[derive(Debug, Clone, PartialEq)]
pub enum IRValue {
    Constant(i32),
    Var(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Negate,
    Complement,
}

fn emit_tacky(e: Expression, instructions: &mut Vec<IRInstruction>) -> IRValue {
    match e {
        Expression::Constant(konst) => IRValue::Constant(konst),
        Expression::Unary(UnaryExpression { kind, expr }) => {
            let src = emit_tacky(*expr, instructions);
            let dst_name = format!("var.{}", make_temporary());
            let dst = IRValue::Var(dst_name.clone());
            let op = match kind {
                UnaryExpressoinKind::Negate => UnaryOp::Negate,
                UnaryExpressoinKind::Complement => UnaryOp::Complement,
            };
            instructions.push(IRInstruction::Unary {
                op,
                src,
                dst: dst.clone(),
            });

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
            functions.push(match func.irfy() {
                IRNode::Function(func) => func,
                _ => unreachable!(),
            });
        }
        IRNode::Program(IRProgram { functions })
    }
}

impl Irfy for FunctionDeclaration {
    fn irfy(&self) -> IRNode {
        let mut instructions = vec![];
        for stmt in &self.stmts {
            instructions.extend(match stmt.irfy() {
                IRNode::Instructions(instrs) => instrs,
                _ => unreachable!(),
            });
        }
        IRNode::Function(IRFunction {
            name: self.name.clone(),
            body: instructions,
        })
    }
}