use crate::codegen::AsmBinaryOp;
use crate::codegen::AsmFunction;
use crate::codegen::AsmInstruction;
use crate::codegen::AsmNode;
use crate::codegen::AsmOperand;
use crate::codegen::AsmProgram;
use crate::codegen::AsmRegister;
use crate::codegen::AsmUnaryOp;
use crate::codegen::ConditionCode;
use crate::codegen::OFFSET_MANAGER;
use anyhow::Result;
use std::fs::File;
use std::io::Write;

pub trait Emit {
    fn emit(&mut self, f: &mut File) -> Result<()>;
}

impl Emit for AsmNode {
    fn emit(&mut self, f: &mut File) -> Result<()> {
        match self {
            AsmNode::Program(prog) => prog.emit(f),
            AsmNode::Function(func) => func.emit(f),
            AsmNode::Operand(operand) => operand.emit(f),
            AsmNode::Instructions(instrs) => instrs.emit(f),
        }
    }
}

impl Emit for AsmProgram {
    fn emit(&mut self, f: &mut File) -> Result<()> {
        for func in self.functions.iter_mut() {
            func.emit(f)?;
        }

        writeln!(f)?;

        Ok(())
    }
}

impl Emit for Vec<AsmInstruction> {
    fn emit(&mut self, f: &mut File) -> Result<()> {
        for instr in self.iter_mut() {
            instr.emit(f)?;
        }

        Ok(())
    }
}

impl Emit for AsmFunction {
    fn emit(&mut self, f: &mut File) -> Result<()> {
        if let Some(instr) = self.instructions.get_mut(0) {
            let offset_manager = OFFSET_MANAGER.lock().unwrap();
            *instr =
                AsmInstruction::AllocateStack(offset_manager.offset.unsigned_abs() as usize);
        }

        writeln!(f, "\t.globl {}", self.name)?;
        writeln!(f, "{}:", self.name)?;

        writeln!(f, "\tpushq %rbp")?;
        writeln!(f, "\tmovq %rsp, %rbp")?;

        self.instructions.emit(f)?;

        Ok(())
    }
}

impl Emit for AsmInstruction {
    fn emit(&mut self, f: &mut File) -> Result<()> {
        if let AsmInstruction::Label(_) = self {
        } else {
            write!(f, "\t")?;
        }

        match self {
            AsmInstruction::Mov { src, dst } => {
                write!(f, "movl ")?;
                src.emit(f)?;
                write!(f, ", ")?;
                dst.emit(f)?;
                writeln!(f)?;
            }
            AsmInstruction::Ret => {
                writeln!(f, "movq %rbp, %rsp")?;
                writeln!(f, "\tpopq %rbp")?;
                write!(f, "\tret")?;
            }
            AsmInstruction::Unary { op, operand } => {
                match op {
                    AsmUnaryOp::Neg => write!(f, "negl ")?,
                    AsmUnaryOp::Not => write!(f, "notl ")?,
                }
                operand.emit(f)?;
                writeln!(f)?;
            }
            AsmInstruction::AllocateStack(n) => {
                writeln!(f, "sub ${}, %rsp", n)?;
            }
            AsmInstruction::Cdq => {
                writeln!(f, "cdq")?;
            }
            AsmInstruction::Binary { op, lhs, rhs } => {
                let instr = match op {
                    AsmBinaryOp::Add => "addl",
                    AsmBinaryOp::Sub => "subl",
                    AsmBinaryOp::Mul => "imull",
                };

                write!(f, "{} ", instr)?;

                lhs.emit(f)?;
                write!(f, ", ")?;
                rhs.emit(f)?;

                writeln!(f)?;
            }
            AsmInstruction::Idiv(operand) => {
                write!(f, "idivl ")?;
                operand.emit(f)?;
                writeln!(f)?;
            }
            AsmInstruction::Imul { src, dst } => {
                write!(f, "imull ")?;
                src.emit(f)?;
                write!(f, ", ")?;
                dst.emit(f)?;
                writeln!(f)?;
            }
            AsmInstruction::Cmp { lhs, rhs } => {
                write!(f, "cmpl ")?;
                lhs.emit(f)?;
                write!(f, ", ")?;
                rhs.emit(f)?;
                writeln!(f)?;
            }
            AsmInstruction::Jmp { target } => {
                write!(f, "jmp ")?;
                writeln!(f, ".L{}", target)?;
            }
            AsmInstruction::JmpCC { condition, target } => {
                let suffix = match condition {
                    ConditionCode::E => "e",
                    ConditionCode::NE => "ne",
                    ConditionCode::L => "l",
                    ConditionCode::LE => "le",
                    ConditionCode::G => "g",
                    ConditionCode::GE => "ge", 
                };

                write!(f, "j{} ", suffix)?;
                writeln!(f, ".L{}", target)?;
            }
            AsmInstruction::SetCC { condition, operand } => {
                let suffix = match condition {
                    ConditionCode::E => "e",
                    ConditionCode::NE => "ne",
                    ConditionCode::L => "l",
                    ConditionCode::LE => "le",
                    ConditionCode::G => "g",
                    ConditionCode::GE => "ge", 
                };

                write!(f, "set{} ", suffix)?;
                operand.emit(f)?;
                writeln!(f)?;
            }
            AsmInstruction::Label(label) => {
                writeln!(f, ".L{}:", label)?;
            }
        }

        Ok(())
    }
}

impl Emit for AsmOperand {
    fn emit(&mut self, f: &mut File) -> Result<()> {
        match self {
            AsmOperand::Imm(n) => write!(f, "${}", n)?,
            AsmOperand::Register(reg) => reg.emit(f)?,
            AsmOperand::Stack(n) => write!(f, "{}(%rbp)", n)?,
            AsmOperand::Pseudo(_) => unreachable!(),
        }
        Ok(())
    }
}

impl Emit for AsmRegister {
    fn emit(&mut self, f: &mut File) -> Result<()> {
        match self {
            AsmRegister::AX => write!(f, "%eax")?,
            AsmRegister::DX => write!(f, "%edx")?,
            AsmRegister::R10 => write!(f, "%r10d")?,
            AsmRegister::R11 => write!(f, "%r11d")?,
        }
        Ok(())
    }
}
