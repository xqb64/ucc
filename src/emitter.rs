use crate::codegen::AsmFunction;
use crate::codegen::AsmInstruction;
use crate::codegen::AsmNode;
use crate::codegen::AsmOperand;
use crate::codegen::AsmProgram;
use crate::codegen::AsmRegister;
use crate::codegen::AsmUnaryOp;
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
                AsmInstruction::AllocateStack(offset_manager.offset.unsigned_abs() as usize + 4);
        }

        writeln!(f, ".globl {}", self.name)?;
        writeln!(f, "{}:", self.name)?;

        writeln!(f, "  pushq %rbp")?;
        writeln!(f, "  movq %rsp, %rbp")?;

        self.instructions.emit(f)?;

        Ok(())
    }
}

impl Emit for AsmInstruction {
    fn emit(&mut self, f: &mut File) -> Result<()> {
        write!(f, "  ")?;

        match self {
            AsmInstruction::Mov { src, dst } => {
                write!(f, "movl ")?;
                src.emit(f)?;
                write!(f, ", ")?;
                dst.emit(f)?;
            }
            AsmInstruction::Ret => {
                writeln!(f, "movq %rbp, %rsp")?;
                writeln!(f, "popq %rbp")?;
                write!(f, "ret")?;
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
        }

        writeln!(f)?;

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
            AsmRegister::R10 => write!(f, "%r10d")?,
        }
        Ok(())
    }
}
