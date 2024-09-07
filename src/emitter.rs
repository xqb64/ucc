use crate::codegen::AsmInstruction;
use crate::codegen::OFFSET_MANAGER;
use anyhow::Result;
use std::fs::File;
use std::io::Write;
use crate::codegen::AsmProgram;
use crate::codegen::AsmOperand;
use crate::codegen::Register;
use crate::codegen::AsmUnaryOp;


pub trait Emit {
    fn emit(&mut self, f: &mut File) -> Result<()>;
}

impl Emit for AsmProgram {
    fn emit(&mut self, f: &mut File) -> Result<()> {
        for func in self.functions.iter_mut() {
            if let Some(instr) = func.instructions.get_mut(0) {
                let offset_manager = OFFSET_MANAGER.lock().unwrap();
                *instr = AsmInstruction::AllocateStack(offset_manager.offset.unsigned_abs() as usize + 4);
            }

            writeln!(f, ".globl {}", func.name)?;
            writeln!(f, "{}:", func.name)?;

            writeln!(f, "  pushq %rbp")?;
            writeln!(f, "  movq %rsp, %rbp")?;

            for instr in &mut func.instructions {
                instr.emit(f)?;
            }
        }

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

impl Emit for Register {
    fn emit(&mut self, f: &mut File) -> Result<()> {
        match self {
            Register::AX => write!(f, "%eax")?,
            Register::R10 => write!(f, "%r10d")?,
        }
        Ok(())
    }
}
