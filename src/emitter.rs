use crate::codegen::AsmBinaryOp;
use crate::codegen::AsmFunction;
use crate::codegen::AsmInstruction;
use crate::codegen::AsmNode;
use crate::codegen::AsmOperand;
use crate::codegen::AsmProgram;
use crate::codegen::AsmRegister;
use crate::codegen::AsmStaticConstant;
use crate::codegen::AsmStaticVariable;
use crate::codegen::AsmSymtabEntry;
use crate::codegen::AsmType;
use crate::codegen::AsmUnaryOp;
use crate::codegen::ConditionCode;
use crate::codegen::ASM_SYMBOL_TABLE;
use crate::typechecker::StaticInit;
use anyhow::Result;
use std::fs::File;
use std::io::Write;

pub trait Emit {
    fn emit(&mut self, f: &mut File, asm_type: &mut AsmType) -> Result<()>;
}

impl Emit for AsmNode {
    fn emit(&mut self, f: &mut File, asm_type: &mut AsmType) -> Result<()> {
        match self {
            AsmNode::Program(prog) => prog.emit(f, asm_type),
            AsmNode::Function(func) => func.emit(f, asm_type),
            AsmNode::Operand(operand) => operand.emit(f, asm_type),
            AsmNode::Instructions(instrs) => instrs.emit(f, asm_type),
            AsmNode::StaticVariable(static_var) => static_var.emit(f, asm_type),
            AsmNode::StaticConstant(static_const) => static_const.emit(f, asm_type),
        }
    }
}

impl Emit for AsmProgram {
    fn emit(&mut self, f: &mut File, asm_type: &mut AsmType) -> Result<()> {
        for func in self.functions.iter_mut() {
            func.emit(f, asm_type)?;
        }

        for static_var in self.static_vars.iter_mut() {
            static_var.emit(f, asm_type)?;
        }

        for static_const in self.static_constants.iter_mut() {
            static_const.emit(f, asm_type)?;
        }

        writeln!(f, ".section	.note.GNU-stack,\"\",@progbits")?;

        writeln!(f)?;

        Ok(())
    }
}

impl Emit for Vec<AsmInstruction> {
    fn emit(&mut self, f: &mut File, asm_type: &mut AsmType) -> Result<()> {
        for instr in self.iter_mut() {
            instr.emit(f, asm_type)?;
        }

        Ok(())
    }
}

impl Emit for AsmStaticVariable {
    fn emit(&mut self, f: &mut File, _asm_type: &mut AsmType) -> Result<()> {
        writeln!(f)?;
        
        match self.init {
            StaticInit::Int(n) => match n {
                0 => writeln!(f, "\t.section .bss")?,
                _ => writeln!(f, "\t.section .data")?,
            },
            StaticInit::Long(n) => match n {
                0 => writeln!(f, "\t.section .bss")?,
                _ => writeln!(f, "\t.section .data")?,
            },
            StaticInit::Uint(n) => match n {
                0 => writeln!(f, "\t.section .bss")?,
                _ => writeln!(f, "\t.section .data")?,
            },
            StaticInit::Ulong(n) => match n {
                0 => writeln!(f, "\t.section .bss")?,
                _ => writeln!(f, "\t.section .data")?,
            },
            StaticInit::Double(n) => match n {
                0.0 => writeln!(f, "\t.section .bss")?,
                _ => writeln!(f, "\t.section .data")?,
            },
        }

        if self.global {
            writeln!(f, "\t.globl {}", self.name)?;
        }

        match self.alignment {
            1 => writeln!(f, "\t.balign 1")?,
            2 => writeln!(f, "\t.balign 2")?,
            4 => writeln!(f, "\t.balign 4")?,
            8 => writeln!(f, "\t.balign 8")?,
            16 => writeln!(f, "\t.balign 16")?,
            _ => unreachable!(),
        }

        writeln!(f, "{}:", self.name)?;

        match self.init {
            StaticInit::Int(n) => match n {
                0 => writeln!(f, "\t.zero 4")?,
                _ => writeln!(f, "\t.long {}", n)?,
            },
            StaticInit::Long(n) => match n {
                0 => writeln!(f, "\t.zero 8")?,
                _ => writeln!(f, "\t.quad {}", n)?,
            },
            StaticInit::Uint(n) => match n {
                0 => writeln!(f, "\t.zero 4")?,
                _ => writeln!(f, "\t.long {}", n)?,
            },
            StaticInit::Ulong(n) => match n {
                0 => writeln!(f, "\t.zero 8")?,
                _ => writeln!(f, "\t.quad {}", n)?,
            },
            StaticInit::Double(n) => match n {
                _ => writeln!(f, "\t.quad {}", n.to_bits())?,
            },
        }

        Ok(())
    }
}

impl Emit for AsmStaticConstant {
    fn emit(&mut self, f: &mut File, asm_type: &mut AsmType) -> Result<()> {
        writeln!(f)?;
        
        writeln!(f, "\t.section .rodata")?;
        writeln!(f, "\t.balign {}", self.alignment)?;
        writeln!(f, "{}:", self.name)?;

        match self.init {
            StaticInit::Int(n) => writeln!(f, "\t.long {}", n)?,
            StaticInit::Long(n) => writeln!(f, "\t.quad {}", n)?,
            StaticInit::Uint(n) => writeln!(f, "\t.long {}", n)?,
            StaticInit::Ulong(n) => writeln!(f, "\t.quad {}", n)?,
            StaticInit::Double(n) => writeln!(f, "\t.quad {}", n.to_bits())?,
        }

        Ok(())
    }
}

impl Emit for AsmFunction {
    fn emit(&mut self, f: &mut File, asm_type: &mut AsmType) -> Result<()> {
        writeln!(f, ".section .text")?;

        if let Some(instr) = self.instructions.get_mut(0) {
            *instr = AsmInstruction::AllocateStack(self.stack_space);
        } else {
            self.instructions.remove(0);
        }

        if self.global {
            writeln!(f, "\t.globl {}", self.name)?;
        }

        writeln!(f, "{}:", self.name)?;

        writeln!(f, "\tpushq %rbp")?;
        writeln!(f, "\tmovq %rsp, %rbp")?;

        self.instructions.emit(f, asm_type)?;

        Ok(())
    }
}

impl Emit for AsmInstruction {
    fn emit(&mut self, f: &mut File, asm_type: &mut AsmType) -> Result<()> {
        if let AsmInstruction::Label(_) = self {
        } else {
            write!(f, "\t")?;
        }

        match self {
            AsmInstruction::Mov { src, dst, asm_type } => {
                let suffix = match asm_type {
                    AsmType::Longword => "l",
                    AsmType::Quadword => "q",
                    AsmType::Double => "sd",
                };
                write!(f, "mov{} ", suffix)?;
                src.emit(f, asm_type)?;
                write!(f, ", ")?;
                dst.emit(f, asm_type)?;
                writeln!(f)?;
            }
            AsmInstruction::Movsx { src, dst } => {
                write!(f, "movslq ")?;
                src.emit(f, &mut AsmType::Longword)?;
                write!(f, ", ")?;
                dst.emit(f, &mut AsmType::Quadword)?;
                writeln!(f)?;
            }
            AsmInstruction::Ret => {
                writeln!(f, "movq %rbp, %rsp")?;
                writeln!(f, "\tpopq %rbp")?;
                write!(f, "\tret")?;
                writeln!(f)?;
            }
            AsmInstruction::Unary {
                op,
                operand,
                asm_type,
            } => {
                let suffix = match asm_type {
                    AsmType::Longword => "l",
                    AsmType::Quadword => "q",
                    _ => todo!(),
                };

                match op {
                    AsmUnaryOp::Neg => write!(f, "neg{} ", suffix)?,
                    AsmUnaryOp::Not => write!(f, "not{} ", suffix)?,
                    AsmUnaryOp::Shr => write!(f, "shr{} ", suffix)?,
                    _ => todo!(),
                }
                operand.emit(f, asm_type)?;
                writeln!(f)?;
            }
            AsmInstruction::AllocateStack(n) => {
                writeln!(f, "subq ${}, %rsp", n)?;
            }
            AsmInstruction::Cdq { asm_type } => match asm_type {
                AsmType::Longword => writeln!(f, "cdq")?,
                AsmType::Quadword => writeln!(f, "cqo")?,
                _ => todo!(),
            },
            AsmInstruction::Binary {
                op,
                lhs,
                rhs,
                asm_type,
            } => {
                let instr = match op {
                    AsmBinaryOp::Add => "add",
                    AsmBinaryOp::Sub => "sub",
                    AsmBinaryOp::Mul => match asm_type {
                        AsmType::Longword => "imul",
                        AsmType::Quadword => "imul",
                        AsmType::Double => "mul",
                    },
                    AsmBinaryOp::Xor => "xor",
                    AsmBinaryOp::And => "and",
                    AsmBinaryOp::Or => "or",
                    AsmBinaryOp::DivDouble => "div",
                };

                let suffix = match asm_type {
                    AsmType::Longword => "l",
                    AsmType::Quadword => "q",
                    AsmType::Double => match op {
                        AsmBinaryOp::Xor => "pd",
                        AsmBinaryOp::DivDouble => "sd",
                        _ => "sd",
                    }
                };

                write!(f, "{}{} ", instr, suffix)?;

                lhs.emit(f, asm_type)?;
                write!(f, ", ")?;
                rhs.emit(f, asm_type)?;

                writeln!(f)?;
            }
            AsmInstruction::Idiv { operand, asm_type } => {
                let suffix = match asm_type {
                    AsmType::Longword => "l",
                    AsmType::Quadword => "q",
                    _ => todo!(),
                };
                write!(f, "idiv{} ", suffix)?;
                operand.emit(f, asm_type)?;
                writeln!(f)?;
            }
            AsmInstruction::Imul { src, dst, asm_type } => {
                let instr = match asm_type {
                    AsmType::Longword => "imul",
                    AsmType::Quadword => "imul",
                    AsmType::Double => "mul",
                };
                
                let suffix = match asm_type {
                    AsmType::Longword => "l",
                    AsmType::Quadword => "q",
                    AsmType::Double => "sd",
                    _ => todo!(),
                };

                write!(f, "{}{} ", instr, suffix)?;
                src.emit(f, asm_type)?;
                write!(f, ", ")?;
                dst.emit(f, asm_type)?;
                writeln!(f)?;
            }
            AsmInstruction::Cmp { lhs, rhs, asm_type } => {
                let instr = match asm_type {
                    AsmType::Longword => "cmpl",
                    AsmType::Quadword => "cmpq",
                    AsmType::Double => "comisd",
                };
                write!(f, "{} ", instr)?;
                lhs.emit(f, asm_type)?;
                write!(f, ", ")?;
                rhs.emit(f, asm_type)?;
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
                    ConditionCode::A => "a",
                    ConditionCode::AE => "ae",
                    ConditionCode::B => "b",
                    ConditionCode::BE => "be",
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
                    ConditionCode::A => "a",
                    ConditionCode::AE => "ae",
                    ConditionCode::B => "b",
                    ConditionCode::BE => "be",
                };

                write!(f, "set{} ", suffix)?;

                operand.emit(f, asm_type)?;

                writeln!(f)?;
            }
            AsmInstruction::Label(label) => {
                writeln!(f, ".L{}:", label)?;
            }
            AsmInstruction::DeallocateStack(n) => {
                writeln!(f, "addq ${}, %rsp", n)?;
            }
            AsmInstruction::Call(target) => {
                // if function is not defined in symbol table, add @PLT
                if let Some(symbol) = ASM_SYMBOL_TABLE.lock().unwrap().get(target).cloned() {
                    if let AsmSymtabEntry::Function { defined } = symbol {
                        if !defined {
                            writeln!(f, "call {}@PLT", target)?;
                        } else {
                            writeln!(f, "call {}", target)?;
                        }
                    }
                } else {
                    writeln!(f, "call {}", target)?;
                }
            }
            AsmInstruction::Push(operand) => {
                write!(f, "pushq ")?;
                operand.emit(f, &mut AsmType::Quadword)?;
                writeln!(f)?;
            }
            AsmInstruction::MovZeroExtend { .. } => unreachable!(),
            AsmInstruction::Cvtsi2sd { asm_type, src, dst } => {
                let suffix = match asm_type {
                    AsmType::Longword => "l",
                    AsmType::Quadword => "q",
                    _ => todo!(),
                };

                write!(f, "cvtsi2sd{} ", suffix)?;
                src.emit(f, asm_type)?;
                write!(f, ", ")?;
                dst.emit(f, &mut AsmType::Double)?;
                writeln!(f)?;
            }
            AsmInstruction::Cvttsd2si { asm_type, src, dst } => {
                let suffix = match asm_type {
                    AsmType::Longword => "l",
                    AsmType::Quadword => "q",
                    _ => todo!(),
                };

                write!(f, "cvttsd2si{} ", suffix)?;
                src.emit(f, &mut AsmType::Double)?;
                write!(f, ", ")?;
                dst.emit(f, asm_type)?;
                writeln!(f)?;
            }
            _ => unreachable!(),
        }

        Ok(())
    }
}

impl Emit for AsmOperand {
    fn emit(&mut self, f: &mut File, asm_type: &mut AsmType) -> Result<()> {
        match self {
            AsmOperand::Imm(n) => write!(f, "${}", *n as u64)?,
            AsmOperand::Register(reg) => reg.emit(f, asm_type)?,
            AsmOperand::Stack(n) => write!(f, "{}(%rbp)", n)?,
            AsmOperand::Pseudo(_) => unreachable!(),
            AsmOperand::Data(identifier) => write!(f, "{}(%rip)", identifier)?,
        }
        Ok(())
    }
}

impl Emit for AsmRegister {
    fn emit(&mut self, f: &mut File, asm_type: &mut AsmType) -> Result<()> {
        match asm_type {
            AsmType::Longword => match self {
                AsmRegister::AX => write!(f, "%eax")?,
                AsmRegister::DX => write!(f, "%edx")?,
                AsmRegister::CX => write!(f, "%ecx")?,
                AsmRegister::DI => write!(f, "%edi")?,
                AsmRegister::SI => write!(f, "%esi")?,
                AsmRegister::R8 => write!(f, "%r8d")?,
                AsmRegister::R9 => write!(f, "%r9d")?,
                AsmRegister::R10 => write!(f, "%r10d")?,
                AsmRegister::R11 => write!(f, "%r11d")?,
                AsmRegister::SP => write!(f, "%esp")?,
                AsmRegister::XMM0 => write!(f, "%xmm0")?,
                AsmRegister::XMM1 => write!(f, "%xmm1")?,
                AsmRegister::XMM2 => write!(f, "%xmm2")?,
                AsmRegister::XMM3 => write!(f, "%xmm3")?,
                AsmRegister::XMM4 => write!(f, "%xmm4")?,
                AsmRegister::XMM5 => write!(f, "%xmm5")?,
                AsmRegister::XMM6 => write!(f, "%xmm6")?,
                AsmRegister::XMM7 => write!(f, "%xmm7")?,
                AsmRegister::XMM14 => write!(f, "%xmm14")?,
                AsmRegister::XMM15 => write!(f, "%xmm15")?,
            },
            AsmType::Quadword => match self {
                AsmRegister::AX => write!(f, "%rax")?,
                AsmRegister::DX => write!(f, "%rdx")?,
                AsmRegister::CX => write!(f, "%rcx")?,
                AsmRegister::DI => write!(f, "%rdi")?,
                AsmRegister::SI => write!(f, "%rsi")?,
                AsmRegister::R8 => write!(f, "%r8")?,
                AsmRegister::R9 => write!(f, "%r9")?,
                AsmRegister::R10 => write!(f, "%r10")?,
                AsmRegister::R11 => write!(f, "%r11")?,
                AsmRegister::SP => write!(f, "%rsp")?,
                AsmRegister::XMM0 => write!(f, "%xmm0")?,
                AsmRegister::XMM1 => write!(f, "%xmm1")?,
                AsmRegister::XMM2 => write!(f, "%xmm2")?,
                AsmRegister::XMM3 => write!(f, "%xmm3")?,
                AsmRegister::XMM4 => write!(f, "%xmm4")?,
                AsmRegister::XMM5 => write!(f, "%xmm5")?,
                AsmRegister::XMM6 => write!(f, "%xmm6")?,
                AsmRegister::XMM7 => write!(f, "%xmm7")?,
                AsmRegister::XMM14 => write!(f, "%xmm14")?,
                AsmRegister::XMM15 => write!(f, "%xmm15")?,
            },
            AsmType::Double => match self {
                AsmRegister::XMM0 => write!(f, "%xmm0")?,
                AsmRegister::XMM1 => write!(f, "%xmm1")?,
                AsmRegister::XMM2 => write!(f, "%xmm2")?,
                AsmRegister::XMM3 => write!(f, "%xmm3")?,
                AsmRegister::XMM4 => write!(f, "%xmm4")?,
                AsmRegister::XMM5 => write!(f, "%xmm5")?,
                AsmRegister::XMM6 => write!(f, "%xmm6")?,
                AsmRegister::XMM7 => write!(f, "%xmm7")?,
                AsmRegister::XMM14 => write!(f, "%xmm14")?,
                AsmRegister::XMM15 => write!(f, "%xmm15")?,
                AsmRegister::AX => write!(f, "%rax")?,
                AsmRegister::DX => write!(f, "%rdx")?,
                AsmRegister::CX => write!(f, "%rcx")?,
                AsmRegister::DI => write!(f, "%rdi")?,
                AsmRegister::SI => write!(f, "%rsi")?,
                AsmRegister::R8 => write!(f, "%r8")?,
                AsmRegister::R9 => write!(f, "%r9")?,
                AsmRegister::R10 => write!(f, "%r10")?,
                AsmRegister::R11 => write!(f, "%r11")?,
                AsmRegister::SP => write!(f, "%rsp")?,
            }
        }

        Ok(())
    }
}
