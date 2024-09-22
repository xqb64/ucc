use crate::{
    codegen::{
        AsmBinaryOp, AsmFunction, AsmInstruction, AsmNode, AsmOperand, AsmProgram, AsmRegister,
        AsmStaticConstant, AsmStaticVariable, AsmSymtabEntry, AsmType, AsmUnaryOp, ConditionCode,
        ASM_SYMBOL_TABLE,
    },
    typechecker::StaticInit,
};
use anyhow::Result;
use std::{fs::File, io::Write};

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

        for init in &self.init {
            match init {
                StaticInit::Int(n) => match n {
                    0 => writeln!(f, "\t.section .bss")?,
                    _ => writeln!(f, "\t.section .data")?,
                },
                StaticInit::Long(n) => match n {
                    0 => writeln!(f, "\t.section .bss")?,
                    _ => writeln!(f, "\t.section .data")?,
                },
                StaticInit::UInt(n) => match n {
                    0 => writeln!(f, "\t.section .bss")?,
                    _ => writeln!(f, "\t.section .data")?,
                },
                StaticInit::ULong(n) => match n {
                    0 => writeln!(f, "\t.section .bss")?,
                    _ => writeln!(f, "\t.section .data")?,
                },
                StaticInit::Double(n) => match n {
                    0.0 => writeln!(f, "\t.section .bss")?,
                    _ => writeln!(f, "\t.section .data")?,
                },
                StaticInit::Zero(_) => {
                    writeln!(f, "\t.section .data")?;
                }
                StaticInit::String(_, _) => {
                    writeln!(f, "\t.section .rodata")?;
                }
                StaticInit::Char(c) => match c {
                    0 => writeln!(f, "\t.section .bss")?,
                    _ => writeln!(f, "\t.section .data")?,
                },
                StaticInit::UChar(c) => match c {
                    0 => writeln!(f, "\t.section .bss")?,
                    _ => writeln!(f, "\t.section .data")?,
                }
                _ => todo!(),
            }
        }

        if self.global {
            writeln!(f, "\t.globl {}", self.name)?;
        }

        writeln!(f, "\t.balign {}", self.alignment)?;

        writeln!(f, "{}:", self.name)?;

        for init in &self.init {
            match init {
                StaticInit::Int(n) => match n {
                    0 => writeln!(f, "\t.zero 4")?,
                    _ => writeln!(f, "\t.long {}", n)?,
                },
                StaticInit::Long(n) => match n {
                    0 => writeln!(f, "\t.zero 8")?,
                    _ => writeln!(f, "\t.quad {}", n)?,
                },
                StaticInit::UInt(n) => match n {
                    0 => writeln!(f, "\t.zero 4")?,
                    _ => writeln!(f, "\t.long {}", n)?,
                },
                StaticInit::ULong(n) => match n {
                    0 => writeln!(f, "\t.zero 8")?,
                    _ => writeln!(f, "\t.quad {}", n)?,
                },
                StaticInit::Double(n) => writeln!(f, "\t.quad {}", n.to_bits())?,
                StaticInit::Zero(n) => {
                    writeln!(f, "\t.zero {}", n)?;
                }
                StaticInit::Char(c) => writeln!(f, "\t.byte {}", c)?,
                StaticInit::UChar(c) => writeln!(f, "\t.byte {}", c)?,
                StaticInit::String(value, null_terminated) => {
                    if *null_terminated {
                        writeln!(f, "\t.asciz \"{}\"", value)?;
                    } else {
                        writeln!(f, "\t.ascii \"{}\"", value)?;
                    }
                }
                _ => todo!(),
            }
        }

        Ok(())
    }
}

impl Emit for AsmStaticConstant {
    fn emit(&mut self, f: &mut File, _asm_type: &mut AsmType) -> Result<()> {
        writeln!(f)?;

        writeln!(f, "\t.section .rodata")?;
        writeln!(f, "\t.balign {}", self.alignment)?;
        writeln!(f, "{}:", self.name)?;

        match &self.init {
            StaticInit::Int(n) => writeln!(f, "\t.long {}", n)?,
            StaticInit::Long(n) => writeln!(f, "\t.quad {}", n)?,
            StaticInit::UInt(n) => writeln!(f, "\t.long {}", n)?,
            StaticInit::ULong(n) => writeln!(f, "\t.quad {}", n)?,
            StaticInit::Double(n) => writeln!(f, "\t.quad {}", n.to_bits())?,
            StaticInit::Char(c) => writeln!(f, "\t.byte {}", c)?,
            StaticInit::UChar(c) => writeln!(f, "\t.byte {}", c)?,
            StaticInit::Zero(n) => {
                writeln!(f, "\t.zero {}", n)?;
            }
            StaticInit::String(value, null_terminated) => {
                if *null_terminated {
                    writeln!(f, "\t.asciz \"{}\"", value)?;
                } else {
                    writeln!(f, "\t.ascii \"{}\"", value)?;
                }
            }
            _ => unreachable!(),
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
            AsmInstruction::Lea { src, dst } => {
                write!(f, "leaq ")?;
                src.emit(f, &mut AsmType::Quadword)?;
                write!(f, ", ")?;
                dst.emit(f, &mut AsmType::Quadword)?;
                writeln!(f)?;
            }
            AsmInstruction::Mov { src, dst, asm_type } => {
                let suffix = match asm_type {
                    AsmType::Byte => "b",
                    AsmType::Longword => "l",
                    AsmType::Quadword => "q",
                    AsmType::Double => "sd",
                    _ => todo!(),
                };
                write!(f, "mov{} ", suffix)?;
                src.emit(f, asm_type)?;
                write!(f, ", ")?;
                dst.emit(f, asm_type)?;
                writeln!(f)?;
            }
            AsmInstruction::Movsx { src_type, src, dst_type, dst } => {
                let suffix = match (&src_type, &dst_type) {
                    (AsmType::Longword, AsmType::Quadword) => "lq",
                    (AsmType::Quadword, AsmType::Longword) => "ql",
                    (AsmType::Byte, AsmType::Longword) => "bl",
                    (AsmType::Byte, AsmType::Quadword) => "bq",
                    (AsmType::Quadword, AsmType::Byte) => "qb",
                    (AsmType::Longword, AsmType::Byte) => "lb",
                    _ => todo!(),
                };
                
                write!(f, "movs{} ", suffix)?;
                src.emit(f, src_type)?;
                write!(f, ", ")?;
                dst.emit(f, dst_type)?;
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
                        _ => todo!(),
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
                    },
                    _ => todo!(),
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
            AsmInstruction::Div { operand, asm_type } => {
                let suffix = match asm_type {
                    AsmType::Longword => "l",
                    AsmType::Quadword => "q",
                    _ => todo!(),
                };
                write!(f, "div{} ", suffix)?;
                operand.emit(f, asm_type)?;
                writeln!(f)?;
            }
            AsmInstruction::Imul { src, dst, asm_type } => {
                let instr = match asm_type {
                    AsmType::Longword => "imul",
                    AsmType::Quadword => "imul",
                    AsmType::Double => "mul",
                    _ => todo!(),
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
                    AsmType::Byte => "cmpb",
                    AsmType::Longword => "cmpl",
                    AsmType::Quadword => "cmpq",
                    AsmType::Double => "comisd",
                    _ => todo!(),
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
            AsmInstruction::MovZeroExtend { src_type, src, dst_type, dst } => {
                let suffix = match (&src_type, &dst_type) {
                    (AsmType::Longword, AsmType::Quadword) => "lq",
                    (AsmType::Quadword, AsmType::Longword) => "ql",
                    (AsmType::Byte, AsmType::Longword) => "bl",
                    (AsmType::Byte, AsmType::Quadword) => "bq",
                    (AsmType::Quadword, AsmType::Byte) => "qb",
                    (AsmType::Longword, AsmType::Byte) => "lb",
                    _ => todo!(),
                };

                write!(f, "movz{} ", suffix)?;
                src.emit(f, src_type)?;
                write!(f, ", ")?;
                dst.emit(f, dst_type)?;
                writeln!(f)?;
            },
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
        }

        Ok(())
    }
}

impl Emit for AsmOperand {
    fn emit(&mut self, f: &mut File, asm_type: &mut AsmType) -> Result<()> {
        match self {
            AsmOperand::Imm(n) => write!(f, "${}", *n as u64)?,
            AsmOperand::Register(reg) => reg.emit(f, asm_type)?,
            AsmOperand::Pseudo(_) => unreachable!(),
            AsmOperand::Data(identifier) => write!(f, "{}(%rip)", identifier)?,
            AsmOperand::Memory(reg, n) => write!(f, "{}({})", n, reg)?,
            AsmOperand::Indexed(reg1, reg2, n) => write!(f, "({}, {}, {})", reg1, reg2, n)?,
            AsmOperand::PseudoMem(_, _) => unreachable!(),
        }
        Ok(())
    }
}

impl std::fmt::Display for AsmRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AsmRegister::AX => write!(f, "%rax"),
            AsmRegister::DX => write!(f, "%rdx"),
            AsmRegister::CX => write!(f, "%rcx"),
            AsmRegister::DI => write!(f, "%rdi"),
            AsmRegister::SI => write!(f, "%rsi"),
            AsmRegister::R8 => write!(f, "%r8"),
            AsmRegister::R9 => write!(f, "%r9"),
            AsmRegister::R10 => write!(f, "%r10"),
            AsmRegister::R11 => write!(f, "%r11"),
            AsmRegister::BP => write!(f, "%rbp"),
            AsmRegister::SP => write!(f, "%rsp"),
            AsmRegister::XMM0 => write!(f, "%xmm0"),
            AsmRegister::XMM1 => write!(f, "%xmm1"),
            AsmRegister::XMM2 => write!(f, "%xmm2"),
            AsmRegister::XMM3 => write!(f, "%xmm3"),
            AsmRegister::XMM4 => write!(f, "%xmm4"),
            AsmRegister::XMM5 => write!(f, "%xmm5"),
            AsmRegister::XMM6 => write!(f, "%xmm6"),
            AsmRegister::XMM7 => write!(f, "%xmm7"),
            AsmRegister::XMM14 => write!(f, "%xmm14"),
            AsmRegister::XMM15 => write!(f, "%xmm15"),
        }
    }
}

impl Emit for AsmRegister {
    fn emit(&mut self, f: &mut File, asm_type: &mut AsmType) -> Result<()> {
        match asm_type {
            AsmType::Byte => match self {
                AsmRegister::AX => write!(f, "%al")?,
                AsmRegister::DX => write!(f, "%dl")?,
                AsmRegister::CX => write!(f, "%cl")?,
                AsmRegister::DI => write!(f, "%dil")?,
                AsmRegister::SI => write!(f, "%sil")?,
                AsmRegister::R8 => write!(f, "%r8b")?,
                AsmRegister::R9 => write!(f, "%r9b")?,
                AsmRegister::R10 => write!(f, "%r10b")?,
                AsmRegister::R11 => write!(f, "%r11b")?,
                AsmRegister::SP => write!(f, "%spl")?,
                AsmRegister::BP => write!(f, "%bpl")?,
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
                AsmRegister::BP => write!(f, "%ebp")?,
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
            AsmType::Quadword | AsmType::Double => match self {
                AsmRegister::AX => write!(f, "%rax")?,
                AsmRegister::DX => write!(f, "%rdx")?,
                AsmRegister::CX => write!(f, "%rcx")?,
                AsmRegister::DI => write!(f, "%rdi")?,
                AsmRegister::SI => write!(f, "%rsi")?,
                AsmRegister::R8 => write!(f, "%r8")?,
                AsmRegister::R9 => write!(f, "%r9")?,
                AsmRegister::R10 => write!(f, "%r10")?,
                AsmRegister::R11 => write!(f, "%r11")?,
                AsmRegister::BP => write!(f, "%rbp")?,
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
            _ => todo!(),
        }

        Ok(())
    }
}
