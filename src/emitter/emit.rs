use crate::{
    codegen::gen::{
        AsmBinaryOp, AsmFunction, AsmInstruction, AsmNode, AsmOperand, AsmProgram, AsmRegister,
        AsmStaticConstant, AsmStaticVariable, AsmSymtabEntry, AsmType, AsmUnaryOp, ConditionCode,
        ASM_SYMBOL_TABLE,
    },
    emitter::util::escape,
    semantics::typechecker::StaticInit,
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

        writeln!(f, "\t.section .data")?;
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
                StaticInit::Char(c) => writeln!(f, "\t.byte {}", *c as u8)?,
                StaticInit::UChar(c) => writeln!(f, "\t.byte {}", *c as u8)?,
                StaticInit::String(value, null_terminated) => {
                    if *null_terminated {
                        writeln!(f, "\t.asciz \"{}\"", escape(value))?;
                    } else {
                        writeln!(f, "\t.ascii \"{}\"", escape(value))?;
                    }
                }
                StaticInit::Pointer(lbl) => writeln!(f, "\t.quad {}", lbl)?,
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
                    writeln!(f, "\t.asciz \"{}\"", escape(value))?;
                } else {
                    writeln!(f, "\t.ascii \"{}\"", escape(value))?;
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
    fn emit(&mut self, f: &mut File, _asm_type: &mut AsmType) -> Result<()> {
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
                    _ => unreachable!(),
                };

                write!(f, "mov{} ", suffix)?;
                src.emit(f, asm_type)?;
                write!(f, ", ")?;
                dst.emit(f, asm_type)?;
                writeln!(f)?;
            }
            
            AsmInstruction::Movsx {
                src_type,
                src,
                dst_type,
                dst,
            } => {
                let suffix = match (&src_type, &dst_type) {
                    (AsmType::Byte, AsmType::Longword) => "bl",
                    (AsmType::Byte, AsmType::Quadword) => "bq",
                    (AsmType::Byte, AsmType::Double) => "lq",
                    (AsmType::Longword, AsmType::Quadword) => "lq",
                    _ => unreachable!(),
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
            
            AsmInstruction::Cdq { asm_type } => match asm_type {
                AsmType::Longword => writeln!(f, "cdq")?,
                AsmType::Quadword => writeln!(f, "cqo")?,
                _ => writeln!(f, "cdq")?,
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
                        _ => unreachable!(),
                    },
                    AsmBinaryOp::Xor => "xor",
                    AsmBinaryOp::And => "and",
                    AsmBinaryOp::Or => "or",
                    AsmBinaryOp::DivDouble => "div",
                    AsmBinaryOp::ShrTwoOp => "shr",
                    AsmBinaryOp::Shl => "shl",
                };

                let suffix = match asm_type {
                    AsmType::Longword => "l",
                    AsmType::Quadword => "q",
                    AsmType::Double => match op {
                        AsmBinaryOp::Xor => "pd",
                        AsmBinaryOp::DivDouble => "sd",
                        _ => "sd",
                    },
                    AsmType::Byte => "b",
                    _ => unreachable!(),
                };

                write!(f, "{}{} ", instr, suffix)?;

                lhs.emit(f, asm_type)?;
                write!(f, ", ")?;
                rhs.emit(f, asm_type)?;

                writeln!(f)?;
            }
            
            AsmInstruction::Idiv { operand, asm_type } => {
                let suffix = match asm_type {
                    AsmType::Byte => "b",
                    AsmType::Longword => "l",
                    AsmType::Quadword => "q",
                    _ => unreachable!(),
                };
                write!(f, "idiv{} ", suffix)?;
                operand.emit(f, asm_type)?;
                writeln!(f)?;
            }
            
            AsmInstruction::Div { operand, asm_type } => {
                let suffix = match asm_type {
                    AsmType::Byte => "b",
                    AsmType::Longword => "l",
                    AsmType::Quadword => "q",
                    _ => unreachable!(),
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
                    _ => unreachable!(),
                };

                let suffix = match asm_type {
                    AsmType::Longword => "l",
                    AsmType::Quadword => "q",
                    AsmType::Double => "sd",
                    _ => unreachable!(),
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
                    _ => unreachable!(),
                };
                write!(f, "{} ", instr)?;
                lhs.emit(f, asm_type)?;
                write!(f, ", ")?;
                rhs.emit(f, asm_type)?;
                writeln!(f)?;
            }
            
            AsmInstruction::Jmp { target } => {
                writeln!(f, "jmp .L{}", target)?;
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

                writeln!(f, "j{} .L{}", suffix, target)?;
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

                operand.emit(f, &mut AsmType::Byte)?;

                writeln!(f)?;
            }
            
            AsmInstruction::Label(label) => {
                writeln!(f, ".L{}:", label)?;
            }
            
            AsmInstruction::Call(target) => {
                if let Some(symbol) = ASM_SYMBOL_TABLE.lock().unwrap().get(target).cloned() {
                    if let AsmSymtabEntry::Function { defined, .. } = symbol {
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
            
            AsmInstruction::MovZeroExtend {
                src_type,
                src,
                dst_type,
                dst,
            } => {
                let suffix = match (&src_type, &dst_type) {
                    (AsmType::Longword, AsmType::Longword) => "l",
                    (AsmType::Longword, AsmType::Quadword) => "l",
                    (AsmType::Byte, AsmType::Longword) => "zbl",
                    (AsmType::Byte, AsmType::Quadword) => "zbq",
                    (AsmType::Quadword, AsmType::Byte) => "zqb",
                    (AsmType::Longword, AsmType::Byte) => "zlb",
                    _ => unreachable!(),
                };

                write!(f, "mov{} ", suffix)?;
                src.emit(f, src_type)?;
                write!(f, ", ")?;
                dst.emit(f, dst_type)?;
                writeln!(f)?;
            }
            
            AsmInstruction::Cvtsi2sd { asm_type, src, dst } => {
                let suffix = match asm_type {
                    AsmType::Longword => "l",
                    AsmType::Quadword => "q",
                    _ => unreachable!(),
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
                    _ => unreachable!(),
                };

                write!(f, "cvttsd2si{} ", suffix)?;
                src.emit(f, &mut AsmType::Double)?;
                write!(f, ", ")?;
                dst.emit(f, asm_type)?;
                writeln!(f)?;
            }

            AsmInstruction::Pop(reg) => {
                write!(f, "popq ")?;
                reg.emit(f, &mut AsmType::Quadword)?;
                writeln!(f)?;
            }
        }

        Ok(())
    }
}

impl Emit for AsmOperand {
    fn emit(&mut self, f: &mut File, asm_type: &mut AsmType) -> Result<()> {
        match self {
            AsmOperand::Imm(n) => write!(f, "${}", { *n })?,
            
            AsmOperand::Register(reg) => reg.emit(f, asm_type)?,
            
            AsmOperand::Data(identifier, offset) => {
                if *offset == 0 {
                    write!(f, "{}(%rip)", identifier)?
                } else {
                    write!(
                        f,
                        "{}{}{}(%rip)",
                        identifier,
                        if *offset >= 0 { "+" } else { "-" },
                        offset.abs()
                    )?
                }
            },
            
            AsmOperand::Memory(reg, n) => {
                if *n == 0 {
                    write!(f, "({})", reg)?
                } else {
                    write!(f, "{}({})", n, reg)?
                }
            }
            
            AsmOperand::Indexed(reg1, reg2, n) => write!(f, "({}, {}, {})", reg1, reg2, n)?,
            
            /* We don't emit any pseudo-like operands. */
            AsmOperand::PseudoMem(_, _) | AsmOperand::Pseudo(_) => unreachable!(),
        }
        Ok(())
    }
}

impl Emit for AsmRegister {
    fn emit(&mut self, f: &mut File, asm_type: &mut AsmType) -> Result<()> {
        use AsmRegister::*;
        use AsmType::*;

        match (self, asm_type) {
            (Ax, Byte) => write!(f, "%al")?,
            (Bx, Byte) => write!(f, "%bl")?,
            (Dx, Byte) => write!(f, "%dl")?,
            (Cx, Byte) => write!(f, "%cl")?,
            (Di, Byte) => write!(f, "%dil")?,
            (Si, Byte) => write!(f, "%sil")?,
            (Sp, Byte) => write!(f, "%spl")?,
            (Bp, Byte) => write!(f, "%bpl")?,
            (R8, Byte) => write!(f, "%r8b")?,
            (R9, Byte) => write!(f, "%r9b")?,
            (R10, Byte) => write!(f, "%r10b")?,
            (R11, Byte) => write!(f, "%r11b")?,
            (R12, Byte) => write!(f, "%r12b")?,
            (R13, Byte) => write!(f, "%r13b")?,
            (R14, Byte) => write!(f, "%r14b")?,
            (R15, Byte) => write!(f, "%r15b")?,

            (Ax, Longword) => write!(f, "%eax")?,
            (Bx, Longword) => write!(f, "%ebx")?,
            (Dx, Longword) => write!(f, "%edx")?,
            (Cx, Longword) => write!(f, "%ecx")?,
            (Di, Longword) => write!(f, "%edi")?,
            (Si, Longword) => write!(f, "%esi")?,
            (Sp, Longword) => write!(f, "%esp")?,
            (Bp, Longword) => write!(f, "%ebp")?,
            (R8, Longword) => write!(f, "%r8d")?,
            (R9, Longword) => write!(f, "%r9d")?,
            (R10, Longword) => write!(f, "%r10d")?,
            (R11, Longword) => write!(f, "%r11d")?,
            (R12, Longword) => write!(f, "%r12d")?,
            (R13, Longword) => write!(f, "%r13d")?,
            (R14, Longword) => write!(f, "%r14d")?,
            (R15, Longword) => write!(f, "%r15d")?,

            (Ax, Quadword | Double) => write!(f, "%rax")?,
            (Bx, Quadword | Double) => write!(f, "%rbx")?,
            (Dx, Quadword | Double) => write!(f, "%rdx")?,
            (Cx, Quadword | Double) => write!(f, "%rcx")?,
            (Di, Quadword | Double) => write!(f, "%rdi")?,
            (Si, Quadword | Double) => write!(f, "%rsi")?,
            (Bp, Quadword | Double) => write!(f, "%rbp")?,
            (Sp, Quadword | Double) => write!(f, "%rsp")?,
            (R8, Quadword | Double) => write!(f, "%r8")?,
            (R9, Quadword | Double) => write!(f, "%r9")?,
            (R10, Quadword | Double) => write!(f, "%r10")?,
            (R11, Quadword | Double) => write!(f, "%r11")?,
            (R12, Quadword | Double) => write!(f, "%r12")?,
            (R13, Quadword | Double) => write!(f, "%r13")?,
            (R14, Quadword | Double) => write!(f, "%r14")?,
            (R15, Quadword | Double) => write!(f, "%r15")?,

            (Xmm0, _) => write!(f, "%xmm0")?,
            (Xmm1, _) => write!(f, "%xmm1")?,
            (Xmm2, _) => write!(f, "%xmm2")?,
            (Xmm3, _) => write!(f, "%xmm3")?,
            (Xmm4, _) => write!(f, "%xmm4")?,
            (Xmm5, _) => write!(f, "%xmm5")?,
            (Xmm6, _) => write!(f, "%xmm6")?,
            (Xmm7, _) => write!(f, "%xmm7")?,
            (Xmm8, _) => write!(f, "%xmm8")?,
            (Xmm9, _) => write!(f, "%xmm9")?,
            (Xmm10, _) => write!(f, "%xmm10")?,
            (Xmm11, _) => write!(f, "%xmm11")?,
            (Xmm12, _) => write!(f, "%xmm12")?,
            (Xmm13, _) => write!(f, "%xmm13")?,
            (Xmm14, _) => write!(f, "%xmm14")?,
            (Xmm15, _) => write!(f, "%xmm15")?,

            _ => todo!(),
        }

        Ok(())
    }
}
