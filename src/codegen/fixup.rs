use crate::codegen::gen::{
    AsmBinaryOp, AsmFunction, AsmInstruction, AsmNode, AsmOperand, AsmProgram, AsmRegister,
    AsmSymtabEntry, AsmType, ASM_SYMBOL_TABLE,
};
use std::collections::BTreeSet;

pub trait Fixup {
    fn fixup(&mut self, callee_saved_args: &BTreeSet<AsmRegister>) -> Self;
}

impl Fixup for AsmNode {
    fn fixup(&mut self, callee_saved_args: &BTreeSet<AsmRegister>) -> Self {
        match self {
            AsmNode::Program(prog) => AsmNode::Program(prog.fixup(callee_saved_args)),
            AsmNode::Function(func) => AsmNode::Function(func.fixup(callee_saved_args)),
            AsmNode::Operand(op) => AsmNode::Operand(op.clone()),
            AsmNode::StaticVariable(static_var) => AsmNode::StaticVariable(static_var.clone()),
            AsmNode::StaticConstant(static_const) => AsmNode::StaticConstant(static_const.clone()),
            _ => unreachable!(),
        }
    }
}

impl Fixup for AsmProgram {
    fn fixup(&mut self, _callee_saved_args: &BTreeSet<AsmRegister>) -> AsmProgram {
        let mut functions = vec![];

        for func in &mut self.functions {
            let func_name = match func {
                AsmNode::Function(f) => f.name.clone(),
                _ => unreachable!(),
            };

            let symbol = ASM_SYMBOL_TABLE
                .lock()
                .unwrap()
                .get(&func_name)
                .cloned()
                .unwrap();

            let callee_saved_args = match symbol {
                AsmSymtabEntry::Function {
                    callee_saved_regs_used,
                    ..
                } => callee_saved_regs_used,
                _ => unreachable!(),
            };

            functions.push(func.fixup(&callee_saved_args));
        }

        AsmProgram {
            functions,
            static_vars: self.static_vars.clone(),
            static_constants: self.static_constants.clone(),
        }
    }
}

impl Fixup for AsmFunction {
    fn fixup(&mut self, callee_saved_args: &BTreeSet<AsmRegister>) -> AsmFunction {
        let mut instructions = vec![];
        let mut instructions_setup = vec![];

        fn round_away_from_zero(n: i64, x: i64) -> i64 {
            if n == 0 {
                panic!("`n` must be a non-zero integer.");
            }

            match x {
                x if x % n == 0 => x,
                x if x < 0 => x - n - (x % n),
                x => x + n - (x % n),
            }
        }

        fn emit_stack_adjustment(
            bytes_for_locals: usize,
            callee_saved_count: usize,
        ) -> AsmInstruction {
            let callee_saved_bytes = 8 * callee_saved_count;

            let total_stack_bytes = callee_saved_bytes + bytes_for_locals;

            let adjusted_stack_bytes = round_away_from_zero(16, total_stack_bytes as i64);

            let stack_adjustment = adjusted_stack_bytes - callee_saved_bytes as i64;

            AsmInstruction::Binary {
                op: AsmBinaryOp::Sub,
                asm_type: AsmType::Quadword,
                lhs: AsmOperand::Imm(stack_adjustment),
                rhs: AsmOperand::Register(AsmRegister::SP),
            }
        }

        instructions_setup.push(emit_stack_adjustment(
            self.stack_space,
            callee_saved_args.len(),
        ));

        for reg in callee_saved_args {
            instructions_setup.push(AsmInstruction::Push(AsmOperand::Register(*reg)));
        }

        self.instructions.splice(0..0, instructions_setup);

        for instr in &mut self.instructions {
            match instr {

                /* 'mov' can't move a value from one memory address to another */
                AsmInstruction::Mov { asm_type, src, dst } if matches!(src, AsmOperand::Memory(_, _) | AsmOperand::Data(_ , _,)) && matches!(dst, AsmOperand::Memory(_, _) | AsmOperand::Data(_ , _,)) => {
                    let scratch = if asm_type == &AsmType::Double {
                        AsmOperand::Register(AsmRegister::XMM14)
                    } else {
                        AsmOperand::Register(AsmRegister::R10)
                    };

                    instructions.extend(vec![
                        AsmInstruction::Mov {
                            asm_type: *asm_type,
                            src: src.clone(),
                            dst: scratch.clone(),
                        },
                        AsmInstruction::Mov {
                            asm_type: *asm_type,
                            src: scratch.clone(),
                            dst: dst.clone(),
                        },
                    ]);
                },

                /* 'mov' can't move a large constant to a memory address */
                AsmInstruction::Mov { asm_type: AsmType::Quadword, src: AsmOperand::Imm(imm), dst } if is_large(*imm) && matches!(dst, AsmOperand::Memory(_, _,) | AsmOperand::Data(_, _,)) => {
                    instructions.extend(vec![
                        AsmInstruction::Mov {
                            asm_type: AsmType::Quadword,
                            src: AsmOperand::Imm(*imm),
                            dst: AsmOperand::Register(AsmRegister::R10),
                        },
                        AsmInstruction::Mov {
                            asm_type: AsmType::Quadword,
                            src: AsmOperand::Register(AsmRegister::R10),
                            dst: dst.clone(),
                        },
                    ]);
                },

                /* 'mov'-ing a quadword-size constant with a longword operand size produces an assembler warning */
                AsmInstruction::Mov { asm_type: AsmType::Longword, src: AsmOperand::Imm(imm), dst } if is_larger_than_uint(*imm) => {
                    let bitmask: i64 = 0xffffffff;
                    let reduced = *imm & bitmask;
                
                    instructions.extend(vec![
                        AsmInstruction::Mov {
                            asm_type: AsmType::Longword,
                            src: AsmOperand::Imm(reduced),
                            dst: dst.clone(),
                        },
                    ]);
                },

                /* 'mov'-ing a longword-size constant with a byte operand size produces an assembler warning */
                AsmInstruction::Mov { asm_type: AsmType::Byte, src: AsmOperand::Imm(imm), dst } if is_larger_than_byte(*imm) => {
                    let bitmask: i64 = 0xff;
                    let reduced = *imm & bitmask;
                
                    instructions.extend(vec![
                        AsmInstruction::Mov {
                            asm_type: AsmType::Byte,
                            src: AsmOperand::Imm(reduced),
                            dst: dst.clone(),
                        },
                    ]);
                },

                /* 'movsx' can't handle immediate source or memory dst */
                AsmInstruction::Movsx { src_type, src: AsmOperand::Imm(imm), dst_type, dst } if matches!(dst, AsmOperand::Memory(_, _,) | AsmOperand::Data(_, _)) => {
                    instructions.extend(vec![
                        AsmInstruction::Mov { asm_type: *src_type, src: AsmOperand::Imm(*imm), dst: AsmOperand::Register(AsmRegister::R10) },
                        AsmInstruction::Movsx { src_type: *src_type, src: AsmOperand::Register(AsmRegister::R10), dst_type: *dst_type, dst: AsmOperand::Register(AsmRegister::R11) },
                        AsmInstruction::Mov { asm_type: *dst_type, src: AsmOperand::Register(AsmRegister::R11), dst: dst.clone() },    
                    ]);
                }

                AsmInstruction::Movsx { src_type, src: AsmOperand::Imm(imm), dst_type, dst } => {
                    instructions.extend(vec![
                        AsmInstruction::Mov { asm_type: *src_type, src: AsmOperand::Imm(*imm), dst: AsmOperand::Register(AsmRegister::R10) },
                        AsmInstruction::Movsx { src_type: *src_type, src: AsmOperand::Register(AsmRegister::R10), dst_type: *dst_type, dst: dst.clone() },

                    ]);
                }

                AsmInstruction::Movsx { src_type, src, dst_type, dst } if matches!(dst, AsmOperand::Memory(_, _) | AsmOperand::Data(_, _)) => {
                    instructions.extend(vec![
                        AsmInstruction::Movsx { src_type: *src_type, src: src.clone(), dst_type: *dst_type, dst: AsmOperand::Register(AsmRegister::R11) },
                        AsmInstruction::Mov { asm_type: *dst_type, src: AsmOperand::Register(AsmRegister::R11), dst: dst.clone() },
                    ]);
                }

                AsmInstruction::MovZeroExtend { src_type: AsmType::Byte, src: AsmOperand::Imm(imm), dst_type, dst } => {
                    /* 'movzeroextend' src can't be an immediate */
                    if is_memory(dst) {
                        instructions.extend(vec![
                            AsmInstruction::Mov { asm_type: AsmType::Byte, src: AsmOperand::Imm(*imm), dst: AsmOperand::Register(AsmRegister::R10) },
                            AsmInstruction::MovZeroExtend { src_type: AsmType::Byte, src: AsmOperand::Register(AsmRegister::R10), dst_type: *dst_type, dst: AsmOperand::Register(AsmRegister::R11) },
                            AsmInstruction::Mov { asm_type: *dst_type, src: AsmOperand::Register(AsmRegister::R11), dst: dst.clone() },
                        ]);
                    } else {
                        instructions.extend(vec![
                            AsmInstruction::Mov { asm_type: AsmType::Byte, src: AsmOperand::Imm(*imm), dst: AsmOperand::Register(AsmRegister::R10) },
                            AsmInstruction::MovZeroExtend { src_type: AsmType::Byte, src: AsmOperand::Register(AsmRegister::R10), dst_type: *dst_type, dst: dst.clone() },
                        ]);
                    }
                }

                /* 'movzeroextend' dst must be a register */
                AsmInstruction::MovZeroExtend { src_type: AsmType::Byte, src, dst_type, dst } if is_memory(dst) => {
                    instructions.extend(vec![
                        AsmInstruction::MovZeroExtend { src_type: AsmType::Byte, src: src.clone(), dst_type: *dst_type, dst: AsmOperand::Register(AsmRegister::R11) },
                        AsmInstruction::Mov { asm_type: *dst_type, src: AsmOperand::Register(AsmRegister::R11), dst: dst.clone() },
                    ]);
                }

                AsmInstruction::MovZeroExtend { src_type: AsmType::Longword, src, dst_type, dst } if is_memory(dst) => {
                    /* to zero-extend a longword to a quadword, we need to move the longword to a register, then move to destination */
                    instructions.extend(vec![
                        AsmInstruction::Mov { asm_type: AsmType::Longword, src: src.clone(), dst: AsmOperand::Register(AsmRegister::R11) },
                        AsmInstruction::Mov { asm_type: *dst_type, src: AsmOperand::Register(AsmRegister::R11), dst: dst.clone() },
                    ]);
                }

                AsmInstruction::MovZeroExtend { src_type: AsmType::Longword, src, dst_type: _, dst } => {
                    /* if destination is already a register, zero-extend with a single mov instruction */
                    instructions.extend(vec![
                        AsmInstruction::Mov { asm_type: AsmType::Longword, src: src.clone(), dst: dst.clone() },
                    ]);
                }

                /* Idiv can't operate on constants */
                AsmInstruction::Idiv { asm_type, operand: AsmOperand::Imm(imm) } => {
                    instructions.extend(vec![
                        AsmInstruction::Mov { asm_type: *asm_type, src: AsmOperand::Imm(*imm), dst: AsmOperand::Register(AsmRegister::R10) },
                        AsmInstruction::Idiv { asm_type: *asm_type, operand: AsmOperand::Register(AsmRegister::R10) },
                    ]);
                }

                AsmInstruction::Div { asm_type, operand: AsmOperand::Imm(imm) } => {
                    instructions.extend(vec![
                        AsmInstruction::Mov { asm_type: *asm_type, src: AsmOperand::Imm(*imm), dst: AsmOperand::Register(AsmRegister::R10) },
                        AsmInstruction::Div { asm_type: *asm_type, operand: AsmOperand::Register(AsmRegister::R10) },
                    ]);
                }

                AsmInstruction::Lea { src, dst } if is_memory(dst) => {
                    instructions.extend(vec![
                        AsmInstruction::Lea {
                            src: src.clone(),
                            dst: AsmOperand::Register(AsmRegister::R11),
                        },
                        AsmInstruction::Mov {
                            asm_type: AsmType::Quadword,
                            src: AsmOperand::Register(AsmRegister::R11),
                            dst: dst.clone(),
                        },
                    ]);
                },

                AsmInstruction::Binary {
                    asm_type: AsmType::Double,
                    op: _,
                    lhs: _,
                    rhs: AsmOperand::Register(_),
                } => instructions.extend(vec![instr.clone()]),

                AsmInstruction::Binary {
                    asm_type: AsmType::Double,
                    op,
                    lhs,
                    rhs,
                } => {
                    instructions.extend(vec![
                        AsmInstruction::Mov {
                            asm_type: AsmType::Double,
                            src: rhs.clone(),
                            dst: AsmOperand::Register(AsmRegister::XMM15),
                        },
                        AsmInstruction::Binary {
                            asm_type: AsmType::Double,
                            op: *op,
                            lhs: lhs.clone(),
                            rhs: AsmOperand::Register(AsmRegister::XMM15),
                        },
                        AsmInstruction::Mov {
                            asm_type: AsmType::Double,
                            src: AsmOperand::Register(AsmRegister::XMM15),
                            dst: rhs.clone(),
                        },
                    ]);
                }

                /* Add | Sub | And | Or can't take large immediates as source operands */
                AsmInstruction::Binary {
                    asm_type: AsmType::Quadword,
                    op,
                    lhs: AsmOperand::Imm(imm),
                    rhs,
                } if is_large(*imm)
                    && matches!(
                        op,
                        AsmBinaryOp::Add | AsmBinaryOp::Sub | AsmBinaryOp::And | AsmBinaryOp::Or
                    ) =>
                {
                    instructions.extend(vec![
                        AsmInstruction::Mov {
                            asm_type: AsmType::Quadword,
                            src: AsmOperand::Imm(*imm),
                            dst: AsmOperand::Register(AsmRegister::R10),
                        },
                        AsmInstruction::Binary {
                            asm_type: AsmType::Quadword,
                            op: *op,
                            lhs: AsmOperand::Register(AsmRegister::R10),
                            rhs: rhs.clone(),
                        },
                    ]);
                }

                /* Add | Sub | And | Or can't take large immediates as source operands */
                AsmInstruction::Binary {
                    asm_type: AsmType::Quadword,
                    op,
                    lhs,
                    rhs: AsmOperand::Imm(imm),
                } if is_large(*imm)
                    && matches!(
                        op,
                        AsmBinaryOp::Add | AsmBinaryOp::Sub | AsmBinaryOp::And | AsmBinaryOp::Or
                    ) =>
                {
                    instructions.extend(vec![
                        AsmInstruction::Mov {
                            asm_type: AsmType::Quadword,
                            src: AsmOperand::Imm(*imm),
                            dst: AsmOperand::Register(AsmRegister::R11),
                        },
                        AsmInstruction::Binary {
                            asm_type: AsmType::Quadword,
                            op: *op,
                            lhs: AsmOperand::Register(AsmRegister::R11),
                            rhs: lhs.clone(),
                        },
                    ]);
                }

                /* Add | Sub | And | Or can't use memory addresses for both operands */
                AsmInstruction::Binary {
                    asm_type,
                    op,
                    lhs,
                    rhs,
                } if matches!(
                        op,
                        AsmBinaryOp::Add | AsmBinaryOp::Sub | AsmBinaryOp::And | AsmBinaryOp::Or
                    ) && matches!(lhs, AsmOperand::Memory(_, _) | AsmOperand::Data(_, _)) && matches!(rhs, AsmOperand::Memory(_, _) | AsmOperand::Data(_, _)) =>
                {
                    instructions.extend(vec![
                        AsmInstruction::Mov {
                            asm_type: *asm_type,
                            src: lhs.clone(),
                            dst: AsmOperand::Register(AsmRegister::R10),
                        },
                        AsmInstruction::Binary {
                            asm_type: *asm_type,
                            op: *op,
                            lhs: AsmOperand::Register(AsmRegister::R10),
                            rhs: rhs.clone(),
                        },
                    ]);
                }

                /* Destination of 'mul' can't be in memory and src can't be a big operand */
                AsmInstruction::Binary {
                    asm_type: AsmType::Quadword,
                    op: AsmBinaryOp::Mul,
                    lhs: AsmOperand::Imm(imm),
                    rhs,
                } if is_large(*imm)
                    && matches!(rhs, AsmOperand::Memory(_, _) | AsmOperand::Data(_, _)) =>
                {
                    instructions.extend(vec![
                        AsmInstruction::Mov {
                            asm_type: AsmType::Quadword,
                            src: AsmOperand::Imm(*imm),
                            dst: AsmOperand::Register(AsmRegister::R10),
                        },
                        AsmInstruction::Mov {
                            asm_type: AsmType::Quadword,
                            src: rhs.clone(),
                            dst: AsmOperand::Register(AsmRegister::R11),
                        },
                        AsmInstruction::Binary {
                            asm_type: AsmType::Quadword,
                            op: AsmBinaryOp::Mul,
                            lhs: AsmOperand::Register(AsmRegister::R10),
                            rhs: AsmOperand::Register(AsmRegister::R11),
                        },
                        AsmInstruction::Mov {
                            asm_type: AsmType::Quadword,
                            src: AsmOperand::Register(AsmRegister::R11),
                            dst: rhs.clone(),
                        },
                    ]);
                }

                AsmInstruction::Binary {
                    asm_type: AsmType::Quadword,
                    op: AsmBinaryOp::Mul,
                    lhs: AsmOperand::Imm(imm),
                    rhs,
                } if is_large(*imm) => {
                    instructions.extend(vec![
                        AsmInstruction::Mov {
                            asm_type: AsmType::Quadword,
                            src: AsmOperand::Imm(*imm),
                            dst: AsmOperand::Register(AsmRegister::R10),
                        },
                        AsmInstruction::Binary {
                            asm_type: AsmType::Quadword,
                            op: AsmBinaryOp::Mul,
                            lhs: AsmOperand::Register(AsmRegister::R10),
                            rhs: rhs.clone(),
                        },
                    ]);
                }

                AsmInstruction::Binary {
                    asm_type,
                    op: AsmBinaryOp::Mul,
                    lhs,
                    rhs,
                } if matches!(rhs, AsmOperand::Memory(_, _) | AsmOperand::Data(_, _)) => {
                    let scratch = if asm_type == &AsmType::Double {
                        AsmOperand::Register(AsmRegister::XMM15)
                    } else {
                        AsmOperand::Register(AsmRegister::R11)
                    };
                    instructions.extend(vec![
                        AsmInstruction::Mov {
                            asm_type: *asm_type,
                            src: rhs.clone(),
                            dst: scratch.clone(),
                        },
                        AsmInstruction::Binary {
                            asm_type: *asm_type,
                            op: AsmBinaryOp::Mul,
                            lhs: lhs.clone(),
                            rhs: scratch.clone(),
                        },
                        AsmInstruction::Mov {
                            asm_type: *asm_type,
                            src: scratch.clone(),
                            dst: rhs.clone(),
                        },
                    ]);
                }

                /* Destination of 'cmp' must be a register. */
                AsmInstruction::Cmp {
                    asm_type: AsmType::Double,
                    lhs: _,
                    rhs: AsmOperand::Register(_),
                } => instructions.push(instr.clone()),


                AsmInstruction::Cmp {
                    asm_type: AsmType::Double,
                    lhs,
                    rhs,
                } => {
                    instructions.extend(vec![
                        AsmInstruction::Mov {
                            asm_type: AsmType::Double,
                            src: rhs.clone(),
                            dst: AsmOperand::Register(AsmRegister::XMM15),
                        },
                        AsmInstruction::Cmp {
                            asm_type: AsmType::Double,
                            lhs: lhs.clone(),
                            rhs: AsmOperand::Register(AsmRegister::XMM15),
                        },
                    ]);
                }

                /* Not both operands of 'cmp' can be in memory */
                AsmInstruction::Cmp { asm_type, lhs, rhs } if matches!(lhs, AsmOperand::Memory(_, _) | AsmOperand::Data(_, _)) && matches!(rhs, AsmOperand::Memory(_, _) | AsmOperand::Data(_, _)) => {
                    instructions.extend(vec![
                        AsmInstruction::Mov {
                            asm_type: *asm_type,
                            src: lhs.clone(),
                            dst: AsmOperand::Register(AsmRegister::R10),
                        },
                        AsmInstruction::Cmp {
                            asm_type: *asm_type,
                            lhs: AsmOperand::Register(AsmRegister::R10),
                            rhs: rhs.clone(),
                        },
                    ]);
                }

                /* First operand of 'cmp' can't be a large constant, and second can't be a constant at all */
                AsmInstruction::Cmp { asm_type: AsmType::Quadword, lhs: AsmOperand::Imm(imm1), rhs: AsmOperand::Imm(imm2) } if is_large(*imm1) => {
                    instructions.extend(vec![
                        AsmInstruction::Mov { asm_type: AsmType::Quadword, src: AsmOperand::Imm(*imm1), dst: AsmOperand::Register(AsmRegister::R10) },
                        AsmInstruction::Mov { asm_type: AsmType::Quadword, src: AsmOperand::Imm(*imm2), dst: AsmOperand::Register(AsmRegister::R11) },
                        AsmInstruction::Cmp { asm_type: AsmType::Quadword, lhs: AsmOperand::Register(AsmRegister::R10), rhs: AsmOperand::Register(AsmRegister::R11) },
                    ]);
                }

                AsmInstruction::Cmp { asm_type: AsmType::Quadword, lhs: AsmOperand::Imm(imm1), rhs } => {
                    instructions.extend(vec![
                        AsmInstruction::Mov { asm_type: AsmType::Quadword, src: AsmOperand::Imm(*imm1), dst: AsmOperand::Register(AsmRegister::R10) },
                        AsmInstruction::Cmp { asm_type: AsmType::Quadword, lhs: AsmOperand::Register(AsmRegister::R10), rhs: rhs.clone() },
                    ]);
                }

                AsmInstruction::Cmp { asm_type: _, lhs, rhs: AsmOperand::Imm(imm) } => {
                    instructions.extend(vec![
                        AsmInstruction::Mov { asm_type: AsmType::Quadword, src: AsmOperand::Imm(*imm), dst: AsmOperand::Register(AsmRegister::R11) },
                        AsmInstruction::Cmp { asm_type: AsmType::Quadword, lhs: lhs.clone(), rhs: AsmOperand::Register(AsmRegister::R11) },
                    ]);
                }

                AsmInstruction::Push(AsmOperand::Imm(n)) => {
                    if *n > i32::MAX as i64 || *n < i32::MIN as i64 {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: AsmOperand::Imm(*n),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Push(AsmOperand::Register(AsmRegister::R10)),
                        ]);
                    } else {
                        instructions.push(instr.clone());
                    }
                }
                AsmInstruction::Push(AsmOperand::Register(reg)) if is_xmm(reg) => instructions
                    .extend(vec![
                        AsmInstruction::Binary {
                            asm_type: AsmType::Quadword,
                            op: AsmBinaryOp::Sub,
                            lhs: AsmOperand::Imm(8),
                            rhs: AsmOperand::Register(AsmRegister::SP),
                        },
                        AsmInstruction::Mov {
                            asm_type: AsmType::Quadword,
                            src: AsmOperand::Register(*reg),
                            dst: AsmOperand::Memory(AsmRegister::SP, 0),
                        },
                    ]),

                AsmInstruction::Cvttsd2si { asm_type, src, dst } if matches!(dst, AsmOperand::Memory(_, _) | AsmOperand::Data(_, _)) => {
                    instructions.extend(vec![
                        AsmInstruction::Cvttsd2si { asm_type: *asm_type, src: src.clone(), dst: AsmOperand::Register(AsmRegister::R11) },
                        AsmInstruction::Mov { asm_type: *asm_type, src: AsmOperand::Register(AsmRegister::R11), dst: dst.clone() },
                    ]);
                }

                AsmInstruction::Cvtsi2sd { asm_type, src, dst } => {
                    if matches!(src, AsmOperand::Imm(_)) && is_memory(dst) {
                        instructions.extend(vec![
                            AsmInstruction::Mov { asm_type: *asm_type, src: src.clone(), dst: AsmOperand::Register(AsmRegister::R10) },
                            AsmInstruction::Cvtsi2sd { asm_type: *asm_type, src: AsmOperand::Register(AsmRegister::R10), dst: AsmOperand::Register(AsmRegister::XMM15) },
                            AsmInstruction::Mov { asm_type: AsmType::Double, src: AsmOperand::Register(AsmRegister::XMM15), dst: dst.clone() },
                        ]);
                    } else if matches!(src, AsmOperand::Imm(_)) {
                        instructions.extend(vec![
                            AsmInstruction::Mov { asm_type: *asm_type, src: src.clone(), dst: AsmOperand::Register(AsmRegister::R10) },
                            AsmInstruction::Cvtsi2sd { asm_type: *asm_type, src: AsmOperand::Register(AsmRegister::R10), dst: dst.clone() },
                        ]);
                    } else if is_memory(dst) {
                        instructions.extend(vec![
                            AsmInstruction::Cvtsi2sd { asm_type: *asm_type, src: src.clone(), dst: AsmOperand::Register(AsmRegister::XMM15) },
                            AsmInstruction::Mov { asm_type: AsmType::Double, src: AsmOperand::Register(AsmRegister::XMM15), dst: dst.clone() },
                        ]);
                    } else {
                        instructions.push(instr.clone());
                    }
                },

                AsmInstruction::Ret => {
                    let restore_regs: Vec<AsmInstruction> = callee_saved_args
                        .iter()
                        .rev()
                        .flat_map(|r| {
                            if !is_xmm(r) {
                                vec![AsmInstruction::Pop(*r)]
                            } else {
                                vec![
                                    AsmInstruction::Mov {
                                        asm_type: AsmType::Quadword,
                                        src: AsmOperand::Memory(AsmRegister::SP, 0),
                                        dst: AsmOperand::Register(*r),
                                    },
                                    AsmInstruction::Binary {
                                        asm_type: AsmType::Quadword,
                                        op: AsmBinaryOp::Add,
                                        lhs: AsmOperand::Imm(8),
                                        rhs: AsmOperand::Register(AsmRegister::SP),
                                    },
                                ]
                            }
                        })
                        .collect();

                    instructions.extend(restore_regs);
                    instructions.push(AsmInstruction::Ret);
                }
                _ => instructions.push(instr.clone()),
            }
        }

        AsmFunction {
            name: self.name.clone(),
            instructions,
            global: self.global,
            stack_space: self.stack_space,
        }
    }
}

fn is_memory(op: &AsmOperand) -> bool {
    matches!(
        op,
        AsmOperand::Memory(_, _)
            | AsmOperand::Data(_, _)
            | AsmOperand::Indexed(_, _, _)
    )
}

fn is_xmm(r: &AsmRegister) -> bool {
    matches!(
        r,
        AsmRegister::XMM0
            | AsmRegister::XMM1
            | AsmRegister::XMM2
            | AsmRegister::XMM3
            | AsmRegister::XMM4
            | AsmRegister::XMM5
            | AsmRegister::XMM6
            | AsmRegister::XMM7
            | AsmRegister::XMM8
            | AsmRegister::XMM9
            | AsmRegister::XMM10
            | AsmRegister::XMM11
            | AsmRegister::XMM12
            | AsmRegister::XMM13
            | AsmRegister::XMM14
            | AsmRegister::XMM15
    )
}

fn is_large(n: i64) -> bool {
    n > i32::MAX as i64 || n < i32::MIN as i64
}

fn is_larger_than_uint(imm: i64) -> bool {
    // use unsigned upper-bound for positives
    let max_i: i64 = 4294967295; // 2^32 - 1

    // use signed 32-bit lower bound for negatives
    let int32_min: i64 = i32::MIN as i64;

    imm > max_i || imm < int32_min
}

fn is_larger_than_byte(imm: i64) -> bool {
    imm >= 256 || imm < -128
}
