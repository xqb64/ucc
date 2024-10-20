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
            _ => unreachable!(),
        }
    }
}

impl Fixup for AsmProgram {
    fn fixup(&mut self, _callee_saved_args: &BTreeSet<AsmRegister>) -> AsmProgram {
        let mut functions = vec![];

        for func in &mut self.functions {
            let symbol = ASM_SYMBOL_TABLE
                .lock()
                .unwrap()
                .get(&func.name)
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

        /* We start by calculating how many bytes the callee-saved registers will occupy.
         * Then, we add bytes that the locals will occupy, and round this up to the nearest
         * multiple of 16.  Working back from this value, we subtract the number of bytes that
         * the callee-saved registers will occupy, and we come to the exact number by which we
         * need to adjust the stack to keep it 16-byte aligned as per SystemV ABI. 
         * 
         * For example, if the pseudo-operand replacement pass has allocated 20 bytes of stack
         * space to store the locals of a particular function.  Normally, we'd subtract 32 bytes
         * from %rsp to maintain the proper stack alignment.  However, if the function uses a single
         * callee-saved register, we should subtract 24 bytes instead.  Then, after we push the
         * callee-saved register to preserve its value, we'll subtract another 8 bytes, thereby
         * maintaining the proper stack alignment.*/
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
                rhs: AsmOperand::Register(AsmRegister::Sp),
            }
        }

        instructions_setup.push(emit_stack_adjustment(
            self.stack_space,
            callee_saved_args.len(),
        ));

        /* If a function uses any callee-saved registers, it must save their values by pushing them
         * on top of the current stack frame.  Later, before returning to the caller, it must pop them off
         * the stack. */
        for reg in callee_saved_args {
            instructions_setup.push(AsmInstruction::Push(AsmOperand::Register(*reg)));
        }

        /* Prepend 'self.instructions' with 'instructions_setup'. */
        self.instructions.splice(0..0, instructions_setup);

        for instr in &mut self.instructions {
            match instr {
                /* 'mov' can't move a value from one memory address to another */
                AsmInstruction::Mov { asm_type, src, dst }
                    if matches!(src, AsmOperand::Memory(_, _) | AsmOperand::Data(_, _,))
                        && matches!(dst, AsmOperand::Memory(_, _) | AsmOperand::Data(_, _,)) =>
                {
                    let scratch = if asm_type == &AsmType::Double {
                        AsmOperand::Register(AsmRegister::Xmm14)
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
                }

                /* 'mov' can't move a large constant to a memory address */
                AsmInstruction::Mov {
                    asm_type: AsmType::Quadword,
                    src: AsmOperand::Imm(imm),
                    dst,
                } if is_large(*imm)
                    && matches!(dst, AsmOperand::Memory(_, _,) | AsmOperand::Data(_, _,)) =>
                {
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
                }

                /* 'mov'-ing a quadword-size constant with a longword operand size produces an assembler warning */
                AsmInstruction::Mov {
                    asm_type: AsmType::Longword,
                    src: AsmOperand::Imm(imm),
                    dst,
                } if is_larger_than_uint(*imm) => {
                    let bitmask: i64 = 0xffffffff;
                    let reduced = *imm & bitmask;

                    instructions.extend(vec![AsmInstruction::Mov {
                        asm_type: AsmType::Longword,
                        src: AsmOperand::Imm(reduced),
                        dst: dst.clone(),
                    }]);
                }

                /* 'mov'-ing a longword-size constant with a byte operand size produces an assembler warning */
                AsmInstruction::Mov {
                    asm_type: AsmType::Byte,
                    src: AsmOperand::Imm(imm),
                    dst,
                } if is_larger_than_byte(*imm) => {
                    let bitmask: i64 = 0xff;
                    let reduced = *imm & bitmask;

                    instructions.extend(vec![AsmInstruction::Mov {
                        asm_type: AsmType::Byte,
                        src: AsmOperand::Imm(reduced),
                        dst: dst.clone(),
                    }]);
                }

                /* 'movsx' can't handle immediate source or memory dst */
                AsmInstruction::Movsx {
                    src_type,
                    src: AsmOperand::Imm(imm),
                    dst_type,
                    dst,
                } if matches!(dst, AsmOperand::Memory(_, _,) | AsmOperand::Data(_, _)) => {
                    instructions.extend(vec![
                        AsmInstruction::Mov {
                            asm_type: *src_type,
                            src: AsmOperand::Imm(*imm),
                            dst: AsmOperand::Register(AsmRegister::R10),
                        },
                        AsmInstruction::Movsx {
                            src_type: *src_type,
                            src: AsmOperand::Register(AsmRegister::R10),
                            dst_type: *dst_type,
                            dst: AsmOperand::Register(AsmRegister::R11),
                        },
                        AsmInstruction::Mov {
                            asm_type: *dst_type,
                            src: AsmOperand::Register(AsmRegister::R11),
                            dst: dst.clone(),
                        },
                    ]);
                }

                AsmInstruction::Movsx {
                    src_type,
                    src: AsmOperand::Imm(imm),
                    dst_type,
                    dst,
                } => {
                    instructions.extend(vec![
                        AsmInstruction::Mov {
                            asm_type: *src_type,
                            src: AsmOperand::Imm(*imm),
                            dst: AsmOperand::Register(AsmRegister::R10),
                        },
                        AsmInstruction::Movsx {
                            src_type: *src_type,
                            src: AsmOperand::Register(AsmRegister::R10),
                            dst_type: *dst_type,
                            dst: dst.clone(),
                        },
                    ]);
                }

                AsmInstruction::Movsx {
                    src_type,
                    src,
                    dst_type,
                    dst,
                } if matches!(dst, AsmOperand::Memory(_, _) | AsmOperand::Data(_, _)) => {
                    instructions.extend(vec![
                        AsmInstruction::Movsx {
                            src_type: *src_type,
                            src: src.clone(),
                            dst_type: *dst_type,
                            dst: AsmOperand::Register(AsmRegister::R11),
                        },
                        AsmInstruction::Mov {
                            asm_type: *dst_type,
                            src: AsmOperand::Register(AsmRegister::R11),
                            dst: dst.clone(),
                        },
                    ]);
                }

                AsmInstruction::MovZeroExtend {
                    src_type: AsmType::Byte,
                    src: AsmOperand::Imm(imm),
                    dst_type,
                    dst,
                } => {
                    /* 'movzeroextend' src can't be an immediate */
                    if is_memory(dst) {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Byte,
                                src: AsmOperand::Imm(*imm),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::MovZeroExtend {
                                src_type: AsmType::Byte,
                                src: AsmOperand::Register(AsmRegister::R10),
                                dst_type: *dst_type,
                                dst: AsmOperand::Register(AsmRegister::R11),
                            },
                            AsmInstruction::Mov {
                                asm_type: *dst_type,
                                src: AsmOperand::Register(AsmRegister::R11),
                                dst: dst.clone(),
                            },
                        ]);
                    } else {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Byte,
                                src: AsmOperand::Imm(*imm),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::MovZeroExtend {
                                src_type: AsmType::Byte,
                                src: AsmOperand::Register(AsmRegister::R10),
                                dst_type: *dst_type,
                                dst: dst.clone(),
                            },
                        ]);
                    }
                }

                /* 'movzeroextend' dst must be a register */
                AsmInstruction::MovZeroExtend {
                    src_type: AsmType::Byte,
                    src,
                    dst_type,
                    dst,
                } if is_memory(dst) => {
                    instructions.extend(vec![
                        AsmInstruction::MovZeroExtend {
                            src_type: AsmType::Byte,
                            src: src.clone(),
                            dst_type: *dst_type,
                            dst: AsmOperand::Register(AsmRegister::R11),
                        },
                        AsmInstruction::Mov {
                            asm_type: *dst_type,
                            src: AsmOperand::Register(AsmRegister::R11),
                            dst: dst.clone(),
                        },
                    ]);
                }

                AsmInstruction::MovZeroExtend {
                    src_type: AsmType::Longword,
                    src,
                    dst_type,
                    dst,
                } if is_memory(dst) => {
                    /* to zero-extend a longword to a quadword, we need to move the longword to a register, then move to destination */
                    instructions.extend(vec![
                        AsmInstruction::Mov {
                            asm_type: AsmType::Longword,
                            src: src.clone(),
                            dst: AsmOperand::Register(AsmRegister::R11),
                        },
                        AsmInstruction::Mov {
                            asm_type: *dst_type,
                            src: AsmOperand::Register(AsmRegister::R11),
                            dst: dst.clone(),
                        },
                    ]);
                }

                AsmInstruction::MovZeroExtend {
                    src_type: AsmType::Longword,
                    src,
                    dst_type: _,
                    dst,
                } => {
                    /* if destination is already a register, zero-extend with a single mov instruction */
                    instructions.extend(vec![AsmInstruction::Mov {
                        asm_type: AsmType::Longword,
                        src: src.clone(),
                        dst: dst.clone(),
                    }]);
                }

                /* Idiv can't operate on constants */
                AsmInstruction::Idiv {
                    asm_type,
                    operand: AsmOperand::Imm(imm),
                } => {
                    instructions.extend(vec![
                        AsmInstruction::Mov {
                            asm_type: *asm_type,
                            src: AsmOperand::Imm(*imm),
                            dst: AsmOperand::Register(AsmRegister::R10),
                        },
                        AsmInstruction::Idiv {
                            asm_type: *asm_type,
                            operand: AsmOperand::Register(AsmRegister::R10),
                        },
                    ]);
                }

                AsmInstruction::Div {
                    asm_type,
                    operand: AsmOperand::Imm(imm),
                } => {
                    instructions.extend(vec![
                        AsmInstruction::Mov {
                            asm_type: *asm_type,
                            src: AsmOperand::Imm(*imm),
                            dst: AsmOperand::Register(AsmRegister::R10),
                        },
                        AsmInstruction::Div {
                            asm_type: *asm_type,
                            operand: AsmOperand::Register(AsmRegister::R10),
                        },
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
                }

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
                            dst: AsmOperand::Register(AsmRegister::Xmm15),
                        },
                        AsmInstruction::Binary {
                            asm_type: AsmType::Double,
                            op: *op,
                            lhs: lhs.clone(),
                            rhs: AsmOperand::Register(AsmRegister::Xmm15),
                        },
                        AsmInstruction::Mov {
                            asm_type: AsmType::Double,
                            src: AsmOperand::Register(AsmRegister::Xmm15),
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
                ) && matches!(lhs, AsmOperand::Memory(_, _) | AsmOperand::Data(_, _))
                    && matches!(rhs, AsmOperand::Memory(_, _) | AsmOperand::Data(_, _)) =>
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
                        AsmOperand::Register(AsmRegister::Xmm15)
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
                            dst: AsmOperand::Register(AsmRegister::Xmm15),
                        },
                        AsmInstruction::Cmp {
                            asm_type: AsmType::Double,
                            lhs: lhs.clone(),
                            rhs: AsmOperand::Register(AsmRegister::Xmm15),
                        },
                    ]);
                }

                /* Not both operands of 'cmp' can be in memory */
                AsmInstruction::Cmp { asm_type, lhs, rhs }
                    if matches!(lhs, AsmOperand::Memory(_, _) | AsmOperand::Data(_, _))
                        && matches!(rhs, AsmOperand::Memory(_, _) | AsmOperand::Data(_, _)) =>
                {
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
                AsmInstruction::Cmp {
                    asm_type: AsmType::Quadword,
                    lhs: AsmOperand::Imm(imm1),
                    rhs: AsmOperand::Imm(imm2),
                } if is_large(*imm1) => {
                    instructions.extend(vec![
                        AsmInstruction::Mov {
                            asm_type: AsmType::Quadword,
                            src: AsmOperand::Imm(*imm1),
                            dst: AsmOperand::Register(AsmRegister::R10),
                        },
                        AsmInstruction::Mov {
                            asm_type: AsmType::Quadword,
                            src: AsmOperand::Imm(*imm2),
                            dst: AsmOperand::Register(AsmRegister::R11),
                        },
                        AsmInstruction::Cmp {
                            asm_type: AsmType::Quadword,
                            lhs: AsmOperand::Register(AsmRegister::R10),
                            rhs: AsmOperand::Register(AsmRegister::R11),
                        },
                    ]);
                }

                AsmInstruction::Cmp {
                    asm_type: AsmType::Quadword,
                    lhs: AsmOperand::Imm(imm1),
                    rhs,
                } => {
                    instructions.extend(vec![
                        AsmInstruction::Mov {
                            asm_type: AsmType::Quadword,
                            src: AsmOperand::Imm(*imm1),
                            dst: AsmOperand::Register(AsmRegister::R10),
                        },
                        AsmInstruction::Cmp {
                            asm_type: AsmType::Quadword,
                            lhs: AsmOperand::Register(AsmRegister::R10),
                            rhs: rhs.clone(),
                        },
                    ]);
                }

                AsmInstruction::Cmp {
                    asm_type: _,
                    lhs,
                    rhs: AsmOperand::Imm(imm),
                } => {
                    instructions.extend(vec![
                        AsmInstruction::Mov {
                            asm_type: AsmType::Quadword,
                            src: AsmOperand::Imm(*imm),
                            dst: AsmOperand::Register(AsmRegister::R11),
                        },
                        AsmInstruction::Cmp {
                            asm_type: AsmType::Quadword,
                            lhs: lhs.clone(),
                            rhs: AsmOperand::Register(AsmRegister::R11),
                        },
                    ]);
                }

                AsmInstruction::Push(AsmOperand::Imm(n)) if is_large(*n) => {
                    instructions.extend(vec![
                        AsmInstruction::Mov {
                            asm_type: AsmType::Quadword,
                            src: AsmOperand::Imm(*n),
                            dst: AsmOperand::Register(AsmRegister::R10),
                        },
                        AsmInstruction::Push(AsmOperand::Register(AsmRegister::R10)),
                    ]);
                }

                AsmInstruction::Push(AsmOperand::Register(reg)) if is_xmm(reg) => instructions
                    .extend(vec![
                        AsmInstruction::Binary {
                            asm_type: AsmType::Quadword,
                            op: AsmBinaryOp::Sub,
                            lhs: AsmOperand::Imm(8),
                            rhs: AsmOperand::Register(AsmRegister::Sp),
                        },
                        AsmInstruction::Mov {
                            asm_type: AsmType::Quadword,
                            src: AsmOperand::Register(*reg),
                            dst: AsmOperand::Memory(AsmRegister::Sp, 0),
                        },
                    ]),

                AsmInstruction::Cvttsd2si { asm_type, src, dst }
                    if matches!(dst, AsmOperand::Memory(_, _) | AsmOperand::Data(_, _)) =>
                {
                    instructions.extend(vec![
                        AsmInstruction::Cvttsd2si {
                            asm_type: *asm_type,
                            src: src.clone(),
                            dst: AsmOperand::Register(AsmRegister::R11),
                        },
                        AsmInstruction::Mov {
                            asm_type: *asm_type,
                            src: AsmOperand::Register(AsmRegister::R11),
                            dst: dst.clone(),
                        },
                    ]);
                }

                AsmInstruction::Cvtsi2sd { asm_type, src, dst } => {
                    if matches!(src, AsmOperand::Imm(_)) && is_memory(dst) {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: *asm_type,
                                src: src.clone(),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Cvtsi2sd {
                                asm_type: *asm_type,
                                src: AsmOperand::Register(AsmRegister::R10),
                                dst: AsmOperand::Register(AsmRegister::Xmm15),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Double,
                                src: AsmOperand::Register(AsmRegister::Xmm15),
                                dst: dst.clone(),
                            },
                        ]);
                    } else if matches!(src, AsmOperand::Imm(_)) {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: *asm_type,
                                src: src.clone(),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Cvtsi2sd {
                                asm_type: *asm_type,
                                src: AsmOperand::Register(AsmRegister::R10),
                                dst: dst.clone(),
                            },
                        ]);
                    } else if is_memory(dst) {
                        instructions.extend(vec![
                            AsmInstruction::Cvtsi2sd {
                                asm_type: *asm_type,
                                src: src.clone(),
                                dst: AsmOperand::Register(AsmRegister::Xmm15),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Double,
                                src: AsmOperand::Register(AsmRegister::Xmm15),
                                dst: dst.clone(),
                            },
                        ]);
                    } else {
                        instructions.push(instr.clone());
                    }
                }

                AsmInstruction::Ret => {
                    /* Before returning to the caller, any callee-saved registers must be popped off
                     * of the stack in reverse-order of how we pushed them.
                     * 
                     * Since 'pop' operates only on general purpose registers, for XMM registers we need
                     * to use a combination of mov+add. */
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
                                        src: AsmOperand::Memory(AsmRegister::Sp, 0),
                                        dst: AsmOperand::Register(*r),
                                    },
                                    AsmInstruction::Binary {
                                        asm_type: AsmType::Quadword,
                                        op: AsmBinaryOp::Add,
                                        lhs: AsmOperand::Imm(8),
                                        rhs: AsmOperand::Register(AsmRegister::Sp),
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
        AsmOperand::Memory(_, _) | AsmOperand::Data(_, _) | AsmOperand::Indexed(_, _, _)
    )
}

fn is_xmm(r: &AsmRegister) -> bool {
    matches!(
        r,
        AsmRegister::Xmm0
            | AsmRegister::Xmm1
            | AsmRegister::Xmm2
            | AsmRegister::Xmm3
            | AsmRegister::Xmm4
            | AsmRegister::Xmm5
            | AsmRegister::Xmm6
            | AsmRegister::Xmm7
            | AsmRegister::Xmm8
            | AsmRegister::Xmm9
            | AsmRegister::Xmm10
            | AsmRegister::Xmm11
            | AsmRegister::Xmm12
            | AsmRegister::Xmm13
            | AsmRegister::Xmm14
            | AsmRegister::Xmm15
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
    !(-128..256).contains(&imm)
}
