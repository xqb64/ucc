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
            AsmNode::Instructions(instrs) => AsmNode::Instructions(instrs.fixup(callee_saved_args)),
            AsmNode::Operand(op) => AsmNode::Operand(op.clone()),
            AsmNode::StaticVariable(static_var) => AsmNode::StaticVariable(static_var.clone()),
            AsmNode::StaticConstant(static_const) => AsmNode::StaticConstant(static_const.clone()),
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

        let save_reg = |r: &AsmRegister| AsmInstruction::Push(AsmOperand::Register(*r));

        for reg in callee_saved_args {
            instructions_setup.push(save_reg(reg));
        }

        self.instructions.splice(0..0, instructions_setup);

        for instr in &mut self.instructions {
            match instr {
                AsmInstruction::Pop(reg) if is_xmm(reg) => {
                    instructions.push(AsmInstruction::Binary {
                        asm_type: AsmType::Quadword,
                        op: AsmBinaryOp::Add,
                        lhs: AsmOperand::Imm(8),
                        rhs: AsmOperand::Register(AsmRegister::SP),
                    });
                }
                AsmInstruction::Lea { src, dst } => match dst {
                    AsmOperand::Memory(AsmRegister::BP, _)
                    | AsmOperand::Memory(_, _)
                    | AsmOperand::Data(_, _) => {
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
                    _ => instructions.push(instr.clone()),
                },
                AsmInstruction::Mov {
                    asm_type: AsmType::Double,
                    src,
                    dst,
                } => match (&src, &dst) {
                    (AsmOperand::Data(_, _), AsmOperand::Data(_, _)) => {
                        println!("Data, Data");
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Double,
                                src: src.clone(),
                                dst: AsmOperand::Register(AsmRegister::XMM14),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Double,
                                src: AsmOperand::Register(AsmRegister::XMM14),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    (AsmOperand::Memory(_, _), AsmOperand::Data(_, _)) => {
                        println!("Memory, Data");
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Double,
                                src: src.clone(),
                                dst: AsmOperand::Register(AsmRegister::XMM14),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Double,
                                src: AsmOperand::Register(AsmRegister::XMM14),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    (AsmOperand::Data(_, _), AsmOperand::Memory(_, _)) => {
                        println!("Data, Memory");
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Double,
                                src: src.clone(),
                                dst: AsmOperand::Register(AsmRegister::XMM14),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Double,
                                src: AsmOperand::Register(AsmRegister::XMM14),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    (AsmOperand::Memory(_, _), AsmOperand::Memory(_, _)) => {
                        println!("Memory, Memory");
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Double,
                                src: src.clone(),
                                dst: AsmOperand::Register(AsmRegister::XMM14),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Double,
                                src: AsmOperand::Register(AsmRegister::XMM14),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    _ => {
                        println!("Cloning: {:?}", instr.clone());
                        instructions.push(instr.clone());
                    }
                },
                AsmInstruction::Mov {
                    asm_type: AsmType::Quadword,
                    src,
                    dst,
                } => match (&src, &dst) {
                    (AsmOperand::Imm(_), AsmOperand::Memory(_, _)) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: src.clone(),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: AsmOperand::Register(AsmRegister::R10),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    (AsmOperand::Imm(_), AsmOperand::Data(_, _)) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: src.clone(),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: AsmOperand::Register(AsmRegister::R10),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    (AsmOperand::Memory(_, _), AsmOperand::Memory(_, _)) => {
                        let scratch = AsmOperand::Register(AsmRegister::R10);

                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: src.clone(),
                                dst: scratch.clone(),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: scratch.clone(),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    (AsmOperand::Data(_, _), AsmOperand::Memory(_, _)) => {
                        let scratch = AsmOperand::Register(AsmRegister::R10);

                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: src.clone(),
                                dst: scratch.clone(),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: scratch.clone(),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    (AsmOperand::Memory(_, _), AsmOperand::Data(_, _)) => {
                        let scratch = AsmOperand::Register(AsmRegister::R10);

                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: src.clone(),
                                dst: scratch.clone(),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: scratch.clone(),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    (AsmOperand::Data(_, _), AsmOperand::Data(_, _)) => {
                        let scratch = AsmOperand::Register(AsmRegister::R10);

                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: src.clone(),
                                dst: scratch.clone(),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: scratch.clone(),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    _ => instructions.push(instr.clone()),
                },
                AsmInstruction::Mov {
                    asm_type: AsmType::Longword,
                    src,
                    dst,
                } => match (&src, &dst) {
                    (AsmOperand::Imm(imm), AsmOperand::Memory(_, _)) => {
                        if *imm < i32::MIN as i64 || *imm > i32::MAX as i64 {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: AsmType::Longword,
                                    src: AsmOperand::Imm(*imm as i32 as i64),
                                    dst: AsmOperand::Register(AsmRegister::R10),
                                },
                                AsmInstruction::Mov {
                                    asm_type: AsmType::Longword,
                                    src: AsmOperand::Register(AsmRegister::R10),
                                    dst: dst.clone(),
                                },
                            ]);
                        } else {
                            instructions.push(instr.clone());
                        }
                    }
                    (AsmOperand::Imm(_), AsmOperand::Data(_, _)) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Longword,
                                src: src.clone(),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Longword,
                                src: AsmOperand::Register(AsmRegister::R10),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    (AsmOperand::Memory(_, _), AsmOperand::Memory(_, _)) => {
                        let scratch = AsmOperand::Register(AsmRegister::R10);

                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Longword,
                                src: src.clone(),
                                dst: scratch.clone(),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Longword,
                                src: scratch.clone(),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    (AsmOperand::Data(_, _), AsmOperand::Memory(_, _)) => {
                        let scratch = AsmOperand::Register(AsmRegister::R10);

                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Longword,
                                src: src.clone(),
                                dst: scratch.clone(),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Longword,
                                src: scratch.clone(),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    (AsmOperand::Memory(_, _), AsmOperand::Data(_, _)) => {
                        let scratch = AsmOperand::Register(AsmRegister::R10);

                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Longword,
                                src: src.clone(),
                                dst: scratch.clone(),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Longword,
                                src: scratch.clone(),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    (AsmOperand::Data(_, _), AsmOperand::Data(_, _)) => {
                        let scratch = AsmOperand::Register(AsmRegister::R10);

                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Longword,
                                src: src.clone(),
                                dst: scratch.clone(),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Longword,
                                src: scratch.clone(),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    _ => instructions.push(instr.clone()),
                },
                AsmInstruction::Mov {
                    asm_type: AsmType::Byte,
                    src,
                    dst,
                } => match (&src, &dst) {
                    (AsmOperand::Imm(imm), AsmOperand::Memory(_, _)) => {
                        if *imm < i8::MIN as i64 || *imm > i8::MAX as i64 {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: AsmType::Byte,
                                    src: AsmOperand::Imm(*imm as i8 as i64),
                                    dst: AsmOperand::Register(AsmRegister::R10),
                                },
                                AsmInstruction::Mov {
                                    asm_type: AsmType::Byte,
                                    src: AsmOperand::Register(AsmRegister::R10),
                                    dst: dst.clone(),
                                },
                            ]);
                        } else {
                            instructions.push(instr.clone());
                        }
                    }
                    (AsmOperand::Imm(_), AsmOperand::Data(_, _)) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Byte,
                                src: src.clone(),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Byte,
                                src: AsmOperand::Register(AsmRegister::R10),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    (AsmOperand::Memory(_, _), AsmOperand::Memory(_, _)) => {
                        let scratch = AsmOperand::Register(AsmRegister::R10);

                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Byte,
                                src: src.clone(),
                                dst: scratch.clone(),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Byte,
                                src: scratch.clone(),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    (AsmOperand::Data(_, _), AsmOperand::Memory(_, _)) => {
                        let scratch = AsmOperand::Register(AsmRegister::R10);

                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Byte,
                                src: src.clone(),
                                dst: scratch.clone(),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Byte,
                                src: scratch.clone(),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    (AsmOperand::Memory(_, _), AsmOperand::Data(_, _)) => {
                        let scratch = AsmOperand::Register(AsmRegister::R10);

                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Byte,
                                src: src.clone(),
                                dst: scratch.clone(),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Byte,
                                src: scratch.clone(),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    (AsmOperand::Data(_, _), AsmOperand::Data(_, _)) => {
                        let scratch = AsmOperand::Register(AsmRegister::R10);

                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Byte,
                                src: src.clone(),
                                dst: scratch.clone(),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Byte,
                                src: scratch.clone(),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    _ => instructions.push(instr.clone()),
                },
                AsmInstruction::Mov { asm_type, src, dst } => match (&src, &dst) {
                    (AsmOperand::Data(_, _), AsmOperand::Data(_, _)) => {
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
                    }
                    (AsmOperand::Data(_, _), AsmOperand::Memory(_, _)) => {
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
                    }
                    (AsmOperand::Memory(_, _), AsmOperand::Memory(_, _)) => {
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
                    }
                    _ => instructions.push(instr.clone()),
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
                            dst: AsmOperand::Register(AsmRegister::R10),
                        },
                        AsmInstruction::Binary {
                            asm_type: AsmType::Quadword,
                            op: *op,
                            lhs: lhs.clone(),
                            rhs: AsmOperand::Register(AsmRegister::R10),
                        },
                    ]);
                }
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
                AsmInstruction::Binary {
                    op,
                    lhs,
                    rhs,
                    asm_type,
                } => match op {
                    AsmBinaryOp::Add | AsmBinaryOp::Sub => match (&lhs, &rhs) {
                        (
                            AsmOperand::Memory(AsmRegister::BP, src_n),
                            AsmOperand::Memory(AsmRegister::BP, dst_n),
                        ) => match asm_type {
                            AsmType::Byte | AsmType::Longword | AsmType::Quadword => {
                                instructions.extend(vec![
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: AsmOperand::Memory(AsmRegister::BP, *src_n),
                                        dst: AsmOperand::Register(AsmRegister::R10),
                                    },
                                    AsmInstruction::Binary {
                                        asm_type: *asm_type,
                                        op: *op,
                                        lhs: AsmOperand::Register(AsmRegister::R10),
                                        rhs: AsmOperand::Memory(AsmRegister::BP, *dst_n),
                                    },
                                ]);
                            }
                            AsmType::Double => {
                                instructions.extend(vec![
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: rhs.clone(),
                                        dst: AsmOperand::Register(AsmRegister::XMM15),
                                    },
                                    AsmInstruction::Binary {
                                        asm_type: *asm_type,
                                        op: *op,
                                        lhs: lhs.clone(),
                                        rhs: AsmOperand::Register(AsmRegister::XMM15),
                                    },
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: AsmOperand::Register(AsmRegister::XMM15),
                                        dst: rhs.clone(),
                                    },
                                ]);
                            }
                            _ => instructions.push(instr.clone()),
                        },
                        (AsmOperand::Data(src, _), AsmOperand::Memory(AsmRegister::BP, dst_n)) => {
                            if asm_type == &AsmType::Double {
                                instructions.extend(vec![
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: rhs.clone(),
                                        dst: AsmOperand::Register(AsmRegister::XMM15),
                                    },
                                    AsmInstruction::Binary {
                                        asm_type: *asm_type,
                                        op: *op,
                                        lhs: lhs.clone(),
                                        rhs: AsmOperand::Register(AsmRegister::XMM15),
                                    },
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: AsmOperand::Register(AsmRegister::XMM15),
                                        dst: rhs.clone(),
                                    },
                                ]);
                            } else {
                                instructions.extend(vec![
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: AsmOperand::Data(src.clone(), 0),
                                        dst: AsmOperand::Register(AsmRegister::R10),
                                    },
                                    AsmInstruction::Binary {
                                        asm_type: *asm_type,
                                        op: *op,
                                        lhs: AsmOperand::Register(AsmRegister::R10),
                                        rhs: AsmOperand::Memory(AsmRegister::BP, *dst_n),
                                    },
                                ]);
                            }
                        }
                        (AsmOperand::Data(src, offset1), AsmOperand::Data(dst, offset2)) => {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Data(src.clone(), *offset1),
                                    dst: AsmOperand::Register(AsmRegister::R10),
                                },
                                AsmInstruction::Binary {
                                    asm_type: *asm_type,
                                    op: *op,
                                    lhs: AsmOperand::Register(AsmRegister::R10),
                                    rhs: AsmOperand::Data(dst.clone(), *offset2),
                                },
                            ]);
                        }
                        (AsmOperand::Imm(konst), AsmOperand::Memory(AsmRegister::BP, dst_n)) => {
                            if *konst < i32::MIN as i64 || *konst > i32::MAX as i64 {
                                instructions.extend(vec![
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: AsmOperand::Imm(*konst),
                                        dst: AsmOperand::Register(AsmRegister::R10),
                                    },
                                    AsmInstruction::Binary {
                                        asm_type: *asm_type,
                                        op: *op,
                                        lhs: AsmOperand::Register(AsmRegister::R10),
                                        rhs: AsmOperand::Memory(AsmRegister::BP, *dst_n),
                                    },
                                ])
                            } else {
                                instructions.push(instr.clone());
                            }
                        }
                        (AsmOperand::Memory(AsmRegister::BP, dst_n), AsmOperand::Imm(konst)) => {
                            if *konst < i32::MIN as i64 || *konst > i32::MAX as i64 {
                                instructions.extend(vec![
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: AsmOperand::Imm(*konst),
                                        dst: AsmOperand::Register(AsmRegister::R11),
                                    },
                                    AsmInstruction::Binary {
                                        asm_type: *asm_type,
                                        op: *op,
                                        lhs: AsmOperand::Register(AsmRegister::R11),
                                        rhs: AsmOperand::Memory(AsmRegister::BP, *dst_n),
                                    },
                                ])
                            } else {
                                instructions.push(instr.clone());
                            }
                        }
                        _ => instructions.push(instr.clone()),
                    },
                    AsmBinaryOp::Mul => match (&lhs, &rhs) {
                        (
                            AsmOperand::Memory(AsmRegister::BP, _),
                            AsmOperand::Memory(AsmRegister::BP, _),
                        ) => match asm_type {
                            AsmType::Byte | AsmType::Longword | AsmType::Quadword => {
                                instructions.extend(vec![
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: lhs.clone(),
                                        dst: AsmOperand::Register(AsmRegister::R10),
                                    },
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: rhs.clone(),
                                        dst: AsmOperand::Register(AsmRegister::R11),
                                    },
                                    AsmInstruction::Imul {
                                        asm_type: *asm_type,
                                        src: AsmOperand::Register(AsmRegister::R10),
                                        dst: AsmOperand::Register(AsmRegister::R11),
                                    },
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: AsmOperand::Register(AsmRegister::R11),
                                        dst: rhs.clone(),
                                    },
                                ]);
                            }
                            AsmType::Double => {
                                instructions.extend(vec![
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: rhs.clone(),
                                        dst: AsmOperand::Register(AsmRegister::XMM15),
                                    },
                                    AsmInstruction::Imul {
                                        asm_type: AsmType::Double,
                                        src: lhs.clone(),
                                        dst: AsmOperand::Register(AsmRegister::XMM15),
                                    },
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: AsmOperand::Register(AsmRegister::XMM15),
                                        dst: rhs.clone(),
                                    },
                                ]);
                            }
                            _ => todo!(),
                        },
                        (AsmOperand::Data(_, _), AsmOperand::Memory(AsmRegister::BP, _)) => {
                            if asm_type == &AsmType::Double {
                                instructions.extend(vec![
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: rhs.clone(),
                                        dst: AsmOperand::Register(AsmRegister::XMM15),
                                    },
                                    AsmInstruction::Imul {
                                        asm_type: *asm_type,
                                        src: lhs.clone(),
                                        dst: AsmOperand::Register(AsmRegister::XMM15),
                                    },
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: AsmOperand::Register(AsmRegister::XMM15),
                                        dst: rhs.clone(),
                                    },
                                ])
                            } else {
                                instructions.extend(vec![
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: lhs.clone(),
                                        dst: AsmOperand::Register(AsmRegister::R10),
                                    },
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: rhs.clone(),
                                        dst: AsmOperand::Register(AsmRegister::R11),
                                    },
                                    AsmInstruction::Imul {
                                        asm_type: *asm_type,
                                        src: AsmOperand::Register(AsmRegister::R10),
                                        dst: AsmOperand::Register(AsmRegister::R11),
                                    },
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: AsmOperand::Register(AsmRegister::R11),
                                        dst: rhs.clone(),
                                    },
                                ]);
                            }
                        }
                        (AsmOperand::Imm(konst), AsmOperand::Memory(AsmRegister::BP, _)) => {
                            if *konst < i32::MIN as i64 || *konst > i32::MAX as i64 {
                                instructions.extend(vec![
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: lhs.clone(),
                                        dst: AsmOperand::Register(AsmRegister::R10),
                                    },
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: rhs.clone(),
                                        dst: AsmOperand::Register(AsmRegister::R11),
                                    },
                                    AsmInstruction::Imul {
                                        asm_type: *asm_type,
                                        src: AsmOperand::Register(AsmRegister::R10),
                                        dst: AsmOperand::Register(AsmRegister::R11),
                                    },
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: AsmOperand::Register(AsmRegister::R11),
                                        dst: rhs.clone(),
                                    },
                                ]);
                            } else {
                                instructions.extend(vec![
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: rhs.clone(),
                                        dst: AsmOperand::Register(AsmRegister::R11),
                                    },
                                    AsmInstruction::Imul {
                                        asm_type: *asm_type,
                                        src: lhs.clone(),
                                        dst: AsmOperand::Register(AsmRegister::R11),
                                    },
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: AsmOperand::Register(AsmRegister::R11),
                                        dst: rhs.clone(),
                                    },
                                ]);
                            }
                        }
                        _ => {
                            instructions.push(instr.clone());
                        }
                    },
                    AsmBinaryOp::Xor => match (&lhs, &rhs) {
                        (AsmOperand::Data(_, _), AsmOperand::Memory(AsmRegister::BP, _)) => {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: rhs.clone(),
                                    dst: AsmOperand::Register(AsmRegister::XMM15),
                                },
                                AsmInstruction::Binary {
                                    asm_type: *asm_type,
                                    op: *op,
                                    lhs: lhs.clone(),
                                    rhs: AsmOperand::Register(AsmRegister::XMM15),
                                },
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Register(AsmRegister::XMM15),
                                    dst: rhs.clone(),
                                },
                            ]);
                        }
                        _ => instructions.push(instr.clone()),
                    },
                    AsmBinaryOp::DivDouble => match (&lhs, &rhs) {
                        (
                            AsmOperand::Memory(AsmRegister::BP, _),
                            AsmOperand::Memory(AsmRegister::BP, _),
                        ) => {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: rhs.clone(),
                                    dst: AsmOperand::Register(AsmRegister::XMM15),
                                },
                                AsmInstruction::Binary {
                                    asm_type: *asm_type,
                                    op: *op,
                                    lhs: lhs.clone(),
                                    rhs: AsmOperand::Register(AsmRegister::XMM15),
                                },
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Register(AsmRegister::XMM15),
                                    dst: rhs.clone(),
                                },
                            ]);
                        }
                        (AsmOperand::Data(_, _), AsmOperand::Memory(AsmRegister::BP, _)) => {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: rhs.clone(),
                                    dst: AsmOperand::Register(AsmRegister::XMM15),
                                },
                                AsmInstruction::Binary {
                                    asm_type: *asm_type,
                                    op: *op,
                                    lhs: lhs.clone(),
                                    rhs: AsmOperand::Register(AsmRegister::XMM15),
                                },
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Register(AsmRegister::XMM15),
                                    dst: rhs.clone(),
                                },
                            ]);
                        }
                        _ => instructions.push(instr.clone()),
                    },
                    _ => {
                        instructions.push(instr.clone());
                    }
                },
                AsmInstruction::Idiv { operand, asm_type } => {
                    if let AsmOperand::Imm(konst) = operand {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: *asm_type,
                                src: AsmOperand::Imm(*konst),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Idiv {
                                asm_type: *asm_type,
                                operand: AsmOperand::Register(AsmRegister::R10),
                            },
                        ]);
                    } else {
                        instructions.push(instr.clone());
                    }
                }
                AsmInstruction::Div {
                    operand,
                    asm_type: _,
                } => {
                    if let AsmOperand::Imm(konst) = operand {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: AsmOperand::Imm(*konst),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Div {
                                asm_type: AsmType::Quadword,
                                operand: AsmOperand::Register(AsmRegister::R10),
                            },
                        ]);
                    } else {
                        instructions.push(instr.clone());
                    }
                }
                AsmInstruction::MovZeroExtend {
                    src_type: AsmType::Byte,
                    src: AsmOperand::Imm(imm),
                    dst_type,
                    dst,
                } => match dst {
                    AsmOperand::Memory(_, _)
                    | AsmOperand::Data(_, _)
                    | AsmOperand::Indexed(_, _, _) => {
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
                    }
                    _ => {
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
                },
                AsmInstruction::MovZeroExtend {
                    src_type: AsmType::Byte,
                    src,
                    dst_type,
                    dst,
                } => match dst {
                    AsmOperand::Memory(_, _)
                    | AsmOperand::Data(_, _)
                    | AsmOperand::Indexed(_, _, _) => {
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
                    _ => {
                        instructions.push(instr.clone());
                    }
                },
                AsmInstruction::MovZeroExtend {
                    src_type: AsmType::Longword,
                    src,
                    dst_type,
                    dst,
                } => {
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
                AsmInstruction::Cmp { lhs, rhs, asm_type } => match (&lhs, &rhs) {
                    (
                        AsmOperand::Memory(AsmRegister::BP, src_n),
                        AsmOperand::Memory(AsmRegister::BP, dst_n),
                    ) => {
                        if asm_type == &AsmType::Double {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: rhs.clone(),
                                    dst: AsmOperand::Register(AsmRegister::XMM15),
                                },
                                AsmInstruction::Cmp {
                                    asm_type: *asm_type,
                                    lhs: lhs.clone(),
                                    rhs: AsmOperand::Register(AsmRegister::XMM15),
                                },
                            ]);
                        } else {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Memory(AsmRegister::BP, *src_n),
                                    dst: AsmOperand::Register(AsmRegister::R10),
                                },
                                AsmInstruction::Cmp {
                                    asm_type: *asm_type,
                                    lhs: AsmOperand::Register(AsmRegister::R10),
                                    rhs: AsmOperand::Memory(AsmRegister::BP, *dst_n),
                                },
                            ]);
                        }
                    }
                    (AsmOperand::Data(src, offset), AsmOperand::Memory(AsmRegister::BP, dst_n)) => {
                        if asm_type == &AsmType::Double {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: rhs.clone(),
                                    dst: AsmOperand::Register(AsmRegister::XMM15),
                                },
                                AsmInstruction::Cmp {
                                    asm_type: *asm_type,
                                    lhs: lhs.clone(),
                                    rhs: AsmOperand::Register(AsmRegister::XMM15),
                                },
                            ]);
                        } else {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Data(src.clone(), *offset),
                                    dst: AsmOperand::Register(AsmRegister::R10),
                                },
                                AsmInstruction::Cmp {
                                    asm_type: *asm_type,
                                    lhs: AsmOperand::Register(AsmRegister::R10),
                                    rhs: AsmOperand::Memory(AsmRegister::BP, *dst_n),
                                },
                            ]);
                        }
                    }
                    (AsmOperand::Memory(AsmRegister::BP, src_n), AsmOperand::Data(dst, offset)) => {
                        if asm_type == &AsmType::Double {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: rhs.clone(),
                                    dst: AsmOperand::Register(AsmRegister::XMM15),
                                },
                                AsmInstruction::Cmp {
                                    asm_type: *asm_type,
                                    lhs: lhs.clone(),
                                    rhs: AsmOperand::Register(AsmRegister::XMM15),
                                },
                            ]);
                        } else {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Memory(AsmRegister::BP, *src_n),
                                    dst: AsmOperand::Register(AsmRegister::R10),
                                },
                                AsmInstruction::Cmp {
                                    asm_type: *asm_type,
                                    lhs: AsmOperand::Register(AsmRegister::R10),
                                    rhs: AsmOperand::Data(dst.clone(), *offset),
                                },
                            ]);
                        }
                    }
                    (AsmOperand::Data(src, offset1), AsmOperand::Data(dst, offset2)) => {
                        match asm_type {
                            AsmType::Longword | AsmType::Quadword => {
                                instructions.extend(vec![
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: AsmOperand::Data(src.clone(), *offset1),
                                        dst: AsmOperand::Register(AsmRegister::R10),
                                    },
                                    AsmInstruction::Cmp {
                                        asm_type: *asm_type,
                                        lhs: AsmOperand::Register(AsmRegister::R10),
                                        rhs: AsmOperand::Data(dst.clone(), *offset2),
                                    },
                                ]);
                            }
                            AsmType::Double => {
                                instructions.extend(vec![
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: rhs.clone(),
                                        dst: AsmOperand::Register(AsmRegister::XMM15),
                                    },
                                    AsmInstruction::Cmp {
                                        asm_type: *asm_type,
                                        lhs: lhs.clone(),
                                        rhs: AsmOperand::Register(AsmRegister::XMM15),
                                    },
                                ]);
                            }
                            _ => todo!(),
                        }
                    }
                    (AsmOperand::Imm(konst1), AsmOperand::Imm(konst2)) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: *asm_type,
                                src: AsmOperand::Imm(*konst1),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Mov {
                                asm_type: *asm_type,
                                src: AsmOperand::Imm(*konst2),
                                dst: AsmOperand::Register(AsmRegister::R11),
                            },
                            AsmInstruction::Cmp {
                                asm_type: *asm_type,
                                lhs: AsmOperand::Register(AsmRegister::R10),
                                rhs: AsmOperand::Register(AsmRegister::R11),
                            },
                        ]);
                    }
                    (AsmOperand::Imm(konst), _) => {
                        if *konst > i32::MAX as i64 || *konst < i32::MIN as i64 {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Imm(*konst),
                                    dst: AsmOperand::Register(AsmRegister::R10),
                                },
                                AsmInstruction::Cmp {
                                    asm_type: *asm_type,
                                    lhs: AsmOperand::Register(AsmRegister::R10),
                                    rhs: rhs.clone(),
                                },
                            ]);
                        } else {
                            instructions.push(instr.clone());
                        }
                    }
                    (_, AsmOperand::Imm(konst)) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: *asm_type,
                                src: AsmOperand::Imm(*konst),
                                dst: AsmOperand::Register(AsmRegister::R11),
                            },
                            AsmInstruction::Cmp {
                                asm_type: *asm_type,
                                lhs: lhs.clone(),
                                rhs: AsmOperand::Register(AsmRegister::R11),
                            },
                        ]);
                    }
                    _ => instructions.push(instr.clone()),
                },
                AsmInstruction::Movsx {
                    src_type,
                    src,
                    dst_type,
                    dst,
                } => match (&src, &dst) {
                    (AsmOperand::Imm(konst), AsmOperand::Memory(AsmRegister::BP, dst_n)) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Longword,
                                src: AsmOperand::Imm(*konst),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Movsx {
                                src_type: *src_type,
                                src: AsmOperand::Register(AsmRegister::R10),
                                dst_type: *dst_type,
                                dst: AsmOperand::Register(AsmRegister::R11),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: AsmOperand::Register(AsmRegister::R11),
                                dst: AsmOperand::Memory(AsmRegister::BP, *dst_n),
                            },
                        ]);
                    }
                    (_, AsmOperand::Memory(AsmRegister::BP, _dst_n)) => {
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
                    _ => instructions.push(instr.clone()),
                },
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
                AsmInstruction::Cvttsd2si { asm_type, src, dst } => match (&src, &dst, *asm_type) {
                    (
                        AsmOperand::Memory(AsmRegister::BP, _),
                        AsmOperand::Memory(AsmRegister::BP, _),
                        AsmType::Double,
                    ) => {
                        instructions.extend(vec![
                            AsmInstruction::Cvttsd2si {
                                asm_type: *asm_type,
                                src: src.clone(),
                                dst: AsmOperand::Register(AsmRegister::XMM15),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Double,
                                src: AsmOperand::Register(AsmRegister::XMM15),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    (
                        AsmOperand::Memory(AsmRegister::BP, _),
                        AsmOperand::Memory(AsmRegister::BP, _),
                        AsmType::Quadword,
                    ) => {
                        instructions.extend(vec![
                            AsmInstruction::Cvttsd2si {
                                asm_type: *asm_type,
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
                    (
                        AsmOperand::Memory(AsmRegister::BP, _),
                        AsmOperand::Memory(AsmRegister::BP, _),
                        AsmType::Longword,
                    ) => {
                        instructions.extend(vec![
                            AsmInstruction::Cvttsd2si {
                                asm_type: *asm_type,
                                src: src.clone(),
                                dst: AsmOperand::Register(AsmRegister::R11),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Longword,
                                src: AsmOperand::Register(AsmRegister::R11),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    (AsmOperand::Data(_, _), AsmOperand::Memory(AsmRegister::BP, _), _) => {
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
                    (AsmOperand::Imm(_), _, _) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Longword,
                                src: src.clone(),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Cvttsd2si {
                                asm_type: *asm_type,
                                src: AsmOperand::Register(AsmRegister::R10),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    (AsmOperand::Register(_), AsmOperand::Memory(AsmRegister::BP, _), _) => {
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
                    _ => instructions.push(instr.clone()),
                },
                AsmInstruction::Cvtsi2sd { asm_type, src, dst } => match (&src, &dst, *asm_type) {
                    (
                        AsmOperand::Memory(AsmRegister::BP, _),
                        AsmOperand::Memory(AsmRegister::BP, _),
                        AsmType::Double,
                    ) => {
                        instructions.extend(vec![
                            AsmInstruction::Cvtsi2sd {
                                asm_type: *asm_type,
                                src: src.clone(),
                                dst: AsmOperand::Register(AsmRegister::XMM15),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Double,
                                src: AsmOperand::Register(AsmRegister::XMM15),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    (AsmOperand::Imm(_), _, _) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: *asm_type,
                                src: src.clone(),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Cvtsi2sd {
                                asm_type: *asm_type,
                                src: AsmOperand::Register(AsmRegister::R10),
                                dst: AsmOperand::Register(AsmRegister::XMM15),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Double,
                                src: AsmOperand::Register(AsmRegister::XMM15),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    (_, AsmOperand::Memory(AsmRegister::BP, _), _) => {
                        instructions.extend(vec![
                            AsmInstruction::Cvtsi2sd {
                                asm_type: *asm_type,
                                src: src.clone(),
                                dst: AsmOperand::Register(AsmRegister::XMM15),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Double,
                                src: AsmOperand::Register(AsmRegister::XMM15),
                                dst: dst.clone(),
                            },
                        ]);
                    }
                    _ => instructions.push(instr.clone()),
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

impl Fixup for Vec<AsmInstruction> {
    fn fixup(&mut self, _callee_saved_args: &BTreeSet<AsmRegister>) -> Vec<AsmInstruction> {
        let mut instructions = vec![];

        for instr in self {
            match instr {
                AsmInstruction::Mov { src, dst, asm_type } => match (src, dst) {
                    (
                        AsmOperand::Memory(AsmRegister::BP, src_n),
                        AsmOperand::Memory(AsmRegister::BP, dst_n),
                    ) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: *asm_type,
                                src: AsmOperand::Memory(AsmRegister::BP, *src_n),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Mov {
                                asm_type: *asm_type,
                                src: AsmOperand::Register(AsmRegister::R10),
                                dst: AsmOperand::Memory(AsmRegister::BP, *dst_n),
                            },
                        ]);
                    }
                    _ => instructions.push(instr.clone()),
                },
                _ => instructions.push(instr.clone()),
            }
        }

        instructions
    }
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
