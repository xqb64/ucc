use crate::codegen::gen::STATIC_CONSTANTS;
use crate::codegen::gen::{
    AsmFunction, AsmInstruction, AsmNode, AsmOperand, AsmProgram, AsmRegister, AsmStaticConstant,
    AsmStaticVariable, AsmSymtabEntry, ASM_SYMBOL_TABLE, VAR_TO_STACK_POS,
};
use crate::typechecker::IdentifierAttrs;
use crate::typechecker::SYMBOL_TABLE;

pub trait ReplacePseudo {
    fn replace_pseudo(&self) -> Self;
}

impl ReplacePseudo for AsmNode {
    fn replace_pseudo(&self) -> Self {
        match self {
            AsmNode::Program(prog) => AsmNode::Program(prog.replace_pseudo()),
            AsmNode::Function(func) => AsmNode::Function(func.replace_pseudo()),
            AsmNode::Operand(op) => AsmNode::Operand(op.replace_pseudo()),
            AsmNode::Instructions(instrs) => {
                AsmNode::Instructions(instrs.to_owned().replace_pseudo())
            }
            AsmNode::StaticVariable(static_var) => {
                AsmNode::StaticVariable(static_var.to_owned().replace_pseudo())
            }
            AsmNode::StaticConstant(static_const) => {
                AsmNode::StaticConstant(static_const.to_owned().replace_pseudo())
            }
        }
    }
}

impl ReplacePseudo for AsmStaticVariable {
    fn replace_pseudo(&self) -> Self {
        self.clone()
    }
}

impl ReplacePseudo for AsmStaticConstant {
    fn replace_pseudo(&self) -> Self {
        self.clone()
    }
}

impl ReplacePseudo for Vec<AsmInstruction> {
    fn replace_pseudo(&self) -> Self {
        let mut instructions = vec![];
        for instr in self {
            instructions.push(instr.replace_pseudo());
        }
        instructions
    }
}

impl ReplacePseudo for AsmInstruction {
    fn replace_pseudo(&self) -> Self {
        match self {
            AsmInstruction::Mov { src, dst, asm_type } => AsmInstruction::Mov {
                asm_type: *asm_type,
                src: src.replace_pseudo(),
                dst: dst.replace_pseudo(),
            },
            AsmInstruction::Unary {
                op,
                operand,
                asm_type,
            } => AsmInstruction::Unary {
                asm_type: *asm_type,
                op: *op,
                operand: operand.replace_pseudo(),
            },
            AsmInstruction::Binary {
                op,
                lhs,
                rhs,
                asm_type,
            } => AsmInstruction::Binary {
                asm_type: *asm_type,
                op: *op,
                lhs: lhs.replace_pseudo(),
                rhs: rhs.replace_pseudo(),
            },
            AsmInstruction::Idiv { operand, asm_type } => AsmInstruction::Idiv {
                asm_type: *asm_type,
                operand: operand.replace_pseudo(),
            },
            AsmInstruction::AllocateStack(n) => AsmInstruction::AllocateStack(*n),
            AsmInstruction::Ret => AsmInstruction::Ret,
            AsmInstruction::Cmp { lhs, rhs, asm_type } => AsmInstruction::Cmp {
                asm_type: *asm_type,
                lhs: lhs.replace_pseudo(),
                rhs: rhs.replace_pseudo(),
            },
            AsmInstruction::SetCC { condition, operand } => AsmInstruction::SetCC {
                condition: condition.clone(),
                operand: operand.replace_pseudo(),
            },
            AsmInstruction::Push(operand) => AsmInstruction::Push(operand.replace_pseudo()),
            AsmInstruction::Movsx {
                src_type,
                src,
                dst_type,
                dst,
            } => AsmInstruction::Movsx {
                src_type: *src_type,
                src: src.replace_pseudo(),
                dst_type: *dst_type,
                dst: dst.replace_pseudo(),
            },
            AsmInstruction::Div { operand, asm_type } => AsmInstruction::Div {
                asm_type: *asm_type,
                operand: operand.replace_pseudo(),
            },
            AsmInstruction::MovZeroExtend {
                src_type,
                src,
                dst_type,
                dst,
            } => AsmInstruction::MovZeroExtend {
                src_type: *src_type,
                src: src.replace_pseudo(),
                dst_type: *dst_type,
                dst: dst.replace_pseudo(),
            },
            AsmInstruction::Cvtsi2sd { asm_type, src, dst } => AsmInstruction::Cvtsi2sd {
                asm_type: *asm_type,
                src: src.replace_pseudo(),
                dst: dst.replace_pseudo(),
            },
            AsmInstruction::Cvttsd2si { asm_type, src, dst } => AsmInstruction::Cvttsd2si {
                asm_type: *asm_type,
                src: src.replace_pseudo(),
                dst: dst.replace_pseudo(),
            },
            AsmInstruction::Lea { src, dst } => AsmInstruction::Lea {
                src: src.replace_pseudo(),
                dst: dst.replace_pseudo(),
            },
            _ => self.clone(),
        }
    }
}

impl ReplacePseudo for AsmOperand {
    fn replace_pseudo(&self) -> Self {
        match self {
            AsmOperand::Pseudo(name) => {
                if let Some(symbol) = SYMBOL_TABLE.lock().unwrap().get(name).cloned() {
                    match symbol.attrs {
                        IdentifierAttrs::StaticAttr {
                            initial_value: _,
                            global: _,
                        } => AsmOperand::Data(name.clone(), 0),
                        _ => {
                            let _type = match ASM_SYMBOL_TABLE
                                .lock()
                                .unwrap()
                                .get(name)
                                .cloned()
                                .unwrap()
                            {
                                AsmSymtabEntry::Object {
                                    _type,
                                    is_static: _,
                                    is_constant: _,
                                } => _type,
                                _ => unreachable!(),
                            };

                            AsmOperand::Memory(
                                AsmRegister::BP,
                                VAR_TO_STACK_POS
                                    .lock()
                                    .unwrap()
                                    .var_to_stack_pos(name, _type)
                                    .0
                                    .try_into()
                                    .unwrap(),
                            )
                        }
                    }
                } else {
                    let _type = match ASM_SYMBOL_TABLE.lock().unwrap().get(name).cloned().unwrap() {
                        AsmSymtabEntry::Object {
                            _type,
                            is_static: _,
                            is_constant: _,
                        } => _type,
                        _ => unreachable!(),
                    };

                    AsmOperand::Memory(
                        AsmRegister::BP,
                        VAR_TO_STACK_POS
                            .lock()
                            .unwrap()
                            .var_to_stack_pos(name, _type)
                            .0
                            .try_into()
                            .unwrap(),
                    )
                }
            }
            AsmOperand::PseudoMem(name, offset) => {
                let (is_static, is_constant, _type) =
                    match ASM_SYMBOL_TABLE.lock().unwrap().get(name).cloned().unwrap() {
                        AsmSymtabEntry::Object {
                            _type,
                            is_static,
                            is_constant,
                        } => (is_static, is_constant, _type),
                        _ => unreachable!(),
                    };

                if !is_static && !is_constant {
                    let previously_assigned: isize = VAR_TO_STACK_POS
                        .lock()
                        .unwrap()
                        .var_to_stack_pos(name, _type)
                        .0
                        .try_into()
                        .unwrap();
                    AsmOperand::Memory(AsmRegister::BP, previously_assigned + *offset)
                } else {
                    AsmOperand::Data(name.clone(), *offset)
                }
            }
            _ => self.clone(),
        }
    }
}

impl ReplacePseudo for AsmProgram {
    fn replace_pseudo(&self) -> Self {
        let mut functions = vec![];
        for func in &self.functions {
            functions.push(func.replace_pseudo());
        }
        AsmProgram {
            functions,
            static_vars: self.static_vars.clone(),
            static_constants: STATIC_CONSTANTS.lock().unwrap().to_owned(),
        }
    }
}

impl ReplacePseudo for AsmFunction {
    fn replace_pseudo(&self) -> Self {
        if ASM_SYMBOL_TABLE
            .lock()
            .unwrap()
            .get(&self.name)
            .is_some_and(|f| match f {
                AsmSymtabEntry::Function {
                    returns_on_stack, ..
                } => *returns_on_stack,
                _ => false,
            })
        {
            VAR_TO_STACK_POS.lock().unwrap().last_used_stack_pos.0 = -8;
        } else {
            VAR_TO_STACK_POS.lock().unwrap().clear();
        }

        let mut instructions = vec![];
        for instr in &self.instructions {
            instructions.push(instr.replace_pseudo());
        }

        let s = VAR_TO_STACK_POS
            .lock()
            .unwrap()
            .last_used_stack_pos
            .0
            .unsigned_abs() as usize;

        let stack_space = (s + 15) & !15;

        AsmFunction {
            name: self.name.clone(),
            instructions,
            global: self.global,
            stack_space,
        }
    }
}
