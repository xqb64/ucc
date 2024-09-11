use std::{collections::HashMap, sync::Mutex};

use crate::{
    ir::{
        BinaryOp, IRFunction, IRInstruction, IRNode, IRProgram, IRStaticVariable, IRValue, UnaryOp,
    }, lexer::Const, parser::Type, typechecker::{IdentifierAttrs, StaticInit, SYMBOL_TABLE}
};

#[derive(Debug, Clone, PartialEq)]
pub enum AsmNode {
    Program(AsmProgram),
    Function(AsmFunction),
    StaticVariable(AsmStaticVariable),
    Operand(AsmOperand),
    Instructions(Vec<AsmInstruction>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmProgram {
    pub functions: Vec<AsmNode>,
    pub static_vars: Vec<AsmNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmFunction {
    pub name: String,
    pub instructions: Vec<AsmInstruction>,
    pub global: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmStaticVariable {
    pub name: String,
    pub init: StaticInit,
    pub global: bool,
    pub alignment: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmInstruction {
    Mov {
        asm_type: AsmType,
        src: AsmOperand,
        dst: AsmOperand,
    },
    Movsx {
        src: AsmOperand,
        dst: AsmOperand,
    },
    Unary {
        asm_type: AsmType,
        op: AsmUnaryOp,
        operand: AsmOperand,
    },
    Binary {
        asm_type: AsmType,
        op: AsmBinaryOp,
        lhs: AsmOperand,
        rhs: AsmOperand,
    },
    Cmp {
        asm_type: AsmType,
        lhs: AsmOperand,
        rhs: AsmOperand,
    },
    Imul {
        asm_type: AsmType,
        src: AsmOperand,
        dst: AsmOperand,
    },
    Idiv { 
        asm_type: AsmType,
        operand: AsmOperand,
    },
    Cdq {
        asm_type: AsmType,
    },
    Jmp {
        target: String,
    },
    JmpCC {
        condition: ConditionCode,
        target: String,
    },
    SetCC {
        condition: ConditionCode,
        operand: AsmOperand,
    },
    Label(String),
    AllocateStack(usize),
    DeallocateStack(usize),
    Push(AsmOperand),
    Call(String),
    Ret,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConditionCode {
    E,
    NE,
    L,
    LE,
    G,
    GE,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmOperand {
    Imm(i64),
    Pseudo(String),
    Stack(i32),
    Register(AsmRegister),
    Data(String),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AsmRegister {
    AX,
    CX,
    DX,
    DI,
    SI,
    R8,
    R9,
    R10,
    R11,
    SP,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum AsmUnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum AsmBinaryOp {
    Add,
    Sub,
    Mul,
}

pub trait Codegen {
    fn codegen(&self) -> AsmNode;
}

impl Codegen for IRNode {
    fn codegen(&self) -> AsmNode {
        match self {
            IRNode::Program(prog) => prog.codegen(),
            IRNode::Function(func) => func.codegen(),
            IRNode::Instructions(instrs) => instrs.codegen(),
            IRNode::Value(value) => value.codegen(),
            IRNode::StaticVariable(static_var) => static_var.codegen(),
        }
    }
}

impl Codegen for IRProgram {
    fn codegen(&self) -> AsmNode {
        let mut functions = vec![];
        for func in &self.functions {
            functions.push(func.codegen());
        }

        let mut static_vars = vec![];
        for static_var in &self.static_vars {
            static_vars.push(static_var.codegen());
        }
        AsmNode::Program(AsmProgram {
            functions,
            static_vars,
        })
    }
}

impl Codegen for IRStaticVariable {
    fn codegen(&self) -> AsmNode {
        AsmNode::StaticVariable(AsmStaticVariable {
            name: self.name.clone(),
            init: self.init.clone(),
            global: self.global,
            alignment: 8,
        })
    }
}

impl Codegen for IRFunction {
    fn codegen(&self) -> AsmNode {
        let mut instructions = vec![];

        instructions.push(AsmInstruction::AllocateStack(0));

        let registers = [
            AsmRegister::DI,
            AsmRegister::SI,
            AsmRegister::DX,
            AsmRegister::CX,
            AsmRegister::R8,
            AsmRegister::R9,
        ];

        for (reg_idx, arg) in self.params.iter().take(6).enumerate() {
            instructions.push(AsmInstruction::Mov {
                asm_type: AsmType::Longword,
                src: AsmOperand::Register(registers[reg_idx]),
                dst: AsmOperand::Pseudo(arg.to_owned()),
            });
        }

        let mut stack_offset = 16;
        for arg in self.params.iter().skip(6) {
            instructions.push(AsmInstruction::Mov {
                asm_type: AsmType::Quadword,
                src: AsmOperand::Stack(stack_offset),
                dst: AsmOperand::Pseudo(arg.to_owned()),
            });
            stack_offset += 8;
        }

        for instr in &self.body {
            instructions.extend::<Vec<AsmInstruction>>(instr.codegen().into());
        }

        let symbol = match SYMBOL_TABLE.lock().unwrap().get(&self.name).cloned() {
            Some(symbol) => symbol,
            None => unreachable!(),
        };

        let global = match symbol.attrs {
            IdentifierAttrs::FuncAttr { defined: _, global } => global,
            _ => unreachable!(),
        };

        AsmNode::Function(AsmFunction {
            name: self.name.clone(),
            instructions,
            global,
        })
    }
}

impl Codegen for Vec<IRInstruction> {
    fn codegen(&self) -> AsmNode {
        let mut instructions = vec![];

        for instr in self {
            instructions.extend::<Vec<AsmInstruction>>(instr.codegen().into());
        }

        AsmNode::Instructions(instructions)
    }
}

impl Codegen for IRValue {
    fn codegen(&self) -> AsmNode {
        match self {
            IRValue::Constant(n) => match n {
                Const::Int(n) => AsmNode::Operand(AsmOperand::Imm(*n as i64)),
                Const::Long(n) => AsmNode::Operand(AsmOperand::Imm(*n)),
            }
            IRValue::Var(name) => AsmNode::Operand(AsmOperand::Pseudo(name.to_owned())),
        }
    }
}

impl Codegen for IRInstruction {
    fn codegen(&self) -> AsmNode {
        match self {
            IRInstruction::Unary { op, src, dst } => {
                println!("here in unary");
                let asm_type = get_asm_type(&dst);
                println!("here in unary enough");

                match op {
                    UnaryOp::Negate | UnaryOp::Complement => AsmNode::Instructions(vec![
                        AsmInstruction::Mov {
                            asm_type,
                            src: src.codegen().into(),
                            dst: dst.codegen().into(),
                        },
                        AsmInstruction::Unary {
                            asm_type,
                            op: (*op).into(),
                            operand: dst.codegen().into(),
                        },
                    ]),
                    UnaryOp::Not => AsmNode::Instructions(vec![
                        AsmInstruction::Cmp {
                            asm_type,
                            lhs: AsmOperand::Imm(0),
                            rhs: src.codegen().into(),
                        },
                        AsmInstruction::Mov {
                            asm_type,
                            src: AsmOperand::Imm(0),
                            dst: dst.codegen().into(),
                        },
                        AsmInstruction::SetCC {
                            condition: ConditionCode::E,
                            operand: dst.codegen().into(),
                        },
                    ]),
                }
            }
            IRInstruction::Ret(value) => AsmNode::Instructions(vec![
                AsmInstruction::Mov {
                    asm_type: AsmType::Longword,
                    src: value.codegen().into(),
                    dst: AsmOperand::Register(AsmRegister::AX),
                },
                AsmInstruction::Ret,
            ]),
            IRInstruction::Binary { op, lhs, rhs, dst } => { 
                println!("here in binary");
                let asm_type = get_asm_type(&lhs);
                println!("here in binary enough");

                match op {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul => AsmNode::Instructions(vec![
                        AsmInstruction::Mov {
                            asm_type,
                            src: lhs.codegen().into(),
                            dst: dst.codegen().into(),
                        },
                        AsmInstruction::Binary {
                            asm_type,
                            op: (*op).into(),
                            lhs: rhs.codegen().into(),
                            rhs: dst.codegen().into(),
                        },
                    ]),
                    BinaryOp::Div => AsmNode::Instructions(vec![
                        AsmInstruction::Mov {
                            asm_type,
                            src: lhs.codegen().into(),
                            dst: AsmOperand::Register(AsmRegister::AX),
                        },
                        AsmInstruction::Cdq {
                            asm_type,
                        },
                        AsmInstruction::Idiv {
                            asm_type,
                            operand: rhs.codegen().into()
                        },
                        AsmInstruction::Mov {
                            asm_type,
                            src: AsmOperand::Register(AsmRegister::AX),
                            dst: dst.codegen().into(),
                        },
                    ]),
                    BinaryOp::Rem => AsmNode::Instructions(vec![
                        AsmInstruction::Mov {
                            asm_type,
                            src: lhs.codegen().into(),
                            dst: AsmOperand::Register(AsmRegister::AX),
                        },
                        AsmInstruction::Cdq { asm_type },
                        AsmInstruction::Idiv {
                            asm_type,
                            operand: rhs.codegen().into()
                        },
                        AsmInstruction::Mov {
                            asm_type,
                            src: AsmOperand::Register(AsmRegister::DX),
                            dst: dst.codegen().into(),
                        },
                    ]),
                    BinaryOp::Less
                    | BinaryOp::LessEqual
                    | BinaryOp::Greater
                    | BinaryOp::GreaterEqual
                    | BinaryOp::Equal
                    | BinaryOp::NotEqual => AsmNode::Instructions(vec![
                        AsmInstruction::Cmp {
                            asm_type,
                            lhs: rhs.codegen().into(),
                            rhs: lhs.codegen().into(),
                        },
                        AsmInstruction::Mov {
                            asm_type,
                            src: AsmOperand::Imm(0),
                            dst: dst.codegen().into(),
                        },
                        AsmInstruction::SetCC {
                            condition: match op {
                                BinaryOp::Less => ConditionCode::L,
                                BinaryOp::LessEqual => ConditionCode::LE,
                                BinaryOp::Greater => ConditionCode::G,
                                BinaryOp::GreaterEqual => ConditionCode::GE,
                                BinaryOp::Equal => ConditionCode::E,
                                BinaryOp::NotEqual => ConditionCode::NE,
                                _ => unreachable!(),
                            },
                            operand: dst.codegen().into(),
                        },
                    ]),
                }
            },
            IRInstruction::JumpIfZero { condition, target } => {
                println!("here in jump if zero");
                let asm_type = get_asm_type(&condition);
                println!("here in jump if zero enough");

                AsmNode::Instructions(vec![
                    AsmInstruction::Cmp {
                        asm_type,
                        lhs: AsmOperand::Imm(0),
                        rhs: condition.codegen().into(),
                    },
                    AsmInstruction::JmpCC {
                        condition: ConditionCode::E,
                        target: target.to_owned(),
                    },
                ])
            }
            IRInstruction::JumpIfNotZero { condition, target } => { 
                println!("here in jump if not zero");
                let asm_type = get_asm_type(&condition);
                println!("here in jump if not zero enough");

                AsmNode::Instructions(vec![
                    AsmInstruction::Cmp {
                        asm_type,
                        lhs: AsmOperand::Imm(0),
                        rhs: condition.codegen().into(),
                    },
                    AsmInstruction::JmpCC {
                        condition: ConditionCode::NE,
                        target: target.to_owned(),
                    },
                ])
            }
            IRInstruction::Jump(target) => AsmNode::Instructions(vec![AsmInstruction::Jmp {
                target: target.to_owned(),
            }]),
            IRInstruction::Label(label) => {
                AsmNode::Instructions(vec![AsmInstruction::Label(label.to_owned())])
            }
            IRInstruction::Copy { src, dst } => {
                let asm_type = get_asm_type(&src);

                AsmNode::Instructions(vec![AsmInstruction::Mov {
                    asm_type,
                    src: src.codegen().into(),
                dst: dst.codegen().into(),
            }])
        }
            IRInstruction::Call { target, args, dst } => {
                let arg_registers = [
                    AsmRegister::DI,
                    AsmRegister::SI,
                    AsmRegister::DX,
                    AsmRegister::CX,
                    AsmRegister::R8,
                    AsmRegister::R9,
                ];

                let register_args = args.iter().take(6).collect::<Vec<_>>();
                let stack_args = args.iter().skip(6).collect::<Vec<_>>();

                let stack_padding = if stack_args.len() % 2 != 0 { 8 } else { 0 };

                let mut instructions = vec![];

                if stack_padding != 0 {
                    instructions.push(AsmInstruction::AllocateStack(stack_padding));
                }

                for (reg_index, reg_arg) in register_args.into_iter().enumerate() {
                    let reg = arg_registers[reg_index];
                    let asm_arg = reg_arg.codegen().into();
                    instructions.push(AsmInstruction::Mov {
                        asm_type: AsmType::Longword,
                        src: asm_arg,
                        dst: AsmOperand::Register(reg),
                    });
                }

                for stack_arg in stack_args.iter().rev() {
                    let asm_arg = stack_arg.codegen().into();


                    match asm_arg {
                        AsmOperand::Imm(_) | AsmOperand::Register(_) => {
                            instructions.push(AsmInstruction::Push(asm_arg.clone()));
                        }
                        AsmOperand::Data(ref name) | AsmOperand::Pseudo(ref name) => {
                            let is_quadword = match SYMBOL_TABLE.lock().unwrap().get(name) {
                                Some(symbol) => match symbol.attrs {
                                    IdentifierAttrs::StaticAttr {
                                        initial_value: _,
                                        global: _,
                                    } => true,
                                    _ => false,
                                },
                                None => false,
                            };
                            if is_quadword {
                                instructions.push(AsmInstruction::Push(asm_arg));
                            } else {
                                instructions.push(AsmInstruction::Mov {
                                    asm_type: AsmType::Quadword,
                                    src: asm_arg,
                                    dst: AsmOperand::Register(AsmRegister::AX),
                                });
                                instructions
                                    .push(AsmInstruction::Push(AsmOperand::Register(AsmRegister::AX)));    
                            }
                        }
                        _ => {
                            instructions.push(AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: asm_arg,
                                dst: AsmOperand::Register(AsmRegister::AX),
                            });
                            instructions
                                .push(AsmInstruction::Push(AsmOperand::Register(AsmRegister::AX)));
                        }
                    }
                }

                instructions.push(AsmInstruction::Call(target.to_owned()));

                let bytes_to_remove = 8 * stack_args.len() + stack_padding;
                if bytes_to_remove != 0 {
                    instructions.push(AsmInstruction::DeallocateStack(bytes_to_remove));
                }

                let asm_dst = dst.codegen().into();
                instructions.push(AsmInstruction::Mov {
                    asm_type: AsmType::Longword,
                    src: AsmOperand::Register(AsmRegister::AX),
                    dst: asm_dst,
                });

                AsmNode::Instructions(instructions)
            }
            IRInstruction::SignExtend { src, dst } => {
                AsmNode::Instructions(vec![
                    AsmInstruction::Movsx {
                        src: src.codegen().into(),
                        dst: dst.codegen().into(),
                    },
                ])
            }
            IRInstruction::Truncate { src, dst } => {
                AsmNode::Instructions(vec![
                    AsmInstruction::Mov {
                        asm_type: AsmType::Longword,
                        src: src.codegen().into(),
                        dst: dst.codegen().into(),
                    },
                ])
            }
        }
    }
}

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
        }
    }
}

impl ReplacePseudo for AsmStaticVariable {
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
            AsmInstruction::Unary { op, operand, asm_type } => AsmInstruction::Unary {
                asm_type: *asm_type,
                op: *op,
                operand: operand.replace_pseudo(),
            },
            AsmInstruction::Binary { op, lhs, rhs, asm_type } => AsmInstruction::Binary {
                asm_type: *asm_type,
                op: *op,
                lhs: lhs.replace_pseudo(),
                rhs: rhs.replace_pseudo(),
            },
            AsmInstruction::Idiv { operand, asm_type } => AsmInstruction::Idiv { asm_type: *asm_type, operand: operand.replace_pseudo() },
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
            AsmInstruction::Movsx { src, dst } => {
                AsmInstruction::Movsx { src: src.replace_pseudo(), dst: dst.replace_pseudo() }
            }
            _ => self.clone(),
        }
    }
}

impl ReplacePseudo for AsmOperand {
    fn replace_pseudo(&self) -> Self {
        match self {
            AsmOperand::Imm(_) => self.clone(),
            AsmOperand::Pseudo(name) => {
                let mut offset_manager = OFFSET_MANAGER.lock().unwrap();
                if let Some(symbol) = SYMBOL_TABLE.lock().unwrap().get(name) {
                    match symbol.attrs {
                        IdentifierAttrs::StaticAttr {
                            initial_value: _,
                            global: _,
                        } => AsmOperand::Data(name.clone()),
                        _ => {
                            let mut offset = offset_manager.get_offset(name);

                            let _type = match ASM_SYMBOL_TABLE.lock().unwrap().get(name).cloned().unwrap() {
                                AsmSymtabEntry::Object { _type, is_static: _ } => _type,
                                _ => unreachable!(),
                            };

                            if _type == AsmType::Quadword {
                                offset -= 4;
                                offset_manager.offset -= 4;                                
                            }

                            AsmOperand::Stack(offset)
                        }
                    }
                } else {
                    let offset = offset_manager.get_offset(name);
                    AsmOperand::Stack(offset)
                }
            }
            AsmOperand::Stack(_) => self.clone(),
            AsmOperand::Register(_) => self.clone(),
            AsmOperand::Data(_) => self.clone(),
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
        }
    }
}

impl ReplacePseudo for AsmFunction {
    fn replace_pseudo(&self) -> Self {
        // clear offsets
        OFFSET_MANAGER.lock().unwrap().offsets.clear();
        OFFSET_MANAGER.lock().unwrap().offset = -4;

        let mut instructions = vec![];
        for instr in &self.instructions {
            instructions.push(instr.replace_pseudo());
        }
        AsmFunction {
            name: self.name.clone(),
            instructions,
            global: self.global,
        }
    }
}

pub trait Fixup {
    fn fixup(&mut self) -> Self;
}

impl Fixup for AsmNode {
    fn fixup(&mut self) -> Self {
        match self {
            AsmNode::Program(prog) => AsmNode::Program(prog.fixup()),
            AsmNode::Function(func) => AsmNode::Function(func.fixup()),
            AsmNode::Instructions(instrs) => AsmNode::Instructions(instrs.fixup()),
            AsmNode::Operand(op) => AsmNode::Operand(op.clone()),
            AsmNode::StaticVariable(static_var) => AsmNode::StaticVariable(static_var.clone()),
        }
    }
}

impl Fixup for AsmProgram {
    fn fixup(&mut self) -> AsmProgram {
        let mut functions = vec![];

        for func in &mut self.functions {
            functions.push(func.fixup());
        }

        AsmProgram {
            functions,
            static_vars: self.static_vars.clone(),
        }
    }
}

impl Fixup for AsmFunction {
    fn fixup(&mut self) -> AsmFunction {
        let mut instructions = vec![];

        for instr in &mut self.instructions {
            match instr {
                AsmInstruction::Mov { src, dst, asm_type } => match asm_type {
                    AsmType::Longword => match (src, dst) {
                        (AsmOperand::Stack(src_n), AsmOperand::Stack(dst_n)) => {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Stack(*src_n),
                                    dst: AsmOperand::Register(AsmRegister::R10),
                                },
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Register(AsmRegister::R10),
                                    dst: AsmOperand::Stack(*dst_n),
                                },
                            ]);
                        }
                        (AsmOperand::Data(src), AsmOperand::Stack(dst_n)) => {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Data(src.clone()),
                                    dst: AsmOperand::Register(AsmRegister::R10),
                                },
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Register(AsmRegister::R10),
                                    dst: AsmOperand::Stack(*dst_n),
                                },
                            ]);
                        }
                        (AsmOperand::Stack(src_n), AsmOperand::Data(dst)) => {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Stack(*src_n),
                                    dst: AsmOperand::Register(AsmRegister::R10),
                                },
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Register(AsmRegister::R10),
                                    dst: AsmOperand::Data(dst.clone()),
                                },
                            ]);
                        }
                        (AsmOperand::Data(src), AsmOperand::Data(dst)) => {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Data(src.clone()),
                                    dst: AsmOperand::Register(AsmRegister::R10),
                                },
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Register(AsmRegister::R10),
                                    dst: AsmOperand::Data(dst.clone()),
                                },
                            ]);
                        }
                        _ => instructions.push(instr.clone()),
                    }
                    AsmType::Quadword => match (src, dst) {
                        (AsmOperand::Stack(src_n), AsmOperand::Stack(dst_n)) => {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Stack(*src_n),
                                    dst: AsmOperand::Register(AsmRegister::R10),
                                },
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Register(AsmRegister::R10),
                                    dst: AsmOperand::Stack(*dst_n),
                                },
                            ]);
                        }
                        (AsmOperand::Data(src), AsmOperand::Stack(dst_n)) => {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Data(src.clone()),
                                    dst: AsmOperand::Register(AsmRegister::R10),
                                },
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Register(AsmRegister::R10),
                                    dst: AsmOperand::Stack(*dst_n),
                                },
                            ]);
                        }
                        (AsmOperand::Stack(src_n), AsmOperand::Data(dst)) => {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Stack(*src_n),
                                    dst: AsmOperand::Register(AsmRegister::R10),
                                },
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Register(AsmRegister::R10),
                                    dst: AsmOperand::Data(dst.clone()),
                                },
                            ]);
                        }
                        (AsmOperand::Data(src), AsmOperand::Data(dst)) => {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Data(src.clone()),
                                    dst: AsmOperand::Register(AsmRegister::R10),
                                },
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Register(AsmRegister::R10),
                                    dst: AsmOperand::Data(dst.clone()),
                                },
                            ]);
                        }
                        (AsmOperand::Imm(konst), AsmOperand::Stack(dst_n)) => {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Imm(*konst),
                                    dst: AsmOperand::Register(AsmRegister::R10),
                                },
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Register(AsmRegister::R10),
                                    dst: AsmOperand::Stack(*dst_n),
                                },
                            ]);
                        }
                        (AsmOperand::Imm(konst), AsmOperand::Data(dst)) => {
                            // if konst is too large to fit into i32, we need to load it into a register r10
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Imm(*konst),
                                    dst: AsmOperand::Register(AsmRegister::R10),
                                },
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Register(AsmRegister::R10),
                                    dst: AsmOperand::Data(dst.to_owned()),
                                },
                            ]);
                        }
                        _ => instructions.push(instr.clone()),
                    }
                },
                AsmInstruction::Binary { op, lhs, rhs, asm_type } => match op {
                    AsmBinaryOp::Add | AsmBinaryOp::Sub => match (lhs, rhs) {
                        (AsmOperand::Stack(src_n), AsmOperand::Stack(dst_n)) => {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Stack(*src_n),
                                    dst: AsmOperand::Register(AsmRegister::R10),
                                },
                                AsmInstruction::Binary {
                                    asm_type: *asm_type,
                                    op: *op,
                                    lhs: AsmOperand::Register(AsmRegister::R10),
                                    rhs: AsmOperand::Stack(*dst_n),
                                },
                            ]);
                        }
                        (AsmOperand::Data(src), AsmOperand::Stack(dst_n)) => {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Data(src.clone()),
                                    dst: AsmOperand::Register(AsmRegister::R10),
                                },
                                AsmInstruction::Binary {
                                    asm_type: *asm_type,
                                    op: *op,
                                    lhs: AsmOperand::Register(AsmRegister::R10),
                                    rhs: AsmOperand::Stack(*dst_n),
                                },
                            ]);
                        }
                        (AsmOperand::Data(src), AsmOperand::Data(dst)) => {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Data(src.clone()),
                                    dst: AsmOperand::Register(AsmRegister::R10),
                                },
                                AsmInstruction::Binary {
                                    asm_type: *asm_type,
                                    op: *op,
                                    lhs: AsmOperand::Register(AsmRegister::R10),
                                    rhs: AsmOperand::Data(dst.clone()),
                                },
                            ]);
                        }
                        (AsmOperand::Imm(konst), AsmOperand::Stack(dst_n)) => {
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
                                        rhs: AsmOperand::Stack(*dst_n),
                                    },
                                ])
                            }
                            else {
                                instructions.push(instr.clone());
                            }
                        }
                        _ => instructions.push(instr.clone()),
                    },
                    AsmBinaryOp::Mul => match (lhs.clone(), rhs.clone()) {
                        (AsmOperand::Stack(_), AsmOperand::Stack(_)) => {
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
                        (AsmOperand::Imm(konst), AsmOperand::Stack(_)) => {
                            // if konst can't fit in i32, we need to load it into a register
                            // and then multiply
                            if konst < i32::MIN as i64 || konst > i32::MAX as i64 {
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
                        _ => instructions.push(instr.clone()),
                    },
                }
                AsmInstruction::Idiv{ operand, asm_type } => {
                    if let AsmOperand::Imm(konst) = operand {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: *asm_type,
                                src: AsmOperand::Imm(*konst),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Idiv { asm_type: *asm_type, operand: AsmOperand::Register(AsmRegister::R10) },
                        ]);
                    } else {
                        instructions.push(instr.clone());
                    }
                },
                AsmInstruction::Cmp { lhs, rhs, asm_type } => match (lhs.clone(), rhs.clone()) {
                    (AsmOperand::Stack(src_n), AsmOperand::Stack(dst_n)) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: *asm_type,
                                src: AsmOperand::Stack(src_n),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Cmp {
                                asm_type: *asm_type,
                                lhs: AsmOperand::Register(AsmRegister::R10),
                                rhs: AsmOperand::Stack(dst_n),
                            },
                        ]);
                    }
                    (AsmOperand::Data(src), AsmOperand::Stack(dst_n)) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: *asm_type,
                                src: AsmOperand::Data(src.clone()),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Cmp {
                                asm_type: *asm_type,
                                lhs: AsmOperand::Register(AsmRegister::R10),
                                rhs: AsmOperand::Stack(dst_n),
                            },
                        ]);
                    }
                    (AsmOperand::Stack(src_n), AsmOperand::Data(dst)) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: *asm_type,
                                src: AsmOperand::Stack(src_n),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Cmp {
                                asm_type: *asm_type,
                                lhs: AsmOperand::Register(AsmRegister::R10),
                                rhs: AsmOperand::Data(dst.clone()),
                            },
                        ]);
                    }
                    (AsmOperand::Data(src), AsmOperand::Data(dst)) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: *asm_type,
                                src: AsmOperand::Data(src),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Cmp {
                                asm_type: *asm_type,
                                lhs: AsmOperand::Register(AsmRegister::R10),
                                rhs: AsmOperand::Data(dst.clone()),
                            },
                        ]);
                    }
                    (AsmOperand::Imm(konst1), AsmOperand::Imm(konst2)) => {
                        // move one constant to r10, the other to r11
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: *asm_type,
                                src: AsmOperand::Imm(konst1),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Mov {
                                asm_type: *asm_type,
                                src: AsmOperand::Imm(konst2),
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
                        if konst > i32::MAX as i64 || konst < i32::MIN as i64 {                            
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: *asm_type,
                                    src: AsmOperand::Imm(konst),
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
                                src: AsmOperand::Imm(konst),
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
                AsmInstruction::Movsx { src, dst } => match (src, dst) {
                    (AsmOperand::Data(src), AsmOperand::Stack(dst_n)) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Longword,
                                src: AsmOperand::Data(src.to_owned()),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Movsx {
                                src: AsmOperand::Register(AsmRegister::R10),
                                dst: AsmOperand::Register(AsmRegister::R11),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: AsmOperand::Register(AsmRegister::R11),
                                dst: AsmOperand::Stack(*dst_n),
                            },
                        ]);
                    }
                    (AsmOperand::Stack(src_n), AsmOperand::Stack(dst_n)) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Longword,
                                src: AsmOperand::Imm(*src_n as i64),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Movsx {
                                src: AsmOperand::Register(AsmRegister::R10),
                                dst: AsmOperand::Register(AsmRegister::R11),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: AsmOperand::Register(AsmRegister::R11),
                                dst: AsmOperand::Stack(*dst_n),
                            },
                        ]);
                    }
                    (AsmOperand::Imm(konst), AsmOperand::Stack(dst_n)) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Longword,
                                src: AsmOperand::Imm(*konst),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Movsx {
                                src: AsmOperand::Register(AsmRegister::R10),
                                dst: AsmOperand::Register(AsmRegister::R11),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: AsmOperand::Register(AsmRegister::R11),
                                dst: AsmOperand::Stack(*dst_n),
                            },
                        ]);
                    }
                    _ => instructions.push(instr.clone()),
                },
                AsmInstruction::Push(operand) => match operand {
                    AsmOperand::Imm(n) => {
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
                    _ => instructions.push(instr.clone()),
                }
                _ => instructions.push(instr.clone()),
            }
        }

        AsmFunction {
            name: self.name.clone(),
            instructions,
            global: self.global,
        }
    }
}

impl Fixup for Vec<AsmInstruction> {
    fn fixup(&mut self) -> Vec<AsmInstruction> {
        let mut instructions = vec![];

        for instr in self {
            match instr {
                AsmInstruction::Mov { src, dst, asm_type } => match (src, dst) {
                    (AsmOperand::Stack(src_n), AsmOperand::Stack(dst_n)) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: *asm_type,
                                src: AsmOperand::Stack(*src_n),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Mov {   
                                asm_type: *asm_type,
                                src: AsmOperand::Register(AsmRegister::R10),
                                dst: AsmOperand::Stack(*dst_n),
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

impl From<AsmNode> for AsmFunction {
    fn from(node: AsmNode) -> AsmFunction {
        match node {
            AsmNode::Function(func) => func,
            _ => unreachable!(),
        }
    }
}

impl From<AsmNode> for Vec<AsmInstruction> {
    fn from(node: AsmNode) -> Vec<AsmInstruction> {
        match node {
            AsmNode::Instructions(instrs) => instrs,
            _ => unreachable!(),
        }
    }
}

impl From<AsmNode> for AsmOperand {
    fn from(node: AsmNode) -> AsmOperand {
        match node {
            AsmNode::Operand(op) => op,
            _ => unreachable!(),
        }
    }
}

impl From<UnaryOp> for AsmUnaryOp {
    fn from(op: UnaryOp) -> AsmUnaryOp {
        match op {
            UnaryOp::Negate => AsmUnaryOp::Neg,
            UnaryOp::Complement => AsmUnaryOp::Not,
            _ => unreachable!(),
        }
    }
}

impl From<BinaryOp> for AsmBinaryOp {
    fn from(op: BinaryOp) -> AsmBinaryOp {
        match op {
            BinaryOp::Add => AsmBinaryOp::Add,
            BinaryOp::Sub => AsmBinaryOp::Sub,
            BinaryOp::Mul => AsmBinaryOp::Mul,
            _ => unreachable!(),
        }
    }
}

lazy_static::lazy_static! {
    pub static ref OFFSET_MANAGER: std::sync::Mutex<OffsetManager> = std::sync::Mutex::new(OffsetManager::new());
}

pub struct OffsetManager {
    pub offsets: std::collections::HashMap<String, i32>,
    pub offset: i32,
}

impl OffsetManager {
    fn new() -> OffsetManager {
        OffsetManager {
            offsets: std::collections::HashMap::new(),
            offset: -4,
        }
    }

    pub fn get_offset(&mut self, name: &str) -> i32 {
        if !self.offsets.contains_key(name) {
            self.offsets.insert(name.to_owned(), self.offset);
            self.offset -= 4;
        }

        self.offsets[name]
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum AsmType {
    Longword,
    Quadword,
}

fn get_asm_type(value: &IRValue) -> AsmType {
    match value {
        IRValue::Constant(konst) => match konst {
            Const::Int(_) => AsmType::Longword,
            Const::Long(_) => AsmType::Quadword,
        }
        IRValue::Var(var_name) => {
            println!("getting: {}", var_name);
            match SYMBOL_TABLE.lock().unwrap().get(var_name).unwrap()._type {
                Type::Int => AsmType::Longword,
                Type::Long => AsmType::Quadword,
                _ => unreachable!(),
            }
        }
    }
}

pub fn build_asm_symbol_table() {
    let frontend_symtab = SYMBOL_TABLE.lock().unwrap();
    let mut asm_symbol_table = ASM_SYMBOL_TABLE.lock().unwrap();

    for (identifier, symbol) in frontend_symtab.iter() {
        let entry = match symbol.attrs {
            IdentifierAttrs::FuncAttr { defined, .. } => AsmSymtabEntry::Function { defined },
            IdentifierAttrs::StaticAttr { initial_value, .. } => {
                let asm_type = match symbol._type {
                    Type::Int => AsmType::Longword,
                    Type::Long => AsmType::Quadword,
                    _ => panic!("Unsupported type for static variable"),
                };
                AsmSymtabEntry::Object {
                    _type: asm_type,
                    is_static: true,
                }
            },
            IdentifierAttrs::LocalAttr => {
                let asm_type = match symbol._type {
                    Type::Int => AsmType::Longword,
                    Type::Long => AsmType::Quadword,
                    _ => panic!("Unsupported type for static backend_symtab: {}", identifier),
                };
                AsmSymtabEntry::Object {
                    _type: asm_type,
                    is_static: false,
                }
            }
        };

        asm_symbol_table.insert(identifier.clone(), entry);
    }

    println!("{:?}", asm_symbol_table); 
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmSymtabEntry {
    Function {
        defined: bool,
    },
    Object {
        _type: AsmType,
        is_static: bool,
    }
}

lazy_static::lazy_static! {
    pub static ref ASM_SYMBOL_TABLE: Mutex<HashMap<String, AsmSymtabEntry>> = Mutex::new(HashMap::new());
}