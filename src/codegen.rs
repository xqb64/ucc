use std::{collections::HashMap, sync::Mutex};

use crate::{
    ir::{
        make_temporary, BinaryOp, IRFunction, IRInstruction, IRNode, IRProgram, IRStaticVariable,
        IRValue, UnaryOp,
    },
    lexer::Const,
    parser::Type,
    typechecker::{get_common_type, get_signedness, IdentifierAttrs, StaticInit, SYMBOL_TABLE},
};

#[derive(Debug, Clone, PartialEq)]
pub enum AsmNode {
    Program(AsmProgram),
    Function(AsmFunction),
    StaticVariable(AsmStaticVariable),
    StaticConstant(AsmStaticConstant),
    Operand(AsmOperand),
    Instructions(Vec<AsmInstruction>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmProgram {
    pub functions: Vec<AsmNode>,
    pub static_vars: Vec<AsmNode>,
    pub static_constants: Vec<AsmStaticConstant>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmFunction {
    pub name: String,
    pub instructions: Vec<AsmInstruction>,
    pub global: bool,
    pub stack_space: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmStaticVariable {
    pub name: String,
    pub init: StaticInit,
    pub global: bool,
    pub alignment: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmStaticConstant {
    pub name: String,
    pub init: StaticInit,
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
    MovZeroExtend {
        src: AsmOperand,
        dst: AsmOperand,
    },
    Cvttsd2si {
        asm_type: AsmType,
        src: AsmOperand,
        dst: AsmOperand,
    },
    Cvtsi2sd {
        asm_type: AsmType,
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
    Div {
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
    A,
    AE,
    B,
    BE,
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
    XMM0,
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7,
    XMM14,
    XMM15,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum AsmUnaryOp {
    Neg,
    Not,
    Shr,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum AsmBinaryOp {
    Add,
    Sub,
    Mul,
    DivDouble,
    And,
    Or,
    Xor,
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
            static_constants: STATIC_CONSTANTS.lock().unwrap().to_owned(),
        })
    }
}

impl Codegen for IRStaticVariable {
    fn codegen(&self) -> AsmNode {
        AsmNode::StaticVariable(AsmStaticVariable {
            name: self.name.clone(),
            init: self.init.clone(),
            global: self.global,
            alignment: match self._type {
                Type::Int => 4,
                Type::Long => 8,
                Type::Uint => 4,
                Type::Ulong => 8,
                _ => unreachable!(),
            },
        })
    }
}

impl Codegen for IRFunction {
    fn codegen(&self) -> AsmNode {
        let mut instructions = vec![];

        instructions.push(AsmInstruction::AllocateStack(0));

        let (int_reg_params, double_reg_params, stack_params) = classify_parameters(self.params.clone());

        let int_regs = [
            AsmRegister::DI,
            AsmRegister::SI,
            AsmRegister::DX,
            AsmRegister::CX,
            AsmRegister::R8,
            AsmRegister::R9,
        ];
    
        for (idx, (param_type, param)) in int_reg_params.iter().enumerate() {
            let reg = int_regs[idx];
            let instr = AsmInstruction::Mov {
                asm_type: param_type.clone(),
                src: AsmOperand::Register(reg),
                dst: AsmOperand::Pseudo(param.clone()),
            };
            instructions.push(instr);
        }
    
        let double_regs = [
            AsmRegister::XMM0,
            AsmRegister::XMM1,
            AsmRegister::XMM2,
            AsmRegister::XMM3,
            AsmRegister::XMM4,
            AsmRegister::XMM5,
            AsmRegister::XMM6,
            AsmRegister::XMM7,
        ];
            
        for (idx, (_param_type, param)) in double_reg_params.iter().enumerate() {
            let reg = double_regs[idx];
            let instr = AsmInstruction::Mov {
                asm_type: AsmType::Double,
                src: AsmOperand::Register(reg),
                dst: AsmOperand::Pseudo(param.clone()),
            };
            instructions.push(instr);
        }
    
        let mut offset = 16;
        for (param_type, param) in stack_params {
            let instr = AsmInstruction::Mov {
                asm_type: param_type,
                src: AsmOperand::Stack(offset),
                dst: AsmOperand::Pseudo(param.clone()),
            };
            instructions.push(instr);
            offset += 8;
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
            stack_space: 0,
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
                Const::UInt(n) => AsmNode::Operand(AsmOperand::Imm(*n as i64)),
                Const::ULong(n) => AsmNode::Operand(AsmOperand::Imm(*n as i64)),
                Const::Double(n) => {
                    let static_const = AsmStaticConstant {
                        name: format!("static_const.{}", make_temporary()),
                        init: StaticInit::Double(*n),
                        alignment: 8,
                    };
                    STATIC_CONSTANTS.lock().unwrap().push(static_const.clone());
                    AsmNode::Operand(AsmOperand::Data(static_const.name))
                },
            },
            IRValue::Var(name) => AsmNode::Operand(AsmOperand::Pseudo(name.to_owned())),
        }
    }
}

impl Codegen for IRInstruction {
    fn codegen(&self) -> AsmNode {
        match self {
            IRInstruction::Unary { op, src, dst } => {
                let asm_type = get_asm_type(&dst);

                if asm_type == AsmType::Double && *op == UnaryOp::Negate {
                    let static_const = AsmStaticConstant {
                        name: format!("static_const.{}", make_temporary()),
                        init: StaticInit::Double(-1.0),
                        alignment: 16,
                    };
                    STATIC_CONSTANTS.lock().unwrap().push(static_const.clone(   ));

                    return AsmNode::Instructions(vec![
                        AsmInstruction::Mov {
                            asm_type,
                            src: src.codegen().into(),
                            dst: dst.codegen().into(),
                        },
                        AsmInstruction::Binary {
                            asm_type,
                            op: AsmBinaryOp::Xor,
                            lhs: AsmOperand::Data(static_const.name),
                            rhs: dst.codegen().into(),
                        },
                    ]);
                }

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
                    UnaryOp::Not => {
                        if asm_type == AsmType::Double {
                            AsmNode::Instructions(vec![
                                AsmInstruction::Binary { asm_type: AsmType::Double, op: AsmBinaryOp::Xor, lhs: AsmOperand::Register(AsmRegister::XMM0), rhs: AsmOperand::Register(AsmRegister::XMM0) },
                                AsmInstruction::Cmp { asm_type: AsmType::Double, lhs: src.codegen().into(), rhs: AsmOperand::Register(AsmRegister::XMM0) },
                                AsmInstruction::SetCC {
                                    condition: ConditionCode::E,
                                    operand: dst.codegen().into(),
                                },
                            ])
                        } else {
                            AsmNode::Instructions(vec![
                                AsmInstruction::Cmp {
                                    asm_type: get_asm_type(src),
                                    lhs: AsmOperand::Imm(0),
                                    rhs: src.codegen().into(),
                                },
                                AsmInstruction::Mov {
                                    asm_type: get_asm_type(src),
                                    src: AsmOperand::Imm(0),
                                    dst: dst.codegen().into(),
                                },
                                AsmInstruction::SetCC {
                                    condition: ConditionCode::E,
                                    operand: dst.codegen().into(),
                                },
                            ])
                        } 
                    }
                }
            }
            IRInstruction::Ret(value) => {
                let asm_type = get_asm_type(value);
                if asm_type == AsmType::Double {
                    AsmNode::Instructions(vec![
                        AsmInstruction::Mov {
                                asm_type: AsmType::Double,
                                src: value.codegen().into(),
                                dst: AsmOperand::Register(AsmRegister::XMM0),
                            },
                        AsmInstruction::Ret,
                    ])
                } else {
                    AsmNode::Instructions(vec![
                        AsmInstruction::Mov {
                                asm_type: get_asm_type(value),
                                src: value.codegen().into(),
                                dst: AsmOperand::Register(AsmRegister::AX),
                            },
                        AsmInstruction::Ret,
                    ])
                }
            },
            IRInstruction::Binary { op, lhs, rhs, dst } => {
                let asm_type = get_asm_type(&lhs);

                match op {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul => {
                        println!("lhs, rhs, dst: {:?}, {:?}, {:?}", lhs, rhs, dst);
                        AsmNode::Instructions(vec![
                            AsmInstruction::Mov {
                                asm_type: get_asm_type(&dst),
                                src: lhs.codegen().into(),
                                dst: dst.codegen().into(),
                            },
                            AsmInstruction::Binary {
                                asm_type: get_asm_type(&dst),
                                op: (*op).into(),
                                lhs: rhs.codegen().into(),
                                rhs: dst.codegen().into(),
                            },
                        ])
                    }
                    BinaryOp::Div => {
                        match (lhs.clone(), rhs.clone()) {
                            (IRValue::Constant(Const::Double(_)), IRValue::Constant(Const::Double(_))) => {
                                return AsmNode::Instructions(vec![
                                    AsmInstruction::Mov {
                                        asm_type: AsmType::Double,
                                        src: lhs.codegen().into(),
                                        dst: dst.codegen().into(),
                                    },
                                    AsmInstruction::Binary {
                                        asm_type: AsmType::Double,
                                        op: AsmBinaryOp::DivDouble,
                                        lhs: rhs.codegen().into(),
                                        rhs: dst.codegen().into(),
                                    },
                                ]);
                            }
                            (IRValue::Var(var1), IRValue::Constant(Const::Double(_))) => {
                                let var1_type = match SYMBOL_TABLE.lock().unwrap().get(&var1).cloned() {
                                    Some(symbol) => symbol._type,
                                    None => unreachable!(),
                                };

                                if var1_type == Type::Double {
                                    return AsmNode::Instructions(vec![
                                        AsmInstruction::Mov {
                                            asm_type: AsmType::Double,
                                            src: lhs.codegen().into(),
                                            dst: dst.codegen().into(),
                                        },
                                        AsmInstruction::Binary {
                                            asm_type: AsmType::Double,
                                            op: AsmBinaryOp::DivDouble,
                                            lhs: rhs.codegen().into(),
                                            rhs: dst.codegen().into(),
                                        },
                                    ]);
                                }
                            }
                            (IRValue::Constant(Const::Double(_)), IRValue::Var(var2)) => {
                                let var2_type = match SYMBOL_TABLE.lock().unwrap().get(&var2).cloned() {
                                    Some(symbol) => symbol._type,
                                    None => unreachable!(),
                                };

                                if var2_type == Type::Double {
                                    return AsmNode::Instructions(vec![
                                        AsmInstruction::Mov {
                                            asm_type: AsmType::Double,
                                            src: lhs.codegen().into(),
                                            dst: dst.codegen().into(),
                                        },
                                        AsmInstruction::Binary {
                                            asm_type: AsmType::Double,
                                            op: AsmBinaryOp::DivDouble,
                                            lhs: rhs.codegen().into(),
                                            rhs: dst.codegen().into(),
                                        },
                                    ]);
                                }
                            }
                            (IRValue::Var(var1), IRValue::Var(var2)) => {
                                let var1_type = match SYMBOL_TABLE.lock().unwrap().get(&var1).cloned() {
                                    Some(symbol) => symbol._type,
                                    None => unreachable!(),
                                };

                                let var2_type = match SYMBOL_TABLE.lock().unwrap().get(&var2).cloned() {
                                    Some(symbol) => symbol._type,
                                    None => unreachable!(),
                                };
                                
                                if var1_type == Type::Double && var2_type == Type::Double {
                                    return AsmNode::Instructions(vec![
                                        AsmInstruction::Mov {
                                            asm_type: AsmType::Double,
                                            src: lhs.codegen().into(),
                                            dst: dst.codegen().into(),
                                        },
                                        AsmInstruction::Binary {
                                            asm_type: AsmType::Double,
                                            op: AsmBinaryOp::DivDouble,
                                            lhs: rhs.codegen().into(),
                                            rhs: dst.codegen().into(),
                                        },
                                    ]);
                                }
                            }
                            _ => {}
                        }

                        let mut v = vec![AsmInstruction::Mov {
                            asm_type,
                            src: lhs.codegen().into(),
                            dst: AsmOperand::Register(AsmRegister::AX),
                        }];

                        let signedness = match lhs {
                            IRValue::Var(name) => get_signedness(
                                &SYMBOL_TABLE
                                    .lock()
                                    .unwrap()
                                    .get(name)
                                    .cloned()
                                    .unwrap()
                                    ._type,
                            ),
                            IRValue::Constant(konst) => match konst {
                                Const::Int(_) => true,
                                Const::Long(_) => true,
                                Const::UInt(_) => false,
                                Const::ULong(_) => false,
                                Const::Double(_) => true,
                                _ => todo!(),
                            },
                        };

                        if signedness {
                            v.extend(vec![
                                AsmInstruction::Cdq { asm_type },
                                AsmInstruction::Idiv {
                                    asm_type,
                                    operand: rhs.codegen().into(),
                                },
                                AsmInstruction::Mov {
                                    asm_type,
                                    src: AsmOperand::Register(AsmRegister::AX),
                                    dst: dst.codegen().into(),
                                },
                            ])
                        } else {
                            v.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: AsmType::Longword,
                                    src: AsmOperand::Imm(0),
                                    dst: AsmOperand::Register(AsmRegister::DX),
                                },
                                AsmInstruction::Div {
                                    asm_type: AsmType::Longword,
                                    operand: rhs.codegen().into(),
                                },
                                AsmInstruction::Mov {
                                    asm_type,
                                    src: AsmOperand::Register(AsmRegister::AX),
                                    dst: dst.codegen().into(),
                                },
                            ])
                        }

                        AsmNode::Instructions(v)
                    }
                    BinaryOp::Rem => {
                        let mut v = vec![AsmInstruction::Mov {
                            asm_type,
                            src: lhs.codegen().into(),
                            dst: AsmOperand::Register(AsmRegister::AX),
                        }];

                        let signedness = match lhs {
                            IRValue::Var(name) => get_signedness(
                                &SYMBOL_TABLE
                                    .lock()
                                    .unwrap()
                                    .get(name)
                                    .cloned()
                                    .unwrap()
                                    ._type,
                            ),
                            IRValue::Constant(konst) => match konst {
                                Const::Int(_) => true,
                                Const::Long(_) => true,
                                Const::UInt(_) => false,
                                Const::ULong(_) => false,
                                Const::Double(_) => true,
                                _ => todo!(),
                            },
                        };

                        if signedness {
                            v.extend(vec![
                                AsmInstruction::Cdq { asm_type },
                                AsmInstruction::Idiv {
                                    asm_type,
                                    operand: rhs.codegen().into(),
                                },
                                AsmInstruction::Mov {
                                    asm_type,
                                    src: AsmOperand::Register(AsmRegister::DX),
                                    dst: dst.codegen().into(),
                                },
                            ])
                        } else {
                            v.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: AsmType::Quadword,
                                    src: lhs.codegen().into(),
                                    dst: AsmOperand::Register(AsmRegister::AX),
                                },
                                AsmInstruction::Mov {
                                    asm_type: AsmType::Quadword,
                                    src: AsmOperand::Imm(0),
                                    dst: AsmOperand::Register(AsmRegister::DX),
                                },
                                AsmInstruction::Div {
                                    asm_type: AsmType::Quadword,
                                    operand: rhs.codegen().into(),
                                },
                                AsmInstruction::Mov {
                                    asm_type,
                                    src: AsmOperand::Register(AsmRegister::DX),
                                    dst: dst.codegen().into(),
                                },
                            ])
                        }

                        AsmNode::Instructions(v)
                    }
                    BinaryOp::Less
                    | BinaryOp::LessEqual
                    | BinaryOp::Greater
                    | BinaryOp::GreaterEqual
                    | BinaryOp::Equal
                    | BinaryOp::NotEqual => {
                        let lhs_type = match lhs {
                            IRValue::Var(name) => {
                                SYMBOL_TABLE
                                    .lock()
                                    .unwrap()
                                    .get(name)
                                    .cloned()
                                    .unwrap()
                                    ._type
                            }
                            IRValue::Constant(konst) => match konst {
                                Const::Int(_) => Type::Int,
                                Const::Long(_) => Type::Long,
                                Const::UInt(_) => Type::Uint,
                                Const::ULong(_) => Type::Ulong,
                                Const::Double(_) => Type::Double,
                                _ => todo!(),
                            },
                        };

                        let rhs_type = match rhs {
                            IRValue::Var(name) => {
                                SYMBOL_TABLE
                                    .lock()
                                    .unwrap()
                                    .get(name)
                                    .cloned()
                                    .unwrap()
                                    ._type
                            }
                            IRValue::Constant(konst) => match konst {
                                Const::Int(_) => Type::Int,
                                Const::Long(_) => Type::Long,
                                Const::UInt(_) => Type::Uint,
                                Const::ULong(_) => Type::Ulong,
                                Const::Double(_) => Type::Double,
                                _ => todo!(),
                            },
                        };

                        if lhs_type == Type::Double && rhs_type == Type::Double {
                            AsmNode::Instructions(vec![
                                AsmInstruction::Cmp {
                                    asm_type: AsmType::Double,
                                    lhs: rhs.codegen().into(),
                                    rhs: lhs.codegen().into(),
                                },
                                AsmInstruction::Mov {
                                    asm_type: AsmType::Longword,
                                    src: AsmOperand::Imm(0),
                                    dst: dst.codegen().into(),
                                },
                                AsmInstruction::SetCC {
                                    condition: match op {
                                        BinaryOp::Less => ConditionCode::B,
                                        BinaryOp::LessEqual => ConditionCode::BE,
                                        BinaryOp::Greater => ConditionCode::A,
                                        BinaryOp::GreaterEqual => ConditionCode::AE,
                                        BinaryOp::Equal => ConditionCode::E,
                                        BinaryOp::NotEqual => ConditionCode::NE,
                                        _ => unreachable!(),
                                    },
                                    operand: dst.codegen().into(),
                                },
                            ])
                        } else {
                            let signedness = get_signedness(&get_common_type(&lhs_type, &rhs_type));

                            if signedness {
                                AsmNode::Instructions(vec![
                                    AsmInstruction::Cmp {
                                        asm_type,
                                        lhs: rhs.codegen().into(),
                                        rhs: lhs.codegen().into(),
                                    },
                                    AsmInstruction::Mov {
                                        asm_type: get_asm_type(dst),
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
                                ])
                            } else {
                                AsmNode::Instructions(vec![
                                    AsmInstruction::Cmp {
                                        asm_type,
                                        lhs: rhs.codegen().into(),
                                        rhs: lhs.codegen().into(),
                                    },
                                    AsmInstruction::Mov {
                                        asm_type: get_asm_type(dst),
                                        src: AsmOperand::Imm(0),
                                        dst: dst.codegen().into(),
                                    },
                                    AsmInstruction::SetCC {
                                        condition: match op {
                                            BinaryOp::Less => ConditionCode::B,
                                            BinaryOp::LessEqual => ConditionCode::BE,
                                            BinaryOp::Greater => ConditionCode::A,
                                            BinaryOp::GreaterEqual => ConditionCode::AE,
                                            BinaryOp::Equal => ConditionCode::E,
                                            BinaryOp::NotEqual => ConditionCode::NE,
                                            _ => unreachable!(),
                                        },
                                        operand: dst.codegen().into(),
                                    },
                                ])
                            }
                        }
                    }
                }
            }
            IRInstruction::JumpIfZero { condition, target } => {
                let asm_type = get_asm_type(&condition);

                if asm_type == AsmType::Double {
                    AsmNode::Instructions(vec![
                        AsmInstruction::Binary { asm_type: AsmType::Double, op: AsmBinaryOp::Xor, lhs: AsmOperand::Register(AsmRegister::XMM0), rhs: AsmOperand::Register(AsmRegister::XMM0) },
                        AsmInstruction::Cmp { asm_type: AsmType::Double, lhs: condition.codegen().into(), rhs: AsmOperand::Register(AsmRegister::XMM0) },
                        AsmInstruction::JmpCC { condition: ConditionCode::E, target: target.clone() },                                    
                    ])
                } else {
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
            }
            IRInstruction::JumpIfNotZero { condition, target } => {
                let asm_type = get_asm_type(&condition);

                if asm_type == AsmType::Double {
                    AsmNode::Instructions(vec![
                        AsmInstruction::Binary { asm_type: AsmType::Double, op: AsmBinaryOp::Xor, lhs: AsmOperand::Register(AsmRegister::XMM0), rhs: AsmOperand::Register(AsmRegister::XMM0) },
                        AsmInstruction::Cmp { asm_type: AsmType::Double, lhs: condition.codegen().into(), rhs: AsmOperand::Register(AsmRegister::XMM0) },
                        AsmInstruction::JmpCC { condition: ConditionCode::E, target: target.clone() },                                    
                    ])
                } else {
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
                let int_registers = [
                    AsmRegister::DI,
                    AsmRegister::SI,
                    AsmRegister::DX,
                    AsmRegister::CX,
                    AsmRegister::R8,
                    AsmRegister::R9,
                ];

                let double_registers = [
                    AsmRegister::XMM0,
                    AsmRegister::XMM1,
                    AsmRegister::XMM2,
                    AsmRegister::XMM3,
                    AsmRegister::XMM4,
                    AsmRegister::XMM5,
                    AsmRegister::XMM6,
                    AsmRegister::XMM7,
                ];

                let mut instructions = vec![];

                let (int_args, double_args, stack_args) = classify_parameters_from_irvalue(args.clone());

                let stack_padding = if stack_args.len() % 2 != 0 { 8 } else { 0 };

                if stack_padding != 0 {
                    instructions.push(AsmInstruction::AllocateStack(stack_padding));
                }

                for (reg_index, (reg_type, reg_arg)) in int_args.into_iter().enumerate() {
                    let reg = int_registers[reg_index];
                    instructions.push(AsmInstruction::Mov {
                        asm_type: reg_type,
                        src: reg_arg.into(),
                        dst: AsmOperand::Register(reg),
                    });
                }

                for (reg_index, (_reg_type, reg_arg)) in double_args.into_iter().enumerate() {
                    let reg = double_registers[reg_index];

                    instructions.push(AsmInstruction::Mov {
                        asm_type: AsmType::Double,
                        src: reg_arg.into(),
                        dst: AsmOperand::Register(reg),
                    });
                }

                for (arg_type, stack_arg) in stack_args.iter().rev() {
                    match stack_arg {
                        AsmNode::Operand(AsmOperand::Imm(_)) | AsmNode::Operand(AsmOperand::Register(_)) => {
                            if arg_type == &AsmType::Quadword || arg_type == &AsmType::Double {
                                instructions.push(AsmInstruction::Push(stack_arg.clone().into()));
                            } else {
                                instructions.push(AsmInstruction::Mov {
                                    asm_type: *arg_type,
                                    src: stack_arg.clone().into(),
                                    dst: AsmOperand::Register(AsmRegister::AX),
                                });
                                instructions.push(AsmInstruction::Push(AsmOperand::Register(AsmRegister::AX)));
                            }
                        }
                        _ => {
                            instructions.push(AsmInstruction::Mov {
                                asm_type: *arg_type,
                                src: stack_arg.clone().into(),
                                dst: AsmOperand::Register(AsmRegister::AX),
                            });
                            instructions.push(AsmInstruction::Push(AsmOperand::Register(AsmRegister::AX)));
                        }
                    }
                }

                instructions.push(AsmInstruction::Call(target.to_owned()));

                let bytes_to_remove = 8 * stack_args.len() + stack_padding;
                if bytes_to_remove != 0 {
                    instructions.push(AsmInstruction::DeallocateStack(bytes_to_remove));
                }

                let assembly_dst = dst.codegen().into();
                let return_type = get_asm_type(&dst);

                if return_type == AsmType::Double {
                    instructions.push(AsmInstruction::Mov {
                        asm_type: AsmType::Double,
                        src: AsmOperand::Register(AsmRegister::XMM0),
                        dst: assembly_dst,
                    });
                } else {
                    instructions.push(AsmInstruction::Mov {
                        asm_type: return_type,
                        src: AsmOperand::Register(AsmRegister::AX),
                        dst: assembly_dst,
                    });
                }

                AsmNode::Instructions(instructions)
            }
            IRInstruction::SignExtend { src, dst } => {
                AsmNode::Instructions(vec![AsmInstruction::Movsx {
                    src: src.codegen().into(),
                    dst: dst.codegen().into(),
                }])
            }
            IRInstruction::Truncate { src, dst } => {
                AsmNode::Instructions(vec![AsmInstruction::Mov {
                    asm_type: AsmType::Longword,
                    src: src.codegen().into(),
                    dst: dst.codegen().into(),
                }])
            }
            IRInstruction::ZeroExtend { src, dst } => {
                AsmNode::Instructions(vec![AsmInstruction::MovZeroExtend {
                    src: src.codegen().into(),
                    dst: dst.codegen().into(),
                }])
            }
            IRInstruction::UIntToDouble { src, dst } => {
                AsmNode::Instructions(vec![
                    AsmInstruction::MovZeroExtend { src: src.codegen().into(), dst: AsmOperand::Register(AsmRegister::AX) },
                    AsmInstruction::Cvtsi2sd { asm_type: AsmType::Quadword, src: AsmOperand::Register(AsmRegister::AX), dst: dst.codegen().into() }
                ])
            }
            IRInstruction::DoubleToInt { src, dst } => {
                AsmNode::Instructions(vec![
                    AsmInstruction::Cvttsd2si { asm_type: get_asm_type(dst), src: src.codegen().into(), dst: dst.codegen().into() },
                ])
            }
            IRInstruction::IntToDouble { src, dst } => {
                AsmNode::Instructions(vec![
                    AsmInstruction::Cvtsi2sd { asm_type: get_asm_type(src), src: src.codegen().into(), dst: dst.codegen().into() }
                ])
            }
            _ => {
                println!("self: {:?}", self);
                todo!()
            },
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
            AsmInstruction::Movsx { src, dst } => AsmInstruction::Movsx {
                src: src.replace_pseudo(),
                dst: dst.replace_pseudo(),
            },
            AsmInstruction::Div { operand, asm_type } => AsmInstruction::Div {
                asm_type: *asm_type,
                operand: operand.replace_pseudo(),
            },
            AsmInstruction::MovZeroExtend { src, dst } => AsmInstruction::MovZeroExtend {
                src: src.replace_pseudo(),
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
            _ => self.clone(),
        }
    }
}

lazy_static::lazy_static! {
    static ref STATIC_CONSTANTS: Mutex<Vec<AsmStaticConstant>> = Mutex::new(Vec::new());
}

impl ReplacePseudo for AsmOperand {
    fn replace_pseudo(&self) -> Self {
        match self {
            AsmOperand::Imm(_) => self.clone(),
            AsmOperand::Pseudo(name) => {
                if let Some(symbol) = SYMBOL_TABLE.lock().unwrap().get(name).cloned() {
                    match symbol.attrs {
                        IdentifierAttrs::StaticAttr {
                            initial_value: _,
                            global: _,
                        } => AsmOperand::Data(name.clone()),
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

                            AsmOperand::Stack(
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

                    AsmOperand::Stack(
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
            static_constants: STATIC_CONSTANTS.lock().unwrap().to_owned(),
        }
    }
}

impl ReplacePseudo for AsmFunction {
    fn replace_pseudo(&self) -> Self {
        VAR_TO_STACK_POS.lock().unwrap().clear();
        let mut instructions = vec![];
        for instr in &self.instructions {
            instructions.push(instr.replace_pseudo());
        }

        let s = VAR_TO_STACK_POS.lock().unwrap().last_used_stack_pos.0.abs() as usize;

        // find next multiple of 16 of s
        let stack_space = (s + 15) & !15;

        AsmFunction {
            name: self.name.clone(),
            instructions,
            global: self.global,
            stack_space,
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
            AsmNode::StaticConstant(static_const) => AsmNode::StaticConstant(static_const.clone()),
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
            static_constants: self.static_constants.clone(),
        }
    }
}

impl Fixup for AsmFunction {
    fn fixup(&mut self) -> AsmFunction {
        let mut instructions = vec![];

        for instr in &mut self.instructions {
            match instr {
                AsmInstruction::Mov { src, dst, asm_type } => match (src, dst) {
                    (AsmOperand::Stack(src_n), AsmOperand::Stack(dst_n)) => {
                        match asm_type {
                            AsmType::Longword | AsmType::Quadword => {
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
                            AsmType::Double => {
                                instructions.extend(vec![
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: AsmOperand::Stack(*src_n),
                                        dst: AsmOperand::Register(AsmRegister::XMM14),
                                    },
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: AsmOperand::Register(AsmRegister::XMM14),
                                        dst: AsmOperand::Stack(*dst_n),
                                    },
                                ]);        
                            }
                        }
                    }
                    (AsmOperand::Data(src), AsmOperand::Stack(dst_n)) => {
                        match asm_type {
                            AsmType::Longword | AsmType::Quadword => {
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
                            AsmType::Double => {
                                instructions.extend(vec![
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: AsmOperand::Data(src.clone()),
                                        dst: AsmOperand::Register(AsmRegister::XMM14),
                                    },
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: AsmOperand::Register(AsmRegister::XMM14),
                                        dst: AsmOperand::Stack(*dst_n),
                                    },
                                ]);        
                            }
                        }
                    }
                    (AsmOperand::Stack(src_n), AsmOperand::Data(dst)) => {
                        println!("HERE (Stack, Data)");
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
                        println!("HERE (Data, Data)");
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
                    (AsmOperand::Imm(konst), AsmOperand::Stack(dst_n)) => match asm_type {
                        AsmType::Longword => {
                            if *konst < i32::MIN as i64 || *konst > i32::MAX as i64 {
                                instructions.extend(vec![
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: AsmOperand::Imm(*konst as i32 as i64),
                                        dst: AsmOperand::Register(AsmRegister::R10),
                                    },
                                    AsmInstruction::Mov {
                                        asm_type: *asm_type,
                                        src: AsmOperand::Register(AsmRegister::R10),
                                        dst: AsmOperand::Stack(*dst_n),
                                    },
                                ]);
                            } else {
                                instructions.extend(vec![AsmInstruction::Mov {
                                    asm_type: asm_type.clone(),
                                    src: AsmOperand::Imm(*konst),
                                    dst: AsmOperand::Stack(*dst_n),
                                }]);
                            }
                        }
                        AsmType::Quadword => {
                            if *konst < i32::MIN as i64 || *konst > i32::MAX as i64 {
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
                            } else {
                                instructions.extend(vec![AsmInstruction::Mov {
                                    asm_type: asm_type.clone(),
                                    src: AsmOperand::Imm(*konst),
                                    dst: AsmOperand::Stack(*dst_n),
                                }]);
                            }
                        }
                        _ => todo!(),
                    },
                    (AsmOperand::Imm(konst), AsmOperand::Data(dst)) => {
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
                },
                AsmInstruction::Binary {
                    op,
                    lhs,
                    rhs,
                    asm_type,
                } => match op {
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
                            } else {
                                instructions.push(instr.clone());
                            }
                        }
                        _ => instructions.push(instr.clone()),
                    },
                    AsmBinaryOp::Mul => {
                        match (lhs.clone(), rhs.clone()) {
                            (AsmOperand::Stack(_), AsmOperand::Stack(_)) => {
                                match asm_type {
                                    AsmType::Longword | AsmType::Quadword => {
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
                                }
                            }
                            (AsmOperand::Data(_), AsmOperand::Stack(_)) => {
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
                        }
                    }
                    _ => todo!(),
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
                            AsmInstruction::Idiv {
                                asm_type: AsmType::Quadword,
                                operand: AsmOperand::Register(AsmRegister::R10),
                            },
                        ]);
                    } else {
                        instructions.push(instr.clone());
                    }
                }
                AsmInstruction::MovZeroExtend { src, dst } => match dst {
                    AsmOperand::Register(reg) => {
                        instructions.extend(vec![AsmInstruction::Mov {
                            asm_type: AsmType::Longword,
                            src: src.clone(),
                            dst: AsmOperand::Register(*reg),
                        }]);
                    }
                    AsmOperand::Stack(dst_n) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Longword,
                                src: src.clone(),
                                dst: AsmOperand::Register(AsmRegister::R11),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: AsmOperand::Register(AsmRegister::R11),
                                dst: AsmOperand::Stack(*dst_n),
                            },
                        ]);
                    }
                    AsmOperand::Data(_) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Longword,
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
                AsmInstruction::Movsx { src, dst } => match (src.clone(), dst.clone()) {
                    (AsmOperand::Imm(konst), AsmOperand::Stack(dst_n)) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Longword,
                                src: AsmOperand::Imm(konst),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Movsx {
                                src: AsmOperand::Register(AsmRegister::R10),
                                dst: AsmOperand::Register(AsmRegister::R11),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: AsmOperand::Register(AsmRegister::R11),
                                dst: AsmOperand::Stack(dst_n),
                            },
                        ]);
                    }
                    (_, AsmOperand::Stack(_dst_n)) => {
                        instructions.extend(vec![
                            AsmInstruction::Movsx {
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
                },
                AsmInstruction::Cvtsi2sd { asm_type, src, dst } => match src {
                    AsmOperand::Imm(n) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                asm_type: asm_type.clone(),
                                src: AsmOperand::Imm(*n),
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
                    _ => instructions.push(instr.clone()),
                },
                AsmInstruction::Cvttsd2si { asm_type, src, dst } => {
                    match (src.clone(), dst.clone()) {
                        (AsmOperand::Stack(_), AsmOperand::Stack(_)) => {
                            instructions.extend(vec![
                                AsmInstruction::Cvttsd2si { asm_type: asm_type.clone(), src: src.clone(), dst: AsmOperand::Register(AsmRegister::R11) },
                                AsmInstruction::Mov {
                                    asm_type: asm_type.clone(),
                                    src: AsmOperand::Register(AsmRegister::R11),
                                    dst: dst.clone(),
                                },
                            ]);
                        }
                        _ => instructions.push(instr.clone()),
                    }
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

impl From<AsmNode> for AsmStaticConstant {
    fn from(value: AsmNode) -> Self {
        match value {
            AsmNode::StaticConstant(static_const) => static_const,
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

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum AsmType {
    Longword,
    Quadword,
    Double,
}

fn classify_parameters_from_irvalue(parameters: Vec<IRValue>) -> (Vec<(AsmType, AsmNode)>, Vec<(AsmType, AsmNode)>, Vec<(AsmType, AsmNode)>) {
    let mut int_reg_args = vec![];
    let mut double_reg_args = vec![];
    let mut stack_args = vec![];

    for parameter in parameters {
        let type_of_param = get_asm_type(&parameter);
        let operand: AsmNode = parameter.codegen().into();

        let typed_operand = (type_of_param, operand);

        if type_of_param == AsmType::Double {
            if double_reg_args.len() < 8 {
                double_reg_args.push(typed_operand);
            } else {
                stack_args.push(typed_operand);
            }
        } else {
            if int_reg_args.len() < 6 {
                int_reg_args.push(typed_operand);
            } else {
                stack_args.push(typed_operand);
            }
        }
    }

    (int_reg_args, double_reg_args, stack_args)
}

fn classify_parameters(parameters: Vec<String>) -> (Vec<(AsmType, String)>, Vec<(AsmType, String)>, Vec<(AsmType, String)>) {
    let mut int_reg_args = vec![];
    let mut double_reg_args = vec![];
    let mut stack_args = vec![];

    for parameter in parameters {
        let param_entry = ASM_SYMBOL_TABLE.lock().unwrap().get(&parameter).unwrap().clone();
        let type_of_param = match param_entry {
            AsmSymtabEntry::Object { _type, .. } => _type,
            _ => unreachable!(),
        };

        match type_of_param {
            AsmType::Double => {
                if double_reg_args.len() < 8 {
                    double_reg_args.push((type_of_param, parameter));
                } else {
                    stack_args.push((type_of_param, parameter));
                }
            }
            _ => {
                if int_reg_args.len() < 6 {
                    int_reg_args.push((type_of_param, parameter));
                } else {
                    stack_args.push((type_of_param, parameter));
                }
            }
        }
    }

    (int_reg_args, double_reg_args, stack_args)
}


fn get_asm_type(value: &IRValue) -> AsmType {
    match value {
        IRValue::Constant(konst) => match konst {
            Const::Int(_) => AsmType::Longword,
            Const::Long(_) => AsmType::Quadword,
            Const::UInt(_) => AsmType::Longword,
            Const::ULong(_) => AsmType::Quadword,
            Const::Double(_) => AsmType::Double,
            _ => todo!(),
        },
        IRValue::Var(var_name) => match SYMBOL_TABLE
            .lock()
            .unwrap()
            .get(var_name)
            .cloned()
            .unwrap()
            ._type
        {
            Type::Int => AsmType::Longword,
            Type::Long => AsmType::Quadword,
            Type::Uint => AsmType::Longword,
            Type::Ulong => AsmType::Quadword,
            Type::Double => AsmType::Double,
            _ => unreachable!(),
        },
    }
}

pub fn build_asm_symbol_table() {
    let frontend_symtab = SYMBOL_TABLE.lock().unwrap();
    let mut asm_symbol_table = ASM_SYMBOL_TABLE.lock().unwrap();

    for (identifier, symbol) in frontend_symtab.iter() {
        let entry = match symbol.attrs {
            IdentifierAttrs::FuncAttr { defined, .. } => AsmSymtabEntry::Function { defined },
            IdentifierAttrs::StaticAttr {
                initial_value: _, ..
            } => {
                let asm_type = match symbol._type {
                    Type::Int => AsmType::Longword,
                    Type::Long => AsmType::Quadword,
                    Type::Uint => AsmType::Longword,
                    Type::Ulong => AsmType::Quadword,
                    Type::Double => AsmType::Double,
                    _ => panic!("Unsupported type for static variable"),
                };
                AsmSymtabEntry::Object {
                    _type: asm_type,
                    is_static: true,
                    is_constant: false,
                }
            }
            IdentifierAttrs::LocalAttr => {
                let asm_type = match symbol._type {
                    Type::Int => AsmType::Longword,
                    Type::Long => AsmType::Quadword,
                    Type::Uint => AsmType::Longword,
                    Type::Ulong => AsmType::Quadword,
                    Type::Double => AsmType::Double,
                    _ => {
                        panic!("Unsupported type for static backend_symtab: {}", identifier);
                    }
                };
                AsmSymtabEntry::Object {
                    _type: asm_type,
                    is_static: false,
                    is_constant: false,
                }
            }
        };

        asm_symbol_table.insert(identifier.clone(), entry);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmSymtabEntry {
    Function {
        defined: bool,
    },
    Object {
        _type: AsmType,
        is_static: bool,
        is_constant: bool,
    },
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct StackPosition(pub i64);

pub struct VarToStackPos {
    last_used_stack_pos: StackPosition,
    var_to_stack_pos: HashMap<String, StackPosition>,
}

impl Default for VarToStackPos {
    fn default() -> Self {
        Self {
            last_used_stack_pos: StackPosition(0),
            var_to_stack_pos: HashMap::new(),
        }
    }
}

impl VarToStackPos {
    pub fn var_to_stack_pos(&mut self, ident: &str, asm_type: AsmType) -> StackPosition {
        let pos = self
            .var_to_stack_pos
            .entry(ident.to_owned())
            .or_insert_with(|| {
                let alloc = OperandByteLen::from(asm_type) as i64;
                self.last_used_stack_pos.0 -= alloc;

                let alignment = Alignment::default_of(asm_type) as i64;
                let rem = self.last_used_stack_pos.0 % alignment;
                if rem != 0 {
                    self.last_used_stack_pos.0 -= alignment + rem;
                }

                self.last_used_stack_pos
            });
        *pos
    }

    pub fn clear(&mut self) {
        self.var_to_stack_pos.clear();
        self.last_used_stack_pos = StackPosition(0);
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum OperandByteLen {
    B1 = 1,
    B4 = 4,
    B8 = 8,
}

impl<T: Into<AsmType>> From<T> for OperandByteLen {
    fn from(t: T) -> Self {
        let asm_type = t.into();
        match asm_type {
            AsmType::Longword => Self::B4,
            AsmType::Quadword => Self::B8,
            AsmType::Double => Self::B8,
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum Alignment {
    B4 = 4,
    B8 = 8,
    B16 = 16,
}

impl Alignment {
    /// The required alignment, in stack and in asm sections (`.data`, `.rodata`, `.literal8`, ...),
    ///   according to System V x64 ABI.
    /// On any individual item, the actual declared alignment may be a multiple of this basic amount,
    ///   eg b/c an instruction accessing it requires the operand to be aligned a certain amount.
    pub fn default_of<T: Into<AsmType>>(t: T) -> Self {
        let asm_type = t.into();
        match asm_type {
            AsmType::Longword => Self::B4,
            AsmType::Quadword => Self::B8,
            AsmType::Double => Self::B8,
        }
    }
}

lazy_static::lazy_static! {
    pub static ref ASM_SYMBOL_TABLE: Mutex<HashMap<String, AsmSymtabEntry>> = Mutex::new(HashMap::new());
    static ref VAR_TO_STACK_POS: Mutex<VarToStackPos> = Mutex::new(VarToStackPos::default());
}
