use std::{collections::HashMap, sync::Mutex};

use crate::{
    ir::{
        make_temporary, BinaryOp, IRFunction, IRInstruction, IRNode, IRProgram, IRStaticConstant,
        IRStaticVariable, IRValue, UnaryOp,
    },
    lexer::Const,
    parser::Type,
    typechecker::{
        get_common_type, get_signedness, get_size_of_type, IdentifierAttrs, StaticInit,
        SYMBOL_TABLE,
    },
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
    pub init: Vec<StaticInit>,
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
        src_type: AsmType,
        src: AsmOperand,
        dst_type: AsmType,
        dst: AsmOperand,
    },
    MovZeroExtend {
        src_type: AsmType,
        src: AsmOperand,
        dst_type: AsmType,
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
    Lea {
        src: AsmOperand,
        dst: AsmOperand,
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
    Memory(AsmRegister, isize),
    Register(AsmRegister),
    Data(String, isize),
    PseudoMem(String, isize),
    Indexed(AsmRegister, AsmRegister, isize),
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
    BP,
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
    Shl,
    ShrTwoOp,
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
            IRNode::StaticConstant(static_const) => static_const.codegen(),
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
                Type::Double => 8,
                Type::Pointer(_) => 8,
                Type::Array { .. } => get_alignment_of_type(&self._type),
                Type::Char | Type::UChar | Type::SChar => 1,
                _ => unreachable!(),
            },
        })
    }
}

impl Codegen for IRStaticConstant {
    fn codegen(&self) -> AsmNode {
        AsmNode::StaticConstant(AsmStaticConstant {
            name: self.name.clone(),
            init: self.init.clone(),
            alignment: get_alignment_of_type(&self._type),
        })
    }
}

fn get_alignment_of_type(t: &Type) -> usize {
    match t {
        Type::Char | Type::UChar | Type::SChar => 1,
        Type::Int => 4,
        Type::Long => 8,
        Type::Uint => 4,
        Type::Ulong => 8,
        Type::Double => 8,
        Type::Pointer(_) => 8,
        Type::Array { element, size } => {
            if get_size_of_type(element) * size >= 16 {
                16
            } else {
                get_alignment_of_type(element)
            }
        }
        _ => unreachable!(),
    }
}

impl Codegen for IRFunction {
    fn codegen(&self) -> AsmNode {
        let mut instructions = vec![];

        instructions.push(AsmInstruction::AllocateStack(0));

        let (int_reg_params, double_reg_params, stack_params) = classify_parameters(&self.params);

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
                asm_type: *param_type,
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
                src: AsmOperand::Memory(AsmRegister::BP, offset),
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
                    AsmNode::Operand(AsmOperand::Data(static_const.name, 0))
                }
                Const::Char(n) => AsmNode::Operand(AsmOperand::Imm(*n as i64)),
                Const::UChar(n) => AsmNode::Operand(AsmOperand::Imm(*n as i64)),
            },
            IRValue::Var(name) => {
                let symbol = SYMBOL_TABLE.lock().unwrap().get(name).cloned().unwrap();
                let type_of_symbol = symbol._type.clone();

                match type_of_symbol {
                    Type::Array { .. } => {
                        AsmNode::Operand(AsmOperand::PseudoMem(name.to_owned(), 0))
                    }
                    _ => AsmNode::Operand(AsmOperand::Pseudo(name.to_owned())),
                }
            }
        }
    }
}

impl Codegen for IRInstruction {
    fn codegen(&self) -> AsmNode {
        match self {
            IRInstruction::Unary { op, src, dst } => {
                let asm_type = get_asm_type(src);

                match op {
                    UnaryOp::Complement => AsmNode::Instructions(vec![
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
                    UnaryOp::Negate => {
                        if asm_type == AsmType::Double {
                            let static_const = AsmStaticConstant {
                                name: format!("static_const.{}", make_temporary()),
                                init: StaticInit::Double(-0.0),
                                alignment: 16,
                            };

                            STATIC_CONSTANTS.lock().unwrap().push(static_const.clone());

                            AsmNode::Instructions(vec![
                                AsmInstruction::Mov {
                                    asm_type: AsmType::Double,
                                    src: src.codegen().into(),
                                    dst: dst.codegen().into(),
                                },
                                AsmInstruction::Binary {
                                    asm_type: AsmType::Double,
                                    op: AsmBinaryOp::Xor,
                                    lhs: AsmOperand::Data(static_const.name, 0),
                                    rhs: dst.codegen().into(),
                                },
                            ])
                        } else {
                            AsmNode::Instructions(vec![
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
                            ])
                        }
                    }
                    UnaryOp::Not => {
                        if asm_type == AsmType::Double {
                            AsmNode::Instructions(vec![
                                AsmInstruction::Binary {
                                    asm_type: AsmType::Double,
                                    op: AsmBinaryOp::Xor,
                                    lhs: AsmOperand::Register(AsmRegister::XMM0),
                                    rhs: AsmOperand::Register(AsmRegister::XMM0),
                                },
                                AsmInstruction::Cmp {
                                    asm_type: AsmType::Double,
                                    lhs: src.codegen().into(),
                                    rhs: AsmOperand::Register(AsmRegister::XMM0),
                                },
                                AsmInstruction::Mov {
                                    asm_type: get_asm_type(dst),
                                    src: AsmOperand::Imm(0),
                                    dst: dst.codegen().into(),
                                },
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
                if value.is_some() {
                    let asm_type = get_asm_type(&value.clone().unwrap());
                    if asm_type == AsmType::Double {
                        AsmNode::Instructions(vec![
                            AsmInstruction::Mov {
                                asm_type: AsmType::Double,
                                src: value.clone().unwrap().codegen().into(),
                                dst: AsmOperand::Register(AsmRegister::XMM0),
                            },
                            AsmInstruction::Ret,
                        ])
                    } else {
                        AsmNode::Instructions(vec![
                            AsmInstruction::Mov {
                                asm_type: get_asm_type(&value.clone().unwrap()),
                                src: value.clone().unwrap().codegen().into(),
                                dst: AsmOperand::Register(AsmRegister::AX),
                            },
                            AsmInstruction::Ret,
                        ])
                    }
                } else {
                    AsmNode::Instructions(vec![AsmInstruction::Ret])
                }
            }
            IRInstruction::Binary { op, lhs, rhs, dst } => {
                let asm_type = get_asm_type(lhs);

                match op {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul => AsmNode::Instructions(vec![
                        AsmInstruction::Mov {
                            asm_type: get_asm_type(dst),
                            src: lhs.codegen().into(),
                            dst: dst.codegen().into(),
                        },
                        AsmInstruction::Binary {
                            asm_type: get_asm_type(dst),
                            op: (*op).into(),
                            lhs: rhs.codegen().into(),
                            rhs: dst.codegen().into(),
                        },
                    ]),
                    BinaryOp::Div => {
                        match (&lhs, &rhs) {
                            (
                                IRValue::Constant(Const::Double(_)),
                                IRValue::Constant(Const::Double(_)),
                            ) => {
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
                                let var1_type =
                                    match SYMBOL_TABLE.lock().unwrap().get(var1).cloned() {
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
                                let var2_type =
                                    match SYMBOL_TABLE.lock().unwrap().get(var2).cloned() {
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
                                let var1_type =
                                    match SYMBOL_TABLE.lock().unwrap().get(var1).cloned() {
                                        Some(symbol) => symbol._type,
                                        None => unreachable!(),
                                    };

                                let var2_type =
                                    match SYMBOL_TABLE.lock().unwrap().get(var2).cloned() {
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
                                Const::Char(_) => true,
                                Const::UChar(_) => false,
                                Const::Double(_) => unreachable!(),
                            },
                        };

                        let is_pointer_src = match lhs {
                            IRValue::Var(name) => {
                                let symbol =
                                    SYMBOL_TABLE.lock().unwrap().get(name).cloned().unwrap();
                                matches!(symbol._type, Type::Pointer(_))
                            }
                            _ => false,
                        };

                        if signedness || (is_pointer_src) {
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
                            ]);
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
                                Const::Char(_) => true,
                                Const::UChar(_) => false,
                                Const::Double(_) => unreachable!(),
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
                                AsmInstruction::Idiv {
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
                                Const::Char(_) => Type::Char,
                                Const::UChar(_) => Type::UChar,
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
                                Const::Char(_) => Type::Char,
                                Const::UChar(_) => Type::UChar,
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
                            let signedness = get_signedness(get_common_type(&lhs_type, &rhs_type));

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
                let asm_type = get_asm_type(condition);

                if asm_type == AsmType::Double {
                    AsmNode::Instructions(vec![
                        AsmInstruction::Binary {
                            asm_type: AsmType::Double,
                            op: AsmBinaryOp::Xor,
                            lhs: AsmOperand::Register(AsmRegister::XMM0),
                            rhs: AsmOperand::Register(AsmRegister::XMM0),
                        },
                        AsmInstruction::Cmp {
                            asm_type: AsmType::Double,
                            lhs: condition.codegen().into(),
                            rhs: AsmOperand::Register(AsmRegister::XMM0),
                        },
                        AsmInstruction::JmpCC {
                            condition: ConditionCode::E,
                            target: target.clone(),
                        },
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
                let asm_type = get_asm_type(condition);

                if asm_type == AsmType::Double {
                    AsmNode::Instructions(vec![
                        AsmInstruction::Binary {
                            asm_type: AsmType::Double,
                            op: AsmBinaryOp::Xor,
                            lhs: AsmOperand::Register(AsmRegister::XMM0),
                            rhs: AsmOperand::Register(AsmRegister::XMM0),
                        },
                        AsmInstruction::Cmp {
                            asm_type: AsmType::Double,
                            lhs: condition.codegen().into(),
                            rhs: AsmOperand::Register(AsmRegister::XMM0),
                        },
                        AsmInstruction::JmpCC {
                            condition: ConditionCode::NE,
                            target: target.clone(),
                        },
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
                let asm_type = get_asm_type(src);

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

                let (int_args, double_args, stack_args) = classify_parameters_from_irvalue(args);

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
                        AsmNode::Operand(AsmOperand::Imm(_))
                        | AsmNode::Operand(AsmOperand::Register(_)) => {
                            instructions.push(AsmInstruction::Push(stack_arg.clone().into()));
                        }
                        _ => {
                            if arg_type == &AsmType::Quadword || arg_type == &AsmType::Double {
                                instructions.push(AsmInstruction::Push(stack_arg.clone().into()));
                            } else {
                                instructions.push(AsmInstruction::Mov {
                                    asm_type: *arg_type,
                                    src: stack_arg.clone().into(),
                                    dst: AsmOperand::Register(AsmRegister::AX),
                                });
                                instructions.push(AsmInstruction::Push(AsmOperand::Register(
                                    AsmRegister::AX,
                                )));
                            }
                        }
                    }
                }

                instructions.push(AsmInstruction::Call(target.to_owned()));

                let bytes_to_remove = 8 * stack_args.len() + stack_padding;
                if bytes_to_remove != 0 {
                    instructions.push(AsmInstruction::DeallocateStack(bytes_to_remove));
                }

                if dst.is_some() {
                    let dst = dst.clone().unwrap();
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
                }

                AsmNode::Instructions(instructions)
            }
            IRInstruction::SignExtend { src, dst } => {
                AsmNode::Instructions(vec![AsmInstruction::Movsx {
                    src_type: get_asm_type(src),
                    src: src.codegen().into(),
                    dst_type: get_asm_type(dst),
                    dst: dst.codegen().into(),
                }])
            }
            IRInstruction::Truncate { src, dst } => {
                AsmNode::Instructions(vec![AsmInstruction::Mov {
                    asm_type: get_asm_type(dst),
                    src: src.codegen().into(),
                    dst: dst.codegen().into(),
                }])
            }
            IRInstruction::ZeroExtend { src, dst } => {
                AsmNode::Instructions(vec![AsmInstruction::MovZeroExtend {
                    src_type: get_asm_type(src),
                    src: src.codegen().into(),
                    dst_type: get_asm_type(dst),
                    dst: dst.codegen().into(),
                }])
            }
            IRInstruction::UIntToDouble { src, dst } => {
                let src_type = get_asm_type(src);
                match src_type {
                    AsmType::Byte => AsmNode::Instructions(vec![
                        AsmInstruction::MovZeroExtend {
                            src_type: AsmType::Byte,
                            src: src.codegen().into(),
                            dst_type: AsmType::Longword,
                            dst: AsmOperand::Register(AsmRegister::AX),
                        },
                        AsmInstruction::Cvtsi2sd {
                            asm_type: AsmType::Longword,
                            src: AsmOperand::Register(AsmRegister::AX),
                            dst: dst.codegen().into(),
                        },
                    ]),
                    AsmType::Longword => AsmNode::Instructions(vec![
                        AsmInstruction::MovZeroExtend {
                            src_type: AsmType::Longword,
                            src: src.codegen().into(),
                            dst_type: AsmType::Longword,
                            dst: AsmOperand::Register(AsmRegister::AX),
                        },
                        AsmInstruction::Cvtsi2sd {
                            asm_type: AsmType::Quadword,
                            src: AsmOperand::Register(AsmRegister::AX),
                            dst: dst.codegen().into(),
                        },
                    ]),
                    AsmType::Quadword => {
                        let label1 = format!("label_1.{}", make_temporary());
                        let label2 = format!("label_2.{}", make_temporary());

                        AsmNode::Instructions(vec![
                            AsmInstruction::Cmp {
                                asm_type: AsmType::Quadword,
                                lhs: AsmOperand::Imm(0),
                                rhs: src.codegen().into(),
                            },
                            AsmInstruction::JmpCC {
                                condition: ConditionCode::L,
                                target: label1.clone(),
                            },
                            /* Below: src f64 < (1<<63) */
                            AsmInstruction::Cvtsi2sd {
                                asm_type: AsmType::Quadword,
                                src: src.codegen().into(),
                                dst: dst.codegen().into(),
                            },
                            AsmInstruction::Jmp {
                                target: label2.clone(),
                            },
                            /* Below: src f64 >= (1<<63) */
                            AsmInstruction::Label(label1.clone()),
                            AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: src.codegen().into(),
                                dst: AsmOperand::Register(AsmRegister::AX),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: AsmOperand::Register(AsmRegister::AX),
                                dst: AsmOperand::Register(AsmRegister::DX),
                            },
                            AsmInstruction::Unary {
                                asm_type: AsmType::Quadword,
                                op: AsmUnaryOp::Shr,
                                operand: AsmOperand::Register(AsmRegister::DX),
                            },
                            AsmInstruction::Binary {
                                op: AsmBinaryOp::And,
                                asm_type: AsmType::Quadword,
                                lhs: AsmOperand::Imm(1),
                                rhs: AsmOperand::Register(AsmRegister::AX),
                            }, // src f64 - (1<<63)f64
                            AsmInstruction::Binary {
                                op: AsmBinaryOp::Or,
                                asm_type: AsmType::Quadword,
                                lhs: AsmOperand::Register(AsmRegister::AX),
                                rhs: AsmOperand::Register(AsmRegister::DX),
                            }, // src f64 - (1<<63)f64
                            AsmInstruction::Cvtsi2sd {
                                asm_type: AsmType::Quadword,
                                src: AsmOperand::Register(AsmRegister::DX),
                                dst: dst.codegen().into(),
                            },
                            AsmInstruction::Binary {
                                op: AsmBinaryOp::Add,
                                asm_type: AsmType::Double,
                                lhs: dst.codegen().into(),
                                rhs: dst.codegen().into(),
                            }, // (i64 as u64) + (1<<63)
                            AsmInstruction::Label(label2.clone()),
                        ])
                    }
                    _ => unreachable!(),
                }
            }
            IRInstruction::DoubleToInt { src, dst } => {
                let dst_type = get_asm_type(dst);
                match dst_type {
                    AsmType::Byte => AsmNode::Instructions(vec![
                        AsmInstruction::Cvttsd2si {
                            asm_type: AsmType::Longword,
                            src: src.codegen().into(),
                            dst: AsmOperand::Register(AsmRegister::AX),
                        },
                        AsmInstruction::Mov {
                            asm_type: AsmType::Byte,
                            src: AsmOperand::Register(AsmRegister::AX),
                            dst: dst.codegen().into(),
                        },
                    ]),
                    AsmType::Longword | AsmType::Quadword => {
                        AsmNode::Instructions(vec![AsmInstruction::Cvttsd2si {
                            asm_type: get_asm_type(dst),
                            src: src.codegen().into(),
                            dst: dst.codegen().into(),
                        }])
                    }
                    _ => unreachable!(),
                }
            }
            IRInstruction::DoubletoUInt { src, dst } => {
                let dst_type = get_asm_type(dst);
                match dst_type {
                    AsmType::Byte => AsmNode::Instructions(vec![
                        AsmInstruction::Cvttsd2si {
                            asm_type: AsmType::Longword,
                            src: src.codegen().into(),
                            dst: AsmOperand::Register(AsmRegister::AX),
                        },
                        AsmInstruction::Mov {
                            asm_type: AsmType::Byte,
                            src: AsmOperand::Register(AsmRegister::AX),
                            dst: dst.codegen().into(),
                        },
                    ]),
                    AsmType::Longword => AsmNode::Instructions(vec![
                        AsmInstruction::Cvttsd2si {
                            asm_type: AsmType::Quadword,
                            src: src.codegen().into(),
                            dst: AsmOperand::Register(AsmRegister::AX),
                        },
                        AsmInstruction::Mov {
                            asm_type: AsmType::Longword,
                            src: AsmOperand::Register(AsmRegister::AX),
                            dst: dst.codegen().into(),
                        },
                    ]),
                    AsmType::Quadword => {
                        let i64_ceil_as_u64 = (i64::MAX as u64) + 1;
                        let i64_ceil_as_f64 = i64_ceil_as_u64 as f64;

                        // create constant
                        let static_constant = AsmStaticConstant {
                            name: format!("static_const.{}", make_temporary()),
                            init: StaticInit::Double(i64_ceil_as_f64),
                            alignment: 8,
                        };
                        STATIC_CONSTANTS
                            .lock()
                            .unwrap()
                            .push(static_constant.clone());

                        let label_out_of_range = format!("label_out_of_range.{}", make_temporary());
                        let label_end = format!("label_end.{}", make_temporary());

                        AsmNode::Instructions(vec![
                            AsmInstruction::Cmp {
                                asm_type: AsmType::Double,
                                lhs: AsmOperand::Data(static_constant.name.clone(), 0),
                                rhs: src.codegen().into(),
                            },
                            AsmInstruction::JmpCC {
                                condition: ConditionCode::AE,
                                target: label_out_of_range.clone(),
                            },
                            /* Below: src f64 < (1<<63) */
                            AsmInstruction::Cvttsd2si {
                                asm_type: AsmType::Quadword,
                                src: src.codegen().into(),
                                dst: dst.codegen().into(),
                            },
                            AsmInstruction::Jmp {
                                target: label_end.clone(),
                            },
                            /* Below: src f64 >= (1<<63) */
                            AsmInstruction::Label(label_out_of_range),
                            AsmInstruction::Mov {
                                asm_type: AsmType::Double,
                                src: src.codegen().into(),
                                dst: AsmOperand::Register(AsmRegister::XMM0),
                            },
                            AsmInstruction::Binary {
                                op: AsmBinaryOp::Sub,
                                asm_type: AsmType::Double,
                                lhs: AsmOperand::Data(static_constant.name, 0),
                                rhs: AsmOperand::Register(AsmRegister::XMM0),
                            }, // src f64 - (1<<63)f64
                            AsmInstruction::Cvttsd2si {
                                asm_type: AsmType::Quadword,
                                src: AsmOperand::Register(AsmRegister::XMM0),
                                dst: dst.codegen().into(),
                            }, // ( src f64 - (1<<63)f64 ) as i64
                            AsmInstruction::Binary {
                                op: AsmBinaryOp::Add,
                                asm_type: AsmType::Quadword,
                                lhs: dst.codegen().into(),
                                rhs: AsmOperand::Imm(i64_ceil_as_u64 as i64),
                            }, // (i64 as u64) + (1<<63)
                            AsmInstruction::Label(label_end.clone()),
                        ])
                    }
                    _ => todo!(),
                }
            }
            IRInstruction::IntToDouble { src, dst } => {
                let src_type = get_asm_type(src);
                match src_type {
                    AsmType::Byte => AsmNode::Instructions(vec![
                        AsmInstruction::Movsx {
                            src_type: AsmType::Byte,
                            src: src.codegen().into(),
                            dst_type: AsmType::Longword,
                            dst: AsmOperand::Register(AsmRegister::AX),
                        },
                        AsmInstruction::Cvtsi2sd {
                            asm_type: AsmType::Longword,
                            src: AsmOperand::Register(AsmRegister::AX),
                            dst: dst.codegen().into(),
                        },
                    ]),
                    _ => AsmNode::Instructions(vec![AsmInstruction::Cvtsi2sd {
                        asm_type: get_asm_type(src),
                        src: src.codegen().into(),
                        dst: dst.codegen().into(),
                    }]),
                }
            }
            IRInstruction::Load { src_ptr, dst } => AsmNode::Instructions(vec![
                AsmInstruction::Mov {
                    asm_type: AsmType::Quadword,
                    src: src_ptr.codegen().into(),
                    dst: AsmOperand::Register(AsmRegister::R9),
                },
                AsmInstruction::Mov {
                    asm_type: get_asm_type(dst),
                    src: AsmOperand::Memory(AsmRegister::R9, 0),
                    dst: dst.codegen().into(),
                },
            ]),
            IRInstruction::Store { src, dst_ptr } => AsmNode::Instructions(vec![
                AsmInstruction::Mov {
                    asm_type: AsmType::Quadword,
                    src: dst_ptr.codegen().into(),
                    dst: AsmOperand::Register(AsmRegister::R9),
                },
                AsmInstruction::Mov {
                    asm_type: get_asm_type(src),
                    src: src.codegen().into(),
                    dst: AsmOperand::Memory(AsmRegister::R9, 0),
                },
            ]),
            IRInstruction::GetAddress { src, dst } => {
                AsmNode::Instructions(vec![AsmInstruction::Lea {
                    src: src.codegen().into(),
                    dst: dst.codegen().into(),
                }])
            }
            IRInstruction::AddPtr {
                ptr,
                index,
                scale,
                dst,
            } => match scale {
                1 | 2 | 4 | 8 => AsmNode::Instructions(vec![
                    AsmInstruction::Mov {
                        asm_type: AsmType::Quadword,
                        src: ptr.codegen().into(),
                        dst: AsmOperand::Register(AsmRegister::AX),
                    },
                    AsmInstruction::Mov {
                        asm_type: AsmType::Quadword,
                        src: index.codegen().into(),
                        dst: AsmOperand::Register(AsmRegister::DX),
                    },
                    AsmInstruction::Lea {
                        src: AsmOperand::Indexed(AsmRegister::AX, AsmRegister::DX, *scale as isize),
                        dst: dst.codegen().into(),
                    },
                ]),
                _ => AsmNode::Instructions(vec![
                    AsmInstruction::Mov {
                        asm_type: AsmType::Quadword,
                        src: ptr.codegen().into(),
                        dst: AsmOperand::Register(AsmRegister::AX),
                    },
                    AsmInstruction::Mov {
                        asm_type: AsmType::Quadword,
                        src: index.codegen().into(),
                        dst: AsmOperand::Register(AsmRegister::DX),
                    },
                    AsmInstruction::Binary {
                        asm_type: AsmType::Quadword,
                        op: AsmBinaryOp::Mul,
                        lhs: AsmOperand::Imm(*scale as i64),
                        rhs: AsmOperand::Register(AsmRegister::DX),
                    },
                    AsmInstruction::Lea {
                        src: AsmOperand::Indexed(AsmRegister::AX, AsmRegister::DX, 1),
                        dst: dst.codegen().into(),
                    },
                ]),
            },
            IRInstruction::CopyToOffset { src, dst, offset } => {
                let type_of_src = get_asm_type(src);
                AsmNode::Instructions(vec![AsmInstruction::Mov {
                    asm_type: type_of_src,
                    src: src.codegen().into(),
                    dst: AsmOperand::PseudoMem(dst.to_owned(), *offset as isize),
                }])
            }
            _ => todo!(),
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
            AsmOperand::Memory(AsmRegister::BP, _) => self.clone(),
            AsmOperand::Register(_) => self.clone(),
            AsmOperand::Data(_, _) => self.clone(),
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
                    AsmOperand::Data(name.clone(), 0)
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
        VAR_TO_STACK_POS.lock().unwrap().clear();
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
                AsmInstruction::Lea { src, dst } => {
                    // destination of Lea must be a register
                    match dst {
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
                    }
                }
                AsmInstruction::Mov {
                    asm_type: AsmType::Double,
                    src,
                    dst,
                } => match (&src, &dst) {
                    (AsmOperand::Memory(_, _), AsmOperand::Data(_, _)) => {
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
                    _ => instructions.push(instr.clone()),
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
                        (AsmOperand::Data(src, offset), AsmOperand::Memory(AsmRegister::BP, dst_n)) => {
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
                    AsmBinaryOp::Mul => {
                        match (&lhs, &rhs) {
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
                                // if konst can't fit in i32, we need to load it into a register
                                // and then multiply
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
                            _ => instructions.push(instr.clone()),
                        }
                    }
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
                } => match dst {
                    AsmOperand::Memory(_, _)
                    | AsmOperand::Data(_, _)
                    | AsmOperand::Indexed(_, _, _) => {
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
                    _ => {
                        instructions.push(instr.clone());
                    }
                },
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
                AsmInstruction::Cmp { lhs, rhs, asm_type } => {
                    match (&lhs, &rhs) {
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
                        (AsmOperand::Data(src, offset1), AsmOperand::Data(dst, offset2)) => match asm_type {
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
                        },
                        (AsmOperand::Imm(konst1), AsmOperand::Imm(konst2)) => {
                            // move one constant to r10, the other to r11
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
                    }
                }
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
    Byte,
    Longword,
    Quadword,
    Double,
    Bytearray { size: usize, alignment: usize },
}

type ParameterAsmNode = (AsmType, AsmNode);
type ParametersAsmNode = (
    Vec<ParameterAsmNode>,
    Vec<ParameterAsmNode>,
    Vec<ParameterAsmNode>,
);

fn classify_parameters_from_irvalue(parameters: &[IRValue]) -> ParametersAsmNode {
    let mut int_reg_args = vec![];
    let mut double_reg_args = vec![];
    let mut stack_args = vec![];

    for parameter in parameters {
        let type_of_param = get_asm_type(parameter);
        let operand: AsmNode = parameter.codegen();

        let typed_operand = (type_of_param, operand);

        if type_of_param == AsmType::Double {
            if double_reg_args.len() < 8 {
                double_reg_args.push(typed_operand);
            } else {
                stack_args.push(typed_operand);
            }
        } else if int_reg_args.len() < 6 {
            int_reg_args.push(typed_operand);
        } else {
            stack_args.push(typed_operand);
        }
    }

    (int_reg_args, double_reg_args, stack_args)
}

type ParameterString = (AsmType, String);
type ParametersString = (
    Vec<ParameterString>,
    Vec<ParameterString>,
    Vec<ParameterString>,
);

fn classify_parameters(parameters: &[String]) -> ParametersString {
    let mut int_reg_args = vec![];
    let mut double_reg_args = vec![];
    let mut stack_args = vec![];

    for parameter in parameters {
        let param_entry = ASM_SYMBOL_TABLE
            .lock()
            .unwrap()
            .get(parameter)
            .unwrap()
            .clone();
        let type_of_param = match param_entry {
            AsmSymtabEntry::Object { _type, .. } => _type,
            _ => unreachable!(),
        };

        match type_of_param {
            AsmType::Double => {
                if double_reg_args.len() < 8 {
                    double_reg_args.push((type_of_param, parameter.to_owned()));
                } else {
                    stack_args.push((type_of_param, parameter.to_owned()));
                }
            }
            _ => {
                if int_reg_args.len() < 6 {
                    int_reg_args.push((type_of_param, parameter.to_owned()));
                } else {
                    stack_args.push((type_of_param, parameter.to_owned()));
                }
            }
        }
    }

    (int_reg_args, double_reg_args, stack_args)
}

fn get_asm_type(value: &IRValue) -> AsmType {
    match value {
        IRValue::Constant(konst) => match konst {
            Const::Char(_) | Const::UChar(_) => AsmType::Byte,
            Const::Int(_) => AsmType::Longword,
            Const::Long(_) => AsmType::Quadword,
            Const::UInt(_) => AsmType::Longword,
            Const::ULong(_) => AsmType::Quadword,
            Const::Double(_) => AsmType::Double,
        },
        IRValue::Var(var_name) => {
            let _type = SYMBOL_TABLE
                .lock()
                .unwrap()
                .get(var_name)
                .cloned()
                .unwrap()
                ._type;
            match _type {
                Type::Char | Type::UChar | Type::SChar => AsmType::Byte,
                Type::Int => AsmType::Longword,
                Type::Long => AsmType::Quadword,
                Type::Uint => AsmType::Longword,
                Type::Ulong => AsmType::Quadword,
                Type::Double => AsmType::Double,
                Type::Pointer(_) => AsmType::Quadword,
                Type::Array { ref element, size } => AsmType::Bytearray {
                    size: get_size_of_type(element) * size,
                    alignment: get_alignment_of_type(&_type),
                },
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
            IdentifierAttrs::StaticAttr {
                initial_value: _, ..
            } => {
                let asm_type = match symbol._type {
                    Type::Char | Type::UChar | Type::SChar => AsmType::Byte,
                    Type::Int => AsmType::Longword,
                    Type::Long => AsmType::Quadword,
                    Type::Uint => AsmType::Longword,
                    Type::Ulong => AsmType::Quadword,
                    Type::Double => AsmType::Double,
                    Type::Pointer(_) => AsmType::Quadword,
                    Type::Array { ref element, size } => AsmType::Bytearray {
                        size: get_size_of_type(element) * size,
                        alignment: get_alignment_of_type(&symbol._type),
                    },
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
                    Type::Char | Type::UChar | Type::SChar => AsmType::Byte,
                    Type::Int => AsmType::Longword,
                    Type::Long => AsmType::Quadword,
                    Type::Uint => AsmType::Longword,
                    Type::Ulong => AsmType::Quadword,
                    Type::Double => AsmType::Double,
                    Type::Pointer(_) => AsmType::Quadword,
                    Type::Array { ref element, size } => AsmType::Bytearray {
                        size: get_size_of_type(element) * size,
                        alignment: get_alignment_of_type(&symbol._type),
                    },
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
            IdentifierAttrs::ConstantAttr(_) => {
                let asm_type = match symbol._type {
                    Type::Char | Type::UChar | Type::SChar => AsmType::Byte,
                    Type::Int => AsmType::Longword,
                    Type::Long => AsmType::Quadword,
                    Type::Uint => AsmType::Longword,
                    Type::Ulong => AsmType::Quadword,
                    Type::Double => AsmType::Double,
                    Type::Pointer(_) => AsmType::Quadword,
                    Type::Array { ref element, size } => AsmType::Bytearray {
                        size: get_size_of_type(element) * size,
                        alignment: get_alignment_of_type(&symbol._type),
                    },
                    _ => {
                        panic!("Unsupported type for static variable");
                    }
                };
                AsmSymtabEntry::Object {
                    _type: asm_type,
                    is_static: false,
                    is_constant: true,
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
                let alloc = match OperandByteLen::from(asm_type) {
                    OperandByteLen::B1 => 1,
                    OperandByteLen::B4 => 4,
                    OperandByteLen::B8 => 8,
                    OperandByteLen::Other(size) => size as i64,
                };
                self.last_used_stack_pos.0 -= alloc;

                let alignment = match Alignment::default_of(asm_type) {
                    Alignment::B1 => 1,
                    Alignment::B4 => 4,
                    Alignment::B8 => 8,
                    Alignment::B16 => 16,
                    Alignment::Other(size) => size,
                };
                let rem = self.last_used_stack_pos.0 % alignment as i64;
                if rem != 0 {
                    self.last_used_stack_pos.0 -= alignment as i64 + rem;
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

#[repr(i64)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum OperandByteLen {
    B1 = 1,
    B4 = 4,
    B8 = 8,
    Other(usize),
}

impl<T: Into<AsmType>> From<T> for OperandByteLen {
    fn from(t: T) -> Self {
        let asm_type = t.into();
        match asm_type {
            AsmType::Byte => Self::B1,
            AsmType::Longword => Self::B4,
            AsmType::Quadword => Self::B8,
            AsmType::Double => Self::B8,
            AsmType::Bytearray { size, alignment: _ } => Self::Other(size),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
#[repr(usize)]
pub enum Alignment {
    B1 = 1,
    B4 = 4,
    B8 = 8,
    B16 = 16,
    Other(usize),
}

impl Alignment {
    /// The required alignment, in stack and in asm sections (`.data`, `.rodata`, `.literal8`, ...),
    ///   according to System V x64 ABI.
    /// On any individual item, the actual declared alignment may be a multiple of this basic amount,
    ///   eg b/c an instruction accessing it requires the operand to be aligned a certain amount.
    pub fn default_of<T: Into<AsmType>>(t: T) -> Self {
        let asm_type = t.into();
        match asm_type {
            AsmType::Byte => Self::B1,
            AsmType::Longword => Self::B4,
            AsmType::Quadword => Self::B8,
            AsmType::Double => Self::B8,
            AsmType::Bytearray { size: _, alignment } => Self::Other(alignment),
        }
    }
}

lazy_static::lazy_static! {
    pub static ref ASM_SYMBOL_TABLE: Mutex<HashMap<String, AsmSymtabEntry>> = Mutex::new(HashMap::new());
    static ref VAR_TO_STACK_POS: Mutex<VarToStackPos> = Mutex::new(VarToStackPos::default());
}
