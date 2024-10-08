use std::{
    collections::{BTreeMap, BTreeSet},
    sync::Mutex,
};

use crate::{
    cfg::{self, BasicBlock, Instr, SimpleInstr, CFG},
    ir::{
        make_temporary, BinaryOp, IRFunction, IRInstruction, IRNode, IRProgram, IRStaticConstant,
        IRStaticVariable, IRValue, UnaryOp,
    },
    lexer::Const,
    parser::Type,
    typechecker::{
        get_common_type, get_signedness, get_size_of_type, is_scalar, IdentifierAttrs, MemberEntry,
        StaticInit, CURRENT_FN_RETURNS_ON_STACK, SYMBOL_TABLE, TYPE_TABLE,
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
    Pop(AsmRegister),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum AsmOperand {
    Imm(i64),
    Pseudo(String),
    Memory(AsmRegister, isize),
    Register(AsmRegister),
    Data(String, isize),
    PseudoMem(String, isize),
    Indexed(AsmRegister, AsmRegister, isize),
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq, PartialOrd, Ord)]
pub enum AsmRegister {
    AX,
    BX,
    CX,
    DX,
    DI,
    SI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
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
    XMM8,
    XMM9,
    XMM10,
    XMM11,
    XMM12,
    XMM13,
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
                Type::Struct { ref tag } => {
                    TYPE_TABLE
                        .lock()
                        .unwrap()
                        .get(tag)
                        .cloned()
                        .unwrap()
                        .alignment
                }
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
        Type::Struct { tag } => {
            let struct_type = TYPE_TABLE.lock().unwrap().get(tag).cloned().unwrap();
            struct_type.alignment
        }
        _ => unreachable!(),
    }
}

impl Codegen for IRFunction {
    fn codegen(&self) -> AsmNode {
        let return_on_stack = returns_on_stack(&self.name);
        let params_as_tacky = self
            .params
            .iter()
            .map(|x| IRValue::Var(x.clone()))
            .collect::<Vec<IRValue>>();

        let mut instructions = vec![];

        // instructions.push(AsmInstruction::AllocateStack(0));

        let (int_reg_params, double_reg_params, stack_params) =
            classify_parameters(&params_as_tacky, return_on_stack);

        let int_regs: [AsmRegister; 6] = [
            AsmRegister::DI,
            AsmRegister::SI,
            AsmRegister::DX,
            AsmRegister::CX,
            AsmRegister::R8,
            AsmRegister::R9,
        ];

        let mut reg_index = 0;

        if return_on_stack {
            instructions.push(AsmInstruction::Mov {
                asm_type: AsmType::Quadword,
                src: AsmOperand::Register(AsmRegister::DI),
                dst: AsmOperand::Memory(AsmRegister::BP, -8),
            });
            reg_index = 1;
        }

        for (param_type, param) in int_reg_params.iter() {
            let reg = int_regs[reg_index];

            match param_type {
                AsmType::Bytearray { size, alignment: _ } => {
                    copy_bytes_from_reg(&reg, &param, *size, &mut instructions);
                }
                _ => {
                    let instr = AsmInstruction::Mov {
                        asm_type: *param_type,
                        src: AsmOperand::Register(reg),
                        dst: param.clone(),
                    };
                    instructions.push(instr);
                }
            }

            reg_index += 1;
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

        for (idx, param) in double_reg_params.iter().enumerate() {
            let reg = double_regs[idx];
            let instr = AsmInstruction::Mov {
                asm_type: AsmType::Double,
                src: AsmOperand::Register(reg),
                dst: param.clone(),
            };
            instructions.push(instr);
        }

        let mut offset = 16;
        for (param_type, param) in stack_params {
            match param_type {
                AsmType::Bytearray { size, alignment: _ } => {
                    instructions.extend(copy_bytes(
                        &AsmOperand::Memory(AsmRegister::BP, offset),
                        &param,
                        size,
                    ));
                }
                _ => {
                    let instr = AsmInstruction::Mov {
                        asm_type: param_type,
                        src: AsmOperand::Memory(AsmRegister::BP, offset),
                        dst: param.clone(),
                    };
                    instructions.push(instr);
                }
            }
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

        *CURRENT_FN_RETURNS_ON_STACK.lock().unwrap() = 0;

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
                    _ if is_scalar(&type_of_symbol) => {
                        AsmNode::Operand(AsmOperand::Pseudo(name.to_owned()))
                    }
                    _ => AsmNode::Operand(AsmOperand::PseudoMem(name.to_owned(), 0)),
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
                                    asm_type: get_asm_type(dst),
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
                let mut instructions = vec![];

                if value.is_none() {
                    return AsmNode::Instructions(vec![AsmInstruction::Ret]);
                }

                let (int_retvals, double_retvals, return_in_memory) =
                    classify_return_value(&value.clone().unwrap());

                if return_in_memory {
                    instructions.push(AsmInstruction::Mov {
                        asm_type: AsmType::Quadword,
                        src: AsmOperand::Memory(AsmRegister::BP, -8),
                        dst: AsmOperand::Register(AsmRegister::AX),
                    });

                    let return_storage = AsmOperand::Memory(AsmRegister::AX, 0);
                    let ret_operand = value.clone().unwrap().codegen().into();
                    let t = tacky_type(&value.clone().unwrap());

                    instructions.extend(copy_bytes(
                        &ret_operand,
                        &return_storage,
                        get_size_of_type(&t),
                    ));
                } else {
                    let int_return_registers = [AsmRegister::AX, AsmRegister::DX];
                    let double_return_registers = [AsmRegister::XMM0, AsmRegister::XMM1];

                    let mut reg_index = 0;
                    for (t, op) in int_retvals {
                        let r = int_return_registers[reg_index];
                        match t {
                            AsmType::Bytearray { size, alignment: _ } => {
                                copy_bytes_to_reg(op, r, size, &mut instructions);
                            }
                            _ => {
                                instructions.push(AsmInstruction::Mov {
                                    asm_type: t,
                                    src: op,
                                    dst: AsmOperand::Register(r),
                                });
                            }
                        }
                        reg_index += 1;
                    }

                    let mut reg_index = 0;
                    for op in double_retvals {
                        let r = double_return_registers[reg_index];
                        instructions.push(AsmInstruction::Mov {
                            asm_type: AsmType::Double,
                            src: op,
                            dst: AsmOperand::Register(r),
                        });
                        reg_index += 1;
                    }
                }

                instructions.push(AsmInstruction::Ret);

                AsmNode::Instructions(instructions)
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
                let t = tacky_type(src);
                let asm_type = get_asm_type(src);

                if is_scalar(&t) {
                    AsmNode::Instructions(vec![AsmInstruction::Mov {
                        asm_type,
                        src: src.codegen().into(),
                        dst: dst.codegen().into(),
                    }])
                } else {
                    let byte_count = get_size_of_type(&t);

                    AsmNode::Instructions(copy_bytes(
                        &src.codegen().into(),
                        &dst.codegen().into(),
                        byte_count,
                    ))
                }
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

                let mut return_in_memory = false;
                let mut int_dests = vec![];
                let mut double_dests = vec![];

                let mut reg_index = 0;

                if dst.is_some() {
                    (int_dests, double_dests, return_in_memory) =
                        classify_return_value(&dst.clone().unwrap());
                }

                if return_in_memory {
                    let dst_operand = dst.clone().unwrap().codegen().into();

                    instructions.push(AsmInstruction::Lea {
                        src: dst_operand,
                        dst: AsmOperand::Register(AsmRegister::DI),
                    });

                    reg_index = 1;
                }

                let (int_args, double_args, stack_args) =
                    classify_parameters(args, return_in_memory);

                let stack_padding = if stack_args.len() % 2 != 0 { 8 } else { 0 };

                if stack_padding != 0 {
                    instructions.push(AsmInstruction::AllocateStack(stack_padding));
                }

                for (reg_type, reg_arg) in int_args.iter() {
                    let reg = int_registers[reg_index];
                    match reg_type {
                        AsmType::Bytearray { size, alignment: _ } => {
                            copy_bytes_to_reg(reg_arg.clone(), reg, *size, &mut instructions);
                        }
                        _ => {
                            instructions.push(AsmInstruction::Mov {
                                asm_type: *reg_type,
                                src: reg_arg.clone(),
                                dst: AsmOperand::Register(reg),
                            });
                        }
                    }

                    reg_index += 1;
                }

                for (reg_index, reg_arg) in double_args.into_iter().enumerate() {
                    let reg = double_registers[reg_index];

                    instructions.push(AsmInstruction::Mov {
                        asm_type: AsmType::Double,
                        src: reg_arg.into(),
                        dst: AsmOperand::Register(reg),
                    });
                }

                for (arg_type, stack_arg) in stack_args.iter().rev() {
                    match arg_type {
                        AsmType::Bytearray { size, alignment: _ } => {
                            instructions.push(AsmInstruction::Binary {
                                asm_type: AsmType::Quadword,
                                op: AsmBinaryOp::Sub,
                                lhs: AsmOperand::Imm(8),
                                rhs: AsmOperand::Register(AsmRegister::SP),
                            });
                            instructions.extend(copy_bytes(
                                stack_arg,
                                &AsmOperand::Memory(AsmRegister::SP, 0),
                                *size,
                            ));
                        }
                        _ => match stack_arg {
                            AsmOperand::Imm(_) | AsmOperand::Register(_) => {
                                instructions.push(AsmInstruction::Push(stack_arg.clone().into()));
                            }
                            _ => {
                                if arg_type == &AsmType::Quadword || arg_type == &AsmType::Double {
                                    instructions
                                        .push(AsmInstruction::Push(stack_arg.clone().into()));
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
                        },
                    }
                }

                instructions.push(AsmInstruction::Call(target.to_owned()));

                let bytes_to_remove = 8 * stack_args.len() + stack_padding;
                if bytes_to_remove != 0 {
                    instructions.push(AsmInstruction::DeallocateStack(bytes_to_remove));
                }

                if dst.is_some() && !return_in_memory {
                    let int_return_registers = [AsmRegister::AX, AsmRegister::DX];
                    let double_return_registers = [AsmRegister::XMM0, AsmRegister::XMM1];

                    let mut reg_index = 0;

                    for (t, op) in int_dests {
                        let r = int_return_registers[reg_index];
                        match t {
                            AsmType::Bytearray { size, alignment: _ } => {
                                copy_bytes_from_reg(&r, &op, size, &mut instructions);
                            }
                            _ => {
                                instructions.push(AsmInstruction::Mov {
                                    asm_type: t.clone(),
                                    src: AsmOperand::Register(r.clone()),
                                    dst: op.into(),
                                });
                            }
                        }
                        reg_index += 1;
                    }

                    // retrieve values in xmm registers
                    let mut reg_index = 0;
                    for op in double_dests {
                        let r = double_return_registers[reg_index];
                        instructions.push(AsmInstruction::Mov {
                            asm_type: AsmType::Double,
                            src: AsmOperand::Register(r.clone()),
                            dst: op.into(),
                        });
                        reg_index += 1;
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
            IRInstruction::Load { src_ptr, dst } => {
                let t = tacky_type(&dst);
                if is_scalar(&t) {
                    AsmNode::Instructions(vec![
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
                    ])
                } else {
                    let byte_count = get_size_of_type(&t);

                    let mut initial_instr = vec![AsmInstruction::Mov {
                        asm_type: AsmType::Quadword,
                        src: src_ptr.codegen().into(),
                        dst: AsmOperand::Register(AsmRegister::R9),
                    }];

                    initial_instr.extend(copy_bytes(
                        &AsmOperand::Memory(AsmRegister::R9, 0),
                        &dst.codegen().into(),
                        byte_count,
                    ));

                    AsmNode::Instructions(initial_instr)
                }
            }
            IRInstruction::Store { src, dst_ptr } => {
                let t = tacky_type(&src);

                if is_scalar(&t) {
                    AsmNode::Instructions(vec![
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
                    ])
                } else {
                    let byte_count = get_size_of_type(&t);

                    let mut initial_instr = vec![AsmInstruction::Mov {
                        asm_type: AsmType::Quadword,
                        src: dst_ptr.codegen().into(),
                        dst: AsmOperand::Register(AsmRegister::R9),
                    }];

                    initial_instr.extend(copy_bytes(
                        &src.codegen().into(),
                        &AsmOperand::Memory(AsmRegister::R9, 0),
                        byte_count,
                    ));

                    AsmNode::Instructions(initial_instr)
                }
            }
            IRInstruction::GetAddress { src, dst } => {
                AsmNode::Instructions(vec![AsmInstruction::Lea {
                    src: src.codegen().into(),
                    dst: dst.codegen().into(),
                }])
            }
            IRInstruction::AddPtr {
                ptr,
                index: IRValue::Constant(Const::Long(konst)),
                scale,
                dst,
            } => AsmNode::Instructions(vec![
                AsmInstruction::Mov {
                    asm_type: AsmType::Quadword,
                    src: ptr.codegen().into(),
                    dst: AsmOperand::Register(AsmRegister::R9),
                },
                AsmInstruction::Lea {
                    src: AsmOperand::Memory(AsmRegister::R9, *konst as isize * *scale as isize),
                    dst: dst.codegen().into(),
                },
            ]),
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

                let t = tacky_type(&src);

                if is_scalar(&t) {
                    AsmNode::Instructions(vec![AsmInstruction::Mov {
                        asm_type: type_of_src,
                        src: src.codegen().into(),
                        dst: AsmOperand::PseudoMem(dst.to_owned(), *offset as isize),
                    }])
                } else {
                    let byte_count = get_size_of_type(&t);
                    AsmNode::Instructions(copy_bytes(
                        &src.codegen().into(),
                        &AsmOperand::PseudoMem(dst.to_owned(), *offset as isize),
                        byte_count,
                    ))
                }
            }
            IRInstruction::CopyFromOffset { src, offset, dst } => {
                let type_of_dst = get_asm_type(dst);

                let t = tacky_type(&dst);

                if is_scalar(&t) {
                    AsmNode::Instructions(vec![AsmInstruction::Mov {
                        asm_type: type_of_dst,
                        src: AsmOperand::PseudoMem(src.to_owned(), *offset as isize),
                        dst: dst.codegen().into(),
                    }])
                } else {
                    let byte_count = get_size_of_type(&t);
                    AsmNode::Instructions(copy_bytes(
                        &AsmOperand::PseudoMem(src.to_owned(), *offset as isize),
                        &dst.codegen().into(),
                        byte_count,
                    ))
                }
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

            let stack_adjustment = (adjusted_stack_bytes - callee_saved_bytes as i64) as i64;

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

        let save_reg = |r: &AsmRegister| AsmInstruction::Push(AsmOperand::Register(r.clone()));

        for reg in callee_saved_args {
            instructions_setup.push(save_reg(reg));
        }

        self.instructions.splice(0..0, instructions_setup);

        for instr in &mut self.instructions {
            match instr {
                AsmInstruction::Pop(reg) if is_xmm(&reg) => {
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
                AsmInstruction::Binary { asm_type: AsmType::Double, op, lhs, rhs: AsmOperand::Register(reg) } => {
                    instructions.extend(vec![
                        instr.clone()
                    ])
                }
                AsmInstruction::Binary { asm_type: AsmType::Double, op, lhs, rhs } => {
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
                            _ => {
                                instructions.push(instr.clone());
                            }
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
                        (
                            AsmOperand::Data(src, offset),
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
                        (
                            AsmOperand::Memory(AsmRegister::BP, src_n),
                            AsmOperand::Data(dst, offset),
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
                    // Generate restoration instructions for callee-saved registers
                    let restore_regs: Vec<AsmInstruction> = callee_saved_args
                        .iter()
                        .rev() // Reverse to restore in the opposite order of saving
                        .flat_map(|r| {
                            if !is_xmm(r) {
                                // For non-XMM registers, use Pop
                                vec![AsmInstruction::Pop(r.clone())]
                            } else {
                                // For XMM registers, use Mov to restore and Add to adjust SP
                                vec![
                                    AsmInstruction::Mov {
                                        asm_type: AsmType::Quadword,
                                        src: AsmOperand::Memory(AsmRegister::SP, 0), // Assuming memory operand syntax
                                        dst: AsmOperand::Register(r.clone()),
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

fn classify_parameters(
    params: &[IRValue],
    return_on_stack: bool,
) -> (
    Vec<(AsmType, AsmOperand)>,
    Vec<AsmOperand>,
    Vec<(AsmType, AsmOperand)>,
) {
    // Map parameters to their types and converted values
    let typed_params: Vec<(Type, AsmOperand)> = params
        .iter()
        .map(|v| (tacky_type(v), v.codegen().into()))
        .collect();

    // Call the helper function
    classify_params_helper(&typed_params, return_on_stack)
}

fn classify_params_helper(
    typed_asm_vals: &[(Type, AsmOperand)],
    return_on_stack: bool,
) -> (
    Vec<(AsmType, AsmOperand)>,
    Vec<AsmOperand>,
    Vec<(AsmType, AsmOperand)>,
) {
    // Determine how many integer registers are available based on whether the return is on the stack
    let int_regs_available = if return_on_stack { 5 } else { 6 };

    // Initialize register and stack argument vectors
    let mut int_reg_args = vec![];
    let mut double_reg_args = vec![];
    let mut stack_args = vec![];

    // Process each parameter type
    for (tacky_t, operand) in typed_asm_vals {
        let typed_operand = (convert_type(tacky_t), operand.clone());

        match tacky_t {
            // Handle structure types
            Type::Struct { tag } => {
                let struct_entry = match tacky_t {
                    Type::Struct { tag } => TYPE_TABLE.lock().unwrap().get(tag).unwrap().clone(),
                    _ => unreachable!(),
                };

                let classes = classify_structure(&struct_entry);
                let mut use_stack = true;

                let struct_size = struct_entry.size;

                let name_of_v = match operand {
                    AsmOperand::PseudoMem(name, _) => name.clone(),
                    _ => unreachable!(),
                };

                if classes[0] != Class::Memory {
                    let mut tentative_ints = vec![];
                    let mut tentative_doubles = vec![];

                    let mut offset = 0;

                    for class in &classes {
                        let operand = AsmOperand::PseudoMem(name_of_v.clone(), offset);
                        if class == &Class::Sse {
                            tentative_doubles.push(operand);
                        } else {
                            let eightbyte_type = get_eightbyte_type(offset, struct_size);
                            tentative_ints.push((eightbyte_type, operand));
                        }
                        offset += 8;
                    }

                    if (tentative_doubles.len() + double_reg_args.len() <= 8)
                        && (tentative_ints.len() + int_reg_args.len() <= int_regs_available)
                    {
                        double_reg_args.extend(tentative_doubles);
                        int_reg_args.extend(tentative_ints);
                        use_stack = false;
                    }
                }

                if use_stack {
                    let mut offset = 0;
                    for _ in classes {
                        let operand = AsmOperand::PseudoMem(name_of_v.clone(), offset);
                        stack_args.push((get_eightbyte_type(offset, struct_size), operand));
                        offset += 8;
                    }
                }
            }

            // Handle floating-point types
            Type::Double => {
                if double_reg_args.len() < 8 {
                    double_reg_args.push(operand.clone());
                } else {
                    stack_args.push(typed_operand);
                }
            }

            // Handle scalar types (integers, etc.)
            _ => {
                if int_reg_args.len() < int_regs_available {
                    int_reg_args.push(typed_operand);
                } else {
                    stack_args.push(typed_operand);
                }
            }
        }
    }

    (int_reg_args, double_reg_args, stack_args)
}

fn get_eightbyte_type(offset: isize, struct_size: usize) -> AsmType {
    let bytes_from_end = struct_size as isize - offset;

    if bytes_from_end >= 8 {
        return AsmType::Quadword;
    }

    if bytes_from_end == 4 {
        return AsmType::Longword;
    }

    if bytes_from_end == 1 {
        return AsmType::Byte;
    }

    AsmType::Bytearray {
        size: bytes_from_end as usize,
        alignment: 8,
    }
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
                Type::Struct { ref tag } => {
                    let struct_size = TYPE_TABLE.lock().unwrap().get(tag).unwrap().size;
                    let struct_alignment = TYPE_TABLE.lock().unwrap().get(tag).unwrap().alignment;
                    AsmType::Bytearray {
                        size: struct_size,
                        alignment: struct_alignment,
                    }
                }
                _ => unreachable!(),
            }
        }
    }
}

fn returns_on_stack(name: &str) -> bool {
    match SYMBOL_TABLE
        .lock()
        .unwrap()
        .get(name)
        .cloned()
        .unwrap()
        ._type
    {
        Type::Func { params: _, ret } => match &*ret {
            Type::Struct { tag } => {
                let struct_entry = TYPE_TABLE
                    .lock()
                    .unwrap()
                    .get(tag)
                    .cloned()
                    .unwrap()
                    .clone();
                match classify_structure(&struct_entry).as_slice() {
                    [Class::Memory, ..] => true,
                    _ => false,
                }
            }
            _ => false,
        },
        _ => unreachable!(),
    }
}

fn classify_param_types(params: &[Type], return_on_stack: bool) -> Vec<AsmRegister> {
    let f = |t: &Type| {
        if is_scalar(t) {
            (t.to_owned(), AsmOperand::Pseudo("DUMMY".to_string()))
        } else {
            (t.to_owned(), AsmOperand::PseudoMem("DUMMY".to_string(), 0))
        }
    };

    let int_regs: [AsmRegister; 6] = [
        AsmRegister::DI,
        AsmRegister::SI,
        AsmRegister::DX,
        AsmRegister::CX,
        AsmRegister::R8,
        AsmRegister::R9,
    ];

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

    let (ints, dbls, _) =
        classify_params_helper(&params.iter().map(f).collect::<Vec<_>>(), return_on_stack);

    let int_regs_final: Vec<_> = int_regs.iter().take(ints.len()).collect();
    let double_regs_final: Vec<_> = double_regs.iter().take(dbls.len()).collect();

    int_regs_final
        .iter()
        .chain(double_regs_final.iter())
        .map(|x| x.to_owned().to_owned())
        .collect()
}

pub fn build_asm_symbol_table() {
    use crate::typechecker::is_complete;

    let frontend_symtab = SYMBOL_TABLE.lock().unwrap().clone();
    let mut asm_symbol_table = ASM_SYMBOL_TABLE.lock().unwrap();

    for (identifier, symbol) in frontend_symtab.iter() {
        let entry = match symbol.attrs {
            IdentifierAttrs::FuncAttr { defined, .. } => match &symbol._type {
                Type::Func { params, ret } => {
                    if is_complete(&ret) || ret == &Type::Void.into() {
                        let (return_regs, returns_on_stack) = classify_return_type(ret);
                        let param_regs = classify_param_types(params, returns_on_stack);

                        AsmSymtabEntry::Function {
                            defined,
                            bytes_required: 0,
                            returns_on_stack,
                            callee_saved_regs_used: BTreeSet::new(),
                            param_regs,
                            return_regs,
                        }
                    } else {
                        assert!(!defined);

                        AsmSymtabEntry::Function {
                            defined,
                            bytes_required: 0,
                            returns_on_stack: false,
                            callee_saved_regs_used: BTreeSet::new(),
                            param_regs: Vec::new(),
                            return_regs: Vec::new(),
                        }
                    }
                }
                _ => unreachable!(),
            },
            IdentifierAttrs::StaticAttr {
                initial_value: _, ..
            } => {
                if is_complete(&symbol._type) {
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
                        Type::Struct { ref tag } => {
                            let struct_size = TYPE_TABLE.lock().unwrap().get(tag).unwrap().size;
                            let struct_alignment =
                                TYPE_TABLE.lock().unwrap().get(tag).unwrap().alignment;
                            AsmType::Bytearray {
                                size: struct_size,
                                alignment: struct_alignment,
                            }
                        }
                        _ => panic!("Unsupported type for static variable"),
                    };
                    AsmSymtabEntry::Object {
                        _type: asm_type,
                        is_static: true,
                        is_constant: false,
                    }
                } else {
                    AsmSymtabEntry::Object {
                        _type: AsmType::Byte,
                        is_static: true,
                        is_constant: false,
                    }
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
                    Type::Struct { ref tag } => {
                        let struct_size = TYPE_TABLE.lock().unwrap().get(tag).unwrap().size;
                        let struct_alignment =
                            TYPE_TABLE.lock().unwrap().get(tag).unwrap().alignment;
                        AsmType::Bytearray {
                            size: struct_size,
                            alignment: struct_alignment,
                        }
                    }
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
        returns_on_stack: bool,
        bytes_required: usize,
        param_regs: Vec<AsmRegister>,
        return_regs: Vec<AsmRegister>,
        callee_saved_regs_used: BTreeSet<AsmRegister>,
    },
    Object {
        _type: AsmType,
        is_static: bool,
        is_constant: bool,
    },
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct StackPosition(pub i64);

#[derive(Debug, Clone, PartialEq)]
pub struct VarToStackPos {
    last_used_stack_pos: StackPosition,
    var_to_stack_pos: BTreeMap<String, StackPosition>,
}

impl Default for VarToStackPos {
    fn default() -> Self {
        Self {
            last_used_stack_pos: StackPosition(0),
            var_to_stack_pos: BTreeMap::new(),
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
    pub static ref ASM_SYMBOL_TABLE: Mutex<BTreeMap<String, AsmSymtabEntry>> = Mutex::new(BTreeMap::new());
    static ref VAR_TO_STACK_POS: Mutex<VarToStackPos> = Mutex::new(VarToStackPos::default());
}

pub fn tacky_type(value: &IRValue) -> Type {
    match value {
        IRValue::Constant(konst) => match konst {
            Const::Char(_) => Type::Char,
            Const::UChar(_) => Type::UChar,
            Const::Int(_) => Type::Int,
            Const::Long(_) => Type::Long,
            Const::UInt(_) => Type::Uint,
            Const::ULong(_) => Type::Ulong,
            Const::Double(_) => Type::Double,
        },
        IRValue::Var(var_name) => {
            let symbol = SYMBOL_TABLE.lock().unwrap().get(var_name).cloned().unwrap();
            symbol._type
        }
    }
}

fn add_offset(byte_count: usize, operand: &AsmOperand) -> AsmOperand {
    match operand {
        AsmOperand::PseudoMem(name, offset) => {
            AsmOperand::PseudoMem(name.clone(), *offset + byte_count as isize)
        }
        AsmOperand::Memory(base, offset) => {
            AsmOperand::Memory(*base, *offset + byte_count as isize)
        }
        _ => {
            unreachable!()
        }
    }
}

fn copy_bytes(src: &AsmOperand, dst: &AsmOperand, byte_count: usize) -> Vec<AsmInstruction> {
    if byte_count == 0 {
        return vec![];
    }

    let (operand_type, operand_size) = if byte_count < 4 {
        (AsmType::Byte, 1)
    } else if byte_count < 8 {
        (AsmType::Longword, 4)
    } else {
        (AsmType::Quadword, 8)
    };

    let next_src = add_offset(operand_size, src);
    let next_dst = add_offset(operand_size, dst);

    let bytes_left = byte_count - operand_size;

    let mut instructions = vec![AsmInstruction::Mov {
        asm_type: operand_type,
        src: src.clone(),
        dst: dst.clone(),
    }];
    instructions.extend(copy_bytes(&next_src, &next_dst, bytes_left));

    instructions
}

#[derive(Debug, Clone, PartialEq)]
pub enum Class {
    Memory,
    Sse,
    Integer,
}

use crate::typechecker::StructEntry;

fn classify_structure(struct_entry: &StructEntry) -> Vec<Class> {
    let mut size: isize = struct_entry.size as isize;

    if size > 16 {
        let mut result = vec![];
        while size > 0 {
            result.push(Class::Memory);
            size -= 8;
        }
        return result;
    }

    let scalar_types = flatten_member_types(&struct_entry.members);

    let first = scalar_types.first().unwrap();
    let last = scalar_types.last().unwrap();

    if size > 8 {
        if first == &Type::Double && last == &Type::Double {
            return vec![Class::Sse, Class::Sse];
        }

        if first == &Type::Double {
            return vec![Class::Sse, Class::Integer];
        }

        if last == &Type::Double {
            return vec![Class::Integer, Class::Sse];
        }

        return vec![Class::Integer, Class::Integer];
    } else if first == &Type::Double {
        return vec![Class::Sse];
    } else {
        return vec![Class::Integer];
    }
}

fn flatten_member_types(members: &Vec<MemberEntry>) -> Vec<Type> {
    let mut result = vec![];
    for member in members {
        match &member._type {
            Type::Struct { tag } => {
                let struct_entry = TYPE_TABLE.lock().unwrap().get(tag).cloned().unwrap();
                result.extend(flatten_member_types(&struct_entry.members));
            }
            Type::Array { element, size } => {
                for _ in 0..*size {
                    result.push(*element.clone());
                }
            }
            _ => result.push(member._type.clone()),
        }
    }
    result
}

fn copy_bytes_to_reg(
    src_op: AsmOperand,
    dst_reg: AsmRegister,
    byte_count: usize,
    instructions: &mut Vec<AsmInstruction>,
) {
    let mut offset = byte_count as isize - 1;
    while offset >= 0 {
        let src_byte = add_offset(offset as usize, &src_op);
        instructions.push(AsmInstruction::Mov {
            asm_type: AsmType::Byte,
            src: src_byte,
            dst: AsmOperand::Register(dst_reg),
        });
        if offset > 0 {
            instructions.push(AsmInstruction::Binary {
                asm_type: AsmType::Quadword,
                op: AsmBinaryOp::Shl,
                lhs: AsmOperand::Imm(8),
                rhs: AsmOperand::Register(dst_reg.clone()),
            });
        }
        offset -= 1;
    }
}

fn classify_return_value(retval: &IRValue) -> (Vec<(AsmType, AsmOperand)>, Vec<AsmOperand>, bool) {
    let t = tacky_type(retval);
    let val = retval.codegen().into();
    classify_return_helper(&t, &val)
}

fn classify_return_helper(
    ret_type: &Type,
    asm_retval: &AsmOperand,
) -> (Vec<(AsmType, AsmOperand)>, Vec<AsmOperand>, bool) {
    match ret_type {
        // Handling structure types
        Type::Struct { tag } => {
            let struct_entry = TYPE_TABLE.lock().unwrap().get(tag).unwrap().clone();
            let classes = classify_structure(&struct_entry);

            let struct_size = struct_entry.size;

            if classes[0] == Class::Memory {
                return (vec![], vec![], true);
            } else {
                let mut int_retvals = vec![];
                let mut double_retvals = vec![];

                let mut offset = 0;

                let name_of_retval = match asm_retval {
                    AsmOperand::PseudoMem(n, _) => n.clone(),
                    _ => unreachable!(),
                };

                for class in classes {
                    let operand = AsmOperand::PseudoMem(name_of_retval.clone(), offset);
                    match class {
                        Class::Sse => {
                            double_retvals.push(operand);
                        }
                        Class::Integer => {
                            let eightbyte_type = get_eightbyte_type(offset, struct_size);
                            int_retvals.push((eightbyte_type, operand));
                        }
                        Class::Memory => unreachable!(),
                    }
                    offset += 8;
                }

                return (int_retvals, double_retvals, false);
            }
        }

        // Handling floating-point return types
        Type::Double => (vec![], vec![asm_retval.clone()], false),

        // Handling other scalar types
        t => {
            let typed_operand = (convert_type(t), asm_retval.clone());
            (vec![typed_operand], vec![], false)
        }
    }
}

fn convert_type(t: &Type) -> AsmType {
    match t {
        Type::Char | Type::UChar | Type::SChar => AsmType::Byte,
        Type::Int | Type::Uint => AsmType::Longword,
        Type::Long | Type::Ulong => AsmType::Quadword,
        Type::Double => AsmType::Double,
        Type::Array { element, size } => AsmType::Bytearray {
            size: get_size_of_type(element) * size,
            alignment: get_alignment_of_type(t),
        },
        Type::Struct { tag } => {
            let struct_size = TYPE_TABLE.lock().unwrap().get(tag).unwrap().size;
            let struct_alignment = TYPE_TABLE.lock().unwrap().get(tag).unwrap().alignment;
            AsmType::Bytearray {
                size: struct_size,
                alignment: struct_alignment,
            }
        }
        Type::Pointer(_) => AsmType::Quadword,
        _ => unreachable!(),
    }
}

fn copy_bytes_from_reg(
    src_reg: &AsmRegister,
    dst_op: &AsmOperand,
    byte_count: usize,
    instructions: &mut Vec<AsmInstruction>,
) {
    let mut offset = 0;
    while offset < byte_count {
        let dst_byte = add_offset(offset, dst_op);
        instructions.push(AsmInstruction::Mov {
            asm_type: AsmType::Byte,
            src: AsmOperand::Register(src_reg.clone()),
            dst: dst_byte,
        });
        if offset < byte_count - 1 {
            instructions.push(AsmInstruction::Binary {
                asm_type: AsmType::Quadword,
                op: AsmBinaryOp::ShrTwoOp,
                lhs: AsmOperand::Imm(8),
                rhs: AsmOperand::Register(src_reg.clone()),
            });
        }
        offset += 1;
    }
}

fn classify_return_type(t: &Type) -> (Vec<AsmRegister>, bool) {
    match t {
        Type::Void => (vec![], false),
        t => {
            let asm_val = if is_scalar(t) {
                AsmOperand::Pseudo("DUMMY".to_string())
            } else {
                AsmOperand::PseudoMem("DUMMY".to_string(), 0)
            };

            let (ints, dbls, return_on_stack) = classify_return_helper(t, &asm_val);
            if return_on_stack {
                (vec![AsmRegister::AX], true)
            } else {
                let int_regs: Vec<AsmRegister> = vec![AsmRegister::AX, AsmRegister::DX]
                    .iter()
                    .take(ints.len())
                    .cloned()
                    .collect();
                let dbl_regs: Vec<AsmRegister> = vec![AsmRegister::XMM0, AsmRegister::XMM1]
                    .iter()
                    .take(dbls.len())
                    .cloned()
                    .collect();

                (
                    int_regs.iter().chain(dbl_regs.iter()).cloned().collect(),
                    false,
                )
            }
        }
    }
}

pub trait RegAlloc {
    fn reg_alloc(&mut self, aliased_pseudos: &BTreeSet<String>) -> Self;
}

impl RegAlloc for AsmProgram {
    fn reg_alloc(&mut self, aliased_pseudos: &BTreeSet<String>) -> Self {
        let mut functions = vec![];
        for func in &mut self.functions {
            functions.push(match func {
                AsmNode::Function(f) => AsmNode::Function(f.reg_alloc(aliased_pseudos)),
                _ => unreachable!(),
            });
        }
        AsmProgram {
            functions,
            static_constants: self.static_constants.clone(),
            static_vars: self.static_vars.clone(),
        }
    }
}

impl RegAlloc for AsmFunction {
    fn reg_alloc(&mut self, aliased_pseudos: &BTreeSet<String>) -> Self {
        let static_vars = SYMBOL_TABLE
            .lock()
            .unwrap()
            .iter()
            .filter(|(name, symbol)| match symbol.attrs {
                IdentifierAttrs::StaticAttr { .. } => true,
                _ => false,
            })
            .map(|(name, symbol)| name.clone())
            .collect();

        let gp_graph = loop {
            let mut gp_graph = build_interference_graph(
                &self.name,
                &static_vars,
                aliased_pseudos,
                &self.instructions,
                &RegisterClass::GP,
            );

            let mut coalesced_regs = coalesce(&mut gp_graph, &self.instructions, &RegisterClass::GP);
            if coalesced_regs.nothing_was_coalesced() {
                break gp_graph;
            }

            self.instructions = rewrite_coalesced(self.instructions.clone(), &mut coalesced_regs)
        };
        // print_graphviz(&self.name, &gp_graph);

        let gp_spilled_graph = add_spill_costs(gp_graph, &self.instructions);
        // print_graphviz(&self.name, &gp_spilled_graph);

        let colored_gp_graph = color_graph(gp_spilled_graph, GP_REGISTERS.len(), &RegisterClass::GP);
        // print_graphviz(&self.name, &colored_gp_graph);

        let gp_register_map = make_register_map(&self.name, &colored_gp_graph, &RegisterClass::GP);

        let xmm_graph = loop {
            let mut xmm_graph = build_interference_graph(
                &self.name,
                &static_vars,
                aliased_pseudos,
                &self.instructions,
                &RegisterClass::XMM,
            );

            let mut coalesced_regs = coalesce(&mut xmm_graph, &self.instructions, &RegisterClass::XMM);
            if coalesced_regs.nothing_was_coalesced() {
                break xmm_graph;
            }

            self.instructions = rewrite_coalesced(self.instructions.clone(), &mut coalesced_regs)            
        };

        let xmm_spilled_graph = add_spill_costs(xmm_graph, &self.instructions);
        let colored_xmm_graph = color_graph(xmm_spilled_graph, XMM_REGISTERS.len(), &RegisterClass::XMM);
        let xmm_register_map = make_register_map(&self.name, &colored_xmm_graph, &RegisterClass::XMM);

        // Merge GP and XMM register maps
        let mut register_map = gp_register_map;
        register_map.extend(xmm_register_map);

        let transformed_instructions = replace_pseudoregs(&self.instructions, &register_map);

        AsmFunction {
            name: self.name.clone(),
            instructions: transformed_instructions,
            global: self.global,
            stack_space: self.stack_space,
        }
    }
}

impl RegAlloc for AsmNode {
    fn reg_alloc(&mut self, aliased_pseudos: &BTreeSet<String>) -> Self {
        match self {
            AsmNode::Program(prog) => AsmNode::Program(prog.reg_alloc(aliased_pseudos)),
            AsmNode::Function(f) => AsmNode::Function(f.reg_alloc(aliased_pseudos)),
            _ => unreachable!(),
        }
    }
}

type NodeId = AsmOperand;

#[derive(Debug, Clone, PartialEq)]
struct Node {
    id: NodeId,
    neighbors: BTreeSet<AsmOperand>,
    spill_cost: f64,
    color: Option<usize>,
    pruned: bool,
}

type Graph = BTreeMap<NodeId, Node>;

fn mk_base_graph(all_hardregs: &BTreeSet<AsmOperand>) -> Graph {
    let mut graph = BTreeMap::new();

    for r in all_hardregs.iter() {
        let node = Node {
            id: r.clone(),
            neighbors: all_hardregs
                .iter()
                .filter(|reg| reg != &r)
                .map(|x| x.to_owned())
                .collect(),
            spill_cost: f64::INFINITY,
            color: None,
            pruned: false,
        };

        graph.insert(r.clone(), node);
    }

    graph
}

fn build_interference_graph(
    fn_name: &str,
    static_vars: &BTreeSet<String>,
    aliased_pseudos: &BTreeSet<String>,
    instructions: &[AsmInstruction],
    register_class: &RegisterClass,
) -> Graph {
    let all_hardregs = match register_class {
        RegisterClass::GP => GP_REGISTERS.iter().map(|x| AsmOperand::Register(*x)).collect(),
        RegisterClass::XMM => XMM_REGISTERS.iter().map(|x| AsmOperand::Register(*x)).collect(),
    };

    let mut graph = mk_base_graph(&all_hardregs);

    add_pseudo_nodes(&mut graph, aliased_pseudos, instructions, register_class);

    let cfg: CFG<(), AsmInstruction> =
        CFG::instructions_to_cfg("spam".to_string(), instructions.to_vec());
    let mut analyzed_cfg =
        LivenessAnalysis::analyze(&fn_name, cfg, static_vars, aliased_pseudos, &all_hardregs, register_class);

    add_edges(&mut analyzed_cfg, &mut graph, register_class);

    graph
}

fn add_edge(graph: &mut Graph, nd_id1: &AsmOperand, nd_id2: &AsmOperand) {
    if let Some(nd1) = graph.get_mut(nd_id1) {
        nd1.neighbors.insert(nd_id2.clone());
    }

    if let Some(nd2) = graph.get_mut(nd_id2) {
        nd2.neighbors.insert(nd_id1.clone());
    }
}

fn add_edges(
    liveness_cfg: &CFG<BTreeSet<AsmOperand>, AsmInstruction>,
    interference_graph: &mut Graph,
    register_class: &RegisterClass,
) {
    // Iterate over all basic blocks
    for (_, block) in &liveness_cfg.basic_blocks {
        // Iterate over all instructions in the block
        for (live_after_instr, instr) in &block.instructions {
            // Obtain the registers used and written by the instruction
            let (_unused, updated_regs) = regs_used_and_written(instr, register_class);

            // Define a closure to handle each live register
            let mut handle_livereg = |l: &AsmOperand| {
                // Check if the instruction is a `Mov` where src == l
                let is_mov_src = match instr {
                    AsmInstruction::Mov { src, .. } if src == l => true,
                    _ => false,
                };

                if is_mov_src {
                    // Do nothing for this case
                    return;
                }

                // Iterate over all updated_regs
                for u in &updated_regs {
                    if u != l
                        && interference_graph.contains_key(l)
                        && interference_graph.contains_key(u)
                    {
                        add_edge(interference_graph, l, u);
                    }
                }
            };

            // Iterate over all live registers after the instruction
            for live_reg in live_after_instr {
                handle_livereg(live_reg);
            }
        }
    }
}

type OperandSet = BTreeSet<AsmOperand>;

fn regs_used_and_written(instr: &AsmInstruction, register_class: &RegisterClass) -> (OperandSet, OperandSet) {
    let (ops_used, ops_written) = match instr {
        AsmInstruction::Mov { src, dst, .. } => (vec![src.clone()], vec![dst.clone()]),
        AsmInstruction::Movsx { src, dst, .. } => (vec![src.clone()], vec![dst.clone()]),
        AsmInstruction::MovZeroExtend { src, dst, .. } => (vec![src.clone()], vec![dst.clone()]),
        AsmInstruction::Cvtsi2sd { src, dst, .. } => (vec![src.clone()], vec![dst.clone()]),
        AsmInstruction::Cvttsd2si { src, dst, .. } => (vec![src.clone()], vec![dst.clone()]),
        AsmInstruction::Binary { lhs, rhs, .. } => (vec![lhs.clone(), rhs.clone()], vec![rhs.clone()]),
        AsmInstruction::Unary { operand, .. } => (vec![operand.clone()], vec![operand.clone()]),
        AsmInstruction::Cmp { lhs, rhs, .. } => (vec![lhs.clone(), rhs.clone()], vec![]),
        AsmInstruction::SetCC { operand, .. } => (vec![], vec![operand.clone()]),
        AsmInstruction::Push(operand) => (vec![operand.clone()], vec![]),
        AsmInstruction::Idiv { operand, .. } | AsmInstruction::Div { operand, .. } => (
            vec![
                operand.clone(),
                AsmOperand::Register(AsmRegister::AX),
                AsmOperand::Register(AsmRegister::DX),
            ],
            vec![
                AsmOperand::Register(AsmRegister::AX),
                AsmOperand::Register(AsmRegister::DX),
            ],
        ),
        AsmInstruction::Cdq { .. } => (
            vec![AsmOperand::Register(AsmRegister::AX)],
            vec![AsmOperand::Register(AsmRegister::DX)],
        ),
        AsmInstruction::Call(func_name) => {
            let all_hardregs = match register_class {
                RegisterClass::GP => GP_REGISTERS,
                RegisterClass::XMM => XMM_REGISTERS,
            };

            let used_regs = {
                let table = ASM_SYMBOL_TABLE.lock().unwrap();
                if let Some(entry) = table.get(func_name) {
                    match entry {
                        AsmSymtabEntry::Function { param_regs, .. } => param_regs
                            .iter()
                            .filter(|r| all_hardregs.contains(r))
                            .cloned()
                            .map(AsmOperand::Register)
                            .collect::<Vec<_>>(),
                        _ => vec![],
                    }
                } else {
                    vec![]
                }
            };

            let regs = match register_class {
                RegisterClass::GP => GP_CALLER_SAVED_REGISTERS,
                RegisterClass::XMM => XMM_CALLER_SAVED_REGISTERS,
            };

            (used_regs, regs.iter().map(|r| AsmOperand::Register(*r)).collect())
        }
        AsmInstruction::Lea { src, dst } => (vec![src.clone()], vec![dst.clone()]),
        AsmInstruction::Jmp { .. }
        | AsmInstruction::JmpCC { .. }
        | AsmInstruction::Label(_)
        | AsmInstruction::Ret => (vec![], vec![]),
        _ => (vec![], vec![]),
    };

    // Convert operands used to a set of registers/pseudo-registers read
    let regs_used_to_read = |opr: &AsmOperand| -> Vec<AsmOperand> {
        match opr {
            AsmOperand::Pseudo(_) | AsmOperand::Register(_) => vec![opr.clone()],
            AsmOperand::Memory(base, _) => vec![AsmOperand::Register(base.clone())],
            AsmOperand::Indexed(base, index, _) => vec![
                AsmOperand::Register(base.clone()),
                AsmOperand::Register(index.clone()),
            ],
            AsmOperand::Imm(_) | AsmOperand::Data(_, _) | AsmOperand::PseudoMem(_, _) => vec![],
        }
    };

    // Convert operands written into a list of registers read or written
    let regs_used_to_update = |opr: &AsmOperand| -> (Vec<AsmOperand>, Vec<AsmOperand>) {
        match opr {
            AsmOperand::Pseudo(_) | AsmOperand::Register(_) => (vec![], vec![opr.clone()]),
            AsmOperand::Memory(base, _) => (vec![AsmOperand::Register(base.clone())], vec![]),
            AsmOperand::Indexed(base, index, _) => (
                vec![
                    AsmOperand::Register(base.clone()),
                    AsmOperand::Register(index.clone()),
                ],
                vec![]
            ),
            AsmOperand::Imm(_) | AsmOperand::Data(_, _) | AsmOperand::PseudoMem(_, _) => (vec![], vec![]),
        }
    };

    // Apply `regs_used_to_read` to the list of operands that are used
    let regs_read1: Vec<AsmOperand> = ops_used.iter().flat_map(regs_used_to_read).collect();

    // Apply `regs_used_to_update` to the list of operands that are written
    let (regs_read2, regs_written): (Vec<_>, Vec<_>) = ops_written
        .iter()
        .map(regs_used_to_update)
        .unzip();

    let regs_read2 = regs_read2.into_iter().flatten().collect::<Vec<_>>();
    let regs_written = regs_written.into_iter().flatten().collect::<Vec<_>>();

    // Combine both lists of registers read
    let regs_read = [regs_read1, regs_read2].concat();

    // Return the read and written registers as `OperandSet`
    (
        regs_read.into_iter().collect(),
        regs_written.into_iter().collect(),
    )
}

struct LivenessAnalysis;

impl LivenessAnalysis {
    fn meet(
        fn_name: &str,
        cfg: &CFG<BTreeSet<AsmOperand>, AsmInstruction>,
        block: &BasicBlock<BTreeSet<AsmOperand>, AsmInstruction>,
        all_hardregs: &BTreeSet<AsmOperand>,
    ) -> OperandSet {
        let mut live_at_exit = OperandSet::new();

        let all_return_regs = match ASM_SYMBOL_TABLE.lock().unwrap().get(fn_name).unwrap() {
            AsmSymtabEntry::Function { return_regs, .. } => return_regs
                .iter()
                .map(|r| AsmOperand::Register(r.clone()))
                .collect::<OperandSet>(),
            _ => unreachable!(),
        };
        let return_regs = all_hardregs
            .intersection(&all_return_regs)
            .cloned()
            .collect::<OperandSet>();

        for succ in &block.succs {
            match succ {
                cfg::NodeId::Exit => live_at_exit.extend(return_regs.clone()),
                cfg::NodeId::Entry => panic!("Internal error: malformed interference graph"),
                cfg::NodeId::Block(id) => {
                    let succ_live_registers = cfg.get_block_value(*id);
                    live_at_exit = live_at_exit.union(&succ_live_registers).cloned().collect();
                }
            }
        }

        live_at_exit
    }

    fn transfer(
        static_and_aliased_vars: &BTreeSet<String>,
        block: &BasicBlock<BTreeSet<AsmOperand>, AsmInstruction>,
        end_live_regs: OperandSet,
        register_class: &RegisterClass
    ) -> BasicBlock<BTreeSet<AsmOperand>, AsmInstruction> {
        let mut current_live_regs = end_live_regs.clone();
        let mut annotated_instructions = Vec::new();

        for (idx, instr) in block.instructions.iter().enumerate().rev() {
            annotated_instructions.push((current_live_regs.clone(), instr.1.clone()));
            
            let (regs_used, regs_written) = regs_used_and_written(&instr.1, register_class);

            let without_killed: BTreeSet<_> = current_live_regs
                .difference(&regs_written)
                .cloned()
                .collect();
            current_live_regs = without_killed.union(&regs_used).cloned().collect();
        }

        BasicBlock {
            instructions: annotated_instructions.into_iter().rev().collect(),
            value: current_live_regs,
            ..block.clone()
        }
    }

    fn analyze(
        fn_name: &str,
        cfg: cfg::CFG<(), AsmInstruction>,
        static_vars: &BTreeSet<String>,
        aliased_vars: &BTreeSet<String>,
        all_hardregs: &BTreeSet<AsmOperand>,
        register_class: &RegisterClass
    ) -> cfg::CFG<BTreeSet<AsmOperand>, AsmInstruction> {
        // Initialize the CFG with empty live variable sets
        let mut starting_cfg = cfg.initialize_annotation(BTreeSet::new());

        // Combine static variables and aliased variables
        let static_and_aliased_vars = static_vars
            .union(aliased_vars)
            .cloned()
            .collect::<BTreeSet<_>>();

        // Create the worklist by cloning the basic blocks
        let mut worklist: Vec<(usize, BasicBlock<BTreeSet<AsmOperand>, AsmInstruction>)> =
            starting_cfg.basic_blocks.clone();

        // Process the worklist in a loop
        while let Some((block_idx, blk)) = worklist.pop() {
            // Save the old annotation (live variables)
            let old_annotation = blk.value.clone();

            // Calculate live variables at the exit of the block
            let live_vars_at_exit = Self::meet(fn_name, &starting_cfg, &blk, all_hardregs);

            // Transfer function: propagate live variables through the block
            let block = Self::transfer(&static_and_aliased_vars, &blk, live_vars_at_exit, register_class);

            // Update the CFG with the new block
            starting_cfg.update_basic_block(block_idx, block.clone());

            // Get the new live variable annotation
            let new_annotation = starting_cfg.get_block_value(block_idx);

            // If the live variables have changed, update the worklist with predecessors
            if old_annotation != *new_annotation {
                let block_predecessors = starting_cfg.get_preds(&blk.id);

                for pred in block_predecessors {
                    match pred {
                        cfg::NodeId::Block(_) => {
                            let pred_block = starting_cfg.get_block_by_id(&pred);
                            if !worklist.contains(&pred_block) {
                                worklist.push(pred_block.clone());
                            }
                        }
                        cfg::NodeId::Entry => continue,
                        cfg::NodeId::Exit => panic!("Internal error: malformed CFG"),
                    }
                }
            }
        }

        starting_cfg
    }
}

fn get_operands(instr: &AsmInstruction) -> Vec<AsmOperand> {
    match instr {
        AsmInstruction::Mov { src, dst, .. } => vec![src.clone(), dst.clone()],
        AsmInstruction::Movsx { src, dst, .. } => vec![src.clone(), dst.clone()],
        AsmInstruction::MovZeroExtend { src, dst, .. } => vec![src.clone(), dst.clone()],
        AsmInstruction::Lea { src, dst, .. } => vec![src.clone(), dst.clone()],
        AsmInstruction::Cvttsd2si { src, dst, .. } => vec![src.clone(), dst.clone()],
        AsmInstruction::Cvtsi2sd { src, dst, .. } => vec![src.clone(), dst.clone()],
        AsmInstruction::Unary { operand, .. } => vec![operand.clone()],
        AsmInstruction::Binary { lhs, rhs, .. } => vec![lhs.clone(), rhs.clone()],
        AsmInstruction::Cmp { lhs, rhs, .. } => vec![lhs.clone(), rhs.clone()],
        AsmInstruction::Idiv { operand, .. } => vec![operand.clone()],
        AsmInstruction::Div { operand, .. } => vec![operand.clone()],
        AsmInstruction::SetCC { operand, .. } => vec![operand.clone()],
        AsmInstruction::Push(operand) => vec![operand.clone()],
        _ => vec![],
    }
}

fn pseudo_is_current_type(
    pseudo: &str,
    register_class: &RegisterClass,
) -> bool {
    if register_class == &RegisterClass::GP {
        match ASM_SYMBOL_TABLE.lock().unwrap().get(pseudo).unwrap() {
            AsmSymtabEntry::Object {
                _type,
                is_static,
                is_constant,
            } => {
                return _type != &AsmType::Double;
            }
            _ => false,
        }
    } else if register_class == &RegisterClass::XMM {
        match ASM_SYMBOL_TABLE.lock().unwrap().get(pseudo).unwrap() {
            AsmSymtabEntry::Object {
                _type,
                is_static,
                is_constant,
            } => {
                return _type == &AsmType::Double;
            }
            _ => false,
        }
    } else {
        false
    }
}

 
fn get_pseudo_nodes(
    aliased_pseudos: &BTreeSet<String>,
    instructions: &[AsmInstruction],
    register_class: &RegisterClass,
) -> Vec<Node> {
    // Helper to initialize a node for a pseudo-register
    fn initialize_node(pseudo: String) -> Node {
        Node {
            id: NodeId::Pseudo(pseudo),
            neighbors: OperandSet::new(),
            spill_cost: 0.0,
            color: None,
            pruned: false,
        }
    }

    fn is_static(pseudo: &str) -> bool {
        match ASM_SYMBOL_TABLE.lock().unwrap().get(pseudo).unwrap() {
            AsmSymtabEntry::Object {
                _type,
                is_static,
                is_constant: _,
            } => *is_static,
            _ => false,
        }
    }

    // Helper function to check if the operand is a pseudo of the current type
    fn pseudo_is_valid(
        pseudo: &String,
        register_class: &RegisterClass,
        aliased_pseudos: &BTreeSet<String>,
    ) -> bool {
        // Assume `pseudo_is_current_type` and `is_static` are helper functions
        pseudo_is_current_type(pseudo, register_class) && !is_static(pseudo) && !aliased_pseudos.contains(pseudo)
    }

    // Extract and filter pseudo-registers from the instructions
    let mut pseudos: Vec<String> = instructions
        .iter()
        .flat_map(|instr| {
            get_operands(instr)
                .iter()
                .filter_map(|op| match op {
                    AsmOperand::Pseudo(r) if pseudo_is_valid(r, register_class, aliased_pseudos) => {
                        Some(r.clone())
                    }
                    _ => None,
                })
                .collect::<Vec<_>>()
        })
        .collect();

    // Remove duplicates by sorting and deduplicating
    pseudos.sort();
    pseudos.dedup();

    // Initialize nodes for each unique pseudo-register
    pseudos.into_iter().map(initialize_node).collect()
}


fn add_pseudo_nodes(
    graph: &mut Graph,
    aliased_pseudos: &BTreeSet<String>,
    instructions: &[AsmInstruction],
    register_class: &RegisterClass,
) {
    let pseudo_nodes = get_pseudo_nodes(aliased_pseudos, instructions, register_class);

    for node in pseudo_nodes {
        graph.insert(node.id.clone(), node); // Insert node into the graph
    }
}

impl std::fmt::Display for AsmOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            AsmOperand::Register(reg) => {
                let mut s = format!("{}", reg);
                s = s.replace("%", "");
                write!(f, "{}", s)
            }
            AsmOperand::Pseudo(pseudo) => write!(f, "{}", pseudo),
            AsmOperand::Memory(base, offset) => write!(f, "Memory({}, {})", base, offset),
            AsmOperand::Indexed(base, index, scale) => {
                write!(f, "Indexed({}, {}, {})", base, index, scale)
            }
            AsmOperand::Imm(imm) => write!(f, "Imm({})", imm),
            AsmOperand::Data(data, size) => write!(f, "Data({}, {})", data, size),
            AsmOperand::PseudoMem(name, offset) => write!(f, "PseudoMem({}, {})", name, offset),
        }
    }
}

fn print_graphviz(fn_name: &str, graph: &Graph) {
    use std::io::Write;

    let tmp = make_temporary();
    let dot_filename = format!("{}.{}.dot", fn_name, tmp);
    let png_filename = format!("{}.{}.png", fn_name, tmp);

    // Open a file for writing the Graphviz DOT format
    let mut file = match std::fs::File::create(&dot_filename) {
        Ok(f) => f,
        Err(e) => {
            return;
        }
    };

    writeln!(file, "graph {} {{", fn_name).expect("Failed to write to DOT file");

    // Iterate over the nodes in the graph
    for (key, node) in graph {
        let node_id = &node.id; // Assuming node has an `id` field

        let color = match node.color {
            Some(c) => format!("[color=red, style=filled, fillcolor=\"/set312/{}\"]", c),
            None => String::new(),
        };

        writeln!(file, "  \"{}\";", node_id).expect("Failed to write node to DOT file");

        // Write edges (neighbors) for each node
        for neighbor in &node.neighbors {
            writeln!(file, " \"{}\" -- \"{}\";", node_id, neighbor)
                .expect("Failed to write edge to DOT file");
        }
    }

    writeln!(file, "}}").expect("Failed to write closing brace to DOT file");

    // Run the Graphviz dot command to create the PNG file
    let output = std::process::Command::new("dot")
        .arg(&dot_filename)
        .arg("-Tpng")
        .arg("-o")
        .arg(&png_filename)
        .output();

    match output {
        Ok(o) if o.status.success() => {
            println!("Graphviz output successfully written to: {}", png_filename);
        }
        Ok(o) => {
            eprintln!("Graphviz failed with error: {:?}", o);
        }
        Err(e) => {
            eprintln!("Failed to execute Graphviz command: {}", e);
        }
    }
}

impl Instr for AsmInstruction {
    fn simplify(&self) -> SimpleInstr {
        match self {
            AsmInstruction::Label(lbl) => SimpleInstr::Label(lbl.clone()),
            AsmInstruction::JmpCC { target, .. } => SimpleInstr::ConditionalJump(target.clone()),
            AsmInstruction::Jmp { target } => SimpleInstr::UnconditionalJump(target.clone()),
            AsmInstruction::Ret => SimpleInstr::Return,
            _ => SimpleInstr::Other,
        }
    }

    fn pp_instr(&self) -> String {
        match self {
            AsmInstruction::Label(lbl) => format!("Label({})", lbl),
            AsmInstruction::JmpCC { target, .. } => format!("ConditionalJump({})", target),
            AsmInstruction::Jmp { target } => format!("UnconditionalJump({})", target),
            AsmInstruction::Ret => format!("Return"),
            _ => format!("{:?}", self),
        }
    }

    fn is_jump(&self) -> bool {
        match self {
            AsmInstruction::Jmp { .. } | AsmInstruction::JmpCC { .. } => true,
            _ => false,
        }
    }

    fn is_label(&self) -> bool {
        match self {
            AsmInstruction::Label(_) => true,
            _ => false,
        }
    }
}

// Assuming `AsmInstruction`, `AsmOperand`, `Pseudo`, `Node`, and `Graph` are already defined
fn add_spill_costs(
    graph: BTreeMap<NodeId, Node>,
    instructions: &[AsmInstruction],
) -> BTreeMap<NodeId, Node> {
    // Increment the count for a pseudo, or set it to 1 if not present
    let incr_count = |counts: &mut BTreeMap<String, usize>, pseudo: &str| {
        *counts.entry(pseudo.to_string()).or_insert(0) += 1;
    };

    // Get a list of all operands in the function, filter out all but pseudoregs
    let operands: Vec<AsmOperand> = instructions
        .iter()
        .flat_map(get_operands) // assuming get_operands returns an iterator or a Vec
        .collect();

    // Filter out only pseudoregs
    let pseudos: Vec<String> = operands
        .into_iter()
        .filter_map(|operand| match operand {
            AsmOperand::Pseudo(r) => Some(r),
            _ => None,
        })
        .collect();

    // Create a map from pseudoregs to counts - this may include pseudos that aren't in the interference graph
    let mut count_map: BTreeMap<String, usize> = BTreeMap::new();
    for pseudo in pseudos {
        incr_count(&mut count_map, &pseudo);
    }

    // Set each node's spill cost to the count from count_map
    graph
        .into_iter()
        .map(|(node_id, mut node)| {
            if let NodeId::Pseudo(ref r) = node.id {
                if let Some(&count) = count_map.get(r) {
                    node.spill_cost = count as f64;
                }
            }
            (node_id, node)
        })
        .collect()
}

use std::collections::VecDeque;

fn color_graph(mut graph: Graph, max_colors: usize, register_class: &RegisterClass) -> Graph {
    // Initialize a stack to keep track of the coloring order
    let mut stack: VecDeque<NodeId> = VecDeque::new();

    // Repeat until all nodes are pruned
    while !graph.values().all(|nd| nd.pruned) {
        // Find a node with degree less than max_colors
        let node_opt = graph
            .values()
            .find(|nd| {
                !nd.pruned
                    && nd
                        .neighbors
                        .iter()
                        .filter(|n| !graph.get(*n).unwrap().pruned)
                        .count()
                        < max_colors
            })
            .cloned();

        match node_opt {
            Some(node) => {
                graph.get_mut(&node.id).unwrap().pruned = true;
                stack.push_back(node.id.clone());
            }
            None => {
                // Spill: Choose a node to spill based on spill_cost / degree
                let spill_node = graph
                    .values()
                    .filter(|nd| !nd.pruned)
                    .min_by(|a, b| {
                        let a_degree = a
                            .neighbors
                            .iter()
                            .filter(|n| !graph.get(*n).unwrap().pruned)
                            .count();
                        let b_degree = b
                            .neighbors
                            .iter()
                            .filter(|n| !graph.get(*n).unwrap().pruned)
                            .count();
                        (a.spill_cost / a_degree as f64)
                            .partial_cmp(&(b.spill_cost / b_degree as f64))
                            .unwrap_or(std::cmp::Ordering::Equal)
                    })
                    .cloned()
                    .expect("No nodes available to spill");

                graph.get_mut(&spill_node.id).unwrap().pruned = true;
                stack.push_back(spill_node.id.clone());
            }
        }
    }

    // Assign colors
    while let Some(node_id) = stack.pop_back() {
        let node = graph.get(&node_id).unwrap();
        let mut used_colors = BTreeSet::new();

        // Collect colors used by the neighbors
        for neighbor_id in &node.neighbors {
            if let Some(neighbor) = graph.get(neighbor_id) {
                if let Some(color) = neighbor.color {
                    used_colors.insert(color);
                }
            }
        }

        // Get all available colors by removing the used colors
        let mut available_colors: Vec<usize> = (0..max_colors).collect();
        available_colors.retain(|c| !used_colors.contains(c));

        if !available_colors.is_empty() {
            if let NodeId::Register(reg) = &node_id {
                let caller_saved_regs = get_caller_saved_registers(register_class);

                if caller_saved_regs.contains(reg) {
                    // Assign the lowest color for caller-saved registers
                    graph.get_mut(&node_id).unwrap().color = Some(*available_colors.iter().min().unwrap());
                } else {
                    // Assign the highest color for callee-saved registers
                    graph.get_mut(&node_id).unwrap().color = Some(*available_colors.iter().max().unwrap());
                }
            } else {
                // Default: Assign the lowest available color
                graph.get_mut(&node_id).unwrap().color = Some(*available_colors.iter().min().unwrap());
            }
        } else {
            // In case no colors are available, the node remains uncolored (could be a spill)
            graph.get_mut(&node_id).unwrap().color = None;
        }

        // Reset the pruned flag, since we've assigned a color
        graph.get_mut(&node_id).unwrap().pruned = false;
    }

    graph
}


type IntMap = BTreeMap<usize, AsmRegister>;
type RegSet = BTreeSet<AsmRegister>;
type StringMap = BTreeMap<String, AsmRegister>;

fn make_register_map(
    fn_name: &str,
    graph: &BTreeMap<NodeId, Node>,
    register_class: &RegisterClass,
) -> BTreeMap<String, AsmRegister> {
    // Step 1: Build map from colors to hard registers
    let mut colors_to_regs: IntMap = BTreeMap::new();

    for (nd_id, node) in graph.iter() {
        match nd_id {
            NodeId::Register(r) => {
                if let Some(color) = node.color {
                    // Insert color -> register into colors_to_regs
                    // In Rust, BTreeMap::insert replaces existing entries, but colors are unique
                    colors_to_regs.insert(color, r.clone());
                }
            }
            _ => {
                // Ignore other NodeId variants
            }
        }
    }

    // Step 2: Build map from pseudoregisters to hard registers
    let mut used_callee_saved: RegSet = BTreeSet::new();
    let mut reg_map: StringMap = BTreeMap::new();

    for node in graph.values() {
        if let Node {
            id: NodeId::Pseudo(ref p),
            color: Some(c),
            ..
        } = node
        {
            if let Some(hardreg) = colors_to_regs.get(c) {
                let regs = get_caller_saved_registers(register_class);
                if !regs.contains(hardreg) {
                    used_callee_saved.insert(hardreg.clone());
                }

                reg_map.insert(p.clone(), hardreg.clone());
            }
        }
    }

    // Step 3: Update the symbol table with used callee-saved registers
    if let Some(func) = ASM_SYMBOL_TABLE.lock().unwrap().get_mut(fn_name) {
        if let AsmSymtabEntry::Function {
            ref mut callee_saved_regs_used,
            ..
        } = func
        {
            callee_saved_regs_used.extend(used_callee_saved);
        }
    }

    // Return the register map
    reg_map
}

fn replace_pseudoregs(
    instructions: &Vec<AsmInstruction>,
    reg_map: &BTreeMap<String, AsmRegister>,
) -> Vec<AsmInstruction> {
    let replace_op = |op: AsmOperand| -> AsmOperand {
        match op {
            AsmOperand::Pseudo(ref p) => {
                // Try to replace the pseudoreg with a hard register
                if let Some(&hardreg) = reg_map.get(p) {
                    AsmOperand::Register(hardreg)
                } else {
                    op
                }
            }
            _ => op,
        }
    };

    cleanup_movs(
        instructions
            .into_iter()
            .map(|instr| replace_ops(|instr| replace_op(instr), instr.to_owned()))
            .collect(),
    )
}

fn cleanup_movs(instructions: Vec<AsmInstruction>) -> Vec<AsmInstruction> {
    // Check if a move instruction is redundant (i.e., src and dst are the same)
    let is_redundant_mov = |instr: &AsmInstruction| -> bool {
        match instr {
            AsmInstruction::Mov { src, dst, .. } if src == dst => true,
            _ => false,
        }
    };

    // Filter out redundant move instructions
    instructions
        .into_iter()
        .filter(|instr| !is_redundant_mov(instr))
        .collect()
}

fn replace_ops<F>(mut f: F, instr: AsmInstruction) -> AsmInstruction
where
    F: FnMut(AsmOperand) -> AsmOperand,
{
    match instr {
        AsmInstruction::Mov { asm_type, src, dst } => AsmInstruction::Mov {
            asm_type,
            src: f(src),
            dst: f(dst),
        },
        AsmInstruction::Movsx {
            src,
            src_type,
            dst,
            dst_type,
        } => AsmInstruction::Movsx {
            src_type: src_type,
            src: f(src),
            dst_type: dst_type,
            dst: f(dst),
        },
        AsmInstruction::MovZeroExtend {
            src,
            src_type,
            dst,
            dst_type,
        } => AsmInstruction::MovZeroExtend {
            src_type: src_type,
            src: f(src),
            dst_type: dst_type,
            dst: f(dst),
        },
        AsmInstruction::Lea { src, dst } => AsmInstruction::Lea {
            src: f(src),
            dst: f(dst),
        },
        AsmInstruction::Cvttsd2si { asm_type, src, dst } => AsmInstruction::Cvttsd2si {
            asm_type,
            src: f(src),
            dst: f(dst),
        },
        AsmInstruction::Cvtsi2sd { asm_type, src, dst } => AsmInstruction::Cvtsi2sd {
            asm_type,
            src: f(src),
            dst: f(dst),
        },
        AsmInstruction::Unary {
            op,
            asm_type,
            operand,
        } => AsmInstruction::Unary {
            op,
            asm_type,
            operand: f(operand),
        },
        AsmInstruction::Binary {
            op,
            asm_type,
            lhs,
            rhs,
        } => AsmInstruction::Binary {
            asm_type: asm_type,
            op: op,
            lhs: f(lhs),
            rhs: f(rhs),
        },
        AsmInstruction::Cmp { asm_type, lhs, rhs } => AsmInstruction::Cmp {
            asm_type,
            lhs: f(lhs),
            rhs: f(rhs),
        },
        AsmInstruction::Idiv { asm_type, operand } => AsmInstruction::Idiv {
            asm_type,
            operand: f(operand),
        },
        AsmInstruction::Div { asm_type, operand } => AsmInstruction::Div {
            asm_type,
            operand: f(operand),
        },
        AsmInstruction::SetCC { condition, operand } => AsmInstruction::SetCC {
            condition,
            operand: f(operand),
        },
        AsmInstruction::Push(v) => AsmInstruction::Push(f(v)),
        AsmInstruction::Label(_)
        | AsmInstruction::Call(_)
        | AsmInstruction::Ret
        | AsmInstruction::Cdq { .. }
        | AsmInstruction::Jmp { .. }
        | AsmInstruction::JmpCC { .. }
        | _ => instr,
    }
}

const GP_REGISTERS: &[AsmRegister] = &[
    AsmRegister::AX,
    AsmRegister::BX,
    AsmRegister::CX,
    AsmRegister::DX,
    AsmRegister::SI,
    AsmRegister::DI,
    AsmRegister::R8,
    AsmRegister::R9,
    AsmRegister::R12,
    AsmRegister::R13,
    AsmRegister::R14,
    AsmRegister::R15,
];

const XMM_REGISTERS: &[AsmRegister] = &[
    AsmRegister::XMM0,
    AsmRegister::XMM1,
    AsmRegister::XMM2,
    AsmRegister::XMM3,
    AsmRegister::XMM4,
    AsmRegister::XMM5,
    AsmRegister::XMM6,
    AsmRegister::XMM7,
    AsmRegister::XMM8,
    AsmRegister::XMM9,
    AsmRegister::XMM10,
    AsmRegister::XMM11,
    AsmRegister::XMM12,
    AsmRegister::XMM13,
];

// General-Purpose Caller-Saved Registers
const GP_CALLER_SAVED_REGISTERS: &[AsmRegister] = &[
    AsmRegister::AX,
    AsmRegister::CX,
    AsmRegister::DX,
    AsmRegister::SI,
    AsmRegister::DI,
    AsmRegister::R8,
    AsmRegister::R9,
];

// XMM Caller-Saved Registers
const XMM_CALLER_SAVED_REGISTERS: &[AsmRegister] = &[
    AsmRegister::XMM0,
    AsmRegister::XMM1,
    AsmRegister::XMM2,
    AsmRegister::XMM3,
    AsmRegister::XMM4,
    AsmRegister::XMM5,
    AsmRegister::XMM6,
    AsmRegister::XMM7,
    AsmRegister::XMM8,
    AsmRegister::XMM9,
    AsmRegister::XMM10,
    AsmRegister::XMM11,
    AsmRegister::XMM12,
    AsmRegister::XMM13,
];

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum RegisterClass {
    GP,
    XMM,
}

fn get_caller_saved_registers(register_class: &RegisterClass) -> &[AsmRegister] {
    match register_class {
        RegisterClass::GP => GP_CALLER_SAVED_REGISTERS,
        RegisterClass::XMM => XMM_CALLER_SAVED_REGISTERS,
    }
}

#[derive(Debug, PartialEq, Clone)]
struct DisjointSet<T: Ord + Clone> {
    reg_map: BTreeMap<T, T>,  // Map to store the parent of each element
}

impl<T: Ord + Clone> DisjointSet<T> {
    // ❶ Initialize an empty disjoint set
    pub fn new() -> Self {
        Self {
            reg_map: BTreeMap::new(),
        }
    }

    // ❷ Union operation: Add an entry (x -> y) in the map
    pub fn union(&mut self, x: T, y: T) {
        self.reg_map.insert(x, y);
    }

    // ❸ Find operation: Find the root of 'r' with path compression
    pub fn find(&mut self, r: T) -> T {
        if let Some(parent) = self.reg_map.get(&r).cloned() {
            // ❹ Recursively find the root and apply path compression
            let root = self.find(parent.clone());
            self.reg_map.insert(r.clone(), root.clone()); // Path compression
            root
        } else {
            r // If 'r' is not in the map, it is its own root
        }
    }

    // ❺ Check if the reg_map is empty (meaning nothing was coalesced)
    pub fn nothing_was_coalesced(&self) -> bool {
        self.reg_map.is_empty()
    }
}

fn coalesce(graph: &mut Graph, instructions: &[AsmInstruction], register_class: &RegisterClass) -> DisjointSet<AsmOperand> {
    let mut coalesced_regs = DisjointSet::new();
    
    for i in instructions {
        match i {
            AsmInstruction::Mov { asm_type, src, dst } => {
                let src = coalesced_regs.find(src.clone());
                let dst = coalesced_regs.find(dst.clone());

                // if src is in the graph
                if let Some(src_node) = graph.get(&src) {
                    // if dst is in the graph
                    if let Some(dst_node) = graph.get(&dst) {
                        // if src and dst are not the same
                        if src != dst {
                            // if src and dst are not neighbors
                            if !src_node.neighbors.contains(&dst) {

                                if conservative_coalesceable(graph, src.clone(), dst.clone(), register_class) {
                                    // Coalesce src and dst
                                    
                                    let to_keep;
                                    let to_merge;
                                    if let AsmOperand::Register(reg) = src {
                                        if GP_REGISTERS.contains(&reg) || XMM_REGISTERS.contains(&reg) {
                                            to_keep = src;
                                            to_merge = dst;
                                        } else {
                                            to_keep = dst;
                                            to_merge = src;
                                        }
                                    } else {
                                        to_keep = dst;
                                        to_merge = src;
                                    }

                                    coalesced_regs.union(to_merge.clone(), to_keep.clone());
                                    update_graph(graph, to_merge, to_keep);
                                }
                            }
                        }
                    }
                }
            }
            _ => continue,
        }
    }

    coalesced_regs.clone()
}

fn rewrite_coalesced(
    instructions: Vec<AsmInstruction>,
    coalesced_regs: &mut DisjointSet<AsmOperand>
) -> Vec<AsmInstruction> {
    // Closure to rewrite an individual instruction
    let mut rewrite_instruction = |instr: &AsmInstruction| -> Option<AsmInstruction> {
        match instr {
            AsmInstruction::Mov { asm_type, src, dst } => {
                let new_src = coalesced_regs.find(src.clone());
                let new_dst = coalesced_regs.find(dst.clone());
                // Remove move if the source and destination are the same
                if new_src == new_dst {
                    None
                } else {
                    Some(AsmInstruction::Mov { asm_type: asm_type.to_owned(), src: new_src, dst: new_dst })
                }
            }
            // Handle other instructions by replacing their operands using coalesced_regs.find
            _ => Some(replace_ops(
                |x| coalesced_regs.find(x.clone()), // Inline the closure to avoid double borrowing
                instr.clone(),
            )),
        }
    };

    // Filter and map instructions, removing redundant moves
    instructions
        .into_iter()
        .filter_map(|x| rewrite_instruction(&x))
        .collect()
}

fn update_graph(graph: &mut Graph, x: AsmOperand, y: AsmOperand) {
    let node_to_remove = graph.get(&x).unwrap().clone();

    for neighbor in node_to_remove.neighbors.iter() {
        add_edge(graph, &y, &neighbor);
        remove_edge(graph, &x, &neighbor);
    }

    graph.remove(&x);
}

fn remove_edge(graph: &mut Graph, x: &AsmOperand, y: &AsmOperand) {
    let x_node = graph.get_mut(x).unwrap();
    x_node.neighbors.remove(y);
    let y_node = graph.get_mut(y).unwrap();
    y_node.neighbors.remove(x);
}

fn conservative_coalesceable(graph: &mut Graph, src: AsmOperand, dst: AsmOperand, register_class: &RegisterClass) -> bool {
    if briggs_test(graph, &src, &dst, register_class) {
        return true;
    }
    if let AsmOperand::Register(reg) = src {
        return george_test(graph, src, dst, register_class);
    }
    if let AsmOperand::Register(reg) = dst {
        return george_test(graph, dst, src, register_class);
    }
    false
}

fn briggs_test(graph: &mut Graph, src: &AsmOperand, dst: &AsmOperand, register_class: &RegisterClass) -> bool {
    let mut significant_neighbors = 0;

    let k = match register_class {
        RegisterClass::GP => GP_REGISTERS.len(),
        RegisterClass::XMM => XMM_REGISTERS.len(),
    };

    let x_node = graph.get(&src).unwrap();
    let y_node = graph.get(&dst).unwrap();

    let mut combined_neighbors = x_node.neighbors.iter().collect::<BTreeSet<_>>();
    combined_neighbors = combined_neighbors.union(&y_node.neighbors.iter().collect::<BTreeSet<_>>()).cloned().collect::<BTreeSet<_>>();

    for n in combined_neighbors {
        let neighbor_node = graph.get(&n).unwrap();
        let mut degree = neighbor_node.neighbors.len();

        // if are neighbors n,x and n,y
        if neighbor_node.neighbors.contains(&src) && neighbor_node.neighbors.contains(&dst) {
            degree -= 1;
        }

        if degree >= k {
            significant_neighbors += 1;
        }
    }

    significant_neighbors < k
}

fn george_test(graph: &mut Graph, hardreg: AsmOperand, pseudoreg: AsmOperand, register_class: &RegisterClass) -> bool {
    let pseudo_node = graph.get(&pseudoreg).unwrap();

    let k = match register_class {
        RegisterClass::GP => GP_REGISTERS.len(),
        RegisterClass::XMM => XMM_REGISTERS.len(),
    };

    for n in pseudo_node.neighbors.iter() {
        let neighbor_node = graph.get(&n).unwrap();
        if neighbor_node.neighbors.contains(&hardreg) {
            continue;
        }

        if neighbor_node.neighbors.len() < k {
            continue;
        }

        return false;
    }
    
    true
}