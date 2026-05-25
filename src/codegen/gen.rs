use std::{
    collections::{BTreeMap, BTreeSet},
    sync::Mutex,
};

use crate::{
    ir::gen::{
        make_temporary, BinaryOp, IRFunction, IRInstruction, IRNode, IRProgram, IRStaticConstant,
        IRStaticVariable, IRValue, UnaryOp,
    },
    lexer::lex::Const,
    parser::ast::Type,
    semantics::typechecker::{
        get_common_type, get_signedness, get_size_of_type, is_pointer_type, is_scalar,
        IdentifierAttrs, MemberEntry, StaticInit, StructEntry, SYMBOL_TABLE, TYPE_TABLE,
    },
};

const VA_GP_REG_SAVE_SIZE: usize = 48;
const VA_FP_REG_SAVE_SIZE: usize = 128;
const VA_REG_SAVE_AREA_SIZE: usize = VA_GP_REG_SAVE_SIZE + VA_FP_REG_SAVE_SIZE;

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
    pub functions: Vec<AsmFunction>,
    pub static_vars: Vec<AsmStaticVariable>,
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

    /* Move with sign extension. */
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

    /* Convert with truncation scalar double to signed integer. */
    Cvttsd2si {
        asm_type: AsmType,
        src: AsmOperand,
        dst: AsmOperand,
    },

    /* Convert signed integer to scalar double. */
    Cvtsi2sd {
        asm_type: AsmType,
        src: AsmOperand,
        dst: AsmOperand,
    },

    /* Convert with truncation scalar float to signed integer. */
    Cvttss2si {
        asm_type: AsmType,
        src: AsmOperand,
        dst: AsmOperand,
    },

    /* Convert signed integer to scalar float. */
    Cvtsi2ss {
        asm_type: AsmType,
        src: AsmOperand,
        dst: AsmOperand,
    },

    /* Convert scalar double to scalar float. */
    Cvtsd2ss {
        src: AsmOperand,
        dst: AsmOperand,
    },

    /* Convert scalar float to scalar double. */
    Cvtss2sd {
        src: AsmOperand,
        dst: AsmOperand,
    },

    /* Generic 'unary' instruction. */
    Unary {
        asm_type: AsmType,
        op: AsmUnaryOp,
        operand: AsmOperand,
    },

    /* Generic 'binary' instruction. */
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

    /* Signed division.
     * It divides the value in accumulator (AX, AX:DX, RAX:RDX) by the
     * specified divisor.  The result of the division are quotient and
     * remainder.  The quotient is stored in AX (RAX for 64-bit), and
     * the remainder is stored in DX (RDX for 64-bit). */
    Idiv {
        asm_type: AsmType,
        operand: AsmOperand,
    },

    /* Unsigned division.
     * It divides the value in accumulator (AX, AX:DX, RAX:RDX) by the
     * specified divisor.  The result of the division are quotient and
     * remainder.  The quotient is stored in AX (RAX for 64-bit), and
     * the remainder is stored in DX (RDX for 64-bit). */
    Div {
        asm_type: AsmType,
        operand: AsmOperand,
    },

    /* Sign-extends EAX into EDX.  Used before performing a signed division
     * to form a 64-bit dividend pair consisting of EDX:EAX. */
    Cdq {
        asm_type: AsmType,
    },

    /* Unconditional jump to 'target'. */
    Jmp {
        target: String,
    },

    /* Conditional jump to 'target'. */
    JmpCC {
        condition: ConditionCode,
        target: String,
    },

    /* Set a byte in a register to 1 or 0 based on the result of a previous
     * comparison or arithmetic operation.
     *
     * For example:
     *
     * mov $5, %eax
     * cmp $3, %eax  # sets flags
     * setne %al     # set AL to 1 if not equal, 0 otherwise
     *
     * See ConditionCode enum for more info.
     */
    SetCC {
        condition: ConditionCode,
        operand: AsmOperand,
    },

    /* Computes the effective address (without a memory access) of 'src' and
     * stores it in 'dst'.
     *
     * For example:
     *
     * movl $5, %eax
     * movl $3, %ecx
     * leal 8(%eax, %ecx, 4), %ebx
     *
     * 8: displacement
     * %eax: base
     * %ecx: index
     * 4: scale factor
     *
     * ...so, this is:
     *
     * EBX = EAX + ECX*4 + 8 */
    Lea {
        src: AsmOperand,
        dst: AsmOperand,
    },

    Push(AsmOperand),
    Pop(AsmRegister),
    Label(String),
    Call(String),
    Ret,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConditionCode {
    /* signed */
    E,  /* equal */
    NE, /* not equal */
    L,  /* less, */
    LE, /* less or equal */
    G,  /* greater */
    GE, /* greater or equal */

    /* unsigned */
    A,  /* above */
    AE, /* above or equal */
    B,  /* below */
    BE, /* below or equal */
    P,  /* parity / unordered floating-point comparison */
    NP, /* not parity / ordered floating-point comparison */
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
    Ax,
    Bx,
    Cx,
    Dx,
    Di,
    Si,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    Bp,
    Sp,
    Xmm0,
    Xmm1,
    Xmm2,
    Xmm3,
    Xmm4,
    Xmm5,
    Xmm6,
    Xmm7,
    Xmm8,
    Xmm9,
    Xmm10,
    Xmm11,
    Xmm12,
    Xmm13,
    Xmm14,
    Xmm15,
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
    Sal,
    Sar,
    Shl,
    ShrTwoOp,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum AsmType {
    Byte,
    Word,
    Longword,
    Quadword,
    Float,
    Double,
    Bytearray { size: usize, alignment: usize },
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
            functions.push(func.codegen().into());
        }

        let mut static_vars = vec![];
        for static_var in &self.static_vars {
            static_vars.push(static_var.codegen().into());
        }

        let mut static_constants = vec![];
        for static_const in &self.static_constants {
            static_constants.push(static_const.codegen().into());
        }

        static_constants = static_constants
            .into_iter()
            .chain(
                STATIC_CONSTANTS
                    .lock()
                    .unwrap()
                    .iter()
                    .map(|x| x.to_owned()),
            )
            .collect::<Vec<AsmStaticConstant>>();

        AsmNode::Program(AsmProgram {
            functions,
            static_vars,
            static_constants,
        })
    }
}

impl Codegen for IRStaticVariable {
    fn codegen(&self) -> AsmNode {
        AsmNode::StaticVariable(AsmStaticVariable {
            name: self.name.clone(),
            init: self.init.clone(),
            global: self.global,
            alignment: get_alignment_of_type(&self.ty),
        })
    }
}

impl Codegen for IRStaticConstant {
    fn codegen(&self) -> AsmNode {
        AsmNode::StaticConstant(AsmStaticConstant {
            name: self.name.clone(),
            init: self.init.clone(),
            alignment: get_alignment_of_type(&self.ty),
        })
    }
}

fn vararg_register_save_area_name(function_name: &str) -> String {
    format!("{}.va_reg_save_area", function_name)
}

fn ensure_local_object_asm_symbol(name: String, ty: AsmType) {
    ASM_SYMBOL_TABLE
        .lock()
        .unwrap()
        .entry(name)
        .or_insert(AsmSymtabEntry::Object {
            ty,
            is_static: false,
            is_constant: false,
        });
}

fn emit_vararg_register_save_area(function_name: &str) -> Vec<AsmInstruction> {
    let save_area_name = vararg_register_save_area_name(function_name);
    ensure_local_object_asm_symbol(
        save_area_name.clone(),
        AsmType::Bytearray {
            size: VA_REG_SAVE_AREA_SIZE,
            alignment: 16,
        },
    );

    let mut instructions = vec![];
    let int_regs = [
        AsmRegister::Di,
        AsmRegister::Si,
        AsmRegister::Dx,
        AsmRegister::Cx,
        AsmRegister::R8,
        AsmRegister::R9,
    ];

    for (idx, reg) in int_regs.iter().enumerate() {
        instructions.push(AsmInstruction::Mov {
            asm_type: AsmType::Quadword,
            src: AsmOperand::Register(*reg),
            dst: AsmOperand::PseudoMem(save_area_name.clone(), (idx * 8) as isize),
        });
    }

    let double_regs = [
        AsmRegister::Xmm0,
        AsmRegister::Xmm1,
        AsmRegister::Xmm2,
        AsmRegister::Xmm3,
        AsmRegister::Xmm4,
        AsmRegister::Xmm5,
        AsmRegister::Xmm6,
        AsmRegister::Xmm7,
    ];

    for (idx, reg) in double_regs.iter().enumerate() {
        instructions.push(AsmInstruction::Mov {
            asm_type: AsmType::Double,
            src: AsmOperand::Register(*reg),
            dst: AsmOperand::PseudoMem(
                save_area_name.clone(),
                (VA_GP_REG_SAVE_SIZE + idx * 16) as isize,
            ),
        });
    }

    instructions
}

fn function_vararg_offsets(function_name: &str) -> (usize, usize, isize) {
    let symbol = SYMBOL_TABLE
        .lock()
        .unwrap()
        .get(function_name)
        .cloned()
        .unwrap();
    let return_on_stack = returns_on_stack(function_name);
    let Type::Func { params, .. } = symbol.ty else {
        unreachable!()
    };

    let typed_params = params
        .iter()
        .map(|t| {
            if is_scalar(t) {
                (t.clone(), AsmOperand::Pseudo("DUMMY".to_string()))
            } else {
                (t.clone(), AsmOperand::PseudoMem("DUMMY".to_string(), 0))
            }
        })
        .collect::<Vec<_>>();

    let (int_reg_params, double_reg_params, stack_params) =
        classify_params_helper(&typed_params, return_on_stack);

    let hidden_sret_registers = if return_on_stack { 1 } else { 0 };
    let gp_offset = (hidden_sret_registers + int_reg_params.len()) * 8;
    let fp_offset = VA_GP_REG_SAVE_SIZE + double_reg_params.len() * 16;
    let overflow_arg_area = 16 + stack_params.len() * 8;

    (gp_offset, fp_offset, overflow_arg_area as isize)
}

fn codegen_va_start(function_name: &str, list: &IRValue) -> Vec<AsmInstruction> {
    let (gp_offset, fp_offset, overflow_arg_area) = function_vararg_offsets(function_name);
    let save_area_name = vararg_register_save_area_name(function_name);

    vec![
        AsmInstruction::Mov {
            asm_type: AsmType::Quadword,
            src: list.codegen().into(),
            dst: AsmOperand::Register(AsmRegister::R11),
        },
        AsmInstruction::Mov {
            asm_type: AsmType::Longword,
            src: AsmOperand::Imm(gp_offset as i64),
            dst: AsmOperand::Memory(AsmRegister::R11, 0),
        },
        AsmInstruction::Mov {
            asm_type: AsmType::Longword,
            src: AsmOperand::Imm(fp_offset as i64),
            dst: AsmOperand::Memory(AsmRegister::R11, 4),
        },
        AsmInstruction::Lea {
            src: AsmOperand::Memory(AsmRegister::Bp, overflow_arg_area),
            dst: AsmOperand::Register(AsmRegister::R10),
        },
        AsmInstruction::Mov {
            asm_type: AsmType::Quadword,
            src: AsmOperand::Register(AsmRegister::R10),
            dst: AsmOperand::Memory(AsmRegister::R11, 8),
        },
        AsmInstruction::Lea {
            src: AsmOperand::PseudoMem(save_area_name, 0),
            dst: AsmOperand::Register(AsmRegister::R10),
        },
        AsmInstruction::Mov {
            asm_type: AsmType::Quadword,
            src: AsmOperand::Register(AsmRegister::R10),
            dst: AsmOperand::Memory(AsmRegister::R11, 16),
        },
    ]
}

#[derive(Debug, Clone)]
struct VaArgPiece {
    class: Class,
    offset: usize,
    asm_type: AsmType,
}

#[derive(Debug, Clone)]
struct VaArgLayout {
    pieces: Vec<VaArgPiece>,
    gp_count: usize,
    fp_count: usize,
    stack_only: bool,
    stack_size: usize,
    stack_alignment: usize,
}

fn round_up_to(value: usize, alignment: usize) -> usize {
    if alignment == 0 {
        value
    } else {
        (value + alignment - 1) & !(alignment - 1)
    }
}

fn classify_va_arg_type(ty: &Type) -> VaArgLayout {
    let size = get_size_of_type(ty);
    let stack_size = round_up_to(size, 8);
    let stack_alignment = get_alignment_of_type(ty);

    match ty {
        Type::Struct { tag } | Type::Union { tag } => {
            let entry = TYPE_TABLE.lock().unwrap().get(tag).cloned().unwrap();
            let classes = classify_structure(&entry);

            if classes.first() == Some(&Class::Memory) {
                return VaArgLayout {
                    pieces: vec![],
                    gp_count: 0,
                    fp_count: 0,
                    stack_only: true,
                    stack_size,
                    stack_alignment,
                };
            }

            let mut pieces = vec![];
            let mut gp_count = 0;
            let mut fp_count = 0;
            let mut offset = 0;

            for class in classes {
                let asm_type = match class {
                    Class::Integer => {
                        gp_count += 1;
                        get_eightbyte_type(offset, entry.size)
                    }
                    Class::Sse => {
                        fp_count += 1;
                        get_sse_eightbyte_type(offset, entry.size)
                    }
                    Class::Memory => unreachable!(),
                };

                pieces.push(VaArgPiece {
                    class,
                    offset: offset.try_into().unwrap(),
                    asm_type,
                });
                offset += 8;
            }

            VaArgLayout {
                pieces,
                gp_count,
                fp_count,
                stack_only: false,
                stack_size,
                stack_alignment,
            }
        }
        Type::Float | Type::Double => VaArgLayout {
            pieces: vec![VaArgPiece {
                class: Class::Sse,
                offset: 0,
                asm_type: type2asmtype(ty),
            }],
            gp_count: 0,
            fp_count: 1,
            stack_only: false,
            stack_size,
            stack_alignment,
        },
        _ => VaArgLayout {
            pieces: vec![VaArgPiece {
                class: Class::Integer,
                offset: 0,
                asm_type: type2asmtype(ty),
            }],
            gp_count: 1,
            fp_count: 0,
            stack_only: false,
            stack_size,
            stack_alignment,
        },
    }
}

fn offset_operand(op: &AsmOperand, offset: usize) -> AsmOperand {
    if offset == 0 {
        op.clone()
    } else {
        add_offset(offset, op)
    }
}

fn copy_bytes_with_gp_scratch(
    src: &AsmOperand,
    dst: &AsmOperand,
    byte_count: usize,
    scratch: AsmRegister,
) -> Vec<AsmInstruction> {
    if byte_count == 0 {
        return vec![];
    }

    let (operand_type, operand_size) = if byte_count >= 8 {
        (AsmType::Quadword, 8)
    } else if byte_count >= 4 {
        (AsmType::Longword, 4)
    } else if byte_count >= 2 {
        (AsmType::Word, 2)
    } else {
        (AsmType::Byte, 1)
    };

    let mut instructions = vec![
        AsmInstruction::Mov {
            asm_type: operand_type,
            src: src.clone(),
            dst: AsmOperand::Register(scratch),
        },
        AsmInstruction::Mov {
            asm_type: operand_type,
            src: AsmOperand::Register(scratch),
            dst: dst.clone(),
        },
    ];

    instructions.extend(copy_bytes_with_gp_scratch(
        &add_offset(operand_size, src),
        &add_offset(operand_size, dst),
        byte_count - operand_size,
        scratch,
    ));

    instructions
}

fn copy_qwords_with_xmm_scratch(
    src: &AsmOperand,
    dst: &AsmOperand,
    byte_count: usize,
) -> Vec<AsmInstruction> {
    assert!(byte_count % 8 == 0);

    let mut instructions = vec![];
    let mut offset = 0;

    while offset < byte_count {
        instructions.push(AsmInstruction::Mov {
            asm_type: AsmType::Double,
            src: add_offset(offset, src),
            dst: AsmOperand::Register(AsmRegister::Xmm14),
        });
        instructions.push(AsmInstruction::Mov {
            asm_type: AsmType::Double,
            src: AsmOperand::Register(AsmRegister::Xmm14),
            dst: add_offset(offset, dst),
        });
        offset += 8;
    }

    instructions
}

fn copy_va_arg_from_address(src_reg: AsmRegister, dst: &IRValue, ty: &Type) -> Vec<AsmInstruction> {
    let dst_operand = dst.codegen().into();

    if is_scalar(ty) {
        let asm_type = type2asmtype(ty);
        let scratch = if is_sse_asm_type(&asm_type) {
            AsmOperand::Register(AsmRegister::Xmm14)
        } else {
            AsmOperand::Register(AsmRegister::R11)
        };

        vec![
            AsmInstruction::Mov {
                asm_type,
                src: AsmOperand::Memory(src_reg, 0),
                dst: scratch.clone(),
            },
            AsmInstruction::Mov {
                asm_type,
                src: scratch,
                dst: dst_operand,
            },
        ]
    } else {
        copy_bytes_with_gp_scratch(
            &AsmOperand::Memory(src_reg, 0),
            &dst_operand,
            get_size_of_type(ty),
            AsmRegister::R11,
        )
    }
}

fn codegen_va_arg(list: &IRValue, arg_ty: &Type, dst: &IRValue) -> Vec<AsmInstruction> {
    let layout = classify_va_arg_type(arg_ty);
    let overflow_label = format!("VaArg.{}.overflow", make_temporary());
    let done_label = format!("VaArg.{}.done", make_temporary());
    let mut instructions = vec![AsmInstruction::Mov {
        asm_type: AsmType::Quadword,
        src: list.codegen().into(),
        dst: AsmOperand::Register(AsmRegister::R11),
    }];

    if !layout.stack_only {
        if layout.gp_count > 0 {
            instructions.extend(vec![
                AsmInstruction::Mov {
                    asm_type: AsmType::Longword,
                    src: AsmOperand::Memory(AsmRegister::R11, 0),
                    dst: AsmOperand::Register(AsmRegister::R10),
                },
                AsmInstruction::Cmp {
                    asm_type: AsmType::Longword,
                    lhs: AsmOperand::Imm((VA_GP_REG_SAVE_SIZE - layout.gp_count * 8) as i64),
                    rhs: AsmOperand::Register(AsmRegister::R10),
                },
                AsmInstruction::JmpCC {
                    condition: ConditionCode::A,
                    target: overflow_label.clone(),
                },
            ]);
        }

        if layout.fp_count > 0 {
            instructions.extend(vec![
                AsmInstruction::Mov {
                    asm_type: AsmType::Longword,
                    src: AsmOperand::Memory(AsmRegister::R11, 4),
                    dst: AsmOperand::Register(AsmRegister::R10),
                },
                AsmInstruction::Cmp {
                    asm_type: AsmType::Longword,
                    lhs: AsmOperand::Imm((VA_REG_SAVE_AREA_SIZE - layout.fp_count * 16) as i64),
                    rhs: AsmOperand::Register(AsmRegister::R10),
                },
                AsmInstruction::JmpCC {
                    condition: ConditionCode::A,
                    target: overflow_label.clone(),
                },
            ]);
        }

        let dst_operand: AsmOperand = dst.codegen().into();
        let mut gp_piece_index = 0;
        let mut fp_piece_index = 0;
        for piece in &layout.pieces {
            let (offset_field, piece_reg_save_offset) = match piece.class {
                Class::Integer => {
                    let offset = gp_piece_index * 8;
                    gp_piece_index += 1;
                    (0, offset)
                }
                Class::Sse => {
                    let offset = fp_piece_index * 16;
                    fp_piece_index += 1;
                    (4, offset)
                }
                Class::Memory => unreachable!(),
            };

            instructions.extend(vec![
                AsmInstruction::Mov {
                    asm_type: AsmType::Quadword,
                    src: AsmOperand::Memory(AsmRegister::R11, 16),
                    dst: AsmOperand::Register(AsmRegister::R10),
                },
                AsmInstruction::Mov {
                    asm_type: AsmType::Longword,
                    src: AsmOperand::Memory(AsmRegister::R11, offset_field),
                    dst: AsmOperand::Register(AsmRegister::R9),
                },
            ]);

            if piece_reg_save_offset != 0 {
                instructions.push(AsmInstruction::Binary {
                    asm_type: AsmType::Longword,
                    op: AsmBinaryOp::Add,
                    lhs: AsmOperand::Imm(piece_reg_save_offset as i64),
                    rhs: AsmOperand::Register(AsmRegister::R9),
                });
            }

            instructions.extend(vec![
                AsmInstruction::Lea {
                    src: AsmOperand::Indexed(AsmRegister::R10, AsmRegister::R9, 1),
                    dst: AsmOperand::Register(AsmRegister::R10),
                },
                AsmInstruction::Mov {
                    asm_type: piece.asm_type,
                    src: AsmOperand::Memory(AsmRegister::R10, 0),
                    dst: offset_operand(&dst_operand, piece.offset),
                },
            ]);
        }

        if layout.gp_count > 0 {
            instructions.push(AsmInstruction::Binary {
                asm_type: AsmType::Longword,
                op: AsmBinaryOp::Add,
                lhs: AsmOperand::Imm((layout.gp_count * 8) as i64),
                rhs: AsmOperand::Memory(AsmRegister::R11, 0),
            });
        }

        if layout.fp_count > 0 {
            instructions.push(AsmInstruction::Binary {
                asm_type: AsmType::Longword,
                op: AsmBinaryOp::Add,
                lhs: AsmOperand::Imm((layout.fp_count * 16) as i64),
                rhs: AsmOperand::Memory(AsmRegister::R11, 4),
            });
        }

        instructions.push(AsmInstruction::Jmp {
            target: done_label.clone(),
        });
    }

    instructions.push(AsmInstruction::Label(overflow_label));
    instructions.push(AsmInstruction::Mov {
        asm_type: AsmType::Quadword,
        src: AsmOperand::Memory(AsmRegister::R11, 8),
        dst: AsmOperand::Register(AsmRegister::R10),
    });

    if layout.stack_alignment > 8 {
        let alignment = layout.stack_alignment as i64;
        instructions.extend(vec![
            AsmInstruction::Binary {
                asm_type: AsmType::Quadword,
                op: AsmBinaryOp::Add,
                lhs: AsmOperand::Imm(alignment - 1),
                rhs: AsmOperand::Register(AsmRegister::R10),
            },
            AsmInstruction::Binary {
                asm_type: AsmType::Quadword,
                op: AsmBinaryOp::And,
                lhs: AsmOperand::Imm(-alignment),
                rhs: AsmOperand::Register(AsmRegister::R10),
            },
        ]);
    }

    instructions.extend(copy_va_arg_from_address(AsmRegister::R10, dst, arg_ty));
    instructions.push(AsmInstruction::Mov {
        asm_type: AsmType::Quadword,
        src: list.codegen().into(),
        dst: AsmOperand::Register(AsmRegister::R11),
    });
    instructions.extend(vec![
        AsmInstruction::Lea {
            src: AsmOperand::Memory(AsmRegister::R10, layout.stack_size as isize),
            dst: AsmOperand::Register(AsmRegister::R10),
        },
        AsmInstruction::Mov {
            asm_type: AsmType::Quadword,
            src: AsmOperand::Register(AsmRegister::R10),
            dst: AsmOperand::Memory(AsmRegister::R11, 8),
        },
        AsmInstruction::Label(done_label),
    ]);

    instructions
}

fn codegen_va_copy(dst: &IRValue, src: &IRValue) -> Vec<AsmInstruction> {
    let mut instructions = vec![
        AsmInstruction::Mov {
            asm_type: AsmType::Quadword,
            src: dst.codegen().into(),
            dst: AsmOperand::Register(AsmRegister::R11),
        },
        AsmInstruction::Mov {
            asm_type: AsmType::Quadword,
            src: src.codegen().into(),
            dst: AsmOperand::Register(AsmRegister::R10),
        },
    ];

    instructions.extend(copy_qwords_with_xmm_scratch(
        &AsmOperand::Memory(AsmRegister::R10, 0),
        &AsmOperand::Memory(AsmRegister::R11, 0),
        24,
    ));

    instructions
}

impl Codegen for IRFunction {
    fn codegen(&self) -> AsmNode {
        /* Determine if the function returns a value on the stack.
         *
         * Some functions return large values or structs, which need to be returned
         * via memory instead of registers. If this is the case, we reserve space for
         * the return address on the stack. */
        let return_on_stack = returns_on_stack(&self.name);

        let params_as_ir = self
            .params
            .iter()
            .map(|x| IRValue::Var(x.clone()))
            .collect::<Vec<IRValue>>();

        let mut instructions = vec![];

        /* Split the function parameters into three categories:
         * 1. Integer register parameters
         * 2. Floating-point register parameters
         * 3. Parameters passed via the stack */
        let (int_reg_params, double_reg_params, stack_params) =
            classify_parameters(&params_as_ir, return_on_stack);

        /* The list of integer parameter passing registers. */
        let int_regs: [AsmRegister; 6] = [
            AsmRegister::Di,
            AsmRegister::Si,
            AsmRegister::Dx,
            AsmRegister::Cx,
            AsmRegister::R8,
            AsmRegister::R9,
        ];

        let mut reg_index = 0;

        if is_variadic_function(&self.name) {
            instructions.extend(emit_vararg_register_save_area(&self.name));
        }

        /* If the function returns a value on the stack, we need to store the pointer to
         * the return buffer in memory at -8(%bp). This is done because the Di register is
         * normally used for the first parameter, but if a return buffer is present, it takes
         * precedence and is saved here before processing the actual parameters. */
        if return_on_stack {
            instructions.push(AsmInstruction::Mov {
                asm_type: AsmType::Quadword,
                src: AsmOperand::Register(AsmRegister::Di),
                dst: AsmOperand::Memory(AsmRegister::Bp, -8),
            });

            /* Skip the first register as it's now used for the return buffer. */
            reg_index = 1;
        }

        /* Process integer register parameters.
         *
         * For each parameter passed in an integer register, we move it from the register
         * to the appropriate location (either a local variable or an argument slot in memory).
         * If the parameter is a byte array, we use a specialized function to copy it. */
        for (param_type, param) in int_reg_params.iter() {
            let reg = int_regs[reg_index];

            match param_type {
                AsmType::Bytearray { size, alignment: _ } => {
                    copy_bytes_from_reg(&reg, param, *size, &mut instructions);
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

        /* The list of double parameter passing registers. */
        let double_regs = [
            AsmRegister::Xmm0,
            AsmRegister::Xmm1,
            AsmRegister::Xmm2,
            AsmRegister::Xmm3,
            AsmRegister::Xmm4,
            AsmRegister::Xmm5,
            AsmRegister::Xmm6,
            AsmRegister::Xmm7,
        ];

        /* Process floating-point register parameters.
         * Each parameter is moved from the floating-point register (XMM) to its destination. */
        for (idx, (param_type, param)) in double_reg_params.iter().enumerate() {
            let reg = double_regs[idx];
            let instr = AsmInstruction::Mov {
                asm_type: *param_type,
                src: AsmOperand::Register(reg),
                dst: param.clone(),
            };
            instructions.push(instr);
        }

        /* Start handling stack parameters (parameters passed on the stack instead of registers).
         * Stack parameters start 16 bytes above the base pointer (%bp) due to the calling convention.
         * The first 16 bytes are used for the return address and the previous frame pointer.
         *
         * See Figure 18-4 in the book. */
        let mut offset = 16;
        for (param_type, param) in stack_params {
            match param_type {
                AsmType::Bytearray { size, alignment: _ } => {
                    instructions.extend(copy_bytes(
                        &AsmOperand::Memory(AsmRegister::Bp, offset),
                        &param,
                        size,
                    ));
                }
                _ => {
                    let instr = AsmInstruction::Mov {
                        asm_type: param_type,
                        src: AsmOperand::Memory(AsmRegister::Bp, offset),
                        dst: param.clone(),
                    };
                    instructions.push(instr);
                }
            }
            offset += 8;
        }

        for instr in &self.body {
            if let IRInstruction::VaStart { list } = instr {
                instructions.extend(codegen_va_start(&self.name, list));
            } else {
                instructions.extend::<Vec<AsmInstruction>>(instr.codegen().into());
            }
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
                Const::Short(n) => AsmNode::Operand(AsmOperand::Imm(*n as i64)),
                Const::Int(n) => AsmNode::Operand(AsmOperand::Imm(*n as i64)),
                Const::Long(n) => AsmNode::Operand(AsmOperand::Imm(*n)),
                Const::UShort(n) => AsmNode::Operand(AsmOperand::Imm(*n as i64)),
                Const::UInt(n) => AsmNode::Operand(AsmOperand::Imm(*n as i64)),
                Const::ULong(n) => AsmNode::Operand(AsmOperand::Imm(*n as i64)),
                Const::Float(n) => {
                    let static_const = AsmStaticConstant {
                        name: format!("static_const.{}", make_temporary()),
                        init: StaticInit::Float(*n),
                        alignment: 4,
                    };
                    STATIC_CONSTANTS.lock().unwrap().push(static_const.clone());
                    AsmNode::Operand(AsmOperand::Data(static_const.name, 0))
                }
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
                let type_of_symbol = symbol.ty.clone();

                match type_of_symbol {
                    // A function designator used as a value is represented by
                    // its label address, not by stack/object storage. This lets
                    // `leaq foo(%rip), %reg` materialize a function pointer.
                    Type::Func { .. } => AsmNode::Operand(AsmOperand::Data(name.to_owned(), 0)),
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
                let asm_type = ir2asmtype(src);

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
                        if is_sse_asm_type(&asm_type) {
                            let static_const = AsmStaticConstant {
                                name: format!("static_const.{}", make_temporary()),
                                init: match asm_type {
                                    AsmType::Float => StaticInit::Float(-0.0),
                                    AsmType::Double => StaticInit::Double(-0.0),
                                    _ => unreachable!(),
                                },
                                alignment: 16,
                            };

                            STATIC_CONSTANTS.lock().unwrap().push(static_const.clone());

                            AsmNode::Instructions(vec![
                                AsmInstruction::Mov {
                                    asm_type,
                                    src: src.codegen().into(),
                                    dst: dst.codegen().into(),
                                },
                                AsmInstruction::Binary {
                                    asm_type,
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
                        if is_sse_asm_type(&asm_type) {
                            let dst_operand: AsmOperand = dst.codegen().into();
                            AsmNode::Instructions(vec![
                                AsmInstruction::Binary {
                                    asm_type,
                                    op: AsmBinaryOp::Xor,
                                    lhs: AsmOperand::Register(AsmRegister::Xmm0),
                                    rhs: AsmOperand::Register(AsmRegister::Xmm0),
                                },
                                AsmInstruction::Cmp {
                                    asm_type,
                                    lhs: src.codegen().into(),
                                    rhs: AsmOperand::Register(AsmRegister::Xmm0),
                                },
                                AsmInstruction::Mov {
                                    asm_type: ir2asmtype(dst),
                                    src: AsmOperand::Imm(0),
                                    dst: dst_operand.clone(),
                                },
                                AsmInstruction::SetCC {
                                    condition: ConditionCode::E,
                                    operand: dst_operand.clone(),
                                },
                                AsmInstruction::SetCC {
                                    condition: ConditionCode::NP,
                                    operand: AsmOperand::Register(AsmRegister::R11),
                                },
                                AsmInstruction::Binary {
                                    asm_type: AsmType::Byte,
                                    op: AsmBinaryOp::And,
                                    lhs: AsmOperand::Register(AsmRegister::R11),
                                    rhs: dst_operand,
                                },
                            ])
                        } else {
                            AsmNode::Instructions(vec![
                                AsmInstruction::Cmp {
                                    asm_type: ir2asmtype(src),
                                    lhs: AsmOperand::Imm(0),
                                    rhs: src.codegen().into(),
                                },
                                AsmInstruction::Mov {
                                    asm_type: ir2asmtype(dst),
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
                    UnaryOp::Inc | UnaryOp::Dec => AsmNode::Instructions(vec![
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
                        src: AsmOperand::Memory(AsmRegister::Bp, -8),
                        dst: AsmOperand::Register(AsmRegister::Ax),
                    });

                    let return_storage = AsmOperand::Memory(AsmRegister::Ax, 0);
                    let ret_operand = value.clone().unwrap().codegen().into();
                    let t = ir2type(&value.clone().unwrap());

                    instructions.extend(copy_bytes(
                        &ret_operand,
                        &return_storage,
                        get_size_of_type(&t),
                    ));
                } else {
                    let int_return_registers = [AsmRegister::Ax, AsmRegister::Dx];
                    let double_return_registers = [AsmRegister::Xmm0, AsmRegister::Xmm1];

                    for (reg_index, (t, op)) in int_retvals.iter().enumerate() {
                        let r = int_return_registers[reg_index];
                        match t {
                            AsmType::Bytearray { size, alignment: _ } => {
                                copy_bytes_to_reg(op.to_owned(), r, *size, &mut instructions);
                            }
                            _ => {
                                instructions.push(AsmInstruction::Mov {
                                    asm_type: *t,
                                    src: op.clone(),
                                    dst: AsmOperand::Register(r),
                                });
                            }
                        }
                    }

                    for (reg_index, (return_type, op)) in double_retvals.iter().enumerate() {
                        let r = double_return_registers[reg_index];
                        instructions.push(AsmInstruction::Mov {
                            asm_type: *return_type,
                            src: op.clone(),
                            dst: AsmOperand::Register(r),
                        });
                    }
                }

                instructions.push(AsmInstruction::Ret);

                AsmNode::Instructions(instructions)
            }

            IRInstruction::Binary { op, lhs, rhs, dst } => {
                let asm_type = ir2asmtype(lhs);

                match op {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul => AsmNode::Instructions(vec![
                        AsmInstruction::Mov {
                            asm_type: ir2asmtype(dst),
                            src: lhs.codegen().into(),
                            dst: dst.codegen().into(),
                        },
                        AsmInstruction::Binary {
                            asm_type: ir2asmtype(dst),
                            op: (*op).into(),
                            lhs: rhs.codegen().into(),
                            rhs: dst.codegen().into(),
                        },
                    ]),

                    BinaryOp::Div => {
                        if is_sse_asm_type(&asm_type) {
                            return AsmNode::Instructions(vec![
                                AsmInstruction::Mov {
                                    asm_type,
                                    src: lhs.codegen().into(),
                                    dst: dst.codegen().into(),
                                },
                                AsmInstruction::Binary {
                                    asm_type,
                                    op: AsmBinaryOp::DivDouble,
                                    lhs: rhs.codegen().into(),
                                    rhs: dst.codegen().into(),
                                },
                            ]);
                        }

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
                                        Some(symbol) => symbol.ty,
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
                                        Some(symbol) => symbol.ty,
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
                                        Some(symbol) => symbol.ty,
                                        None => unreachable!(),
                                    };

                                let var2_type =
                                    match SYMBOL_TABLE.lock().unwrap().get(var2).cloned() {
                                        Some(symbol) => symbol.ty,
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
                            dst: AsmOperand::Register(AsmRegister::Ax),
                        }];

                        let t = ir2type(lhs);
                        let signedness = get_signedness(&t);

                        if signedness || is_pointer_type(&t) {
                            v.extend(vec![
                                AsmInstruction::Cdq { asm_type },
                                AsmInstruction::Idiv {
                                    asm_type,
                                    operand: rhs.codegen().into(),
                                },
                                AsmInstruction::Mov {
                                    asm_type,
                                    src: AsmOperand::Register(AsmRegister::Ax),
                                    dst: dst.codegen().into(),
                                },
                            ]);
                        } else {
                            v.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type,
                                    src: AsmOperand::Imm(0),
                                    dst: AsmOperand::Register(AsmRegister::Dx),
                                },
                                AsmInstruction::Div {
                                    asm_type,
                                    operand: rhs.codegen().into(),
                                },
                                AsmInstruction::Mov {
                                    asm_type,
                                    src: AsmOperand::Register(AsmRegister::Ax),
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
                            dst: AsmOperand::Register(AsmRegister::Ax),
                        }];

                        let t = ir2type(lhs);
                        let signedness = get_signedness(&t);

                        if signedness {
                            v.extend(vec![
                                AsmInstruction::Cdq { asm_type },
                                AsmInstruction::Idiv {
                                    asm_type,
                                    operand: rhs.codegen().into(),
                                },
                                AsmInstruction::Mov {
                                    asm_type,
                                    src: AsmOperand::Register(AsmRegister::Dx),
                                    dst: dst.codegen().into(),
                                },
                            ])
                        } else {
                            v.extend(vec![
                                AsmInstruction::Mov {
                                    asm_type: AsmType::Quadword,
                                    src: lhs.codegen().into(),
                                    dst: AsmOperand::Register(AsmRegister::Ax),
                                },
                                AsmInstruction::Mov {
                                    asm_type: AsmType::Quadword,
                                    src: AsmOperand::Imm(0),
                                    dst: AsmOperand::Register(AsmRegister::Dx),
                                },
                                AsmInstruction::Idiv {
                                    asm_type: AsmType::Quadword,
                                    operand: rhs.codegen().into(),
                                },
                                AsmInstruction::Mov {
                                    asm_type,
                                    src: AsmOperand::Register(AsmRegister::Dx),
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
                        let lhs_type = ir2type(lhs);
                        let rhs_type = ir2type(rhs);

                        if is_floating_type(&lhs_type) && is_floating_type(&rhs_type) {
                            let dst_operand: AsmOperand = dst.codegen().into();
                            let mut instrs = vec![
                                AsmInstruction::Cmp {
                                    asm_type,
                                    lhs: rhs.codegen().into(),
                                    rhs: lhs.codegen().into(),
                                },
                                AsmInstruction::Mov {
                                    asm_type: AsmType::Longword,
                                    src: AsmOperand::Imm(0),
                                    dst: dst_operand.clone(),
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
                                    operand: dst_operand.clone(),
                                },
                            ];

                            if *op == BinaryOp::NotEqual {
                                instrs.extend(vec![
                                    AsmInstruction::SetCC {
                                        condition: ConditionCode::P,
                                        operand: AsmOperand::Register(AsmRegister::R11),
                                    },
                                    AsmInstruction::Binary {
                                        asm_type: AsmType::Byte,
                                        op: AsmBinaryOp::Or,
                                        lhs: AsmOperand::Register(AsmRegister::R11),
                                        rhs: dst_operand,
                                    },
                                ]);
                            } else {
                                instrs.extend(vec![
                                    AsmInstruction::SetCC {
                                        condition: ConditionCode::NP,
                                        operand: AsmOperand::Register(AsmRegister::R11),
                                    },
                                    AsmInstruction::Binary {
                                        asm_type: AsmType::Byte,
                                        op: AsmBinaryOp::And,
                                        lhs: AsmOperand::Register(AsmRegister::R11),
                                        rhs: dst_operand,
                                    },
                                ]);
                            }

                            AsmNode::Instructions(instrs)
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
                                        asm_type: ir2asmtype(dst),
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
                                        asm_type: ir2asmtype(dst),
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
                    BinaryOp::BitwiseShl | BinaryOp::BitwiseShr => {
                        fn convert_shift_op(op: &BinaryOp, signed: bool) -> AsmBinaryOp {
                            match op {
                                BinaryOp::BitwiseShl => {
                                    if signed {
                                        AsmBinaryOp::Sal
                                    } else {
                                        AsmBinaryOp::Shl
                                    }
                                }
                                BinaryOp::BitwiseShr => {
                                    if signed {
                                        AsmBinaryOp::Sar
                                    } else {
                                        AsmBinaryOp::ShrTwoOp
                                    }
                                }
                                _ => unreachable!(),
                            }
                        }

                        let is_signed = get_signedness(&ir2type(lhs));

                        let asm_lhs = lhs.codegen().into();
                        let asm_rhs = rhs.codegen().into();

                        let asm_dst: AsmOperand = dst.codegen().into();

                        let asm_op = convert_shift_op(op, is_signed);

                        match asm_rhs {
                            AsmOperand::Imm(_) => AsmNode::Instructions(vec![
                                AsmInstruction::Mov {
                                    asm_type: ir2asmtype(lhs),
                                    src: asm_lhs,
                                    dst: asm_dst.clone(),
                                },
                                AsmInstruction::Binary {
                                    asm_type: ir2asmtype(lhs),
                                    op: asm_op,
                                    lhs: asm_rhs,
                                    rhs: asm_dst.clone(),
                                },
                            ]),
                            _ => AsmNode::Instructions(vec![
                                AsmInstruction::Mov {
                                    asm_type: ir2asmtype(lhs),
                                    src: asm_lhs,
                                    dst: asm_dst.clone(),
                                },
                                AsmInstruction::Mov {
                                    asm_type: AsmType::Byte,
                                    src: asm_rhs,
                                    dst: AsmOperand::Register(AsmRegister::Cx),
                                },
                                AsmInstruction::Binary {
                                    asm_type: ir2asmtype(lhs),
                                    op: asm_op,
                                    lhs: AsmOperand::Register(AsmRegister::Cx),
                                    rhs: asm_dst.clone(),
                                },
                            ]),
                        }
                    }
                    _ => {
                        let asm_lhs = lhs.codegen().into();
                        let asm_rhs = rhs.codegen().into();
                        let asm_dst: AsmOperand = dst.codegen().into();

                        let op = match op {
                            BinaryOp::BitwiseAnd => AsmBinaryOp::And,
                            BinaryOp::BitwiseOr => AsmBinaryOp::Or,
                            BinaryOp::BitwiseXor => AsmBinaryOp::Xor,
                            _ => unreachable!(),
                        };

                        AsmNode::Instructions(vec![
                            AsmInstruction::Mov {
                                asm_type: ir2asmtype(lhs),
                                src: asm_lhs,
                                dst: asm_dst.clone(),
                            },
                            AsmInstruction::Binary {
                                asm_type: ir2asmtype(lhs),
                                op,
                                lhs: asm_rhs,
                                rhs: asm_dst.clone(),
                            },
                        ])
                    }
                }
            }

            IRInstruction::JumpIfZero { condition, target } => {
                let asm_type = ir2asmtype(condition);

                if is_sse_asm_type(&asm_type) {
                    let unordered_label = make_temporary().to_string();
                    AsmNode::Instructions(vec![
                        AsmInstruction::Binary {
                            asm_type,
                            op: AsmBinaryOp::Xor,
                            lhs: AsmOperand::Register(AsmRegister::Xmm0),
                            rhs: AsmOperand::Register(AsmRegister::Xmm0),
                        },
                        AsmInstruction::Cmp {
                            asm_type,
                            lhs: condition.codegen().into(),
                            rhs: AsmOperand::Register(AsmRegister::Xmm0),
                        },
                        AsmInstruction::JmpCC {
                            condition: ConditionCode::P,
                            target: unordered_label.clone(),
                        },
                        AsmInstruction::JmpCC {
                            condition: ConditionCode::E,
                            target: target.clone(),
                        },
                        AsmInstruction::Label(unordered_label),
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
                let asm_type = ir2asmtype(condition);

                if is_sse_asm_type(&asm_type) {
                    AsmNode::Instructions(vec![
                        AsmInstruction::Binary {
                            asm_type,
                            op: AsmBinaryOp::Xor,
                            lhs: AsmOperand::Register(AsmRegister::Xmm0),
                            rhs: AsmOperand::Register(AsmRegister::Xmm0),
                        },
                        AsmInstruction::Cmp {
                            asm_type,
                            lhs: condition.codegen().into(),
                            rhs: AsmOperand::Register(AsmRegister::Xmm0),
                        },
                        AsmInstruction::JmpCC {
                            condition: ConditionCode::P,
                            target: target.clone(),
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
                let t = ir2type(src);
                let asm_type = ir2asmtype(src);

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
                    AsmRegister::Di,
                    AsmRegister::Si,
                    AsmRegister::Dx,
                    AsmRegister::Cx,
                    AsmRegister::R8,
                    AsmRegister::R9,
                ];

                let double_registers = [
                    AsmRegister::Xmm0,
                    AsmRegister::Xmm1,
                    AsmRegister::Xmm2,
                    AsmRegister::Xmm3,
                    AsmRegister::Xmm4,
                    AsmRegister::Xmm5,
                    AsmRegister::Xmm6,
                    AsmRegister::Xmm7,
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
                        dst: AsmOperand::Register(AsmRegister::Di),
                    });

                    reg_index = 1;
                }

                let (int_args, double_args, stack_args) =
                    classify_parameters(args, return_in_memory);
                let variadic_sse_arg_count = double_args.len();

                let stack_padding = if stack_args.len() % 2 != 0 { 8 } else { 0 };

                if stack_padding != 0 {
                    instructions.push(AsmInstruction::Binary {
                        asm_type: AsmType::Quadword,
                        op: AsmBinaryOp::Sub,
                        lhs: AsmOperand::Imm(stack_padding),
                        rhs: AsmOperand::Register(AsmRegister::Sp),
                    });
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

                for (reg_index, (arg_type, reg_arg)) in double_args.into_iter().enumerate() {
                    let reg = double_registers[reg_index];

                    instructions.push(AsmInstruction::Mov {
                        asm_type: arg_type,
                        src: reg_arg,
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
                                rhs: AsmOperand::Register(AsmRegister::Sp),
                            });
                            instructions.extend(copy_bytes(
                                stack_arg,
                                &AsmOperand::Memory(AsmRegister::Sp, 0),
                                *size,
                            ));
                        }
                        _ if *arg_type == AsmType::Float => {
                            instructions.push(AsmInstruction::Binary {
                                asm_type: AsmType::Quadword,
                                op: AsmBinaryOp::Sub,
                                lhs: AsmOperand::Imm(8),
                                rhs: AsmOperand::Register(AsmRegister::Sp),
                            });
                            instructions.push(AsmInstruction::Mov {
                                asm_type: AsmType::Float,
                                src: stack_arg.clone(),
                                dst: AsmOperand::Memory(AsmRegister::Sp, 0),
                            });
                        }
                        _ => match stack_arg {
                            AsmOperand::Imm(_) | AsmOperand::Register(_) => {
                                instructions.push(AsmInstruction::Push(stack_arg.clone()));
                            }
                            _ => {
                                if arg_type == &AsmType::Quadword || arg_type == &AsmType::Double {
                                    instructions.push(AsmInstruction::Push(stack_arg.clone()));
                                } else {
                                    instructions.push(AsmInstruction::Mov {
                                        asm_type: *arg_type,
                                        src: stack_arg.clone(),
                                        dst: AsmOperand::Register(AsmRegister::Ax),
                                    });
                                    instructions.push(AsmInstruction::Push(AsmOperand::Register(
                                        AsmRegister::Ax,
                                    )));
                                }
                            }
                        },
                    }
                }

                if is_variadic_function(target) {
                    instructions.push(AsmInstruction::Mov {
                        asm_type: AsmType::Byte,
                        src: AsmOperand::Imm(variadic_sse_arg_count as i64),
                        dst: AsmOperand::Register(AsmRegister::Ax),
                    });
                }

                instructions.push(AsmInstruction::Call(target.to_owned()));

                let bytes_to_remove = 8 * stack_args.len() + stack_padding as usize;
                if bytes_to_remove != 0 {
                    instructions.push(AsmInstruction::Binary {
                        asm_type: AsmType::Quadword,
                        op: AsmBinaryOp::Add,
                        lhs: AsmOperand::Imm(bytes_to_remove as i64),
                        rhs: AsmOperand::Register(AsmRegister::Sp),
                    });
                }

                if dst.is_some() && !return_in_memory {
                    let int_return_registers = [AsmRegister::Ax, AsmRegister::Dx];
                    let double_return_registers = [AsmRegister::Xmm0, AsmRegister::Xmm1];

                    for (reg_index, (t, op)) in int_dests.iter().enumerate() {
                        let r = int_return_registers[reg_index];
                        match t {
                            AsmType::Bytearray { size, alignment: _ } => {
                                copy_bytes_from_reg(&r, op, *size, &mut instructions);
                            }
                            _ => {
                                instructions.push(AsmInstruction::Mov {
                                    asm_type: *t,
                                    src: AsmOperand::Register(r),
                                    dst: op.clone(),
                                });
                            }
                        }
                    }

                    for (reg_index, (return_type, op)) in double_dests.iter().enumerate() {
                        let r = double_return_registers[reg_index];
                        instructions.push(AsmInstruction::Mov {
                            asm_type: *return_type,
                            src: AsmOperand::Register(r),
                            dst: op.clone(),
                        });
                    }
                }

                AsmNode::Instructions(instructions)
            }

            IRInstruction::VaStart { .. } => {
                unreachable!("VaStart needs the enclosing function during codegen")
            }

            IRInstruction::VaArg { list, arg_ty, dst } => {
                AsmNode::Instructions(codegen_va_arg(list, arg_ty, dst))
            }

            IRInstruction::VaCopy { dst, src } => AsmNode::Instructions(codegen_va_copy(dst, src)),

            IRInstruction::VaEnd { .. } => AsmNode::Instructions(vec![]),

            IRInstruction::SignExtend { src, dst } => {
                AsmNode::Instructions(vec![AsmInstruction::Movsx {
                    src_type: ir2asmtype(src),
                    src: src.codegen().into(),
                    dst_type: ir2asmtype(dst),
                    dst: dst.codegen().into(),
                }])
            }

            IRInstruction::Truncate { src, dst } => {
                AsmNode::Instructions(vec![AsmInstruction::Mov {
                    asm_type: ir2asmtype(dst),
                    src: src.codegen().into(),
                    dst: dst.codegen().into(),
                }])
            }

            IRInstruction::ZeroExtend { src, dst } => {
                AsmNode::Instructions(vec![AsmInstruction::MovZeroExtend {
                    src_type: ir2asmtype(src),
                    src: src.codegen().into(),
                    dst_type: ir2asmtype(dst),
                    dst: dst.codegen().into(),
                }])
            }

            IRInstruction::UIntToDouble { src, dst } => {
                let src_type = ir2asmtype(src);
                match src_type {
                    AsmType::Byte | AsmType::Word => AsmNode::Instructions(vec![
                        AsmInstruction::MovZeroExtend {
                            src_type,
                            src: src.codegen().into(),
                            dst_type: AsmType::Longword,
                            dst: AsmOperand::Register(AsmRegister::Ax),
                        },
                        AsmInstruction::Cvtsi2sd {
                            asm_type: AsmType::Longword,
                            src: AsmOperand::Register(AsmRegister::Ax),
                            dst: dst.codegen().into(),
                        },
                    ]),
                    AsmType::Longword => AsmNode::Instructions(vec![
                        AsmInstruction::MovZeroExtend {
                            src_type: AsmType::Longword,
                            src: src.codegen().into(),
                            dst_type: AsmType::Longword,
                            dst: AsmOperand::Register(AsmRegister::Ax),
                        },
                        AsmInstruction::Cvtsi2sd {
                            asm_type: AsmType::Quadword,
                            src: AsmOperand::Register(AsmRegister::Ax),
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
                                dst: AsmOperand::Register(AsmRegister::Ax),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: AsmOperand::Register(AsmRegister::Ax),
                                dst: AsmOperand::Register(AsmRegister::Dx),
                            },
                            AsmInstruction::Unary {
                                asm_type: AsmType::Quadword,
                                op: AsmUnaryOp::Shr,
                                operand: AsmOperand::Register(AsmRegister::Dx),
                            },
                            AsmInstruction::Binary {
                                op: AsmBinaryOp::And,
                                asm_type: AsmType::Quadword,
                                lhs: AsmOperand::Imm(1),
                                rhs: AsmOperand::Register(AsmRegister::Ax),
                            },
                            AsmInstruction::Binary {
                                op: AsmBinaryOp::Or,
                                asm_type: AsmType::Quadword,
                                lhs: AsmOperand::Register(AsmRegister::Ax),
                                rhs: AsmOperand::Register(AsmRegister::Dx),
                            },
                            AsmInstruction::Cvtsi2sd {
                                asm_type: AsmType::Quadword,
                                src: AsmOperand::Register(AsmRegister::Dx),
                                dst: dst.codegen().into(),
                            },
                            AsmInstruction::Binary {
                                op: AsmBinaryOp::Add,
                                asm_type: AsmType::Double,
                                lhs: dst.codegen().into(),
                                rhs: dst.codegen().into(),
                            },
                            AsmInstruction::Label(label2.clone()),
                        ])
                    }
                    _ => unreachable!(),
                }
            }

            IRInstruction::DoubleToInt { src, dst } => {
                let dst_type = ir2asmtype(dst);
                match dst_type {
                    AsmType::Byte | AsmType::Word => AsmNode::Instructions(vec![
                        AsmInstruction::Cvttsd2si {
                            asm_type: AsmType::Longword,
                            src: src.codegen().into(),
                            dst: AsmOperand::Register(AsmRegister::Ax),
                        },
                        AsmInstruction::Mov {
                            asm_type: dst_type,
                            src: AsmOperand::Register(AsmRegister::Ax),
                            dst: dst.codegen().into(),
                        },
                    ]),
                    AsmType::Longword | AsmType::Quadword => {
                        AsmNode::Instructions(vec![AsmInstruction::Cvttsd2si {
                            asm_type: ir2asmtype(dst),
                            src: src.codegen().into(),
                            dst: dst.codegen().into(),
                        }])
                    }
                    _ => unreachable!(),
                }
            }

            IRInstruction::DoubletoUInt { src, dst } => {
                let dst_type = ir2asmtype(dst);
                match dst_type {
                    AsmType::Byte | AsmType::Word => AsmNode::Instructions(vec![
                        AsmInstruction::Cvttsd2si {
                            asm_type: AsmType::Longword,
                            src: src.codegen().into(),
                            dst: AsmOperand::Register(AsmRegister::Ax),
                        },
                        AsmInstruction::Mov {
                            asm_type: dst_type,
                            src: AsmOperand::Register(AsmRegister::Ax),
                            dst: dst.codegen().into(),
                        },
                    ]),
                    AsmType::Longword => AsmNode::Instructions(vec![
                        AsmInstruction::Cvttsd2si {
                            asm_type: AsmType::Quadword,
                            src: src.codegen().into(),
                            dst: AsmOperand::Register(AsmRegister::Ax),
                        },
                        AsmInstruction::Mov {
                            asm_type: AsmType::Longword,
                            src: AsmOperand::Register(AsmRegister::Ax),
                            dst: dst.codegen().into(),
                        },
                    ]),
                    AsmType::Quadword => {
                        let i64_ceil_as_u64 = (i64::MAX as u64) + 1;
                        let i64_ceil_as_f64 = i64_ceil_as_u64 as f64;

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
                                dst: AsmOperand::Register(AsmRegister::Xmm0),
                            },
                            AsmInstruction::Binary {
                                op: AsmBinaryOp::Sub,
                                asm_type: AsmType::Double,
                                lhs: AsmOperand::Data(static_constant.name, 0),
                                rhs: AsmOperand::Register(AsmRegister::Xmm0),
                            },
                            AsmInstruction::Cvttsd2si {
                                asm_type: AsmType::Quadword,
                                src: AsmOperand::Register(AsmRegister::Xmm0),
                                dst: dst.codegen().into(),
                            },
                            AsmInstruction::Binary {
                                op: AsmBinaryOp::Add,
                                asm_type: AsmType::Quadword,
                                lhs: AsmOperand::Imm(i64_ceil_as_u64 as i64),
                                rhs: dst.codegen().into(),
                            },
                            AsmInstruction::Label(label_end.clone()),
                        ])
                    }
                    _ => todo!(),
                }
            }

            IRInstruction::IntToDouble { src, dst } => {
                let src_type = ir2asmtype(src);
                match src_type {
                    AsmType::Byte | AsmType::Word => AsmNode::Instructions(vec![
                        AsmInstruction::Movsx {
                            src_type,
                            src: src.codegen().into(),
                            dst_type: AsmType::Longword,
                            dst: AsmOperand::Register(AsmRegister::Ax),
                        },
                        AsmInstruction::Cvtsi2sd {
                            asm_type: AsmType::Longword,
                            src: AsmOperand::Register(AsmRegister::Ax),
                            dst: dst.codegen().into(),
                        },
                    ]),
                    _ => AsmNode::Instructions(vec![AsmInstruction::Cvtsi2sd {
                        asm_type: ir2asmtype(src),
                        src: src.codegen().into(),
                        dst: dst.codegen().into(),
                    }]),
                }
            }

            IRInstruction::DoubleToFloat { src, dst } => {
                AsmNode::Instructions(vec![AsmInstruction::Cvtsd2ss {
                    src: src.codegen().into(),
                    dst: dst.codegen().into(),
                }])
            }

            IRInstruction::FloatToDouble { src, dst } => {
                AsmNode::Instructions(vec![AsmInstruction::Cvtss2sd {
                    src: src.codegen().into(),
                    dst: dst.codegen().into(),
                }])
            }

            IRInstruction::FloatToInt { src, dst } => {
                let dst_type = ir2asmtype(dst);
                match dst_type {
                    AsmType::Byte | AsmType::Word => AsmNode::Instructions(vec![
                        AsmInstruction::Cvttss2si {
                            asm_type: AsmType::Longword,
                            src: src.codegen().into(),
                            dst: AsmOperand::Register(AsmRegister::Ax),
                        },
                        AsmInstruction::Mov {
                            asm_type: dst_type,
                            src: AsmOperand::Register(AsmRegister::Ax),
                            dst: dst.codegen().into(),
                        },
                    ]),
                    AsmType::Longword | AsmType::Quadword => {
                        AsmNode::Instructions(vec![AsmInstruction::Cvttss2si {
                            asm_type: ir2asmtype(dst),
                            src: src.codegen().into(),
                            dst: dst.codegen().into(),
                        }])
                    }
                    _ => unreachable!(),
                }
            }

            IRInstruction::FloattoUInt { src, dst } => {
                let dst_type = ir2asmtype(dst);
                match dst_type {
                    AsmType::Byte | AsmType::Word => AsmNode::Instructions(vec![
                        AsmInstruction::Cvttss2si {
                            asm_type: AsmType::Longword,
                            src: src.codegen().into(),
                            dst: AsmOperand::Register(AsmRegister::Ax),
                        },
                        AsmInstruction::Mov {
                            asm_type: dst_type,
                            src: AsmOperand::Register(AsmRegister::Ax),
                            dst: dst.codegen().into(),
                        },
                    ]),
                    AsmType::Longword => AsmNode::Instructions(vec![
                        AsmInstruction::Cvttss2si {
                            asm_type: AsmType::Quadword,
                            src: src.codegen().into(),
                            dst: AsmOperand::Register(AsmRegister::Ax),
                        },
                        AsmInstruction::Mov {
                            asm_type: AsmType::Longword,
                            src: AsmOperand::Register(AsmRegister::Ax),
                            dst: dst.codegen().into(),
                        },
                    ]),
                    AsmType::Quadword => {
                        let i64_ceil_as_u64 = (i64::MAX as u64) + 1;
                        let i64_ceil_as_f32 = i64_ceil_as_u64 as f32;

                        let static_constant = AsmStaticConstant {
                            name: format!("static_const.{}", make_temporary()),
                            init: StaticInit::Float(i64_ceil_as_f32),
                            alignment: 4,
                        };
                        STATIC_CONSTANTS
                            .lock()
                            .unwrap()
                            .push(static_constant.clone());

                        let label_out_of_range = format!("label_out_of_range.{}", make_temporary());
                        let label_end = format!("label_end.{}", make_temporary());

                        AsmNode::Instructions(vec![
                            AsmInstruction::Cmp {
                                asm_type: AsmType::Float,
                                lhs: AsmOperand::Data(static_constant.name.clone(), 0),
                                rhs: src.codegen().into(),
                            },
                            AsmInstruction::JmpCC {
                                condition: ConditionCode::AE,
                                target: label_out_of_range.clone(),
                            },
                            AsmInstruction::Cvttss2si {
                                asm_type: AsmType::Quadword,
                                src: src.codegen().into(),
                                dst: dst.codegen().into(),
                            },
                            AsmInstruction::Jmp {
                                target: label_end.clone(),
                            },
                            AsmInstruction::Label(label_out_of_range),
                            AsmInstruction::Mov {
                                asm_type: AsmType::Float,
                                src: src.codegen().into(),
                                dst: AsmOperand::Register(AsmRegister::Xmm0),
                            },
                            AsmInstruction::Binary {
                                op: AsmBinaryOp::Sub,
                                asm_type: AsmType::Float,
                                lhs: AsmOperand::Data(static_constant.name, 0),
                                rhs: AsmOperand::Register(AsmRegister::Xmm0),
                            },
                            AsmInstruction::Cvttss2si {
                                asm_type: AsmType::Quadword,
                                src: AsmOperand::Register(AsmRegister::Xmm0),
                                dst: dst.codegen().into(),
                            },
                            AsmInstruction::Binary {
                                op: AsmBinaryOp::Add,
                                asm_type: AsmType::Quadword,
                                lhs: AsmOperand::Imm(i64_ceil_as_u64 as i64),
                                rhs: dst.codegen().into(),
                            },
                            AsmInstruction::Label(label_end.clone()),
                        ])
                    }
                    _ => todo!(),
                }
            }

            IRInstruction::IntToFloat { src, dst } => {
                let src_type = ir2asmtype(src);
                match src_type {
                    AsmType::Byte | AsmType::Word => AsmNode::Instructions(vec![
                        AsmInstruction::Movsx {
                            src_type,
                            src: src.codegen().into(),
                            dst_type: AsmType::Longword,
                            dst: AsmOperand::Register(AsmRegister::Ax),
                        },
                        AsmInstruction::Cvtsi2ss {
                            asm_type: AsmType::Longword,
                            src: AsmOperand::Register(AsmRegister::Ax),
                            dst: dst.codegen().into(),
                        },
                    ]),
                    _ => AsmNode::Instructions(vec![AsmInstruction::Cvtsi2ss {
                        asm_type: ir2asmtype(src),
                        src: src.codegen().into(),
                        dst: dst.codegen().into(),
                    }]),
                }
            }

            IRInstruction::UIntToFloat { src, dst } => {
                let src_type = ir2asmtype(src);
                match src_type {
                    AsmType::Byte | AsmType::Word => AsmNode::Instructions(vec![
                        AsmInstruction::MovZeroExtend {
                            src_type,
                            src: src.codegen().into(),
                            dst_type: AsmType::Longword,
                            dst: AsmOperand::Register(AsmRegister::Ax),
                        },
                        AsmInstruction::Cvtsi2ss {
                            asm_type: AsmType::Longword,
                            src: AsmOperand::Register(AsmRegister::Ax),
                            dst: dst.codegen().into(),
                        },
                    ]),
                    AsmType::Longword => AsmNode::Instructions(vec![
                        AsmInstruction::MovZeroExtend {
                            src_type: AsmType::Longword,
                            src: src.codegen().into(),
                            dst_type: AsmType::Longword,
                            dst: AsmOperand::Register(AsmRegister::Ax),
                        },
                        AsmInstruction::Cvtsi2ss {
                            asm_type: AsmType::Quadword,
                            src: AsmOperand::Register(AsmRegister::Ax),
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
                            AsmInstruction::Cvtsi2ss {
                                asm_type: AsmType::Quadword,
                                src: src.codegen().into(),
                                dst: dst.codegen().into(),
                            },
                            AsmInstruction::Jmp {
                                target: label2.clone(),
                            },
                            AsmInstruction::Label(label1.clone()),
                            AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: src.codegen().into(),
                                dst: AsmOperand::Register(AsmRegister::Ax),
                            },
                            AsmInstruction::Mov {
                                asm_type: AsmType::Quadword,
                                src: AsmOperand::Register(AsmRegister::Ax),
                                dst: AsmOperand::Register(AsmRegister::Dx),
                            },
                            AsmInstruction::Unary {
                                asm_type: AsmType::Quadword,
                                op: AsmUnaryOp::Shr,
                                operand: AsmOperand::Register(AsmRegister::Dx),
                            },
                            AsmInstruction::Binary {
                                op: AsmBinaryOp::And,
                                asm_type: AsmType::Quadword,
                                lhs: AsmOperand::Imm(1),
                                rhs: AsmOperand::Register(AsmRegister::Ax),
                            },
                            AsmInstruction::Binary {
                                op: AsmBinaryOp::Or,
                                asm_type: AsmType::Quadword,
                                lhs: AsmOperand::Register(AsmRegister::Ax),
                                rhs: AsmOperand::Register(AsmRegister::Dx),
                            },
                            AsmInstruction::Cvtsi2ss {
                                asm_type: AsmType::Quadword,
                                src: AsmOperand::Register(AsmRegister::Dx),
                                dst: dst.codegen().into(),
                            },
                            AsmInstruction::Binary {
                                op: AsmBinaryOp::Add,
                                asm_type: AsmType::Float,
                                lhs: dst.codegen().into(),
                                rhs: dst.codegen().into(),
                            },
                            AsmInstruction::Label(label2.clone()),
                        ])
                    }
                    _ => unreachable!(),
                }
            }

            IRInstruction::Load { src_ptr, dst } => {
                let t = ir2type(dst);
                if is_scalar(&t) {
                    AsmNode::Instructions(vec![
                        AsmInstruction::Mov {
                            asm_type: AsmType::Quadword,
                            src: src_ptr.codegen().into(),
                            dst: AsmOperand::Register(AsmRegister::R9),
                        },
                        AsmInstruction::Mov {
                            asm_type: ir2asmtype(dst),
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
                let t = ir2type(src);

                if is_scalar(&t) {
                    AsmNode::Instructions(vec![
                        AsmInstruction::Mov {
                            asm_type: AsmType::Quadword,
                            src: dst_ptr.codegen().into(),
                            dst: AsmOperand::Register(AsmRegister::R9),
                        },
                        AsmInstruction::Mov {
                            asm_type: ir2asmtype(src),
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
                        dst: AsmOperand::Register(AsmRegister::Ax),
                    },
                    AsmInstruction::Mov {
                        asm_type: AsmType::Quadword,
                        src: index.codegen().into(),
                        dst: AsmOperand::Register(AsmRegister::Dx),
                    },
                    AsmInstruction::Lea {
                        src: AsmOperand::Indexed(AsmRegister::Ax, AsmRegister::Dx, *scale as isize),
                        dst: dst.codegen().into(),
                    },
                ]),
                _ => AsmNode::Instructions(vec![
                    AsmInstruction::Mov {
                        asm_type: AsmType::Quadword,
                        src: ptr.codegen().into(),
                        dst: AsmOperand::Register(AsmRegister::Ax),
                    },
                    AsmInstruction::Mov {
                        asm_type: AsmType::Quadword,
                        src: index.codegen().into(),
                        dst: AsmOperand::Register(AsmRegister::Dx),
                    },
                    AsmInstruction::Binary {
                        asm_type: AsmType::Quadword,
                        op: AsmBinaryOp::Mul,
                        lhs: AsmOperand::Imm(*scale as i64),
                        rhs: AsmOperand::Register(AsmRegister::Dx),
                    },
                    AsmInstruction::Lea {
                        src: AsmOperand::Indexed(AsmRegister::Ax, AsmRegister::Dx, 1),
                        dst: dst.codegen().into(),
                    },
                ]),
            },

            IRInstruction::CopyToOffset { src, dst, offset } => {
                let type_of_src = ir2asmtype(src);

                let t = ir2type(src);

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
                let type_of_dst = ir2asmtype(dst);

                let t = ir2type(dst);

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

impl From<AsmNode> for AsmStaticVariable {
    fn from(value: AsmNode) -> Self {
        match value {
            AsmNode::StaticVariable(static_var) => static_var,
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

fn classify_parameters(
    params: &[IRValue],
    return_on_stack: bool,
) -> (
    Vec<(AsmType, AsmOperand)>,
    Vec<(AsmType, AsmOperand)>,
    Vec<(AsmType, AsmOperand)>,
) {
    let typed_params: Vec<(Type, AsmOperand)> = params
        .iter()
        .map(|v| (ir2type(v), v.codegen().into()))
        .collect();

    classify_params_helper(&typed_params, return_on_stack)
}

fn classify_params_helper(
    typed_asm_vals: &[(Type, AsmOperand)],
    return_on_stack: bool,
) -> (
    Vec<(AsmType, AsmOperand)>,
    Vec<(AsmType, AsmOperand)>,
    Vec<(AsmType, AsmOperand)>,
) {
    /* Set the number of available integer registers depending on whether return is on stack,
     * because if it is, we will need one register to hold the address of the location where
     * the return value is stored. */
    let int_regs_available = if return_on_stack { 5 } else { 6 };

    let mut int_reg_args = vec![];
    let mut double_reg_args = vec![];
    let mut stack_args = vec![];

    for (t, operand) in typed_asm_vals {
        let typed_operand = (type2asmtype(t), operand.clone());

        match t {
            Type::Struct { tag } | Type::Union { tag } => {
                let struct_entry = TYPE_TABLE.lock().unwrap().get(tag).unwrap().clone();

                /* Classify the structure into register classes */
                let classes = classify_structure(&struct_entry);
                let mut use_stack = true;

                let struct_size = struct_entry.size;

                /* Extract the variable name from the operand */
                let name_of_v = match operand {
                    AsmOperand::PseudoMem(name, _) => name.clone(),
                    _ => unreachable!(),
                };

                /* If the first classification class is not 'Memory' */
                if classes[0] != Class::Memory {
                    /* Tentatively store register operands for integers and doubles */
                    let mut tentative_ints = vec![];
                    let mut tentative_doubles = vec![];

                    let mut offset = 0; /* Offset for each 8-byte section of the structure */

                    /* Iterate over the classification of structure sections */
                    for class in &classes {
                        let operand = AsmOperand::PseudoMem(name_of_v.clone(), offset); /* Create an operand for this section */

                        if class == &Class::Sse {
                            /* If the section is SSE (floating-point), store it as an SSE register argument */
                            let eightbyte_type = get_sse_eightbyte_type(offset, struct_size);
                            tentative_doubles.push((eightbyte_type, operand));
                        } else {
                            /* Otherwise, store it as an integer register argument */
                            let eightbyte_type = get_eightbyte_type(offset, struct_size);
                            tentative_ints.push((eightbyte_type, operand));
                        }

                        /* Move to the next section */
                        offset += 8;
                    }

                    /* If there are enough free registers, use them for this structure */
                    if (tentative_doubles.len() + double_reg_args.len() <= 8)
                        && (tentative_ints.len() + int_reg_args.len() <= int_regs_available)
                    {
                        double_reg_args.extend(tentative_doubles); /* Add double register arguments */
                        int_reg_args.extend(tentative_ints); /* Add integer register arguments */
                        use_stack = false; /* No need to use the stack */
                    }
                }

                /* If not enough registers, use the stack for this structure */
                if use_stack {
                    let mut offset = 0;
                    for class in classes {
                        let operand = AsmOperand::PseudoMem(name_of_v.clone(), offset);
                        let eightbyte_type = match class {
                            Class::Sse => get_sse_eightbyte_type(offset, struct_size),
                            Class::Integer => get_eightbyte_type(offset, struct_size),
                            Class::Memory => get_eightbyte_type(offset, struct_size),
                        };
                        stack_args.push((eightbyte_type, operand));
                        offset += 8;
                    }
                }
            }

            /* If the type is floating-point */
            Type::Float | Type::Double => {
                if double_reg_args.len() < 8 {
                    /* If there are free SSE registers, use them */
                    double_reg_args.push(typed_operand);
                } else {
                    /* Otherwise, use the stack */
                    stack_args.push(typed_operand);
                }
            }

            /* For all other types (e.g., integers) */
            _ => {
                if int_reg_args.len() < int_regs_available {
                    /* If there are free integer registers, use them */
                    int_reg_args.push(typed_operand);
                } else {
                    /* Otherwise, use the stack */
                    stack_args.push(typed_operand);
                }
            }
        }
    }

    /* Return the lists of integer register arguments, double register arguments, and stack arguments */
    (int_reg_args, double_reg_args, stack_args)
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
        AsmRegister::Di,
        AsmRegister::Si,
        AsmRegister::Dx,
        AsmRegister::Cx,
        AsmRegister::R8,
        AsmRegister::R9,
    ];

    let double_regs = [
        AsmRegister::Xmm0,
        AsmRegister::Xmm1,
        AsmRegister::Xmm2,
        AsmRegister::Xmm3,
        AsmRegister::Xmm4,
        AsmRegister::Xmm5,
        AsmRegister::Xmm6,
        AsmRegister::Xmm7,
    ];

    let (ints, dbls, _) =
        classify_params_helper(&params.iter().map(f).collect::<Vec<_>>(), return_on_stack);

    let int_regs_final: Vec<_> = int_regs.iter().take(ints.len()).collect();
    let double_regs_final: Vec<_> = double_regs.iter().take(dbls.len()).collect();

    int_regs_final
        .iter()
        .chain(double_regs_final.iter())
        .map(|&&x| x)
        .collect()
}

fn is_variadic_function(name: &str) -> bool {
    match SYMBOL_TABLE.lock().unwrap().get(name).cloned().unwrap().ty {
        Type::Func { variadic, .. } => variadic,
        _ => false,
    }
}

fn returns_on_stack(name: &str) -> bool {
    match SYMBOL_TABLE.lock().unwrap().get(name).cloned().unwrap().ty {
        Type::Func { ret, .. } => match &*ret {
            Type::Struct { tag } | Type::Union { tag } => {
                let struct_entry = TYPE_TABLE.lock().unwrap().get(tag).cloned().unwrap();

                matches!(
                    classify_structure(&struct_entry).as_slice(),
                    [Class::Memory, ..]
                )
            }
            _ => false,
        },
        _ => unreachable!(),
    }
}

pub fn build_asm_symbol_table() {
    use crate::semantics::typechecker::is_complete;

    let frontend_symtab = SYMBOL_TABLE.lock().unwrap().clone();
    let mut asm_symbol_table = ASM_SYMBOL_TABLE.lock().unwrap();

    for (identifier, symbol) in frontend_symtab.iter() {
        let entry = match symbol.attrs {
            IdentifierAttrs::FuncAttr { defined, .. } => match &symbol.ty {
                Type::Func { params, ret, .. } => {
                    if is_complete(ret) || ret == &Type::Void.into() {
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
                if is_complete(&symbol.ty) {
                    let asm_type = type2asmtype(&symbol.ty);
                    AsmSymtabEntry::Object {
                        ty: asm_type,
                        is_static: true,
                        is_constant: false,
                    }
                } else {
                    AsmSymtabEntry::Object {
                        ty: AsmType::Byte,
                        is_static: true,
                        is_constant: false,
                    }
                }
            }

            IdentifierAttrs::LocalAttr => {
                let asm_type = type2asmtype(&symbol.ty);
                AsmSymtabEntry::Object {
                    ty: asm_type,
                    is_static: false,
                    is_constant: false,
                }
            }

            IdentifierAttrs::EnumConstantAttr(_) => continue,

            IdentifierAttrs::ConstantAttr(_) => {
                let asm_type = type2asmtype(&symbol.ty);
                AsmSymtabEntry::Object {
                    ty: asm_type,
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
        ty: AsmType,
        is_static: bool,
        is_constant: bool,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Class {
    Memory,
    Sse,
    Integer,
}

fn classify_structure(struct_entry: &StructEntry) -> Vec<Class> {
    let mut size: isize = struct_entry.size as isize;

    /* If the aggregate size is greater than 16 bytes, it cannot be returned in registers.
     * We push Class::Memory for each 8-byte block and return the result. */
    if size > 16 {
        let mut result = vec![];
        while size > 0 {
            result.push(Class::Memory);
            size -= 8;
        }
        return result;
    }

    let eightbyte_count = (struct_entry.size + 7) / 8;
    let mut classes = vec![Class::Sse; eightbyte_count.max(1)];
    let mut scalar_fields = vec![];
    collect_scalar_fields(&struct_entry.members, 0, &mut scalar_fields);

    for (offset, ty) in scalar_fields {
        let field_size = get_size_of_type(&ty);
        if field_size == 0 {
            continue;
        }

        let first_eightbyte = offset / 8;
        let last_eightbyte = (offset + field_size - 1) / 8;

        for idx in first_eightbyte..=last_eightbyte {
            if !is_floating_type(&ty) {
                classes[idx] = Class::Integer;
            }
        }
    }

    classes
}

fn collect_scalar_fields(
    members: &Vec<MemberEntry>,
    base_offset: usize,
    result: &mut Vec<(usize, Type)>,
) {
    for member in members {
        collect_scalar_type(&member.ty, base_offset + member.offset, result);
    }
}

fn collect_scalar_type(ty: &Type, offset: usize, result: &mut Vec<(usize, Type)>) {
    match ty {
        Type::Struct { tag } | Type::Union { tag } => {
            let entry = TYPE_TABLE.lock().unwrap().get(tag).cloned().unwrap();
            collect_scalar_fields(&entry.members, offset, result);
        }
        Type::Array { element, size } => {
            let element_size = get_size_of_type(element);
            for idx in 0..*size {
                collect_scalar_type(element, offset + idx * element_size, result);
            }
        }
        _ => result.push((offset, ty.clone())),
    }
}

fn classify_return_value(
    retval: &IRValue,
) -> (Vec<(AsmType, AsmOperand)>, Vec<(AsmType, AsmOperand)>, bool) {
    let t = ir2type(retval);
    let val = retval.codegen().into();
    classify_return_helper(&t, &val)
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
                /* Too large to fit into registers and must be passed via memory. */
                (vec![AsmRegister::Ax], true)
            } else {
                /* Can be passed via registers. */

                /* We use AX and DX for ints, and XMM0 and XMM1 for doubles, as per the SystemV AMD64 ABI,
                 * to return the values. */
                let int_regs: Vec<AsmRegister> = [AsmRegister::Ax, AsmRegister::Dx]
                    .iter()
                    .take(ints.len())
                    .cloned()
                    .collect();
                let dbl_regs: Vec<AsmRegister> = [AsmRegister::Xmm0, AsmRegister::Xmm1]
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

/* Classifies how a return value should be passed based on its return type. */
fn classify_return_helper(
    ret_type: &Type,
    asm_retval: &AsmOperand,
) -> (Vec<(AsmType, AsmOperand)>, Vec<(AsmType, AsmOperand)>, bool) {
    match ret_type {
        Type::Struct { tag } | Type::Union { tag } => {
            /* For structures, we get the struct entry from the type table and classify it. */

            let struct_entry = TYPE_TABLE.lock().unwrap().get(tag).unwrap().clone();
            let classes = classify_structure(&struct_entry);

            let struct_size = struct_entry.size;

            /* If the structure is classified as Memory, return empty vecs and indicate that
             * the return value should be passed via memory, not registers. */
            if classes[0] == Class::Memory {
                (vec![], vec![], true)
            } else {
                /* If it is not, we iterate over each class, and for each class,
                 * we push a PseudoMem operand in the corresponding basket (int/double).
                 * If the class is SSE, the operand is added to double_retvals.
                 * If the class is Integer, the operand is added to int_retvals along with the
                 * eightbyte type (AsmType). */

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
                            let eightbyte_type = get_sse_eightbyte_type(offset, struct_size);
                            double_retvals.push((eightbyte_type, operand));
                        }
                        Class::Integer => {
                            let eightbyte_type = get_eightbyte_type(offset, struct_size);
                            int_retvals.push((eightbyte_type, operand));
                        }
                        Class::Memory => unreachable!(),
                    }

                    /* Finally, we increment the offset. */
                    offset += 8;
                }

                (int_retvals, double_retvals, false)
            }
        }

        /* For floating-point scalars, the return value is added to SSE retvals. */
        Type::Float | Type::Double => (
            vec![],
            vec![(type2asmtype(ret_type), asm_retval.clone())],
            false,
        ),

        /* For other types, the return value is added to the integer-like basket. */
        t => {
            let typed_operand = (type2asmtype(t), asm_retval.clone());
            (vec![typed_operand], vec![], false)
        }
    }
}

fn copy_bytes_from_reg(
    src_reg: &AsmRegister,
    dst_op: &AsmOperand,
    byte_count: usize,
    instructions: &mut Vec<AsmInstruction>,
) {
    /* Start from the first byte (zero-based) index. */
    let mut offset = 0;

    /* Copy each byte from the source register to the destination operand. */
    while offset < byte_count {
        let dst_byte = add_offset(offset, dst_op);
        instructions.push(AsmInstruction::Mov {
            asm_type: AsmType::Byte,
            src: AsmOperand::Register(*src_reg),
            dst: dst_byte,
        });

        /* If there are more bytes left to copy, shift the source register right by 8 bits
         * in order to make space for the next byte. */
        if offset < byte_count - 1 {
            instructions.push(AsmInstruction::Binary {
                asm_type: AsmType::Quadword,
                op: AsmBinaryOp::ShrTwoOp,
                lhs: AsmOperand::Imm(8),
                rhs: AsmOperand::Register(*src_reg),
            });
        }

        /* Move on to the next byte. */
        offset += 1;
    }
}

fn copy_bytes_to_reg(
    src_op: AsmOperand,
    dst_reg: AsmRegister,
    byte_count: usize,
    instructions: &mut Vec<AsmInstruction>,
) {
    /* Start with the last byte (zero-based index). */
    let mut offset = byte_count as isize - 1;

    /* Copy each byte from the source operand to the destination register, in reverse order. */
    while offset >= 0 {
        /* Move the byte from source operand to destination register. */
        let src_byte = add_offset(offset as usize, &src_op);
        instructions.push(AsmInstruction::Mov {
            asm_type: AsmType::Byte,
            src: src_byte,
            dst: AsmOperand::Register(dst_reg),
        });

        /* If there are more bytes left to move, shift the destination register left by 8 bits
         * in order to make room for the next byte. */
        if offset > 0 {
            instructions.push(AsmInstruction::Binary {
                asm_type: AsmType::Quadword,
                op: AsmBinaryOp::Shl,
                lhs: AsmOperand::Imm(8),
                rhs: AsmOperand::Register(dst_reg),
            });
        }

        /* Move on to the next byte. */
        offset -= 1;
    }
}

fn copy_bytes(src: &AsmOperand, dst: &AsmOperand, byte_count: usize) -> Vec<AsmInstruction> {
    /* If there are no bytes to copy, return an empty vector. */
    if byte_count == 0 {
        return vec![];
    }

    /* Determine the appropriate operand type and size based on the number of bytes left to copy. */
    let (operand_type, operand_size) = if byte_count >= 8 {
        /* If 8 bytes or more, use `Quadword` (8 bytes). */
        (AsmType::Quadword, 8)
    } else if byte_count >= 4 {
        /* If fewer than 8 bytes but at least 4, use `Longword` (4 bytes). */
        (AsmType::Longword, 4)
    } else if byte_count >= 2 {
        (AsmType::Word, 2)
    } else {
        (AsmType::Byte, 1)
    };

    /* Calculate the next source and destination operands. */
    let next_src = add_offset(operand_size, src);
    let next_dst = add_offset(operand_size, dst);

    /* Calculate the number of bytes left to copy after this operation. */
    let bytes_left = byte_count - operand_size;

    let mut instructions = vec![AsmInstruction::Mov {
        asm_type: operand_type,
        src: src.clone(),
        dst: dst.clone(),
    }];

    /* Recursively copy the remaining bytes by calling `copy_bytes` with updated operands and bytes left. */
    instructions.extend(copy_bytes(&next_src, &next_dst, bytes_left));

    instructions
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

fn is_sse_asm_type(asm_type: &AsmType) -> bool {
    matches!(asm_type, AsmType::Float | AsmType::Double)
}

fn is_floating_type(t: &Type) -> bool {
    matches!(t, Type::Float | Type::Double)
}

fn type2asmtype(t: &Type) -> AsmType {
    match t {
        Type::Char | Type::UChar | Type::SChar => AsmType::Byte,
        Type::Short | Type::UShort => AsmType::Word,
        Type::Int | Type::UInt | Type::Enum { .. } => AsmType::Longword,
        Type::Long | Type::ULong => AsmType::Quadword,
        Type::Float => AsmType::Float,
        Type::Double => AsmType::Double,
        Type::Array { element, size } => AsmType::Bytearray {
            size: get_size_of_type(element) * size,
            alignment: get_alignment_of_type(t),
        },
        Type::Struct { tag } | Type::Union { tag } => {
            let struct_size = TYPE_TABLE.lock().unwrap().get(tag).unwrap().size;
            let struct_alignment = TYPE_TABLE.lock().unwrap().get(tag).unwrap().alignment;
            AsmType::Bytearray {
                size: struct_size,
                alignment: struct_alignment,
            }
        }
        Type::Pointer(_) => AsmType::Quadword,
        _ => {
            unreachable!()
        }
    }
}

pub fn ir2type(value: &IRValue) -> Type {
    match value {
        IRValue::Constant(konst) => match konst {
            Const::Char(_) => Type::Char,
            Const::UChar(_) => Type::UChar,
            Const::Short(_) => Type::Short,
            Const::UShort(_) => Type::UShort,
            Const::Int(_) => Type::Int,
            Const::Long(_) => Type::Long,
            Const::UInt(_) => Type::UInt,
            Const::ULong(_) => Type::ULong,
            Const::Float(_) => Type::Float,
            Const::Double(_) => Type::Double,
        },
        IRValue::Var(var_name) => {
            let symbol = SYMBOL_TABLE.lock().unwrap().get(var_name).cloned().unwrap();
            symbol.ty
        }
    }
}

fn get_alignment_of_type(t: &Type) -> usize {
    match t {
        Type::Char | Type::UChar | Type::SChar => 1,
        Type::Short | Type::UShort => 2,
        Type::Int | Type::Enum { .. } => 4,
        Type::Long => 8,
        Type::UInt => 4,
        Type::ULong => 8,
        Type::Float => 4,
        Type::Double => 8,
        Type::Pointer(_) => 8,
        Type::Array { element, size } => {
            if get_size_of_type(element) * size >= 16 {
                16
            } else {
                get_alignment_of_type(element)
            }
        }
        Type::Struct { tag } | Type::Union { tag } => {
            let struct_type = TYPE_TABLE.lock().unwrap().get(tag).cloned().unwrap();
            struct_type.alignment
        }
        _ => unreachable!(),
    }
}

fn get_eightbyte_type(offset: isize, struct_size: usize) -> AsmType {
    let bytes_from_end = struct_size as isize - offset;

    if bytes_from_end >= 8 {
        return AsmType::Quadword;
    }

    if bytes_from_end == 4 {
        return AsmType::Longword;
    }

    if bytes_from_end == 2 {
        return AsmType::Word;
    }

    if bytes_from_end == 1 {
        return AsmType::Byte;
    }

    AsmType::Bytearray {
        size: bytes_from_end as usize,
        alignment: 8,
    }
}

fn get_sse_eightbyte_type(offset: isize, struct_size: usize) -> AsmType {
    let bytes_from_end = struct_size as isize - offset;

    if bytes_from_end <= 4 {
        AsmType::Float
    } else {
        AsmType::Double
    }
}

fn ir2asmtype(value: &IRValue) -> AsmType {
    match value {
        IRValue::Constant(konst) => match konst {
            Const::Char(_) | Const::UChar(_) => AsmType::Byte,
            Const::Short(_) | Const::UShort(_) => AsmType::Word,
            Const::Int(_) => AsmType::Longword,
            Const::Long(_) => AsmType::Quadword,
            Const::UInt(_) => AsmType::Longword,
            Const::ULong(_) => AsmType::Quadword,
            Const::Float(_) => AsmType::Float,
            Const::Double(_) => AsmType::Double,
        },
        IRValue::Var(var_name) => {
            let ty = SYMBOL_TABLE
                .lock()
                .unwrap()
                .get(var_name)
                .cloned()
                .unwrap()
                .ty;

            type2asmtype(&ty)
        }
    }
}

/* Represents a position in the stack with a single i64 value. */
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct StackPosition(pub i64);

/* A struct to manage the mapping of variables to their stack positions. */
#[derive(Debug, Clone, PartialEq)]
pub struct VarToStackPos {
    pub last_used_stack_pos: StackPosition,
    pub var_to_stack_pos: BTreeMap<String, StackPosition>,
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
    /* Maps a variable (by its identifier) to a stack position based on its type. */
    pub fn var_to_stack_pos(&mut self, ident: &str, asm_type: AsmType) -> StackPosition {
        /* If the variable doesn't already have a stack position, compute and insert one. */
        let pos = self
            .var_to_stack_pos
            .entry(ident.to_owned())
            .or_insert_with(|| {
                /* Determine the size to allocate based on the operand's byte length. */
                let alloc = match OperandByteLen::from(asm_type) {
                    OperandByteLen::B1 => 1,
                    OperandByteLen::B2 => 2,
                    OperandByteLen::B4 => 4,
                    OperandByteLen::B8 => 8,
                    OperandByteLen::Other(size) => size as i64,
                };
                self.last_used_stack_pos.0 -= alloc;

                /* Determine the alignment for the operand's type. */
                let alignment = match Alignment::default_of(asm_type) {
                    Alignment::B1 => 1,
                    Alignment::B2 => 2,
                    Alignment::B4 => 4,
                    Alignment::B8 => 8,
                    Alignment::B16 => 16,
                    Alignment::Other(size) => size,
                };

                /* Ensure stack position is aligned by adjusting for remainder (if any). */
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
    B2 = 2,
    B4 = 4,
    B8 = 8,
    Other(usize),
}

impl<T: Into<AsmType>> From<T> for OperandByteLen {
    fn from(t: T) -> Self {
        let asm_type = t.into();
        match asm_type {
            AsmType::Byte => Self::B1,
            AsmType::Word => Self::B2,
            AsmType::Longword => Self::B4,
            AsmType::Quadword => Self::B8,
            AsmType::Float => Self::B4,
            AsmType::Double => Self::B8,
            AsmType::Bytearray { size, alignment: _ } => Self::Other(size),
        }
    }
}

#[repr(usize)]
#[derive(PartialEq, Eq, Hash, Debug)]
pub enum Alignment {
    B1 = 1,
    B2 = 2,
    B4 = 4,
    B8 = 8,
    B16 = 16,
    Other(usize),
}

impl Alignment {
    pub fn default_of<T: Into<AsmType>>(t: T) -> Self {
        let asm_type = t.into();
        match asm_type {
            AsmType::Byte => Self::B1,
            AsmType::Word => Self::B2,
            AsmType::Longword => Self::B4,
            AsmType::Quadword => Self::B8,
            AsmType::Float => Self::B4,
            AsmType::Double => Self::B8,
            AsmType::Bytearray { size: _, alignment } => Self::Other(alignment),
        }
    }
}

lazy_static::lazy_static! {
    pub static ref ASM_SYMBOL_TABLE: Mutex<BTreeMap<String, AsmSymtabEntry>> = Mutex::new(BTreeMap::new());
    pub static ref VAR_TO_STACK_POS: Mutex<VarToStackPos> = Mutex::new(VarToStackPos::default());
    pub static ref STATIC_CONSTANTS: Mutex<Vec<AsmStaticConstant>> = Mutex::new(Vec::new());
}

#[cfg(test)]
mod short_tests {
    use super::*;
    use crate::ir::gen::IRValue;
    use crate::lexer::lex::Const;
    use crate::parser::ast::{AggregateKind, Type};

    #[test]
    fn maps_short_types_to_word_asm_type() {
        assert_eq!(type2asmtype(&Type::Short), AsmType::Word);
        assert_eq!(type2asmtype(&Type::UShort), AsmType::Word);
        assert_eq!(get_alignment_of_type(&Type::Short), 2);
        assert_eq!(get_alignment_of_type(&Type::UShort), 2);
    }

    #[test]
    fn maps_short_constants_to_word_asm_type() {
        let short = IRValue::Constant(Const::Short(-1));
        let ushort = IRValue::Constant(Const::UShort(u16::MAX));

        assert_eq!(ir2type(&short), Type::Short);
        assert_eq!(ir2type(&ushort), Type::UShort);
        assert_eq!(ir2asmtype(&short), AsmType::Word);
        assert_eq!(ir2asmtype(&ushort), AsmType::Word);
    }

    #[test]
    fn word_operands_are_two_bytes() {
        assert!(matches!(
            OperandByteLen::from(AsmType::Word),
            OperandByteLen::B2
        ));
        assert_eq!(Alignment::default_of(AsmType::Word), Alignment::B2);
    }

    #[test]
    fn maps_enum_type_to_int_asm_type() {
        let ty = Type::Enum {
            tag: "enum.codegen.test".to_string(),
        };

        assert_eq!(type2asmtype(&ty), AsmType::Longword);
        assert_eq!(get_alignment_of_type(&ty), 4);
    }

    #[test]
    fn maps_float_type_and_constants_to_float_asm_type() {
        let float = IRValue::Constant(Const::Float(1.5));

        assert_eq!(type2asmtype(&Type::Float), AsmType::Float);
        assert_eq!(get_alignment_of_type(&Type::Float), 4);
        assert_eq!(ir2type(&float), Type::Float);
        assert_eq!(ir2asmtype(&float), AsmType::Float);
    }

    #[test]
    fn float_operands_are_four_bytes() {
        assert!(matches!(
            OperandByteLen::from(AsmType::Float),
            OperandByteLen::B4
        ));
        assert_eq!(Alignment::default_of(AsmType::Float), Alignment::B4);
    }

    #[test]
    fn scalar_float_returns_use_sse_register_class() {
        let operand = AsmOperand::Pseudo("retval".to_string());
        let (ints, sse, on_stack) = classify_return_helper(&Type::Float, &operand);

        assert!(ints.is_empty());
        assert_eq!(sse, vec![(AsmType::Float, operand)]);
        assert!(!on_stack);
    }

    #[test]
    fn classifies_trailing_sse_eightbyte_as_float_when_only_four_bytes_remain() {
        assert_eq!(get_sse_eightbyte_type(0, 4), AsmType::Float);
        assert_eq!(get_sse_eightbyte_type(0, 8), AsmType::Double);
        assert_eq!(get_sse_eightbyte_type(8, 12), AsmType::Float);
    }

    #[test]
    fn classifies_union_with_integer_and_float_members_as_integer() {
        let entry = StructEntry {
            kind: AggregateKind::Union,
            alignment: 4,
            size: 4,
            members: vec![
                MemberEntry {
                    name: "f".to_string(),
                    ty: Type::Float,
                    offset: 0,
                },
                MemberEntry {
                    name: "i".to_string(),
                    ty: Type::Int,
                    offset: 0,
                },
            ],
        };

        assert_eq!(classify_structure(&entry), vec![Class::Integer]);
    }

    #[test]
    fn maps_union_type_to_bytearray_asm_type() {
        let tag = "union.codegen.test".to_string();
        TYPE_TABLE.lock().unwrap().insert(
            tag.clone(),
            StructEntry {
                kind: AggregateKind::Union,
                alignment: 8,
                size: 8,
                members: vec![
                    MemberEntry {
                        name: "i".to_string(),
                        ty: Type::Int,
                        offset: 0,
                    },
                    MemberEntry {
                        name: "d".to_string(),
                        ty: Type::Double,
                        offset: 0,
                    },
                ],
            },
        );

        assert_eq!(
            type2asmtype(&Type::Union { tag }),
            AsmType::Bytearray {
                size: 8,
                alignment: 8,
            }
        );
    }

    #[test]
    fn variadic_calls_load_sse_argument_count_into_al() {
        let target = format!("varargs.codegen.{}", make_temporary());
        SYMBOL_TABLE.lock().unwrap().insert(
            target.clone(),
            crate::semantics::typechecker::Symbol {
                ty: Type::Func {
                    params: vec![Type::Int],
                    ret: Box::new(Type::Void),
                    variadic: true,
                },
                attrs: IdentifierAttrs::FuncAttr {
                    defined: false,
                    global: true,
                },
            },
        );

        let node = IRInstruction::Call {
            target: target.clone(),
            args: vec![
                IRValue::Constant(Const::Int(1)),
                IRValue::Constant(Const::Double(2.0)),
            ],
            dst: None,
        }
        .codegen();

        let AsmNode::Instructions(instructions) = node else {
            panic!("expected instruction node");
        };

        let load_al = instructions
            .iter()
            .position(|instr| {
                matches!(
                    instr,
                    AsmInstruction::Mov {
                        asm_type: AsmType::Byte,
                        src: AsmOperand::Imm(1),
                        dst: AsmOperand::Register(AsmRegister::Ax),
                    }
                )
            })
            .expect("variadic call should load SSE register count into %al");
        let call = instructions
            .iter()
            .position(|instr| matches!(instr, AsmInstruction::Call(name) if name == &target))
            .expect("expected function call");

        assert!(load_al < call);
    }
}
