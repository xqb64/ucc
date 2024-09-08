use crate::ir::{BinaryOp, IRFunction, IRInstruction, IRNode, IRProgram, IRValue, UnaryOp};

#[derive(Debug, Clone, PartialEq)]
pub enum AsmNode {
    Program(AsmProgram),
    Function(AsmFunction),
    Operand(AsmOperand),
    Instructions(Vec<AsmInstruction>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmProgram {
    pub functions: Vec<AsmFunction>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmFunction {
    pub name: String,
    pub instructions: Vec<AsmInstruction>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmInstruction {
    Mov {
        src: AsmOperand,
        dst: AsmOperand,
    },
    Unary {
        op: AsmUnaryOp,
        operand: AsmOperand,
    },
    Binary {
        op: AsmBinaryOp,
        lhs: AsmOperand,
        rhs: AsmOperand,
    },
    Cmp {
        lhs: AsmOperand,
        rhs: AsmOperand,
    },
    Imul {
        src: AsmOperand,
        dst: AsmOperand,
    },
    Idiv(AsmOperand),
    Cdq,
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
    Imm(i32),
    Pseudo(String),
    Stack(i32),
    Register(AsmRegister),
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
        }
    }
}

impl Codegen for IRProgram {
    fn codegen(&self) -> AsmNode {
        let mut functions = vec![];
        for func in &self.functions {
            functions.push(func.codegen().into());
        }
        AsmNode::Program(AsmProgram { functions })
    }
}

impl Codegen for IRFunction {
    fn codegen(&self) -> AsmNode {
        let mut instructions = vec![];

        instructions.push(AsmInstruction::AllocateStack(0));

        let registers = vec![
            AsmRegister::DI,
            AsmRegister::SI,
            AsmRegister::DX,
            AsmRegister::CX,
            AsmRegister::R8,
            AsmRegister::R9,
        ];

        let mut reg_idx = 0;

        for arg in self.params.iter().take(6) {
            instructions.push(AsmInstruction::Mov {
                src: AsmOperand::Register(registers[reg_idx]),
                dst: AsmOperand::Pseudo(arg.to_owned()),
            });
            reg_idx += 1;
        }

        let mut stack_offset = 16;
        for arg in self.params.iter().skip(6) {
            instructions.push(AsmInstruction::Mov {
                src: AsmOperand::Stack(stack_offset),
                dst: AsmOperand::Pseudo(arg.to_owned()),
            });
            stack_offset += 8;
        }

        for instr in &self.body {
            instructions.extend::<Vec<AsmInstruction>>(instr.codegen().into());
        }

        AsmNode::Function(AsmFunction {
            name: self.name.clone(),
            instructions,
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
            IRValue::Constant(n) => AsmNode::Operand(AsmOperand::Imm(*n)),
            IRValue::Var(name) => AsmNode::Operand(AsmOperand::Pseudo(name.to_owned())),
        }
    }
}

impl Codegen for IRInstruction {
    fn codegen(&self) -> AsmNode {
        match self {
            IRInstruction::Unary { op, src, dst } => match op {
                UnaryOp::Negate | UnaryOp::Complement => AsmNode::Instructions(vec![
                    AsmInstruction::Mov {
                        src: src.codegen().into(),
                        dst: dst.codegen().into(),
                    },
                    AsmInstruction::Unary {
                        op: (*op).into(),
                        operand: dst.codegen().into(),
                    },
                ]),
                UnaryOp::Not => AsmNode::Instructions(vec![
                    AsmInstruction::Cmp {
                        lhs: AsmOperand::Imm(0),
                        rhs: src.codegen().into(),
                    },
                    AsmInstruction::Mov {
                        src: AsmOperand::Imm(0),
                        dst: dst.codegen().into(),
                    },
                    AsmInstruction::SetCC {
                        condition: ConditionCode::E,
                        operand: dst.codegen().into(),
                    },
                ]),
            },
            IRInstruction::Ret(value) => AsmNode::Instructions(vec![
                AsmInstruction::Mov {
                    src: value.codegen().into(),
                    dst: AsmOperand::Register(AsmRegister::AX),
                },
                AsmInstruction::Ret,
            ]),
            IRInstruction::Binary { op, lhs, rhs, dst } => match op {
                BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul => AsmNode::Instructions(vec![
                    AsmInstruction::Mov {
                        src: lhs.codegen().into(),
                        dst: dst.codegen().into(),
                    },
                    AsmInstruction::Binary {
                        op: (*op).into(),
                        lhs: rhs.codegen().into(),
                        rhs: dst.codegen().into(),
                    },
                ]),
                BinaryOp::Div => AsmNode::Instructions(vec![
                    AsmInstruction::Mov {
                        src: lhs.codegen().into(),
                        dst: AsmOperand::Register(AsmRegister::AX),
                    },
                    AsmInstruction::Cdq,
                    AsmInstruction::Idiv(rhs.codegen().into()),
                    AsmInstruction::Mov {
                        src: AsmOperand::Register(AsmRegister::AX),
                        dst: dst.codegen().into(),
                    },
                ]),
                BinaryOp::Rem => AsmNode::Instructions(vec![
                    AsmInstruction::Mov {
                        src: lhs.codegen().into(),
                        dst: AsmOperand::Register(AsmRegister::AX),
                    },
                    AsmInstruction::Cdq,
                    AsmInstruction::Idiv(rhs.codegen().into()),
                    AsmInstruction::Mov {
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
                        lhs: rhs.codegen().into(),
                        rhs: lhs.codegen().into(),
                    },
                    AsmInstruction::Mov {
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
            },
            IRInstruction::JumpIfZero { condition, target } => AsmNode::Instructions(vec![
                AsmInstruction::Cmp {
                    lhs: AsmOperand::Imm(0),
                    rhs: condition.codegen().into(),
                },
                AsmInstruction::JmpCC {
                    condition: ConditionCode::E,
                    target: target.to_owned(),
                },
            ]),
            IRInstruction::JumpIfNotZero { condition, target } => AsmNode::Instructions(vec![
                AsmInstruction::Cmp {
                    lhs: AsmOperand::Imm(0),
                    rhs: condition.codegen().into(),
                },
                AsmInstruction::JmpCC {
                    condition: ConditionCode::NE,
                    target: target.to_owned(),
                },
            ]),
            IRInstruction::Jump(target) => AsmNode::Instructions(vec![AsmInstruction::Jmp {
                target: target.to_owned(),
            }]),
            IRInstruction::Label(label) => {
                AsmNode::Instructions(vec![AsmInstruction::Label(label.to_owned())])
            }
            IRInstruction::Copy { src, dst } => AsmNode::Instructions(vec![AsmInstruction::Mov {
                src: src.codegen().into(),
                dst: dst.codegen().into(),
            }]),
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

                let stack_padding;
                if stack_args.len() % 2 != 0 {
                    stack_padding = 8;
                } else {
                    stack_padding = 0;
                }

                let mut instructions = vec![];

                if stack_padding != 0 {
                    instructions.push(AsmInstruction::AllocateStack(stack_padding));
                }

                let mut reg_index = 0;
                for reg_arg in register_args {
                    let reg = arg_registers[reg_index];
                    let asm_arg = reg_arg.codegen().into();
                    instructions.push(AsmInstruction::Mov {
                        src: asm_arg,
                        dst: AsmOperand::Register(reg),
                    });
                    reg_index += 1;
                }

                for stack_arg in stack_args.iter().rev() {
                    let asm_arg = stack_arg.codegen().into();
                    match asm_arg {
                        AsmOperand::Imm(_) | AsmOperand::Register(_) => {
                            instructions.push(AsmInstruction::Push(asm_arg));
                        }
                        _ => {
                            instructions.push(AsmInstruction::Mov {
                                src: asm_arg,
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

                let asm_dst = dst.codegen().into();
                instructions.push(AsmInstruction::Mov {
                    src: AsmOperand::Register(AsmRegister::AX),
                    dst: asm_dst,
                });

                AsmNode::Instructions(instructions)
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
            _ => unreachable!(),
        }
    }
}

impl ReplacePseudo for AsmInstruction {
    fn replace_pseudo(&self) -> Self {
        match self {
            AsmInstruction::Mov { src, dst } => AsmInstruction::Mov {
                src: src.replace_pseudo(),
                dst: dst.replace_pseudo(),
            },
            AsmInstruction::Unary { op, operand } => AsmInstruction::Unary {
                op: *op,
                operand: operand.replace_pseudo(),
            },
            AsmInstruction::Binary { op, lhs, rhs } => AsmInstruction::Binary {
                op: *op,
                lhs: lhs.replace_pseudo(),
                rhs: rhs.replace_pseudo(),
            },
            AsmInstruction::Idiv(operand) => AsmInstruction::Idiv(operand.replace_pseudo()),
            AsmInstruction::AllocateStack(n) => AsmInstruction::AllocateStack(*n),
            AsmInstruction::Ret => AsmInstruction::Ret,
            AsmInstruction::Cmp { lhs, rhs } => AsmInstruction::Cmp {
                lhs: lhs.replace_pseudo(),
                rhs: rhs.replace_pseudo(),
            },
            AsmInstruction::SetCC { condition, operand } => AsmInstruction::SetCC {
                condition: condition.clone(),
                operand: operand.replace_pseudo(),
            },
            AsmInstruction::Push(operand) => AsmInstruction::Push(operand.replace_pseudo()),
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
                AsmOperand::Stack(offset_manager.get_offset(name))
            }
            AsmOperand::Stack(_) => self.clone(),
            AsmOperand::Register(_) => self.clone(),
        }
    }
}

impl ReplacePseudo for AsmProgram {
    fn replace_pseudo(&self) -> Self {
        let mut functions = vec![];
        for func in &self.functions {
            functions.push(func.replace_pseudo());
        }
        AsmProgram { functions }
    }
}

impl ReplacePseudo for AsmFunction {
    fn replace_pseudo(&self) -> Self {
        let mut instructions = vec![];
        for instr in &self.instructions {
            instructions.push(instr.replace_pseudo());
        }
        AsmFunction {
            name: self.name.clone(),
            instructions,
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
        }
    }
}

impl Fixup for AsmProgram {
    fn fixup(&mut self) -> AsmProgram {
        let mut functions = vec![];

        for func in &mut self.functions {
            functions.push(func.fixup());
        }

        AsmProgram { functions }
    }
}

impl Fixup for AsmFunction {
    fn fixup(&mut self) -> AsmFunction {
        let mut instructions = vec![];

        for instr in &mut self.instructions {
            match instr {
                AsmInstruction::Mov { src, dst } => match (src, dst) {
                    (AsmOperand::Stack(src_n), AsmOperand::Stack(dst_n)) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                src: AsmOperand::Stack(*src_n),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Mov {
                                src: AsmOperand::Register(AsmRegister::R10),
                                dst: AsmOperand::Stack(*dst_n),
                            },
                        ]);
                    }
                    _ => instructions.push(instr.clone()),
                },
                AsmInstruction::Binary { op, lhs, rhs } => match op {
                    AsmBinaryOp::Add | AsmBinaryOp::Sub => match (lhs, rhs) {
                        (AsmOperand::Stack(src_n), AsmOperand::Stack(dst_n)) => {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    src: AsmOperand::Stack(*src_n),
                                    dst: AsmOperand::Register(AsmRegister::R10),
                                },
                                AsmInstruction::Binary {
                                    op: *op,
                                    lhs: AsmOperand::Register(AsmRegister::R10),
                                    rhs: AsmOperand::Stack(*dst_n),
                                },
                            ]);
                        }
                        _ => instructions.push(instr.clone()),
                    },
                    AsmBinaryOp::Mul => match rhs {
                        AsmOperand::Stack(_) => {
                            instructions.extend(vec![
                                AsmInstruction::Mov {
                                    src: rhs.clone(),
                                    dst: AsmOperand::Register(AsmRegister::R11),
                                },
                                AsmInstruction::Imul {
                                    src: lhs.clone(),
                                    dst: AsmOperand::Register(AsmRegister::R11),
                                },
                                AsmInstruction::Mov {
                                    src: AsmOperand::Register(AsmRegister::R11),
                                    dst: rhs.clone(),
                                },
                            ]);
                        }
                        _ => instructions.push(instr.clone()),
                    },
                },
                AsmInstruction::Idiv(operand) => {
                    if let AsmOperand::Imm(konst) = operand {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                src: AsmOperand::Imm(*konst),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Idiv(AsmOperand::Register(AsmRegister::R10)),
                        ]);
                    } else {
                        instructions.push(instr.clone());
                    }
                }
                AsmInstruction::Cmp { lhs, rhs } => match (lhs.clone(), rhs.clone()) {
                    (AsmOperand::Stack(src_n), AsmOperand::Stack(dst_n)) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                src: AsmOperand::Stack(src_n),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Cmp {
                                lhs: AsmOperand::Register(AsmRegister::R10),
                                rhs: AsmOperand::Stack(dst_n),
                            },
                        ]);
                    }
                    (_, AsmOperand::Imm(konst)) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                src: AsmOperand::Imm(konst),
                                dst: AsmOperand::Register(AsmRegister::R11),
                            },
                            AsmInstruction::Cmp {
                                lhs: lhs.clone(),
                                rhs: AsmOperand::Register(AsmRegister::R11),
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
        }
    }
}

impl Fixup for Vec<AsmInstruction> {
    fn fixup(&mut self) -> Vec<AsmInstruction> {
        let mut instructions = vec![];

        for instr in self {
            match instr {
                AsmInstruction::Mov { src, dst } => match (src, dst) {
                    (AsmOperand::Stack(src_n), AsmOperand::Stack(dst_n)) => {
                        instructions.extend(vec![
                            AsmInstruction::Mov {
                                src: AsmOperand::Stack(*src_n),
                                dst: AsmOperand::Register(AsmRegister::R10),
                            },
                            AsmInstruction::Mov {
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
