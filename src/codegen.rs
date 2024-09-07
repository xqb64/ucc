use crate::ir::{IRFunction, IRInstruction, IRNode, IRProgram, IRValue, UnaryOp};

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
    Mov { src: AsmOperand, dst: AsmOperand },
    Unary { op: AsmUnaryOp, operand: AsmOperand },
    AllocateStack(usize),
    Ret,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmOperand {
    Imm(i32),
    Pseudo(String),
    Stack(i32),
    Register(AsmRegister),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmRegister {
    AX,
    R10,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum AsmUnaryOp {
    Neg,
    Not,
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
            functions.push(match func.codegen() {
                AsmNode::Function(func) => func,
                _ => unreachable!(),
            });
        }
        AsmNode::Program(AsmProgram { functions })
    }
}

impl Codegen for IRFunction {
    fn codegen(&self) -> AsmNode {
        let mut instructions = vec![];

        instructions.push(AsmInstruction::AllocateStack(0));

        for instr in &self.body {
            instructions.extend(match instr.codegen() {
                AsmNode::Instructions(instrs) => instrs,
                _ => unreachable!(),
            });
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
            instructions.extend(match instr.codegen() {
                AsmNode::Instructions(instrs) => instrs,
                _ => unreachable!(),
            });
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
            IRInstruction::Unary { op, src, dst } => AsmNode::Instructions(vec![
                AsmInstruction::Mov {
                    src: src.codegen().into(),
                    dst: dst.codegen().into(),
                },
                AsmInstruction::Unary {
                    op: (*op).into(),
                    operand: dst.codegen().into(),
                },
            ]),
            IRInstruction::Ret(value) => AsmNode::Instructions(vec![
                AsmInstruction::Mov {
                    src: value.codegen().into(),
                    dst: AsmOperand::Register(AsmRegister::AX),
                },
                AsmInstruction::Ret,
            ]),
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
            AsmInstruction::AllocateStack(n) => AsmInstruction::AllocateStack(*n),
            AsmInstruction::Ret => AsmInstruction::Ret,
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
            let mut instructions = vec![];

            for instr in &mut func.instructions {
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

            functions.push(AsmFunction {
                name: func.name.clone(),
                instructions,
            });
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
