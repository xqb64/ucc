use crate::parser::{
    AssignExpression, BinaryExpression, BinaryExpressionKind, BlockItem, Declaration, Expression, ExpressionStatement, FunctionDeclaration, IfStatement, ProgramStatement, ReturnStatement, Statement, UnaryExpression, UnaryExpressionKind, VariableDeclaration
};

#[derive(Debug, Clone, PartialEq)]
pub struct IRProgram {
    pub functions: Vec<IRFunction>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IRFunction {
    pub name: String,
    pub body: Vec<IRInstruction>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IRInstruction {
    Unary {
        op: UnaryOp,
        src: IRValue,
        dst: IRValue,
    },
    Binary {
        op: BinaryOp,
        lhs: IRValue,
        rhs: IRValue,
        dst: IRValue,
    },
    Copy {
        src: IRValue,
        dst: IRValue,
    },
    Jump(String),
    JumpIfZero {
        condition: IRValue,
        target: String,
    },
    JumpIfNotZero {
        condition: IRValue,
        target: String,
    },
    Label(String),
    Ret(IRValue),
}

#[derive(Debug, Clone, PartialEq)]
pub enum IRValue {
    Constant(i32),
    Var(String),
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum UnaryOp {
    Negate,
    Complement,
    Not,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Less,
    Greater,
    Equal,
    NotEqual,
    LessEqual,
    GreaterEqual,
}

fn emit_tacky(e: Expression, instructions: &mut Vec<IRInstruction>) -> IRValue {
    match e {
        Expression::Constant(konst) => IRValue::Constant(konst),
        Expression::Unary(UnaryExpression { kind, expr }) => {
            let src = emit_tacky(*expr, instructions);
            let dst_name = format!("var.{}", make_temporary());
            let dst = IRValue::Var(dst_name.clone());
            let op = match kind {
                UnaryExpressionKind::Negate => UnaryOp::Negate,
                UnaryExpressionKind::Complement => UnaryOp::Complement,
                UnaryExpressionKind::Not => UnaryOp::Not,
            };
            instructions.push(IRInstruction::Unary {
                op,
                src,
                dst: dst.clone(),
            });

            dst
        }
        Expression::Binary(BinaryExpression { kind, lhs, rhs }) => match kind {
            BinaryExpressionKind::And => {
                let tmp = make_temporary();

                let false_label = format!("And.{}.shortcircuit", tmp);
                let end_label = format!("And.{}.end", tmp);

                let result = format!("var.{}", make_temporary());
                let result = IRValue::Var(result.clone());

                let lhs = emit_tacky(*lhs, instructions);
                instructions.push(IRInstruction::JumpIfZero {
                    condition: lhs.clone(),
                    target: false_label.clone(),
                });

                let rhs = emit_tacky(*rhs, instructions);
                instructions.push(IRInstruction::JumpIfZero {
                    condition: rhs.clone(),
                    target: false_label.clone(),
                });

                instructions.push(IRInstruction::Copy {
                    src: IRValue::Constant(1),
                    dst: result.clone(),
                });

                instructions.push(IRInstruction::Jump(end_label.clone()));

                instructions.push(IRInstruction::Label(false_label.clone()));

                instructions.push(IRInstruction::Copy {
                    src: IRValue::Constant(0),
                    dst: result.clone(),
                });

                instructions.push(IRInstruction::Label(end_label.clone()));

                result
            }
            BinaryExpressionKind::Or => {
                let tmp = make_temporary();

                let true_label = format!("Or.{}.shortcircuit", tmp);
                let end_label = format!("Or.{}.end", tmp);

                let result = format!("var.{}", make_temporary());
                let result = IRValue::Var(result.clone());

                let lhs = emit_tacky(*lhs, instructions);
                instructions.push(IRInstruction::JumpIfNotZero {
                    condition: lhs.clone(),
                    target: true_label.clone(),
                });

                let rhs = emit_tacky(*rhs, instructions);
                instructions.push(IRInstruction::JumpIfNotZero {
                    condition: rhs.clone(),
                    target: true_label.clone(),
                });

                instructions.push(IRInstruction::Copy {
                    src: IRValue::Constant(0),
                    dst: result.clone(),
                });

                instructions.push(IRInstruction::Jump(end_label.clone()));

                instructions.push(IRInstruction::Label(true_label.clone()));

                instructions.push(IRInstruction::Copy {
                    src: IRValue::Constant(1),
                    dst: result.clone(),
                });

                instructions.push(IRInstruction::Label(end_label.clone()));

                result
            }
            _ => {
                let lhs = emit_tacky(*lhs, instructions);
                let rhs = emit_tacky(*rhs, instructions);
                let dst_name = format!("var.{}", make_temporary());
                let dst = IRValue::Var(dst_name.clone());
                let op = match kind {
                    BinaryExpressionKind::Add => BinaryOp::Add,
                    BinaryExpressionKind::Sub => BinaryOp::Sub,
                    BinaryExpressionKind::Mul => BinaryOp::Mul,
                    BinaryExpressionKind::Div => BinaryOp::Div,
                    BinaryExpressionKind::Rem => BinaryOp::Rem,
                    BinaryExpressionKind::Less => BinaryOp::Less,
                    BinaryExpressionKind::Greater => BinaryOp::Greater,
                    BinaryExpressionKind::Equal => BinaryOp::Equal,
                    BinaryExpressionKind::NotEqual => BinaryOp::NotEqual,
                    BinaryExpressionKind::GreaterEqual => BinaryOp::GreaterEqual,
                    BinaryExpressionKind::LessEqual => BinaryOp::LessEqual,
                    _ => unreachable!(),
                };
                instructions.push(IRInstruction::Binary {
                    op,
                    lhs,
                    rhs,
                    dst: dst.clone(),
                });

                dst
            }
        },
        Expression::Variable(name) => IRValue::Var(name),
        Expression::Assign(AssignExpression { op: _, lhs, rhs }) => {
            let result = emit_tacky(*rhs, instructions);

            if let Expression::Variable(var) = *lhs {
                instructions.push(IRInstruction::Copy {
                    src: result.clone(),
                    dst: IRValue::Var(var.clone()),
                });
                return IRValue::Var(var);
            } else {
                unimplemented!()
            }
        }
        _ => todo!(),
    }
}

pub fn make_temporary() -> usize {
    static mut TEMPORARY: usize = 0;
    unsafe {
        let temporary = TEMPORARY;
        TEMPORARY += 1;
        temporary
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IRNode {
    Program(IRProgram),
    Function(IRFunction),
    Instructions(Vec<IRInstruction>),
    Value(IRValue),
}

pub trait Irfy {
    fn irfy(&self) -> IRNode;
}

impl Irfy for Statement {
    fn irfy(&self) -> IRNode {
        match self {
            Statement::Program(prog) => prog.irfy(),
            Statement::Return(ret_stmt) => ret_stmt.irfy(),
            Statement::Expression(expr_stmt) => expr_stmt.irfy(),
            Self::If(if_stmt) => if_stmt.irfy(),
            Statement::Null => IRNode::Instructions(vec![]),
        }
    }
}

impl Irfy for ProgramStatement {
    fn irfy(&self) -> IRNode {
        let mut functions = vec![];

        for func in &self.stmts {
            functions.push(func.irfy().into());
        }

        IRNode::Program(IRProgram { functions })
    }
}

impl Irfy for IfStatement {
    fn irfy(&self) -> IRNode {
        unimplemented!()
    }
}

impl Irfy for ReturnStatement {
    fn irfy(&self) -> IRNode {
        let mut instructions = vec![];
        let result = emit_tacky(self.expr.clone(), &mut instructions);
        instructions.push(IRInstruction::Ret(result));
        IRNode::Instructions(instructions)
    }
}

impl Irfy for ExpressionStatement {
    fn irfy(&self) -> IRNode {
        let mut instructions = vec![];
        let _ = emit_tacky(self.expr.clone(), &mut instructions);
        IRNode::Instructions(instructions)
    }
}

impl Irfy for BlockItem {
    fn irfy(&self) -> IRNode {
        match self {
            BlockItem::Declaration(decl) => match decl {
                Declaration::Function(func) => func.irfy(),
                Declaration::Variable(var) => var.irfy(),
            },
            BlockItem::Statement(stmt) => stmt.irfy(),
        }
    }
}

impl Irfy for FunctionDeclaration {
    fn irfy(&self) -> IRNode {
        let mut instructions = vec![];

        for stmt in &self.body {
            instructions.extend::<Vec<IRInstruction>>(stmt.irfy().into());
        }

        instructions.push(IRInstruction::Ret(IRValue::Constant(0)));

        IRNode::Function(IRFunction {
            name: self.name.clone(),
            body: instructions,
        })
    }
}

impl Irfy for VariableDeclaration {
    fn irfy(&self) -> IRNode {
        let mut instructions = vec![];

        if let Some(init) = &self.init {
            let result = emit_tacky(init.clone(), &mut instructions);
            instructions.push(IRInstruction::Copy {
                src: result,
                dst: IRValue::Var(self.name.clone()),
            });
        }

        IRNode::Instructions(instructions)
    }
}

impl From<IRNode> for IRProgram {
    fn from(node: IRNode) -> Self {
        match node {
            IRNode::Program(prog) => prog,
            _ => unreachable!(),
        }
    }
}

impl From<IRNode> for IRFunction {
    fn from(node: IRNode) -> Self {
        match node {
            IRNode::Function(func) => func,
            _ => unreachable!(),
        }
    }
}

impl From<IRNode> for Vec<IRInstruction> {
    fn from(node: IRNode) -> Self {
        match node {
            IRNode::Instructions(instrs) => instrs,
            _ => unreachable!(),
        }
    }
}
