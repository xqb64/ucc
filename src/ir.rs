use crate::{
    lexer::Const,
    parser::{
        AssignExpression, BinaryExpression, BinaryExpressionKind, BlockItem, BlockStatement,
        BreakStatement, CallExpression, CastExpression, ConditionalExpression, ContinueStatement,
        Declaration, DoWhileStatement, Expression, ExpressionStatement, ForInit, ForStatement,
        FunctionDeclaration, IfStatement, ProgramStatement, ReturnStatement, Statement, Type,
        UnaryExpression, UnaryExpressionKind, VariableDeclaration, WhileStatement,
    },
    typechecker::{
        get_signedness, get_size_of_type, get_type, IdentifierAttrs, InitialValue, StaticInit,
        Symbol, SYMBOL_TABLE,
    },
};

#[derive(Debug, Clone, PartialEq)]
pub struct IRProgram {
    pub functions: Vec<IRNode>,
    pub static_vars: Vec<IRNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IRFunction {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<IRInstruction>,
    pub global: bool,
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
    Call {
        target: String,
        args: Vec<IRValue>,
        dst: IRValue,
    },
    SignExtend {
        src: IRValue,
        dst: IRValue,
    },
    ZeroExtend {
        src: IRValue,
        dst: IRValue,
    },
    Truncate {
        src: IRValue,
        dst: IRValue,
    },
    DoubleToInt {
        src: IRValue,
        dst: IRValue,
    },
    IntToDouble {
        src: IRValue,
        dst: IRValue,
    },
    DoubletoUInt {
        src: IRValue,
        dst: IRValue,
    },
    UIntToDouble {
        src: IRValue,
        dst: IRValue,
    },
    Ret(IRValue),
}

#[derive(Debug, Clone, PartialEq)]
pub enum IRValue {
    Constant(Const),
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
    let t = get_type(&e.clone());
    match e {
        Expression::Constant(const_expr) => IRValue::Constant(const_expr.value),
        Expression::Unary(UnaryExpression { kind, expr, _type }) => {
            let src = emit_tacky(*expr, instructions);
            let dst = make_tacky_variable(_type.clone());
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
        Expression::Binary(BinaryExpression {
            kind,
            lhs,
            rhs,
            _type,
        }) => match kind {
            BinaryExpressionKind::And => {
                let tmp = make_temporary();

                let false_label = format!("And.{}.shortcircuit", tmp);
                let end_label = format!("And.{}.end", tmp);

                let result = make_tacky_variable(_type.clone());

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
                    src: IRValue::Constant(Const::Int(1)),
                    dst: result.clone(),
                });

                instructions.push(IRInstruction::Jump(end_label.clone()));

                instructions.push(IRInstruction::Label(false_label.clone()));

                instructions.push(IRInstruction::Copy {
                    src: IRValue::Constant(Const::Int(0)),
                    dst: result.clone(),
                });

                instructions.push(IRInstruction::Label(end_label.clone()));

                result
            }
            BinaryExpressionKind::Or => {
                let tmp = make_temporary();

                let true_label = format!("Or.{}.shortcircuit", tmp);
                let end_label = format!("Or.{}.end", tmp);

                let result = make_tacky_variable(_type.clone());

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
                    src: IRValue::Constant(Const::Int(0)),
                    dst: result.clone(),
                });

                instructions.push(IRInstruction::Jump(end_label.clone()));

                instructions.push(IRInstruction::Label(true_label.clone()));

                instructions.push(IRInstruction::Copy {
                    src: IRValue::Constant(Const::Int(1)),
                    dst: result.clone(),
                });

                instructions.push(IRInstruction::Label(end_label.clone()));

                result
            }
            _ => {
                let lhs = emit_tacky(*lhs, instructions);
                let rhs = emit_tacky(*rhs, instructions);

                let dst = make_tacky_variable(t.clone());

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
        Expression::Variable(var) => IRValue::Var(var.value),
        Expression::Assign(AssignExpression {
            op: _,
            lhs,
            rhs,
            _type,
        }) => {
            let result = emit_tacky(*rhs, instructions);

            if let Expression::Variable(var) = *lhs {
                instructions.push(IRInstruction::Copy {
                    src: result.clone(),
                    dst: IRValue::Var(var.value.clone()),
                });
                IRValue::Var(var.value)
            } else {
                unimplemented!()
            }
        }
        Expression::Conditional(ConditionalExpression {
            condition,
            then_expr,
            else_expr,
            _type,
        }) => {
            let tmp = make_temporary();

            let result = make_tacky_variable(_type.clone());

            let e2_label = format!("Cond.{}.else", tmp);
            let end_label = format!("Cond.{}.end", tmp);
            let condition = emit_tacky(*condition, instructions);

            instructions.push(IRInstruction::JumpIfZero {
                condition: condition.clone(),
                target: e2_label.clone(),
            });

            let e1 = emit_tacky(*then_expr, instructions);

            instructions.push(IRInstruction::Copy {
                src: e1,
                dst: result.clone(),
            });

            instructions.push(IRInstruction::Jump(end_label.clone()));

            instructions.push(IRInstruction::Label(e2_label));

            let e2 = emit_tacky(*else_expr, instructions);

            instructions.push(IRInstruction::Copy {
                src: e2,
                dst: result.clone(),
            });

            instructions.push(IRInstruction::Label(end_label));

            result
        }
        Expression::Call(CallExpression { name, args, _type }) => {
            let mut arg_values = vec![];

            for arg in args {
                arg_values.push(emit_tacky(arg, instructions));
            }

            let result = make_tacky_variable(_type.clone());

            instructions.push(IRInstruction::Call {
                target: name,
                args: arg_values,
                dst: result.clone(),
            });

            result
        }
        Expression::Cast(CastExpression {
            target_type,
            expr,
            _type,
        }) => {

            println!("Casting from {:?} to {:?}", get_type(&expr), target_type);

            let result = emit_tacky(*expr.clone(), instructions);
            let inner_type = get_type(&expr);

            if target_type == inner_type {
                return result;
            }

            match (inner_type.clone(), target_type.clone()) {
                (Type::Int, Type::Double) => {
                    let dst = make_tacky_variable(target_type.clone());
                    instructions.push(IRInstruction::IntToDouble {
                        src: result.clone(),
                        dst: dst.clone(),
                    });
                    return dst;
                }
                (Type::Double, Type::Int) => {
                    let dst = make_tacky_variable(target_type.clone());
                    instructions.push(IRInstruction::DoubleToInt {
                        src: result.clone(),
                        dst: dst.clone(),
                    });
                    return dst;
                }
                (Type::Uint, Type::Double) => {
                    let dst = make_tacky_variable(target_type.clone());
                    instructions.push(IRInstruction::UIntToDouble {
                        src: result.clone(),
                        dst: dst.clone(),
                    });
                    return dst;
                }
                (Type::Double, Type::Uint) => {
                    let dst = make_tacky_variable(target_type.clone());
                    instructions.push(IRInstruction::DoubletoUInt {
                        src: result.clone(),
                        dst: dst.clone(),
                    });
                    return dst;
                }
                (Type::Double, Type::Long) => {
                    let dst = make_tacky_variable(target_type.clone());
                    instructions.push(IRInstruction::DoubleToInt {
                        src: result.clone(),
                        dst: dst.clone(),
                    });
                    return dst;
                }
                (Type::Double, Type::Ulong) => {
                    let dst = make_tacky_variable(target_type.clone());
                    instructions.push(IRInstruction::DoubletoUInt {
                        src: result.clone(),
                        dst: dst.clone(),
                    });
                    return dst;
                }
                (Type::Ulong, Type::Double) => {
                    let dst = make_tacky_variable(target_type.clone());
                    instructions.push(IRInstruction::UIntToDouble {
                        src: result.clone(),
                        dst: dst.clone(),
                    });
                    return dst;
                }
                _ => {}
            }

            let dst = make_tacky_variable(target_type.clone());

            if get_size_of_type(&target_type) == get_size_of_type(&inner_type) {
                instructions.push(IRInstruction::Copy {
                    src: result.clone(),
                    dst: dst.clone(),
                });
            } else if get_size_of_type(&target_type) < get_size_of_type(&inner_type) {
                instructions.push(IRInstruction::Truncate {
                    src: result.clone(),
                    dst: dst.clone(),
                });
            } else if get_signedness(&inner_type) {
                instructions.push(IRInstruction::SignExtend {
                    src: result.clone(),
                    dst: dst.clone(),
                });
            } else {
                instructions.push(IRInstruction::ZeroExtend {
                    src: result.clone(),
                    dst: dst.clone(),
                });
            }

            dst
        }
    }
}

pub fn make_tacky_variable(_type: Type) -> IRValue {
    let var_name = format!("var.{}", make_temporary());
    let symbol = Symbol {
        attrs: IdentifierAttrs::LocalAttr,
        _type,
    };
    SYMBOL_TABLE
        .lock()
        .unwrap()
        .insert(var_name.clone(), symbol);
    IRValue::Var(var_name)
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
    StaticVariable(IRStaticVariable),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IRStaticVariable {
    pub name: String,
    pub _type: Type,
    pub init: StaticInit,
    pub global: bool,
}

pub trait Irfy {
    fn irfy(&self) -> Option<IRNode>;
}

impl Irfy for Statement {
    fn irfy(&self) -> Option<IRNode> {
        match self {
            Statement::Program(prog) => prog.irfy(),
            Statement::Return(ret_stmt) => ret_stmt.irfy(),
            Statement::Expression(expr_stmt) => expr_stmt.irfy(),
            Statement::If(if_stmt) => if_stmt.irfy(),
            Statement::Compound(block_stmt) => block_stmt.irfy(),
            Statement::Break(break_stmt) => break_stmt.irfy(),
            Statement::Continue(continue_stmt) => continue_stmt.irfy(),
            Statement::While(while_stmt) => while_stmt.irfy(),
            Statement::DoWhile(do_while_stmt) => do_while_stmt.irfy(),
            Statement::For(for_stmt) => for_stmt.irfy(),
            Statement::Null => None,
        }
    }
}

impl Irfy for ProgramStatement {
    fn irfy(&self) -> Option<IRNode> {
        let mut instructions = vec![];

        for block_item in &self.block_items {
            match block_item {
                BlockItem::Declaration(decl) => match decl {
                    Declaration::Function(func_decl) => {
                        if let Some(ir_func) = func_decl.irfy() {
                            instructions.push(ir_func);
                        }
                    }
                    Declaration::Variable(var_decl) => {
                        if var_decl.is_global {
                            continue;
                        }
                        if !var_decl.is_global && var_decl.storage_class.is_some() {
                            continue;
                        }
                    }
                },
                BlockItem::Statement(stmt) => {
                    if let Some(ir_stmt) = stmt.irfy() {
                        instructions.push(ir_stmt);
                    }
                }
            }
        }

        Some(IRNode::Program(IRProgram {
            functions: instructions,
            static_vars: vec![],
        }))
    }
}

impl Irfy for BlockStatement {
    fn irfy(&self) -> Option<IRNode> {
        let mut instructions = vec![];

        for stmt in &self.stmts {
            match stmt {
                BlockItem::Declaration(decl) => match decl {
                    Declaration::Variable(var_decl) => {
                        if !var_decl.is_global && var_decl.storage_class.is_some() {
                            continue;
                        } else {
                            instructions
                                .extend::<Vec<IRInstruction>>(var_decl.irfy().unwrap().into());
                        }
                    }
                    Declaration::Function(_func_decl) => {}
                },
                BlockItem::Statement(stmt) => {
                    if let Some(ir_stmt) = stmt.irfy() {
                        instructions.extend::<Vec<IRInstruction>>(ir_stmt.into());
                    }
                }
            }
        }

        Some(IRNode::Instructions(instructions))
    }
}

impl Irfy for IfStatement {
    fn irfy(&self) -> Option<IRNode> {
        let mut instructions = vec![];

        let tmp = make_temporary();

        let else_label = format!("Else.{}", tmp);
        let end_label = format!("End.{}", tmp);

        let condition = emit_tacky(self.condition.clone(), &mut instructions);

        instructions.push(IRInstruction::JumpIfZero {
            condition,
            target: else_label.clone(),
        });

        if let Some(then_branch) = self.then_branch.irfy() {
            instructions.extend::<Vec<IRInstruction>>(then_branch.into());
        }

        instructions.push(IRInstruction::Jump(end_label.clone()));

        instructions.push(IRInstruction::Label(else_label.clone()));

        if self.else_branch.is_some() {
            instructions.extend::<Vec<IRInstruction>>(
                self.else_branch.clone().unwrap().irfy().unwrap().into(),
            );
        }

        instructions.push(IRInstruction::Label(end_label));

        Some(IRNode::Instructions(instructions))
    }
}

impl Irfy for BreakStatement {
    fn irfy(&self) -> Option<IRNode> {
        Some(IRNode::Instructions(vec![IRInstruction::Jump(format!(
            "{}.break",
            self.label.clone()
        ))]))
    }
}

impl Irfy for ContinueStatement {
    fn irfy(&self) -> Option<IRNode> {
        Some(IRNode::Instructions(vec![IRInstruction::Jump(format!(
            "{}.continue",
            self.label.clone()
        ))]))
    }
}

impl Irfy for DoWhileStatement {
    fn irfy(&self) -> Option<IRNode> {
        let mut instructions = vec![];

        let start_label = self.label.clone();
        let continue_label = format!("{}.continue", self.label);
        let break_label = format!("{}.break", self.label);

        instructions.push(IRInstruction::Label(start_label.clone()));

        if let Some(body) = self.body.irfy() {
            instructions.extend::<Vec<IRInstruction>>(body.into());
        }

        instructions.push(IRInstruction::Label(continue_label.clone()));

        let condition = emit_tacky(self.condition.clone(), &mut instructions);

        instructions.push(IRInstruction::JumpIfNotZero {
            condition,
            target: start_label.clone(),
        });

        instructions.push(IRInstruction::Label(break_label.clone()));

        Some(IRNode::Instructions(instructions))
    }
}

impl Irfy for WhileStatement {
    fn irfy(&self) -> Option<IRNode> {
        let mut instructions = vec![];

        let continue_label = format!("{}.continue", self.label);
        let break_label = format!("{}.break", self.label);

        instructions.push(IRInstruction::Label(continue_label.clone()));

        let condition = emit_tacky(self.condition.clone(), &mut instructions);

        instructions.push(IRInstruction::JumpIfZero {
            condition,
            target: break_label.clone(),
        });

        instructions.extend::<Vec<IRInstruction>>(self.body.irfy().unwrap().into());

        instructions.push(IRInstruction::Jump(continue_label.clone()));

        instructions.push(IRInstruction::Label(break_label.clone()));

        Some(IRNode::Instructions(instructions))
    }
}

impl Irfy for ForStatement {
    fn irfy(&self) -> Option<IRNode> {
        let mut instructions = vec![];

        let start_label = format!("{}.start", self.label);
        let break_label = format!("{}.break", self.label);
        let continue_label = format!("{}.continue", self.label);

        instructions.extend::<Vec<IRInstruction>>(self.init.irfy().unwrap().into());

        instructions.push(IRInstruction::Label(start_label.clone()));

        if self.condition.is_some() {
            let condition = emit_tacky(self.condition.clone().unwrap(), &mut instructions);
            instructions.push(IRInstruction::JumpIfZero {
                condition,
                target: break_label.clone(),
            });
        }

        instructions.extend::<Vec<IRInstruction>>(self.body.irfy().unwrap().into());

        instructions.push(IRInstruction::Label(continue_label.clone()));

        if self.post.is_some() {
            emit_tacky(self.post.clone().unwrap(), &mut instructions);
        }

        instructions.push(IRInstruction::Jump(start_label.clone()));

        instructions.push(IRInstruction::Label(break_label.clone()));

        Some(IRNode::Instructions(instructions))
    }
}

impl Irfy for ForInit {
    fn irfy(&self) -> Option<IRNode> {
        match self {
            ForInit::Expression(expr) => {
                let mut instructions = vec![];
                if expr.is_some() {
                    let _ = emit_tacky(expr.clone().unwrap(), &mut instructions);
                }
                Some(IRNode::Instructions(instructions))
            }
            ForInit::Declaration(decl) => decl.irfy(),
        }
    }
}

impl Irfy for ReturnStatement {
    fn irfy(&self) -> Option<IRNode> {
        let mut instructions = vec![];
        let result = emit_tacky(self.expr.clone(), &mut instructions);
        instructions.push(IRInstruction::Ret(result));
        Some(IRNode::Instructions(instructions))
    }
}

impl Irfy for ExpressionStatement {
    fn irfy(&self) -> Option<IRNode> {
        let mut instructions = vec![];
        let _ = emit_tacky(self.expr.clone(), &mut instructions);
        Some(IRNode::Instructions(instructions))
    }
}

impl Irfy for BlockItem {
    fn irfy(&self) -> Option<IRNode> {
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
    fn irfy(&self) -> Option<IRNode> {
        let mut instructions = vec![];

        if self.body.is_none() {
            return None;
        }

        for stmt in self.body.iter() {
            instructions.extend::<Vec<IRInstruction>>(stmt.irfy().unwrap().into());
        }

        instructions.push(IRInstruction::Ret(IRValue::Constant(Const::Int(0))));

        Some(IRNode::Function(IRFunction {
            name: self.name.clone(),
            params: self.params.clone(),
            body: instructions,
            global: self.is_global,
        }))
    }
}

impl Irfy for VariableDeclaration {
    fn irfy(&self) -> Option<IRNode> {
        let mut instructions = vec![];

        if let Some(init) = &self.init {
            let result = emit_tacky(init.clone(), &mut instructions);
            instructions.push(IRInstruction::Copy {
                src: result,
                dst: IRValue::Var(self.name.clone()),
            });
        }

        Some(IRNode::Instructions(instructions))
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
            _ => {
                unreachable!()
            }
        }
    }
}

impl From<IRNode> for Vec<IRInstruction> {
    fn from(node: IRNode) -> Self {
        match node {
            IRNode::Instructions(instrs) => instrs,
            IRNode::StaticVariable(_) => vec![],
            _ => unreachable!(),
        }
    }
}

pub fn convert_symbols_to_tacky() -> Vec<IRNode> {
    let mut tacky_defs = vec![];
    for (name, entry) in SYMBOL_TABLE.lock().unwrap().iter() {
        if let IdentifierAttrs::StaticAttr {
            initial_value,
            global,
        } = entry.attrs
        {
            match initial_value {
                InitialValue::Initial(init) => {
                    tacky_defs.push(IRNode::StaticVariable(IRStaticVariable {
                        name: name.clone(),
                        global,
                        init,
                        _type: entry._type.clone(),
                    }))
                }
                InitialValue::Tentative => {
                    tacky_defs.push(IRNode::StaticVariable(IRStaticVariable {
                        name: name.clone(),
                        _type: entry._type.clone(),
                        global,
                        init: match entry._type {
                            Type::Int => StaticInit::Int(0),
                            Type::Long => StaticInit::Long(0),
                            Type::Ulong => StaticInit::Ulong(0),
                            Type::Uint => StaticInit::Uint(0),
                            Type::Double => StaticInit::Double(0.0),
                            _ => unimplemented!(),
                        },
                    }))
                }
                _ => {}
            }
        }
    }
    tacky_defs
}
