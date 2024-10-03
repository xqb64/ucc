use crate::{
    cfg::{self, BasicBlock, Instr, NodeId, SimpleInstr},
    codegen::tacky_type,
    lexer::Const,
    parser::{
        AddrOfExpression, ArrowExpression, AssignExpression, BinaryExpression,
        BinaryExpressionKind, BlockItem, BlockStatement, BreakStatement, CallExpression,
        CastExpression, ConditionalExpression, ContinueStatement, Declaration, DerefExpression,
        DoWhileStatement, DotExpression, Expression, ExpressionStatement, ForInit, ForStatement,
        FunctionDeclaration, IfStatement, Initializer, ProgramStatement, ReturnStatement,
        SizeofExpression, SizeofTExpression, Statement, StringExpression, SubscriptExpression,
        Type, UnaryExpression, UnaryExpressionKind, VariableDeclaration, WhileStatement,
    },
    typechecker::{
        get_signedness, get_size_of_type, get_type, is_integer_type, is_pointer_type,
        IdentifierAttrs, InitialValue, StaticInit, Symbol, SYMBOL_TABLE, TYPE_TABLE,
    },
};
use anyhow::Result;

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
pub struct IRStaticConstant {
    pub name: String,
    pub _type: Type,
    pub init: StaticInit,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    GetAddress {
        src: IRValue,
        dst: IRValue,
    },
    Load {
        src_ptr: IRValue,
        dst: IRValue,
    },
    Store {
        src: IRValue,
        dst_ptr: IRValue,
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
        dst: Option<IRValue>,
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
    AddPtr {
        ptr: IRValue,
        index: IRValue,
        scale: usize,
        dst: IRValue,
    },
    CopyToOffset {
        src: IRValue,
        dst: String,
        offset: usize,
    },
    CopyFromOffset {
        src: String,
        offset: usize,
        dst: IRValue,
    },
    Ret(Option<IRValue>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IRValue {
    Constant(Const),
    Var(String),
}

#[derive(Debug, Clone, PartialEq, Copy, Eq, Hash)]
pub enum UnaryOp {
    Negate,
    Complement,
    Not,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
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

#[derive(Debug, Clone, PartialEq)]
enum ExpResult {
    PlainOperand(IRValue),
    DereferencedPointer(IRValue),
    SubObject { base: String, offset: usize },
}

fn emit_tacky_and_convert(e: &Expression, instructions: &mut Vec<IRInstruction>) -> IRValue {
    let result = emit_tacky(e, instructions);
    match result {
        ExpResult::SubObject { base, offset } => {
            let dst = make_tacky_variable(get_type(e));
            instructions.push(IRInstruction::CopyFromOffset {
                src: base,
                offset,
                dst: dst.clone(),
            });
            dst
        }
        ExpResult::PlainOperand(val) => val,
        ExpResult::DereferencedPointer(ptr) => {
            let dst = make_tacky_variable(get_type(e));
            instructions.push(IRInstruction::Load {
                src_ptr: ptr,
                dst: dst.clone(),
            });

            dst
        }
    }
}

fn emit_tacky(e: &Expression, instructions: &mut Vec<IRInstruction>) -> ExpResult {
    let t = get_type(e);
    match e {
        Expression::Constant(const_expr) => {
            ExpResult::PlainOperand(IRValue::Constant(const_expr.value))
        }
        Expression::Unary(UnaryExpression { kind, expr, _type }) => {
            let src = emit_tacky_and_convert(expr, instructions);
            let dst = make_tacky_variable(_type);
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

            ExpResult::PlainOperand(dst)
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

                let result = make_tacky_variable(_type);

                let lhs = emit_tacky_and_convert(lhs, instructions);
                instructions.push(IRInstruction::JumpIfZero {
                    condition: lhs.clone(),
                    target: false_label.clone(),
                });

                let rhs = emit_tacky_and_convert(rhs, instructions);
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

                ExpResult::PlainOperand(result)
            }
            BinaryExpressionKind::Or => {
                let tmp = make_temporary();

                let true_label = format!("Or.{}.shortcircuit", tmp);
                let end_label = format!("Or.{}.end", tmp);

                let result = make_tacky_variable(_type);

                let lhs = emit_tacky_and_convert(lhs, instructions);
                instructions.push(IRInstruction::JumpIfNotZero {
                    condition: lhs.clone(),
                    target: true_label.clone(),
                });

                let rhs = emit_tacky_and_convert(rhs, instructions);
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

                ExpResult::PlainOperand(result)
            }
            BinaryExpressionKind::Add => emit_ptr_addition(lhs, rhs, t, instructions),
            BinaryExpressionKind::Sub => {
                let lhs_type = get_type(lhs);
                let rhs_type = get_type(rhs);

                if is_pointer_type(lhs_type) && is_integer_type(rhs_type) {
                    let ptr = emit_tacky_and_convert(lhs, instructions);
                    let index = emit_tacky_and_convert(rhs, instructions);
                    let scale = get_ptr_scale(lhs_type);
                    let dst = make_tacky_variable(lhs_type);

                    let negated_index = make_tacky_variable(rhs_type);

                    instructions.push(IRInstruction::Unary {
                        op: UnaryOp::Negate,
                        src: index.clone(),
                        dst: negated_index.clone(),
                    });

                    instructions.push(IRInstruction::AddPtr {
                        ptr,
                        index: negated_index,
                        scale,
                        dst: dst.clone(),
                    });

                    ExpResult::PlainOperand(dst)
                } else if is_pointer_type(lhs_type) && is_pointer_type(rhs_type) {
                    let ptr1 = emit_tacky_and_convert(lhs, instructions);
                    let ptr2 = emit_tacky_and_convert(rhs, instructions);

                    let ptr_diff: IRValue = make_tacky_variable(lhs_type);

                    let scale = get_ptr_scale(lhs_type);
                    let scale_var = IRValue::Constant(Const::Long(scale as i64));

                    let dst = make_tacky_variable(t);

                    instructions.push(IRInstruction::Binary {
                        op: BinaryOp::Sub,
                        lhs: ptr1,
                        rhs: ptr2,
                        dst: ptr_diff.clone(),
                    });

                    instructions.push(IRInstruction::Binary {
                        op: BinaryOp::Div,
                        lhs: ptr_diff,
                        rhs: scale_var,
                        dst: dst.clone(),
                    });

                    ExpResult::PlainOperand(dst)
                } else if is_pointer_type(lhs_type) && is_pointer_type(rhs_type) {
                    let ptr1 = emit_tacky_and_convert(lhs, instructions);
                    let ptr2 = emit_tacky_and_convert(rhs, instructions);

                    let ptr_diff = make_tacky_variable(&Type::Long);

                    let scale = get_ptr_scale(lhs_type);

                    let dst = make_tacky_variable(_type);

                    instructions.push(IRInstruction::Binary {
                        op: BinaryOp::Sub,
                        lhs: ptr1,
                        rhs: ptr2,
                        dst: ptr_diff.clone(),
                    });

                    instructions.push(IRInstruction::Binary {
                        op: BinaryOp::Div,
                        lhs: ptr_diff,
                        rhs: IRValue::Constant(Const::Long(scale as i64)),
                        dst: dst.clone(),
                    });

                    ExpResult::PlainOperand(dst)
                } else {
                    let lhs = emit_tacky_and_convert(lhs, instructions);
                    let rhs = emit_tacky_and_convert(rhs, instructions);

                    let dst = make_tacky_variable(t);

                    instructions.push(IRInstruction::Binary {
                        op: BinaryOp::Sub,
                        lhs,
                        rhs,
                        dst: dst.clone(),
                    });

                    ExpResult::PlainOperand(dst)
                }
            }
            _ => {
                let lhs = emit_tacky_and_convert(lhs, instructions);
                let rhs = emit_tacky_and_convert(rhs, instructions);

                let dst = make_tacky_variable(t);

                let op = match kind {
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

                ExpResult::PlainOperand(dst)
            }
        },
        Expression::Variable(var) => ExpResult::PlainOperand(IRValue::Var(var.value.to_owned())),
        Expression::Assign(AssignExpression {
            op: _,
            lhs,
            rhs,
            _type,
        }) => emit_assignment(lhs, rhs, instructions),
        Expression::Conditional(ConditionalExpression {
            condition,
            then_expr,
            else_expr,
            _type,
        }) => {
            let tmp = make_temporary();

            let e2_label = format!("Cond.{}.else", tmp);
            let end_label = format!("Cond.{}.end", tmp);
            let condition = emit_tacky_and_convert(condition, instructions);

            instructions.push(IRInstruction::JumpIfZero {
                condition: condition.clone(),
                target: e2_label.clone(),
            });

            if get_type(e) == &Type::Void {
                emit_tacky_and_convert(then_expr, instructions);
                instructions.extend(vec![
                    IRInstruction::Jump(end_label.clone()),
                    IRInstruction::Label(e2_label.clone()),
                ]);
                emit_tacky_and_convert(else_expr, instructions);
                instructions.push(IRInstruction::Label(end_label));
                ExpResult::PlainOperand(IRValue::Var("DUMMY".to_owned()))
            } else {
                let result = make_tacky_variable(_type);

                let e1 = emit_tacky_and_convert(then_expr, instructions);

                instructions.push(IRInstruction::Copy {
                    src: e1,
                    dst: result.clone(),
                });

                instructions.push(IRInstruction::Jump(end_label.clone()));

                instructions.push(IRInstruction::Label(e2_label));

                let e2 = emit_tacky_and_convert(else_expr, instructions);

                instructions.push(IRInstruction::Copy {
                    src: e2,
                    dst: result.clone(),
                });

                instructions.push(IRInstruction::Label(end_label));

                ExpResult::PlainOperand(result)
            }
        }
        Expression::Call(CallExpression { name, args, _type }) => {
            let result = if _type == &Type::Void {
                None
            } else {
                Some(make_tacky_variable(_type))
            };

            let mut arg_values = vec![];

            for arg in args {
                arg_values.push(emit_tacky_and_convert(arg, instructions));
            }

            instructions.push(IRInstruction::Call {
                target: name.to_owned(),
                args: arg_values,
                dst: result.clone(),
            });

            ExpResult::PlainOperand(result.unwrap_or(IRValue::Var("DUMMY".to_owned())))
        }
        Expression::Cast(CastExpression {
            target_type,
            expr,
            _type,
        }) => {
            let result = emit_tacky_and_convert(expr, instructions);
            let inner_type = get_type(expr);

            if target_type == inner_type || target_type == &Type::Void {
                return ExpResult::PlainOperand(result);
            }

            let dst = make_tacky_variable(target_type);

            match (target_type, &inner_type) {
                (Type::Double, _) => {
                    if get_signedness(inner_type) {
                        instructions.push(IRInstruction::IntToDouble {
                            src: result.clone(),
                            dst: dst.clone(),
                        });
                    } else {
                        instructions.push(IRInstruction::UIntToDouble {
                            src: result.clone(),
                            dst: dst.clone(),
                        });
                    }
                }
                (_, Type::Double) => {
                    if get_signedness(target_type) {
                        instructions.push(IRInstruction::DoubleToInt {
                            src: result.clone(),
                            dst: dst.clone(),
                        });
                    } else {
                        instructions.push(IRInstruction::DoubletoUInt {
                            src: result.clone(),
                            dst: dst.clone(),
                        });
                    }
                }
                (_, _) => {
                    if get_size_of_type(target_type) == get_size_of_type(inner_type) {
                        instructions.push(IRInstruction::Copy {
                            src: result.clone(),
                            dst: dst.clone(),
                        });
                    } else if get_size_of_type(target_type) < get_size_of_type(inner_type) {
                        instructions.push(IRInstruction::Truncate {
                            src: result.clone(),
                            dst: dst.clone(),
                        });
                    } else if get_signedness(inner_type) {
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
                }
            }

            ExpResult::PlainOperand(dst)
        }
        Expression::Deref(DerefExpression { expr, _type }) => {
            let ptr = emit_tacky_and_convert(expr, instructions);
            ExpResult::DereferencedPointer(ptr)
        }
        Expression::AddrOf(AddrOfExpression { expr, _type }) => {
            let result = emit_tacky(expr, instructions);

            match result {
                ExpResult::PlainOperand(val) => {
                    let dst = make_tacky_variable(_type);
                    instructions.push(IRInstruction::GetAddress {
                        src: val,
                        dst: dst.clone(),
                    });
                    ExpResult::PlainOperand(dst)
                }
                ExpResult::DereferencedPointer(ptr) => ExpResult::PlainOperand(ptr),
                ExpResult::SubObject { base, offset } => {
                    let dst = make_tacky_variable(get_type(e));
                    instructions.push(IRInstruction::GetAddress {
                        src: IRValue::Var(base.clone()),
                        dst: dst.clone(),
                    });
                    instructions.push(IRInstruction::AddPtr {
                        ptr: dst.clone(),
                        index: IRValue::Constant(Const::Long(offset as i64)),
                        scale: 1,
                        dst: dst.clone(),
                    });
                    ExpResult::PlainOperand(dst)
                }
            }
        }
        Expression::Subscript(SubscriptExpression { expr, index, _type }) => {
            let result = emit_ptr_addition(expr, index, _type, instructions);
            match result {
                ExpResult::PlainOperand(val) => ExpResult::DereferencedPointer(val),
                _ => unreachable!(),
            }
        }
        Expression::Literal(_) => todo!(),
        Expression::String(StringExpression { value, _type }) => {
            let var_name = format!("str.{}", make_temporary());
            let t = Type::Array {
                element: Box::new(Type::Char),
                size: value.len() + 1,
            };
            let symbol = Symbol {
                attrs: IdentifierAttrs::ConstantAttr(StaticInit::String(value.to_owned(), true)),
                _type: t,
            };
            SYMBOL_TABLE
                .lock()
                .unwrap()
                .insert(var_name.clone(), symbol);

            ExpResult::PlainOperand(IRValue::Var(var_name))
        }
        Expression::Sizeof(SizeofExpression { expr, _type }) => ExpResult::PlainOperand(
            IRValue::Constant(Const::ULong(get_size_of_type(get_type(expr)) as u64)),
        ),
        Expression::SizeofT(SizeofTExpression { t, _type }) => {
            ExpResult::PlainOperand(IRValue::Constant(Const::ULong(get_size_of_type(t) as u64)))
        }
        Expression::Dot(DotExpression {
            structure,
            member,
            _type,
        }) => {
            let struct_tag = match get_type(structure) {
                Type::Struct { tag } => tag,
                _ => unreachable!(),
            };
            let struct_def = TYPE_TABLE.lock().unwrap().get(struct_tag).cloned().unwrap();
            let member_offset = struct_def
                .members
                .iter()
                .find(|m| m.name == *member)
                .unwrap()
                .offset;

            let inner_object = emit_tacky(structure, instructions);

            match inner_object {
                ExpResult::PlainOperand(IRValue::Var(v)) => {
                    return ExpResult::SubObject {
                        base: v,
                        offset: member_offset,
                    }
                }
                ExpResult::SubObject { base, offset } => {
                    return ExpResult::SubObject {
                        base,
                        offset: offset + member_offset,
                    }
                }
                ExpResult::DereferencedPointer(ptr) => {
                    let type_of_e = get_type(e);
                    let dst_ptr = make_tacky_variable(&Type::Pointer(type_of_e.to_owned().into()));
                    let instr = IRInstruction::AddPtr {
                        ptr,
                        index: IRValue::Constant(Const::Long(member_offset as i64)),
                        scale: 1,
                        dst: dst_ptr.clone(),
                    };
                    instructions.push(instr);
                    return ExpResult::DereferencedPointer(dst_ptr);
                }
                _ => unreachable!(),
            }
        }
        Expression::Arrow(ArrowExpression {
            pointer,
            member,
            _type,
        }) => {
            let struct_tag = match get_type(pointer) {
                Type::Pointer(referenced) => match &**referenced {
                    Type::Struct { tag } => tag,
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            };
            let struct_def = TYPE_TABLE.lock().unwrap().get(struct_tag).cloned().unwrap();
            let member_offset = struct_def
                .members
                .iter()
                .find(|m| m.name == *member)
                .unwrap()
                .offset;

            let ptr = emit_tacky_and_convert(pointer, instructions);

            if member_offset == 0 {
                return ExpResult::DereferencedPointer(ptr);
            }

            let dst = make_tacky_variable(&Type::Pointer(_type.to_owned().into()));

            let index = IRValue::Constant(Const::Long(member_offset as i64));

            let add_ptr_instr = IRInstruction::AddPtr {
                ptr,
                index,
                scale: 1,
                dst: dst.clone(),
            };

            instructions.push(add_ptr_instr);

            ExpResult::DereferencedPointer(dst)
        }
    }
}

fn emit_assignment(
    lhs: &Expression,
    rhs: &Expression,
    instructions: &mut Vec<IRInstruction>,
) -> ExpResult {
    let lval = emit_tacky(lhs, instructions);
    let rval = emit_tacky_and_convert(rhs, instructions);

    match &lval {
        ExpResult::SubObject { base, offset } => {
            instructions.push(IRInstruction::CopyToOffset {
                src: rval.clone(),
                dst: base.to_owned(),
                offset: *offset,
            });
            ExpResult::PlainOperand(rval)
        }
        ExpResult::PlainOperand(val) => {
            instructions.push(IRInstruction::Copy {
                src: rval,
                dst: val.to_owned(),
            });
            lval
        }
        ExpResult::DereferencedPointer(ptr) => {
            instructions.push(IRInstruction::Store {
                src: rval.clone(),
                dst_ptr: ptr.to_owned(),
            });
            ExpResult::PlainOperand(rval)
        }
    }
}

fn emit_string_init(dst: String, offset: usize, s: &[u8]) -> Vec<IRInstruction> {
    let len = s.len();

    if len == 0 {
        vec![]
    } else if len >= 8 {
        let l = i64::from_le_bytes(s[0..8].try_into().unwrap());
        let instr = IRInstruction::CopyToOffset {
            src: IRValue::Constant(Const::Long(l)),
            dst: dst.clone(),
            offset,
        };
        let rest = &s[8..];
        let mut instructions = vec![instr];
        instructions.extend(emit_string_init(dst, offset + 8, rest));
        return instructions;
    } else if len >= 4 {
        let i = i32::from_le_bytes(s[0..4].try_into().unwrap());
        let instr = IRInstruction::CopyToOffset {
            src: IRValue::Constant(Const::Int(i)),
            dst: dst.clone(),
            offset,
        };
        let rest = &s[4..];
        let mut instructions = vec![instr];
        instructions.extend(emit_string_init(dst, offset + 4, rest));
        return instructions;
    } else {
        let c = s[0] as i8; // Convert u8 to i8
        let instr = IRInstruction::CopyToOffset {
            src: IRValue::Constant(Const::Char(c)),
            dst: dst.clone(),
            offset,
        };
        let rest = &s[1..];
        let mut instructions = vec![instr];
        instructions.extend(emit_string_init(dst, offset + 1, rest));
        return instructions;
    }
}

fn emit_compound_init(
    name: &str,
    value: &Initializer,
    instructions: &mut Vec<IRInstruction>,
    offset: usize,
    inited_type: &Type,
) {
    match value {
        Initializer::Single(_, Expression::String(string_expr)) => {
            if let Type::Array { element: _, size } = inited_type {
                let str_bytes = string_expr.value.as_bytes();
                let padding_sz = size.saturating_sub(str_bytes.len());

                let mut combined_bytes = vec![];
                combined_bytes.extend_from_slice(str_bytes);

                for _ in 0..padding_sz {
                    combined_bytes.push(0);
                }

                instructions.extend(emit_string_init(name.to_owned(), offset, &combined_bytes));
            }
        }
        Initializer::Single(_, single_init) => {
            let v = emit_tacky_and_convert(single_init, instructions);
            instructions.push(IRInstruction::CopyToOffset {
                src: v,
                dst: name.to_string(),
                offset,
            });
        }
        Initializer::Compound(_, _type, compound_init) => {
            if let Type::Array { element, size: _ } = inited_type {
                for (idx, elem_init) in compound_init.iter().enumerate() {
                    let new_offset = offset + idx * get_size_of_type(_type);
                    emit_compound_init(name, elem_init, instructions, new_offset, element);
                }
            } else if let Type::Struct { tag } = inited_type {
                let members = TYPE_TABLE.lock().unwrap().get(tag).unwrap().members.clone();

                for (member, mem_init) in members.iter().zip(compound_init) {
                    let mem_offset = offset + member.offset;
                    emit_compound_init(name, mem_init, instructions, mem_offset, &member._type);
                }
            }
        }
    }
}

fn emit_ptr_addition(
    lhs: &Expression,
    rhs: &Expression,
    t: &Type,
    instructions: &mut Vec<IRInstruction>,
) -> ExpResult {
    let lhs_type = get_type(lhs);
    let rhs_type = get_type(rhs);

    if is_pointer_type(lhs_type) && is_integer_type(rhs_type) {
        let ptr = emit_tacky_and_convert(lhs, instructions);
        let index = emit_tacky_and_convert(rhs, instructions);
        let scale = get_ptr_scale(lhs_type);
        let dst = make_tacky_variable(lhs_type);
        instructions.push(IRInstruction::AddPtr {
            ptr,
            index,
            scale,
            dst: dst.clone(),
        });
        ExpResult::PlainOperand(dst)
    } else if is_integer_type(lhs_type) && is_pointer_type(rhs_type) {
        let ptr = emit_tacky_and_convert(rhs, instructions);
        let index = emit_tacky_and_convert(lhs, instructions);
        let scale = get_ptr_scale(rhs_type);
        let dst = make_tacky_variable(rhs_type);
        instructions.push(IRInstruction::AddPtr {
            ptr,
            index,
            scale,
            dst: dst.clone(),
        });
        ExpResult::PlainOperand(dst)
    } else {
        let lhs = emit_tacky_and_convert(lhs, instructions);
        let rhs = emit_tacky_and_convert(rhs, instructions);

        let dst = make_tacky_variable(t);

        instructions.push(IRInstruction::Binary {
            op: BinaryOp::Add,
            lhs,
            rhs,
            dst: dst.clone(),
        });

        ExpResult::PlainOperand(dst)
    }
}

pub fn make_tacky_variable(t: &Type) -> IRValue {
    let var_name = format!("var.{}", make_temporary());
    let symbol = Symbol {
        attrs: IdentifierAttrs::LocalAttr,
        _type: t.to_owned(),
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

fn get_ptr_scale(t: &Type) -> usize {
    match t {
        Type::Pointer(referenced) => get_size_of_type(referenced),
        _ => unimplemented!(),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IRNode {
    Program(IRProgram),
    Function(IRFunction),
    Instructions(Vec<IRInstruction>),
    Value(IRValue),
    StaticVariable(IRStaticVariable),
    StaticConstant(IRStaticConstant),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IRStaticVariable {
    pub name: String,
    pub _type: Type,
    pub init: Vec<StaticInit>,
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
                    _ => {}
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
                    Declaration::Struct(_) => {}
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

        // Generate code for the condition
        let condition = emit_tacky_and_convert(&self.condition, &mut instructions);

        // If there is no else clause
        if self.else_branch.is_none() {
            // Emit JumpIfZero to end_label (because there is no else clause)
            instructions.push(IRInstruction::JumpIfZero {
                condition,
                target: end_label.clone(),
            });

            // Emit the then clause
            if let Some(then_branch) = self.then_branch.irfy() {
                instructions.extend::<Vec<IRInstruction>>(then_branch.into());
            }

            // End the if statement with the end label
            instructions.push(IRInstruction::Label(end_label));
        } else {
            // If there is an else clause
            // Emit JumpIfZero to else_label
            instructions.push(IRInstruction::JumpIfZero {
                condition,
                target: else_label.clone(),
            });

            // Emit the then clause
            if let Some(then_branch) = self.then_branch.irfy() {
                instructions.extend::<Vec<IRInstruction>>(then_branch.into());
            }

            // Unconditional jump to the end label after then clause
            instructions.push(IRInstruction::Jump(end_label.clone()));

            // Now handle the else clause
            instructions.push(IRInstruction::Label(else_label.clone()));

            // Emit the else clause
            if let Some(else_branch) = &*self.else_branch {
                if let Some(else_instrs) = else_branch.irfy() {
                    instructions.extend::<Vec<IRInstruction>>(else_instrs.into());
                }
            }

            // End the if-else structure with the end label
            instructions.push(IRInstruction::Label(end_label));
        }

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

        let condition = emit_tacky_and_convert(&self.condition, &mut instructions);

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

        let condition = emit_tacky_and_convert(&self.condition, &mut instructions);

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
            let condition =
                emit_tacky_and_convert(self.condition.as_ref().unwrap(), &mut instructions);
            instructions.push(IRInstruction::JumpIfZero {
                condition,
                target: break_label.clone(),
            });
        }

        instructions.extend::<Vec<IRInstruction>>(self.body.irfy().unwrap().into());

        instructions.push(IRInstruction::Label(continue_label.clone()));

        if self.post.is_some() {
            emit_tacky(self.post.as_ref().unwrap(), &mut instructions);
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
                    let _ = emit_tacky(expr.as_ref().unwrap(), &mut instructions);
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
        let result = optionally_emit_tacky_and_convert(&self.expr, &mut instructions);
        instructions.push(IRInstruction::Ret(result));
        Some(IRNode::Instructions(instructions))
    }
}

impl Irfy for ExpressionStatement {
    fn irfy(&self) -> Option<IRNode> {
        let mut instructions = vec![];
        let _ = emit_tacky(&self.expr, &mut instructions);
        Some(IRNode::Instructions(instructions))
    }
}

impl Irfy for BlockItem {
    fn irfy(&self) -> Option<IRNode> {
        match self {
            BlockItem::Declaration(decl) => match decl {
                Declaration::Function(func) => func.irfy(),
                Declaration::Variable(var) => var.irfy(),
                _ => todo!(),
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

        instructions.push(IRInstruction::Ret(Some(IRValue::Constant(Const::Int(0)))));

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

        if let Some(Initializer::Single(_, Expression::String(_))) = &self.init {
            emit_compound_init(
                &self.name,
                self.init.as_ref().unwrap(),
                &mut instructions,
                0,
                &self._type,
            );
        } else if let Some(Initializer::Single(_, init)) = &self.init {
            let result = emit_tacky_and_convert(init, &mut instructions);
            instructions.push(IRInstruction::Copy {
                src: result,
                dst: IRValue::Var(self.name.clone()),
            });
        } else if let Some(Initializer::Compound(_, _type, _)) = &self.init {
            emit_compound_init(
                &self.name,
                self.init.as_ref().unwrap(),
                &mut instructions,
                0,
                &self._type,
            );
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
        } = entry.attrs.clone()
        {
            match initial_value {
                InitialValue::Initial(init) => {
                    tacky_defs.push(IRNode::StaticVariable(IRStaticVariable {
                        name: name.clone(),
                        global,
                        init: init.clone(),
                        _type: entry._type.clone(),
                    }))
                }
                InitialValue::Tentative => {
                    tacky_defs.push(IRNode::StaticVariable(IRStaticVariable {
                        name: name.clone(),
                        _type: entry._type.clone(),
                        global,
                        init: vec![match &entry._type {
                            Type::Int => StaticInit::Int(0),
                            Type::Long => StaticInit::Long(0),
                            Type::Ulong => StaticInit::ULong(0),
                            Type::Uint => StaticInit::UInt(0),
                            Type::Double => StaticInit::Double(0.0),
                            Type::Pointer(_) => StaticInit::ULong(0),
                            Type::Array { element, size } => {
                                StaticInit::Zero(get_size_of_type(element) * size)
                            }
                            Type::Struct { tag } => {
                                StaticInit::Zero(TYPE_TABLE.lock().unwrap().get(tag).unwrap().size)
                            }
                            _ => unimplemented!(),
                        }],
                    }))
                }
                _ => continue,
            }
        }
        if let IdentifierAttrs::ConstantAttr(init) = entry.attrs.clone() {
            tacky_defs.push(IRNode::StaticConstant(IRStaticConstant {
                name: name.clone(),
                init,
                _type: entry._type.clone(),
            }))
        }
    }
    tacky_defs
}

fn optionally_emit_tacky_and_convert(
    e: &Option<Expression>,
    instructions: &mut Vec<IRInstruction>,
) -> Option<IRValue> {
    e.as_ref().map(|e| emit_tacky_and_convert(e, instructions))
}

#[derive(Debug, Clone, PartialEq)]
pub enum Optimization {
    ConstantFolding,
    UnreachableCodeElimination,
    CopyPropagation,
    DeadStoreElimination,
}

pub trait Optimize {
    fn optimize(&mut self, enabled_optimizations: Vec<Optimization>) -> Self;
}

impl Optimize for IRProgram {
    fn optimize(&mut self, enabled_optimizations: Vec<Optimization>) -> Self {
        let mut functions = vec![];
        for func in &mut self.functions {
            functions.push(match func {
                IRNode::Function(f) => IRNode::Function(f.optimize(enabled_optimizations.clone())),
                _ => unreachable!(),
            });
        }
        IRProgram {
            functions,
            static_vars: self.static_vars.clone(),
        }
    }
}

fn address_taken_analysis(instrs: &[IRInstruction]) -> HashSet<String> {
    let addr_taken = |instr: &IRInstruction| -> Option<String> {
        match instr {
            IRInstruction::GetAddress { src, dst: _ } => {
                if let IRValue::Var(v) = src {
                    Some(v.clone())
                } else {
                    None
                }
            },
            _ => None,
        }
    };

    instrs.iter().filter_map(addr_taken).collect()
}

impl Instr for IRInstruction {
    fn simplify(&self) -> SimpleInstr {
        match self {
            IRInstruction::Label(lbl) => SimpleInstr::Label(lbl.clone()),
            IRInstruction::JumpIfZero { target, .. } | IRInstruction::JumpIfNotZero { target, .. } => SimpleInstr::ConditionalJump(target.clone()),
            IRInstruction::Jump(target) => SimpleInstr::UnconditionalJump(target.clone()),
            IRInstruction::Ret(_) => SimpleInstr::Return,
            _ => SimpleInstr::Other,
        }
    }

    fn pp_instr(&self) -> String {
        match self {
            IRInstruction::Label(lbl) => format!("Label({})", lbl),
            IRInstruction::JumpIfZero { target, .. } | IRInstruction::JumpIfNotZero { target, .. } => format!("ConditionalJump({})", target),
            IRInstruction::Jump(target) => format!("UnconditionalJump({})", target),
            IRInstruction::Ret(val) => format!("Return {:?}", val),
            _ => format!("{:?}", self),
        }
    }

    fn is_jump(&self) -> bool {
        match self {
            IRInstruction::Jump(_) | IRInstruction::JumpIfZero {..} | IRInstruction::JumpIfNotZero { .. } => true,
            _ => false,
        }
    }

    fn is_label(&self) -> bool {
        match self {
            IRInstruction::Label(_) => true,
            _ => false,
        }
    }
}

impl Optimize for IRFunction {
    fn optimize(&mut self, enabled_optimizations: Vec<Optimization>) -> Self {
        if self.body.is_empty() {
            return self.clone();
        }

        // let all_static_vars = find_all_static_variables();

        loop {
            let mut aliased_vars = address_taken_analysis(&self.body);

            let post_constant_folding;
            if enabled_optimizations.contains(&Optimization::ConstantFolding) {
                post_constant_folding = constant_folding(&self.body);
            } else {
                post_constant_folding = self.body.clone();
            }

            let mut cfg: cfg::CFG<(), IRInstruction> = cfg::CFG::<(), IRInstruction>::instructions_to_cfg(
                "spam".to_string(),
                post_constant_folding,
            );

            // cfg.print_as_graphviz();
            
            // First optimization: Unreachable code elimination
            if enabled_optimizations.contains(&Optimization::UnreachableCodeElimination) {
                cfg = unreachable_code_elimination(&mut cfg).to_owned();
            }

            // cfg.print_as_graphviz();
            
            // Reannotate the cfg with ReachingCopies for copy propagation
            if enabled_optimizations.contains(&Optimization::CopyPropagation) {
                cfg = copy_propagation::<(), IRInstruction>(&mut aliased_vars, cfg);  // Call copy propagation
            }

            // cfg.print_as_graphviz();
            
            // if enabled_optimizations.contains(&Optimization::DeadStoreElimination) {
            //     cfg = dead_store_elimination(&mut cfg, &all_static_vars);
            // }

            let optimized_function_body = cfg.cfg_to_instructions();

            if optimized_function_body == self.body || optimized_function_body.is_empty() {
                return IRFunction {
                    name: self.name.clone(),
                    params: self.params.clone(),
                    body: optimized_function_body,
                    global: self.global,
                };
            }

            self.body = optimized_function_body;
        }
    }
}

fn constant_folding(instructions: &Vec<IRInstruction>) -> Vec<IRInstruction> {
    let mut optimized_instructions = vec![];

    for instr in instructions {
        match instr {
            IRInstruction::Binary { op, lhs, rhs, dst } => {
                let lhs_val = get_constant_value(lhs);
                let rhs_val = get_constant_value(rhs);

                if let (Some(lhs_val), Some(rhs_val)) = (lhs_val, rhs_val) {
                    let result = match op {
                        BinaryOp::Add => lhs_val + rhs_val,
                        BinaryOp::Sub => lhs_val - rhs_val,
                        BinaryOp::Mul => lhs_val * rhs_val,
                        BinaryOp::Div => lhs_val / rhs_val,
                        BinaryOp::Rem => lhs_val % rhs_val,
                        BinaryOp::Less => match lhs_val < rhs_val {
                            true => Const::Int(1),
                            false => Const::Int(0),
                        },
                        BinaryOp::Greater => match lhs_val > rhs_val {
                            true => Const::Int(1),
                            false => Const::Int(0),
                        },
                        BinaryOp::Equal => match lhs_val == rhs_val {
                            true => Const::Int(1),
                            false => Const::Int(0),
                        },
                        BinaryOp::NotEqual => match lhs_val != rhs_val {
                            true => Const::Int(1),
                            false => Const::Int(0),
                        },
                        BinaryOp::GreaterEqual => match lhs_val >= rhs_val {
                            true => Const::Int(1),
                            false => Const::Int(0),
                        },
                        BinaryOp::LessEqual => match lhs_val <= rhs_val {
                            true => Const::Int(1),
                            false => Const::Int(0),
                        },
                    };
                    optimized_instructions.push(IRInstruction::Copy {
                        src: IRValue::Constant(result),
                        dst: dst.clone(),
                    });
                } else {
                    optimized_instructions.push(instr.clone());
                }
            }
            IRInstruction::Unary { op, src, dst } => {
                let src_val = get_constant_value(src);

                if let Some(src_val) = src_val {
                    let result = match op {
                        UnaryOp::Negate => -src_val,
                        UnaryOp::Complement => match src_val {
                            Const::Int(_) | Const::Long(_) | Const::UInt(_) | Const::ULong(_) => {
                                !src_val
                            }
                            _ => panic!("Complement requires an integer type"),
                        },
                        UnaryOp::Not => match src_val == Const::Int(0) {
                            true => Const::Int(1),
                            false => Const::Int(0),
                        },
                    };
                    optimized_instructions.push(IRInstruction::Copy {
                        src: IRValue::Constant(result),
                        dst: dst.clone(),
                    });
                } else {
                    optimized_instructions.push(instr.clone());
                }
            }
            IRInstruction::JumpIfNotZero { condition, target } => {
                let condition_val = get_constant_value(condition);

                if let Some(condition_val) = condition_val {
                    if !is_zero(&condition_val) {
                        optimized_instructions.push(IRInstruction::Jump(target.clone()));
                    }
                } else {
                    optimized_instructions.push(instr.clone());
                }
            }
            IRInstruction::JumpIfZero { condition, target } => {
                let condition_val = get_constant_value(condition);

                if let Some(condition_val) = condition_val {
                    if is_zero(&condition_val) {
                        optimized_instructions.push(IRInstruction::Jump(target.clone()));
                    }
                } else {
                    optimized_instructions.push(instr.clone());
                }
            }
            IRInstruction::Truncate {
                src: IRValue::Constant(konst),
                dst,
            } => {
                optimized_instructions.extend(evaluate_cast(konst, dst));
            }
            IRInstruction::SignExtend {
                src: IRValue::Constant(konst),
                dst,
            } => {
                optimized_instructions.extend(evaluate_cast(konst, dst));
            }
            IRInstruction::ZeroExtend {
                src: IRValue::Constant(konst),
                dst,
            } => {
                optimized_instructions.extend(evaluate_cast(konst, dst));
            }
            IRInstruction::DoubleToInt {
                src: IRValue::Constant(konst),
                dst,
            } => {
                optimized_instructions.extend(evaluate_cast(konst, dst));
            }
            IRInstruction::DoubletoUInt {
                src: IRValue::Constant(konst),
                dst,
            } => {
                optimized_instructions.extend(evaluate_cast(konst, dst));
            }
            IRInstruction::IntToDouble {
                src: IRValue::Constant(konst),
                dst,
            } => {
                optimized_instructions.extend(evaluate_cast(konst, dst));
            }
            IRInstruction::UIntToDouble {
                src: IRValue::Constant(konst),
                dst,
            } => {
                optimized_instructions.extend(evaluate_cast(konst, dst));
            }
            IRInstruction::Copy {
                src: IRValue::Constant(konst),
                dst,
            } => {
                optimized_instructions.extend(evaluate_cast(konst, dst));
            }
            _ => {
                optimized_instructions.push(instr.clone());
            }
        }
    }

    optimized_instructions
}

fn get_constant_value(value: &IRValue) -> Option<Const> {
    match value {
        IRValue::Constant(konst) => Some(konst.to_owned()),
        _ => None,
    }
}

fn evaluate_cast(konst: &Const, dst: &IRValue) -> Vec<IRInstruction> {
    use crate::codegen::tacky_type;

    let dst_type = tacky_type(dst);
    let converted_src = match const_convert(konst, &dst_type) {
        Ok(result) => result,
        Err(_) => const_convert(&Const::Int(0), &dst_type).unwrap(),
    };

    vec![IRInstruction::Copy {
        src: IRValue::Constant(converted_src),
        dst: dst.clone(),
    }]
}

fn const_convert(konst: &Const, dst_type: &Type) -> Result<Const> {
    match dst_type {
        Type::Int => match konst {
            Const::Int(val) => Ok(Const::Int(*val)),
            Const::Long(val) => Ok(Const::Int(*val as i32)),
            Const::UInt(val) => Ok(Const::Int(*val as i32)),
            Const::ULong(val) => Ok(Const::Int(*val as i32)),
            Const::Double(val) => Ok(Const::Int(*val as i32)),
            Const::Char(val) => Ok(Const::Int(*val as i32)),
            Const::UChar(val) => Ok(Const::Int(*val as i32)),
        },
        Type::Long => match konst {
            Const::Int(val) => Ok(Const::Long(*val as i64)),
            Const::Long(val) => Ok(Const::Long(*val)),
            Const::UInt(val) => Ok(Const::Long(*val as i64)),
            Const::ULong(val) => Ok(Const::Long(*val as i64)),
            Const::Double(val) => Ok(Const::Long(*val as i64)),
            Const::Char(val) => Ok(Const::Long(*val as i64)),
            Const::UChar(val) => Ok(Const::Long(*val as i64)),
        },
        Type::Uint => match konst {
            Const::Int(val) => Ok(Const::UInt(*val as u32)),
            Const::Long(val) => Ok(Const::UInt(*val as u32)),
            Const::UInt(val) => Ok(Const::UInt(*val)),
            Const::ULong(val) => Ok(Const::UInt(*val as u32)),
            Const::Double(val) => Ok(Const::UInt(*val as u32)),
            Const::Char(val) => Ok(Const::UInt(*val as u32)),
            Const::UChar(val) => Ok(Const::UInt(*val as u32)),
        },
        Type::Ulong => match konst {
            Const::Int(val) => Ok(Const::ULong(*val as u64)),
            Const::Long(val) => Ok(Const::ULong(*val as u64)),
            Const::UInt(val) => Ok(Const::ULong(*val as u64)),
            Const::ULong(val) => Ok(Const::ULong(*val)),
            Const::Double(val) => Ok(Const::ULong(*val as u64)),
            Const::Char(val) => Ok(Const::ULong(*val as u64)),
            Const::UChar(val) => Ok(Const::ULong(*val as u64)),
        },
        Type::Double => match konst {
            Const::Int(val) => Ok(Const::Double(*val as f64)),
            Const::Long(val) => Ok(Const::Double(*val as f64)),
            Const::UInt(val) => Ok(Const::Double(*val as f64)),
            Const::ULong(val) => Ok(Const::Double(*val as f64)),
            Const::Double(val) => Ok(Const::Double(*val)),
            Const::Char(val) => Ok(Const::Double(*val as f64)),
            Const::UChar(val) => Ok(Const::Double(*val as f64)),
        },
        Type::Char => match konst {
            Const::Int(val) => Ok(Const::Char(*val as i8)),
            Const::Long(val) => Ok(Const::Char(*val as i8)),
            Const::UInt(val) => Ok(Const::Char(*val as i8)),
            Const::ULong(val) => Ok(Const::Char(*val as i8)),
            Const::Double(val) => Ok(Const::Char(*val as i8)),
            Const::Char(val) => Ok(Const::Char(*val)),
            Const::UChar(val) => Ok(Const::Char(*val as i8)),
        },
        Type::UChar => match konst {
            Const::Int(val) => Ok(Const::UChar(*val as u8)),
            Const::Long(val) => Ok(Const::UChar(*val as u8)),
            Const::UInt(val) => Ok(Const::UChar(*val as u8)),
            Const::ULong(val) => Ok(Const::UChar(*val as u8)),
            Const::Char(val) => Ok(Const::UChar(*val as u8)),
            Const::UChar(val) => Ok(Const::UChar(*val)),
            Const::Double(val) => Ok(Const::UChar(*val as u8)),
        },
        Type::SChar => match konst {
            Const::Int(val) => Ok(Const::Char(*val as i8)),
            Const::Long(val) => Ok(Const::Char(*val as i8)),
            Const::UInt(val) => Ok(Const::Char(*val as i8)),
            Const::ULong(val) => Ok(Const::Char(*val as i8)),
            Const::Char(val) => Ok(Const::Char(*val as i8)),
            Const::UChar(val) => Ok(Const::Char(*val as i8)),
            Const::Double(val) => Ok(Const::Char(*val as i8)),
        },
        Type::Pointer(_) => match konst {
            Const::Int(val) => Ok(Const::ULong(*val as u64)),
            Const::Long(val) => Ok(Const::ULong(*val as u64)),
            Const::UInt(val) => Ok(Const::ULong(*val as u64)),
            Const::ULong(val) => Ok(Const::ULong(*val as u64)),
            Const::Double(val) => Ok(Const::ULong(*val as u64)),
            Const::Char(val) => Ok(Const::ULong(*val as u64)),
            Const::UChar(val) => Ok(Const::ULong(*val as u64)),
        },
        _ => unreachable!(),
    }
}

impl std::ops::Add for Const {
    type Output = Const;

    fn add(self, rhs: Const) -> Self::Output {
        match (self, rhs) {
            (Const::Int(lhs), Const::Int(rhs)) => Const::Int(lhs + rhs),
            (Const::Long(lhs), Const::Long(rhs)) => Const::Long(lhs + rhs),
            (Const::UInt(lhs), Const::UInt(rhs)) => Const::UInt(lhs + rhs),
            (Const::ULong(lhs), Const::ULong(rhs)) => Const::ULong(lhs + rhs),
            (Const::Double(lhs), Const::Double(rhs)) => Const::Double(lhs + rhs),
            (Const::Char(lhs), Const::Char(rhs)) => Const::Char(lhs + rhs),
            (Const::UChar(lhs), Const::UChar(rhs)) => Const::UChar(lhs + rhs),
            _ => unreachable!(),
        }
    }
}

impl std::ops::Sub for Const {
    type Output = Const;

    fn sub(self, rhs: Const) -> Self::Output {
        match (self, rhs) {
            (Const::Int(lhs), Const::Int(rhs)) => Const::Int(lhs - rhs),
            (Const::Long(lhs), Const::Long(rhs)) => Const::Long(lhs - rhs),
            (Const::UInt(lhs), Const::UInt(rhs)) => Const::UInt(lhs - rhs),
            (Const::ULong(lhs), Const::ULong(rhs)) => Const::ULong(lhs - rhs),
            (Const::Double(lhs), Const::Double(rhs)) => Const::Double(lhs - rhs),
            (Const::Char(lhs), Const::Char(rhs)) => Const::Char(lhs - rhs),
            (Const::UChar(lhs), Const::UChar(rhs)) => Const::UChar(lhs - rhs),
            _ => unreachable!(),
        }
    }
}

impl std::ops::Mul for Const {
    type Output = Const;

    fn mul(self, rhs: Const) -> Self::Output {
        match (self, rhs) {
            (Const::Int(lhs), Const::Int(rhs)) => Const::Int(lhs * rhs),
            (Const::Long(lhs), Const::Long(rhs)) => Const::Long(lhs * rhs),
            (Const::UInt(lhs), Const::UInt(rhs)) => Const::UInt(lhs * rhs),
            (Const::ULong(lhs), Const::ULong(rhs)) => Const::ULong(lhs * rhs),
            (Const::Double(lhs), Const::Double(rhs)) => Const::Double(lhs * rhs),
            (Const::Char(lhs), Const::Char(rhs)) => Const::Char(lhs * rhs),
            (Const::UChar(lhs), Const::UChar(rhs)) => Const::UChar(lhs * rhs),
            _ => unreachable!(),
        }
    }
}

impl std::ops::Div for Const {
    type Output = Const;

    fn div(self, rhs: Const) -> Self::Output {
        match (self, rhs) {
            (Const::Int(lhs), Const::Int(rhs)) => Const::Int(lhs.checked_div(rhs).unwrap_or(0)),
            (Const::Long(lhs), Const::Long(rhs)) => Const::Long(lhs.checked_div(rhs).unwrap_or(0)),
            (Const::UInt(lhs), Const::UInt(rhs)) => Const::UInt(lhs.checked_div(rhs).unwrap_or(0)),
            (Const::ULong(lhs), Const::ULong(rhs)) => {
                Const::ULong(lhs.checked_div(rhs).unwrap_or(0))
            }
            (Const::Char(lhs), Const::Char(rhs)) => Const::Char(lhs.checked_div(rhs).unwrap_or(0)),
            (Const::UChar(lhs), Const::UChar(rhs)) => {
                Const::UChar(lhs.checked_div(rhs).unwrap_or(0))
            }
            (Const::Double(lhs), Const::Double(rhs)) => Const::Double(lhs / rhs),
            _ => {
                unreachable!()
            }
        }
    }
}

impl std::ops::Rem for Const {
    type Output = Const;

    fn rem(self, rhs: Const) -> Self::Output {
        match (self, rhs) {
            (Const::Int(lhs), Const::Int(rhs)) => Const::Int(lhs.checked_rem(rhs).unwrap_or(0)),
            (Const::Long(lhs), Const::Long(rhs)) => Const::Long(lhs.checked_rem(rhs).unwrap_or(0)),
            (Const::UInt(lhs), Const::UInt(rhs)) => Const::UInt(lhs.checked_rem(rhs).unwrap_or(0)),
            (Const::ULong(lhs), Const::ULong(rhs)) => {
                Const::ULong(lhs.checked_rem(rhs).unwrap_or(0))
            }
            (Const::Char(lhs), Const::Char(rhs)) => Const::Char(lhs.checked_rem(rhs).unwrap_or(0)),
            (Const::UChar(lhs), Const::UChar(rhs)) => {
                Const::UChar(lhs.checked_rem(rhs).unwrap_or(0))
            }
            _ => unreachable!(),
        }
    }
}

impl std::ops::Neg for Const {
    type Output = Const;

    fn neg(self) -> Self::Output {
        match self {
            Const::Int(val) => Const::Int(-val),
            Const::Long(val) => Const::Long(-val),
            Const::UInt(val) => Const::Int(-(val as i32)),
            Const::ULong(val) => Const::Long(-(val as i64)),
            Const::Double(val) => Const::Double(-val),
            _ => unreachable!(),
        }
    }
}

impl std::ops::Not for Const {
    type Output = Const;

    fn not(self) -> Self::Output {
        match self {
            Const::Int(val) => Const::Int(!val),
            Const::Long(val) => Const::Long(!val),
            Const::UInt(val) => Const::UInt(!val),
            Const::ULong(val) => Const::ULong(!val),
            _ => unreachable!(),
        }
    }
}

use std::fmt::Debug;

fn unreachable_code_elimination<V: Clone + Debug, I: Debug  + Instr + Clone>(cfg: &mut cfg::CFG<V, I>) -> &mut cfg::CFG<V, I> {
    remove_empty_blocks(eliminate_useless_labels(eliminate_useless_jumps(eliminate_unreachable_blocks(cfg))))
}

fn is_zero(konst: &Const) -> bool {
    match konst {
        Const::Int(val) => *val == 0,
        Const::Long(val) => *val == 0,
        Const::UInt(val) => *val == 0,
        Const::ULong(val) => *val == 0,
        Const::Double(val) => *val == 0.0,
        Const::Char(val) => *val == 0,
        Const::UChar(val) => *val == 0,
    }
}

use std::collections::HashSet;

// Module aliases and utility sets
type NodeSet = HashSet<NodeId>;

// DFS to find reachable blocks
pub fn eliminate_unreachable_blocks<V: Clone + Debug, I: Clone + Debug + Instr>(cfg: &mut cfg::CFG<V, I>) -> &mut cfg::CFG<V, I> {
    fn dfs<V: Clone + Debug, I: Clone + Debug + Instr>(cfg: &cfg::CFG<V, I>, explored: &mut NodeSet, node_id: NodeId) {
        if explored.contains(&node_id) {
            return;
        }
        
        explored.insert(node_id.clone());

        let succs = cfg.get_succs(&node_id);
        for succ in succs {
            dfs(cfg, explored, succ.clone());
        }
    }

    let mut reachable_block_ids = HashSet::new();
    dfs(cfg, &mut reachable_block_ids, NodeId::Entry);

    let mut edges_to_remove = vec![];
    let mut blocks_to_remove = vec![];

    // Filter out unreachable blocks
    let updated_blocks: Vec<(usize, BasicBlock<V, I>)> = cfg.basic_blocks.iter()
    .filter(|(_, blk)| {
        if reachable_block_ids.contains(&blk.id) {
            true
        } else {
            // Collect edges to remove
            for pred in &blk.preds {
                edges_to_remove.push((pred.clone(), blk.id.clone()));
            }
            for succ in &blk.succs {
                edges_to_remove.push((blk.id.clone(), succ.clone()));
            }
            blocks_to_remove.push(blk.id.clone());
            false
        }
    })
    .cloned()
    .collect();

    // // Now remove the edges
    // for (pred, succ) in edges_to_remove {
    //     cfg.remove_edge(pred, succ);
    // }

    // dbg!(&blocks_to_remove);

    for block in blocks_to_remove {
        cfg.remove_block(block);
    }    

    // dbg!(&cfg);

    // dbg!(&updated_blocks);

    // cfg.basic_blocks = updated_blocks;

    cfg
}

// Eliminate useless jump instructions
pub fn eliminate_useless_jumps<V: Clone + Debug, I: Clone + Debug + Instr>(cfg: &mut cfg::CFG<V, I>) -> &mut cfg::CFG<V, I> {
    fn drop_last<T>(vec: &mut Vec<T>) {
        vec.pop();
    }

    let updated_blocks: Vec<(usize, BasicBlock<V, I>)> = cfg.basic_blocks.iter()
        .enumerate()
        .map(|(idx, (n, blk))| {
            if idx == cfg.basic_blocks.len() - 1 {
                // Do not modify the last block
                (n.clone(), blk.clone())
            } else {
                match blk.instructions.last() {
                    Some((_, instr)) if instr.is_jump() => {
                        let (_, default_succ) = &cfg.basic_blocks[idx + 1];
                        if blk.succs.iter().all(|succ| succ == &default_succ.id) {
                            // Useless jump, drop the last instruction
                            let mut new_blk = blk.clone();
                            drop_last(&mut new_blk.instructions);
                            (n.clone(), new_blk)
                        } else {
                            (n.clone(), blk.clone())
                        }
                    }
                    _ => (n.clone(), blk.clone())
                }
            }
        })
        .collect();
    
    cfg.basic_blocks = updated_blocks;

    cfg
}

// Eliminate useless label instructions
pub fn eliminate_useless_labels<V: Clone + Debug, I: Clone + Debug + Instr>(cfg: &mut cfg::CFG<V, I>) -> &mut cfg::CFG<V, I> {
    let updated_blocks: Vec<(usize, BasicBlock<V, I>)> = cfg.basic_blocks.iter()
        .enumerate()
        .map(|(idx, (n, blk))| {
            if let Some((_, instr)) = blk.instructions.first() {
                if instr.is_label() {
                    let default_pred = if idx == 0 {
                        NodeId::Entry
                    } else {
                        cfg.basic_blocks[idx - 1].1.id.clone()
                    };

                    if blk.preds.iter().all(|pred| pred == &default_pred) {
                        // Remove the label
                        let mut new_blk = blk.clone();
                        new_blk.instructions.remove(0);
                        return (n.clone(), new_blk);
                    }
                }
            }
            (n.clone(), blk.clone())
        })
        .collect();

    cfg.basic_blocks = updated_blocks;

    cfg
}

pub fn remove_empty_blocks<V: Clone + Debug, I: Clone + Debug + Instr>(cfg: &mut cfg::CFG<V, I>) -> &mut cfg::CFG<V, I> {
    println!("Removing empty blocks");

    let mut blocks_to_remove = Vec::new();

    // Collect empty blocks and their associated edges for removal and addition
    let updated_blocks: Vec<(usize, BasicBlock<V, I>)> = cfg.basic_blocks.iter()
        .filter(|(_, blk)| {
            if blk.instructions.is_empty() {
                blocks_to_remove.push(blk.id.clone());
                false // Mark this block for removal
            } else {
                true // Keep this block
            }
        })
        .cloned()
        .collect();


    for block in blocks_to_remove {
        cfg.remove_block(block);
    }    
    // Return the updated CFG with filtered basic blocks
    // cfg.basic_blocks = updated_blocks;

    cfg
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Cp {
    src: IRValue,
    dst: IRValue,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReachingCopies(HashSet<Cp>);

impl ReachingCopies {
    pub fn new() -> Self {
        ReachingCopies(HashSet::new())
    }

    pub fn intersection(&self, other: &ReachingCopies) -> ReachingCopies {
        let new_set = self.0.intersection(&other.0).cloned().collect();
        ReachingCopies(new_set)
    }

    pub fn union(&self, other: &ReachingCopies) -> ReachingCopies {
        let mut new_set = self.0.clone();
        new_set.extend(other.0.iter().cloned());
        ReachingCopies(new_set)
    }

    pub fn add(&mut self, copy: Cp) {
        self.0.insert(copy);
    }

    pub fn mem(&self, copy: &Cp) -> bool {
        self.0.contains(copy)
    }

    pub fn elements(&self) -> Vec<&Cp> {
        self.0.iter().collect()
    }

    pub fn filter<F>(&self, f: F) -> Self
    where
        F: Fn(&Cp) -> bool,
    {
        ReachingCopies(self.0.iter().filter(|&cp| f(&cp)).cloned().collect())
    }
}

fn same_type(v1: &IRValue, v2: &IRValue) -> bool {
    let t1 = tacky_type(v1);
    let t2 = tacky_type(v2);
    t1 == t2 || get_signedness(&t1) == get_signedness(&t2)
}

fn is_static(var: &str) -> bool {
    match SYMBOL_TABLE.lock().unwrap().get(var).unwrap().attrs {
        IdentifierAttrs::StaticAttr { .. } => true,
        _ => false,
    }
}

fn var_is_aliased(aliased_vars: &HashSet<String>, v: &IRValue) -> bool {
    match v {
        IRValue::Constant(_) => false,
        IRValue::Var(var) => {
            aliased_vars.contains(var) || is_static(var)
        }
    }
}

fn filter_updated(copies: &ReachingCopies, updated: &IRValue) -> ReachingCopies {
    match updated {
        IRValue::Var(_) => {
            let is_killed = |cp: &Cp| cp.src == *updated || cp.dst == *updated;
            copies.filter(|cp| !is_killed(cp))
        }
        IRValue::Constant(_) => copies.clone(),
    }
}

fn get_dst(instr: &IRInstruction) -> Option<IRValue> {
    match instr {
        IRInstruction::Copy { dst, .. } => Some(dst.clone()),
        IRInstruction::Unary { dst, .. } => Some(dst.clone()),
        IRInstruction::Binary { dst, .. } => Some(dst.clone()),
        IRInstruction::Call { dst, .. } => dst.clone(),
        IRInstruction::SignExtend { dst, .. } => Some(dst.clone()),
        IRInstruction::ZeroExtend { dst, .. } => Some(dst.clone()),
        IRInstruction::Truncate { dst, .. } => Some(dst.clone()),
        IRInstruction::IntToDouble { dst, .. } => Some(dst.clone()),
        IRInstruction::DoubleToInt { dst, .. } => Some(dst.clone()),
        IRInstruction::DoubletoUInt { dst, .. } => Some(dst.clone()),
        IRInstruction::UIntToDouble { dst, .. } => Some(dst.clone()),
        IRInstruction::CopyFromOffset { dst, .. } => Some(dst.clone()),
        IRInstruction::CopyToOffset { dst, .. } => Some(IRValue::Var(dst.to_owned())),
        IRInstruction::GetAddress { dst, .. } => Some(dst.clone()),
        IRInstruction::Load { dst, .. } => Some(dst.clone()),
        IRInstruction::AddPtr { dst, .. } => Some(dst.clone()),
        IRInstruction::Store { .. } => None,
        _ => None,
    }
}

fn transfer(
    aliased_vars: &HashSet<String>,
    block: &BasicBlock<ReachingCopies, IRInstruction>,
    initial_reaching_copies: ReachingCopies,
) -> BasicBlock<ReachingCopies, IRInstruction> {
    let is_aliased = |v: &IRValue| var_is_aliased(aliased_vars, v);

    let mut current_copies = initial_reaching_copies.clone();
    let mut annotated_instructions = Vec::new();

    for instr in &block.instructions {
        // Annotate the instruction with the current copies before processing
        let annotated_instr = (current_copies.clone(), instr.1.clone());
        annotated_instructions.push(annotated_instr);

        // Process the instruction to update current_copies
        current_copies = match instr.1 {
            IRInstruction::Copy { ref src, ref dst } => {
                if same_type(src, dst) {
                    let mut updated_copies = filter_updated(&current_copies, dst);
                    updated_copies.add(Cp { src: src.clone(), dst: dst.clone() });
                    updated_copies
                } else {
                    filter_updated(&current_copies, dst)
                }
            }
            IRInstruction::Call { ref dst, .. } => {
                let filtered = match dst {
                    Some(d) => filter_updated(&current_copies, d),
                    None => current_copies.clone(),
                };

                ReachingCopies(
                    filtered.0
                        .iter()
                        .filter(|cp| !(is_aliased(&cp.src) || is_aliased(&cp.dst)))
                        .cloned()
                        .collect(),
                )
            }
            IRInstruction::Store { .. } => ReachingCopies(
                current_copies
                    .0
                    .iter()
                    .filter(|cp| !(is_aliased(&cp.src) || is_aliased(&cp.dst)))
                    .cloned()
                    .collect(),
            ),
            _ => {
                if let Some(dst) = get_dst(&instr.1) {
                    filter_updated(&current_copies, &dst)
                } else {
                    current_copies.clone()
                }
            }
        };
    }

    BasicBlock {
        instructions: annotated_instructions,
        value: current_copies,
        preds: block.preds.clone(),
        succs: block.succs.clone(),
        id: block.id.clone(),
    }
}

fn meet(
    ident: &ReachingCopies,
    cfg: &cfg::CFG<ReachingCopies, IRInstruction>,
    block: &BasicBlock<ReachingCopies, IRInstruction>
) -> ReachingCopies {
    let mut incoming = ident.clone();
    for pred in &block.preds {
        match pred {
            NodeId::Entry => {
                incoming = incoming.intersection(&ReachingCopies::new());
            }
            NodeId::Block(n) => {
                let v = cfg.get_block_value(*n);
                incoming = incoming.intersection(&v);
            }
            _ => panic!("Internal error"),
        }
    }
    incoming
}

fn find_reaching_copies<V: Clone + Debug, I: Clone + Debug + Instr>(
    aliased_vars: &HashSet<String>,
    cfg: cfg::CFG<(), IRInstruction>,
) -> cfg::CFG<ReachingCopies, IRInstruction> {
    let ident = collect_all_copies(&cfg);
    let mut starting_cfg = cfg.initialize_annotation(ident.clone());

    let mut worklist: Vec<(usize, BasicBlock<ReachingCopies, IRInstruction>)> = starting_cfg.basic_blocks.clone();

    while let Some((block_idx, blk)) = worklist.pop() {
        let old_annotation = blk.value.clone();
        let incoming_copies = meet(&ident, &starting_cfg, &blk);
        let block = transfer(aliased_vars, &blk, incoming_copies);
        
        starting_cfg.update_basic_block(block_idx, block);

        let new_annotation = starting_cfg.get_block_value(block_idx);
        if &old_annotation != new_annotation {
            let block_successors = starting_cfg.get_succs(&blk.id);

            for succ in block_successors {
                match succ {
                    NodeId::Block(_) => {
                        let s = starting_cfg.get_block_by_id(&succ);
                        if !worklist.contains(&s) {
                            worklist.push(s.clone());
                        }
                    }
                    NodeId::Exit => continue,
                    _ => panic!("Internal error"),
                }
            }
        }
    }

    starting_cfg
}

fn copy_propagation<V: Clone + Debug, I: Clone + Debug + Instr>(aliased_vars: &HashSet<String>, cfg: cfg::CFG<(), IRInstruction>) -> cfg::CFG<(), IRInstruction> {
    let annotated_cfg = find_reaching_copies::<(), IRInstruction>(aliased_vars, cfg);

    let rewrite_block = |(idx, block): (usize, BasicBlock<ReachingCopies, IRInstruction>)| {
        let new_instructions = block
            .instructions
            .iter()
            .filter_map(|(reaching_copies, instr)| rewrite_instruction(reaching_copies, instr).map(|new_instr| (reaching_copies.clone(), new_instr)))
            .collect::<Vec<_>>();

        BasicBlock {
            instructions: new_instructions,
            ..block.clone()
        }
    };

    let transformed_cfg = cfg::CFG {
        basic_blocks: annotated_cfg
            .basic_blocks
            .iter()
            .map(|(idx, blk)| (*idx, rewrite_block((*idx, blk.clone()))))
            .collect(),
        entry_succs: annotated_cfg.entry_succs.clone(),
        exit_preds: annotated_cfg.exit_preds.clone(),
        debug_label: annotated_cfg.debug_label.clone(),
    };
        
    transformed_cfg.strip_annotations()
}

fn rewrite_instruction(
    reaching_copies: &ReachingCopies,
    instr: &IRInstruction,
) -> Option<IRInstruction> {
    let replace = |op: &IRValue| -> IRValue {
        match op {
            IRValue::Constant(_) => op.clone(),
            IRValue::Var(_) => {
                // Try to find a copy where dst matches op
                reaching_copies
                    .elements()
                    .iter()
                    .find(|cp| cp.dst == op.clone())
                    .map(|cp| cp.src.clone())
                    .unwrap_or(op.clone())
            }
        }
    };

    // Filter out useless copy instructions
    match instr {
        IRInstruction::Copy { src, dst } => {
            for copy in reaching_copies.elements().iter() {
                let c = IRInstruction::Copy { src: copy.src.clone(), dst: copy.dst.clone() };
                if let IRInstruction::Copy { src: copy_src, dst: copy_dst } = &c {
                    if &c == instr || (copy_src == dst && copy_dst == src) {
                        return None;
                    }
                }
            }
            Some(IRInstruction::Copy { src: replace(src), dst: dst.clone() })
        }
        // Handle other instruction rewrites, replacing operands
        IRInstruction::Unary { op, src, dst } => Some(IRInstruction::Unary { src: replace(src), op: *op, dst: dst.clone() }),
        IRInstruction::Binary { op, lhs, rhs, dst } => Some(IRInstruction::Binary {
            lhs: replace(lhs),
            rhs: replace(rhs),
            op: *op, 
            dst: dst.clone(),
        }),
        IRInstruction::Ret(v) => Some(IRInstruction::Ret(v.as_ref().map(|v| replace(v)))),
        IRInstruction::JumpIfZero { condition, target } => Some(IRInstruction::JumpIfZero { condition: replace(condition), target: target.clone() }),
        IRInstruction::JumpIfNotZero { condition, target } => Some(IRInstruction::JumpIfNotZero { condition: replace(condition), target: target.clone() }),
        IRInstruction::SignExtend { src, dst } => Some(IRInstruction::SignExtend { src: replace(src), dst: dst.clone() }),
        IRInstruction::ZeroExtend { src, dst } => Some(IRInstruction::ZeroExtend { src: replace(src), dst: dst.clone() }),
        IRInstruction::Truncate { src, dst } => Some(IRInstruction::Truncate { src: replace(src), dst: dst.clone() }),
        IRInstruction::IntToDouble { src, dst } => Some(IRInstruction::IntToDouble { src: replace(src), dst: dst.clone() }),
        IRInstruction::DoubleToInt { src, dst } => Some(IRInstruction::DoubleToInt { src: replace(src), dst: dst.clone() }),
        IRInstruction::DoubletoUInt { src, dst } => Some(IRInstruction::DoubletoUInt { src: replace(src), dst: dst.clone() }),
        IRInstruction::UIntToDouble { src, dst } => Some(IRInstruction::UIntToDouble { src: replace(src), dst: dst.clone() }),
        IRInstruction::CopyToOffset { src, dst, offset } => Some(IRInstruction::CopyToOffset { src: replace(src), dst: dst.clone(), offset: offset.clone() }),
        IRInstruction::CopyFromOffset { src, dst, offset } => {
            match replace(&IRValue::Var(src.to_owned())) {
                IRValue::Var(new_src) => Some(IRInstruction::CopyFromOffset { src: new_src, dst: dst.clone(), offset: offset.clone() }),
                _ => None,
            }
        }
        IRInstruction::AddPtr { ptr, index, scale, dst } => Some(IRInstruction::AddPtr { ptr: replace(ptr), index: replace(index), scale: *scale, dst: dst.clone() }),
        IRInstruction::Load { src_ptr, dst } => Some(IRInstruction::Load { src_ptr: replace(src_ptr), dst: dst.clone() }),
        IRInstruction::Store { src, dst_ptr } => Some(IRInstruction::Store { src: replace(src), dst_ptr: dst_ptr.clone() }),
        _ => Some(instr.clone()),
    }
}

fn collect_all_copies(cfg: &cfg::CFG<(), IRInstruction>) -> ReachingCopies {
    let mut copies = ReachingCopies::new();

    for (_, block) in &cfg.basic_blocks {
        for (_, instr) in &block.instructions {
            if let IRInstruction::Copy { src, dst } = instr {
                if same_type(&src, &dst) {
                    copies.add(Cp { src: src.clone(), dst: dst.clone() });
                }
            }
        }
    }

    copies
}