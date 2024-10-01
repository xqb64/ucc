use std::{collections::{HashMap, HashSet}, default};

use crate::{
    cfg::{self, instructions_to_cfg, pretty_print_graph_as_graphviz, remove_edge, Node, NodeId}, codegen::tacky_type, lexer::Const, parser::{
        AddrOfExpression, ArrowExpression, AssignExpression, BinaryExpression,
        BinaryExpressionKind, BlockItem, BlockStatement, BreakStatement, CallExpression,
        CastExpression, ConditionalExpression, ContinueStatement, Declaration, DerefExpression,
        DoWhileStatement, DotExpression, Expression, ExpressionStatement, ForInit, ForStatement,
        FunctionDeclaration, IfStatement, Initializer, ProgramStatement, ReturnStatement,
        SizeofExpression, SizeofTExpression, Statement, StringExpression, SubscriptExpression,
        Type, UnaryExpression, UnaryExpressionKind, VariableDeclaration, WhileStatement,
    }, typechecker::{
        get_signedness, get_size_of_type, get_type, is_integer_type, is_pointer_type,
        IdentifierAttrs, InitialValue, StaticInit, Symbol, SYMBOL_TABLE, TYPE_TABLE,
    }
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

impl Optimize for IRFunction {
    fn optimize(&mut self, enabled_optimizations: Vec<Optimization>) -> Self {
        if self.body.is_empty() {
            return self.clone();
        }

        loop {
            let post_constant_folding;
            if enabled_optimizations.contains(&Optimization::ConstantFolding) {
                post_constant_folding = constant_folding(&self.body);
            } else {
                post_constant_folding = self.body.clone();
            }

            let mut cfg = instructions_to_cfg(&post_constant_folding);
            pretty_print_graph_as_graphviz(&cfg);

            if enabled_optimizations.contains(&Optimization::UnreachableCodeElimination) {
                cfg = unreachable_code_elimination(&mut cfg).to_vec();
            }

            pretty_print_graph_as_graphviz(&cfg);

            if enabled_optimizations.contains(&Optimization::CopyPropagation) {
                cfg = copy_propagation(&mut cfg);
            }

            // if enabled_optimizations.contains(&Optimization::DeadStoreElimination) {
            //     cfg = dead_store_elimination(&cfg);
            // }

            let optimized_function_body = control_flow_graph_to_ir(&mut cfg);

            if optimized_function_body == self.body || optimized_function_body.is_empty() {
                println!("optimized_function_body: {:?}", optimized_function_body);
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

fn unreachable_code_elimination(cfg: &mut Vec<Node>) -> &mut Vec<Node> {
    eliminate_empty_blocks(eliminate_useless_labels(eliminate_useless_jumps(
        eliminate_unreachable_blocks(cfg),
    )))
}

fn take_entry_node(cfg: &mut Vec<Node>) -> Node {
    // find the Entry node in 'cfg' and remove it from the list and return it
    let entry_node = cfg
        .iter_mut()
        .find(|node| match node {
            Node::Entry { .. } => true,
            _ => false,
        })
        .take()
        .unwrap()
        .clone();

    cfg.retain(|node| match node {
        Node::Entry { .. } => false,
        _ => true,
    });

    entry_node
}

fn take_exit_node(cfg: &mut Vec<Node>) -> Node {
    let exit_node = cfg
        .iter_mut()
        .find(|node| match node {
            Node::Exit { .. } => true,
            _ => false,
        })
        .take()
        .unwrap()
        .clone();

    cfg.retain(|node| match node {
        Node::Exit { .. } => false,
        _ => true,
    });

    exit_node
}

pub fn eliminate_empty_blocks(cfg: &mut Vec<Node>) -> &mut Vec<Node> {
    use crate::cfg::{add_edge, remove_edge};

    let removed_entry = take_entry_node(cfg);
    let removed_exit = take_exit_node(cfg);

    let mut changes = Vec::new();
    let mut nodes_to_remove = Vec::new();

    for (index, node) in cfg.iter().enumerate() {
        if let Node::Block {
            id,
            instructions,
            predecessors,
            successors,
            ..
        } = node
        {
            if instructions.is_empty() {
                match (predecessors.as_slice(), successors.as_slice()) {
                    ([pred], [succ]) => {
                        changes.push((pred.clone(), id.clone(), succ.clone()));
                        nodes_to_remove.push(index); // Mark this node for removal
                    }
                    _ => {
                        panic!("Empty block should have exactly one predecessor and one successor");
                    }
                }
            }
        }
    }

    for (pred, id, succ) in changes {
        remove_edge(pred.clone(), id.clone(), cfg);
        remove_edge(id.clone(), succ.clone(), cfg);
        add_edge(pred.clone(), succ.clone(), cfg);
    }

    nodes_to_remove.reverse();
    for index in nodes_to_remove {
        cfg.remove(index);
    }

    cfg.insert(0, removed_entry);
    cfg.push(removed_exit);

    cfg
}

pub fn eliminate_useless_labels(cfg: &mut Vec<Node>) -> &mut Vec<Node> {
    let sorted_blocks = sort_basic_blocks(cfg);

    let removed_entry = take_entry_node(sorted_blocks);
    let removed_exit = take_exit_node(sorted_blocks);

    let copy = sorted_blocks.clone();

    let mut i = 0;
    while i < sorted_blocks.len() {
        let block = sorted_blocks.get_mut(i).unwrap();
        if let Node::Block {
            ref mut instructions,
            predecessors,
            ..
        } = block
        {
            let first_instruction = instructions.first();
            match first_instruction {
                Some(IRInstruction::Label(_)) => {
                    let mut keep_jump = false;
                    let default_pred = if i == 0 {
                        removed_entry.clone()
                    } else {
                        copy.get(i - 1).cloned().unwrap()
                    };

                    for pred in predecessors {
                        if *pred
                            != match default_pred {
                                Node::Block { ref id, .. } => id.to_owned(),
                                Node::Entry { .. } => NodeId::Entry,
                                Node::Exit { .. } => NodeId::Exit,
                            }
                        {
                            keep_jump = true;
                            break;
                        }
                    }

                    if !keep_jump {
                        fn is_label(instr: &IRInstruction) -> bool {
                            match instr {
                                IRInstruction::Label(_) => true,
                                _ => false,
                            }
                        }

                        let removed = instructions.remove(0);
                        assert!(is_label(&removed));
                    }
                }
                _ => {}
            }
        }

        i += 1;
    }

    sorted_blocks.insert(0, removed_entry);
    sorted_blocks.push(removed_exit);

    sorted_blocks
}

pub fn eliminate_useless_jumps(cfg: &mut Vec<Node>) -> &mut Vec<Node> {
    let sorted_blocks = sort_basic_blocks(cfg);

    let removed_entry = take_entry_node(sorted_blocks);
    let removed_exit = take_exit_node(sorted_blocks);

    let copy = sorted_blocks.clone();

    let mut i = 0;
    while i < sorted_blocks.len() - 1 {
        let block = sorted_blocks.get_mut(i).unwrap();
        let b = block.clone();
        if let Node::Block {
            ref mut instructions,
            successors,
            ..
        } = block
        {
            match instructions.last() {
                Some(IRInstruction::Jump(_))
                | Some(IRInstruction::JumpIfZero { .. })
                | Some(IRInstruction::JumpIfNotZero { .. }) => {
                    let mut keep_jump = false;
                    let default_succ = copy.get(i + 1).unwrap();

                    for succ in successors {
                        if *succ
                            != match default_succ {
                                Node::Block { ref id, .. } => id.to_owned(),
                                Node::Entry { .. } => NodeId::Entry,
                                Node::Exit { .. } => NodeId::Exit,
                            }
                        {
                            keep_jump = true;
                            break;
                        }
                    }

                    if !keep_jump {
                        fn is_jump(instr: &Option<IRInstruction>) -> bool {
                            match instr {
                                Some(instr) => match instr {
                                    IRInstruction::Jump(_)
                                    | IRInstruction::JumpIfZero { .. }
                                    | IRInstruction::JumpIfNotZero { .. } => true,
                                    _ => false,
                                },
                                _ => false,
                            }
                        }

                        let popped = instructions.pop();
                        assert!(is_jump(&popped));
                    }
                }
                _ => {}
            }
        }
        i += 1;
    }

    sorted_blocks.insert(0, removed_entry);
    sorted_blocks.push(removed_exit);

    sorted_blocks
}

fn sort_basic_blocks(cfg: &mut Vec<Node>) -> &mut Vec<Node> {
    cfg.sort_by_key(|node| match node {
        Node::Block { id, .. } => id.clone(),
        Node::Entry { .. } => NodeId::Entry,
        Node::Exit { .. } => NodeId::Exit,
    });

    cfg
}

pub fn eliminate_unreachable_blocks(cfg: &mut Vec<Node>) -> &mut Vec<Node> {
    let mut visited = HashSet::new();

    fn dfs(graph: &[Node], node_id: NodeId, visited: &mut HashSet<NodeId>) {
        if visited.contains(&node_id) {
            return;
        }

        visited.insert(node_id.clone());

        if let Some(node) = graph.iter().find(|n| match n {
            Node::Entry { .. } => node_id == NodeId::Entry,
            Node::Exit { .. } => node_id == NodeId::Exit,
            Node::Block { id, .. } => id == &node_id,
        }) {
            match node {
                Node::Entry { successors } => {
                    for successor in successors {
                        dfs(graph, successor.clone(), visited);
                    }
                }
                Node::Block { successors, .. } => {
                    for successor in successors {
                        dfs(graph, successor.clone(), visited);
                    }
                }
                Node::Exit { .. } => {}
            }
        }
    }

    dfs(cfg, NodeId::Entry, &mut visited);

    // Collect nodes to remove
    let nodes_to_remove: Vec<usize> = cfg
        .iter()
        .enumerate()
        .filter_map(|(index, blk)| {
            if visited.contains(match blk {
                Node::Block { id, .. } => id,
                Node::Entry { .. } => &NodeId::Entry,
                Node::Exit { .. } => &NodeId::Exit,
            }) {
                None // Keep this node
            } else {
                Some(index) // Mark this node for removal
            }
        })
        .collect();

    let mut edges_to_remove = Vec::new();

    for &index in &nodes_to_remove {
        match &cfg[index] {
            Node::Block {
                id,
                successors,
                predecessors,
                ..
            } => {
                for pred in predecessors {
                    edges_to_remove.push((pred.clone(), id.clone()));
                }
                for succ in successors {
                    edges_to_remove.push((id.clone(), succ.clone()));
                }
            }
            Node::Entry { successors } => {
                for succ in successors {
                    edges_to_remove.push((NodeId::Entry, succ.clone()));
                }
            }
            Node::Exit { predecessors } => {
                for pred in predecessors {
                    edges_to_remove.push((pred.clone(), NodeId::Exit));
                }
            }
        }
    }

    for (src, dest) in edges_to_remove {
        remove_edge(src, dest, cfg);
    }

    for &index in nodes_to_remove.iter().rev() {
        if let Node::Exit { .. } = &cfg[index] {
            continue;
        }
        cfg.remove(index);
    }

    cfg
}

fn dead_store_elimination(instructions: &Vec<Node>) -> Vec<Node> {
    instructions.to_owned()
}

fn control_flow_graph_to_ir(cfg: &mut Vec<Node>) -> Vec<IRInstruction> {
    let mut instructions = vec![];

    for node in cfg {
        match node {
            Node::Block {
                instructions: instrs,
                ..
            } => {
                instructions.extend(instrs.clone());
            }
            _ => {}
        }
    }

    instructions
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

fn get_constant_value(value: &IRValue) -> Option<Const> {
    match value {
        IRValue::Constant(konst) => Some(konst.to_owned()),
        _ => None,
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

fn copy_propagation(cfg: &mut Vec<Node>) -> Vec<Node> {
    let mut annotated_instructions = HashMap::new();
    let mut annotated_blocks = HashMap::new();

    // Perform reaching copies analysis
    find_reaching_copies(cfg, &mut annotated_blocks, &mut annotated_instructions);

    // Iterate over each block and rewrite instructions based on reaching copies
    let mut new_cfg = vec![];
    for block in cfg.iter() {
        let mut new_block = block.clone();
        let instructions = get_block_instructions(block);

        let mut new_instructions = vec![];
        for instr in instructions {
            if let Some(rewritten_instr) = rewrite_instruction(&instr, id2num(&get_block_id(block)), &mut annotated_instructions) {
                new_instructions.push(rewritten_instr);
            }
        }

        // Update the block with new instructions
        match new_block {
            Node::Block { ref mut instructions, .. } => {
                *instructions = new_instructions;
            }
            _ => {}
        }

        new_cfg.push(new_block);
    }

    new_cfg
}

fn find_reaching_copies(cfg: &mut Vec<Node>, annotated_blocks: &mut HashMap<NodeId, Vec<IRInstruction>>, annotated_instructions: &mut HashMap<(usize, IRInstruction), Vec<IRInstruction>>) {
    let all_copies = find_all_copies(cfg);
    let mut worklist = vec![];

    for node in cfg.iter() {
        match node {
            Node::Entry { .. } | Node::Exit { .. } => continue,
            Node::Block { .. } => {
                worklist.push(node.clone());
                println!("inserting block id {} with copies {:?}", get_block_id(node), all_copies.clone());
                annotated_blocks.insert(get_block_id(node), all_copies.clone());
            }
        }
    }

    while !worklist.is_empty() {
        let block = worklist.remove(0);
        let old_annotation = annotated_blocks.get(&get_block_id(&block)).cloned().unwrap();

        let incoming_copies = meet(&block, &all_copies, annotated_blocks);

        transfer(&block, &incoming_copies, annotated_instructions, annotated_blocks);

        if old_annotation != annotated_blocks.get(&get_block_id(&block)).cloned().unwrap() {
            let block_successors = get_block_successors(&block);
            for succ in block_successors.iter() {
                match succ {
                    NodeId::Exit => continue,
                    NodeId::Entry => panic!("WAAAAAAAAAAAAAA"),
                    NodeId::BlockId(_) => {
                        let successor = get_block_by_id(cfg, succ);
                        if !worklist.contains(&successor) {
                            worklist.push(successor.clone());
                        }
                    }
                }
            }
        }
    }
}

fn meet(block: &Node, all_copies: &[IRInstruction], annotated_blocks: &mut HashMap<NodeId, Vec<IRInstruction>>) -> Vec<IRInstruction> {
    let mut incoming_copies = all_copies.to_vec();
    let block_predecessors = get_block_predecessors(block);

    for pred in block_predecessors.iter() {
        match pred {
            NodeId::Entry => return vec![],
            NodeId::Exit => panic!("AAAA"),
            NodeId::BlockId(_) => {
                let empty_vec = vec![];
                let pred_out_copies = annotated_blocks.get(pred).unwrap_or(&empty_vec);
                incoming_copies = incoming_copies
                        .iter()
                        .filter(|copy| pred_out_copies.contains(copy))
                        .cloned()
                        .collect();
            }
        }
    }

    incoming_copies
}

fn id2num(id: &NodeId) -> usize {
    match id {
        NodeId::BlockId(num) => *num,
        _ => panic!("Expected block id"),
    }
}

fn transfer(
    block: &Node,
    initial_reaching_copies: &[IRInstruction],
    annotated_instructions: &mut HashMap<(usize, IRInstruction), Vec<IRInstruction>>,
    annotated_blocks: &mut HashMap<NodeId, Vec<IRInstruction>>,
) {
    let mut current_reaching_copies = initial_reaching_copies.to_vec();

    let block_instructions = get_block_instructions(block);

    for instruction in block_instructions.iter() {
        annotated_instructions.insert((id2num(&get_block_id(block)), instruction.clone()), current_reaching_copies.clone());

        match instruction {
            IRInstruction::Copy { src: _, dst } => {
                if current_reaching_copies.contains(&instruction) {
                    continue;
                }

                let mut to_remove = vec![];
                for (idx, copy) in current_reaching_copies.iter().enumerate() {
                    if let IRInstruction::Copy { src: copy_src, dst: copy_dst } = copy {
                        if copy_src == dst || copy_dst == dst {
                            to_remove.push(idx);
                        }
                    }
                }

                for idx in to_remove.iter().rev() {
                    current_reaching_copies.remove(*idx);
                }

                current_reaching_copies.push(instruction.clone());
            }

            IRInstruction::Call { target: _, args: _, dst } => {
                let mut to_remove = vec![];
                for (idx, copy) in current_reaching_copies.iter().enumerate() {
                    if let IRInstruction::Copy { src: copy_src, dst: copy_dst } = copy {
                        if &Some(copy_src.to_owned()) == dst || &Some(copy_dst.to_owned()) == dst || is_static(copy_src) || is_static(copy_dst) {
                            to_remove.push(idx);
                        }
                    }
                }

                for idx in to_remove.iter().rev() {
                    current_reaching_copies.remove(*idx);
                }
            }

            IRInstruction::Unary { op: _, src: _, dst } => {
                let mut to_remove = vec![];
                for (idx, copy) in current_reaching_copies.iter().enumerate() {
                    if let IRInstruction::Copy { src: copy_src, dst: copy_dst } = copy {
                        if copy_src == dst || copy_dst == dst {
                            to_remove.push(idx);
                        }
                    }
                }

                for idx in to_remove.iter().rev() {
                    current_reaching_copies.remove(*idx);
                }
            }

            IRInstruction::Binary { op: _, lhs: _, rhs: _, dst } => {
                let mut to_remove = vec![];
                for (idx, copy) in current_reaching_copies.iter().enumerate() {
                    if let IRInstruction::Copy { src: copy_src, dst: copy_dst } = copy {
                        if copy_src == dst || copy_dst == dst {
                            to_remove.push(idx);
                        }
                    }
                }

                for idx in to_remove.iter().rev() {
                    current_reaching_copies.remove(*idx);
                }
            }

            _ => {
                match get_dst(instruction) {
                    Some(dst) => filter_updated(&mut current_reaching_copies, dst),
                    None => {}
                }
            }
        }
    }

    println!("inserting block id {} with copies {:?}", get_block_id(block), current_reaching_copies);
    annotated_blocks.insert(get_block_id(block), current_reaching_copies);
}

fn filter_updated(reaching_copies: &mut Vec<IRInstruction>, dst: IRValue) {
    let mut to_remove = vec![];
    for (idx, copy) in reaching_copies.iter().enumerate() {
        if let IRInstruction::Copy { src: copy_src, dst: copy_dst } = copy {
            if copy_src == &dst || copy_dst == &dst {
                to_remove.push(idx);
            }
        }
    }

    for idx in to_remove.iter().rev() {
        reaching_copies.remove(*idx);
    }
}

fn get_dst(instr: &IRInstruction) -> Option<IRValue> {
    match instr {
        IRInstruction::Copy { src: _, dst } => Some(dst.clone()),
        IRInstruction::Unary { op: _, src: _, dst } => Some(dst.clone()),
        IRInstruction::Binary { op: _, lhs: _, rhs: _, dst } => Some(dst.clone()),
        IRInstruction::Call { target: _, args: _, dst } => dst.clone(),
        IRInstruction::CopyFromOffset { dst, .. } => Some(dst.clone()),
        IRInstruction::CopyToOffset { dst, .. } => Some(IRValue::Var(dst.clone())),
        IRInstruction::Load { dst, .. } => Some(dst.clone()),
        IRInstruction::AddPtr { dst, .. } => Some(dst.clone()),
        IRInstruction::GetAddress { dst, .. } => Some(dst.clone()),
        IRInstruction::IntToDouble { dst, .. } => Some(dst.clone()),
        IRInstruction::DoubleToInt { dst, .. } => Some(dst.clone()),
        IRInstruction::UIntToDouble { dst, .. } => Some(dst.clone()),
        IRInstruction::DoubletoUInt { dst, .. } => Some(dst.clone()),
        IRInstruction::SignExtend { dst, .. } => Some(dst.clone()),
        IRInstruction::ZeroExtend { dst, .. } => Some(dst.clone()),
        IRInstruction::Truncate { dst, .. } => Some(dst.clone()),
        IRInstruction::Store { .. } => None,
        IRInstruction::JumpIfNotZero { .. } | IRInstruction::JumpIfZero { .. } | IRInstruction::Label(_) | IRInstruction::Jump(_) | IRInstruction::Ret(_) => None,
    }
}

fn get_block_id(block: &Node) -> NodeId {
    match block {
        Node::Block { id, .. } => id.clone(),
        Node::Entry { .. } => NodeId::Entry,
        Node::Exit { .. } => NodeId::Exit,
    }
}

fn find_all_copies(cfg: &mut Vec<Node>) -> Vec<IRInstruction> {
    let mut all_copies = vec![];

    for block in cfg.iter() {
        match block {
            Node::Block { instructions, .. } => {
                for instruction in instructions.iter() {
                    if let IRInstruction::Copy { src, dst } = instruction {
                        let src_t = tacky_type(src);
                        let dst_t = tacky_type(dst);
                        if src_t == dst_t {
                            all_copies.push(instruction.clone());
                        }
                    }
                }
            }
            _ => {}
        }
    }

    all_copies
}

fn replace_operand(op: &IRValue, reaching_copies: &[IRInstruction]) -> IRValue {
    if let IRValue::Constant(_) = op {
        return op.to_owned();
    }

    for copy in reaching_copies.iter() {
        if let IRInstruction::Copy { src: copy_src, dst: copy_dst } = copy {
            if copy_dst == op {
                println!("replacing operand {:?} with {:?}", op, copy_src);
                return copy_src.to_owned();
            }
        }
    }

    return op.to_owned();
}

fn optionally_replace_operand(op: &Option<IRValue>, reaching_copies: &[IRInstruction]) -> Option<IRValue> {
    match op {
        Some(value) => Some(replace_operand(value, reaching_copies)),
        None => None,
    }
}

fn rewrite_instruction(instr: &IRInstruction, num: usize, annotated_instructions: &mut HashMap<(usize, IRInstruction), Vec<IRInstruction>>) -> Option<IRInstruction> {
    let reaching_copies = annotated_instructions.get(&(num, instr.to_owned())).unwrap();
    println!("reaching copies {:?}", reaching_copies);
    println!("annotated instructions {:?}", annotated_instructions);

    match instr {
        IRInstruction::Copy { src, dst } => {
            for copy in reaching_copies.iter() {
                if let IRInstruction::Copy { src: copy_src, dst: copy_dst } = copy {
                    if copy == instr || (copy_src == dst && copy_dst == src) {
                        return None;
                    }
                }
            }
            let new_src = replace_operand(src, reaching_copies);
            return Some(IRInstruction::Copy { src: new_src, dst: dst.clone() });
        }
        IRInstruction::Unary { op, src, dst } => {
            let new_src = replace_operand(src, reaching_copies);
            return Some(IRInstruction::Unary { op: *op, src: new_src, dst: dst.clone() });
        }
        IRInstruction::Binary { op, lhs, rhs, dst } => {
            let new_lhs = replace_operand(lhs, reaching_copies);
            let new_rhs = replace_operand(rhs, reaching_copies);
            return Some(IRInstruction::Binary { op: *op, lhs: new_lhs, rhs: new_rhs, dst: dst.clone() });
        }
        IRInstruction::Ret(value) => {
            println!("replacing return with copies {:?}", reaching_copies);
            let new_value = optionally_replace_operand(value, reaching_copies);
            return Some(IRInstruction::Ret(new_value));
        }
        IRInstruction::Call { target, args, dst } => {
            let new_args = args.iter().map(|arg| replace_operand(arg, reaching_copies)).collect();
            return Some(IRInstruction::Call { target: target.clone(), args: new_args, dst: dst.clone() });
        }
        IRInstruction::JumpIfNotZero { condition, target } => {
            let new_condition = replace_operand(condition, reaching_copies);
            return Some(IRInstruction::JumpIfNotZero { condition: new_condition, target: target.clone() });
        }
        IRInstruction::JumpIfZero { condition, target } => {
            let new_condition = replace_operand(condition, reaching_copies);
            return Some(IRInstruction::JumpIfZero { condition: new_condition, target: target.clone() });
        }
        _ => return Some(instr.clone()),
    }
}

fn get_block_by_id(cfg: &Vec<Node>, id: &NodeId) -> Node {
    cfg.iter().find(|node| match node {
        Node::Block { id: block_id, .. } => block_id == id,
        _ => false,
    }).unwrap().clone()
}

fn get_block_successors(block: &Node) -> Vec<NodeId> {
    match block {
        Node::Block { successors, .. } => successors.clone(),
        Node::Entry { successors } => successors.clone(),
        Node::Exit { .. } => vec![NodeId::Exit],
    }
}

fn get_block_predecessors(block: &Node) -> Vec<NodeId> {
    match block {
        Node::Block { predecessors, .. } => predecessors.clone(),
        Node::Exit { predecessors } => predecessors.clone(),
        _ => vec![],
    }
}

fn get_block_instructions(block: &Node) -> Vec<IRInstruction> {
    match block {
        Node::Block { instructions, .. } => instructions.clone(),
        _ => vec![],
    }
}

fn is_static(value: &IRValue) -> bool {
    match value {
        IRValue::Var(name) => {
            let symbol_table = SYMBOL_TABLE.lock().unwrap();
            if let Some(entry) = symbol_table.get(name) {
                if let IdentifierAttrs::StaticAttr { .. } = entry.attrs {
                    return true;
                }
            }
            false
        }
        _ => false,
    }
}