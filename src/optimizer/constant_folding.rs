use crate::ir::{BinaryOp, IRInstruction, IRValue, UnaryOp};
use crate::lexer::Const;
use crate::parser::ast::Type;
use anyhow::Result;

pub fn constant_folding(instructions: &Vec<IRInstruction>) -> Vec<IRInstruction> {
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
                        UnaryOp::Not => {
                            if is_zero(&src_val) {
                                Const::Int(1)
                            } else {
                                Const::Int(0)
                            }
                        }
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
    use crate::codegen::gen::tacky_type;

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
            Const::Char(val) => Ok(Const::Char(*val)),
            Const::UChar(val) => Ok(Const::Char(*val as i8)),
            Const::Double(val) => Ok(Const::Char(*val as i8)),
        },
        Type::Pointer(_) => match konst {
            Const::Int(val) => Ok(Const::ULong(*val as u64)),
            Const::Long(val) => Ok(Const::ULong(*val as u64)),
            Const::UInt(val) => Ok(Const::ULong(*val as u64)),
            Const::ULong(val) => Ok(Const::ULong(*val)),
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
