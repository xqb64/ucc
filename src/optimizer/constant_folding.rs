use crate::{
    ir::gen::{BinaryOp, IRInstruction, IRValue, UnaryOp},
    lexer::lex::Const,
    parser::ast::Type,
};

pub fn constant_folding(instructions: &[IRInstruction]) -> Vec<IRInstruction> {
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
                        BinaryOp::Less => (lhs_val < rhs_val).into(),
                        BinaryOp::Greater => (lhs_val > rhs_val).into(),
                        BinaryOp::Equal => (lhs_val == rhs_val).into(),
                        BinaryOp::NotEqual => (lhs_val != rhs_val).into(),
                        BinaryOp::GreaterEqual => (lhs_val >= rhs_val).into(),
                        BinaryOp::LessEqual => (lhs_val <= rhs_val).into(),
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
    let converted_src = const_convert(konst, &dst_type);

    vec![IRInstruction::Copy {
        src: IRValue::Constant(converted_src),
        dst: dst.clone(),
    }]
}

macro_rules! convert_const {
    ($konst:expr, $variant:path, $ty:ty) => {
        match $konst {
            Const::Int(val) => $variant(*val as $ty),
            Const::Long(val) => $variant(*val as $ty),
            Const::UInt(val) => $variant(*val as $ty),
            Const::ULong(val) => $variant(*val as $ty),
            Const::Double(val) => $variant(*val as $ty),
            Const::Char(val) => $variant(*val as $ty),
            Const::UChar(val) => $variant(*val as $ty),
        }
    };
}

fn const_convert(konst: &Const, dst_type: &Type) -> Const {
    match dst_type {
        Type::Int => convert_const!(konst, Const::Int, i32),
        Type::Long => convert_const!(konst, Const::Long, i64),
        Type::Uint => convert_const!(konst, Const::UInt, u32),
        Type::Ulong => convert_const!(konst, Const::ULong, u64),
        Type::Double => convert_const!(konst, Const::Double, f64),
        Type::Char => convert_const!(konst, Const::Char, i8),
        Type::UChar => convert_const!(konst, Const::UChar, u8),
        Type::SChar => convert_const!(konst, Const::Char, i8),
        Type::Pointer(_) => convert_const!(konst, Const::ULong, u64),
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
