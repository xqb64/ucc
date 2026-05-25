use crate::{
    ir::gen::make_temporary,
    lexer::lex::{Const, Span},
    parser::ast::{
        spanof, AddrOfExpression, AggregateKind, ArrowExpression, AssignExpression,
        BinaryExpression, BinaryExpressionKind, BlockItem, BlockStatement, CallExpression,
        CaseStatement, CastExpression, CompoundExpression, ConditionalExpression,
        ConstantExpression, Declaration, DefaultStatement, DerefExpression, DoWhileStatement,
        DotExpression, EnumDeclaration, Expression, ExpressionStatement, ForInit, ForStatement,
        FunctionDeclaration, GotoStatement, IfStatement, Initializer, LabeledStatement,
        LiteralExpression, PostfixExpression, PostfixExpressionKind, Program, ReturnStatement,
        SizeofExpression, SizeofTExpression, Statement, StorageClass, StringExpression,
        StructDeclaration, SubscriptExpression, SwitchStatement, Type, TypedefDeclaration,
        UnaryExpression, UnaryExpressionKind, VaArgExpression, VaCopyExpression, VaEndExpression,
        VaStartExpression, VariableDeclaration, VariableExpression, WhileStatement,
    },
    util::error::{ErrorKind, Result, UccError},
};
use std::{
    any::Any,
    cmp::{max, Ordering},
    collections::BTreeMap,
    sync::Mutex,
};

#[derive(Debug, Clone)]
struct CurrentFunctionContext {
    variadic: bool,
    last_param: Option<String>,
}

lazy_static::lazy_static! {
    pub static ref SYMBOL_TABLE: Mutex<BTreeMap<String, Symbol>> = Mutex::new(BTreeMap::new());
    pub static ref TYPE_TABLE: Mutex<BTreeMap<String, StructEntry>> = Mutex::new(BTreeMap::new());
    static ref CURRENT_FUNCTION_CONTEXT: Mutex<Option<CurrentFunctionContext>> = Mutex::new(None);
}

pub const BUILTIN_VA_LIST_TAG: &str = "__builtin_va_list_tag";

pub fn ensure_builtin_va_list_type() {
    let mut type_table = TYPE_TABLE.lock().unwrap();
    if type_table.contains_key(BUILTIN_VA_LIST_TAG) {
        return;
    }

    type_table.insert(
        BUILTIN_VA_LIST_TAG.to_string(),
        StructEntry {
            kind: AggregateKind::Struct,
            alignment: 8,
            size: 24,
            members: vec![
                MemberEntry {
                    name: "gp_offset".to_string(),
                    ty: Type::UInt,
                    offset: 0,
                },
                MemberEntry {
                    name: "fp_offset".to_string(),
                    ty: Type::UInt,
                    offset: 4,
                },
                MemberEntry {
                    name: "overflow_arg_area".to_string(),
                    ty: Type::Pointer(Box::new(Type::Void)),
                    offset: 8,
                },
                MemberEntry {
                    name: "reg_save_area".to_string(),
                    ty: Type::Pointer(Box::new(Type::Void)),
                    offset: 16,
                },
            ],
        },
    );
}

pub fn builtin_va_list_element_type() -> Type {
    ensure_builtin_va_list_type();
    Type::Struct {
        tag: BUILTIN_VA_LIST_TAG.to_string(),
    }
}

pub fn builtin_va_list_type() -> Type {
    Type::Array {
        element: Box::new(builtin_va_list_element_type()),
        size: 1,
    }
}

pub fn is_builtin_va_list_object_type(t: &Type) -> bool {
    matches!(
        t,
        Type::Array { element, size: 1 }
            if matches!(element.as_ref(), Type::Struct { tag } if tag == BUILTIN_VA_LIST_TAG)
    )
}

pub fn is_builtin_va_list_pointer_type(t: &Type) -> bool {
    matches!(
        t,
        Type::Pointer(inner)
            if matches!(inner.as_ref(), Type::Struct { tag } if tag == BUILTIN_VA_LIST_TAG)
    )
}

pub fn is_builtin_va_list_compatible_type(t: &Type) -> bool {
    is_builtin_va_list_object_type(t) || is_builtin_va_list_pointer_type(t)
}

pub trait Typecheck {
    fn typecheck(self) -> Result<Self>
    where
        Self: Sized;
}

impl Typecheck for Program {
    fn typecheck(self) -> Result<Self> {
        let typechecked_block_items = self
            .block_items
            .into_iter()
            .map(|block_item| block_item.typecheck())
            .collect::<Result<Vec<_>>>()?;

        Ok(Program {
            block_items: typechecked_block_items,
        })
    }
}

impl Typecheck for BlockItem {
    fn typecheck(self) -> Result<Self> {
        match self {
            BlockItem::Declaration(decl) => {
                let typecheck_decl = decl.typecheck()?;
                Ok(BlockItem::Declaration(typecheck_decl))
            }

            BlockItem::Statement(stmt) => {
                let typechecked_stmt = stmt.typecheck()?;
                Ok(BlockItem::Statement(typechecked_stmt))
            }
        }
    }
}

impl Typecheck for Declaration {
    fn typecheck(self) -> Result<Self> {
        match self {
            Declaration::Variable(var_decl) => {
                let typechecked = var_decl.typecheck()?;
                Ok(Declaration::Variable(typechecked))
            }

            Declaration::Function(func_decl) => {
                let typechecked = func_decl.typecheck()?;
                Ok(Declaration::Function(typechecked))
            }

            Declaration::Struct(struct_decl) => {
                let typechecked = struct_decl.typecheck()?;
                Ok(Declaration::Struct(typechecked))
            }

            Declaration::Union(union_decl) => {
                let typechecked = union_decl.typecheck()?;
                Ok(Declaration::Union(typechecked))
            }

            Declaration::Enum(enum_decl) => {
                let typechecked = enum_decl.typecheck()?;
                Ok(Declaration::Enum(typechecked))
            }
            Declaration::Typedef(typedef_decl) => {
                let typechecked = typedef_decl.typecheck()?;
                Ok(Declaration::Typedef(typechecked))
            }
        }
    }
}

impl Typecheck for TypedefDeclaration {
    fn typecheck(self) -> Result<Self> {
        validate_type_specifier(&self.ty)?;
        Ok(self)
    }
}

fn infer_array_size_from_initializer(ty: &Type, init: Option<&Initializer>) -> Type {
    match (ty, init) {
        (Type::Array { element, size: 0 }, Some(Initializer::Compound(_, _, inits))) => {
            Type::Array {
                element: element.clone(),
                size: inits.len(),
            }
        }
        (
            Type::Array { element, size: 0 },
            Some(Initializer::Single(_, Expression::String(string_expr))),
        ) if matches!(element.as_ref(), Type::Char | Type::SChar | Type::UChar) => Type::Array {
            element: element.clone(),
            size: string_expr.value.len() + 1,
        },
        _ => ty.clone(),
    }
}

impl Typecheck for VariableDeclaration {
    fn typecheck(mut self) -> Result<Self> {
        self.ty = infer_array_size_from_initializer(&self.ty, self.init.as_ref());
        if self.ty == Type::Void {
            return Err(UccError {
                msg: format!("Variable declared with void type"),
                kind: ErrorKind::Typecheck,
                span: self.span,
            });
        } else {
            validate_type_specifier(&self.ty)?;
        }

        match self.is_global {
            true => {
                if !is_complete(&self.ty) && self.storage_class != Some(StorageClass::Extern) {
                    return Err(UccError {
                        msg: format!("Variable declared with incomplete type"),
                        kind: ErrorKind::Typecheck,
                        span: self.span,
                    });
                }

                let default_init = if self.storage_class == Some(StorageClass::Extern) {
                    InitialValue::NoInitializer
                } else {
                    InitialValue::Tentative
                };

                let static_init = match &self.init {
                    Some(init) => to_static_init(init, &self.ty)?,
                    None => default_init,
                };

                let is_global = self.storage_class != Some(StorageClass::Static);

                let old_decl = SYMBOL_TABLE.lock().unwrap().get(&self.name).cloned();

                let check_against_previous = |old_d: &Symbol| -> Result<(bool, InitialValue)> {
                    if old_d.ty != self.ty {
                        return Err(UccError {
                            msg: format!("Variable redeclared with different type"),
                            kind: ErrorKind::Typecheck,
                            span: self.span,
                        });
                    }

                    match &old_d.attrs {
                        IdentifierAttrs::StaticAttr {
                            initial_value: prev_init,
                            global: prev_global,
                        } => {
                            let global = if self.storage_class == Some(StorageClass::Extern) {
                                *prev_global
                            } else if is_global == *prev_global {
                                is_global
                            } else {
                                return Err(UccError {
                                    msg: format!("Conflicting variable linkage."),
                                    kind: ErrorKind::Typecheck,
                                    span: self.span,
                                });
                            };

                            let init = match (&prev_init, &static_init) {
                                (InitialValue::Initial(_), InitialValue::Initial(_)) => {
                                    return Err(UccError {
                                        msg: format!(
                                            "Conflicting file-scope variable initializers."
                                        ),
                                        kind: ErrorKind::Typecheck,
                                        span: self.span,
                                    });
                                }
                                (InitialValue::Initial(_), _) => prev_init,
                                (
                                    InitialValue::Tentative,
                                    InitialValue::Tentative | InitialValue::NoInitializer,
                                ) => &InitialValue::Tentative,
                                (_, InitialValue::Initial(_)) => &static_init,
                                (InitialValue::NoInitializer, _) => &static_init,
                            };

                            Ok((global, init.to_owned()))
                        }
                        _ => {
                            unreachable!()
                        }
                    }
                };

                let (global, init) = match old_decl {
                    Some(old_d) => check_against_previous(&old_d)?,
                    None => (is_global, static_init),
                };

                let symbol = Symbol {
                    ty: self.ty.clone(),
                    attrs: IdentifierAttrs::StaticAttr {
                        initial_value: init,
                        global,
                    },
                };

                SYMBOL_TABLE
                    .lock()
                    .unwrap()
                    .insert(self.name.clone(), symbol);

                let typechecked_init = self
                    .init
                    .as_ref()
                    .map(|init| typecheck_init(&self.ty, init))
                    .transpose()?;

                Ok(VariableDeclaration {
                    name: self.name.clone(),
                    ty: self.ty.clone(),
                    init: typechecked_init,
                    storage_class: self.storage_class,
                    is_global: self.is_global,
                    span: self.span,
                })
            }
            false => {
                if !is_complete(&self.ty) {
                    return Err(UccError {
                        msg: format!("Variable declared with incomplete type."),
                        kind: ErrorKind::Typecheck,
                        span: self.span,
                    });
                }

                match self.storage_class {
                    Some(StorageClass::Extern) => {
                        if self.init.is_some() {
                            return Err(UccError {
                                msg: format!("Extern variable with initializer."),
                                kind: ErrorKind::Typecheck,
                                span: self.span,
                            });
                        }

                        let symbol = SYMBOL_TABLE.lock().unwrap().get(&self.name).cloned();

                        match symbol {
                            Some(sym) => {
                                if sym.ty != self.ty {
                                    return Err(UccError {
                                        msg: format!("Variable redeclared with different type."),
                                        kind: ErrorKind::Typecheck,
                                        span: self.span,
                                    });
                                }
                            }
                            None => {
                                let symbol = Symbol {
                                    ty: self.ty.clone(),
                                    attrs: IdentifierAttrs::StaticAttr {
                                        initial_value: InitialValue::NoInitializer,
                                        global: true,
                                    },
                                };

                                SYMBOL_TABLE
                                    .lock()
                                    .unwrap()
                                    .insert(self.name.clone(), symbol);
                            }
                        }

                        let typechecked_init = self
                            .init
                            .as_ref()
                            .map(|init| typecheck_init(&self.ty, init))
                            .transpose()?;

                        Ok(VariableDeclaration {
                            name: self.name.clone(),
                            ty: self.ty.clone(),
                            init: typechecked_init,
                            storage_class: self.storage_class,
                            is_global: self.is_global,
                            span: self.span,
                        })
                    }
                    Some(StorageClass::Static) => {
                        let zero_init = InitialValue::Initial(vec![StaticInit::Zero(
                            get_size_of_type(&self.ty),
                        )]);
                        let static_init = match &self.init {
                            Some(init) => to_static_init(init, &self.ty)?,
                            None => zero_init,
                        };

                        let symbol = Symbol {
                            ty: self.ty.clone(),
                            attrs: IdentifierAttrs::StaticAttr {
                                initial_value: static_init,
                                global: false,
                            },
                        };
                        SYMBOL_TABLE
                            .lock()
                            .unwrap()
                            .insert(self.name.clone(), symbol);

                        let typechecked_init = self
                            .init
                            .as_ref()
                            .map(|init| typecheck_init(&self.ty, init))
                            .transpose()?;

                        Ok(VariableDeclaration {
                            name: self.name.clone(),
                            ty: self.ty.clone(),
                            init: typechecked_init,
                            storage_class: self.storage_class,
                            is_global: self.is_global,
                            span: self.span,
                        })
                    }
                    Some(StorageClass::Typedef) => {
                        return Err(UccError {
                            msg: format!("Typedef storage class reached variable typechecker"),
                            kind: ErrorKind::Typecheck,
                            span: self.span,
                        });
                    }
                    None => {
                        let symbol = Symbol {
                            ty: self.ty.clone(),
                            attrs: IdentifierAttrs::LocalAttr,
                        };
                        SYMBOL_TABLE
                            .lock()
                            .unwrap()
                            .insert(self.name.clone(), symbol);

                        let typechecked_init = self
                            .init
                            .as_ref()
                            .map(|init| typecheck_init(&self.ty, init))
                            .transpose()?;

                        Ok(VariableDeclaration {
                            name: self.name.clone(),
                            ty: self.ty.clone(),
                            init: typechecked_init,
                            storage_class: self.storage_class,
                            is_global: self.is_global,
                            span: self.span,
                        })
                    }
                }
            }
        }
    }
}

fn validate_type_specifier(t: &Type) -> Result<()> {
    match t {
        Type::Array { element, size: _ } => {
            if !is_complete(element) {
                return Err(UccError {
                    msg: format!("Incomplete type."),
                    kind: ErrorKind::Typecheck,
                    span: Span { start: 0, end: 0 },
                });
            }
            validate_type_specifier(element)?;
        }

        Type::Pointer(referenced) => {
            validate_type_specifier(referenced)?;
        }

        Type::Func { params, ret, .. } => {
            for param in params {
                validate_type_specifier(param)?;
            }
            validate_type_specifier(ret)?;
        }

        _ => {}
    }

    Ok(())
}

impl Typecheck for FunctionDeclaration {
    fn typecheck(self) -> Result<Self> {
        if self.ty == Type::Void {
            return Err(UccError {
                msg: format!("Variable declared with void type."),
                kind: ErrorKind::Typecheck,
                span: Span { start: 0, end: 0 },
            });
        } else {
            validate_type_specifier(&self.ty)?;
        }

        let adjust_param_type = |t: Type| -> Result<Type> {
            match t {
                Type::Array { element, .. } => Ok(Type::Pointer(element)),
                Type::Void => {
                    return Err(UccError {
                        msg: format!("Function parameter has void type"),
                        kind: ErrorKind::Typecheck,
                        span: self.span,
                    })
                }
                t => Ok(t),
            }
        };

        let (param_ts, _, fun_type) = match self.ty.clone() {
            Type::Func {
                params,
                ret,
                variadic,
            } => {
                if let Type::Array { .. } = *ret {
                    return Err(UccError {
                        msg: format!("Function return type is an array"),
                        kind: ErrorKind::Typecheck,
                        span: self.span,
                    });
                }
                let param_types: Vec<Type> = params
                    .into_iter()
                    .map(adjust_param_type)
                    .collect::<Result<Vec<_>>>()?;
                (
                    param_types.clone(),
                    ret.clone(),
                    Type::Func {
                        params: param_types.clone(),
                        ret: ret.clone(),
                        variadic,
                    },
                )
            }
            _ => {
                return Err(UccError {
                    msg: format!("Function has non function type"),
                    kind: ErrorKind::Typecheck,
                    span: self.span,
                })
            }
        };

        let has_body = self.body.is_some();

        if has_body {
            for param in param_ts.iter() {
                if !is_complete(param) {
                    return Err(UccError {
                        kind: ErrorKind::Typecheck,
                        msg: format!("Function parameter has incomplete type"),
                        span: self.span,
                    });
                }
            }

            let ret_type = match &self.ty {
                Type::Func { ret, .. } => ret,
                _ => unreachable!(),
            };

            if let Type::Struct { tag } | Type::Union { tag } = &**ret_type {
                if !TYPE_TABLE.lock().unwrap().contains_key(tag) {
                    return Err(UccError {
                        kind: ErrorKind::Typecheck,
                        msg: format!("Function return type is incomplete."),
                        span: self.span,
                    });
                }
            }
        }

        let global = self.storage_class != Some(StorageClass::Static);

        let check_against_previous = |prev: &Symbol| -> Result<(bool, bool)> {
            if prev.ty != fun_type {
                return Err(UccError {
                    kind: ErrorKind::Typecheck,
                    msg: format!("Redeclared function."),
                    span: self.span,
                });
            }

            match &prev.attrs {
                IdentifierAttrs::FuncAttr {
                    global: prev_global,
                    defined: prev_defined,
                } => {
                    if *prev_defined && has_body {
                        return Err(UccError {
                            kind: ErrorKind::Typecheck,
                            msg: format!("Function defined twice."),
                            span: self.span,
                        });
                    } else if *prev_global && self.storage_class == Some(StorageClass::Static) {
                        return Err(UccError {
                            kind: ErrorKind::Typecheck,
                            msg: format!("StaticFunctionDeclarationAfterNonStatic"),
                            span: self.span,
                        });
                    }

                    let defined = has_body || *prev_defined;
                    Ok((defined, *prev_global))
                }
                _ => {
                    return Err(UccError {
                        msg: format!("Symbol has function type but not function attributes"),
                        kind: ErrorKind::Typecheck,
                        span: self.span,
                    })
                }
            }
        };

        let old_decl = SYMBOL_TABLE.lock().unwrap().get(&self.name).cloned();
        let (defined, global) = match old_decl {
            Some(old_d) => check_against_previous(&old_d)?,
            None => (has_body, global),
        };

        SYMBOL_TABLE.lock().unwrap().insert(
            self.name.clone(),
            Symbol {
                ty: fun_type.clone(),
                attrs: IdentifierAttrs::FuncAttr { global, defined },
            },
        );

        if has_body {
            for (param, param_t) in self.params.iter().zip(param_ts) {
                let symbol = Symbol {
                    ty: param_t,
                    attrs: IdentifierAttrs::LocalAttr,
                };
                SYMBOL_TABLE.lock().unwrap().insert(param.clone(), symbol);
            }
        }

        let old_context = CURRENT_FUNCTION_CONTEXT.lock().unwrap().clone();
        if has_body {
            let variadic = matches!(&fun_type, Type::Func { variadic: true, .. });
            *CURRENT_FUNCTION_CONTEXT.lock().unwrap() = Some(CurrentFunctionContext {
                variadic,
                last_param: self.params.last().cloned(),
            });
        }

        let typechecked_body_result = self.body.map(|body| body.typecheck()).transpose();
        *CURRENT_FUNCTION_CONTEXT.lock().unwrap() = old_context;
        let typechecked_body = typechecked_body_result?;

        Ok(FunctionDeclaration {
            name: self.name.clone(),
            ty: self.ty.clone(),
            params: self.params.clone(),
            body: typechecked_body.into(),
            is_global: self.is_global,
            storage_class: self.storage_class,
            span: self.span,
        })
    }
}

impl Typecheck for StructDeclaration {
    fn typecheck(self) -> Result<Self>
    where
        Self: Sized,
    {
        if self.members.is_empty() {
            return Ok(self);
        }

        validate_struct_definition(&self)?;

        let mut member_entries = vec![];
        let mut aggregate_size = 0;
        let mut aggregate_alignment = 1;

        for member in &self.members {
            let member_alignment = alignment(&member.ty);
            let member_offset = match self.kind {
                AggregateKind::Struct => round_up(aggregate_size, member_alignment),
                AggregateKind::Union => 0,
            };
            let m = MemberEntry {
                name: member.name.clone(),
                ty: member.ty.clone(),
                offset: member_offset,
            };

            member_entries.push(m);

            aggregate_alignment = max(aggregate_alignment, member_alignment);
            aggregate_size = match self.kind {
                AggregateKind::Struct => member_offset + get_size_of_type(&member.ty),
                AggregateKind::Union => max(aggregate_size, get_size_of_type(&member.ty)),
            };
        }

        aggregate_size = round_up(aggregate_size, aggregate_alignment);
        let s = StructEntry {
            kind: self.kind,
            alignment: aggregate_alignment,
            size: aggregate_size,
            members: member_entries,
        };

        TYPE_TABLE.lock().unwrap().insert(self.tag.clone(), s);

        Ok(self)
    }
}

impl Typecheck for EnumDeclaration {
    fn typecheck(self) -> Result<Self>
    where
        Self: Sized,
    {
        let mut next_value: i64 = 0;
        let mut processed_members = vec![];

        for member in self.members {
            let value = if let Some(expr) = &member.value {
                let typed_expr = typecheck_and_convert(expr)?;
                if !is_integer_type(get_type(&typed_expr)) {
                    return Err(UccError {
                        msg: format!("Enumerator value is not an integer constant expression"),
                        kind: ErrorKind::Typecheck,
                        span: member.span,
                    });
                }
                eval_integer_constant_expression(&typed_expr)?
            } else {
                next_value
            };

            let value_i32 = i32::try_from(value).map_err(|_| UccError {
                msg: format!("Enumerator value is outside the range of int"),
                kind: ErrorKind::Typecheck,
                span: member.span,
            })?;

            SYMBOL_TABLE.lock().unwrap().insert(
                member.name.clone(),
                Symbol {
                    ty: Type::Int,
                    attrs: IdentifierAttrs::EnumConstantAttr(value_i32),
                },
            );

            processed_members.push(crate::parser::ast::EnumMemberDeclaration {
                name: member.name,
                value: Some(Expression::Constant(ConstantExpression {
                    value: Const::Int(value_i32),
                    ty: Type::Int,
                    span: member.span,
                })),
                span: member.span,
            });

            next_value = value + 1;
        }

        Ok(EnumDeclaration {
            tag: self.tag,
            members: processed_members,
            span: self.span,
        })
    }
}

fn eval_integer_constant_expression(expr: &Expression) -> Result<i64> {
    fn const_to_i64(value: &Const) -> Result<i64> {
        match value {
            Const::Short(v) => Ok(*v as i64),
            Const::UShort(v) => Ok(*v as i64),
            Const::Int(v) => Ok(*v as i64),
            Const::Long(v) => Ok(*v),
            Const::UInt(v) => Ok(*v as i64),
            Const::ULong(v) => i64::try_from(*v).map_err(|_| UccError {
                msg: format!("Integer constant expression is outside the supported range"),
                kind: ErrorKind::Typecheck,
                span: Span { start: 0, end: 0 },
            }),
            Const::Char(v) => Ok(*v as i64),
            Const::UChar(v) => Ok(*v as i64),
            Const::Float(_) | Const::Double(_) => Err(UccError {
                msg: format!("Enumerator value is not an integer constant expression"),
                kind: ErrorKind::Typecheck,
                span: Span { start: 0, end: 0 },
            }),
        }
    }

    match expr {
        Expression::Constant(ConstantExpression { value, .. }) => const_to_i64(value),
        Expression::Cast(CastExpression {
            expr,
            target_type,
            span,
            ..
        }) => {
            if !is_integer_type(target_type) {
                return Err(UccError {
                    msg: format!("Enumerator value is not an integer constant expression"),
                    kind: ErrorKind::Typecheck,
                    span: *span,
                });
            }
            Ok(eval_integer_constant_expression(expr)? as i32 as i64)
        }
        Expression::Unary(UnaryExpression {
            kind, expr, span, ..
        }) => {
            let value = eval_integer_constant_expression(expr)?;
            match kind {
                UnaryExpressionKind::Negate => Ok(value.wrapping_neg()),
                UnaryExpressionKind::Complement => Ok(!value),
                UnaryExpressionKind::Not => Ok((value == 0) as i64),
                UnaryExpressionKind::Inc | UnaryExpressionKind::Dec => Err(UccError {
                    msg: format!("Enumerator value is not an integer constant expression"),
                    kind: ErrorKind::Typecheck,
                    span: *span,
                }),
            }
        }
        Expression::Binary(BinaryExpression {
            kind,
            lhs,
            rhs,
            span,
            ..
        }) => {
            let lhs = eval_integer_constant_expression(lhs)?;
            let rhs = eval_integer_constant_expression(rhs)?;
            match kind {
                BinaryExpressionKind::Add => Ok(lhs.wrapping_add(rhs)),
                BinaryExpressionKind::Sub => Ok(lhs.wrapping_sub(rhs)),
                BinaryExpressionKind::Mul => Ok(lhs.wrapping_mul(rhs)),
                BinaryExpressionKind::Div => Ok(lhs.checked_div(rhs).ok_or_else(|| UccError {
                    msg: format!("Invalid enumerator division"),
                    kind: ErrorKind::Typecheck,
                    span: *span,
                })?),
                BinaryExpressionKind::Rem => Ok(lhs.checked_rem(rhs).ok_or_else(|| UccError {
                    msg: format!("Invalid enumerator remainder"),
                    kind: ErrorKind::Typecheck,
                    span: *span,
                })?),
                BinaryExpressionKind::BitwiseOr => Ok(lhs | rhs),
                BinaryExpressionKind::BitwiseXor => Ok(lhs ^ rhs),
                BinaryExpressionKind::BitwiseAnd => Ok(lhs & rhs),
                BinaryExpressionKind::BitwiseShl => Ok(lhs.wrapping_shl(rhs as u32)),
                BinaryExpressionKind::BitwiseShr => Ok(lhs.wrapping_shr(rhs as u32)),
                BinaryExpressionKind::Equal => Ok((lhs == rhs) as i64),
                BinaryExpressionKind::NotEqual => Ok((lhs != rhs) as i64),
                BinaryExpressionKind::Less => Ok((lhs < rhs) as i64),
                BinaryExpressionKind::Greater => Ok((lhs > rhs) as i64),
                BinaryExpressionKind::LessEqual => Ok((lhs <= rhs) as i64),
                BinaryExpressionKind::GreaterEqual => Ok((lhs >= rhs) as i64),
                BinaryExpressionKind::And => Ok(((lhs != 0) && (rhs != 0)) as i64),
                BinaryExpressionKind::Or => Ok(((lhs != 0) || (rhs != 0)) as i64),
            }
        }
        Expression::Conditional(ConditionalExpression {
            condition,
            then_expr,
            else_expr,
            ..
        }) => {
            if eval_integer_constant_expression(condition)? != 0 {
                eval_integer_constant_expression(then_expr)
            } else {
                eval_integer_constant_expression(else_expr)
            }
        }
        _ => Err(UccError {
            msg: format!("Enumerator value is not an integer constant expression"),
            kind: ErrorKind::Typecheck,
            span: spanof(expr),
        }),
    }
}

fn validate_struct_definition(definition: &StructDeclaration) -> Result<StructDeclaration> {
    use std::collections::BTreeSet;

    let tag = &definition.tag;

    if TYPE_TABLE.lock().unwrap().contains_key(tag) {
        panic!("Structure or union was already declared");
    } else {
        let mut member_names = BTreeSet::new();

        for member in &definition.members {
            let member_name = &member.name;

            if !member_name.is_empty() {
                if member_names.contains(member_name) {
                    return Err(UccError {
                        msg: format!(
                            "Duplicate declaration of member {} in aggregate {}",
                            member_name, tag
                        ),
                        kind: ErrorKind::Typecheck,
                        span: definition.span,
                    });
                } else {
                    member_names.insert(member_name.clone());
                }
            }

            validate_type_specifier(&member.ty)?;

            match &member.ty {
                Type::Func { .. } => {
                    return Err(UccError {
                        msg: format!("Can't declare aggregate member with function type",),
                        kind: ErrorKind::Typecheck,
                        span: definition.span,
                    });
                }
                _ => {
                    if !is_complete(&member.ty) {
                        return Err(UccError {
                            msg: format!("Cannot declare aggregate member with incomplete type"),
                            kind: ErrorKind::Typecheck,
                            span: definition.span,
                        });
                    }
                }
            }
        }
    }

    Ok(definition.to_owned())
}

impl Typecheck for Statement {
    fn typecheck(self) -> Result<Self> {
        match self {
            Statement::Return(ReturnStatement {
                expr: Some(expression),
                target_type,
                belongs_to,
                span,
            }) => {
                if target_type == Some(Type::Void) {
                    return Err(UccError {
                        msg: format!("Return statement with expression in void function"),
                        kind: ErrorKind::Typecheck,
                        span,
                    });
                } else {
                    let ret_type = SYMBOL_TABLE
                        .lock()
                        .unwrap()
                        .get(&belongs_to)
                        .cloned()
                        .unwrap()
                        .ty;

                    let tt = match ret_type {
                        Type::Func { ret, .. } => ret,
                        _ => unreachable!(),
                    };

                    let typechecked_expr = typecheck_and_convert(&expression)?;
                    let converted_expr = convert_by_assignment(&typechecked_expr, &tt)?;

                    Ok(Statement::Return(ReturnStatement {
                        expr: converted_expr.into(),
                        target_type,
                        belongs_to,
                        span,
                    }))
                }
            }

            Statement::Return(ReturnStatement {
                expr: None,
                target_type,
                belongs_to,
                span,
            }) => {
                if target_type == Some(Type::Void) {
                    Ok(Statement::Return(ReturnStatement {
                        expr: None,
                        target_type,
                        belongs_to,
                        span,
                    }))
                } else {
                    return Err(UccError {
                        msg: format!("Return statement with no expression in non-void function"),
                        kind: ErrorKind::Typecheck,
                        span,
                    });
                }
            }

            Statement::Expression(ExpressionStatement { expr, span }) => {
                let typechecked_expr = typecheck_and_convert(&expr)?;
                Ok(Statement::Expression(ExpressionStatement {
                    expr: typechecked_expr,
                    span,
                }))
            }

            Statement::If(IfStatement {
                condition,
                then_branch,
                else_branch,
                span,
            }) => {
                let typechecked_condition = typecheck_scalar(&condition)?;
                let typechecked_then = then_branch.typecheck()?;
                let typechecked_else = match *else_branch {
                    Some(else_branch) => else_branch.typecheck()?.into(),
                    None => None,
                };

                Ok(Statement::If(IfStatement {
                    condition: typechecked_condition,
                    then_branch: typechecked_then.into(),
                    else_branch: typechecked_else.into(),
                    span,
                }))
            }

            Statement::Compound(BlockStatement { stmts, span }) => {
                let typechecked_stmts = stmts
                    .into_iter()
                    .map(|stmt| stmt.typecheck())
                    .collect::<Result<Vec<_>>>()?;
                Ok(Statement::Compound(BlockStatement {
                    stmts: typechecked_stmts,
                    span,
                }))
            }

            Statement::While(WhileStatement {
                condition,
                body,
                label,
                span,
            }) => {
                let typechecked_condition = typecheck_scalar(&condition)?;
                let typechecked_body = body.typecheck()?;

                Ok(Statement::While(WhileStatement {
                    condition: typechecked_condition,
                    body: typechecked_body.into(),
                    label,
                    span,
                }))
            }

            Statement::DoWhile(DoWhileStatement {
                condition,
                body,
                label,
                span,
            }) => {
                let typechecked_condition = typecheck_scalar(&condition)?;
                let typechecked_body = body.typecheck()?;

                Ok(Statement::DoWhile(DoWhileStatement {
                    condition: typechecked_condition,
                    body: typechecked_body.into(),
                    label,
                    span,
                }))
            }

            Statement::For(ForStatement {
                init,
                condition,
                post,
                body,
                label,
                span,
            }) => {
                if let ForInit::Declaration(decl) = &init {
                    if decl.storage_class.is_some() {
                        return Err(UccError {
                            msg: format!("Storage class specifier in for loop init."),
                            kind: ErrorKind::Typecheck,
                            span,
                        });
                    }
                }

                let typechecked_init = optionally_typecheck_for_init(init)?;
                let typechecked_condition = optionally_typecheck_scalar(&condition)?;
                let typechecked_post = post.map(|expr| typecheck_and_convert(&expr)).transpose()?;

                let typechecked_body = body.typecheck()?;

                Ok(Statement::For(ForStatement {
                    init: typechecked_init,
                    condition: typechecked_condition,
                    post: typechecked_post,
                    body: typechecked_body.into(),
                    label,
                    span,
                }))
            }

            Statement::Goto(GotoStatement { .. }) => Ok(self),

            Statement::Labeled(LabeledStatement { label, body, span }) => {
                let typechecked_body = body.typecheck()?;
                Ok(Statement::Labeled(LabeledStatement {
                    label,
                    body: typechecked_body.into(),
                    span,
                }))
            }

            Statement::Switch(SwitchStatement {
                condition,
                body,
                label,
                cases,
                span,
            }) => {
                let typechecked_expr = typecheck_and_convert(&condition)?;

                if !is_integer_type(get_type(&typechecked_expr)) {
                    return Err(UccError {
                        msg: format!("Controlling expression in switch statement must be integer."),
                        kind: ErrorKind::Typecheck,
                        span,
                    });
                }

                let typechecked_expr = if is_small_integer_type(get_type(&typechecked_expr)) {
                    convert_to(&typechecked_expr, &Type::Int)
                } else {
                    typechecked_expr
                };

                let typechecked_body = body.typecheck()?;

                Ok(Statement::Switch(SwitchStatement {
                    condition: typechecked_expr,
                    body: typechecked_body.into(),
                    label,
                    cases,
                    span,
                }))
            }

            Statement::Case(CaseStatement {
                body,
                label,
                value,
                span,
            }) => {
                let typechecked_expr = typecheck_and_convert(&value)?;

                if is_floating_type(get_type(&typechecked_expr)) {
                    return Err(UccError {
                        msg: format!("Case expression can't be a floating type."),
                        kind: ErrorKind::Typecheck,
                        span,
                    });
                }

                let typechecked_body = body.typecheck()?;

                Ok(Statement::Case(CaseStatement {
                    value: typechecked_expr,
                    body: typechecked_body.into(),
                    label,
                    span,
                }))
            }

            Statement::Default(DefaultStatement { body, label, span }) => {
                let typechecked_body = body.typecheck()?;

                Ok(Statement::Default(DefaultStatement {
                    body: typechecked_body.into(),
                    label,
                    span,
                }))
            }

            Statement::Break(_) | Statement::Continue(_) | Statement::Null => Ok(self),
        }
    }
}

fn typecheck_expr(expr: &Expression) -> Result<Expression> {
    match expr {
        Expression::Compound(CompoundExpression {
            kind,
            lhs,
            rhs,
            ty: _,
            result_t: _,
            span,
        }) => {
            let k = (*kind).into();
            typecheck_compound(&k, lhs, rhs, *span)
        }

        Expression::Postfix(PostfixExpression {
            kind,
            expr,
            ty: _,
            span,
        }) => match kind {
            PostfixExpressionKind::Inc => typecheck_postfix_inc(expr, *span),
            PostfixExpressionKind::Dec => typecheck_postfix_dec(expr, *span),
        },

        Expression::VaStart(VaStartExpression {
            list,
            last_param,
            ty: _,
            span,
        }) => {
            let typed_list = typecheck_expr(list)?;
            let typed_last_param = typecheck_expr(last_param)?;
            let list_ty = get_type(&typed_list);

            let context = CURRENT_FUNCTION_CONTEXT.lock().unwrap().clone();
            if !context.as_ref().is_some_and(|ctx| ctx.variadic) {
                return Err(UccError {
                    msg: format!("__builtin_va_start may only be used in a variadic function"),
                    kind: ErrorKind::Typecheck,
                    span: *span,
                });
            }

            if let Some(expected_last_param) = context.and_then(|ctx| ctx.last_param) {
                match &typed_last_param {
                    Expression::Variable(VariableExpression { value, .. })
                        if value == &expected_last_param => {}
                    _ => {
                        return Err(UccError {
                            msg: format!("__builtin_va_start second argument must be the last named parameter"),
                            kind: ErrorKind::Typecheck,
                            span: *span,
                        });
                    }
                }
            }

            if !is_builtin_va_list_compatible_type(list_ty) {
                return Err(UccError {
                    msg: format!("__builtin_va_start first argument must be a va_list"),
                    kind: ErrorKind::Typecheck,
                    span: *span,
                });
            }

            if is_builtin_va_list_object_type(list_ty) && !is_lvalue(&typed_list) {
                return Err(UccError {
                    msg: format!("__builtin_va_start first argument must be a va_list lvalue"),
                    kind: ErrorKind::Typecheck,
                    span: *span,
                });
            }

            Ok(Expression::VaStart(VaStartExpression {
                list: Box::new(typed_list),
                last_param: Box::new(typed_last_param),
                ty: Type::Void,
                span: *span,
            }))
        }

        Expression::VaArg(VaArgExpression {
            list,
            arg_ty,
            ty: _,
            span,
        }) => {
            let typed_list = typecheck_expr(list)?;
            let list_ty = get_type(&typed_list);

            if !is_builtin_va_list_compatible_type(list_ty) {
                return Err(UccError {
                    msg: format!("__builtin_va_arg first argument must be a va_list"),
                    kind: ErrorKind::Typecheck,
                    span: *span,
                });
            }

            if !is_complete(arg_ty)
                || matches!(arg_ty, Type::Void | Type::Func { .. } | Type::Array { .. })
            {
                return Err(UccError {
                    msg: format!("__builtin_va_arg needs a complete object type"),
                    kind: ErrorKind::Typecheck,
                    span: *span,
                });
            }

            Ok(Expression::VaArg(VaArgExpression {
                list: Box::new(typed_list),
                arg_ty: arg_ty.clone(),
                ty: arg_ty.clone(),
                span: *span,
            }))
        }

        Expression::VaCopy(VaCopyExpression {
            dst,
            src,
            ty: _,
            span,
        }) => {
            let typed_dst = typecheck_expr(dst)?;
            let typed_src = typecheck_expr(src)?;

            if !is_builtin_va_list_compatible_type(get_type(&typed_dst))
                || !is_builtin_va_list_compatible_type(get_type(&typed_src))
            {
                return Err(UccError {
                    msg: format!("__builtin_va_copy arguments must be va_list values"),
                    kind: ErrorKind::Typecheck,
                    span: *span,
                });
            }

            if is_builtin_va_list_object_type(get_type(&typed_dst)) && !is_lvalue(&typed_dst) {
                return Err(UccError {
                    msg: format!("__builtin_va_copy destination must be a va_list lvalue"),
                    kind: ErrorKind::Typecheck,
                    span: *span,
                });
            }

            Ok(Expression::VaCopy(VaCopyExpression {
                dst: Box::new(typed_dst),
                src: Box::new(typed_src),
                ty: Type::Void,
                span: *span,
            }))
        }

        Expression::VaEnd(VaEndExpression { list, ty: _, span }) => {
            let typed_list = typecheck_expr(list)?;
            if !is_builtin_va_list_compatible_type(get_type(&typed_list)) {
                return Err(UccError {
                    msg: format!("__builtin_va_end argument must be a va_list"),
                    kind: ErrorKind::Typecheck,
                    span: *span,
                });
            }

            Ok(Expression::VaEnd(VaEndExpression {
                list: Box::new(typed_list),
                ty: Type::Void,
                span: *span,
            }))
        }

        Expression::Call(CallExpression {
            name,
            args,
            ty: _,
            span,
        }) => {
            let f = SYMBOL_TABLE.lock().unwrap().get(name).cloned().unwrap();
            let f_type = f.ty.clone();

            match f_type {
                Type::Func {
                    params,
                    ret,
                    variadic,
                } => {
                    if (!variadic && args.len() != params.len())
                        || (variadic && args.len() < params.len())
                    {
                        return Err(UccError {
                            msg: format!("Function called with the wrong number of arguments."),
                            kind: ErrorKind::Typecheck,
                            span: *span,
                        });
                    }

                    let mut converted_args = vec![];

                    let process_arg = |arg: &Expression, param_type: &Type| -> Result<Expression> {
                        let typed_arg = typecheck_and_convert(arg)?;
                        let converted_arg = convert_by_assignment(&typed_arg, param_type)?;

                        Ok(converted_arg)
                    };

                    for (arg, param_type) in args.iter().zip(params.iter()) {
                        converted_args.push(process_arg(arg, param_type)?);
                    }

                    for arg in args.iter().skip(params.len()) {
                        converted_args.push(apply_default_argument_promotions(arg)?);
                    }

                    Ok(Expression::Call(CallExpression {
                        name: name.clone(),
                        args: converted_args,
                        ty: *ret,
                        span: *span,
                    }))
                }
                _ => {
                    return Err(UccError {
                        msg: format!("Variable used as function name"),
                        kind: ErrorKind::Typecheck,
                        span: *span,
                    })
                }
            }
        }

        Expression::Variable(VariableExpression { value, ty: _, span }) => {
            let symbol = SYMBOL_TABLE
                .lock()
                .unwrap()
                .get(value)
                .cloned()
                .ok_or_else(|| UccError {
                    msg: format!("Undeclared variable reached typechecker: {}", value),
                    kind: ErrorKind::Typecheck,
                    span: *span,
                })?;

            if let IdentifierAttrs::EnumConstantAttr(v) = symbol.attrs {
                return Ok(Expression::Constant(ConstantExpression {
                    value: Const::Int(v),
                    ty: Type::Int,
                    span: *span,
                }));
            }

            let v_type = symbol.ty;

            Ok(Expression::Variable(VariableExpression {
                value: value.clone(),
                ty: v_type,
                span: *span,
            }))
        }

        Expression::Binary(BinaryExpression {
            kind,
            lhs,
            rhs,
            ty: _,
            span,
        }) => match kind {
            BinaryExpressionKind::And | BinaryExpressionKind::Or => {
                typecheck_logical(kind, lhs, rhs, *span)
            }
            BinaryExpressionKind::Add => typecheck_addition(lhs, rhs, *span),
            BinaryExpressionKind::Sub => typecheck_subtraction(lhs, rhs, *span),
            BinaryExpressionKind::Mul | BinaryExpressionKind::Div | BinaryExpressionKind::Rem => {
                typecheck_multiplicative(kind, lhs, rhs, *span)
            }
            BinaryExpressionKind::Equal | BinaryExpressionKind::NotEqual => {
                typecheck_equality(kind, lhs, rhs, *span)
            }
            BinaryExpressionKind::Less
            | BinaryExpressionKind::LessEqual
            | BinaryExpressionKind::Greater
            | BinaryExpressionKind::GreaterEqual => typecheck_relational(kind, lhs, rhs, *span),
            BinaryExpressionKind::BitwiseOr
            | BinaryExpressionKind::BitwiseXor
            | BinaryExpressionKind::BitwiseAnd => typecheck_bitwise(kind, lhs, rhs, *span),
            BinaryExpressionKind::BitwiseShl | BinaryExpressionKind::BitwiseShr => {
                typecheck_bitshift(kind, lhs, rhs, *span)
            }
        },

        Expression::Assign(AssignExpression {
            op,
            lhs,
            rhs,
            ty: _,
            span,
        }) => {
            let typed_lhs = typecheck_and_convert(lhs)?;

            if is_lvalue(&typed_lhs) {
                let typed_rhs = typecheck_and_convert(rhs)?;
                let left_type = get_type(&typed_lhs);
                let converted_right = convert_by_assignment(&typed_rhs, left_type)?;

                Ok(Expression::Assign(AssignExpression {
                    op: op.clone(),
                    lhs: Box::new(typed_lhs.to_owned()),
                    rhs: Box::new(converted_right),
                    ty: left_type.to_owned(),
                    span: *span,
                }))
            } else {
                return Err(UccError {
                    msg: format!("Invalid lvalue in assignment"),
                    kind: ErrorKind::Typecheck,
                    span: *span,
                });
            }
        }

        Expression::Conditional(ConditionalExpression {
            condition,
            then_expr,
            else_expr,
            ty: _,
            span,
        }) => {
            let typed_condition = typecheck_scalar(condition)?;

            let typed_then_expr = typecheck_and_convert(then_expr)?;
            let typed_else_expr = typecheck_and_convert(else_expr)?;

            if !is_scalar(get_type(&typed_condition)) {
                return Err(UccError {
                    msg: format!("Non-scalar condition in scalar expression."),
                    kind: ErrorKind::Typecheck,
                    span: *span,
                });
            }

            let t1 = get_type(&typed_then_expr);
            let t2 = get_type(&typed_else_expr);

            let common_type = match (t1.clone(), t2.clone()) {
                (Type::Void, Type::Void) => Type::Void,
                (Type::Pointer(_), Type::Pointer(_)) => {
                    get_common_ptr_type(&typed_then_expr, &typed_else_expr)?
                }
                (Type::Pointer(_), _) => get_common_ptr_type(&typed_then_expr, &typed_else_expr)?,
                (_, Type::Pointer(_)) => get_common_ptr_type(&typed_then_expr, &typed_else_expr)?,
                _ => get_common_type(t1, t2).to_owned(),
            };
            let converted_then_expr = convert_to(&typed_then_expr, &common_type);
            let converted_else_expr = convert_to(&typed_else_expr, &common_type);

            Ok(Expression::Cast(CastExpression {
                target_type: common_type.clone(),
                expr: Expression::Conditional(ConditionalExpression {
                    condition: typed_condition.into(),
                    then_expr: converted_then_expr.into(),
                    else_expr: converted_else_expr.into(),
                    ty: common_type.clone(),
                    span: *span,
                })
                .into(),
                ty: common_type.to_owned(),
                span: *span,
            }))
        }

        Expression::Unary(UnaryExpression {
            kind,
            expr,
            ty: _,
            span,
        }) => match kind {
            UnaryExpressionKind::Complement => typecheck_complement(expr, *span),
            UnaryExpressionKind::Negate => typecheck_negate(expr, *span),
            UnaryExpressionKind::Not => typecheck_not(expr, *span),
            UnaryExpressionKind::Inc | UnaryExpressionKind::Dec => {
                typecheck_incr(expr, kind.to_owned(), *span)
            }
        },

        Expression::Constant(ConstantExpression { value, ty: _, span }) => match value {
            Const::Short(s) => Ok(Expression::Constant(ConstantExpression {
                value: Const::Short(*s),
                ty: Type::Short,
                span: *span,
            })),
            Const::Int(i) => Ok(Expression::Constant(ConstantExpression {
                value: Const::Int(*i),
                ty: Type::Int,
                span: *span,
            })),
            Const::Long(l) => Ok(Expression::Constant(ConstantExpression {
                value: Const::Long(*l),
                ty: Type::Long,
                span: *span,
            })),
            Const::UShort(us) => Ok(Expression::Constant(ConstantExpression {
                value: Const::UShort(*us),
                ty: Type::UShort,
                span: *span,
            })),
            Const::UInt(u) => Ok(Expression::Constant(ConstantExpression {
                value: Const::UInt(*u),
                ty: Type::UInt,
                span: *span,
            })),
            Const::ULong(ul) => Ok(Expression::Constant(ConstantExpression {
                value: Const::ULong(*ul),
                ty: Type::ULong,
                span: *span,
            })),
            Const::Float(f) => Ok(Expression::Constant(ConstantExpression {
                value: Const::Float(*f),
                ty: Type::Float,
                span: *span,
            })),
            Const::Double(d) => Ok(Expression::Constant(ConstantExpression {
                value: Const::Double(*d),
                ty: Type::Double,
                span: *span,
            })),
            Const::Char(c) => Ok(Expression::Constant(ConstantExpression {
                value: Const::Char(*c),
                ty: Type::Int,
                span: *span,
            })),
            Const::UChar(uc) => Ok(Expression::Constant(ConstantExpression {
                value: Const::UChar(*uc),
                ty: Type::UInt,
                span: *span,
            })),
        },

        Expression::Cast(CastExpression {
            target_type,
            expr,
            ty: _,
            span,
        }) => {
            validate_type_specifier(target_type)?;
            let typed_inner = typecheck_and_convert(expr)?;

            let t1 = get_type(&typed_inner);
            let t2 = target_type;

            if let Type::Array { .. } = t2 {
                return Err(UccError {
                    msg: format!("Array type in cast."),
                    kind: ErrorKind::Typecheck,
                    span: *span,
                });
            }

            if let Type::Pointer(_) = t1 {
                if is_floating_type(t2) {
                    return Err(UccError {
                        msg: format!("Pointer to floating type cast."),
                        kind: ErrorKind::Typecheck,
                        span: *span,
                    });
                }
            }

            if let Type::Pointer(_) = t2 {
                if is_floating_type(t1) {
                    return Err(UccError {
                        msg: format!("Floating type to pointer cast."),
                        kind: ErrorKind::Typecheck,
                        span: *span,
                    });
                }
            }

            if target_type == &Type::Void {
                return Ok(Expression::Cast(CastExpression {
                    target_type: target_type.clone(),
                    expr: Box::new(typed_inner),
                    ty: target_type.clone(),
                    span: *span,
                }));
            }

            if !is_scalar(target_type) {
                return Err(UccError {
                    kind: ErrorKind::Typecheck,
                    msg: format!("Non-scalar type in cast (can only cast to scalar or void)"),
                    span: Span { start: 0, end: 0 },
                });
            }

            if !is_scalar(t1) {
                return Err(UccError {
                    kind: ErrorKind::Typecheck,
                    msg: format!("Cannot cast non-scalar expression to scalar type."),
                    span: Span { start: 0, end: 0 },
                });
            }

            Ok(Expression::Cast(CastExpression {
                target_type: target_type.clone(),
                expr: Box::new(typed_inner),
                ty: target_type.clone(),
                span: *span,
            }))
        }

        Expression::Deref(DerefExpression { expr, ty: _, span }) => {
            let typed_inner = typecheck_and_convert(expr)?;

            let inner_type = get_type(&typed_inner);

            match inner_type {
                Type::Pointer(inner_type) => {
                    if inner_type == &Type::Void.into() {
                        return Err(UccError {
                            kind: ErrorKind::Typecheck,
                            msg: format!("Cannot dereference a void pointer."),
                            span: *span,
                        });
                    }
                    let deref_expr = Expression::Deref(DerefExpression {
                        expr: Box::new(typed_inner.to_owned()),
                        ty: *inner_type.to_owned(),
                        span: *span,
                    });
                    Ok(deref_expr)
                }
                _ => {
                    return Err(UccError {
                        kind: ErrorKind::Typecheck,
                        msg: format!("Cannot dereference a non-pointer type."),
                        span: *span,
                    })
                }
            }
        }

        Expression::AddrOf(AddrOfExpression { expr, ty: _, span }) => {
            if is_lvalue(expr) {
                let typed_inner = typecheck_expr(expr)?;
                let referenced_type = get_type(&typed_inner);
                Ok(Expression::AddrOf(AddrOfExpression {
                    expr: Box::new(typed_inner.to_owned()),
                    ty: Type::Pointer(Box::new(referenced_type.to_owned())),
                    span: *span,
                }))
            } else {
                return Err(UccError {
                    kind: ErrorKind::Typecheck,
                    msg: format!("Cannot take address of non-lvalue."),
                    span: *span,
                });
            }
        }

        Expression::Subscript(SubscriptExpression {
            expr,
            index,
            ty: _,
            span,
        }) => typecheck_subscript(expr, index, *span),

        Expression::String(StringExpression { value, ty: _, span }) => {
            Ok(Expression::String(StringExpression {
                value: value.clone(),
                ty: Type::Array {
                    element: Type::Char.into(),
                    size: value.len() + 1,
                },
                span: *span,
            }))
        }

        Expression::Literal(LiteralExpression {
            name,
            value,
            ty,
            span,
        }) => {
            if ty == &Type::Dummy {
                return Err(UccError {
                    kind: ErrorKind::Typecheck,
                    msg: format!("Compound literal is missing a target type."),
                    span: *span,
                });
            }

            validate_type_specifier(ty)?;
            if !is_complete(ty) {
                return Err(UccError {
                    kind: ErrorKind::Typecheck,
                    msg: format!("Compound literal has incomplete type."),
                    span: *span,
                });
            }

            Ok(Expression::Literal(LiteralExpression {
                name: name.clone(),
                value: Box::new(typecheck_init(ty, value)?),
                ty: ty.clone(),
                span: *span,
            }))
        }

        Expression::SizeofT(SizeofTExpression { t, ty: _, span }) => {
            validate_type_specifier(t)?;
            if !is_complete(t) {
                return Err(UccError {
                    kind: ErrorKind::Typecheck,
                    msg: format!("Sizeof operator applied to incomplete type."),
                    span: *span,
                });
            }
            Ok(Expression::SizeofT(SizeofTExpression {
                t: t.clone(),
                ty: Type::ULong,
                span: *span,
            }))
        }

        Expression::Sizeof(SizeofExpression { expr, ty: _, span }) => {
            let typed_inner = typecheck_expr(expr)?;
            if !is_complete(get_type(&typed_inner)) {
                return Err(UccError {
                    kind: ErrorKind::Typecheck,
                    msg: format!("Sizeof operator applied to incomplete type."),
                    span: *span,
                });
            }
            Ok(Expression::Sizeof(SizeofExpression {
                expr: Box::new(typed_inner),
                ty: Type::ULong,
                span: *span,
            }))
        }

        Expression::Dot(DotExpression {
            structure,
            member,
            ty: _,
            span,
        }) => {
            let typed_structure = typecheck_and_convert(structure)?;
            match get_type(&typed_structure) {
                Type::Struct { tag } | Type::Union { tag } => {
                    let member_def =
                        find_aggregate_member(tag, member).ok_or_else(|| UccError {
                            kind: ErrorKind::Typecheck,
                            msg: format!("Unknown aggregate member."),
                            span: *span,
                        })?;

                    Ok(Expression::Dot(DotExpression {
                        structure: Box::new(typed_structure),
                        member: member.clone(),
                        ty: member_def.ty.to_owned(),
                        span: *span,
                    }))
                }
                _ => {
                    return Err(UccError {
                        kind: ErrorKind::Typecheck,
                        msg: format!("Non aggregate type in dot expression."),
                        span: *span,
                    })
                }
            }
        }

        Expression::Arrow(ArrowExpression {
            pointer,
            member,
            ty: _,
            span,
        }) => {
            let typed_pointer = typecheck_and_convert(pointer)?;
            match get_type(&typed_pointer) {
                Type::Pointer(referenced) => {
                    if let Type::Struct { tag } | Type::Union { tag } = &**referenced {
                        let member_def =
                            find_aggregate_member(tag, member).ok_or_else(|| UccError {
                                kind: ErrorKind::Typecheck,
                                msg: format!("Unknown member in aggregate."),
                                span: *span,
                            })?;

                        Ok(Expression::Arrow(ArrowExpression {
                            pointer: Box::new(typed_pointer),
                            member: member.clone(),
                            ty: member_def.ty.to_owned(),
                            span: *span,
                        }))
                    } else {
                        return Err(UccError {
                            kind: ErrorKind::Typecheck,
                            msg: format!("Non aggregate type in arrow expression."),
                            span: *span,
                        });
                    }
                }
                _ => {
                    return Err(UccError {
                        kind: ErrorKind::Typecheck,
                        msg: format!("Non struct type in arrow expression."),
                        span: *span,
                    })
                }
            }
        }
    }
}

fn optionally_typecheck_for_init(init: ForInit) -> Result<ForInit> {
    match init {
        ForInit::Declaration(decl) => {
            let typechecked = decl.typecheck()?;
            Ok(ForInit::Declaration(typechecked))
        }
        ForInit::Expression(Some(expr)) => {
            let typechecked = typecheck_and_convert(&expr)?;
            Ok(ForInit::Expression(typechecked.into()))
        }
        _ => Ok(init.to_owned()),
    }
}

fn find_aggregate_member_in_entry(entry: &StructEntry, member_name: &str) -> Option<MemberEntry> {
    if let Some(member) = entry
        .members
        .iter()
        .find(|member| !member.name.is_empty() && member.name == member_name)
    {
        return Some(member.clone());
    }

    for member in &entry.members {
        if !member.name.is_empty() {
            continue;
        }

        if let Some(nested_entry) = aggregate_entry_for_type(&member.ty) {
            if let Some(mut nested_member) =
                find_aggregate_member_in_entry(&nested_entry, member_name)
            {
                nested_member.offset += member.offset;
                return Some(nested_member);
            }
        }
    }

    None
}

fn find_aggregate_member(tag: &str, member_name: &str) -> Option<MemberEntry> {
    let entry = TYPE_TABLE.lock().unwrap().get(tag).cloned()?;
    find_aggregate_member_in_entry(&entry, member_name)
}

fn initializer_designator_name(init: &Initializer) -> Option<&str> {
    match init {
        Initializer::Single(name, _) | Initializer::Compound(name, _, _) if !name.is_empty() => {
            Some(name.as_str())
        }
        _ => None,
    }
}

fn initializer_with_name(name: &str, init: &Initializer) -> Initializer {
    match init {
        Initializer::Single(_, expr) => Initializer::Single(name.to_string(), expr.clone()),
        Initializer::Compound(_, ty, elems) => {
            Initializer::Compound(name.to_string(), ty.clone(), elems.clone())
        }
    }
}

fn unwrap_untyped_braced_initializer(name: &str, lit: &LiteralExpression) -> Option<Initializer> {
    if lit.ty == Type::Dummy {
        Some(initializer_with_name(name, &lit.value))
    } else {
        None
    }
}

fn aggregate_entry_for_type(t: &Type) -> Option<StructEntry> {
    match t {
        Type::Struct { tag } | Type::Union { tag } => TYPE_TABLE.lock().unwrap().get(tag).cloned(),
        _ => None,
    }
}

fn find_visible_member_in_entry(entry: &StructEntry, member_name: &str) -> Option<MemberEntry> {
    // First: directly declared member.
    if let Some(member) = entry
        .members
        .iter()
        .find(|member| !member.name.is_empty() && member.name == member_name)
    {
        return Some(member.clone());
    }

    // Then: members promoted through anonymous struct/union members.
    for member in &entry.members {
        if !member.name.is_empty() {
            continue;
        }

        let Some(nested_entry) = aggregate_entry_for_type(&member.ty) else {
            continue;
        };

        if let Some(mut nested_member) = find_visible_member_in_entry(&nested_entry, member_name) {
            nested_member.offset += member.offset;
            return Some(nested_member);
        }
    }

    None
}

fn member_type_contains_designator(member_ty: &Type, member_name: &str) -> bool {
    aggregate_entry_for_type(member_ty)
        .and_then(|entry| find_visible_member_in_entry(&entry, member_name))
        .is_some()
}

fn find_designated_member_index(members: &[MemberEntry], member_name: &str) -> Option<usize> {
    // Direct designator: `.field = ...`
    if let Some(index) = members
        .iter()
        .position(|member| !member.name.is_empty() && member.name == member_name)
    {
        return Some(index);
    }

    // Anonymous aggregate designator:
    //
    // struct Outer {
    //     union { int reg; long imm; };
    //     int tag;
    // };
    //
    // struct Outer x = { .reg = 1 };
    //
    // `.reg` should select the anonymous union slot.
    members.iter().position(|member| {
        member.name.is_empty() && member_type_contains_designator(&member.ty, member_name)
    })
}

fn is_zero_initializer_expr(expr: &Expression) -> bool {
    match expr {
        Expression::Constant(ConstantExpression { value, .. }) => matches!(
            value,
            Const::Short(0)
                | Const::Int(0)
                | Const::Long(0)
                | Const::UShort(0)
                | Const::UInt(0)
                | Const::ULong(0)
                | Const::Char(0)
                | Const::UChar(0)
        ),
        Expression::Cast(CastExpression { expr, .. }) => is_zero_initializer_expr(expr),
        _ => false,
    }
}

fn is_aggregate_type(t: &Type) -> bool {
    matches!(
        t,
        Type::Array { .. } | Type::Struct { .. } | Type::Union { .. }
    )
}

fn initializer_for_selected_member(member: &MemberEntry, init: &Initializer) -> Initializer {
    // C permits zero-initializing an aggregate member with a scalar zero through
    // brace elision, e.g.:
    //
    //     struct Outer { struct Inner inner; int tag; } x = {0};
    //
    // The outer struct selects `inner` for the `0`.  Passing that scalar `0`
    // directly to typecheck_init(&Inner, ...) fails with "cannot convert".
    // Since the scalar is zero, the result is exactly the same as a fully-zeroed
    // initializer for the selected aggregate member.
    if is_aggregate_type(&member.ty) {
        if let Initializer::Single(_, expr) = init {
            if is_zero_initializer_expr(expr) {
                return Initializer::zero(&member.ty);
            }
        }
    }

    let Some(member_name) = initializer_designator_name(init) else {
        return init.clone();
    };

    // If `.reg = 1` selected an anonymous union/struct member, recurse into that
    // anonymous aggregate with a compound initializer that still contains `.reg`.
    //
    // Without this wrapping, typecheck_init sees:
    //
    //     target type: anonymous union
    //     initializer: Single("reg", ...)
    //
    // and rejects it as "single initializer for aggregate type".
    if member.name.is_empty() && member_type_contains_designator(&member.ty, member_name) {
        Initializer::Compound(String::new(), member.ty.clone(), vec![init.clone()])
    } else {
        init.clone()
    }
}

fn typecheck_struct_init_with_designators(
    tag: &str,
    name: &str,
    compound_init: &[Initializer],
) -> Result<Initializer> {
    let struct_def = TYPE_TABLE.lock().unwrap().get(tag).unwrap().clone();

    if compound_init.len() > struct_def.members.len() {
        return Err(UccError {
            kind: ErrorKind::Typecheck,
            msg: format!("Too many initiailezers."),
            span: Span { start: 0, end: 0 },
        });
    }

    let mut typechecked_inits = struct_def
        .members
        .iter()
        .map(|member| Initializer::zero(&member.ty))
        .collect::<Vec<_>>();
    let mut next_member = 0usize;

    for init_elem in compound_init {
        let member_index = if let Some(member_name) = initializer_designator_name(init_elem) {
            find_designated_member_index(&struct_def.members, member_name).ok_or_else(|| {
                UccError {
                    kind: ErrorKind::Typecheck,
                    msg: format!(
                        "Unknown aggregate member `{}` in designated initializer.",
                        member_name
                    ),
                    span: Span { start: 0, end: 0 },
                }
            })?
        } else {
            let idx = next_member;
            if idx >= struct_def.members.len() {
                return Err(UccError {
                    kind: ErrorKind::Typecheck,
                    msg: format!("Too many initiailezers."),
                    span: Span { start: 0, end: 0 },
                });
            }
            idx
        };

        let member = &struct_def.members[member_index];
        let init_for_member = initializer_for_selected_member(member, init_elem);

        typechecked_inits[member_index] = typecheck_init(&member.ty, &init_for_member)?;

        next_member = member_index + 1;
    }

    Ok(Initializer::Compound(
        name.to_string(),
        Type::Struct {
            tag: tag.to_string(),
        },
        typechecked_inits,
    ))
}

fn typecheck_union_init_with_designator(
    tag: &str,
    name: &str,
    compound_init: &[Initializer],
) -> Result<Initializer> {
    let union_def = TYPE_TABLE.lock().unwrap().get(tag).unwrap().clone();

    if compound_init.len() > 1 {
        return Err(UccError {
            kind: ErrorKind::Typecheck,
            msg: format!("Too many initiailezers."),
            span: Span { start: 0, end: 0 },
        });
    }

    let Some(init_elem) = compound_init.first() else {
        let first_member = union_def.members.first().unwrap();
        return Ok(Initializer::Compound(
            name.to_string(),
            Type::Union {
                tag: tag.to_string(),
            },
            vec![Initializer::zero(&first_member.ty)],
        ));
    };

    let member = if let Some(member_name) = initializer_designator_name(init_elem) {
        let member_index = find_designated_member_index(&union_def.members, member_name)
            .ok_or_else(|| UccError {
                kind: ErrorKind::Typecheck,
                msg: format!(
                    "Unknown aggregate member `{}` in designated initializer.",
                    member_name
                ),
                span: Span { start: 0, end: 0 },
            })?;
        &union_def.members[member_index]
    } else {
        union_def.members.first().unwrap()
    };

    let init_for_member = initializer_for_selected_member(member, init_elem);

    Ok(Initializer::Compound(
        name.to_string(),
        Type::Union {
            tag: tag.to_string(),
        },
        vec![typecheck_init(&member.ty, &init_for_member)?],
    ))
}

fn typecheck_init(target_type: &Type, init: &Initializer) -> Result<Initializer> {
    match (target_type, init) {
        (_, Initializer::Single(name, Expression::Literal(lit))) => {
            if let Some(nested_init) = unwrap_untyped_braced_initializer(name, lit) {
                return typecheck_init(target_type, &nested_init);
            }

            let typechecked_expr = typecheck_and_convert(&Expression::Literal(lit.clone()))?;
            let converted_expr = convert_by_assignment(&typechecked_expr, target_type)?;
            Ok(Initializer::Single(name.clone(), converted_expr))
        }
        (_, Initializer::Compound(name, _, inits))
            if is_scalar(target_type) && inits.len() == 1 =>
        {
            let inner = initializer_with_name(name, &inits[0]);
            typecheck_init(target_type, &inner)
        }
        (
            Type::Array { element, size },
            Initializer::Single(
                name,
                Expression::String(StringExpression {
                    value,
                    ty: _,
                    span: _,
                }),
            ),
        ) => {
            if !is_char_type(element) {
                return Err(UccError {
                    kind: ErrorKind::Typecheck,
                    msg: format!("Cannot init array with non-char type."),
                    span: Span { start: 0, end: 0 },
                });
            }

            if value.len() > *size {
                return Err(UccError {
                    kind: ErrorKind::Typecheck,
                    msg: format!("String too long for array."),
                    span: Span { start: 0, end: 0 },
                });
            }

            Ok(Initializer::Single(
                name.to_owned(),
                Expression::String(StringExpression {
                    value: value.clone(),
                    ty: target_type.clone(),
                    span: Span { start: 0, end: 0 },
                }),
            ))
        }
        (Type::Struct { tag }, Initializer::Compound(name, _, compound_init)) => {
            typecheck_struct_init_with_designators(tag, name, compound_init)
        }
        (Type::Union { tag }, Initializer::Compound(name, _, compound_init)) => {
            typecheck_union_init_with_designator(tag, name, compound_init)
        }
        (_, Initializer::Single(name, expr)) => {
            let typechecked_expr = typecheck_and_convert(expr)?;
            let converted_expr = convert_by_assignment(&typechecked_expr, target_type)?;
            Ok(Initializer::Single(name.clone(), converted_expr))
        }
        (Type::Array { element, size }, Initializer::Compound(name, _, inits)) => {
            if inits.len() > *size {
                return Err(UccError {
                    kind: ErrorKind::Typecheck,
                    msg: format!("Too many initiailezers."),
                    span: Span { start: 0, end: 0 },
                });
            }

            let mut typechecked_inits = vec![];

            for init in inits.iter() {
                let typechecked_init = typecheck_init(element, init)?;
                typechecked_inits.push(typechecked_init);
            }

            while typechecked_inits.len() < *size {
                typechecked_inits.push(Initializer::zero(element));
            }

            Ok(Initializer::Compound(
                name.clone(),
                *element.clone(),
                typechecked_inits,
            ))
        }
        _ => {
            return Err(UccError {
                kind: ErrorKind::Typecheck,
                msg: format!("Cannot init a scalar object with compound init."),
                span: Span { start: 0, end: 0 },
            })
        }
    }
}

fn typecheck_logical(
    kind: &BinaryExpressionKind,
    lhs: &Expression,
    rhs: &Expression,
    span: Span,
) -> Result<Expression> {
    let typed_lhs = typecheck_scalar(lhs)?;
    let typed_rhs = typecheck_scalar(rhs)?;

    Ok(Expression::Binary(BinaryExpression {
        kind: *kind,
        lhs: Box::new(typed_lhs),
        rhs: Box::new(typed_rhs),
        ty: Type::Int,
        span,
    }))
}

fn typecheck_addition(lhs: &Expression, rhs: &Expression, span: Span) -> Result<Expression> {
    let typed_lhs = typecheck_and_convert(lhs)?;
    let typed_rhs = typecheck_and_convert(rhs)?;

    if is_arithmetic(get_type(&typed_lhs)) && is_arithmetic(get_type(&typed_rhs)) {
        let common_type = get_common_type(get_type(&typed_lhs), get_type(&typed_rhs));

        let converted_lhs = convert_to(&typed_lhs, common_type);
        let converted_rhs = convert_to(&typed_rhs, common_type);

        Ok(Expression::Binary(BinaryExpression {
            kind: BinaryExpressionKind::Add,
            lhs: Box::new(converted_lhs),
            rhs: Box::new(converted_rhs),
            ty: common_type.to_owned(),
            span,
        }))
    } else if is_ptr_to_complete(get_type(&typed_lhs)) && is_integer_type(get_type(&typed_rhs)) {
        let converted_rhs = convert_to(&typed_rhs, &Type::Long);

        Ok(Expression::Binary(BinaryExpression {
            kind: BinaryExpressionKind::Add,
            lhs: Box::new(typed_lhs.clone()),
            rhs: Box::new(converted_rhs),
            ty: get_type(&typed_lhs).to_owned(),
            span,
        }))
    } else if is_ptr_to_complete(get_type(&typed_rhs)) && is_integer_type(get_type(&typed_lhs)) {
        let converted_lhs = convert_to(&typed_lhs, &Type::Long);

        Ok(Expression::Binary(BinaryExpression {
            kind: BinaryExpressionKind::Add,
            lhs: Box::new(converted_lhs),
            rhs: Box::new(typed_rhs.clone()),
            ty: get_type(&typed_rhs).to_owned(),
            span,
        }))
    } else {
        return Err(UccError {
            kind: ErrorKind::Typecheck,
            msg: format!("Invalid operands for addition."),
            span,
        });
    }
}

fn typecheck_subtraction(lhs: &Expression, rhs: &Expression, span: Span) -> Result<Expression> {
    let typed_lhs = typecheck_and_convert(lhs)?;
    let typed_rhs = typecheck_and_convert(rhs)?;

    let t1 = get_type(&typed_lhs);
    let t2 = get_type(&typed_rhs);

    if is_arithmetic(t1) && is_arithmetic(t2) {
        let common_type = get_common_type(t1, t2);
        let converted_lhs = convert_to(&typed_lhs, common_type);
        let converted_rhs = convert_to(&typed_rhs, common_type);

        Ok(Expression::Binary(BinaryExpression {
            kind: BinaryExpressionKind::Sub,
            lhs: Box::new(converted_lhs),
            rhs: Box::new(converted_rhs),
            ty: common_type.to_owned(),
            span,
        }))
    } else if is_ptr_to_complete(t1) && is_integer_type(t2) {
        let converted_rhs = convert_to(&typed_rhs, &Type::Long);

        Ok(Expression::Binary(BinaryExpression {
            kind: BinaryExpressionKind::Sub,
            lhs: Box::new(typed_lhs.clone()),
            rhs: Box::new(converted_rhs),
            ty: t1.to_owned(),
            span,
        }))
    } else if is_ptr_to_complete(t1) && get_type(&typed_lhs) == get_type(&typed_rhs) {
        Ok(Expression::Binary(BinaryExpression {
            kind: BinaryExpressionKind::Sub,
            lhs: Box::new(typed_lhs.clone()),
            rhs: Box::new(typed_rhs.clone()),
            ty: Type::Long,
            span,
        }))
    } else {
        return Err(UccError {
            kind: ErrorKind::Typecheck,
            msg: format!("Invalid operands for subtraction."),
            span,
        });
    }
}

fn typecheck_multiplicative(
    kind: &BinaryExpressionKind,
    lhs: &Expression,
    rhs: &Expression,
    span: Span,
) -> Result<Expression> {
    let typed_lhs = typecheck_and_convert(lhs)?;
    let typed_rhs = typecheck_and_convert(rhs)?;

    let t1 = get_type(&typed_lhs);
    let t2 = get_type(&typed_rhs);

    if is_arithmetic(t1) && is_arithmetic(t2) {
        let common_type = get_common_type(t1, t2);
        let converted_lhs = convert_to(&typed_lhs, common_type);
        let converted_rhs = convert_to(&typed_rhs, common_type);

        match kind {
            BinaryExpressionKind::Rem if is_floating_type(common_type) => {
                return Err(UccError {
                    kind: ErrorKind::Typecheck,
                    msg: format!("Rem can't be applied to float types."),
                    span,
                });
            }
            BinaryExpressionKind::Mul | BinaryExpressionKind::Div | BinaryExpressionKind::Rem => {
                Ok(Expression::Binary(BinaryExpression {
                    kind: *kind,
                    lhs: Box::new(converted_lhs),
                    rhs: Box::new(converted_rhs),
                    ty: common_type.to_owned(),
                    span,
                }))
            }
            _ => unreachable!(),
        }
    } else {
        return Err(UccError {
            kind: ErrorKind::Typecheck,
            msg: format!("Invalid operands for remainder."),
            span,
        });
    }
}

fn typecheck_equality(
    kind: &BinaryExpressionKind,
    lhs: &Expression,
    rhs: &Expression,
    span: Span,
) -> Result<Expression> {
    let typed_lhs = typecheck_and_convert(lhs)?;
    let typed_rhs = typecheck_and_convert(rhs)?;

    let t1 = get_type(&typed_lhs);
    let t2 = get_type(&typed_rhs);

    let common_type = if is_pointer_type(t1) || is_pointer_type(t2) {
        get_common_ptr_type(&typed_lhs, &typed_rhs)?
    } else if is_arithmetic(t1) && is_arithmetic(t2) {
        get_common_type(t1, t2).to_owned()
    } else {
        return Err(UccError {
            kind: ErrorKind::Typecheck,
            msg: format!("Invalid operands for equality."),
            span,
        });
    };

    let converted_lhs = convert_to(&typed_lhs, &common_type);
    let converted_rhs = convert_to(&typed_rhs, &common_type);

    Ok(Expression::Binary(BinaryExpression {
        kind: *kind,
        lhs: Box::new(converted_lhs),
        rhs: Box::new(converted_rhs),
        ty: Type::Int,
        span,
    }))
}

fn typecheck_relational(
    kind: &BinaryExpressionKind,
    lhs: &Expression,
    rhs: &Expression,
    span: Span,
) -> Result<Expression> {
    let typed_lhs = typecheck_and_convert(lhs)?;
    let typed_rhs = typecheck_and_convert(rhs)?;

    let t1 = get_type(&typed_lhs);
    let t2 = get_type(&typed_rhs);

    let common_type = if is_arithmetic(t1) && is_arithmetic(t2) {
        get_common_type(t1, t2)
    } else if is_pointer_type(t1) && t1 == t2 {
        t2
    } else {
        return Err(UccError {
            kind: ErrorKind::Typecheck,
            msg: format!("Invalid operands for rel op."),
            span,
        });
    };

    let converted_lhs = convert_to(&typed_lhs, common_type);
    let converted_rhs = convert_to(&typed_rhs, common_type);

    Ok(Expression::Binary(BinaryExpression {
        kind: *kind,
        lhs: Box::new(converted_lhs),
        rhs: Box::new(converted_rhs),
        ty: Type::Int,
        span,
    }))
}

fn typecheck_not(expr: &Expression, span: Span) -> Result<Expression> {
    let typed_expr = typecheck_and_convert(expr)?;

    if !is_scalar(get_type(&typed_expr)) {
        return Err(UccError {
            kind: ErrorKind::Typecheck,
            msg: format!("Invalid operands for logical not."),
            span,
        });
    }

    Ok(Expression::Unary(UnaryExpression {
        kind: UnaryExpressionKind::Not,
        expr: Box::new(typed_expr),
        ty: Type::Int,
        span,
    }))
}

fn typecheck_complement(expr: &Expression, span: Span) -> Result<Expression> {
    let typed_expr = typecheck_and_convert(expr)?;

    let t = get_type(&typed_expr);

    if !is_integer_type(t) {
        return Err(UccError {
            kind: ErrorKind::Typecheck,
            msg: format!("Invalid operands for bitwise complement."),
            span,
        });
    }

    if is_small_integer_type(t) {
        let typed_expr = convert_to(&typed_expr, &Type::Int);
        return Ok(Expression::Unary(UnaryExpression {
            kind: UnaryExpressionKind::Complement,
            expr: Box::new(typed_expr),
            ty: Type::Int,
            span,
        }));
    }

    Ok(Expression::Unary(UnaryExpression {
        kind: UnaryExpressionKind::Complement,
        expr: Box::new(typed_expr.clone()),
        ty: t.to_owned(),
        span,
    }))
}

fn typecheck_negate(expr: &Expression, span: Span) -> Result<Expression> {
    let typed_expr = typecheck_and_convert(expr)?;

    let inner_t = get_type(&typed_expr);

    let typed_expr = if is_arithmetic(inner_t) {
        if is_small_integer_type(inner_t) {
            convert_to(&typed_expr, &Type::Int)
        } else {
            typed_expr.clone()
        }
    } else {
        return Err(UccError {
            kind: ErrorKind::Typecheck,
            msg: format!("Invalid operands for negation."),
            span,
        });
    };

    Ok(Expression::Unary(UnaryExpression {
        kind: UnaryExpressionKind::Negate,
        expr: Box::new(typed_expr.clone()),
        ty: get_type(&typed_expr).to_owned(),
        span,
    }))
}

fn typecheck_bitwise(
    kind: &BinaryExpressionKind,
    lhs: &Expression,
    rhs: &Expression,
    span: Span,
) -> Result<Expression> {
    let typed_lhs = typecheck_and_convert(lhs)?;
    let typed_rhs = typecheck_and_convert(rhs)?;

    let lhs_type = get_type(&typed_lhs);
    let rhs_type = get_type(&typed_rhs);

    if !(is_integer_type(lhs_type) && is_integer_type(rhs_type)) {
        return Err(UccError {
            kind: ErrorKind::Typecheck,
            msg: format!("Both operands in a bitwise op must be ints."),
            span,
        });
    }

    let common_type = get_common_type(lhs_type, rhs_type);

    let converted_lhs = convert_to(&typed_lhs, common_type);
    let converted_rhs = convert_to(&typed_rhs, common_type);

    Ok(Expression::Binary(BinaryExpression {
        kind: *kind,
        lhs: converted_lhs.into(),
        rhs: converted_rhs.into(),
        ty: common_type.clone(),
        span,
    }))
}

fn typecheck_bitshift(
    kind: &BinaryExpressionKind,
    lhs: &Expression,
    rhs: &Expression,
    span: Span,
) -> Result<Expression> {
    let typed_lhs = typecheck_and_convert(lhs)?;
    let typed_rhs = typecheck_and_convert(rhs)?;

    let lhs_type = get_type(&typed_lhs);
    let rhs_type = get_type(&typed_rhs);

    if !(is_integer_type(lhs_type) && is_integer_type(rhs_type)) {
        return Err(UccError {
            kind: ErrorKind::Typecheck,
            msg: format!("Both operands in a bitshift op must be ints."),
            span,
        });
    }

    let typed_lhs = if is_small_integer_type(lhs_type) {
        convert_to(&typed_lhs, &Type::Int)
    } else {
        typed_lhs.clone()
    };

    let typed_rhs = if is_small_integer_type(rhs_type) {
        convert_to(&typed_rhs, &Type::Int)
    } else {
        typed_rhs.clone()
    };

    Ok(Expression::Binary(BinaryExpression {
        kind: *kind,
        lhs: typed_lhs.clone().into(),
        rhs: typed_rhs.into(),
        ty: get_type(&typed_lhs).clone(),
        span,
    }))
}

fn typecheck_incr(expr: &Expression, kind: UnaryExpressionKind, span: Span) -> Result<Expression> {
    let typed_expr = typecheck_and_convert(expr)?;

    if is_lvalue(&typed_expr)
        && (is_arithmetic(get_type(&typed_expr)) || is_ptr_to_complete(get_type(&typed_expr)))
    {
        return Ok(Expression::Unary(UnaryExpression {
            kind,
            expr: typed_expr.clone().into(),
            ty: get_type(&typed_expr).to_owned(),
            span,
        }));
    }

    return Err(UccError {
        kind: ErrorKind::Typecheck,
        msg: format!("operand of ++/-- must be an lvalue with arithemtic or ptr type"),
        span,
    });
}

fn typecheck_postfix_inc(expr: &Expression, span: Span) -> Result<Expression> {
    let typed_expr = typecheck_and_convert(expr)?;

    if is_lvalue(&typed_expr)
        && (is_arithmetic(get_type(&typed_expr)) || is_ptr_to_complete(get_type(&typed_expr)))
    {
        return Ok(Expression::Postfix(PostfixExpression {
            expr: typed_expr.clone().into(),
            kind: PostfixExpressionKind::Inc,
            ty: get_type(&typed_expr).to_owned(),
            span,
        }));
    }

    return Err(UccError {
        kind: ErrorKind::Typecheck,
        msg: format!("operand of postfix ++ must be an lvalue with arithemtic or ptr type"),
        span,
    });
}

fn typecheck_postfix_dec(expr: &Expression, span: Span) -> Result<Expression> {
    let typed_expr = typecheck_and_convert(expr)?;

    if is_lvalue(&typed_expr)
        && (is_arithmetic(get_type(&typed_expr)) || is_ptr_to_complete(get_type(&typed_expr)))
    {
        return Ok(Expression::Postfix(PostfixExpression {
            expr: typed_expr.clone().into(),
            kind: PostfixExpressionKind::Dec,
            ty: get_type(&typed_expr).to_owned(),
            span,
        }));
    }

    return Err(UccError {
        kind: ErrorKind::Typecheck,
        msg: format!("operand of postfix -- must be an lvalue with arithemtic or ptr type"),
        span,
    });
}

fn typecheck_subscript(expr: &Expression, index: &Expression, span: Span) -> Result<Expression> {
    let typed_e1 = typecheck_and_convert(expr)?;
    let typed_e2 = typecheck_and_convert(index)?;

    let t1 = get_type(&typed_e1);
    let t2 = get_type(&typed_e2);

    let (ptr_type, converted_lhs, converted_rhs) = if is_ptr_to_complete(t1) && is_integer_type(t2)
    {
        (t1, typed_e1.clone(), convert_to(&typed_e2, &Type::Long))
    } else if is_ptr_to_complete(t2) && is_integer_type(t1) {
        (t2, convert_to(&typed_e1, &Type::Long), typed_e2.clone())
    } else {
        return Err(UccError {
            kind: ErrorKind::Typecheck,
            msg: format!("Invalid operands for subscript."),
            span: spanof(&index),
        });
    };

    let result_type = match ptr_type {
        Type::Pointer(ptr_type) => ptr_type,
        _ => unreachable!(),
    };

    Ok(Expression::Subscript(SubscriptExpression {
        expr: Box::new(converted_lhs),
        index: Box::new(converted_rhs),
        ty: *result_type.clone(),
        span,
    }))
}

fn typecheck_compound(
    kind: &BinaryExpressionKind,
    lhs: &Expression,
    rhs: &Expression,
    span: Span,
) -> Result<Expression> {
    let typed_lhs = typecheck_and_convert(lhs)?;
    if is_lvalue(&typed_lhs) {
        let lhs_type = get_type(&typed_lhs);
        let typed_rhs = typecheck_and_convert(rhs)?;
        let rhs_type = get_type(&typed_rhs);

        match kind {
            BinaryExpressionKind::Rem
            | BinaryExpressionKind::BitwiseAnd
            | BinaryExpressionKind::BitwiseOr
            | BinaryExpressionKind::BitwiseXor
            | BinaryExpressionKind::BitwiseShl
            | BinaryExpressionKind::BitwiseShr => {
                if !is_integer_type(lhs_type) || !is_integer_type(rhs_type) {
                    return Err(UccError {
                        kind: ErrorKind::Typecheck,
                        msg: format!("Operator only supports integer types."),
                        span,
                    });
                }
            }
            BinaryExpressionKind::Mul | BinaryExpressionKind::Div => {
                if !is_arithmetic(lhs_type) || !is_arithmetic(rhs_type) {
                    return Err(UccError {
                        kind: ErrorKind::Typecheck,
                        msg: format!("Operator only supports arithmetic types."),
                        span,
                    });
                }
            }
            BinaryExpressionKind::Add | BinaryExpressionKind::Sub => {
                if !((is_arithmetic(lhs_type) && is_arithmetic(rhs_type))
                    || (is_ptr_to_complete(lhs_type) && is_integer_type(rhs_type)))
                {
                    return Err(UccError {
                        kind: ErrorKind::Typecheck,
                        msg: format!("Invalid types for += / -="),
                        span,
                    });
                }
            }
            _ => (),
        }

        let (result_t, converted_rhs) = {
            if kind == &BinaryExpressionKind::BitwiseShl
                || kind == &BinaryExpressionKind::BitwiseShr
            {
                let lhs_type = if is_small_integer_type(lhs_type) {
                    Type::Int
                } else {
                    lhs_type.clone()
                };
                let converted_rhs = if is_small_integer_type(get_type(&typed_rhs)) {
                    convert_to(&typed_rhs, &Type::Int)
                } else {
                    typed_rhs.clone()
                };

                (lhs_type.clone(), converted_rhs.clone())
            } else if is_pointer_type(lhs_type) {
                (lhs_type.clone(), convert_to(&typed_rhs, &Type::Long))
            } else {
                let common_type = get_common_type(lhs_type, rhs_type);
                (common_type.clone(), convert_to(&typed_rhs, common_type))
            }
        };

        Ok(Expression::Compound(CompoundExpression {
            kind: (*kind).into(),
            lhs: typed_lhs.clone().into(),
            rhs: converted_rhs.into(),
            result_t,
            ty: lhs_type.to_owned(),
            span,
        }))
    } else {
        return Err(UccError {
            kind: ErrorKind::Typecheck,
            msg: format!("lhs of compound assignment must be lvalue"),
            span,
        });
    }
}

pub fn typecheck_and_convert(e: &Expression) -> Result<Expression> {
    let typed_expr = typecheck_expr(e)?;
    let type_of_expr = get_type(&typed_expr);

    match type_of_expr {
        Type::Array { element, .. } => Ok(Expression::AddrOf(AddrOfExpression {
            expr: typed_expr.to_owned().into(),
            ty: Type::Pointer(element.to_owned()),
            span: spanof(e),
        })),

        // In most expression contexts, a function designator converts to a
        // pointer to that function. This is required for code such as:
        //
        //     qsort(base, n, size, compare_fn);
        //
        // where the fourth argument has function-pointer type and
        // `compare_fn` is written without an explicit `&`.
        Type::Func { .. } => Ok(Expression::AddrOf(AddrOfExpression {
            expr: typed_expr.to_owned().into(),
            ty: Type::Pointer(Box::new(type_of_expr.to_owned())),
            span: spanof(e),
        })),

        Type::Struct { .. } | Type::Union { .. } => {
            if !is_complete(type_of_expr) {
                return Err(UccError {
                    kind: ErrorKind::Typecheck,
                    msg: format!("Unknown aggregate type."),
                    span: spanof(e),
                });
            }
            Ok(typed_expr)
        }

        _ => Ok(typed_expr),
    }
}

fn convert_by_assignment(e: &Expression, target_type: &Type) -> Result<Expression> {
    if get_type(e) == target_type {
        Ok(e.clone())
    } else if (is_arithmetic(get_type(e)) && is_arithmetic(target_type))
        || (is_null_ptr_constant(e) && is_pointer_type(target_type))
        || (target_type == &Type::Pointer(Type::Void.into()) && is_pointer_type(get_type(e)))
        || (is_pointer_type(target_type) && get_type(e) == &Type::Pointer(Type::Void.into()))
    {
        Ok(convert_to(e, target_type))
    } else {
        return Err(UccError {
            kind: ErrorKind::Typecheck,
            msg: format!("cannot convert"),
            span: spanof(e),
        });
    }
}

fn apply_default_argument_promotions(arg: &Expression) -> Result<Expression> {
    let typed_arg = typecheck_and_convert(arg)?;
    let arg_type = get_type(&typed_arg);

    if is_small_integer_type(arg_type) {
        Ok(convert_to(&typed_arg, &Type::Int))
    } else if arg_type == &Type::Float {
        Ok(convert_to(&typed_arg, &Type::Double))
    } else {
        Ok(typed_arg)
    }
}

pub fn convert_to(e: &Expression, ty: &Type) -> Expression {
    if get_type(e) == ty {
        return e.clone();
    }
    Expression::Cast(CastExpression {
        target_type: ty.clone(),
        expr: Box::new(e.clone()),
        ty: ty.clone(),
        span: spanof(e),
    })
}

fn get_common_ptr_type<'a>(e1: &'a Expression, e2: &'a Expression) -> Result<Type> {
    let e1_t = get_type(e1);
    let e2_t = get_type(e2);

    if e1_t == e2_t {
        Ok(e1_t.to_owned())
    } else if is_null_ptr_constant(e1) {
        Ok(e2_t.to_owned())
    } else if is_null_ptr_constant(e2) {
        Ok(e1_t.to_owned())
    } else if e1_t == &Type::Pointer(Type::Void.into()) && is_pointer_type(e2_t)
        || e2_t == &Type::Pointer(Type::Void.into()) && is_pointer_type(e1_t)
    {
        Ok(Type::Pointer(Type::Void.into()))
    } else {
        return Err(UccError {
            msg: format!("Incompatible pointer types"),
            kind: ErrorKind::Typecheck,
            span: Span { start: 0, end: 0 },
        });
    }
}

pub fn get_common_type<'a>(mut type1: &'a Type, mut type2: &'a Type) -> &'a Type {
    if is_small_integer_type(type1) {
        type1 = &Type::Int;
    }

    if is_small_integer_type(type2) {
        type2 = &Type::Int;
    }

    if type1 == type2 {
        return type1;
    }

    if type1 == &Type::Double || type2 == &Type::Double {
        return &Type::Double;
    }

    if type1 == &Type::Float || type2 == &Type::Float {
        return &Type::Float;
    }

    if get_size_of_type(type1) == get_size_of_type(type2) {
        if get_signedness(type1) {
            return type2;
        } else {
            return type1;
        }
    }

    if get_size_of_type(type1) > get_size_of_type(type2) {
        type1
    } else {
        type2
    }
}

pub fn get_size_of_type(t: &Type) -> usize {
    match t {
        Type::Char | Type::UChar | Type::SChar => 1,
        Type::Short | Type::UShort => 2,
        Type::Int => 4,
        Type::Enum { .. } => 4,
        Type::UInt => 4,
        Type::Long => 8,
        Type::ULong => 8,
        Type::Float => 4,
        Type::Double => 8,
        Type::Pointer(_) => 8,
        Type::Array { element, size } => get_size_of_type(element) * size,
        Type::Struct { tag } | Type::Union { tag } => {
            let aggregate_def = TYPE_TABLE.lock().unwrap().get(tag).unwrap().clone();
            aggregate_def.size
        }
        _ => {
            unreachable!()
        }
    }
}

pub fn get_signedness(t: &Type) -> bool {
    match t {
        Type::Short => true,
        Type::UShort => false,
        Type::Int | Type::Enum { .. } => true,
        Type::UInt => false,
        Type::Long => true,
        Type::ULong => false,
        Type::Pointer(_) => false,
        Type::Char | Type::SChar => true,
        Type::UChar => false,
        _ => unreachable!(),
    }
}

pub fn get_type(e: &Expression) -> &Type {
    match e {
        Expression::Assign(assign) => &assign.ty,
        Expression::Binary(binary) => &binary.ty,
        Expression::Call(call) => &call.ty,
        Expression::VaStart(va_start) => &va_start.ty,
        Expression::VaArg(va_arg) => &va_arg.ty,
        Expression::VaCopy(va_copy) => &va_copy.ty,
        Expression::VaEnd(va_end) => &va_end.ty,
        Expression::Cast(cast) => &cast.ty,
        Expression::Conditional(conditional) => &conditional.ty,
        Expression::Constant(constant) => &constant.ty,
        Expression::Unary(unary) => &unary.ty,
        Expression::Variable(variable) => &variable.ty,
        Expression::Deref(deref) => &deref.ty,
        Expression::AddrOf(addr_of) => &addr_of.ty,
        Expression::Literal(literal) => &literal.ty,
        Expression::Subscript(subscript) => &subscript.ty,
        Expression::String(string) => &string.ty,
        Expression::Sizeof(sizeof) => &sizeof.ty,
        Expression::SizeofT(sizeof_t) => &sizeof_t.ty,
        Expression::Arrow(arrow) => &arrow.ty,
        Expression::Dot(dot) => &dot.ty,
        Expression::Postfix(postfix) => &postfix.ty,
        Expression::Compound(compound) => &compound.ty,
    }
}

fn typecheck_scalar(e: &Expression) -> Result<Expression> {
    let typechecked_expr = typecheck_and_convert(e)?;
    if is_scalar(get_type(&typechecked_expr)) {
        Ok(typechecked_expr)
    } else {
        return Err(UccError {
            msg: format!("Expected a scalar expression, got non-scalar"),
            kind: ErrorKind::Typecheck,
            span: Span { start: 0, end: 0 },
        });
    }
}

fn optionally_typecheck_scalar(e: &Option<Expression>) -> Result<Option<Expression>> {
    match e {
        Some(expr) => {
            let typechecked_expr = typecheck_scalar(expr)?;
            Ok(Some(typechecked_expr))
        }
        None => Ok(None),
    }
}

fn is_floating_type(t: &Type) -> bool {
    matches!(t, Type::Float | Type::Double)
}

fn is_arithmetic(t: &Type) -> bool {
    matches!(
        t,
        Type::Short
            | Type::UShort
            | Type::Int
            | Type::Enum { .. }
            | Type::UInt
            | Type::Long
            | Type::ULong
            | Type::Float
            | Type::Double
            | Type::Char
            | Type::UChar
            | Type::SChar
    )
}

pub fn is_integer_type(t: &Type) -> bool {
    matches!(
        t,
        Type::Short
            | Type::UShort
            | Type::Int
            | Type::Enum { .. }
            | Type::UInt
            | Type::Long
            | Type::ULong
            | Type::Char
            | Type::UChar
            | Type::SChar
    )
}

pub fn is_pointer_type(t: &Type) -> bool {
    matches!(t, Type::Pointer(_))
}

fn is_null_ptr_constant(e: &Expression) -> bool {
    match e {
        Expression::Constant(ConstantExpression { value, .. }) => matches!(
            value,
            Const::Short(0)
                | Const::Int(0)
                | Const::Long(0)
                | Const::UShort(0)
                | Const::UInt(0)
                | Const::ULong(0)
                | Const::Char(0)
                | Const::UChar(0)
        ),
        Expression::Cast(CastExpression { expr, .. }) => is_null_ptr_constant(expr),
        _ => false,
    }
}

pub fn is_char_type(t: &Type) -> bool {
    matches!(t, Type::Char | Type::UChar | Type::SChar)
}

pub fn is_small_integer_type(t: &Type) -> bool {
    matches!(
        t,
        Type::Char | Type::UChar | Type::SChar | Type::Short | Type::UShort
    )
}

pub fn is_scalar(t: &Type) -> bool {
    !matches!(
        t,
        Type::Void
            | Type::Array { .. }
            | Type::Func { .. }
            | Type::Struct { .. }
            | Type::Union { .. }
    )
}

pub fn is_complete(t: &Type) -> bool {
    match t {
        Type::Void => false,
        Type::Struct { tag } | Type::Union { tag } => TYPE_TABLE.lock().unwrap().contains_key(tag),
        Type::Enum { .. } => true,
        _ => true,
    }
}

fn is_ptr_to_complete(t: &Type) -> bool {
    match t {
        Type::Pointer(inner) => is_complete(inner),
        _ => false,
    }
}

fn is_lvalue(e: &Expression) -> bool {
    match e {
        Expression::Variable(_)
        | Expression::Deref(_)
        | Expression::Subscript(_)
        | Expression::String(_)
        | Expression::Arrow(_) => true,
        Expression::Dot(dot) => is_lvalue(&dot.structure),
        _ => false,
    }
}

#[derive(Debug, Clone)]
pub enum StaticInit {
    Short(i16),
    Int(i32),
    Long(i64),
    UShort(u16),
    UInt(u32),
    ULong(u64),
    Float(f32),
    Double(f64),
    Char(i32),
    UChar(u32),
    String(String, bool),
    Pointer(String),
    Zero(usize),
}

impl PartialEq for StaticInit {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (StaticInit::Short(s1), StaticInit::Short(s2)) => s1 == s2,
            (StaticInit::Int(i1), StaticInit::Int(i2)) => i1 == i2,
            (StaticInit::Long(l1), StaticInit::Long(l2)) => l1 == l2,
            (StaticInit::UShort(us1), StaticInit::UShort(us2)) => us1 == us2,
            (StaticInit::UInt(u1), StaticInit::UInt(u2)) => u1 == u2,
            (StaticInit::ULong(ul1), StaticInit::ULong(ul2)) => ul1 == ul2,
            (StaticInit::Float(f1), StaticInit::Float(f2)) => {
                f1 == f2 || f1.total_cmp(f2) == Ordering::Equal
            }
            (StaticInit::Double(d1), StaticInit::Double(d2)) => {
                d1 == d2 || d1.total_cmp(d2) == Ordering::Equal
            }
            (StaticInit::Char(c1), StaticInit::Char(c2)) => c1 == c2,
            (StaticInit::UChar(uc1), StaticInit::UChar(uc2)) => uc1 == uc2,
            (StaticInit::String(s1, b1), StaticInit::String(s2, b2)) => s1 == s2 && b1 == b2,
            (StaticInit::Pointer(p1), StaticInit::Pointer(p2)) => p1 == p2,
            (StaticInit::Zero(z1), StaticInit::Zero(z2)) => z1 == z2,
            (a, b) => a.type_id().cmp(&b.type_id()) == Ordering::Equal,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IdentifierAttrs {
    FuncAttr {
        defined: bool,
        global: bool,
    },
    StaticAttr {
        initial_value: InitialValue,
        global: bool,
    },
    ConstantAttr(StaticInit),
    EnumConstantAttr(i32),
    LocalAttr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum InitialValue {
    Tentative,
    Initial(Vec<StaticInit>),
    NoInitializer,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub ty: Type,
    pub attrs: IdentifierAttrs,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructEntry {
    pub kind: AggregateKind,
    pub alignment: usize,
    pub size: usize,
    pub members: Vec<MemberEntry>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberEntry {
    pub name: String,
    pub ty: Type,
    pub offset: usize,
}

fn alignment(t: &Type) -> usize {
    match t {
        Type::Char | Type::UChar | Type::SChar => 1,
        Type::Short | Type::UShort => 2,
        Type::Int | Type::UInt | Type::Float | Type::Enum { .. } => 4,
        Type::Double | Type::Long | Type::ULong | Type::Pointer(_) => 8,
        Type::Struct { tag } | Type::Union { tag } => TYPE_TABLE.lock().unwrap()[tag].alignment,
        Type::Array { element, size: _ } => alignment(element),
        Type::Dummy | Type::Void | Type::Func { .. } => unreachable!(),
    }
}

pub fn round_up(value: usize, alignment: usize) -> usize {
    (value + alignment - 1) & !(alignment - 1)
}

macro_rules! convert_to_static {
    ($konst:expr, $ty:ty, $variant:path) => {
        match $konst {
            Const::Short(val) => $variant(*val as $ty),
            Const::UShort(val) => $variant(*val as $ty),
            Const::Int(val) => $variant(*val as $ty),
            Const::Long(val) => $variant(*val as $ty),
            Const::UInt(val) => $variant(*val as $ty),
            Const::ULong(val) => $variant(*val as $ty),
            Const::Float(val) => $variant(*val as $ty),
            Const::Double(val) => $variant(*val as $ty),
            _ => unreachable!(),
        }
    };
}

fn const2staticinit(konst: &Const, t: &Type) -> StaticInit {
    match t {
        Type::Short => convert_to_static!(konst, i16, StaticInit::Short),
        Type::Int | Type::Enum { .. } => convert_to_static!(konst, i32, StaticInit::Int),
        Type::UShort => convert_to_static!(konst, u16, StaticInit::UShort),
        Type::UInt => convert_to_static!(konst, u32, StaticInit::UInt),
        Type::Long => convert_to_static!(konst, i64, StaticInit::Long),
        Type::ULong => convert_to_static!(konst, u64, StaticInit::ULong),
        Type::Float => convert_to_static!(konst, f32, StaticInit::Float),
        Type::Double => convert_to_static!(konst, f64, StaticInit::Double),
        Type::Pointer(_) => convert_to_static!(konst, u64, StaticInit::ULong),
        Type::Char | Type::SChar => convert_to_static!(konst, i32, StaticInit::Char),
        Type::UChar => convert_to_static!(konst, u32, StaticInit::UChar),
        _ => unreachable!(),
    }
}

fn static_init_helper(init: &Initializer, t: &Type) -> Result<Vec<StaticInit>> {
    match (t, init) {
        (_, Initializer::Single(name, Expression::Literal(lit))) => {
            if let Some(nested_init) = unwrap_untyped_braced_initializer(name, lit) {
                return static_init_helper(&nested_init, t);
            }

            Err(UccError {
                msg: format!("StaticInitError::NonConstantInitializer"),
                kind: ErrorKind::Typecheck,
                span: Span { start: 0, end: 0 },
            })
        }
        (_, Initializer::Compound(name, _, inits)) if is_scalar(t) && inits.len() == 1 => {
            let inner = initializer_with_name(name, &inits[0]);
            static_init_helper(&inner, t)
        }
        (Type::Pointer(_), Initializer::Single(_, Expression::String(string_expr))) => {
            let str_id = format!("string.{}", make_temporary());
            let symbol = Symbol {
                ty: Type::Array {
                    element: Box::new(Type::Char),
                    size: string_expr.value.len() + 1,
                },
                attrs: IdentifierAttrs::ConstantAttr(StaticInit::String(
                    string_expr.value.to_owned(),
                    true,
                )),
            };
            SYMBOL_TABLE.lock().unwrap().insert(str_id.clone(), symbol);
            Ok(vec![StaticInit::Pointer(str_id)])
        }
        (Type::Struct { tag }, Initializer::Compound(_name, _ty, compound_init)) => {
            let struct_def = TYPE_TABLE.lock().unwrap().get(tag).unwrap().clone();

            if compound_init.len() > struct_def.members.len() {
                return Err(UccError {
                    msg: format!("Too many initializers"),
                    kind: ErrorKind::Typecheck,
                    span: Span { start: 0, end: 0 },
                });
            }

            let mut ordered_inits = struct_def
                .members
                .iter()
                .map(|member| Initializer::zero(&member.ty))
                .collect::<Vec<_>>();
            let mut next_member = 0usize;

            for init_elem in compound_init {
                let member_index = if let Some(member_name) = initializer_designator_name(init_elem)
                {
                    find_designated_member_index(&struct_def.members, member_name).ok_or_else(
                        || UccError {
                            msg: format!(
                                "Unknown aggregate member `{}` in designated initializer.",
                                member_name
                            ),
                            kind: ErrorKind::Typecheck,
                            span: Span { start: 0, end: 0 },
                        },
                    )?
                } else {
                    let idx = next_member;
                    if idx >= struct_def.members.len() {
                        return Err(UccError {
                            msg: format!("Too many initializers"),
                            kind: ErrorKind::Typecheck,
                            span: Span { start: 0, end: 0 },
                        });
                    }
                    idx
                };
                let member = &struct_def.members[member_index];
                ordered_inits[member_index] = initializer_for_selected_member(member, init_elem);
                next_member = member_index + 1;
            }

            let mut current_offset = 0;

            let mut static_inits = vec![];

            for (i, init_elem) in ordered_inits.iter().enumerate() {
                let member = struct_def.members[i].clone();
                if member.offset != current_offset {
                    static_inits.push(StaticInit::Zero(member.offset - current_offset));
                }

                let more_static_inits = static_init_helper(init_elem, &member.ty)?;
                static_inits.extend(more_static_inits);

                current_offset = member.offset + get_size_of_type(&member.ty);
            }

            if struct_def.size != current_offset {
                static_inits.push(StaticInit::Zero(struct_def.size - current_offset));
            }

            Ok(static_inits)
        }
        (Type::Union { tag }, Initializer::Compound(_name, _ty, compound_init)) => {
            let union_def = TYPE_TABLE.lock().unwrap().get(tag).unwrap().clone();

            if compound_init.len() > 1 {
                return Err(UccError {
                    msg: format!("Too many initializers"),
                    kind: ErrorKind::Typecheck,
                    span: Span { start: 0, end: 0 },
                });
            }

            let first_init = compound_init.first();
            let member = if let Some(init_elem) = first_init {
                if let Some(member_name) = initializer_designator_name(init_elem) {
                    let member_index =
                        find_designated_member_index(&union_def.members, member_name).ok_or_else(
                            || UccError {
                                msg: format!(
                                    "Unknown aggregate member `{}` in designated initializer.",
                                    member_name
                                ),
                                kind: ErrorKind::Typecheck,
                                span: Span { start: 0, end: 0 },
                            },
                        )?;
                    &union_def.members[member_index]
                } else {
                    union_def.members.first().unwrap()
                }
            } else {
                union_def.members.first().unwrap()
            };

            let mut static_inits = if let Some(init_elem) = first_init {
                let init_for_member = initializer_for_selected_member(member, init_elem);
                static_init_helper(&init_for_member, &member.ty)?
            } else {
                vec![StaticInit::Zero(get_size_of_type(&member.ty))]
            };

            let initialized_size = get_size_of_type(&member.ty);
            if union_def.size != initialized_size {
                static_inits.push(StaticInit::Zero(union_def.size - initialized_size));
            }

            Ok(static_inits)
        }
        (Type::Struct { .. } | Type::Union { .. }, Initializer::Single(_, _)) => {
            return Err(UccError {
                kind: ErrorKind::Typecheck,
                msg: format!("Single initializer for aggregate type"),
                span: Span { start: 0, end: 0 },
            });
        }
        (Type::Array { element, size }, Initializer::Single(_, expr)) => {
            if let Expression::String(string_expr) = expr {
                if !is_char_type(element) {
                    return Err(UccError {
                        kind: ErrorKind::Typecheck,
                        msg: format!("Cannot init an array with non char type."),
                        span: Span { start: 0, end: 0 },
                    });
                }

                let len_diff = size - string_expr.value.len();
                match len_diff {
                    0 => Ok(vec![StaticInit::String(
                        string_expr.value.to_owned(),
                        false,
                    )]),
                    1 => Ok(vec![StaticInit::String(string_expr.value.to_owned(), true)]),
                    n if n > 0 => {
                        let mut initializers =
                            vec![StaticInit::String(string_expr.value.to_owned(), true)];
                        initializers.push(StaticInit::Zero(n - 1));
                        Ok(initializers)
                    }
                    _ => {
                        return Err(UccError {
                            kind: ErrorKind::Typecheck,
                            msg: format!("String too long for array"),
                            span: Span { start: 0, end: 0 },
                        })
                    }
                }
            } else {
                return Err(UccError {
                    kind: ErrorKind::Typecheck,
                    msg: format!("Can't initialize array with non-string"),
                    span: Span { start: 0, end: 0 },
                });
            }
        }
        (_, Initializer::Single(_, Expression::Constant(ConstantExpression { value, .. }))) => {
            if matches!(
                value,
                Const::Short(0)
                    | Const::UShort(0)
                    | Const::Int(0)
                    | Const::Long(0)
                    | Const::UInt(0)
                    | Const::ULong(0)
                    | Const::Float(0.0)
                    | Const::Double(0.0)
            ) {
                Ok(vec![StaticInit::Zero(get_size_of_type(t))])
            } else {
                Ok(vec![const2staticinit(value, t)])
            }
        }
        (_, Initializer::Single(_, Expression::Variable(VariableExpression { value, .. }))) => {
            let symbol = SYMBOL_TABLE.lock().unwrap().get(value).cloned();
            if let Some(Symbol {
                attrs: IdentifierAttrs::EnumConstantAttr(v),
                ..
            }) = symbol
            {
                if v == 0 {
                    Ok(vec![StaticInit::Zero(get_size_of_type(t))])
                } else if matches!(t, Type::Pointer(_)) {
                    return Err(UccError {
                        msg: format!("InvalidPointerInitializer"),
                        kind: ErrorKind::Typecheck,
                        span: Span { start: 0, end: 0 },
                    });
                } else if is_scalar(t) {
                    Ok(vec![const2staticinit(&Const::Int(v), t)])
                } else {
                    return Err(UccError {
                        msg: format!("StaticInitError::NonConstantInitializer"),
                        kind: ErrorKind::Typecheck,
                        span: Span { start: 0, end: 0 },
                    });
                }
            } else {
                return Err(UccError {
                    msg: format!("StaticInitError::NonConstantInitializer"),
                    kind: ErrorKind::Typecheck,
                    span: Span { start: 0, end: 0 },
                });
            }
        }
        (Type::Pointer(_), Initializer::Single(_, expr)) if is_null_ptr_constant(expr) => {
            Ok(vec![StaticInit::Zero(get_size_of_type(t))])
        }
        (Type::Pointer(_), _) => {
            return Err(UccError {
                msg: format!("InvalidPointerInitializer"),
                kind: ErrorKind::Typecheck,
                span: Span { start: 0, end: 0 },
            })
        }
        (_, Initializer::Single(_, _)) => {
            return Err(UccError {
                msg: format!("StaticInitError::NonConstantInitializer"),
                kind: ErrorKind::Typecheck,
                span: Span { start: 0, end: 0 },
            })
        }
        (Type::Array { element, size }, Initializer::Compound(_, _, inits)) => {
            let mut static_inits = Vec::with_capacity(inits.len());
            for init in inits.iter() {
                let static_init = static_init_helper(init, element)?;
                static_inits.extend(static_init);
            }

            let padding_size = size.saturating_sub(inits.len());
            let padding = match padding_size.cmp(&0) {
                std::cmp::Ordering::Greater => {
                    vec![StaticInit::Zero(get_size_of_type(element) * padding_size)]
                }
                std::cmp::Ordering::Equal => vec![],
                std::cmp::Ordering::Less => {
                    return Err(UccError {
                        msg: format!("Too many initializers"),
                        kind: ErrorKind::Typecheck,
                        span: Span { start: 0, end: 0 },
                    })
                }
            };

            static_inits.extend(padding);
            Ok(static_inits)
        }
        (_, Initializer::Compound(_, _, _)) => {
            return Err(UccError {
                kind: ErrorKind::Typecheck,
                msg: format!("compound init for scalar type"),
                span: Span { start: 0, end: 0 },
            });
        }
    }
}

fn to_static_init(init: &Initializer, t: &Type) -> Result<InitialValue> {
    let init_list = static_init_helper(init, t)?;
    Ok(InitialValue::Initial(init_list))
}

#[cfg(test)]
mod union_tests {
    use super::*;
    use crate::parser::ast::{AggregateKind, MemberDeclaration};

    fn span() -> Span {
        Span { start: 0, end: 0 }
    }

    fn member(name: &str, ty: Type) -> MemberDeclaration {
        MemberDeclaration {
            name: name.to_string(),
            ty,
            span: span(),
        }
    }

    #[test]
    fn lays_out_union_members_at_offset_zero() {
        let tag = "union.layout.test".to_string();
        TYPE_TABLE.lock().unwrap().remove(&tag);

        StructDeclaration {
            tag: tag.clone(),
            kind: AggregateKind::Union,
            members: vec![member("i", Type::Int), member("d", Type::Double)],
            span: span(),
        }
        .typecheck()
        .unwrap();

        let entry = TYPE_TABLE.lock().unwrap().get(&tag).cloned().unwrap();
        assert_eq!(entry.kind, AggregateKind::Union);
        assert_eq!(entry.size, 8);
        assert_eq!(entry.alignment, 8);
        assert_eq!(
            entry.members.iter().map(|m| m.offset).collect::<Vec<_>>(),
            vec![0, 0]
        );
    }

    #[test]
    fn typechecks_union_initializer_against_first_member() {
        let tag = "union.init.test".to_string();
        TYPE_TABLE.lock().unwrap().remove(&tag);

        StructDeclaration {
            tag: tag.clone(),
            kind: AggregateKind::Union,
            members: vec![member("i", Type::Int), member("d", Type::Double)],
            span: span(),
        }
        .typecheck()
        .unwrap();

        let init = Initializer::Compound(
            String::new(),
            Type::Dummy,
            vec![Initializer::Single(
                String::new(),
                Expression::Constant(ConstantExpression {
                    value: Const::Int(7),
                    ty: Type::Int,
                    span: span(),
                }),
            )],
        );

        let typed = typecheck_init(&Type::Union { tag: tag.clone() }, &init).unwrap();
        assert!(matches!(
            typed,
            Initializer::Compound(_, Type::Union { tag: ref typed_tag }, ref inits)
                if typed_tag == &tag && inits.len() == 1
        ));
    }

    #[test]
    fn typechecks_designated_aggregate_initializers() {
        let tag = "struct.designated.init.test".to_string();
        TYPE_TABLE.lock().unwrap().remove(&tag);

        StructDeclaration {
            tag: tag.clone(),
            kind: AggregateKind::Struct,
            members: vec![member("kind", Type::Int), member("len", Type::Long)],
            span: span(),
        }
        .typecheck()
        .unwrap();

        let init = Initializer::Compound(
            String::new(),
            Type::Dummy,
            vec![
                Initializer::Single(
                    "len".to_string(),
                    Expression::Constant(ConstantExpression {
                        value: Const::Long(42),
                        ty: Type::Long,
                        span: span(),
                    }),
                ),
                Initializer::Single(
                    "kind".to_string(),
                    Expression::Constant(ConstantExpression {
                        value: Const::Int(7),
                        ty: Type::Int,
                        span: span(),
                    }),
                ),
            ],
        );

        let typed = typecheck_init(&Type::Struct { tag: tag.clone() }, &init).unwrap();
        let Initializer::Compound(_, Type::Struct { tag: typed_tag }, inits) = typed else {
            panic!("expected typed struct initializer");
        };
        assert_eq!(typed_tag, tag);
        assert!(matches!(
            &inits[0],
            Initializer::Single(name, Expression::Constant(ConstantExpression { value: Const::Int(7), .. }))
                if name == "kind"
        ));
        assert!(matches!(
            &inits[1],
            Initializer::Single(name, Expression::Constant(ConstantExpression { value: Const::Long(42), .. }))
                if name == "len"
        ));
    }
}

#[cfg(test)]
mod enum_tests {
    use super::*;
    use crate::ir::gen::make_temporary;
    use crate::parser::ast::{
        BinaryExpression, BinaryExpressionKind, EnumMemberDeclaration, VariableExpression,
    };

    fn span() -> Span {
        Span { start: 0, end: 0 }
    }

    fn enum_const_values(names: &[String]) -> Vec<i32> {
        names
            .iter()
            .map(|name| {
                let table = SYMBOL_TABLE.lock().unwrap();
                match &table.get(name).unwrap().attrs {
                    IdentifierAttrs::EnumConstantAttr(value) => *value,
                    other => panic!("expected enum constant attr, got {other:?}"),
                }
            })
            .collect()
    }

    #[test]
    fn assigns_implicit_and_explicit_enumerator_values() {
        let suffix = make_temporary();
        let a = format!("enum.test.A.{suffix}");
        let b = format!("enum.test.B.{suffix}");
        let c = format!("enum.test.C.{suffix}");
        let decl = EnumDeclaration {
            tag: Some(format!("enum.test.tag.{suffix}")),
            members: vec![
                EnumMemberDeclaration {
                    name: a.clone(),
                    value: None,
                    span: span(),
                },
                EnumMemberDeclaration {
                    name: b.clone(),
                    value: Some(Expression::Constant(ConstantExpression {
                        value: Const::Int(5),
                        ty: Type::Int,
                        span: span(),
                    })),
                    span: span(),
                },
                EnumMemberDeclaration {
                    name: c.clone(),
                    value: None,
                    span: span(),
                },
            ],
            span: span(),
        };

        decl.typecheck().unwrap();

        assert_eq!(enum_const_values(&[a, b, c]), vec![0, 5, 6]);
    }

    #[test]
    fn allows_enum_constants_in_static_initializers() {
        let suffix = make_temporary();
        let name = format!("enum.test.static.A.{suffix}");
        SYMBOL_TABLE.lock().unwrap().insert(
            name.clone(),
            Symbol {
                ty: Type::Int,
                attrs: IdentifierAttrs::EnumConstantAttr(9),
            },
        );

        let init = Initializer::Single(
            String::new(),
            Expression::Variable(VariableExpression {
                value: name,
                ty: Type::Dummy,
                span: span(),
            }),
        );

        assert_eq!(
            static_init_helper(&init, &Type::Int).unwrap(),
            vec![StaticInit::Int(9)]
        );
    }

    #[test]
    fn rejects_nonzero_enum_constants_as_static_pointer_initializers() {
        let suffix = make_temporary();
        let name = format!("enum.test.ptr.A.{suffix}");
        SYMBOL_TABLE.lock().unwrap().insert(
            name.clone(),
            Symbol {
                ty: Type::Int,
                attrs: IdentifierAttrs::EnumConstantAttr(1),
            },
        );

        let init = Initializer::Single(
            String::new(),
            Expression::Variable(VariableExpression {
                value: name,
                ty: Type::Dummy,
                span: span(),
            }),
        );

        assert!(static_init_helper(&init, &Type::Pointer(Box::new(Type::Int))).is_err());
    }

    #[test]
    fn accepts_previous_enumerators_in_constant_expressions() {
        let suffix = make_temporary();
        let a = format!("enum.test.expr.A.{suffix}");
        let b = format!("enum.test.expr.B.{suffix}");
        let decl = EnumDeclaration {
            tag: Some(format!("enum.test.expr.tag.{suffix}")),
            members: vec![
                EnumMemberDeclaration {
                    name: a.clone(),
                    value: Some(Expression::Constant(ConstantExpression {
                        value: Const::Int(3),
                        ty: Type::Int,
                        span: span(),
                    })),
                    span: span(),
                },
                EnumMemberDeclaration {
                    name: b.clone(),
                    value: Some(Expression::Binary(BinaryExpression {
                        kind: BinaryExpressionKind::Mul,
                        lhs: Box::new(Expression::Variable(VariableExpression {
                            value: a.clone(),
                            ty: Type::Dummy,
                            span: span(),
                        })),
                        rhs: Box::new(Expression::Constant(ConstantExpression {
                            value: Const::Int(4),
                            ty: Type::Int,
                            span: span(),
                        })),
                        ty: Type::Dummy,
                        span: span(),
                    })),
                    span: span(),
                },
            ],
            span: span(),
        };

        decl.typecheck().unwrap();

        assert_eq!(enum_const_values(&[a, b]), vec![3, 12]);
    }
}

#[cfg(test)]
mod typedef_varargs_tests {
    use super::*;
    use crate::ir::gen::make_temporary;

    fn span() -> Span {
        Span { start: 0, end: 0 }
    }

    fn konst(value: Const, ty: Type) -> Expression {
        Expression::Constant(ConstantExpression {
            value,
            ty,
            span: span(),
        })
    }

    fn insert_variadic_symbol(name: &str) {
        SYMBOL_TABLE.lock().unwrap().insert(
            name.to_string(),
            Symbol {
                ty: Type::Func {
                    params: vec![Type::Int],
                    ret: Box::new(Type::Int),
                    variadic: true,
                },
                attrs: IdentifierAttrs::FuncAttr {
                    defined: false,
                    global: true,
                },
            },
        );
    }

    #[test]
    fn variadic_calls_apply_default_argument_promotions() {
        let name = format!("varargs.typecheck.{}", make_temporary());
        insert_variadic_symbol(&name);

        let call = Expression::Call(CallExpression {
            name,
            args: vec![
                konst(Const::Int(1), Type::Int),
                konst(Const::Float(1.5), Type::Float),
                konst(Const::Short(2), Type::Short),
            ],
            ty: Type::Dummy,
            span: span(),
        });

        let typed = typecheck_expr(&call).unwrap();
        let Expression::Call(call) = typed else {
            panic!("expected call expression");
        };

        assert_eq!(call.args.len(), 3);
        assert!(matches!(
            &call.args[1],
            Expression::Cast(CastExpression { target_type, .. }) if target_type == &Type::Double
        ));
        assert!(matches!(
            &call.args[2],
            Expression::Cast(CastExpression { target_type, .. }) if target_type == &Type::Int
        ));
    }

    #[test]
    fn variadic_calls_still_require_fixed_arguments() {
        let name = format!("varargs.too_few.{}", make_temporary());
        insert_variadic_symbol(&name);

        let call = Expression::Call(CallExpression {
            name,
            args: vec![],
            ty: Type::Dummy,
            span: span(),
        });

        assert!(typecheck_expr(&call).is_err());
    }

    #[test]
    fn typechecks_typedef_declarations_without_emitting_symbols() {
        let decl = TypedefDeclaration {
            name: "I".to_string(),
            ty: Type::Int,
            span: span(),
        };

        assert_eq!(decl.clone().typecheck().unwrap(), decl);
    }
}

#[cfg(test)]
mod selfhost_regression_tests {
    use super::*;
    use crate::lexer::lex::Lexer;
    use crate::parser::recursive_descent::Parser;
    use crate::semantics::resolver::Resolve;
    use std::collections::{BTreeMap, VecDeque};

    fn parse_resolve_typecheck(src: &str) -> Result<Program> {
        let tokens: VecDeque<_> = Lexer::new(src.to_string()).collect();
        let mut parser = Parser::new(tokens);
        let program = parser.parse()?;
        let resolved = program.resolve(&mut BTreeMap::new(), &mut BTreeMap::new())?;
        resolved.typecheck()
    }

    #[test]
    fn local_variables_reuse_complete_file_scope_struct_tags() {
        let suffix = make_temporary();
        let src = format!(
            "struct Token_{suffix} {{ int kind; int len; char *start; }}; \
             int f_{suffix}(void) {{ struct Token_{suffix} token; token.kind = 1; return token.kind; }}"
        );

        parse_resolve_typecheck(&src).unwrap();
    }

    #[test]
    fn designated_initializers_find_anonymous_aggregate_members() {
        let suffix = make_temporary();
        let src = format!(
            "struct Outer_{suffix} {{ union {{ int a; long b; }}; int c; }}; \
             struct Outer_{suffix} obj_{suffix} = {{ .a = 1, .c = 2 }};"
        );

        parse_resolve_typecheck(&src).unwrap();
    }

    #[test]
    fn static_pointer_initializers_accept_cast_null_pointer_constants() {
        let suffix = make_temporary();
        let src = format!("char *ptr_{suffix} = (void *)0;");

        parse_resolve_typecheck(&src).unwrap();
    }
}
