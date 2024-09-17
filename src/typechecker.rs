use crate::{
    lexer::Const,
    parser::{
        AddrOfExpression, AssignExpression, BinaryExpression, BinaryExpressionKind, BlockItem, BlockStatement, CallExpression, CastExpression, ConditionalExpression, ConstantExpression, Declaration, DerefExpression, DoWhileStatement, Expression, ExpressionStatement, ForInit, ForStatement, FunctionDeclaration, IfStatement, Initializer, LiteralExpression, ProgramStatement, ReturnStatement, Statement, StorageClass, SubscriptExpression, Type, UnaryExpression, UnaryExpressionKind, VariableDeclaration, VariableExpression, WhileStatement
    },
};
use anyhow::{bail, Ok, Result};
use std::{collections::HashMap, sync::Mutex};

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub _type: Type,
    pub attrs: IdentifierAttrs,
}

lazy_static::lazy_static! {
    pub static ref SYMBOL_TABLE: Mutex<HashMap<String, Symbol>> = Mutex::new(HashMap::new());
}

pub trait Typecheck {
    fn typecheck(&self) -> Result<BlockItem>;
}

impl Typecheck for BlockItem {
    fn typecheck(&self) -> Result<BlockItem> {
        match self {
            BlockItem::Declaration(decl) => decl.typecheck(),
            BlockItem::Statement(stmt) => stmt.typecheck(),
        }
    }
}

impl Typecheck for Declaration {
    fn typecheck(&self) -> Result<BlockItem> {
        match self {
            Declaration::Variable(var_decl) => var_decl.typecheck(),
            Declaration::Function(func_decl) => func_decl.typecheck(),
        }
    }
}

fn const2type(konst: &Const, t: &Type) -> StaticInit {
    match konst {
        Const::Int(i) => match t {
            Type::Int => StaticInit::Int(*i),
            Type::Uint => StaticInit::Uint(*i as u32),
            Type::Long => StaticInit::Long(*i as i64),
            Type::Ulong => StaticInit::Ulong(*i as u64),
            Type::Double => StaticInit::Double(*i as f64),
            Type::Pointer(_) => StaticInit::Ulong(*i as u64),
            _ => unreachable!(),
        },
        Const::Long(l) => match t {
            Type::Int => StaticInit::Int(*l as i32),
            Type::Uint => StaticInit::Uint(*l as u32),
            Type::Long => StaticInit::Long(*l),
            Type::Ulong => StaticInit::Ulong(*l as u64),
            Type::Double => StaticInit::Double(*l as f64),
            Type::Pointer(_) => StaticInit::Ulong(*l as u64),
            _ => unreachable!(),
        },
        Const::UInt(u) => match t {
            Type::Int => StaticInit::Int(*u as i32),
            Type::Uint => StaticInit::Uint(*u),
            Type::Long => StaticInit::Long(*u as i64),
            Type::Ulong => StaticInit::Ulong(*u as u64),
            Type::Double => StaticInit::Double(*u as f64),
            Type::Pointer(_) => StaticInit::Ulong(*u as u64),
            _ => unreachable!(),
        },
        Const::ULong(ul) => match t {
            Type::Int => StaticInit::Int(*ul as i32),
            Type::Uint => StaticInit::Uint(*ul as u32),
            Type::Long => StaticInit::Long(*ul as i64),
            Type::Ulong => StaticInit::Ulong(*ul),
            Type::Double => StaticInit::Double(*ul as f64),
            Type::Pointer(_) => StaticInit::Ulong(*ul),
            _ => unreachable!(),
        },
        Const::Double(d) => match t {
            Type::Int => StaticInit::Int(*d as i32),
            Type::Uint => StaticInit::Uint(*d as u32),
            Type::Long => StaticInit::Long(*d as i64),
            Type::Ulong => StaticInit::Ulong(*d as u64),
            Type::Double => StaticInit::Double(*d),
            Type::Pointer(_) => StaticInit::Ulong(*d as u64),
            _ => unreachable!(),
        },
    }
}

impl Typecheck for VariableDeclaration {
    fn typecheck(&self) -> Result<BlockItem> {
        match self.is_global {
            true => {
                let default_init = if self.storage_class == Some(StorageClass::Extern) {
                    InitialValue::NoInitializer
                } else {
                    InitialValue::Tentative
                };
    
                let static_init = match &self.init {
                    Some(init) => to_static_init(init.clone(), &self._type)?,
                    None => default_init,
                };
    
                let is_global = self.storage_class != Some(StorageClass::Static);
    
                let old_decl = SYMBOL_TABLE.lock().unwrap().get(&self.name).cloned();
    
                // Define the closure for checking against previous declarations
                let check_against_previous = |old_d: &Symbol| -> Result<(bool, InitialValue)> {
                    if old_d._type != self._type {
                        bail!("Variable redeclaration with different type");
                    }
    
                    match &old_d.attrs {
                        IdentifierAttrs::StaticAttr { initial_value, global } => {
                            let global = if self.storage_class == Some(StorageClass::Extern) {
                                *global
                            } else if is_global == *global {
                                is_global
                            } else {
                                bail!("Conflicting variable linkage");
                            };
    
                            let init = match (initial_value.clone(), static_init.clone()) {
                                (InitialValue::Initial(_), InitialValue::Initial(_)) => bail!("conflicting file-scope variable initializers"),
                                (InitialValue::Initial(_), _) => initial_value.clone(),
                                (InitialValue::Tentative, InitialValue::Tentative) => InitialValue::Tentative,
                                (InitialValue::Tentative, InitialValue::NoInitializer) => InitialValue::Tentative,
                                (_, InitialValue::Initial(_)) => static_init.clone(),
                                (_, InitialValue::NoInitializer) => static_init.clone(),
                                _ => unreachable!(),
                            };
    
                            Ok((global, init))
                        }
                        _ => unreachable!(),
                    }
                };
    
                let (global, init) = match old_decl {
                    Some(old_d) => check_against_previous(&old_d)?,
                    None => (is_global, static_init),
                };
    
                let symbol = Symbol {
                    _type: self._type.clone(),
                    attrs: IdentifierAttrs::StaticAttr {
                        initial_value: init,
                        global,
                    },
                };
    
                SYMBOL_TABLE
                    .lock()
                    .unwrap()
                    .insert(self.name.clone(), symbol);
    
                Ok(BlockItem::Declaration(Declaration::Variable(
                    VariableDeclaration {
                        _type: self._type.clone(),
                        name: self.name.clone(),
                        init: self.init.clone(),
                        storage_class: self.storage_class,
                        is_global: self.is_global,
                    },
                )))
            }
            false => {
                match self.storage_class {
                    Some(StorageClass::Extern) => {
                        if self.init.is_some() {
                            bail!("Extern variable with initializer");
                        }

                        // if an extern local var is already in the symbol table, don't need to add it again
                        let symbol = SYMBOL_TABLE.lock().unwrap().get(&self.name).cloned();

                        match symbol {
                            Some(sym) => {
                                if sym._type != self._type {
                                    bail!("Variable redeclaration with different type");
                                }
                            }
                            None => {
                                let symbol = Symbol {
                                    _type: self._type.clone(),
                                    attrs: IdentifierAttrs::LocalAttr,
                                };

                                SYMBOL_TABLE.lock().unwrap().insert(self.name.clone(), symbol);
                            }
                        }

                        Ok(BlockItem::Declaration(Declaration::Variable(
                            VariableDeclaration {
                                _type: self._type.clone(),
                                name: self.name.clone(),
                                init: self.init.clone(),
                                storage_class: self.storage_class,
                                is_global: self.is_global,
                            },
                        )))
                    }
                    Some(StorageClass::Static) => {
                        let zero_init = InitialValue::NoInitializer; // Placeholder, adjust if needed
                        let static_init = match &self.init {
                            Some(init) => to_static_init(init.clone(), &self._type)?,
                            None => zero_init,
                        };
        
                        let symbol = Symbol {
                            _type: self._type.clone(),
                            attrs: IdentifierAttrs::StaticAttr {
                                initial_value: static_init,
                                global: false,
                            },
                        };
                        SYMBOL_TABLE.lock().unwrap().insert(self.name.clone(), symbol);
        
                        Ok(BlockItem::Declaration(Declaration::Variable(
                            VariableDeclaration {
                                _type: self._type.clone(),
                                name: self.name.clone(),
                                init: self.init.clone(),
                                storage_class: self.storage_class,
                                is_global: self.is_global,
                            },
                        )))
                    }
                    None => {
                        let symbol = Symbol {
                            _type: self._type.clone(),
                            attrs: IdentifierAttrs::LocalAttr,
                        };
                        SYMBOL_TABLE.lock().unwrap().insert(self.name.clone(), symbol);
        
                        Ok(BlockItem::Declaration(Declaration::Variable(
                            VariableDeclaration {
                                _type: self._type.clone(),
                                name: self.name.clone(),
                                init: optionally_typecheck_init(&self.init, &self._type)?,
                                storage_class: self.storage_class,
                                is_global: self.is_global,
                            },
                        )))
                    }
                }
            }
        }
    }
}

fn optionally_typecheck_init(init: &Option<Initializer>, t: &Type) -> Result<Option<Initializer>> {
    match init {
        Some(init) => {
            let typechecked_init = typecheck_init(t, init)?;
            Ok(Some(typechecked_init))
        }
        None => Ok(None),
    }
}

fn static_init_helper(init: Initializer, t: &Type) -> Result<Vec<StaticInit>> {
    let unwrapped_init = match init {
        Initializer::Single(ref expr) => match expr {
            Expression::Literal(LiteralExpression { value, _type }) => *value.clone(),
            _ => init.clone(),
        },
        Initializer::Compound(inits) => Initializer::Compound(inits),
    };
    match (t, &unwrapped_init) {
        (Type::Array { element, size }, Initializer::Single(_)) => {
            bail!("StaticInitError::ArrayInitializationError");
        }
        (_, Initializer::Single(Expression::Constant(ConstantExpression { value, _type }))) => {
            if let Const::Int(0) | Const::Long(0) | Const::UInt(0) | Const::ULong(0) | Const::Double(0.0) = value {
                Ok(vec![StaticInit::Zero(get_size_of_type(t))])
            } else {
                Ok(vec![const2type(&value, t)])
            }
        }
        (Type::Pointer(_), _) => bail!("InvalidPointerInitializer"),
        (_, Initializer::Single(_)) => bail!("StaticInitError::NonConstantInitializer"),
        (Type::Array { element, size }, Initializer::Compound(inits)) => {
            let mut static_inits = Vec::with_capacity(inits.len());
            let element_type = element.clone();
            
            for init in inits.iter() {
                let static_init = static_init_helper(init.clone(), &element_type)?;
                static_inits.extend(static_init);
            }

            let padding = match size - inits.len() {
                0 => vec![],
                n if n > 0 => vec![StaticInit::Zero(get_size_of_type(&element_type))],
                _ => bail!("Too many initializers"),
            };

            static_inits.extend(padding);

            Ok(static_inits)
        }
        (_, Initializer::Compound(_)) => {
            println!("unwrapped_init: {:?}", unwrapped_init);
            bail!("Compound initializer for scalar type");
        }
    }
}


fn to_static_init(init: Initializer, t: &Type) -> Result<InitialValue> {
    let init_list = static_init_helper(init, t)?;
    Ok(InitialValue::Initial(init_list))
}

impl Typecheck for FunctionDeclaration {
    fn typecheck(&self) -> Result<BlockItem> {
        let adjust_param_type = |t: Type| match t {
            Type::Array { element, .. } => Type::Pointer(element),
            t => t,
        };
    
        let (param_ts, return_t, fun_type) = match self._type.clone() {
            Type::Func { params, ret } => {
                if let Type::Array { element, size } = *ret {
                    bail!("Function return type is an array");
                }
                let param_types: Vec<Type> = params.into_iter().map(adjust_param_type).collect();
                (
                    param_types.clone(),
                    ret.clone(),
                    Type::Func {
                        params: param_types.clone(),
                        ret: ret.clone(),
                    },
                )
            }
            _ => bail!("Function has non-function type"),
        };
    
        let has_body = self.body.is_some();
        let global = self.storage_class != Some(StorageClass::Static);
    
        // Helper function to reconcile current and previous declarations
        let check_against_previous = |prev: &Symbol| -> Result<(bool, bool)> {
            if prev._type != fun_type {
                bail!("RedeclaredFunction");
            }
    
            match &prev.attrs {
                IdentifierAttrs::FuncAttr { global: prev_global, defined: prev_defined } => {
                    if *prev_defined && has_body {
                        bail!("FunctionDefinedTwice");
                    } else if *prev_global && self.storage_class == Some(StorageClass::Static) {
                        bail!("StaticFunctionDeclarationAfterNonStatic");
                    }
    
                    let defined = has_body || *prev_defined;
                    Ok((defined, *prev_global))
                }
                _ => bail!("Symbol has function type but not function attributes"),
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
                _type: fun_type,
                attrs: IdentifierAttrs::FuncAttr {
                    global,
                    defined,
                },
            },
        );
    
        if has_body {
            for (param, param_t) in self.params.iter().zip(param_ts) {
                let symbol = Symbol { _type: param_t, attrs: IdentifierAttrs::LocalAttr };
                SYMBOL_TABLE.lock().unwrap().insert(param.clone(), symbol);
            }
        }
    
        let body = if self.body.is_some() {
            Some(self.body.clone().map(|b| b.typecheck()).unwrap()?)
        } else {
            None
        };
    
        Ok(BlockItem::Declaration(Declaration::Function(FunctionDeclaration {
            _type: self._type.clone(),
            name: self.name.clone(),
            params: self.params.clone(),
            body: body.into(),
            storage_class: self.storage_class,
            is_global: self.is_global,
        })))
    }
}

impl Typecheck for Statement {
    fn typecheck(&self) -> Result<BlockItem> {
        match self {
            Statement::Program(ProgramStatement { block_items: stmts }) => {
                let mut typechecked_block_items = vec![];

                for block_item in stmts {
                    typechecked_block_items.push(block_item.typecheck()?);
                }

                Ok(BlockItem::Statement(Statement::Program(ProgramStatement {
                    block_items: typechecked_block_items,
                })))
            }
            Statement::Expression(ExpressionStatement { expr }) => {
                let typechecked_expr = typecheck_and_convert(expr)?;

                Ok(BlockItem::Statement(Statement::Expression(
                    ExpressionStatement {
                        expr: typechecked_expr,
                    },
                )))
            }
            Statement::Compound(BlockStatement { stmts }) => {
                let mut typechecked_stmts = vec![];

                for stmt in stmts {
                    typechecked_stmts.push(stmt.typecheck()?);
                }

                Ok(BlockItem::Statement(Statement::Compound(BlockStatement {
                    stmts: typechecked_stmts,
                })))
            }
            Statement::If(IfStatement {
                condition,
                then_branch,
                else_branch,
            }) => {
                let typechecked_condition = typecheck_and_convert(condition)?;
                let typechecked_then_branch = then_branch.typecheck()?;

                let typechecked_else_branch = if else_branch.is_some() {
                    Some(else_branch.as_ref().clone().unwrap().typecheck()?)
                } else {
                    None
                };

                Ok(BlockItem::Statement(Statement::If(IfStatement {
                    condition: typechecked_condition,
                    then_branch: typechecked_then_branch.into(),
                    else_branch: typechecked_else_branch.into(),
                })))
            }
            Statement::While(WhileStatement {
                condition,
                body,
                label,
            }) => {
                let typechecked_condition = typecheck_and_convert(condition)?;
                let typchecked_body = body.typecheck()?;

                Ok(BlockItem::Statement(Statement::While(WhileStatement {
                    condition: typechecked_condition,
                    body: typchecked_body.into(),
                    label: label.clone(),
                })))
            }
            Statement::DoWhile(DoWhileStatement {
                condition,
                body,
                label,
            }) => {
                let typechecked_expr = typecheck_and_convert(condition)?;
                let typechecked_body = body.typecheck()?;

                Ok(BlockItem::Statement(Statement::DoWhile(DoWhileStatement {
                    condition: typechecked_expr,
                    body: typechecked_body.into(),
                    label: label.clone(),
                })))
            }
            Statement::For(ForStatement {
                init,
                condition,
                post,
                body,
                label,
            }) => {
                if let ForInit::Declaration(decl) = init {
                    if decl.storage_class.is_some() {
                        bail!("Storage class specifier in for loop initializer");
                    }
                }

                let typechecked_decl = if let ForInit::Declaration(decl) = init {
                    Some(decl.typecheck()?)
                } else {
                    None
                };

                let typechecked_for_init = if let ForInit::Expression(Some(for_init_expr)) = init {
                    Some(typecheck_and_convert(for_init_expr)?)
                } else {
                    None
                };

                let typechecked_condition = if condition.is_some() {
                    Some(typecheck_and_convert(condition.as_ref().unwrap())?)
                } else {
                    None
                };

                let typechecked_post = if post.is_some() {
                    Some(typecheck_and_convert(post.as_ref().unwrap())?)
                } else {
                    None
                };

                let typechecked_body = body.typecheck()?;

                Ok(BlockItem::Statement(Statement::For(ForStatement {
                    init: match (typechecked_decl, typechecked_for_init) {
                        (Some(decl), None) => ForInit::Declaration(match decl {
                            BlockItem::Declaration(Declaration::Variable(var_decl)) => var_decl,
                            _ => unreachable!(),
                        }),
                        (None, Some(expr)) => ForInit::Expression(Some(expr)),
                        _ => ForInit::Expression(None),
                    },
                    condition: typechecked_condition,
                    post: typechecked_post,
                    body: typechecked_body.into(),
                    label: label.clone(),
                })))
            }
            Statement::Return(ReturnStatement { expr, target_type }) => {
                let typechecked_expr = typecheck_and_convert(expr)?;

                Ok(BlockItem::Statement(Statement::Return(ReturnStatement {
                    expr: typechecked_expr,
                    target_type: target_type.clone(),
                })))
            }
            Statement::Break(_) | Statement::Continue(_) | Statement::Null => {
                Ok(BlockItem::Statement(self.clone()))
            }
        }
    }
}

fn typecheck_init(target_type: &Type, init: &Initializer) -> Result<Initializer> {
    match (target_type, init) {
        (Type::Array { element, size }, Initializer::Compound(inits)) => {
            if inits.len() > *size {
                bail!("Too many initializers");
            }

            let mut typechecked_inits = vec![];

            for init in inits.iter() {
                let typechecked_init = typecheck_init(element, init)?;
                typechecked_inits.push(typechecked_init);
            }

            while typechecked_inits.len() < *size {
                typechecked_inits.push(zero_initializer(&*element));
            }

            Ok(Initializer::Compound(typechecked_inits))
        }
        (_, Initializer::Single(expr)) => {
            match expr {
                Expression::Literal(_) => {
                    Ok(Initializer::Single(expr.clone()))
                }
                _ => {
                    let typechecked_expr = typecheck_and_convert(expr)?;
                    println!("target_type: {:?}, typechecked_expr: {:?}", target_type, typechecked_expr);
                    let converted_expr = convert_by_assignment(&typechecked_expr, target_type)?;
                    Ok(Initializer::Single(converted_expr))        
                }
            }
        }
        _ => {
            bail!("can't init a scalar object iwth a compound initializer");
        }
    }
}

fn zero_initializer(t: &Type) -> Initializer {
    match t {
        Type::Int => Initializer::Single(Expression::Constant(ConstantExpression {
            value: Const::Int(0),
            _type: Type::Int,
        })),
        Type::Uint => Initializer::Single(Expression::Constant(ConstantExpression {
            value: Const::UInt(0),
            _type: Type::Uint,
        })),
        Type::Long => Initializer::Single(Expression::Constant(ConstantExpression {
            value: Const::Long(0),
            _type: Type::Long,
        })),
        Type::Ulong => Initializer::Single(Expression::Constant(ConstantExpression {
            value: Const::ULong(0),
            _type: Type::Ulong,
        })),
        Type::Double => Initializer::Single(Expression::Constant(ConstantExpression {
            value: Const::Double(0.0),
            _type: Type::Double,
        })),
        Type::Pointer(_) => Initializer::Single(Expression::Constant(ConstantExpression {
            value: Const::Int(0),
            _type: Type::Int,
        })),
        Type::Array { element, size } => {
            let mut inits = vec![];

            for _ in 0..*size {
                inits.push(zero_initializer(element));
            }

            Initializer::Compound(inits)
        }
        _ => unreachable!(),
    }
}

fn typecheck_logical(kind: &BinaryExpressionKind, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    let typed_lhs = typecheck_and_convert(lhs)?;
    let typed_rhs = typecheck_and_convert(rhs)?;

    Ok(Expression::Binary(BinaryExpression {
        kind: kind.clone(),
        lhs: Box::new(typed_lhs),
        rhs: Box::new(typed_rhs),
        _type: Type::Int,
    }))
}

fn typecheck_addition(lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    let typed_lhs = typecheck_and_convert(lhs)?;
    let typed_rhs = typecheck_and_convert(rhs)?;

    if is_arithmetic(&get_type(&typed_lhs)) && is_arithmetic(&get_type(&typed_rhs)) {
        let common_type = get_common_type(&get_type(&typed_lhs), &get_type(&typed_rhs));

        let converted_lhs = convert_to(&typed_lhs, &common_type);
        let converted_rhs = convert_to(&typed_rhs, &common_type);

        Ok(Expression::Binary(BinaryExpression {
            kind: BinaryExpressionKind::Add,
            lhs: Box::new(converted_lhs),
            rhs: Box::new(converted_rhs),
            _type: common_type,
        }))
    } else if is_pointer_type(&get_type(&typed_lhs)) && is_integer_type(&get_type(&typed_rhs)) {
        let converted_rhs = convert_to(&typed_rhs, &Type::Long);

        Ok(Expression::Binary(BinaryExpression {
            kind: BinaryExpressionKind::Add,
            lhs: Box::new(typed_lhs.clone()),
            rhs: Box::new(converted_rhs),
            _type: get_type(&typed_lhs),
        }))
    } else if is_pointer_type(&get_type(&typed_rhs)) && is_integer_type(&get_type(&typed_lhs)) {
        let converted_lhs = convert_to(&typed_lhs, &Type::Long);

        Ok(Expression::Binary(BinaryExpression {
            kind: BinaryExpressionKind::Add,
            lhs: Box::new(converted_lhs),
            rhs: Box::new(typed_rhs.clone()),
            _type: get_type(&typed_rhs),
        }))
    } else {
        bail!("Invalid operands for addition");
    }
}

fn typecheck_subtraction(lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    let typed_lhs = typecheck_and_convert(lhs)?;
    let typed_rhs = typecheck_and_convert(rhs)?;

    let t1 = get_type(&typed_lhs);
    let t2 = get_type(&typed_rhs);

    if is_arithmetic(&t1) && is_arithmetic(&t2) {
        let common_type = get_common_type(&t1, &t2);
        let converted_lhs = convert_to(&typed_lhs, &common_type);
        let converted_rhs = convert_to(&typed_rhs, &common_type);

        Ok(Expression::Binary(BinaryExpression {
            kind: BinaryExpressionKind::Sub,
            lhs: Box::new(converted_lhs),
            rhs: Box::new(converted_rhs),
            _type: common_type,
        }))
    } else if is_pointer_type(&t1) && is_integer_type(&t2) {
        let converted_rhs = convert_to(&typed_rhs, &Type::Long);
        
        Ok(Expression::Binary(BinaryExpression {
            kind: BinaryExpressionKind::Sub,
            lhs: Box::new(typed_lhs.clone()),
            rhs: Box::new(converted_rhs),
            _type: t1,
        }))
    } else if is_pointer_type(&t1) && get_type(&typed_lhs) == get_type(&typed_rhs) {
        Ok(Expression::Binary(BinaryExpression {
            kind: BinaryExpressionKind::Sub,
            lhs: Box::new(typed_lhs.clone()),
            rhs: Box::new(typed_rhs.clone()),
            _type: Type::Long,
        }))
    } else {
        bail!("Invalid operands for subtraction");
    }
}

fn typecheck_multiplicative(kind: &BinaryExpressionKind, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    let typed_lhs = typecheck_and_convert(lhs)?;
    let typed_rhs = typecheck_and_convert(rhs)?;

    let t1 = get_type(&typed_lhs);
    let t2 = get_type(&typed_rhs);

    if is_pointer_type(&t1) || is_pointer_type(&t2) {
        bail!("multiplication, division, and remainder operators cannot be applied to pointer types");
    } else {
        let common_type = get_common_type(&t1, &t2);
        let converted_lhs = convert_to(&typed_lhs, &common_type);
        let converted_rhs = convert_to(&typed_rhs, &common_type);
        
        match kind {
            BinaryExpressionKind::Rem if common_type == Type::Double => {
                bail!("remainder operator cannot be applied to floating-point types");
            }
            BinaryExpressionKind::Mul | BinaryExpressionKind::Div | BinaryExpressionKind::Rem => {
                Ok(Expression::Binary(BinaryExpression {
                    kind: kind.clone(),
                    lhs: Box::new(converted_lhs),
                    rhs: Box::new(converted_rhs),
                    _type: common_type,
                }))
            }
            _ => unreachable!(),
        }
    }
}

fn typecheck_equality(kind: &BinaryExpressionKind, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    let typed_lhs = typecheck_and_convert(lhs)?;
    let typed_rhs = typecheck_and_convert(rhs)?;

    let t1 = get_type(&typed_lhs);
    let t2 = get_type(&typed_rhs);

    let common_type = if is_pointer_type(&t1) || is_pointer_type(&t2) {
        get_common_ptr_type(&typed_lhs, &typed_rhs)?
    } else {
        get_common_type(&t1, &t2)
    };

    let converted_lhs = convert_to(&typed_lhs, &common_type);
    let converted_rhs = convert_to(&typed_rhs, &common_type);

    Ok(Expression::Binary(BinaryExpression {
        kind: kind.clone(),
        lhs: Box::new(converted_lhs),
        rhs: Box::new(converted_rhs),
        _type: Type::Int,
    }))
}

fn typecheck_relational(kind: &BinaryExpressionKind, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    let typed_lhs = typecheck_and_convert(lhs)?;
    let typed_rhs = typecheck_and_convert(rhs)?;

    let t1 = get_type(&typed_lhs);
    let t2 = get_type(&typed_rhs);

    let common_type = if is_arithmetic(&t1) && is_arithmetic(&t2) {
        get_common_type(&t1, &t2)
    } else if is_pointer_type(&t1) && t1 == t2 {
        t2
    } else {
        bail!("Invalid operands for relational operator");
    };

    let converted_lhs = convert_to(&typed_lhs, &common_type);
    let converted_rhs = convert_to(&typed_rhs, &common_type);

    Ok(Expression::Binary(BinaryExpression {
        kind: kind.clone(),
        lhs: Box::new(converted_lhs),
        rhs: Box::new(converted_rhs),
        _type: Type::Int,
    }))
}

fn typecheck_not(expr: &Expression) -> Result<Expression> {
    let typed_expr = typecheck_and_convert(expr)?;

    Ok(Expression::Unary(UnaryExpression {
        kind: UnaryExpressionKind::Not,
        expr: Box::new(typed_expr),
        _type: Type::Int,
    }))
}

fn typecheck_complement(expr: &Expression) -> Result<Expression> {
    let typed_expr = typecheck_and_convert(expr)?;
    
    let t = get_type(&typed_expr);

    if t == Type::Double || is_pointer_type(&t) {
        bail!("Invalid operand for bitwise complement");
    }

    Ok(Expression::Unary(UnaryExpression {
        kind: UnaryExpressionKind::Complement,
        expr: Box::new(typed_expr),
        _type: t,
    }))
}

fn typecheck_negate(expr: &Expression) -> Result<Expression> {
    let typed_expr = typecheck_and_convert(expr)?;

    let t = get_type(&typed_expr);

    match t {
        Type::Pointer(_) => bail!("can't negate a ptr"),
        _ => {
            return Ok(Expression::Unary(UnaryExpression {
                kind: UnaryExpressionKind::Negate,
                expr: Box::new(typed_expr),
                _type: t,
            }));
        }
    }
}

fn typecheck_subscript(expr: &Expression, index: &Expression) -> Result<Expression> {
    let typed_e1 = typecheck_and_convert(expr)?;
    let typed_e2 = typecheck_and_convert(index)?;

    let t1 = get_type(&typed_e1);
    let t2 = get_type(&typed_e2);

    let (ptr_type, converted_lhs, converted_rhs) = if is_pointer_type(&t1) && is_integer_type(&t2) {
        (t1, typed_e1.clone(), convert_to(&typed_e2, &Type::Long))
    } else if is_pointer_type(&t2) && is_integer_type(&t1) {
        (t2, convert_to(&typed_e1, &Type::Long), typed_e2.clone())
    } else {
        bail!("Invalid operands for subscript");
    };

    let result_type = match ptr_type {
        Type::Pointer(ptr_type) => *ptr_type,
        _ => unreachable!(),
    };

    Ok(Expression::Subscript(SubscriptExpression {
        expr: Box::new(converted_lhs),
        index: Box::new(converted_rhs),
        _type: result_type,
    }))
}

fn typecheck_expr(expr: &Expression) -> Result<Expression> {
    match expr {
        Expression::Call(CallExpression { name, args, _type }) => {
            let f = SYMBOL_TABLE.lock().unwrap().get(name).cloned().unwrap();
            let f_type = f._type.clone();

            match f_type {
                Type::Func { params, ret } => {
                    if args.len() != params.len() {
                        bail!("Function called with the wrong number of arguments");
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

                    Ok(Expression::Call(CallExpression {
                        name: name.clone(),
                        args: converted_args,
                        _type: *ret,
                    }))
                }
                _ => bail!("Variable used as function name"),
            }
        }
        Expression::Variable(VariableExpression { value, _type }) => {
            println!("getting type for variable: {:?}", value);
            let v_type = SYMBOL_TABLE
                .lock()
                .unwrap()
                .get(value)
                .cloned()
                .unwrap()
                ._type;

            let some_fn_type = Type::Func {
                params: vec![Type::Int],
                ret: Type::Int.into(),
            };

            if std::mem::discriminant(&v_type) == std::mem::discriminant(&some_fn_type) {
                bail!("function used as a variable");
            }

            Ok(Expression::Variable(VariableExpression {
                value: value.clone(),
                _type: v_type,
            }))
        }
        Expression::Binary(BinaryExpression {
            kind,
            lhs,
            rhs,
            _type,
        }) => {
            match kind {
                BinaryExpressionKind::And | BinaryExpressionKind::Or => typecheck_logical(kind, lhs, rhs),
                BinaryExpressionKind::Add => typecheck_addition(lhs, rhs),
                BinaryExpressionKind::Sub => typecheck_subtraction(lhs, rhs),
                BinaryExpressionKind::Mul | BinaryExpressionKind::Div | BinaryExpressionKind::Rem => typecheck_multiplicative(kind, lhs, rhs),
                BinaryExpressionKind::Equal | BinaryExpressionKind::NotEqual => typecheck_equality(kind, lhs, rhs),
                BinaryExpressionKind::Less | BinaryExpressionKind::LessEqual | BinaryExpressionKind::Greater | BinaryExpressionKind::GreaterEqual => typecheck_relational(kind, lhs, rhs),
            }
        }
        Expression::Assign(AssignExpression {
            op,
            lhs,
            rhs,
            _type,
        }) => {
            match *lhs.clone() {
                Expression::Variable(_) | Expression::Deref(_) | Expression::Subscript(_) => {
                    let typed_lhs = typecheck_and_convert(lhs)?;
       
                    // if typed_lhs is not an lvalue
                    match typed_lhs {
                        Expression::Variable(_) | Expression::Deref(_) | Expression::Subscript(_) => {}
                        _ => {
                            bail!("Invalid lvalue in assignment");
                        }
                    }
       
                    let typed_rhs = typecheck_and_convert(rhs)?;
        
                    let left_type = get_type(&typed_lhs);
        
                    let converted_right = convert_by_assignment(&typed_rhs, &left_type)?;
        
                    Ok(Expression::Assign(AssignExpression {
                        op: op.clone(),
                        lhs: Box::new(typed_lhs),
                        rhs: Box::new(converted_right),
                        _type: left_type,
                    }))
                }
                _ => {
                    bail!("Invalid lvalue in assignment");
                }
            }
        }
        Expression::Conditional(ConditionalExpression {
            condition,
            then_expr,
            else_expr,
            _type,
        }) => {
            let typed_condition = typecheck_and_convert(condition)?;

            let typed_then_expr = typecheck_and_convert(then_expr)?;
            let typed_else_expr = typecheck_and_convert(else_expr)?;

            let t1 = get_type(&typed_then_expr);
            let t2 = get_type(&typed_else_expr);

            let common_type = match (t1.clone(), t2.clone()) {
                (Type::Pointer(ptr1), Type::Pointer(ptr2)) => {
                    get_common_ptr_type(&typed_then_expr, &typed_else_expr)?
                }
                (Type::Pointer(ptr1), _) => {
                    get_common_ptr_type(&typed_then_expr, &typed_else_expr)?
                }
                (_, Type::Pointer(ptr2)) => {
                    get_common_ptr_type(&typed_then_expr, &typed_else_expr)?
                }
                _ => {
                    get_common_type(&t1, &t2)
                }
            };
            let converted_then_expr = convert_to(&typed_then_expr, &common_type);
            let converted_else_expr = convert_to(&typed_else_expr, &common_type);

            Ok(Expression::Cast(CastExpression { target_type: common_type.clone(), expr: Expression::Conditional(ConditionalExpression {
                condition: typed_condition.into(),
                then_expr: converted_then_expr.into(),
                else_expr: converted_else_expr.into(),
                _type: common_type.clone(),
            }).into(), _type: common_type }))
        }
        Expression::Unary(UnaryExpression { kind, expr, _type }) => {
            match kind {
                UnaryExpressionKind::Complement => typecheck_complement(expr),
                UnaryExpressionKind::Negate => typecheck_negate(expr),
                UnaryExpressionKind::Not => typecheck_not(expr),
            }
        }
        Expression::Constant(ConstantExpression { value, _type }) => match value {
            Const::Int(i) => Ok(Expression::Constant(ConstantExpression {
                value: Const::Int(*i),
                _type: Type::Int,
            })),
                Const::Long(l) => Ok(Expression::Constant(ConstantExpression {
                value: Const::Long(*l),
                _type: Type::Long,
            })),
            Const::UInt(u) => Ok(Expression::Constant(ConstantExpression {
                value: Const::UInt(*u),
                _type: Type::Uint,
            })),
            Const::ULong(ul) => Ok(Expression::Constant(ConstantExpression {
                value: Const::ULong(*ul),
                _type: Type::Ulong,
            })),
            Const::Double(d) => Ok(Expression::Constant(ConstantExpression {
                value: Const::Double(*d),
                _type: Type::Double,
            })),
        },
        Expression::Cast(CastExpression {
            target_type,
            expr,
            _type,
        }) => {
            let typed_inner = typecheck_and_convert(expr)?;

            let t1 = get_type(&typed_inner);
            let t2 = target_type;

            match t2 {
                Type::Array { element, size } => {
                    bail!("Array type in cast");
                }
                _ => {}
            }

            if let Type::Pointer(_) = t1 {
                if let Type::Double = t2 {
                    bail!("Pointer to double cast");
                }
            }

            if let Type::Pointer(_) = t2 {
                if let Type::Double = t1 {
                    bail!("Double to pointer cast");
                }
            }

            Ok(Expression::Cast(CastExpression {
                target_type: target_type.clone(),
                expr: Box::new(typed_inner),
                _type: target_type.clone(),
            }))
        }
        Expression::Deref(DerefExpression { expr, _type }) => {
            let typed_inner = typecheck_and_convert(expr)?;

            let inner_type = get_type(&typed_inner);

            match inner_type {
                Type::Pointer(inner_type) => {
                    let deref_expr = Expression::Deref(DerefExpression {
                        expr: Box::new(typed_inner),
                        _type: *inner_type,
                    });
                    Ok(deref_expr)
                }
                _ => bail!("Dereference of non-pointer type"),
            }
        }
        Expression::AddrOf(AddrOfExpression { expr, _type }) => {
            match *expr.clone() {
                Expression::Variable(_) | Expression::Deref(_) | Expression::Subscript(_) => {
                    let typed_inner = typecheck_expr(&expr)?;
                    let referenced_type = get_type(&typed_inner);
                    Ok(Expression::AddrOf(AddrOfExpression {
                        expr: Box::new(typed_inner),
                        _type: Type::Pointer(Box::new(referenced_type)),
                    }))                
                        
                }
                _ => {
                    bail!("Can't take address of a non-lvalue");
                }
            }
        }
        Expression::Subscript(SubscriptExpression { expr, index, _type }) => typecheck_subscript(expr, index),
        Expression::Literal(LiteralExpression { value, _type }) => {
            Ok(Expression::Literal(LiteralExpression {
                value: value.clone(),
                _type: _type.clone(),
            }))
        }
        _ => todo!(),
    }
}

fn typecheck_and_convert(e: &Expression) -> Result<Expression> {
    let typed_expr = typecheck_expr(e)?;
    let type_of_expr = get_type(&typed_expr);
    match type_of_expr {
        Type::Array { element, size } => {
            let addr_of_expr = Expression::AddrOf(AddrOfExpression { expr: typed_expr.into(), _type: Type::Pointer(element) });
            return Ok(addr_of_expr);
        }
        _ => return Ok(typed_expr),
    }
}

fn convert_by_assignment(e: &Expression, target_type: &Type) -> Result<Expression> {
    println!("got expression {:?}", e);
    if get_type(e) == *target_type {
        return Ok(e.clone());
    } else if is_arithmetic(&get_type(e)) && is_arithmetic(target_type) {
        return Ok(convert_to(e, target_type));
    } else if is_null_ptr_constant(e) && is_pointer_type(target_type) {
        return Ok(convert_to(e, target_type));
    } else {
        bail!("cannot convert");
   }
}

fn is_arithmetic(t: &Type) -> bool {
    match t {
        Type::Int => true,
        Type::Uint => true,
        Type::Long => true,
        Type::Ulong => true,
        Type::Double => true,
        _ => false,
    }
}

fn is_integer_type(t: &Type) -> bool {
    match t {
        Type::Int => true,
        Type::Uint => true,
        Type::Long => true,
        Type::Ulong => true,
        _ => false,
    }
}

fn is_pointer_type(t: &Type) -> bool {
    match t {
        Type::Pointer(_) => true,
        _ => false,
    }
}

fn is_null_ptr_constant(e: &Expression) -> bool {
    match e {
        Expression::Constant(ConstantExpression { value, _type }) => match value {
            Const::Int(0) => true,
            Const::Long(0) => true,
            Const::UInt(0) => true,
            Const::ULong(0) => true,
            _ => {
                false
            }
        },
        _ => {
            false
        }
    }
}

fn get_common_ptr_type(e1: &Expression, e2: &Expression) -> Result<Type> {
    let e1_t = get_type(e1);
    let e2_t = get_type(e2);

    if e1_t == e2_t {
        return Ok(e1_t);
    } else if is_null_ptr_constant(e1) {
        return Ok(e2_t);
    } else if is_null_ptr_constant(e2) {
        return Ok(e1_t);
    } else {
        bail!("Incompatible pointer types");
    }
}

pub fn get_common_type(type1: &Type, type2: &Type) -> Type {
    if type1 == type2 {
        return type1.clone();
    }

    if type1 == &Type::Double || type2 == &Type::Double {
        return Type::Double;
    }

    if get_size_of_type(type1) == get_size_of_type(type2) {
        if get_signedness(type1) {
            return type2.clone();
        } else {
            return type1.clone();
        }
    }

    if get_size_of_type(type1) > get_size_of_type(type2) {
        return type1.clone();
    } else {
        return type2.clone();
    }
}

pub fn get_size_of_type(t: &Type) -> usize {
    match t {
        Type::Int => 4,
        Type::Uint => 4,
        Type::Long => 8,
        Type::Ulong => 8,
        Type::Double => 8,
        Type::Pointer(_) => 8,
        Type::Array { element, size } => get_size_of_type(element) * size,
        _ => {
            unreachable!()
        }
    }
}

pub fn get_signedness(t: &Type) -> bool {
    match t {
        Type::Int => true,
        Type::Uint => false,
        Type::Long => true,
        Type::Ulong => false,
        Type::Double => true,
        Type::Pointer(_) => false,
        _ => unreachable!(),
    }
}

fn convert_to(e: &Expression, _type: &Type) -> Expression {
    if get_type(e) == *_type {
        return e.clone();
    }
    let cast_expr = Expression::Cast(CastExpression {
        target_type: _type.clone(),
        expr: Box::new(e.clone()),
        _type: _type.clone(),
    });
    return cast_expr;
}

pub fn get_type(e: &Expression) -> Type {
    match e {
        Expression::Assign(AssignExpression {
            op: _,
            lhs: _,
            rhs: _,
            _type,
        }) => _type.clone(),
        Expression::Binary(BinaryExpression {
            kind: _,
            lhs: _,
            rhs: _,
            _type,
        }) => _type.clone(),
        Expression::Call(CallExpression {
            name: _,
            args: _,
            _type,
        }) => _type.clone(),
        Expression::Cast(CastExpression {
            target_type: _,
            expr: _,
            _type,
        }) => _type.clone(),
        Expression::Conditional(ConditionalExpression {
            condition: _,
            then_expr: _,
            else_expr: _,
            _type,
        }) => _type.clone(),
        Expression::Constant(ConstantExpression { value: _, _type }) => _type.clone(),
        Expression::Unary(UnaryExpression {
            kind: _,
            expr: _,
            _type,
        }) => _type.clone(),
        Expression::Variable(VariableExpression { value: _, _type }) => _type.clone(),
        Expression::Deref(DerefExpression { expr: _, _type }) => _type.clone(),
        Expression::AddrOf(AddrOfExpression { expr: _, _type }) => _type.clone(),
        Expression::Literal(LiteralExpression { value: _, _type }) => _type.clone(),
        Expression::Subscript(SubscriptExpression { expr: _, index: _, _type }) => _type.clone(),
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
    LocalAttr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum InitialValue {
    Tentative,
    Initial(Vec<StaticInit>),
    NoInitializer,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum StaticInit {
    Int(i32),
    Long(i64),
    Uint(u32),
    Ulong(u64),
    Double(f64),
    Zero(usize),
}
