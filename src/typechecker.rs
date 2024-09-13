use crate::{
    lexer::Const,
    parser::{
        AssignExpression, BinaryExpression, BinaryExpressionKind, BlockItem, BlockStatement,
        CallExpression, CastExpression, ConditionalExpression, ConstantExpression, Declaration,
        DoWhileStatement, Expression, ExpressionStatement, ForInit, ForStatement,
        FunctionDeclaration, IfStatement, ProgramStatement, ReturnStatement, Statement,
        StorageClass, Type, UnaryExpression, UnaryExpressionKind, VariableDeclaration,
        VariableExpression, WhileStatement,
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
    // convert konst to t
    match konst {
        Const::Int(i) => {
            match t {
                Type::Int => StaticInit::Int(*i),
                Type::Uint => StaticInit::Uint(*i as u32),
                Type::Long => StaticInit::Long(*i as i64),
                Type::Ulong => StaticInit::Ulong(*i as u64),
                _ => unreachable!()
            }
        }
        Const::Long(l) => {
            match t {
                Type::Int => StaticInit::Int(*l as i32),
                Type::Uint => StaticInit::Uint(*l as u32),
                Type::Long => StaticInit::Long(*l),
                Type::Ulong => StaticInit::Ulong(*l as u64),
                _ => unreachable!()
            }
        }
        Const::UInt(u) => {
            match t {
                Type::Int => StaticInit::Int(*u as i32),
                Type::Uint => StaticInit::Uint(*u),
                Type::Long => StaticInit::Long(*u as i64),
                Type::Ulong => StaticInit::Ulong(*u as u64),
                _ => unreachable!()
            }
        }
        Const::ULong(ul) => {
            match t {
                Type::Int => StaticInit::Int(*ul as i32),
                Type::Uint => StaticInit::Uint(*ul as u32),
                Type::Long => StaticInit::Long(*ul as i64),
                Type::Ulong => StaticInit::Ulong(*ul),
                _ => unreachable!()
            }
        }
    }
}

impl Typecheck for VariableDeclaration {
    fn typecheck(&self) -> Result<BlockItem> {
        match self.is_global {
            true => {
                let mut initial_value;

                if let Some(Expression::Constant(konst)) = &self.init {
                    initial_value = match konst.value {
                        Const::Int(i) => InitialValue::Initial(const2type(&konst.value, &self._type)),
                        Const::Long(l) => InitialValue::Initial(const2type(&konst.value, &self._type)),
                        Const::UInt(u) => InitialValue::Initial(const2type(&konst.value, &self._type)),
                        Const::ULong(ul) => InitialValue::Initial(const2type(&konst.value, &self._type)),
                    }
                } else if self.init.is_none() {
                    if self
                        .storage_class
                        .is_some_and(|sc| sc == StorageClass::Extern)
                    {
                        initial_value = InitialValue::NoInitializer;
                    } else {
                        initial_value = InitialValue::Tentative;
                    }
                } else {
                    bail!("no constant initializer");
                }

                let mut is_global = self.storage_class != Some(StorageClass::Static);

                if SYMBOL_TABLE.lock().unwrap().contains_key(&self.name) {
                    let old_decl = SYMBOL_TABLE
                        .lock()
                        .unwrap()
                        .get(&self.name)
                        .cloned()
                        .unwrap();

                    if old_decl._type != self._type {
                        bail!("Function {} redeclared as variable", self.name);
                    }

                    if self
                        .storage_class
                        .is_some_and(|sc| sc == StorageClass::Extern)
                    {
                        is_global = match old_decl.attrs {
                            IdentifierAttrs::StaticAttr {
                                initial_value: _,
                                global,
                            } => global,
                            _ => unreachable!(),
                        };
                    } else if is_global
                        != match old_decl.attrs {
                            IdentifierAttrs::StaticAttr {
                                initial_value: _,
                                global,
                            } => global,
                            _ => unreachable!(),
                        }
                    {
                        bail!("Conflicting variable linkage {:?}", self);
                    }

                    if let IdentifierAttrs::StaticAttr {
                        initial_value: old_init,
                        global: _,
                    } = old_decl.attrs
                    {
                        if let InitialValue::Initial(_) = old_init {
                            if let InitialValue::Initial(_) = initial_value {
                                bail!("Conflicting file-scope variable definitions");
                            } else {
                                initial_value = old_init;
                            }
                        } else if let InitialValue::Tentative = old_init {
                            if let InitialValue::Tentative = initial_value {
                                initial_value = InitialValue::Tentative;
                            }
                        }
                    }
                }

                let symbol = Symbol {
                    _type: self._type.clone(),
                    attrs: IdentifierAttrs::StaticAttr {
                        initial_value,
                        global: is_global,
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
                let initial_value;
                if self
                    .storage_class
                    .is_some_and(|sc| sc == StorageClass::Extern)
                {
                    if self.init.is_some() {
                        bail!(
                            "Extern local variable {} cannot have an initializer",
                            self.name
                        );
                    }

                    if SYMBOL_TABLE.lock().unwrap().contains_key(&self.name) {
                        let old_decl = SYMBOL_TABLE
                            .lock()
                            .unwrap()
                            .get(&self.name)
                            .cloned()
                            .unwrap();
                        if old_decl._type != self._type {
                            bail!("Function {} redeclared as variable", self.name);
                        }

                        Ok(BlockItem::Declaration(Declaration::Variable(
                            VariableDeclaration {
                                _type: self._type.clone(),
                                name: self.name.clone(),
                                init: None,
                                storage_class: self.storage_class,
                                is_global: self.is_global,
                            },
                        )))
                    } else {
                        let symbol = Symbol {
                            _type: self._type.clone(),
                            attrs: IdentifierAttrs::StaticAttr {
                                initial_value: InitialValue::NoInitializer,
                                global: true,
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
                                init: None,
                                storage_class: self.storage_class,
                                is_global: self.is_global,
                            },
                        )))
                    }
                } else if self
                    .storage_class
                    .is_some_and(|sc| sc == StorageClass::Static)
                {
                    if let Some(Expression::Constant(konst)) = &self.init {
                        match konst.value {
                            Const::Int(i) => {
                                initial_value = InitialValue::Initial(StaticInit::Int(i));
                            }
                            Const::Long(l) => {
                                initial_value = InitialValue::Initial(StaticInit::Long(l));
                            }
                            Const::UInt(u) => {
                                initial_value = InitialValue::Initial(StaticInit::Uint(u));
                            }
                            Const::ULong(ul) => {
                                initial_value = InitialValue::Initial(StaticInit::Ulong(ul));
                            }
                        }
                    } else if self.init.is_none() {
                        initial_value = InitialValue::Initial(StaticInit::Int(0));
                    } else {
                        bail!("no constant initializer");
                    }

                    let symbol = Symbol {
                        _type: self._type.clone(),
                        attrs: IdentifierAttrs::StaticAttr {
                            initial_value,
                            global: false,
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
                } else {
                    let symbol = Symbol {
                        _type: self._type.clone(),
                        attrs: IdentifierAttrs::LocalAttr,
                    };

                    SYMBOL_TABLE
                        .lock()
                        .unwrap()
                        .insert(self.name.clone(), symbol);

                    let typechecked_init = if self.init.is_some() {
                        Some(typecheck_expr(self.init.as_ref().unwrap())?)
                    } else {
                        None
                    };

                    let left_type = self._type.clone();

                    let converted_right = if typechecked_init.is_some() {
                        Some(convert_to(&typechecked_init.as_ref().unwrap(), &left_type))
                    } else {
                        None
                    };

                    Ok(BlockItem::Declaration(Declaration::Variable(
                        VariableDeclaration {
                            _type: self._type.clone(),
                            name: self.name.clone(),
                            init: converted_right.into(),
                            storage_class: self.storage_class,
                            is_global: self.is_global,
                        },
                    )))
                }
            }
        }
    }
}

impl Typecheck for FunctionDeclaration {
    fn typecheck(&self) -> Result<BlockItem> {
        let fun_type = self._type.clone();
        let has_body = self.body.is_some();

        let mut already_defined = false;

        let mut is_global = self.storage_class != Some(StorageClass::Static);

        if SYMBOL_TABLE.lock().unwrap().contains_key(&self.name) {
            let old_decl = SYMBOL_TABLE
                .lock()
                .unwrap()
                .get(&self.name)
                .cloned()
                .unwrap();

            if old_decl._type != fun_type {
                bail!(
                    "Incompatible function declarations for function {}",
                    self.name
                );
            }

            already_defined = match old_decl.attrs {
                IdentifierAttrs::FuncAttr { defined, global: _ } => defined,
                _ => unreachable!(),
            };

            if already_defined && has_body {
                bail!("Function {} already defined", self.name);
            }

            match old_decl.attrs {
                IdentifierAttrs::FuncAttr { defined: _, global } => {
                    if global
                        && self
                            .storage_class
                            .is_some_and(|sc| sc == StorageClass::Static)
                    {
                        bail!(
                            "Static function declaration follows non-static {}",
                            self.name
                        );
                    }

                    is_global = global;
                }
                _ => unreachable!(),
            }
        }

        let symbol = Symbol {
            _type: fun_type.clone(),
            attrs: IdentifierAttrs::FuncAttr {
                defined: already_defined || has_body,
                global: is_global,
            },
        };

        SYMBOL_TABLE
            .lock()
            .unwrap()
            .insert(self.name.clone(), symbol);

        let typechecked_body = if has_body {
            let fn_params = match fun_type {
                Type::Func { params, ret: _ } => params.clone(),
                _ => unreachable!(),
            };
            for (param, _type) in self.params.iter().zip(fn_params.iter()) {
                let symbol = Symbol {
                    _type: _type.clone(),
                    attrs: IdentifierAttrs::LocalAttr,
                };

                SYMBOL_TABLE.lock().unwrap().insert(param.clone(), symbol);
            }

            Some(self.body.as_ref().clone().unwrap().typecheck()?)
        } else {
            None
        };

        Ok(BlockItem::Declaration(Declaration::Function(
            FunctionDeclaration {
                _type: self._type.clone(),
                name: self.name.clone(),
                params: self.params.clone(),
                body: typechecked_body.into(),
                storage_class: self.storage_class,
                is_global: self.is_global,
            },
        )))
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
                let typechecked_expr = typecheck_expr(expr)?;

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
                let typechecked_condition = typecheck_expr(condition)?;
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
                let typechecked_condition = typecheck_expr(condition)?;
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
                let typechecked_expr = typecheck_expr(condition)?;
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
                    Some(typecheck_expr(for_init_expr)?)
                } else {
                    None
                };

                let typechecked_condition = if condition.is_some() {
                    Some(typecheck_expr(condition.as_ref().unwrap())?)
                } else {
                    None
                };

                let typechecked_post = if post.is_some() {
                    Some(typecheck_expr(post.as_ref().unwrap())?)
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
                let typechecked_expr = typecheck_expr(expr)?;

                Ok(BlockItem::Statement(Statement::Return(ReturnStatement {
                    expr: Expression::Cast(CastExpression {
                        target_type: target_type.clone().unwrap_or(Type::Int),
                        expr: typechecked_expr.into(),
                        _type: get_type(expr),
                    }),
                    target_type: target_type.clone(),
                })))
            }
            Statement::Break(_) | Statement::Continue(_) | Statement::Null => {
                Ok(BlockItem::Statement(self.clone()))
            }
        }
    }
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

                    for (arg, param_type) in args.iter().zip(params.iter()) {
                        let typed_arg = typecheck_expr(arg)?;

                        let converted_arg = convert_to(&typed_arg, param_type);

                        converted_args.push(converted_arg);
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
        Expression::Variable(var) => {
            let v_type = SYMBOL_TABLE
                .lock()
                .unwrap()
                .get(&var.value)
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
                value: var.value.clone(),
                _type: v_type,
            }))
        }
        Expression::Binary(BinaryExpression {
            kind,
            lhs,
            rhs,
            _type,
        }) => {
            let typed_lhs = typecheck_expr(lhs)?;
            let typed_rhs = typecheck_expr(rhs)?;

            match kind {
                BinaryExpressionKind::And | BinaryExpressionKind::Or => {
                    return Ok(Expression::Binary(BinaryExpression {
                        kind: kind.clone(),
                        lhs: Box::new(typed_lhs),
                        rhs: Box::new(typed_rhs),
                        _type: Type::Int,
                    }));
                }
                _ => {
                    let t1 = get_type(&typed_lhs);
                    let t2 = get_type(&typed_rhs);

                    println!("t1: {:?}", t1);
                    println!("t2: {:?}", t2);

                    let common_type = get_common_type(&t1, &t2);

                    println!("common type: {:?}", common_type);

                    let converted_lhs = convert_to(&typed_lhs, &common_type);
                    let converted_rhs = convert_to(&typed_rhs, &common_type);

                    match kind {
                        BinaryExpressionKind::Add
                        | BinaryExpressionKind::Sub
                        | BinaryExpressionKind::Mul
                        | BinaryExpressionKind::Div
                        | BinaryExpressionKind::Rem => Ok(Expression::Binary(BinaryExpression {
                            kind: kind.clone(),
                            lhs: Box::new(converted_lhs),
                            rhs: Box::new(converted_rhs),
                            _type: common_type,
                        })),
                        _ => Ok(Expression::Binary(BinaryExpression {
                            kind: kind.clone(),
                            lhs: Box::new(converted_lhs),
                            rhs: Box::new(converted_rhs),
                            _type: Type::Int,
                        })),
                    }
                }
            }
        }
        Expression::Assign(AssignExpression {
            op,
            lhs,
            rhs,
            _type,
        }) => {
            let typed_lhs = typecheck_expr(lhs)?;
            let typed_rhs = typecheck_expr(rhs)?;

            let left_type = get_type(&typed_lhs);

            let converted_right = convert_to(&typed_rhs, &left_type);

            Ok(Expression::Assign(AssignExpression {
                op: op.clone(),
                lhs: Box::new(typed_lhs),
                rhs: Box::new(converted_right),
                _type: left_type,
            }))
        }
        Expression::Conditional(ConditionalExpression {
            condition,
            then_expr,
            else_expr,
            _type,
        }) => {
            let typed_condition = typecheck_expr(condition)?;

            let typed_then_expr = typecheck_expr(then_expr)?;
            let typed_else_expr = typecheck_expr(else_expr)?;

            let t1 = get_type(&typed_then_expr);
            let t2 = get_type(&typed_else_expr);

            let common_type = get_common_type(&t1, &t2);

            Ok(Expression::Conditional(ConditionalExpression {
                condition: typed_condition.into(),
                then_expr: typed_then_expr.into(),
                else_expr: typed_else_expr.into(),
                _type: common_type,
            }))
        }
        Expression::Unary(UnaryExpression { kind, expr, _type }) => {
            let typed_inner = typecheck_expr(&expr)?;
            match kind {
                UnaryExpressionKind::Not => Ok(Expression::Unary(UnaryExpression {
                    kind: UnaryExpressionKind::Not,
                    expr: Box::new(typed_inner),
                    _type: Type::Int,
                })),
                _ => Ok(Expression::Unary(UnaryExpression {
                    kind: kind.clone(),
                    expr: Box::new(typed_inner.clone()),
                    _type: get_type(&typed_inner),
                })),
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
        },
        Expression::Cast(CastExpression {
            target_type,
            expr,
            _type,
        }) => {
            let typed_inner = typecheck_expr(expr)?;

            Ok(Expression::Cast(CastExpression {
                target_type: target_type.clone(),
                expr: Box::new(typed_inner),
                _type: target_type.clone(),
            }))
        }
    }
}

pub fn get_common_type(type1: &Type, type2: &Type) -> Type {
    if type1 == type2 {
        return type1.clone();
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
    println!("getting size of {:?}", t);
    match t {
        Type::Int => 4,
        Type::Uint => 4,
        Type::Long => 8,
        Type::Ulong => 8,
        _ => todo!(),
    }
}

pub fn get_signedness(t: &Type) -> bool {
    match t {
        Type::Int => true,
        Type::Uint => false,
        Type::Long => true,
        Type::Ulong => false,
        _ => todo!(),
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
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
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

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum InitialValue {
    Tentative,
    Initial(StaticInit),
    NoInitializer,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum StaticInit {
    Int(i32),
    Long(i64),
    Uint(u32),
    Ulong(u64),
}
