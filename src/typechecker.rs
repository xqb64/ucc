use crate::{lexer::Const, parser::{
    AssignExpression, BinaryExpression, BinaryExpressionKind, BlockItem, BlockStatement, CallExpression, CastExpression, ConditionalExpression, ConstantExpression, Declaration, DoWhileStatement, Expression, ExpressionStatement, ForInit, ForStatement, FunctionDeclaration, IfStatement, ProgramStatement, ReturnStatement, Statement, StorageClass, Type, UnaryExpression, UnaryExpressionKind, VariableDeclaration, VariableExpression, WhileStatement
}};
use anyhow::{bail, Ok, Result};
use std::{collections::HashMap, sync::Mutex};


#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub ty: Type,
    pub attrs: IdentifierAttrs,
}

lazy_static::lazy_static! {
    pub static ref SYMBOL_TABLE: Mutex<HashMap<String, Symbol>> = Mutex::new(HashMap::new());
}

pub trait Typecheck {
    fn typecheck(&self) -> Result<()>;
}

impl Typecheck for BlockItem {
    fn typecheck(&self) -> Result<()> {
        match self {
            BlockItem::Declaration(decl) => decl.typecheck(),
            BlockItem::Statement(stmt) => stmt.typecheck(),
        }
    }
}

impl Typecheck for Declaration {
    fn typecheck(&self) -> Result<()> {
        match self {
            Declaration::Variable(var_decl) => var_decl.typecheck(),
            Declaration::Function(func_decl) => func_decl.typecheck(),
        }
    }
}

impl Typecheck for VariableDeclaration {
    fn typecheck(&self) -> Result<()> {
        match self.is_global {
            true => {
                let mut initial_value;

                if let Some(Expression::Constant(konst)) = &self.init {
                    initial_value = match konst.value {
                        Const::Int(i) => InitialValue::Initial(Init::Int(i)),
                        Const::Long(l) => InitialValue::Initial(Init::Long(l)),
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

                    if old_decl.ty != self._type {
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
                    ty: self._type.clone(),
                    attrs: IdentifierAttrs::StaticAttr {
                        initial_value,
                        global: is_global,
                    },
                };

                SYMBOL_TABLE
                    .lock()
                    .unwrap()
                    .insert(self.name.clone(), symbol);
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
                        if old_decl.ty != self._type {
                            bail!("Function {} redeclared as variable", self.name);
                        }
                    } else {
                        let symbol = Symbol {
                            ty: self._type.clone(),
                            attrs: IdentifierAttrs::StaticAttr {
                                initial_value: InitialValue::NoInitializer,
                                global: true,
                            },
                        };
                        println!("inserting variable {} with symbol {:?}", self.name, symbol);
                        SYMBOL_TABLE
                            .lock()
                            .unwrap()
                            .insert(self.name.clone(), symbol);
                    }
                } else if self
                    .storage_class
                    .is_some_and(|sc| sc == StorageClass::Static)
                {
                    if let Some(Expression::Constant(konst)) = &self.init {
                        match konst.value {
                            Const::Int(i) => {
                                initial_value = InitialValue::Initial(Init::Int(i));
                            }
                            Const::Long(l) => {
                                initial_value = InitialValue::Initial(Init::Long(l));
                            }
                        }
                    } else if self.init.is_none() {
                        initial_value = InitialValue::Initial(Init::Int(0));
                    } else {
                        bail!("no constant initializer");
                    }

                    let symbol = Symbol {
                        ty: self._type.clone(),
                        attrs: IdentifierAttrs::StaticAttr {
                            initial_value,
                            global: false,
                        },
                    };

                    println!("inserting variable {} with symbol {:?}", self.name, symbol);
                    SYMBOL_TABLE
                        .lock()
                        .unwrap()
                        .insert(self.name.clone(), symbol);
                } else {
                    let symbol = Symbol {
                        ty: self._type.clone(),
                        attrs: IdentifierAttrs::LocalAttr,
                    };
                    println!("inserting variable {} with symbol {:?}", self.name, symbol);
                    SYMBOL_TABLE
                        .lock()
                        .unwrap()
                        .insert(self.name.clone(), symbol);
                    if self.init.is_some() {
                        typecheck_expr(self.init.as_ref().unwrap())?;
                    }
                }
            }
        }

        Ok(())
    }
}

impl Typecheck for FunctionDeclaration {
    fn typecheck(&self) -> Result<()> {
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

            if old_decl.ty != fun_type {
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
            ty: fun_type.clone(),
            attrs: IdentifierAttrs::FuncAttr {
                defined: already_defined || has_body,
                global: is_global,
            },
        };

        println!("inserting function {} with symbol {:?}", self.name, symbol);
        SYMBOL_TABLE
            .lock()
            .unwrap()
            .insert(self.name.clone(), symbol);

        if has_body {
            let fn_params = match fun_type {
                Type::Func { params, ret: _ } => params.clone(),
                _ => unreachable!(),
            };
            for (param, _type) in self.params.iter().zip(fn_params.iter()) {
                let symbol = Symbol {
                    ty: _type.clone(),
                    attrs: IdentifierAttrs::LocalAttr,
                };
                SYMBOL_TABLE.lock().unwrap().insert(param.clone(), symbol);
            }

            self.body.as_ref().clone().unwrap().typecheck()?;
        }

        Ok(())
    }
}

impl Typecheck for Statement {
    fn typecheck(&self) -> Result<()> {
        match self {
            Statement::Program(ProgramStatement { block_items: stmts }) => {
                for block_item in stmts {
                    block_item.typecheck()?;
                }

                Ok(())
            }
            Statement::Expression(ExpressionStatement { expr }) => {
                typecheck_expr(expr)?;

                Ok(())
            }
            Statement::Compound(BlockStatement { stmts }) => {
                for stmt in stmts {
                    stmt.typecheck()?;
                }

                Ok(())
            }
            Statement::If(IfStatement {
                condition,
                then_branch,
                else_branch,
            }) => {
                typecheck_expr(condition)?;
                then_branch.typecheck()?;

                if else_branch.is_some() {
                    else_branch.as_ref().clone().unwrap().typecheck()?;
                }

                Ok(())
            }
            Statement::While(WhileStatement {
                condition,
                body,
                label: _,
            }) => {
                typecheck_expr(condition)?;
                body.typecheck()?;

                Ok(())
            }
            Statement::DoWhile(DoWhileStatement {
                condition,
                body,
                label: _,
            }) => {
                typecheck_expr(condition)?;
                body.typecheck()?;

                Ok(())
            }
            Statement::For(ForStatement {
                init,
                condition,
                post,
                body,
                label: _,
            }) => {
                if let ForInit::Declaration(decl) = init {
                    if decl.storage_class.is_some() {
                        bail!("Storage class specifier in for loop initializer");
                    }
                }

                if let ForInit::Declaration(decl) = init {
                    decl.typecheck()?;
                }

                if let ForInit::Expression(Some(for_init_expr)) = init {
                    typecheck_expr(for_init_expr)?;
                }

                if condition.is_some() {
                    typecheck_expr(condition.as_ref().unwrap())?;
                }

                if post.is_some() {
                    typecheck_expr(post.as_ref().unwrap())?;
                }

                body.typecheck()?;

                Ok(())
            }
            Statement::Return(ReturnStatement { expr }) => {
                typecheck_expr(expr)?;

                Ok(())
            }
            Statement::Break(_) | Statement::Continue(_) | Statement::Null => Ok(()),
        }
    }
}

fn typecheck_expr(expr: &Expression) -> Result<Expression> {
    match expr {
        Expression::Call(CallExpression { name, args, _type }) => {
            let f = SYMBOL_TABLE.lock().unwrap().get(name).cloned().unwrap();
            let f_type = f.ty.clone();

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

                    Ok(Expression::Call(CallExpression { name: name.clone(), args: converted_args, _type: *ret }))
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
                .ty;

            let some_fn_type = Type::Func { params: vec![Type::Int], ret: Type::Int.into() };

            if std::mem::discriminant(&v_type) == std::mem::discriminant(&some_fn_type) {
                bail!("function used as a variable");
            }

            Ok(Expression::Variable(VariableExpression { value: var.value.clone(), _type: v_type }))
        }
        Expression::Binary(BinaryExpression { kind, lhs, rhs, _type }) => {
            let typed_lhs = typecheck_expr(lhs)?;
            let typed_rhs = typecheck_expr(rhs)?;

            match kind {
                BinaryExpressionKind::And | BinaryExpressionKind::Or => {
                    return Ok(Expression::Binary(BinaryExpression { kind: kind.clone(), lhs: Box::new(typed_lhs), rhs: Box::new(typed_rhs), _type: Type::Int }));
                }
                _ => {
                    let t1 = get_type(&typed_lhs);
                    let t2 = get_type(&typed_rhs);

                    let common_type = get_common_type(&t1, &t2);

                    let converted_lhs = convert_to(&typed_lhs, &common_type);
                    let converted_rhs = convert_to(&typed_rhs, &common_type);

                    match kind {
                        BinaryExpressionKind::Add | BinaryExpressionKind::Sub | BinaryExpressionKind::Mul | BinaryExpressionKind::Div | BinaryExpressionKind::Rem => {
                            Ok(Expression::Binary(BinaryExpression { kind: kind.clone(), lhs: Box::new(converted_lhs), rhs: Box::new(converted_rhs), _type: common_type }))
                        }
                        _ => {
                            Ok(Expression::Binary(BinaryExpression { kind: kind.clone(), lhs: Box::new(converted_lhs), rhs: Box::new(converted_rhs), _type: Type::Int }))
                        }
                    }
                }
            }
        }
        Expression::Assign(AssignExpression { op, lhs, rhs, _type }) => {
            let typed_lhs = typecheck_expr(lhs)?;
            let typed_rhs = typecheck_expr(rhs)?;

            let left_type = get_type(&typed_lhs);

            let converted_right = convert_to(&typed_rhs, &left_type);

            Ok(Expression::Assign(AssignExpression { op: op.clone(), lhs: Box::new(typed_lhs), rhs: Box::new(converted_right), _type: left_type }))
        }
        Expression::Conditional(ConditionalExpression {
            condition,
            then_expr,
            else_expr,
            _type
        }) => {
            let typed_condition = typecheck_expr(condition)?;
            
            let typed_then_expr = typecheck_expr(then_expr)?;
            let typed_else_expr = typecheck_expr(else_expr)?;

            let t1 = get_type(&typed_then_expr);
            let t2 = get_type(&typed_else_expr);

            let common_type = get_common_type(&t1, &t2);

            Ok(Expression::Conditional(ConditionalExpression { condition: typed_condition.into(), then_expr: typed_then_expr.into(), else_expr: typed_else_expr.into(), _type: common_type }))
        }
        Expression::Unary(UnaryExpression { kind, expr, _type }) => {
            let typed_inner = typecheck_expr(&expr)?;
            match kind {
                UnaryExpressionKind::Not => {
                    Ok(Expression::Unary(UnaryExpression { kind: UnaryExpressionKind::Not, expr: Box::new(typed_inner), _type: Type::Int }))
                }
                _ => {
                    Ok(Expression::Unary(UnaryExpression { kind: UnaryExpressionKind::Not, expr: Box::new(typed_inner.clone()), _type: get_type(&typed_inner) }))
                }
            }
        }
        Expression::Constant(ConstantExpression { value, _type }) => {
            match value {
                Const::Int(i) => Ok(Expression::Constant(ConstantExpression { value: Const::Int(*i), _type: Type::Int })),
                Const::Long(l) => Ok(Expression::Constant(ConstantExpression { value: Const::Long(*l), _type: Type::Long })),
            }
        },
        Expression::Cast(CastExpression { target_type, expr, _type }) => {
            let typed_inner = typecheck_expr(expr)?;

            Ok(Expression::Cast(CastExpression { target_type: target_type.clone(), expr: Box::new(typed_inner), _type: target_type.clone() }))
        }
    }
}

fn get_common_type(type1: &Type, type2: &Type) -> Type {
    if type1 == type2 {
        return type1.clone();
    } else {
        return Type::Long;
    }
}

fn convert_to(e: &Expression, _type: &Type) -> Expression {
    if get_type(e) == *_type {
        return e.clone();
    }
    let cast_expr = Expression::Cast(CastExpression { target_type: _type.clone(), expr: Box::new(e.clone()), _type: _type.clone() });
    return cast_expr;
}

fn get_type(e: &Expression) -> Type {
    match e {
        Expression::Assign(AssignExpression { op: _, lhs: _, rhs: _, _type }) => _type.clone(),
        Expression::Binary(BinaryExpression { kind: _, lhs: _, rhs: _, _type }) => _type.clone(),
        Expression::Call(CallExpression { name: _, args: _, _type }) => _type.clone(),
        Expression::Cast(CastExpression { target_type: _, expr: _, _type }) => _type.clone(),
        Expression::Conditional(ConditionalExpression { condition: _, then_expr: _, else_expr: _, _type }) => _type.clone(),
        Expression::Constant(ConstantExpression { value: _, _type }) => _type.clone(),
        Expression::Unary(UnaryExpression { kind: _, expr: _, _type }) => _type.clone(),
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
    Initial(Init),
    NoInitializer,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Init {
    Int(i32),
    Long(i64),
}