use crate::parser::{
    AssignExpression, BinaryExpression, BlockItem, BlockStatement, CallExpression,
    ConditionalExpression, Declaration, DoWhileStatement, Expression, ExpressionStatement, ForInit,
    ForStatement, FunctionDeclaration, IfStatement, ProgramStatement, ReturnStatement, Statement,
    StorageClass, UnaryExpression, VariableDeclaration, WhileStatement,
};
use anyhow::{bail, Ok, Result};
use std::{collections::HashMap, sync::Mutex};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Func(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub ty: Type,
    pub attrs: IdentifierAttrs,
}

lazy_static::lazy_static! {
    pub static ref SYMBOL_TABLE: Mutex<HashMap<String, Symbol>> = Mutex::new(HashMap::new());
}

fn typecheck_variable_declaration(var_decl: &VariableDeclaration) -> Result<()> {
    match var_decl.is_global {
        true => {
            let mut initial_value;

            if let Some(Expression::Constant(konst)) = var_decl.init {
                initial_value = InitialValue::Initial(konst);
            } else if var_decl.init.is_none() {
                if var_decl
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

            let mut is_global = var_decl.storage_class != Some(StorageClass::Static);

            if SYMBOL_TABLE.lock().unwrap().contains_key(&var_decl.name) {
                let old_decl = SYMBOL_TABLE
                    .lock()
                    .unwrap()
                    .get(&var_decl.name)
                    .cloned()
                    .unwrap();

                if old_decl.ty != Type::Int {
                    bail!("Function {} redeclared as variable", var_decl.name);
                }

                if var_decl
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
                } else {
                    if is_global
                        != match old_decl.attrs {
                            IdentifierAttrs::StaticAttr {
                                initial_value: _,
                                global,
                            } => global,
                            _ => unreachable!(),
                        }
                    {
                        bail!("Conflicting variable linkage {:?}", var_decl);
                    }
                }

                match old_decl.attrs {
                    IdentifierAttrs::StaticAttr {
                        initial_value: old_init,
                        global: _,
                    } => {
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
                    _ => {}
                };
            }

            let symbol = Symbol {
                ty: Type::Int,
                attrs: IdentifierAttrs::StaticAttr {
                    initial_value,
                    global: is_global,
                },
            };

            SYMBOL_TABLE
                .lock()
                .unwrap()
                .insert(var_decl.name.clone(), symbol);
        }
        false => {
            let initial_value;
            if var_decl
                .storage_class
                .is_some_and(|sc| sc == StorageClass::Extern)
            {
                if var_decl.init.is_some() {
                    bail!(
                        "Extern local variable {} cannot have an initializer",
                        var_decl.name
                    );
                }

                if SYMBOL_TABLE.lock().unwrap().contains_key(&var_decl.name) {
                    let old_decl = SYMBOL_TABLE
                        .lock()
                        .unwrap()
                        .get(&var_decl.name)
                        .cloned()
                        .unwrap();
                    if old_decl.ty != Type::Int {
                        bail!("Function {} redeclared as variable", var_decl.name);
                    }
                } else {
                    let symbol = Symbol {
                        ty: Type::Int,
                        attrs: IdentifierAttrs::StaticAttr {
                            initial_value: InitialValue::NoInitializer,
                            global: true,
                        },
                    };
                    SYMBOL_TABLE
                        .lock()
                        .unwrap()
                        .insert(var_decl.name.clone(), symbol);
                }
            } else if var_decl
                .storage_class
                .is_some_and(|sc| sc == StorageClass::Static)
            {
                if let Some(Expression::Constant(konst)) = var_decl.init {
                    initial_value = InitialValue::Initial(konst);
                } else if var_decl.init.is_none() {
                    initial_value = InitialValue::Initial(0);
                } else {
                    bail!("no constant initializer");
                }

                let symbol = Symbol {
                    ty: Type::Int,
                    attrs: IdentifierAttrs::StaticAttr {
                        initial_value,
                        global: false,
                    },
                };

                SYMBOL_TABLE
                    .lock()
                    .unwrap()
                    .insert(var_decl.name.clone(), symbol);
            } else {
                let symbol = Symbol {
                    ty: Type::Int,
                    attrs: IdentifierAttrs::LocalAttr,
                };
                SYMBOL_TABLE
                    .lock()
                    .unwrap()
                    .insert(var_decl.name.clone(), symbol);
                if var_decl.init.is_some() {
                    typecheck_expr(var_decl.init.as_ref().unwrap())?;
                }
            }
        }
    }

    Ok(())
}

fn typecheck_function_declaration(func_decl: &FunctionDeclaration) -> Result<()> {
    let fun_type = Type::Func(func_decl.params.len());
    let has_body = func_decl.body.is_some();

    let mut already_defined = false;

    let mut is_global = func_decl.storage_class != Some(StorageClass::Static);

    if SYMBOL_TABLE.lock().unwrap().contains_key(&func_decl.name) {
        let old_decl = SYMBOL_TABLE
            .lock()
            .unwrap()
            .get(&func_decl.name)
            .cloned()
            .unwrap();

        if old_decl.ty != fun_type {
            bail!(
                "Incompatible function declarations for function {}",
                func_decl.name
            );
        }

        already_defined = match old_decl.attrs {
            IdentifierAttrs::FuncAttr { defined, global: _ } => defined,
            _ => unreachable!(),
        };

        if already_defined && has_body {
            bail!("Function {} already defined", func_decl.name);
        }

        match old_decl.attrs {
            IdentifierAttrs::FuncAttr { defined: _, global } => {
                if global
                    && func_decl
                        .storage_class
                        .is_some_and(|sc| sc == StorageClass::Static)
                {
                    bail!(
                        "Static function declaration follows non-static {}",
                        func_decl.name
                    );
                }

                is_global = global;
            }
            _ => unreachable!(),
        }
    }

    let symbol = Symbol {
        ty: fun_type,
        attrs: IdentifierAttrs::FuncAttr {
            defined: already_defined || has_body,
            global: is_global,
        },
    };
    SYMBOL_TABLE
        .lock()
        .unwrap()
        .insert(func_decl.name.clone(), symbol);

    if has_body {
        for param in &func_decl.params {
            let symbol = Symbol {
                ty: Type::Int,
                attrs: IdentifierAttrs::LocalAttr,
            };
            SYMBOL_TABLE.lock().unwrap().insert(param.clone(), symbol);
        }

        typecheck_block(&func_decl.body.as_ref().clone().unwrap())?;
    }

    Ok(())
}

pub fn typecheck_block(block: &BlockItem) -> Result<()> {
    match block {
        BlockItem::Declaration(decl) => match decl {
            Declaration::Variable(var_decl) => typecheck_variable_declaration(var_decl),
            Declaration::Function(func_decl) => typecheck_function_declaration(func_decl),
        },
        BlockItem::Statement(stmt) => typecheck_statement(stmt),
    }
}

fn typecheck_statement(stmt: &Statement) -> Result<()> {
    match stmt {
        Statement::Program(ProgramStatement { block_items: stmts }) => {
            for block_item in stmts {
                typecheck_block(block_item)?;
            }

            Ok(())
        }
        Statement::Expression(ExpressionStatement { expr }) => typecheck_expr(expr),
        Statement::Compound(BlockStatement { stmts }) => {
            for stmt in stmts {
                typecheck_block(stmt)?;
            }

            Ok(())
        }
        Statement::If(IfStatement {
            condition,
            then_branch,
            else_branch,
        }) => {
            typecheck_expr(condition)?;
            typecheck_block(&*then_branch)?;

            if else_branch.is_some() {
                typecheck_block(&else_branch.as_ref().clone().unwrap())?;
            }

            Ok(())
        }
        Statement::While(WhileStatement {
            condition,
            body,
            label: _,
        }) => {
            typecheck_expr(condition)?;
            typecheck_block(&body)?;

            Ok(())
        }
        Statement::DoWhile(DoWhileStatement {
            condition,
            body,
            label: _,
        }) => {
            typecheck_expr(condition)?;
            typecheck_block(&body)?;

            Ok(())
        }
        Statement::For(ForStatement {
            init,
            condition,
            post,
            body,
            label: _,
        }) => {
            match init {
                ForInit::Declaration(decl) => {
                    if decl.storage_class.is_some() {
                        bail!("Storage class specifier in for loop initializer");
                    }
                }
                _ => {}
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

            typecheck_block(&body)?;

            Ok(())
        }
        Statement::Return(ReturnStatement { expr }) => {
            typecheck_expr(expr)?;

            Ok(())
        }
        Statement::Break(_) | Statement::Continue(_) | Statement::Null => Ok(()),
    }
}

fn typecheck_expr(e: &Expression) -> Result<()> {
    match e {
        Expression::Call(CallExpression { name, args }) => {
            let f = SYMBOL_TABLE.lock().unwrap().get(name).cloned().unwrap();
            let f_type = f.ty.clone();

            if f_type == Type::Int {
                bail!("{} is not a function", name);
            }

            if let Type::Func(param_count) = f_type {
                if param_count != args.len() {
                    bail!(
                        "Function {} expects {} arguments, got {}",
                        name,
                        param_count,
                        args.len()
                    );
                }
            }

            for arg in args {
                typecheck_expr(arg)?;
            }

            Ok(())
        }
        Expression::Variable(var) => {
            if let Some(symbol) = SYMBOL_TABLE.lock().unwrap().get(var) {
                if symbol.ty != Type::Int {
                    bail!("{} is not a variable", var);
                }
            }

            Ok(())
        }
        Expression::Binary(BinaryExpression { kind: _, lhs, rhs }) => {
            typecheck_expr(&lhs)?;
            typecheck_expr(&rhs)?;

            Ok(())
        }
        Expression::Assign(AssignExpression { op: _, lhs, rhs }) => {
            typecheck_expr(&lhs)?;
            typecheck_expr(&rhs)?;

            Ok(())
        }
        Expression::Conditional(ConditionalExpression {
            condition,
            then_expr,
            else_expr,
        }) => {
            typecheck_expr(&condition)?;
            typecheck_expr(&then_expr)?;
            typecheck_expr(&else_expr)?;

            Ok(())
        }
        Expression::Unary(UnaryExpression { kind: _, expr }) => {
            typecheck_expr(&expr)?;

            Ok(())
        }
        Expression::Constant(_) => Ok(()),
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
    Initial(i32),
    NoInitializer,
}
