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

                if let Some(Expression::Constant(konst)) = self.init {
                    initial_value = todo!();
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

                    if old_decl.ty != Type::Int {
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
                    ty: Type::Int,
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
                        if old_decl.ty != Type::Int {
                            bail!("Function {} redeclared as variable", self.name);
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
                            .insert(self.name.clone(), symbol);
                    }
                } else if self
                    .storage_class
                    .is_some_and(|sc| sc == StorageClass::Static)
                {
                    if let Some(Expression::Constant(konst)) = self.init {
                        initial_value = todo!();
                    } else if self.init.is_none() {
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
                        .insert(self.name.clone(), symbol);
                } else {
                    let symbol = Symbol {
                        ty: Type::Int,
                        attrs: IdentifierAttrs::LocalAttr,
                    };
                    SYMBOL_TABLE
                        .lock()
                        .unwrap()
                        .insert(self.name.clone(), symbol);
                    if self.init.is_some() {
                        self.init.as_ref().unwrap().typecheck()?;
                    }
                }
            }
        }

        Ok(())
    }
}

impl Typecheck for FunctionDeclaration {
    fn typecheck(&self) -> Result<()> {
        let fun_type = Type::Func(self.params.len());
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
            ty: fun_type,
            attrs: IdentifierAttrs::FuncAttr {
                defined: already_defined || has_body,
                global: is_global,
            },
        };
        SYMBOL_TABLE
            .lock()
            .unwrap()
            .insert(self.name.clone(), symbol);

        if has_body {
            for param in &self.params {
                let symbol = Symbol {
                    ty: Type::Int,
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
            Statement::Expression(ExpressionStatement { expr }) => expr.typecheck(),
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
                condition.typecheck()?;
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
                condition.typecheck()?;
                body.typecheck()?;

                Ok(())
            }
            Statement::DoWhile(DoWhileStatement {
                condition,
                body,
                label: _,
            }) => {
                condition.typecheck()?;
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

                if let ForInit::Expression(Some(for_init_expr)) = init {
                    for_init_expr.typecheck()?;
                }

                if condition.is_some() {
                    condition.as_ref().unwrap().typecheck()?;
                }

                if post.is_some() {
                    post.as_ref().unwrap().typecheck()?;
                }

                body.typecheck()?;

                Ok(())
            }
            Statement::Return(ReturnStatement { expr }) => {
                expr.typecheck()?;

                Ok(())
            }
            Statement::Break(_) | Statement::Continue(_) | Statement::Null => Ok(()),
        }
    }
}

impl Typecheck for Expression {
    fn typecheck(&self) -> Result<()> {
        match self {
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
                    arg.typecheck()?;
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
                lhs.typecheck()?;
                rhs.typecheck()?;

                Ok(())
            }
            Expression::Assign(AssignExpression { op: _, lhs, rhs }) => {
                lhs.typecheck()?;
                rhs.typecheck()?;

                Ok(())
            }
            Expression::Conditional(ConditionalExpression {
                condition,
                then_expr,
                else_expr,
            }) => {
                condition.typecheck()?;
                then_expr.typecheck()?;
                else_expr.typecheck()?;

                Ok(())
            }
            Expression::Unary(UnaryExpression { kind: _, expr }) => {
                expr.typecheck()?;

                Ok(())
            }
            Expression::Constant(_) => Ok(()),
        }
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
