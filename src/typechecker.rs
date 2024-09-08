use crate::parser::{
    AssignExpression, BinaryExpression, BlockItem, BlockStatement, CallExpression,
    ConditionalExpression, Declaration, DoWhileStatement, Expression, ExpressionStatement, ForInit,
    ForStatement, FunctionDeclaration, IfStatement, ProgramStatement, ReturnStatement, Statement,
    UnaryExpression, VariableDeclaration, WhileStatement,
};
use anyhow::{bail, Ok, Result};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Func(usize),
}

pub struct Symbol {
    ty: Type,
    defined: bool,
}

fn typecheck_variable_declaration(
    var_decl: &VariableDeclaration,
    symbol_table: &mut HashMap<String, Symbol>,
) -> Result<()> {
    symbol_table.insert(
        var_decl.name.clone(),
        Symbol {
            ty: Type::Int,
            defined: false,
        },
    );

    if var_decl.init.is_some() {
        typecheck_expr(var_decl.init.as_ref().unwrap(), symbol_table)?;
    }

    Ok(())
}

fn typecheck_function_declaration(
    func_decl: &FunctionDeclaration,
    symbol_table: &mut HashMap<String, Symbol>,
) -> Result<()> {
    let fun_type = Type::Func(func_decl.params.len());
    let has_body = func_decl.body.is_some();

    let mut already_defined = false;

    if symbol_table.contains_key(&func_decl.name) {
        let old_decl = symbol_table.get(&func_decl.name).unwrap();

        if old_decl.ty != fun_type {
            bail!(
                "Incompatible function declarations for function {}",
                func_decl.name
            );
        }

        already_defined = old_decl.defined;

        if already_defined && has_body {
            bail!("Function {} already defined", func_decl.name);
        }
    }

    symbol_table.insert(
        func_decl.name.clone(),
        Symbol {
            ty: fun_type,
            defined: already_defined || has_body,
        },
    );

    if has_body {
        for param in &func_decl.params {
            symbol_table.insert(
                param.clone(),
                Symbol {
                    ty: Type::Int,
                    defined: true,
                },
            );
        }

        typecheck_block(&func_decl.body.as_ref().clone().unwrap(), symbol_table)?;
    }

    Ok(())
}

pub fn typecheck_block(
    block: &BlockItem,
    symbol_table: &mut HashMap<String, Symbol>,
) -> Result<()> {
    match block {
        BlockItem::Declaration(decl) => match decl {
            Declaration::Variable(var_decl) => {
                typecheck_variable_declaration(var_decl, symbol_table)
            }
            Declaration::Function(func_decl) => {
                typecheck_function_declaration(func_decl, symbol_table)
            }
        },
        BlockItem::Statement(stmt) => typecheck_statement(stmt, symbol_table),
    }
}

fn typecheck_statement(stmt: &Statement, symbol_table: &mut HashMap<String, Symbol>) -> Result<()> {
    match stmt {
        Statement::Program(ProgramStatement { stmts }) => {
            for block_item in stmts {
                typecheck_block(block_item, symbol_table)?;
            }

            Ok(())
        }
        Statement::Expression(ExpressionStatement { expr }) => typecheck_expr(expr, symbol_table),
        Statement::Compound(BlockStatement { stmts }) => {
            for stmt in stmts {
                typecheck_block(stmt, symbol_table)?;
            }

            Ok(())
        }
        Statement::If(IfStatement {
            condition,
            then_branch,
            else_branch,
        }) => {
            typecheck_expr(condition, symbol_table)?;
            typecheck_block(&*then_branch, symbol_table)?;

            if else_branch.is_some() {
                typecheck_block(&else_branch.as_ref().clone().unwrap(), symbol_table)?;
            }

            Ok(())
        }
        Statement::While(WhileStatement {
            condition,
            body,
            label: _,
        }) => {
            typecheck_expr(condition, symbol_table)?;
            typecheck_block(&body, symbol_table)?;

            Ok(())
        }
        Statement::DoWhile(DoWhileStatement {
            condition,
            body,
            label: _,
        }) => {
            typecheck_expr(condition, symbol_table)?;
            typecheck_block(&body, symbol_table)?;

            Ok(())
        }
        Statement::For(ForStatement {
            init,
            condition,
            post,
            body,
            label: _,
        }) => {
            if let ForInit::Expression(Some(for_init_expr)) = init {
                typecheck_expr(for_init_expr, symbol_table)?;
            }

            if condition.is_some() {
                typecheck_expr(condition.as_ref().unwrap(), symbol_table)?;
            }

            if post.is_some() {
                typecheck_expr(post.as_ref().unwrap(), symbol_table)?;
            }

            typecheck_block(&body, symbol_table)?;

            Ok(())
        }
        Statement::Return(ReturnStatement { expr }) => {
            typecheck_expr(expr, symbol_table)?;

            Ok(())
        }
        Statement::Break(_) | Statement::Continue(_) | Statement::Null => Ok(()),
    }
}

fn typecheck_expr(e: &Expression, symbol_table: &mut HashMap<String, Symbol>) -> Result<()> {
    match e {
        Expression::Call(CallExpression { name, args }) => {
            let f = symbol_table.get(name).unwrap();
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
                typecheck_expr(arg, symbol_table)?;
            }

            Ok(())
        }
        Expression::Variable(var) => {
            if let Some(symbol) = symbol_table.get(var) {
                if symbol.ty != Type::Int {
                    bail!("{} is not a variable", var);
                }
            }

            Ok(())
        }
        Expression::Binary(BinaryExpression { kind: _, lhs, rhs }) => {
            typecheck_expr(&lhs, symbol_table)?;
            typecheck_expr(&rhs, symbol_table)?;

            Ok(())
        }
        Expression::Assign(AssignExpression { op: _, lhs, rhs }) => {
            typecheck_expr(&lhs, symbol_table)?;
            typecheck_expr(&rhs, symbol_table)?;

            Ok(())
        }
        Expression::Conditional(ConditionalExpression {
            condition,
            then_expr,
            else_expr,
        }) => {
            typecheck_expr(&condition, symbol_table)?;
            typecheck_expr(&then_expr, symbol_table)?;
            typecheck_expr(&else_expr, symbol_table)?;

            Ok(())
        }
        Expression::Unary(UnaryExpression { kind: _, expr }) => {
            typecheck_expr(&expr, symbol_table)?;

            Ok(())
        }
        Expression::Constant(_) => Ok(()),
    }
}
