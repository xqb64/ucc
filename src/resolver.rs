use std::collections::HashMap;
use anyhow::{bail, Result};

use crate::{ir::make_temporary, parser::{AssignExpression, BinaryExpression, BlockItem, Declaration, Expression, ExpressionStatement, FunctionDeclaration, ProgramStatement, ReturnStatement, Statement, UnaryExpression, VariableDeclaration}};

fn resolve_declaration(decl: Declaration, variable_map: &mut HashMap<String, String>) -> Result<Declaration> {
    match decl {
        Declaration::Variable(VariableDeclaration { name, mut init }) => {
            if variable_map.contains_key(&name) {
                bail!("redeclaration of variable: {}", name);
            }

            let unique_name = format!("var.{}.{}", name, make_temporary());

            variable_map.insert(name.clone(), unique_name.clone());

            if init.is_some() {
                init = Some(resolve_exp(init.unwrap(), variable_map)?);
            }

            Ok(Declaration::Variable(VariableDeclaration { name: unique_name, init }))
        }
        Declaration::Function(func) => {
            let mut resolved_block_items = vec![];
            for block_item in func.body {
                resolved_block_items.push(resolve_block_item(block_item, variable_map)?);
            }
            Ok(Declaration::Function(FunctionDeclaration { name: func.name, body: resolved_block_items }))
        }
    }
}

pub fn resolve_block_item(item: BlockItem, variable_map: &mut HashMap<String, String>) -> Result<BlockItem> {
    match item {
        BlockItem::Declaration(decl) => {
            let resolved_decl = resolve_declaration(decl, variable_map)?;
            Ok(BlockItem::Declaration(resolved_decl))
        }
        BlockItem::Statement(stmt) => {
            let resolved_stmt = resolve_statement(stmt, variable_map)?;
            Ok(BlockItem::Statement(resolved_stmt))
        }
    }
}

pub fn resolve_statement(s: Statement, variable_map: &mut HashMap<String, String>) -> Result<Statement> {
    match s {
        Statement::Expression(ExpressionStatement { expr }) => {
            let resolved_exp = resolve_exp(expr, variable_map)?;
            Ok(Statement::Expression(ExpressionStatement { expr: resolved_exp }))
        }
        Statement::Return(ReturnStatement { expr }) => {
            let resolved_exp = resolve_exp(expr, variable_map)?;
            Ok(Statement::Return(ReturnStatement { expr: resolved_exp } ))
        }
        Statement::Null => Ok(Statement::Null),
        Statement::Program(prog) => {
            let mut resolved_block_items = vec![];
            for block_item in prog.stmts {
                resolved_block_items.push(resolve_block_item(block_item, variable_map)?);
            }
            Ok(Statement::Program(ProgramStatement { stmts: resolved_block_items }))
        }
    }
}

fn resolve_exp(exp: Expression, variable_map: &mut HashMap<String, String>) -> Result<Expression> {
    match exp {
        Expression::Assign(AssignExpression { op, lhs, rhs }) => {
            let valid = match *lhs {
                Expression::Variable(_) => true,
                _ => false,
            };

            if !valid {
                bail!("lhs of assignment must be a variable");
            }
            
            let resolved_lhs = resolve_exp(*lhs, variable_map)?;
            let resolved_rhs = resolve_exp(*rhs, variable_map)?;

            Ok(Expression::Assign(AssignExpression { op, lhs: resolved_lhs.into(), rhs: resolved_rhs.into() }))
        }
        Expression::Variable(name) => {
            if variable_map.contains_key(&name) {
                Ok(Expression::Variable(variable_map[&name].clone()))
            } else {
                bail!("undeclared variable: {}", name);
            }
        }
        Expression::Constant(konst) => Ok(Expression::Constant(konst)),
        Expression::Unary(UnaryExpression { kind, expr }) => {
            let resolved_expr = resolve_exp(*expr, variable_map)?;

            Ok(Expression::Unary(UnaryExpression { kind, expr: resolved_expr.into() }))
        }
        Expression::Binary(BinaryExpression { kind, lhs, rhs }) => {
            let resolved_lhs = resolve_exp(*lhs, variable_map)?;
            let resolved_rhs = resolve_exp(*rhs, variable_map)?;

            Ok(Expression::Binary(BinaryExpression { kind, lhs: resolved_lhs.into(), rhs: resolved_rhs.into() }))
        }
    }
}