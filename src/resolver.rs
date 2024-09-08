use anyhow::{bail, Result};
use std::collections::HashMap;

use crate::{
    ir::make_temporary,
    parser::{
        AssignExpression, BinaryExpression, BlockItem, ConditionalExpression, Declaration, Expression, ExpressionStatement, FunctionDeclaration, IfStatement, ProgramStatement, ReturnStatement, Statement, UnaryExpression, VariableDeclaration
    },
};

fn resolve_declaration(
    decl: &Declaration,
    variable_map: &mut HashMap<String, String>,
) -> Result<Declaration> {
    match decl.to_owned() {
        Declaration::Variable(VariableDeclaration { name, mut init }) => {
            if variable_map.contains_key(&name) {
                bail!("redeclaration of variable: {}", name);
            }

            let unique_name = format!("var.{}.{}", name, make_temporary());

            variable_map.insert(name.clone(), unique_name.clone());

            if init.is_some() {
                init = Some(resolve_exp(&init.unwrap(), variable_map)?);
            }

            Ok(Declaration::Variable(VariableDeclaration {
                name: unique_name,
                init,
            }))
        }
        Declaration::Function(func) => {
            let resolved_block_items = func
                .body
                .iter()
                .map(|block_item| resolve_block_item(block_item, variable_map))
                .collect::<Result<Vec<_>>>()?;
            Ok(Declaration::Function(FunctionDeclaration {
                name: func.name.clone(),
                body: resolved_block_items,
            }))
        }
    }
}

pub fn resolve_block_item(
    item: &BlockItem,
    variable_map: &mut HashMap<String, String>,
) -> Result<BlockItem> {
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

pub fn resolve_statement(
    s: &Statement,
    variable_map: &mut HashMap<String, String>,
) -> Result<Statement> {
    match s {
        Statement::Expression(ExpressionStatement { expr }) => {
            let resolved_exp = resolve_exp(expr, variable_map)?;
            Ok(Statement::Expression(ExpressionStatement {
                expr: resolved_exp,
            }))
        }
        Statement::Return(ReturnStatement { expr }) => {
            let resolved_exp = resolve_exp(expr, variable_map)?;
            Ok(Statement::Return(ReturnStatement { expr: resolved_exp }))
        }
        Statement::Null => Ok(Statement::Null),
        Statement::Program(prog) => {
            let resolved_block_items = prog
                .stmts
                .iter()
                .map(|block_item| resolve_block_item(block_item, variable_map))
                .collect::<Result<Vec<_>>>()?;
            Ok(Statement::Program(ProgramStatement {
                stmts: resolved_block_items,
            }))
        }
        Statement::If(IfStatement { condition, then_branch, else_branch }) => {
            let resolved_condition = resolve_exp(condition, variable_map)?;
            let resolved_then_branch = then_branch.iter().map(|block_item| resolve_block_item(block_item, variable_map)).collect::<Result<Vec<_>>>()?;
            let mut resolved_else_branch = None;
            if else_branch.is_some() {
                resolved_else_branch = Some(else_branch.as_ref().unwrap().iter().map(|block_item| resolve_block_item(block_item, variable_map)).collect::<Result<Vec<_>>>()?);
            }
            Ok(Statement::If(IfStatement {
                condition: resolved_condition,
                then_branch: resolved_then_branch.into(),
                else_branch: resolved_else_branch.into(),
            }))
        }
    }
}

fn resolve_exp(exp: &Expression, variable_map: &mut HashMap<String, String>) -> Result<Expression> {
    match exp.to_owned() {
        Expression::Assign(AssignExpression { op, ref lhs, rhs }) => {
            if let Expression::Variable(_) = &**lhs {
                let resolved_lhs = resolve_exp(&lhs, variable_map)?;
                let resolved_rhs = resolve_exp(&rhs, variable_map)?;

                Ok(Expression::Assign(AssignExpression {
                    op,
                    lhs: resolved_lhs.into(),
                    rhs: resolved_rhs.into(),
                }))
            } else {
                bail!("left-hand side of assignment must be a variable");
            }
        }
        Expression::Variable(name) => {
            let variable = variable_map
                .get(&name)
                .ok_or_else(|| anyhow::anyhow!("undeclared variable: {}", name))?;
            Ok(Expression::Variable(variable.clone()))
        }
        Expression::Constant(konst) => Ok(Expression::Constant(konst)),
        Expression::Unary(UnaryExpression { kind, expr }) => {
            let resolved_expr = resolve_exp(&*expr, variable_map)?;

            Ok(Expression::Unary(UnaryExpression {
                kind,
                expr: resolved_expr.into(),
            }))
        }
        Expression::Binary(BinaryExpression { kind, lhs, rhs }) => {
            let resolved_lhs = resolve_exp(&*lhs, variable_map)?;
            let resolved_rhs = resolve_exp(&*rhs, variable_map)?;

            Ok(Expression::Binary(BinaryExpression {
                kind,
                lhs: resolved_lhs.into(),
                rhs: resolved_rhs.into(),
            }))
        }
        Expression::Conditional(ConditionalExpression { condition, then_expr, else_expr }) => {
            let resolved_condition = resolve_exp(&*condition, variable_map)?;
            let resolved_then_expr = resolve_exp(&*then_expr, variable_map)?;
            let resolved_else_expr = resolve_exp(&*else_expr, variable_map)?;

            match (&resolved_condition, &resolved_then_expr, &resolved_else_expr) {
                (Expression::Binary(_), Expression::Assign(_), Expression::Assign(_)) => bail!("invalid ternary assignment"),
                _ => {}
            }

            Ok(Expression::Conditional(ConditionalExpression {
                condition: resolved_condition.into(),
                then_expr: resolved_then_expr.into(),
                else_expr: resolved_else_expr.into(),
            }))
        }
    }
}
