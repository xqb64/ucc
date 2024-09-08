use anyhow::{bail, Result};

use crate::{ir::make_temporary, parser::{BlockItem, BlockStatement, BreakStatement, ContinueStatement, Declaration, DoWhileStatement, ForStatement, FunctionDeclaration, IfStatement, ProgramStatement, Statement, WhileStatement}};

pub fn label_statement(s: &Statement, current_label: String) -> Result<Statement> {
    match s {
        Statement::Break(BreakStatement { label: _ }) => {
            if current_label.is_empty() {
                bail!("break statement not within loop");
            }

            Ok(Statement::Break(BreakStatement { label: current_label }))
        }
        
        Statement::Continue(ContinueStatement { label: _ }) => {
            if current_label.is_empty() {
                bail!("continue statement not within loop");
            }

            Ok(Statement::Continue(ContinueStatement { label: current_label }))
        }

        Statement::While(WhileStatement { label: _, condition, body }) => {
            let new_label = format!("While.{}", make_temporary());
            let labeled_body = label_block_item(body, new_label.clone())?;

            Ok(Statement::While(WhileStatement { label: new_label, condition: condition.clone(), body: labeled_body.into() }))
        }

        Statement::DoWhile(DoWhileStatement { label: _, body, condition }) => {
            let new_label = format!("DoWhile.{}", make_temporary());
            let labeled_body = label_block_item(body, new_label.clone())?;

            Ok(Statement::DoWhile(DoWhileStatement { label: new_label, body: labeled_body.into(), condition: condition.clone() }))
        }

        Statement::For(ForStatement { label: _, init, condition, post, body }) => {
            let new_label = format!("For.{}", make_temporary());
            let labeled_body = label_block_item(body, new_label.clone())?;

            Ok(Statement::For(ForStatement { label: new_label, init: init.clone(), condition: condition.clone(), post: post.clone(), body: labeled_body.into() }))
        }

        Statement::If(IfStatement { condition, then_branch, else_branch }) => {
            let labeled_then_body = label_block_item(then_branch, current_label.clone())?;
            let labeled_else_body = label_optional_block_item(else_branch, current_label.clone())?;

            Ok(Statement::If(IfStatement { condition: condition.clone(), then_branch: labeled_then_body.into(), else_branch: labeled_else_body.into() }))
        }

        Statement::Compound(BlockStatement { stmts }) => {
            let labeled_items = stmts.iter().map(|item| label_block_item(item, current_label.clone())).collect::<Result<Vec<_>>>()?;

            Ok(Statement::Compound(BlockStatement { stmts: labeled_items }))
        }

        Statement::Program(ProgramStatement { stmts }) => {
            let labeled_items = stmts.iter().map(|item| label_block_item(item, current_label.clone())).collect::<Result<Vec<_>>>()?;

            Ok(Statement::Program(ProgramStatement { stmts: labeled_items }))
        }

        _ => Ok(s.clone())
    }
}

fn label_block_item(block_item: &BlockItem, current_label: String) -> Result<BlockItem> {
    match block_item {
        BlockItem::Statement(s) => {
            let labeled_statement = label_statement(s, current_label)?;

            Ok(BlockItem::Statement(labeled_statement))
        }

        BlockItem::Declaration(decl) => {
            match decl {
                Declaration::Function(FunctionDeclaration { name, body }) => {
                    let labeled_body = label_optional_block_item(&body, current_label)?;
                    Ok(BlockItem::Declaration(Declaration::Function(FunctionDeclaration { name: name.clone(), body: labeled_body.into() })))
                }
                Declaration::Variable(var_decl) => Ok(BlockItem::Declaration(Declaration::Variable(var_decl.clone()))),
            }
        },
    }
}

fn label_optional_block_item(block_item: &Option<BlockItem>, current_label: String) -> Result<Option<BlockItem>> {
    match block_item {
        Some(b) => {
            let labeled_block_item = label_block_item(b, current_label)?;

            Ok(Some(labeled_block_item))
        }

        None => Ok(None),
    }
}