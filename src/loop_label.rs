use anyhow::{bail, Result};

use crate::{ir::make_temporary, parser::{BlockItem, BlockStatement, BreakStatement, ContinueStatement, Declaration, DoWhileStatement, ExpressionStatement, ForStatement, FunctionDeclaration, IfStatement, ProgramStatement, ReturnStatement, Statement, WhileStatement}};

pub trait Label {
    fn label(&self, current_label: String) -> Result<BlockItem>;
}

impl Label for ProgramStatement {
    fn label(&self, current_label: String) -> Result<BlockItem> {
        let labeled_items = self.stmts.iter().map(|item| item.label(current_label.clone())).collect::<Result<Vec<_>>>()?;
        Ok(BlockItem::Statement(Statement::Program(ProgramStatement { stmts: labeled_items })))
    }
}

impl Label for BlockStatement {
    fn label(&self, current_label: String) -> Result<BlockItem> {
        let labeled_items = self.stmts.iter().map(|item| item.label(current_label.clone())).collect::<Result<Vec<_>>>()?;
        Ok(BlockItem::Statement(Statement::Compound(BlockStatement { stmts: labeled_items })))
    }
}

impl Label for IfStatement {
    fn label(&self, current_label: String) -> Result<BlockItem> {
        let labeled_then_body = self.then_branch.label(current_label.clone())?;
        let labeled_else_body = label_optional_block_item(&self.else_branch, current_label.clone())?;
        Ok(BlockItem::Statement(Statement::If(IfStatement { condition: self.condition.clone(), then_branch: labeled_then_body.into(), else_branch: labeled_else_body.into() })))
    }
}

impl Label for BreakStatement {
    fn label(&self, current_label: String) -> Result<BlockItem> {
        if current_label.is_empty() {
            bail!("break statement not within loop");
        }

        Ok(BlockItem::Statement(Statement::Break(BreakStatement { label: current_label })))
    }
}

impl Label for ContinueStatement {
    fn label(&self, current_label: String) -> Result<BlockItem> {
        if current_label.is_empty() {
            bail!("continue statement not within loop");
        }

        Ok(BlockItem::Statement(Statement::Continue(ContinueStatement { label: current_label })))
    }
}

impl Label for WhileStatement {
    fn label(&self, _current_label: String) -> Result<BlockItem> {
        let new_label = format!("While.{}", make_temporary());
        let labeled_body = self.body.label(new_label.clone())?;

        Ok(BlockItem::Statement(Statement::While(WhileStatement { label: new_label, condition: self.condition.clone(), body: labeled_body.into() })))
    }
}

impl Label for DoWhileStatement {
    fn label(&self, _current_label: String) -> Result<BlockItem> {
        let new_label = format!("DoWhile.{}", make_temporary());
        let labeled_body = self.body.label(new_label.clone())?;

        Ok(BlockItem::Statement(Statement::DoWhile(DoWhileStatement { label: new_label, body: labeled_body.into(), condition: self.condition.clone() })))
    }
}

impl Label for ForStatement {
    fn label(&self, _current_label: String) -> Result<BlockItem> {
        let new_label = format!("For.{}", make_temporary());
        let labeled_body = self.body.label(new_label.clone())?;

        Ok(BlockItem::Statement(Statement::For(ForStatement { label: new_label, init: self.init.clone(), condition: self.condition.clone(), post: self.post.clone(), body: labeled_body.into() })))
    }
}

impl Label for ReturnStatement {
    fn label(&self, _current_label: String) -> Result<BlockItem> {
        Ok(BlockItem::Statement(Statement::Return(ReturnStatement { expr: self.expr.clone() })))
    }
}

impl Label for ExpressionStatement {
    fn label(&self, _current_label: String) -> Result<BlockItem> {
        Ok(BlockItem::Statement(Statement::Expression(self.clone())))
    }
}

impl Label for Statement {
    fn label(&self, current_label: String) -> Result<BlockItem> {
        match self {
            Statement::Program(p) => p.label(current_label),
            Statement::Compound(b) => b.label(current_label),
            Statement::If(i) => i.label(current_label),
            Statement::Break(b) => b.label(current_label),
            Statement::Continue(c) => c.label(current_label),
            Statement::While(w) => w.label(current_label),
            Statement::DoWhile(d) => d.label(current_label),
            Statement::For(f) => f.label(current_label),
            Statement::Expression(e) => e.label(current_label),
            Statement::Return(r) => r.label(current_label),
            Self::Null => Ok(BlockItem::Statement(Statement::Null)),
        }
    }
}

impl Label for BlockItem {
    fn label(&self, current_label: String) -> Result<BlockItem> {
        match self {
            BlockItem::Statement(s) => {
                let labeled_statement = s.label(current_label)?;
                Ok(labeled_statement)
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
}


fn label_optional_block_item(block_item: &Option<BlockItem>, current_label: String) -> Result<Option<BlockItem>> {
    match block_item {
        Some(b) => {
            let labeled_block_item = b.label( current_label)?;

            Ok(Some(labeled_block_item))
        }

        None => Ok(None),
    }
}