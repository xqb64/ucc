use anyhow::{bail, Result};

use crate::{
    ir::make_temporary,
    parser::{
        BlockItem, BlockStatement, BreakStatement, ContinueStatement, Declaration,
        DoWhileStatement, ExpressionStatement, ForStatement, FunctionDeclaration, IfStatement,
        ProgramStatement, ReturnStatement, Statement, WhileStatement,
    },
};

pub trait Label {
    fn label(&mut self, current_label: &str) -> Result<&mut Self>;
}

impl Label for ProgramStatement {
    fn label(&mut self, current_label: &str) -> Result<&mut Self> {
        for block_item in self.block_items.iter_mut() {
            block_item.label(current_label)?;
        }
        Ok(self)
    }
}

impl Label for BlockStatement {
    fn label(&mut self, current_label: &str) -> Result<&mut Self> {
        for stmt in self.stmts.iter_mut() {
            stmt.label(current_label)?;
        }
        Ok(self)
    }
}

fn optionally_label_block_item(
    block_item: &mut Option<BlockItem>,
    current_label: &str,
) -> Result<Option<BlockItem>> {
    if block_item.is_some() {
        let labeled_block_item = block_item.as_mut().unwrap().label(current_label)?;
        Ok(Some(labeled_block_item.to_owned()))
    } else {
        Ok(None)
    }
}

impl Label for IfStatement {
    fn label(&mut self, current_label: &str) -> Result<&mut Self> {
        self.then_branch.label(current_label)?;
        self.else_branch = optionally_label_block_item(&mut self.else_branch, current_label)?.into();
        Ok(self)
    }
}

impl Label for BreakStatement {
    fn label(&mut self, current_label: &str) -> Result<&mut Self> {
        if current_label.is_empty() {
            bail!("break statement not within loop");
        }

        self.label = current_label.to_string();

        Ok(self)
    }
}

impl Label for ContinueStatement {
    fn label(&mut self, current_label: &str) -> Result<&mut Self> {
        if current_label.is_empty() {
            bail!("continue statement not within loop");
        }

        self.label = current_label.to_string();

        Ok(self)
    }
}

impl Label for WhileStatement {
    fn label(&mut self, _current_label: &str) -> Result<&mut Self> {
        let new_label = format!("While.{}", make_temporary());
        
        self.body.label(&new_label)?;
        self.label = new_label;

        Ok(self)
    }
}

impl Label for DoWhileStatement {
    fn label(&mut self, _current_label: &str) -> Result<&mut Self> {
        let new_label = format!("DoWhile.{}", make_temporary());
        
        self.body.label(&new_label)?;
        self.label = new_label;

        Ok(self)
    }
}

impl Label for ForStatement {
    fn label(&mut self, _current_label: &str) -> Result<&mut Self> {
        let new_label = format!("For.{}", make_temporary());
        self.body.label(&new_label)?;

        self.label = new_label;

        Ok(self)
    }
}

impl Label for ReturnStatement {
    fn label(&mut self, _current_label: &str) -> Result<&mut Self> {
        Ok(self)
    }
}

impl Label for ExpressionStatement {
    fn label(&mut self, _current_label: &str) -> Result<&mut Self> {
        Ok(self)
    }
}

impl Label for Statement {
    fn label(&mut self, current_label: &str) -> Result<&mut Self> {
        match self {
            Statement::Program(p) => {
                p.label(current_label)?;
                Ok(self)
            },
            Statement::Compound(b) => {
                b.label(current_label)?;
                Ok(self)
            },
            Statement::If(i) => {
                i.label(current_label)?;
                Ok(self)
            }
            Statement::Break(b) => {
                b.label(current_label)?;
                Ok(self)
            }
            Statement::Continue(c) => {
                c.label(current_label)?;
                Ok(self)
            }
            Statement::While(w) => {
                w.label(current_label)?;
                Ok(self)
            }
            Statement::DoWhile(d) => {
                d.label(current_label)?;
                Ok(self)
            }
            Statement::For(f) => {
                f.label(current_label)?;
                Ok(self)
            }
            Statement::Expression(e) => {
                e.label(current_label)?;
                Ok(self)
            }
            Statement::Return(r) => {
                r.label(current_label)?;
                Ok(self)
            }
            Self::Null => Ok(self),
        }
    }
}

impl Label for BlockItem {
    fn label(&mut self, current_label: &str) -> Result<&mut Self> {
        match self {
            BlockItem::Statement(s) => {
                s.label(current_label)?;
                Ok(self)
            }

            BlockItem::Declaration(decl) => {
                decl.label(current_label)?;
                Ok(self)
            },
        }
    }
}

impl Label for Declaration {
    fn label(&mut self, current_label: &str) -> Result<&mut Self> {
        match self {
            Declaration::Variable(_) => Ok(self),
            Declaration::Function(f) => {
                f.label(current_label)?;
                Ok(self)
            }
        }
    }
}

impl Label for FunctionDeclaration {
    fn label(&mut self, current_label: &str) -> Result<&mut Self> {
        self.body = optionally_label_block_item(&mut self.body, current_label)?.into();
        Ok(self)
    }
}