use anyhow::{bail, Result};

use crate::{
    ir::make_temporary,
    parser::{
        BlockItem, BlockStatement, BreakStatement, ContinueStatement, Declaration,
        DoWhileStatement, ExpressionStatement, ForStatement, FunctionDeclaration, IfStatement,
        ProgramStatement, ReturnStatement, Statement, WhileStatement,
    },
};

pub trait LoopLabel {
    fn loop_label(&mut self, current_label: &str) -> Result<&mut Self>;
}

impl LoopLabel for ProgramStatement {
    fn loop_label(&mut self, current_label: &str) -> Result<&mut Self> {
        for block_item in self.block_items.iter_mut() {
            block_item.loop_label(current_label)?;
        }
        Ok(self)
    }
}

impl LoopLabel for BlockStatement {
    fn loop_label(&mut self, current_label: &str) -> Result<&mut Self> {
        for stmt in self.stmts.iter_mut() {
            stmt.loop_label(current_label)?;
        }
        Ok(self)
    }
}

impl LoopLabel for IfStatement {
    fn loop_label(&mut self, current_label: &str) -> Result<&mut Self> {
        self.then_branch.loop_label(current_label)?;

        if let Some(ref mut else_branch) = *self.else_branch {
            else_branch.loop_label(current_label)?;
        }

        Ok(self)
    }
}

impl LoopLabel for BreakStatement {
    fn loop_label(&mut self, current_label: &str) -> Result<&mut Self> {
        if current_label.is_empty() {
            bail!("break statement not within loop");
        }

        self.label = current_label.to_string();

        Ok(self)
    }
}

impl LoopLabel for ContinueStatement {
    fn loop_label(&mut self, current_label: &str) -> Result<&mut Self> {
        if current_label.is_empty() {
            bail!("continue statement not within loop");
        }

        self.label = current_label.to_string();

        Ok(self)
    }
}

impl LoopLabel for WhileStatement {
    fn loop_label(&mut self, _current_label: &str) -> Result<&mut Self> {
        let new_label = format!("While.{}", make_temporary());

        self.body.loop_label(&new_label)?;
        self.label = new_label;

        Ok(self)
    }
}

impl LoopLabel for DoWhileStatement {
    fn loop_label(&mut self, _current_label: &str) -> Result<&mut Self> {
        let new_label = format!("DoWhile.{}", make_temporary());

        self.body.loop_label(&new_label)?;
        self.label = new_label;

        Ok(self)
    }
}

impl LoopLabel for ForStatement {
    fn loop_label(&mut self, _current_label: &str) -> Result<&mut Self> {
        let new_label = format!("For.{}", make_temporary());
        self.body.loop_label(&new_label)?;

        self.label = new_label;

        Ok(self)
    }
}

impl LoopLabel for ReturnStatement {
    fn loop_label(&mut self, _current_label: &str) -> Result<&mut Self> {
        Ok(self)
    }
}

impl LoopLabel for ExpressionStatement {
    fn loop_label(&mut self, _current_label: &str) -> Result<&mut Self> {
        Ok(self)
    }
}

impl LoopLabel for Statement {
    fn loop_label(&mut self, current_label: &str) -> Result<&mut Self> {
        match self {
            Statement::Program(p) => {
                p.loop_label(current_label)?;
            }
            Statement::Compound(b) => {
                b.loop_label(current_label)?;
            }
            Statement::If(i) => {
                i.loop_label(current_label)?;
            }
            Statement::Break(b) => {
                b.loop_label(current_label)?;
            }
            Statement::Continue(c) => {
                c.loop_label(current_label)?;
            }
            Statement::While(w) => {
                w.loop_label(current_label)?;
            }
            Statement::DoWhile(d) => {
                d.loop_label(current_label)?;
            }
            Statement::For(f) => {
                f.loop_label(current_label)?;
            }
            Statement::Expression(e) => {
                e.loop_label(current_label)?;
            }
            Statement::Return(r) => {
                r.loop_label(current_label)?;
            }
            Self::Null => {}
        }
        Ok(self)
    }
}

impl LoopLabel for BlockItem {
    fn loop_label(&mut self, current_label: &str) -> Result<&mut Self> {
        match self {
            BlockItem::Statement(s) => {
                s.loop_label(current_label)?;
                Ok(self)
            }

            BlockItem::Declaration(decl) => {
                decl.loop_label(current_label)?;
                Ok(self)
            }
        }
    }
}

impl LoopLabel for Declaration {
    fn loop_label(&mut self, current_label: &str) -> Result<&mut Self> {
        match self {
            Declaration::Variable(_) => Ok(self),
            Declaration::Function(f) => {
                f.loop_label(current_label)?;
                Ok(self)
            }
            _ => todo!(),
        }
    }
}

impl LoopLabel for FunctionDeclaration {
    fn loop_label(&mut self, current_label: &str) -> Result<&mut Self> {
        if let Some(ref mut body) = *self.body {
            body.loop_label(current_label)?;
        }
        Ok(self)
    }
}
