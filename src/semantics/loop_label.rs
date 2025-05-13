use anyhow::{bail, Result};

use crate::{
    ir::gen::make_temporary,
    parser::ast::{
        BlockItem, BlockStatement, BreakStatement, CaseStatement, ContinueStatement, Declaration,
        DefaultStatement, DoWhileStatement, ExpressionStatement, ForStatement, FunctionDeclaration,
        GotoStatement, IfStatement, LabeledStatement, Program, ReturnStatement, Statement,
        SwitchStatement, WhileStatement,
    },
};

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct LabelContext<'a> {
    pub loop_label: &'a str,
    pub switch_label: &'a str,
    pub innermost: LabelKind<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LabelKind<'a> {
    None,
    Loop(&'a str),
    Switch(&'a str),
}

pub trait LoopLabel {
    fn loop_label(&mut self, ctx: LabelContext) -> Result<&mut Self>;
}

impl LoopLabel for Program {
    fn loop_label(&mut self, ctx: LabelContext) -> Result<&mut Self> {
        for block_item in self.block_items.iter_mut() {
            block_item.loop_label(ctx)?;
        }
        Ok(self)
    }
}

impl LoopLabel for BlockStatement {
    fn loop_label(&mut self, ctx: LabelContext) -> Result<&mut Self> {
        for stmt in self.stmts.iter_mut() {
            stmt.loop_label(ctx)?;
        }
        Ok(self)
    }
}

impl LoopLabel for IfStatement {
    fn loop_label(&mut self, ctx: LabelContext) -> Result<&mut Self> {
        self.then_branch.loop_label(ctx)?;

        if let Some(ref mut else_branch) = *self.else_branch {
            else_branch.loop_label(ctx)?;
        }

        Ok(self)
    }
}

impl LoopLabel for BreakStatement {
    fn loop_label(&mut self, ctx: LabelContext) -> Result<&mut Self> {
        match ctx.innermost {
            LabelKind::Loop(label) | LabelKind::Switch(label) => {
                self.label = label.to_string();
                Ok(self)
            }
            LabelKind::None => bail!("break statement not within loop or switch"),
        }
    }
}

impl LoopLabel for ContinueStatement {
    fn loop_label(&mut self, ctx: LabelContext) -> Result<&mut Self> {
        if ctx.loop_label.is_empty() {
            bail!("continue statement not within loop");
        }
        self.label = ctx.loop_label.to_string();
        Ok(self)
    }
}

impl LoopLabel for SwitchStatement {
    fn loop_label(&mut self, ctx: LabelContext) -> Result<&mut Self> {
        let new_switch_label = format!(
            "{}{}Switch.{}",
            ctx.loop_label,
            if !ctx.loop_label.is_empty() { "." } else { "" },
            make_temporary()
        );

        self.label = new_switch_label.clone();

        let new_ctx = LabelContext {
            loop_label: ctx.loop_label,
            switch_label: &new_switch_label,
            innermost: LabelKind::Switch(&new_switch_label),
        };
        self.body.loop_label(new_ctx)?;
        Ok(self)
    }
}

impl LoopLabel for CaseStatement {
    fn loop_label(&mut self, ctx: LabelContext) -> Result<&mut Self> {
        self.body.loop_label(ctx)?;
        self.label = format!("{}.case.{}", ctx.switch_label, make_temporary());
        Ok(self)
    }
}

impl LoopLabel for DefaultStatement {
    fn loop_label(&mut self, ctx: LabelContext) -> Result<&mut Self> {
        self.body.loop_label(ctx)?;
        self.label = format!("{}.default", ctx.switch_label);
        Ok(self)
    }
}

impl LoopLabel for WhileStatement {
    fn loop_label(&mut self, ctx: LabelContext) -> Result<&mut Self> {
        let new_loop_label = format!(
            "{}{}While.{}",
            ctx.loop_label,
            if !ctx.loop_label.is_empty() { "." } else { "" },
            make_temporary()
        );

        self.label = new_loop_label.clone();

        let new_ctx = LabelContext {
            loop_label: &new_loop_label,
            switch_label: ctx.switch_label,
            innermost: LabelKind::Loop(&new_loop_label),
        };
        self.body.loop_label(new_ctx)?;
        Ok(self)
    }
}

impl LoopLabel for DoWhileStatement {
    fn loop_label(&mut self, ctx: LabelContext) -> Result<&mut Self> {
        let new_loop_label = format!(
            "{}{}DoWhile.{}",
            ctx.loop_label,
            if !ctx.loop_label.is_empty() { "." } else { "" },
            make_temporary()
        );

        self.label = new_loop_label.clone();

        let new_ctx = LabelContext {
            loop_label: &new_loop_label,
            switch_label: ctx.switch_label,
            innermost: LabelKind::Loop(&new_loop_label),
        };
        self.body.loop_label(new_ctx)?;
        Ok(self)
    }
}

impl LoopLabel for ForStatement {
    fn loop_label(&mut self, ctx: LabelContext) -> Result<&mut Self> {
        let new_loop_label = format!(
            "{}{}For.{}",
            ctx.loop_label,
            if !ctx.loop_label.is_empty() { "." } else { "" },
            make_temporary()
        );

        self.label = new_loop_label.clone();

        let new_ctx = LabelContext {
            loop_label: &new_loop_label,
            switch_label: ctx.switch_label,
            innermost: LabelKind::Loop(&new_loop_label),
        };
        self.body.loop_label(new_ctx)?;
        Ok(self)
    }
}

impl LoopLabel for ReturnStatement {
    fn loop_label(&mut self, _ctx: LabelContext) -> Result<&mut Self> {
        Ok(self)
    }
}

impl LoopLabel for ExpressionStatement {
    fn loop_label(&mut self, _ctx: LabelContext) -> Result<&mut Self> {
        Ok(self)
    }
}

impl LoopLabel for GotoStatement {
    fn loop_label(&mut self, _ctx: LabelContext) -> Result<&mut Self> {
        Ok(self)
    }
}

impl LoopLabel for LabeledStatement {
    fn loop_label(&mut self, ctx: LabelContext) -> Result<&mut Self> {
        self.body.loop_label(ctx)?;

        Ok(self)
    }
}

impl LoopLabel for Statement {
    fn loop_label(&mut self, ctx: LabelContext) -> Result<&mut Self> {
        match self {
            Statement::Compound(b) => {
                b.loop_label(ctx)?;
            }

            Statement::If(i) => {
                i.loop_label(ctx)?;
            }

            Statement::Break(b) => {
                b.loop_label(ctx)?;
            }

            Statement::Continue(c) => {
                c.loop_label(ctx)?;
            }

            Statement::While(w) => {
                w.loop_label(ctx)?;
            }

            Statement::DoWhile(d) => {
                d.loop_label(ctx)?;
            }

            Statement::For(f) => {
                f.loop_label(ctx)?;
            }

            Statement::Expression(e) => {
                e.loop_label(ctx)?;
            }

            Statement::Return(r) => {
                r.loop_label(ctx)?;
            }

            Statement::Goto(g) => {
                g.loop_label(ctx)?;
            }

            Statement::Labeled(l) => {
                l.loop_label(ctx)?;
            }

            Statement::Switch(s) => {
                s.loop_label(ctx)?;
            }

            Statement::Case(c) => {
                c.loop_label(ctx)?;
            }

            Statement::Default(d) => {
                d.loop_label(ctx)?;
            }

            Self::Null => {}
        }

        Ok(self)
    }
}

impl LoopLabel for BlockItem {
    fn loop_label(&mut self, ctx: LabelContext) -> Result<&mut Self> {
        match self {
            BlockItem::Statement(s) => {
                s.loop_label(ctx)?;
                Ok(self)
            }

            BlockItem::Declaration(decl) => {
                decl.loop_label(ctx)?;
                Ok(self)
            }
        }
    }
}

impl LoopLabel for Declaration {
    fn loop_label(&mut self, ctx: LabelContext) -> Result<&mut Self> {
        match self {
            Declaration::Variable(_) => Ok(self),
            Declaration::Function(f) => {
                f.loop_label(ctx)?;
                Ok(self)
            }
            Declaration::Struct(_) => Ok(self),
        }
    }
}

impl LoopLabel for FunctionDeclaration {
    fn loop_label(&mut self, ctx: LabelContext) -> Result<&mut Self> {
        if let Some(ref mut body) = *self.body {
            body.loop_label(ctx)?;
        }
        Ok(self)
    }
}
