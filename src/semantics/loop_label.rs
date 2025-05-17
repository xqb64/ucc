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
    fn loop_label(self, ctx: LabelContext) -> Result<Self>
    where
        Self: Sized;
}

impl LoopLabel for Program {
    fn loop_label(self, ctx: LabelContext) -> Result<Self> {
        let labeled_block_items = self
            .block_items
            .into_iter()
            .map(|block_item| block_item.loop_label(ctx))
            .collect::<Result<Vec<_>>>()?;
        Ok(Program {
            block_items: labeled_block_items,
        })
    }
}

impl LoopLabel for BlockStatement {
    fn loop_label(self, ctx: LabelContext) -> Result<Self> {
        let labeled_stmts = self
            .stmts
            .into_iter()
            .map(|stmt| stmt.loop_label(ctx))
            .collect::<Result<Vec<_>>>()?;
        Ok(BlockStatement {
            stmts: labeled_stmts.into(),
        })
    }
}

impl LoopLabel for IfStatement {
    fn loop_label(self, ctx: LabelContext) -> Result<Self> {
        let labeled_then = self.then_branch.loop_label(ctx)?;

        let labeled_else = match *self.else_branch {
            Some(else_branch) => else_branch.loop_label(ctx)?.into(),
            None => None,
        };

        Ok(IfStatement {
            condition: self.condition.clone(),
            then_branch: labeled_then.into(),
            else_branch: labeled_else.into(),
        })
    }
}

impl LoopLabel for BreakStatement {
    fn loop_label(self, ctx: LabelContext) -> Result<Self> {
        match ctx.innermost {
            LabelKind::Loop(label) | LabelKind::Switch(label) => Ok(BreakStatement {
                label: label.to_string(),
            }),
            LabelKind::None => bail!("break statement not within loop or switch"),
        }
    }
}

impl LoopLabel for ContinueStatement {
    fn loop_label(self, ctx: LabelContext) -> Result<Self> {
        if ctx.loop_label.is_empty() {
            bail!("continue statement not within loop");
        }

        Ok(ContinueStatement {
            label: ctx.loop_label.to_string(),
        })
    }
}

impl LoopLabel for SwitchStatement {
    fn loop_label(self, ctx: LabelContext) -> Result<Self> {
        let new_switch_label = format!(
            "{}{}Switch.{}",
            ctx.loop_label,
            if !ctx.loop_label.is_empty() { "." } else { "" },
            make_temporary()
        );

        let new_ctx = LabelContext {
            loop_label: ctx.loop_label,
            switch_label: &new_switch_label,
            innermost: LabelKind::Switch(&new_switch_label),
        };

        let labeled_body = self.body.loop_label(new_ctx)?;

        Ok(SwitchStatement {
            condition: self.condition.clone(),
            body: labeled_body.into(),
            label: new_switch_label,
            cases: self.cases.clone(),
        })
    }
}

impl LoopLabel for CaseStatement {
    fn loop_label(self, ctx: LabelContext) -> Result<Self> {
        let labeled_body = self.body.loop_label(ctx)?;
        let label = format!("{}.case.{}", ctx.switch_label, make_temporary());
        Ok(CaseStatement {
            value: self.value.clone(),
            body: labeled_body.into(),
            label,
        })
    }
}

impl LoopLabel for DefaultStatement {
    fn loop_label(self, ctx: LabelContext) -> Result<Self> {
        let labeled_body = self.body.loop_label(ctx)?;
        let label = format!("{}.default", ctx.switch_label);
        Ok(DefaultStatement {
            body: labeled_body.into(),
            label,
        })
    }
}

impl LoopLabel for WhileStatement {
    fn loop_label(self, ctx: LabelContext) -> Result<Self> {
        let new_loop_label = format!(
            "{}{}While.{}",
            ctx.loop_label,
            if !ctx.loop_label.is_empty() { "." } else { "" },
            make_temporary()
        );

        let new_ctx = LabelContext {
            loop_label: &new_loop_label,
            switch_label: ctx.switch_label,
            innermost: LabelKind::Loop(&new_loop_label),
        };

        let labeled_body = self.body.loop_label(new_ctx)?;

        Ok(WhileStatement {
            condition: self.condition.clone(),
            body: labeled_body.into(),
            label: new_loop_label,
        })
    }
}

impl LoopLabel for DoWhileStatement {
    fn loop_label(self, ctx: LabelContext) -> Result<Self> {
        let new_loop_label = format!(
            "{}{}DoWhile.{}",
            ctx.loop_label,
            if !ctx.loop_label.is_empty() { "." } else { "" },
            make_temporary()
        );

        let new_ctx = LabelContext {
            loop_label: &new_loop_label,
            switch_label: ctx.switch_label,
            innermost: LabelKind::Loop(&new_loop_label),
        };

        let labeled_body = self.body.loop_label(new_ctx)?;

        Ok(DoWhileStatement {
            condition: self.condition.clone(),
            body: labeled_body.into(),
            label: new_loop_label,
        })
    }
}

impl LoopLabel for ForStatement {
    fn loop_label(self, ctx: LabelContext) -> Result<Self> {
        let new_loop_label = format!(
            "{}{}For.{}",
            ctx.loop_label,
            if !ctx.loop_label.is_empty() { "." } else { "" },
            make_temporary()
        );

        let new_ctx = LabelContext {
            loop_label: &new_loop_label,
            switch_label: ctx.switch_label,
            innermost: LabelKind::Loop(&new_loop_label),
        };
        let labeled_body = self.body.loop_label(new_ctx)?;

        Ok(ForStatement {
            init: self.init.clone(),
            condition: self.condition.clone(),
            post: self.post.clone(),
            body: labeled_body.into(),
            label: new_loop_label,
        })
    }
}

impl LoopLabel for ReturnStatement {
    fn loop_label(self, _ctx: LabelContext) -> Result<Self> {
        Ok(ReturnStatement {
            expr: self.expr.clone(),
            target_type: self.target_type.clone(),
            belongs_to: self.belongs_to.clone(),
        })
    }
}

impl LoopLabel for ExpressionStatement {
    fn loop_label(self, _ctx: LabelContext) -> Result<Self> {
        Ok(ExpressionStatement {
            expr: self.expr.clone(),
        })
    }
}

impl LoopLabel for GotoStatement {
    fn loop_label(self, _ctx: LabelContext) -> Result<Self> {
        Ok(GotoStatement {
            label: self.label.clone(),
        })
    }
}

impl LoopLabel for LabeledStatement {
    fn loop_label(self, ctx: LabelContext) -> Result<Self> {
        let labeled_body = self.body.loop_label(ctx)?;

        Ok(LabeledStatement {
            label: self.label.clone(),
            body: labeled_body.into(),
        })
    }
}

impl LoopLabel for Statement {
    fn loop_label(self, ctx: LabelContext) -> Result<Self> {
        match self {
            Statement::Compound(stmt_block) => {
                let labeled = stmt_block.loop_label(ctx)?;
                Ok(Statement::Compound(labeled))
            }

            Statement::If(stmt_if) => {
                let labeled = stmt_if.loop_label(ctx)?;
                Ok(Statement::If(labeled))
            }

            Statement::Break(stmt_break) => {
                let labeled = stmt_break.loop_label(ctx)?;
                Ok(Statement::Break(labeled))
            }

            Statement::Continue(stmt_continue) => {
                let labeled = stmt_continue.loop_label(ctx)?;
                Ok(Statement::Continue(labeled))
            }

            Statement::While(stmt_while) => {
                let labeled = stmt_while.loop_label(ctx)?;
                Ok(Statement::While(labeled))
            }

            Statement::DoWhile(stmt_do_while) => {
                let labeled = stmt_do_while.loop_label(ctx)?;
                Ok(Statement::DoWhile(labeled))
            }

            Statement::For(stmt_for) => {
                let labeled = stmt_for.loop_label(ctx)?;
                Ok(Statement::For(labeled))
            }

            Statement::Expression(stmt_expr) => {
                let labeled = stmt_expr.loop_label(ctx)?;
                Ok(Statement::Expression(labeled))
            }

            Statement::Return(stmt_return) => {
                let labeled = stmt_return.loop_label(ctx)?;
                Ok(Statement::Return(labeled))
            }

            Statement::Goto(stmt_goto) => {
                let labeled = stmt_goto.loop_label(ctx)?;
                Ok(Statement::Goto(labeled))
            }

            Statement::Labeled(stmt_labeled) => {
                let labeled = stmt_labeled.loop_label(ctx)?;
                Ok(Statement::Labeled(labeled))
            }

            Statement::Switch(stmt_switch) => {
                let labeled = stmt_switch.loop_label(ctx)?;
                Ok(Statement::Switch(labeled))
            }

            Statement::Case(stmt_case) => {
                let labeled = stmt_case.loop_label(ctx)?;
                Ok(Statement::Case(labeled))
            }

            Statement::Default(stmt_default) => {
                let labeled = stmt_default.loop_label(ctx)?;
                Ok(Statement::Default(labeled))
            }

            Self::Null => Ok(Statement::Null),
        }
    }
}

impl LoopLabel for BlockItem {
    fn loop_label(self, ctx: LabelContext) -> Result<Self> {
        match self {
            BlockItem::Statement(stmt) => {
                let labeled_stmt = stmt.loop_label(ctx)?;
                Ok(BlockItem::Statement(labeled_stmt))
            }

            BlockItem::Declaration(decl) => {
                let labeled_decl = decl.loop_label(ctx)?;
                Ok(BlockItem::Declaration(labeled_decl))
            }
        }
    }
}

impl LoopLabel for Declaration {
    fn loop_label(self, ctx: LabelContext) -> Result<Self> {
        match self {
            Declaration::Variable(_) => Ok(self),
            Declaration::Function(func_decl) => {
                let labeled = func_decl.loop_label(ctx)?;
                Ok(Declaration::Function(labeled))
            }
            Declaration::Struct(_) => Ok(self),
        }
    }
}

impl LoopLabel for FunctionDeclaration {
    fn loop_label(self, ctx: LabelContext) -> Result<Self> {
        let labeled_body = match *self.body {
            Some(body) => body.loop_label(ctx)?.into(),
            None => None,
        };

        Ok(FunctionDeclaration {
            name: self.name.clone(),
            ty: self.ty.clone(),
            params: self.params.clone(),
            body: labeled_body.into(),
            is_global: self.is_global,
            storage_class: self.storage_class,
        })
    }
}
