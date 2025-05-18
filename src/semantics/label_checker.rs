use crate::parser::ast::{
    BlockItem, BlockStatement, BreakStatement, CaseStatement, ContinueStatement, Declaration,
    DefaultStatement, DoWhileStatement, ExpressionStatement, ForStatement, FunctionDeclaration,
    GotoStatement, IfStatement, LabeledStatement, Program, ReturnStatement, Statement,
    SwitchStatement, WhileStatement,
};
use anyhow::{bail, Result};
use std::collections::HashSet;

pub trait LabelCollect {
    fn collect_labels(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self>
    where
        Self: Sized;
}

impl LabelCollect for Program {
    fn collect_labels(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        let collected_block_items = self
            .block_items
            .into_iter()
            .map(|block_item| block_item.collect_labels(labels, funcname))
            .collect::<Result<Vec<_>>>()?;
        Ok(Program {
            block_items: collected_block_items,
        })
    }
}

impl LabelCollect for BlockItem {
    fn collect_labels(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        match self {
            BlockItem::Declaration(decl) => {
                let collected = decl.collect_labels(labels, funcname)?;
                Ok(BlockItem::Declaration(collected))
            }

            BlockItem::Statement(stmt) => {
                let collected = stmt.collect_labels(labels, funcname)?;
                Ok(BlockItem::Statement(collected))
            }
        }
    }
}

impl LabelCollect for Declaration {
    fn collect_labels(self, labels: &mut HashSet<String>, _funcname: &str) -> Result<Self> {
        match self {
            Declaration::Variable(_) => Ok(self),
            Declaration::Function(func_decl) => {
                let name = func_decl.name.clone();
                let collected = func_decl.collect_labels(labels, &name)?;
                Ok(Declaration::Function(collected))
            }
            Declaration::Struct(_) => Ok(self),
        }
    }
}

impl LabelCollect for FunctionDeclaration {
    fn collect_labels(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        let collected = match *self.body {
            Some(body) => body.collect_labels(labels, funcname)?.into(),
            None => None,
        };

        Ok(FunctionDeclaration {
            name: self.name.clone(),
            ty: self.ty.clone(),
            params: self.params.clone(),
            body: collected.into(),
            is_global: self.is_global,
            storage_class: self.storage_class,
        })
    }
}

impl LabelCollect for Statement {
    fn collect_labels(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        match self {
            Statement::Return(stmt_return) => {
                let collected = stmt_return.collect_labels(labels, funcname)?;
                Ok(Statement::Return(collected))
            }

            Statement::Expression(stmt_expr) => {
                let collected = stmt_expr.collect_labels(labels, funcname)?;
                Ok(Statement::Expression(collected))
            }

            Statement::If(stmt_if) => {
                let collected = stmt_if.collect_labels(labels, funcname)?;
                Ok(Statement::If(collected))
            }

            Statement::Compound(stmt_compound) => {
                let collected = stmt_compound.collect_labels(labels, funcname)?;
                Ok(Statement::Compound(collected))
            }

            Statement::While(stmt_while) => {
                let collected = stmt_while.collect_labels(labels, funcname)?;
                Ok(Statement::While(collected))
            }

            Statement::DoWhile(stmt_do_while) => {
                let collected = stmt_do_while.collect_labels(labels, funcname)?;
                Ok(Statement::DoWhile(collected))
            }

            Statement::For(stmt_for) => {
                let collected = stmt_for.collect_labels(labels, funcname)?;
                Ok(Statement::For(collected))
            }

            Statement::Break(stmt_break) => {
                let collected = stmt_break.collect_labels(labels, funcname)?;
                Ok(Statement::Break(collected))
            }

            Statement::Continue(stmt_continue) => {
                let collected = stmt_continue.collect_labels(labels, funcname)?;
                Ok(Statement::Continue(collected))
            }

            Statement::Goto(stmt_goto) => {
                let collected = stmt_goto.collect_labels(labels, funcname)?;
                Ok(Statement::Goto(collected))
            }

            Statement::Labeled(stmt_labeled) => {
                let collected = stmt_labeled.collect_labels(labels, funcname)?;
                Ok(Statement::Labeled(collected))
            }

            Statement::Switch(stmt_switch) => {
                let collected = stmt_switch.collect_labels(labels, funcname)?;
                Ok(Statement::Switch(collected))
            }

            Statement::Case(stmt_case) => {
                let collected = stmt_case.collect_labels(labels, funcname)?;
                Ok(Statement::Case(collected))
            }

            Statement::Default(stmt_default) => {
                let collected = stmt_default.collect_labels(labels, funcname)?;
                Ok(Statement::Default(collected))
            }

            Self::Null => Ok(Statement::Null),
        }
    }
}

impl LabelCollect for BlockStatement {
    fn collect_labels(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        let collected_stmts = self
            .stmts
            .into_iter()
            .map(|stmt| stmt.collect_labels(labels, funcname))
            .collect::<Result<Vec<_>>>()?;

        Ok(BlockStatement {
            stmts: collected_stmts,
        })
    }
}

impl LabelCollect for SwitchStatement {
    fn collect_labels(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        let collected_body = self.body.collect_labels(labels, funcname)?;

        Ok(SwitchStatement {
            condition: self.condition.clone(),
            body: collected_body.into(),
            label: self.label.clone(),
            cases: self.cases.clone(),
        })
    }
}

impl LabelCollect for CaseStatement {
    fn collect_labels(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        let collected_body = self.body.collect_labels(labels, funcname)?;

        Ok(CaseStatement {
            value: self.value.clone(),
            body: collected_body.into(),
            label: self.label.clone(),
        })
    }
}

impl LabelCollect for DefaultStatement {
    fn collect_labels(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        let collected_body = self.body.collect_labels(labels, funcname)?;

        Ok(DefaultStatement {
            body: collected_body.into(),
            label: self.label.clone(),
        })
    }
}

impl LabelCollect for IfStatement {
    fn collect_labels(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        let collected_then = self.then_branch.collect_labels(labels, funcname)?;

        let collected_else = match *self.else_branch {
            Some(else_branch) => else_branch.collect_labels(labels, funcname)?.into(),
            None => None,
        };

        Ok(IfStatement {
            condition: self.condition.clone(),
            then_branch: collected_then.into(),
            else_branch: collected_else.into(),
        })
    }
}

impl LabelCollect for BreakStatement {
    fn collect_labels(self, _labels: &mut HashSet<String>, _funcname: &str) -> Result<Self> {
        Ok(self)
    }
}

impl LabelCollect for ContinueStatement {
    fn collect_labels(self, _labels: &mut HashSet<String>, _funcname: &str) -> Result<Self> {
        Ok(self)
    }
}

impl LabelCollect for WhileStatement {
    fn collect_labels(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        let collected_body = self.body.collect_labels(labels, funcname)?;
        Ok(WhileStatement {
            condition: self.condition.clone(),
            body: collected_body.into(),
            label: self.label.clone(),
        })
    }
}

impl LabelCollect for DoWhileStatement {
    fn collect_labels(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        let collected_body = self.body.collect_labels(labels, funcname)?;
        Ok(DoWhileStatement {
            condition: self.condition.clone(),
            body: collected_body.into(),
            label: self.label.clone(),
        })
    }
}

impl LabelCollect for ForStatement {
    fn collect_labels(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        let collected_body = self.body.collect_labels(labels, funcname)?;
        Ok(ForStatement {
            init: self.init.clone(),
            condition: self.condition.clone(),
            post: self.post.clone(),
            body: collected_body.into(),
            label: self.label.clone(),
        })
    }
}

impl LabelCollect for ReturnStatement {
    fn collect_labels(self, _labels: &mut HashSet<String>, _funcname: &str) -> Result<Self> {
        Ok(self)
    }
}

impl LabelCollect for ExpressionStatement {
    fn collect_labels(self, _labels: &mut HashSet<String>, _funcname: &str) -> Result<Self> {
        Ok(self)
    }
}

impl LabelCollect for GotoStatement {
    fn collect_labels(self, _labels: &mut HashSet<String>, _funcname: &str) -> Result<Self> {
        Ok(self)
    }
}

impl LabelCollect for LabeledStatement {
    fn collect_labels(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        if !labels.insert(format!("{}.{}", self.label.clone(), funcname)) {
            bail!("duplicate label: {}", self.label.clone());
        }

        let collected_body = self.body.collect_labels(labels, funcname)?;

        Ok(LabeledStatement {
            label: self.label.clone(),
            body: collected_body.into(),
        })
    }
}

pub trait LabelCheck {
    fn label_check(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self>
    where
        Self: Sized;
}

impl LabelCheck for Program {
    fn label_check(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        let checked_block_items = self
            .block_items
            .into_iter()
            .map(|block_item| block_item.label_check(labels, funcname))
            .collect::<Result<Vec<_>>>()?;
        Ok(Program {
            block_items: checked_block_items,
        })
    }
}

impl LabelCheck for BlockItem {
    fn label_check(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        match self {
            BlockItem::Statement(stmt) => {
                let checked = stmt.label_check(labels, funcname)?;
                Ok(BlockItem::Statement(checked))
            }

            BlockItem::Declaration(decl) => {
                let checked = decl.label_check(labels, funcname)?;
                Ok(BlockItem::Declaration(checked))
            }
        }
    }
}

impl LabelCheck for Declaration {
    fn label_check(self, labels: &mut HashSet<String>, _funcname: &str) -> Result<Self> {
        match self {
            Declaration::Variable(_) => Ok(self),
            Declaration::Function(func_decl) => {
                let name = func_decl.name.clone();
                let checked = func_decl.label_check(labels, &name)?;

                Ok(Declaration::Function(checked))
            }
            Declaration::Struct(_) => Ok(self),
        }
    }
}

impl LabelCheck for FunctionDeclaration {
    fn label_check(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        let checked_body = match *self.body {
            Some(body) => body.label_check(labels, funcname)?.into(),
            None => None,
        };

        Ok(FunctionDeclaration {
            name: self.name.clone(),
            ty: self.ty.clone(),
            params: self.params.clone(),
            body: checked_body.into(),
            is_global: self.is_global,
            storage_class: self.storage_class,
        })
    }
}

impl LabelCheck for Statement {
    fn label_check(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        match self {
            Statement::Compound(stmt_compound) => {
                let checked = stmt_compound.label_check(labels, funcname)?;
                Ok(Statement::Compound(checked))
            }

            Statement::If(stmt_if) => {
                let checked = stmt_if.label_check(labels, funcname)?;
                Ok(Statement::If(checked))
            }

            Statement::Break(stmt_break) => {
                let checked = stmt_break.label_check(labels, funcname)?;
                Ok(Statement::Break(checked))
            }

            Statement::Continue(stmt_continue) => {
                let checked = stmt_continue.label_check(labels, funcname)?;
                Ok(Statement::Continue(checked))
            }

            Statement::While(stmt_while) => {
                let checked = stmt_while.label_check(labels, funcname)?;
                Ok(Statement::While(checked))
            }

            Statement::DoWhile(stmt_do_while) => {
                let checked = stmt_do_while.label_check(labels, funcname)?;
                Ok(Statement::DoWhile(checked))
            }

            Statement::For(stmt_for) => {
                let checked = stmt_for.label_check(labels, funcname)?;
                Ok(Statement::For(checked))
            }

            Statement::Expression(stmt_expr) => {
                let checked = stmt_expr.label_check(labels, funcname)?;
                Ok(Statement::Expression(checked))
            }

            Statement::Return(stmt_return) => {
                let checked = stmt_return.label_check(labels, funcname)?;
                Ok(Statement::Return(checked))
            }

            Statement::Goto(stmt_goto) => {
                let checked = stmt_goto.label_check(labels, funcname)?;
                Ok(Statement::Goto(checked))
            }

            Statement::Labeled(stmt_labeled) => {
                let checked = stmt_labeled.label_check(labels, funcname)?;
                Ok(Statement::Labeled(checked))
            }

            Statement::Switch(stmt_switch) => {
                let checked = stmt_switch.label_check(labels, funcname)?;
                Ok(Statement::Switch(checked))
            }

            Statement::Case(stmt_case) => {
                let checked = stmt_case.label_check(labels, funcname)?;
                Ok(Statement::Case(checked))
            }

            Statement::Default(stmt_default) => {
                let checked = stmt_default.label_check(labels, funcname)?;
                Ok(Statement::Default(checked))
            }

            Self::Null => Ok(Statement::Null),
        }
    }
}

impl LabelCheck for ReturnStatement {
    fn label_check(self, _labels: &mut HashSet<String>, _funcname: &str) -> Result<Self> {
        Ok(self)
    }
}

impl LabelCheck for ExpressionStatement {
    fn label_check(self, _labels: &mut HashSet<String>, _funcname: &str) -> Result<Self> {
        Ok(self)
    }
}

impl LabelCheck for IfStatement {
    fn label_check(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        let checked_then = self.then_branch.label_check(labels, funcname)?;

        let checked_else = match *self.else_branch {
            Some(else_branch) => else_branch.label_check(labels, funcname)?.into(),
            None => None,
        };

        Ok(IfStatement {
            condition: self.condition.clone(),
            then_branch: checked_then.into(),
            else_branch: checked_else.into(),
        })
    }
}

impl LabelCheck for BlockStatement {
    fn label_check(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        let checked_stmts = self
            .stmts
            .into_iter()
            .map(|stmt| stmt.label_check(labels, funcname))
            .collect::<Result<Vec<_>>>()?;
        Ok(BlockStatement {
            stmts: checked_stmts,
        })
    }
}

impl LabelCheck for WhileStatement {
    fn label_check(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        let checked_body = self.body.label_check(labels, funcname)?;
        Ok(WhileStatement {
            condition: self.condition.clone(),
            body: checked_body.into(),
            label: self.label.clone(),
        })
    }
}

impl LabelCheck for DoWhileStatement {
    fn label_check(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        let checked_body = self.body.label_check(labels, funcname)?;
        Ok(DoWhileStatement {
            condition: self.condition.clone(),
            body: checked_body.into(),
            label: self.label.clone(),
        })
    }
}

impl LabelCheck for ForStatement {
    fn label_check(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        let checked_body = self.body.label_check(labels, funcname)?;
        Ok(ForStatement {
            init: self.init.clone(),
            condition: self.condition.clone(),
            post: self.post.clone(),
            body: checked_body.into(),
            label: self.label.clone(),
        })
    }
}

impl LabelCheck for BreakStatement {
    fn label_check(self, _labels: &mut HashSet<String>, _funcname: &str) -> Result<Self> {
        Ok(self)
    }
}

impl LabelCheck for ContinueStatement {
    fn label_check(self, _labels: &mut HashSet<String>, _funcname: &str) -> Result<Self> {
        Ok(self)
    }
}

impl LabelCheck for GotoStatement {
    fn label_check(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        if !labels.contains(&format!("{}.{}", self.label.clone(), funcname)) {
            bail!(
                "non existing label: {}",
                format!("{}.{}", self.label.clone(), funcname)
            );
        }
        Ok(self)
    }
}

impl LabelCheck for LabeledStatement {
    fn label_check(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        let checked_body = self.body.label_check(labels, funcname)?;

        Ok(LabeledStatement {
            label: self.label.clone(),
            body: checked_body.into(),
        })
    }
}

impl LabelCheck for SwitchStatement {
    fn label_check(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        let checked_body = self.body.label_check(labels, funcname)?;
        Ok(SwitchStatement {
            condition: self.condition.clone(),
            body: checked_body.into(),
            label: self.label.clone(),
            cases: self.cases.clone(),
        })
    }
}

impl LabelCheck for CaseStatement {
    fn label_check(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        let checked_body = self.body.label_check(labels, funcname)?;
        Ok(CaseStatement {
            value: self.value.clone(),
            body: checked_body.into(),
            label: self.label.clone(),
        })
    }
}

impl LabelCheck for DefaultStatement {
    fn label_check(self, labels: &mut HashSet<String>, funcname: &str) -> Result<Self> {
        let checked_body = self.body.label_check(labels, funcname)?;
        Ok(DefaultStatement {
            body: checked_body.into(),
            label: self.label.clone(),
        })
    }
}
