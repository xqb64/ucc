use crate::parser::ast::{
    BlockItem, BlockStatement, BreakStatement, ContinueStatement, Declaration, DoWhileStatement,
    ExpressionStatement, ForStatement, FunctionDeclaration, GotoStatement, IfStatement,
    LabeledStatement, Program, ReturnStatement, Statement, WhileStatement,
};
use anyhow::{bail, Result};
use std::collections::HashSet;

pub trait LabelCollect {
    fn collect_labels(&mut self, labels: &mut HashSet<String>, funcname: &str)
        -> Result<&mut Self>;
}

impl LabelCollect for Program {
    fn collect_labels(
        &mut self,
        labels: &mut HashSet<String>,
        funcname: &str,
    ) -> Result<&mut Self> {
        for block_item in self.block_items.iter_mut() {
            block_item.collect_labels(labels, funcname)?;
        }
        Ok(self)
    }
}

impl LabelCollect for BlockItem {
    fn collect_labels(
        &mut self,
        labels: &mut HashSet<String>,
        funcname: &str,
    ) -> Result<&mut Self> {
        match self {
            BlockItem::Statement(s) => {
                s.collect_labels(labels, funcname)?;
                Ok(self)
            }

            BlockItem::Declaration(decl) => {
                decl.collect_labels(labels, funcname)?;
                Ok(self)
            }
        }
    }
}

impl LabelCollect for Declaration {
    fn collect_labels(
        &mut self,
        labels: &mut HashSet<String>,
        _funcname: &str,
    ) -> Result<&mut Self> {
        match self {
            Declaration::Variable(_) => Ok(self),
            Declaration::Function(f) => {
                f.collect_labels(labels, &f.name.clone())?;
                Ok(self)
            }
            Declaration::Struct(_) => Ok(self),
        }
    }
}

impl LabelCollect for FunctionDeclaration {
    fn collect_labels(
        &mut self,
        labels: &mut HashSet<String>,
        funcname: &str,
    ) -> Result<&mut Self> {
        if let Some(ref mut body) = *self.body {
            body.collect_labels(labels, funcname)?;
        }
        Ok(self)
    }
}

impl LabelCollect for Statement {
    fn collect_labels(
        &mut self,
        labels: &mut HashSet<String>,
        funcname: &str,
    ) -> Result<&mut Self> {
        match self {
            Statement::Compound(b) => {
                b.collect_labels(labels, funcname)?;
            }

            Statement::If(i) => {
                i.collect_labels(labels, funcname)?;
            }

            Statement::Break(b) => {
                b.collect_labels(labels, funcname)?;
            }

            Statement::Continue(c) => {
                c.collect_labels(labels, funcname)?;
            }

            Statement::While(w) => {
                w.collect_labels(labels, funcname)?;
            }

            Statement::DoWhile(d) => {
                d.collect_labels(labels, funcname)?;
            }

            Statement::For(f) => {
                f.collect_labels(labels, funcname)?;
            }

            Statement::Expression(e) => {
                e.collect_labels(labels, funcname)?;
            }

            Statement::Return(r) => {
                r.collect_labels(labels, funcname)?;
            }

            Statement::Goto(g) => {
                g.collect_labels(labels, funcname)?;
            }

            Statement::Labeled(l) => {
                l.collect_labels(labels, funcname)?;
            }

            Self::Null => {}
        }

        Ok(self)
    }
}

impl LabelCollect for BlockStatement {
    fn collect_labels(
        &mut self,
        labels: &mut HashSet<String>,
        funcname: &str,
    ) -> Result<&mut Self> {
        for stmt in self.stmts.iter_mut() {
            stmt.collect_labels(labels, funcname)?;
        }
        Ok(self)
    }
}

impl LabelCollect for IfStatement {
    fn collect_labels(
        &mut self,
        labels: &mut HashSet<String>,
        funcname: &str,
    ) -> Result<&mut Self> {
        self.then_branch.collect_labels(labels, funcname)?;

        if let Some(ref mut else_branch) = *self.else_branch {
            else_branch.collect_labels(labels, funcname)?;
        }

        Ok(self)
    }
}

impl LabelCollect for BreakStatement {
    fn collect_labels(
        &mut self,
        _labels: &mut HashSet<String>,
        _funcname: &str,
    ) -> Result<&mut Self> {
        Ok(self)
    }
}

impl LabelCollect for ContinueStatement {
    fn collect_labels(
        &mut self,
        _labels: &mut HashSet<String>,
        _funcname: &str,
    ) -> Result<&mut Self> {
        Ok(self)
    }
}

impl LabelCollect for WhileStatement {
    fn collect_labels(
        &mut self,
        labels: &mut HashSet<String>,
        funcname: &str,
    ) -> Result<&mut Self> {
        self.body.collect_labels(labels, funcname)?;
        Ok(self)
    }
}

impl LabelCollect for DoWhileStatement {
    fn collect_labels(
        &mut self,
        labels: &mut HashSet<String>,
        funcname: &str,
    ) -> Result<&mut Self> {
        self.body.collect_labels(labels, funcname)?;
        Ok(self)
    }
}

impl LabelCollect for ForStatement {
    fn collect_labels(
        &mut self,
        labels: &mut HashSet<String>,
        funcname: &str,
    ) -> Result<&mut Self> {
        self.body.collect_labels(labels, funcname)?;
        Ok(self)
    }
}

impl LabelCollect for ReturnStatement {
    fn collect_labels(
        &mut self,
        _labels: &mut HashSet<String>,
        _funcname: &str,
    ) -> Result<&mut Self> {
        Ok(self)
    }
}

impl LabelCollect for ExpressionStatement {
    fn collect_labels(
        &mut self,
        _labels: &mut HashSet<String>,
        _funcname: &str,
    ) -> Result<&mut Self> {
        Ok(self)
    }
}

impl LabelCollect for GotoStatement {
    fn collect_labels(
        &mut self,
        _labels: &mut HashSet<String>,
        _funcname: &str,
    ) -> Result<&mut Self> {
        Ok(self)
    }
}

impl LabelCollect for LabeledStatement {
    fn collect_labels(
        &mut self,
        labels: &mut HashSet<String>,
        funcname: &str,
    ) -> Result<&mut Self> {
        if !labels.insert(format!("{}.{}", self.label.clone(), funcname)) {
            bail!("duplicate label: {}", self.label.clone());
        }

        self.body.collect_labels(labels, funcname)?;

        Ok(self)
    }
}

pub trait LabelCheck {
    fn label_check(&mut self, labels: &mut HashSet<String>, funcname: &str) -> Result<&mut Self>;
}

impl LabelCheck for Program {
    fn label_check(&mut self, labels: &mut HashSet<String>, funcname: &str) -> Result<&mut Self> {
        for block_item in self.block_items.iter_mut() {
            block_item.collect_labels(labels, funcname)?;
            block_item.label_check(labels, funcname)?;
        }
        Ok(self)
    }
}

impl LabelCheck for BlockItem {
    fn label_check(&mut self, labels: &mut HashSet<String>, funcname: &str) -> Result<&mut Self> {
        match self {
            BlockItem::Statement(s) => {
                s.label_check(labels, funcname)?;
                Ok(self)
            }

            BlockItem::Declaration(decl) => {
                decl.label_check(labels, funcname)?;
                Ok(self)
            }
        }
    }
}

impl LabelCheck for Declaration {
    fn label_check(&mut self, _labels: &mut HashSet<String>, _funcname: &str) -> Result<&mut Self> {
        match self {
            Declaration::Variable(_) => Ok(self),
            Declaration::Function(f) => {
                let mut l = HashSet::new();

                f.collect_labels(&mut l, &f.name.clone())?;
                f.label_check(&mut l, &f.name.clone())?;

                Ok(self)
            }
            Declaration::Struct(_) => Ok(self),
        }
    }
}

impl LabelCheck for FunctionDeclaration {
    fn label_check(&mut self, labels: &mut HashSet<String>, funcname: &str) -> Result<&mut Self> {
        if let Some(ref mut body) = *self.body {
            body.label_check(labels, funcname)?;
        }
        Ok(self)
    }
}

impl LabelCheck for Statement {
    fn label_check(&mut self, labels: &mut HashSet<String>, funcname: &str) -> Result<&mut Self> {
        match self {
            Statement::Compound(b) => {
                b.label_check(labels, funcname)?;
            }

            Statement::If(i) => {
                i.label_check(labels, funcname)?;
            }

            Statement::Break(b) => {
                b.label_check(labels, funcname)?;
            }

            Statement::Continue(c) => {
                c.label_check(labels, funcname)?;
            }

            Statement::While(w) => {
                w.label_check(labels, funcname)?;
            }

            Statement::DoWhile(d) => {
                d.label_check(labels, funcname)?;
            }

            Statement::For(f) => {
                f.label_check(labels, funcname)?;
            }

            Statement::Expression(e) => {
                e.label_check(labels, funcname)?;
            }

            Statement::Return(r) => {
                r.label_check(labels, funcname)?;
            }

            Statement::Goto(g) => {
                g.label_check(labels, funcname)?;
            }

            Statement::Labeled(l) => {
                l.label_check(labels, funcname)?;
            }

            Self::Null => {}
        }

        Ok(self)
    }
}

impl LabelCheck for BlockStatement {
    fn label_check(&mut self, labels: &mut HashSet<String>, funcname: &str) -> Result<&mut Self> {
        for stmt in self.stmts.iter_mut() {
            stmt.label_check(labels, funcname)?;
        }
        Ok(self)
    }
}

impl LabelCheck for IfStatement {
    fn label_check(&mut self, labels: &mut HashSet<String>, funcname: &str) -> Result<&mut Self> {
        self.then_branch.label_check(labels, funcname)?;

        if let Some(ref mut else_branch) = *self.else_branch {
            else_branch.label_check(labels, funcname)?;
        }

        Ok(self)
    }
}

impl LabelCheck for BreakStatement {
    fn label_check(&mut self, _labels: &mut HashSet<String>, _funcname: &str) -> Result<&mut Self> {
        Ok(self)
    }
}

impl LabelCheck for ContinueStatement {
    fn label_check(&mut self, _labels: &mut HashSet<String>, _funcname: &str) -> Result<&mut Self> {
        Ok(self)
    }
}

impl LabelCheck for WhileStatement {
    fn label_check(&mut self, labels: &mut HashSet<String>, funcname: &str) -> Result<&mut Self> {
        self.body.label_check(labels, funcname)?;
        Ok(self)
    }
}

impl LabelCheck for DoWhileStatement {
    fn label_check(&mut self, labels: &mut HashSet<String>, funcname: &str) -> Result<&mut Self> {
        self.body.label_check(labels, funcname)?;
        Ok(self)
    }
}

impl LabelCheck for ForStatement {
    fn label_check(&mut self, labels: &mut HashSet<String>, funcname: &str) -> Result<&mut Self> {
        self.body.label_check(labels, funcname)?;
        Ok(self)
    }
}

impl LabelCheck for ReturnStatement {
    fn label_check(&mut self, _labels: &mut HashSet<String>, _funcname: &str) -> Result<&mut Self> {
        Ok(self)
    }
}

impl LabelCheck for ExpressionStatement {
    fn label_check(&mut self, _labels: &mut HashSet<String>, _funcname: &str) -> Result<&mut Self> {
        Ok(self)
    }
}

impl LabelCheck for GotoStatement {
    fn label_check(&mut self, labels: &mut HashSet<String>, funcname: &str) -> Result<&mut Self> {
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
    fn label_check(&mut self, labels: &mut HashSet<String>, funcname: &str) -> Result<&mut Self> {
        self.body.label_check(labels, funcname)?;

        Ok(self)
    }
}
