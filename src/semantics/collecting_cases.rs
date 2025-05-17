use crate::semantics::typechecker::convert_to;
use crate::{
    ir::gen::expr2const,
    parser::ast::{
        BlockItem, BlockStatement, BreakStatement, CaseStatement, ConstantExpression,
        ContinueStatement, Declaration, DefaultStatement, DoWhileStatement, Expression,
        ExpressionStatement, ForStatement, FunctionDeclaration, GotoStatement, IfStatement,
        LabeledStatement, Program, ReturnStatement, Statement, SwitchStatement, Type,
        WhileStatement,
    },
};
use anyhow::{bail, Result};

use super::typechecker::{get_type, is_integer_type};

pub trait SwitchCaseCollect {
    fn collect_switch_cases(self, cases: &mut Vec<Statement>, control: &Type) -> Result<Self>
    where
        Self: Sized;
}

impl SwitchCaseCollect for Program {
    fn collect_switch_cases(self, cases: &mut Vec<Statement>, control: &Type) -> Result<Self> {
        let collected_block_items = self
            .block_items
            .into_iter()
            .map(|block_item| block_item.collect_switch_cases(cases, control))
            .collect::<Result<Vec<_>>>()?;

        Ok(Program {
            block_items: collected_block_items,
        })
    }
}

impl SwitchCaseCollect for BlockItem {
    fn collect_switch_cases(self, cases: &mut Vec<Statement>, control: &Type) -> Result<Self> {
        match self {
            BlockItem::Statement(stmt) => {
                let collected = stmt.collect_switch_cases(cases, control)?;
                Ok(BlockItem::Statement(collected))
            }

            BlockItem::Declaration(decl) => {
                let collected = decl.collect_switch_cases(cases, control)?;
                Ok(BlockItem::Declaration(collected))
            }
        }
    }
}

impl SwitchCaseCollect for Declaration {
    fn collect_switch_cases(self, cases: &mut Vec<Statement>, control: &Type) -> Result<Self> {
        match self {
            Declaration::Variable(_) => Ok(self),
            Declaration::Function(func_decl) => {
                let collected = func_decl.collect_switch_cases(cases, control)?;
                Ok(Declaration::Function(collected))
            }
            Declaration::Struct(_) => Ok(self),
        }
    }
}

impl SwitchCaseCollect for FunctionDeclaration {
    fn collect_switch_cases(self, cases: &mut Vec<Statement>, control: &Type) -> Result<Self> {
        let collected_body = match *self.body {
            Some(body) => body.collect_switch_cases(cases, control)?.into(),
            None => None,
        };
        Ok(FunctionDeclaration {
            name: self.name.clone(),
            ty: self.ty.clone(),
            params: self.params.clone(),
            body: collected_body.into(),
            is_global: self.is_global,
            storage_class: self.storage_class.clone(),
        })
    }
}

impl SwitchCaseCollect for Statement {
    fn collect_switch_cases(self, cases: &mut Vec<Statement>, control: &Type) -> Result<Self> {
        match self {
            Statement::Compound(stmt_compound) => {
                let collected = stmt_compound.collect_switch_cases(cases, control)?;
                Ok(Statement::Compound(collected))
            }

            Statement::If(stmt_if) => {
                let collected = stmt_if.collect_switch_cases(cases, control)?;
                Ok(Statement::If(collected))
            }

            Statement::Break(stmt_break) => {
                let collected = stmt_break.collect_switch_cases(cases, control)?;
                Ok(Statement::Break(collected))
            }

            Statement::Continue(stmt_continue) => {
                let collected = stmt_continue.collect_switch_cases(cases, control)?;
                Ok(Statement::Continue(collected))
            }

            Statement::While(stmt_while) => {
                let collected = stmt_while.collect_switch_cases(cases, control)?;
                Ok(Statement::While(collected))
            }

            Statement::DoWhile(stmt_do_while) => {
                let collected = stmt_do_while.collect_switch_cases(cases, control)?;
                Ok(Statement::DoWhile(collected))
            }

            Statement::For(stmt_for) => {
                let collected = stmt_for.collect_switch_cases(cases, control)?;
                Ok(Statement::For(collected))
            }

            Statement::Expression(stmt_expr) => {
                let collected = stmt_expr.collect_switch_cases(cases, control)?;
                Ok(Statement::Expression(collected))
            }

            Statement::Return(stmt_return) => {
                let collected = stmt_return.collect_switch_cases(cases, control)?;
                Ok(Statement::Return(collected))
            }

            Statement::Goto(stmt_goto) => {
                let collected = stmt_goto.collect_switch_cases(cases, control)?;
                Ok(Statement::Goto(collected))
            }

            Statement::Labeled(stmt_labeled) => {
                let collected = stmt_labeled.collect_switch_cases(cases, control)?;
                Ok(Statement::Labeled(collected))
            }

            Statement::Switch(stmt_switch) => {
                let collected = stmt_switch.collect_switch_cases(cases, control)?;
                Ok(Statement::Switch(collected))
            }

            Statement::Case(stmt_case) => {
                let collected = stmt_case.collect_switch_cases(cases, control)?;
                Ok(Statement::Case(collected))
            }

            Statement::Default(stmt_default) => {
                let collected = stmt_default.collect_switch_cases(cases, control)?;
                Ok(Statement::Default(collected))
            }

            Self::Null => Ok(Statement::Null),
        }
    }
}

impl SwitchCaseCollect for BlockStatement {
    fn collect_switch_cases(self, cases: &mut Vec<Statement>, control: &Type) -> Result<Self> {
        let collected_stmts = self
            .stmts
            .into_iter()
            .map(|stmt| stmt.collect_switch_cases(cases, control))
            .collect::<Result<Vec<_>>>()?;
        Ok(BlockStatement {
            stmts: collected_stmts,
        })
    }
}

impl SwitchCaseCollect for IfStatement {
    fn collect_switch_cases(self, cases: &mut Vec<Statement>, control: &Type) -> Result<Self> {
        let collected_then = self.then_branch.collect_switch_cases(cases, control)?;
        let collected_else = match *self.else_branch {
            Some(else_branch) => else_branch.collect_switch_cases(cases, control)?.into(),
            None => None,
        };

        Ok(IfStatement {
            condition: self.condition.clone(),
            then_branch: collected_then.into(),
            else_branch: collected_else.into(),
        })
    }
}

impl SwitchCaseCollect for BreakStatement {
    fn collect_switch_cases(self, _cases: &mut Vec<Statement>, _control: &Type) -> Result<Self> {
        Ok(self)
    }
}

impl SwitchCaseCollect for ContinueStatement {
    fn collect_switch_cases(self, _cases: &mut Vec<Statement>, _control: &Type) -> Result<Self> {
        Ok(self)
    }
}

impl SwitchCaseCollect for WhileStatement {
    fn collect_switch_cases(self, cases: &mut Vec<Statement>, control: &Type) -> Result<Self> {
        let collected_body = self.body.collect_switch_cases(cases, control)?;
        Ok(WhileStatement {
            condition: self.condition.clone(),
            body: collected_body.into(),
            label: self.label.clone(),
        })
    }
}

impl SwitchCaseCollect for DoWhileStatement {
    fn collect_switch_cases(self, cases: &mut Vec<Statement>, control: &Type) -> Result<Self> {
        let collected_body = self.body.collect_switch_cases(cases, control)?;
        Ok(DoWhileStatement {
            condition: self.condition.clone(),
            body: collected_body.into(),
            label: self.label.clone(),
        })
    }
}

impl SwitchCaseCollect for ForStatement {
    fn collect_switch_cases(self, cases: &mut Vec<Statement>, control: &Type) -> Result<Self> {
        let collected_body = self.body.collect_switch_cases(cases, control)?;
        Ok(ForStatement {
            init: self.init.clone(),
            condition: self.condition.clone(),
            post: self.post.clone(),
            body: collected_body.into(),
            label: self.label.clone(),
        })
    }
}

impl SwitchCaseCollect for ReturnStatement {
    fn collect_switch_cases(self, _cases: &mut Vec<Statement>, _control: &Type) -> Result<Self> {
        Ok(self)
    }
}

impl SwitchCaseCollect for ExpressionStatement {
    fn collect_switch_cases(self, _cases: &mut Vec<Statement>, _control: &Type) -> Result<Self> {
        Ok(self)
    }
}

impl SwitchCaseCollect for GotoStatement {
    fn collect_switch_cases(self, _cases: &mut Vec<Statement>, _control: &Type) -> Result<Self> {
        Ok(self)
    }
}

impl SwitchCaseCollect for LabeledStatement {
    fn collect_switch_cases(self, cases: &mut Vec<Statement>, control: &Type) -> Result<Self> {
        let collected_body = self.body.collect_switch_cases(cases, control)?;

        Ok(LabeledStatement {
            label: self.label.clone(),
            body: collected_body.into(),
        })
    }
}

impl SwitchCaseCollect for SwitchStatement {
    fn collect_switch_cases(self, _cases: &mut Vec<Statement>, _control: &Type) -> Result<Self> {
        let mut new_cases = vec![];

        let collected_body = self
            .body
            .collect_switch_cases(&mut new_cases, get_type(&self.condition))?;

        Ok(SwitchStatement {
            condition: self.condition.clone(),
            body: collected_body.into(),
            label: self.label.clone(),
            cases: new_cases,
        })
    }
}

use crate::semantics::typechecker::typecheck_and_convert;

impl SwitchCaseCollect for CaseStatement {
    fn collect_switch_cases(self, cases: &mut Vec<Statement>, control: &Type) -> Result<Self> {
        if !is_integer_type(get_type(&self.value)) {
            bail!("switch condition not a constant expression");
        }

        if !self.label.contains("Switch") {
            bail!("case outside the switch statement");
        }

        let mut new_val = None;
        if get_type(&self.value) != control {
            let typechecked_value = typecheck_and_convert(&self.value)?;
            if is_integer_type(get_type(&typechecked_value))
                && get_type(&typechecked_value) != control
            {
                new_val = Some(Expression::Constant(ConstantExpression {
                    value: expr2const(&convert_to(&self.value, control)),
                    ty: control.clone(),
                }));
            }
        }

        let this_value = if new_val.is_some() {
            new_val.as_ref().unwrap()
        } else {
            &self.value
        };
        if cases.iter().any(|stmt| {
            if let Statement::Case(case_stmt) = stmt {
                case_stmt.value == *this_value
            } else {
                false
            }
        }) {
            bail!("duplicate case value");
        }

        cases.push(Statement::Case(CaseStatement {
            value: if new_val.is_some() {
                new_val.clone().unwrap()
            } else {
                self.value.clone()
            },
            body: self.body.clone(),
            label: self.label.clone(),
        }));

        let collected_body = self.body.collect_switch_cases(cases, control)?;

        Ok(CaseStatement {
            value: if new_val.is_some() {
                new_val.unwrap()
            } else {
                self.value.clone()
            },
            body: collected_body.into(),
            label: self.label.clone(),
        })
    }
}

impl SwitchCaseCollect for DefaultStatement {
    fn collect_switch_cases(self, cases: &mut Vec<Statement>, control: &Type) -> Result<Self> {
        if cases
            .iter()
            .any(|stmt| matches!(stmt, Statement::Default(_)))
        {
            bail!("multiple defaults in a switch statement");
        }

        if !self.label.contains("Switch") {
            bail!("default outside the switch statement");
        }

        cases.push(Statement::Default(self.clone()));

        let collected_body = self.body.collect_switch_cases(cases, control)?;

        Ok(DefaultStatement {
            body: collected_body.into(),
            label: self.label.clone(),
        })
    }
}
