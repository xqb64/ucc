use crate::{ir::gen::expr2const, parser::ast::{
    BlockItem, BlockStatement, BreakStatement, CaseStatement, ConstantExpression, ContinueStatement, Declaration, DefaultStatement, DoWhileStatement, Expression, ExpressionStatement, ForStatement, FunctionDeclaration, GotoStatement, IfStatement, LabeledStatement, Program, ReturnStatement, Statement, SwitchStatement, Type, WhileStatement
}};
use anyhow::{bail, Result};
use crate::semantics::typechecker::convert_to;

use super::typechecker::{get_type, is_integer_type};

pub trait SwitchCaseCollect {
    fn collect_switch_cases(&mut self, cases: &mut Vec<Statement>, control: &Type) -> Result<&mut Self>;
}

impl SwitchCaseCollect for Program {
    fn collect_switch_cases(&mut self, cases: &mut Vec<Statement>, control: &Type) -> Result<&mut Self> {
        for block_item in self.block_items.iter_mut() {
            block_item.collect_switch_cases(cases, control)?;
        }
        Ok(self)
    }
}

impl SwitchCaseCollect for BlockItem {
    fn collect_switch_cases(&mut self, cases: &mut Vec<Statement>, control: &Type) -> Result<&mut Self> {
        match self {
            BlockItem::Statement(s) => {
                s.collect_switch_cases(cases, control)?;
                Ok(self)
            }

            BlockItem::Declaration(decl) => {
                decl.collect_switch_cases(cases, control)?;
                Ok(self)
            }
        }
    }
}

impl SwitchCaseCollect for Declaration {
    fn collect_switch_cases(&mut self, cases: &mut Vec<Statement>, control: &Type) -> Result<&mut Self> {
        match self {
            Declaration::Variable(_) => Ok(self),
            Declaration::Function(f) => {
                f.collect_switch_cases(cases, control)?;
                Ok(self)
            }
            Declaration::Struct(_) => Ok(self),
        }
    }
}

impl SwitchCaseCollect for FunctionDeclaration {
    fn collect_switch_cases(&mut self, cases: &mut Vec<Statement>, control: &Type) -> Result<&mut Self> {
        if let Some(ref mut body) = *self.body {
            body.collect_switch_cases(cases, control)?;
        }
        Ok(self)
    }
}

impl SwitchCaseCollect for Statement {
    fn collect_switch_cases(&mut self, cases: &mut Vec<Statement>, control: &Type) -> Result<&mut Self> {
        match self {
            Statement::Compound(b) => {
                b.collect_switch_cases(cases, control)?;
            }

            Statement::If(i) => {
                i.collect_switch_cases(cases, control)?;
            }

            Statement::Break(b) => {
                b.collect_switch_cases(cases, control)?;
            }

            Statement::Continue(c) => {
                c.collect_switch_cases(cases, control)?;
            }

            Statement::While(w) => {
                w.collect_switch_cases(cases, control)?;
            }

            Statement::DoWhile(d) => {
                d.collect_switch_cases(cases, control)?;
            }

            Statement::For(f) => {
                f.collect_switch_cases(cases, control)?;
            }

            Statement::Expression(e) => {
                e.collect_switch_cases(cases, control)?;
            }

            Statement::Return(r) => {
                r.collect_switch_cases(cases, control)?;
            }

            Statement::Goto(g) => {
                g.collect_switch_cases(cases, control)?;
            }

            Statement::Labeled(l) => {
                l.collect_switch_cases(cases, control)?;
            }

            Statement::Switch(s) => {
                s.collect_switch_cases(cases, control)?;
            }

            Statement::Case(c) => {
                c.collect_switch_cases(cases, control)?;
            }

            Statement::Default(d) => {
                d.collect_switch_cases(cases, control)?;
            }

            Self::Null => {}
        }

        Ok(self)
    }
}

impl SwitchCaseCollect for BlockStatement {
    fn collect_switch_cases(&mut self, cases: &mut Vec<Statement>, control: &Type) -> Result<&mut Self> {
        for stmt in self.stmts.iter_mut() {
            stmt.collect_switch_cases(cases, control)?;
        }
        Ok(self)
    }
}

impl SwitchCaseCollect for IfStatement {
    fn collect_switch_cases(&mut self, cases: &mut Vec<Statement>, control: &Type) -> Result<&mut Self> {
        self.then_branch.collect_switch_cases(cases, control)?;

        if let Some(ref mut else_branch) = *self.else_branch {
            else_branch.collect_switch_cases(cases, control)?;
        }

        Ok(self)
    }
}

impl SwitchCaseCollect for BreakStatement {
    fn collect_switch_cases(&mut self, _cases: &mut Vec<Statement>, control: &Type) -> Result<&mut Self> {
        Ok(self)
    }
}

impl SwitchCaseCollect for ContinueStatement {
    fn collect_switch_cases(&mut self, _cases: &mut Vec<Statement>, control: &Type) -> Result<&mut Self> {
        Ok(self)
    }
}

impl SwitchCaseCollect for WhileStatement {
    fn collect_switch_cases(&mut self, cases: &mut Vec<Statement>, control: &Type) -> Result<&mut Self> {
        self.body.collect_switch_cases(cases, control)?;
        Ok(self)
    }
}

impl SwitchCaseCollect for DoWhileStatement {
    fn collect_switch_cases(&mut self, cases: &mut Vec<Statement>, control: &Type) -> Result<&mut Self> {
        self.body.collect_switch_cases(cases, control)?;
        Ok(self)
    }
}

impl SwitchCaseCollect for ForStatement {
    fn collect_switch_cases(&mut self, cases: &mut Vec<Statement>, control: &Type) -> Result<&mut Self> {
        self.body.collect_switch_cases(cases, control)?;
        Ok(self)
    }
}

impl SwitchCaseCollect for ReturnStatement {
    fn collect_switch_cases(&mut self, _cases: &mut Vec<Statement>, control: &Type) -> Result<&mut Self> {
        Ok(self)
    }
}

impl SwitchCaseCollect for ExpressionStatement {
    fn collect_switch_cases(&mut self, _cases: &mut Vec<Statement>, control: &Type) -> Result<&mut Self> {
        Ok(self)
    }
}

impl SwitchCaseCollect for GotoStatement {
    fn collect_switch_cases(&mut self, _cases: &mut Vec<Statement>, control: &Type) -> Result<&mut Self> {
        Ok(self)
    }
}

impl SwitchCaseCollect for LabeledStatement {
    fn collect_switch_cases(&mut self, cases: &mut Vec<Statement>, control: &Type) -> Result<&mut Self> {
        self.body.collect_switch_cases(cases, control)?;

        Ok(self)
    }
}

impl SwitchCaseCollect for SwitchStatement {
    fn collect_switch_cases(&mut self, cases: &mut Vec<Statement>, control: &Type) -> Result<&mut Self> {
        let mut new_cases = vec![];

        self.body.collect_switch_cases(&mut new_cases, get_type(&self.condition))?;

        self.cases = new_cases;

        Ok(self)
    }
}

use crate::semantics::typechecker::typecheck_and_convert;

impl SwitchCaseCollect for CaseStatement {

    fn collect_switch_cases(&mut self, cases: &mut Vec<Statement>, control: &Type) -> Result<&mut Self> {
        if !is_integer_type(get_type(&self.value)) {
            bail!("switch condition not a constant expression");
        }

        if !self.label.contains("Switch") {
            bail!("case outside the switch statement");
        }
        
        if get_type(&self.value) != control {
            let typechecked_value = typecheck_and_convert(&self.value)?;
            if is_integer_type(get_type(&typechecked_value)) && get_type(&typechecked_value) != control {
                self.value = Expression::Constant(ConstantExpression { value: expr2const(&convert_to(&self.value, control)), _type: control.clone() });
            }
        }

        let this_value = &self.value;
        println!("this_value is: {:?}", this_value);
        println!("cases is: {:?}", cases);
        if {
            cases.iter().any(|stmt| {
                if let Statement::Case(case_stmt) = stmt {
                    case_stmt.value == *this_value
                } else {
                    false
                }
            })
        } {
            bail!("duplicate case value");
        }

        cases.push(Statement::Case(self.clone()));

        self.body.collect_switch_cases(cases, control)?;

        Ok(self)
    }
}

impl SwitchCaseCollect for DefaultStatement {
    fn collect_switch_cases(&mut self, cases: &mut Vec<Statement>, control: &Type) -> Result<&mut Self> {
        if cases.iter().any(|stmt| matches!(stmt, Statement::Default(_))) {
            bail!("multiple defaults in a switch statement");
        }
  
        if !self.label.contains("Switch") {
            bail!("default outside the switch statement");
        }

       cases.push(Statement::Default(self.clone()));
        self.body.collect_switch_cases(cases, control)?;

        Ok(self)
    }
}
