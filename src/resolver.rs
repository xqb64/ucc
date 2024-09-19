use anyhow::{bail, Result};
use std::collections::HashMap;

use crate::{
    ir::make_temporary,
    parser::{
        AddrOfExpression, AssignExpression, BinaryExpression, BlockItem, BlockStatement,
        BreakStatement, CallExpression, CastExpression, ConditionalExpression, ContinueStatement,
        Declaration, DerefExpression, DoWhileStatement, Expression, ExpressionStatement, ForInit,
        ForStatement, FunctionDeclaration, IfStatement, Initializer, LiteralExpression,
        ProgramStatement, ReturnStatement, Statement, StorageClass, SubscriptExpression, Type,
        UnaryExpression, VariableDeclaration, VariableExpression, WhileStatement,
    },
};

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    name: String,
    from_current_scope: bool,
    has_linkage: bool,
}

pub trait Resolve {
    fn resolve(&mut self, variable_map: &mut HashMap<String, Variable>) -> Result<BlockItem>;
}

impl Resolve for Declaration {
    fn resolve(&mut self, variable_map: &mut HashMap<String, Variable>) -> Result<BlockItem> {
        match self {
            Declaration::Variable(var_decl) => var_decl.resolve(variable_map),
            Declaration::Function(func_decl) => func_decl.resolve(variable_map),
        }
    }
}

fn optionally_resolve_init(
    init: &Option<Initializer>,
    variable_map: &mut HashMap<String, Variable>,
) -> Result<Option<Initializer>> {
    if init.is_some() {
        let resolved_init = resolve_init(init.as_ref().unwrap(), variable_map)?;
        Ok(Some(resolved_init))
    } else {
        Ok(None)
    }
}

impl Resolve for VariableDeclaration {
    fn resolve(&mut self, variable_map: &mut HashMap<String, Variable>) -> Result<BlockItem> {
        match self.is_global {
            true => {
                variable_map.insert(
                    self.name.clone(),
                    Variable {
                        from_current_scope: true,
                        name: self.name.clone(),
                        has_linkage: false,
                    },
                );

                Ok(BlockItem::Declaration(Declaration::Variable(
                    VariableDeclaration {
                        name: self.name.clone(),
                        init: optionally_resolve_init(&self.init, variable_map)?,
                        storage_class: self.storage_class,
                        is_global: self.is_global,
                        _type: self._type.clone(),
                    },
                )))
            }
            false => {
                if variable_map.contains_key(&self.name) {
                    let prev_entry = variable_map.get(&self.name).unwrap();
                    if prev_entry.from_current_scope
                        && !(prev_entry.has_linkage
                            && self
                                .storage_class
                                .is_some_and(|sc| sc == StorageClass::Extern))
                    {
                        bail!("conflicting local declarations: {}", self.name);
                    }
                }

                if self
                    .storage_class
                    .is_some_and(|sc| sc == StorageClass::Extern)
                {
                    variable_map.insert(
                        self.name.clone(),
                        Variable {
                            from_current_scope: true,
                            name: self.name.clone(),
                            has_linkage: true,
                        },
                    );

                    Ok(BlockItem::Declaration(Declaration::Variable(
                        VariableDeclaration {
                            name: self.name.clone(),
                            init: optionally_resolve_init(&self.init, variable_map)?,
                            storage_class: self.storage_class,
                            is_global: self.is_global,
                            _type: self._type.clone(),
                        },
                    )))
                } else {
                    let unique_name = format!("var.{}.{}", self.name, make_temporary());

                    variable_map.insert(
                        self.name.clone(),
                        Variable {
                            from_current_scope: true,
                            name: unique_name.clone(),
                            has_linkage: false,
                        },
                    );

                    Ok(BlockItem::Declaration(Declaration::Variable(
                        VariableDeclaration {
                            name: unique_name,
                            init: optionally_resolve_init(&self.init, variable_map)?,
                            storage_class: self.storage_class,
                            is_global: self.is_global,
                            _type: self._type.clone(),
                        },
                    )))
                }
            }
        }
    }
}

fn resolve_init(
    init: &Initializer,
    variable_map: &mut HashMap<String, Variable>,
) -> Result<Initializer> {
    match init {
        Initializer::Single(name, single_init) => {
            let resolved_expr = resolve_exp(single_init, variable_map)?;
            Ok(Initializer::Single(name.clone(), resolved_expr))
        }
        Initializer::Compound(name, _type, compound_init) => {
            let mut resolved_inits = vec![];
            for init in compound_init {
                resolved_inits.push(resolve_init(init, variable_map)?);
            }
            Ok(Initializer::Compound(
                name.clone(),
                _type.clone(),
                resolved_inits,
            ))
        }
    }
}

impl Resolve for FunctionDeclaration {
    fn resolve(&mut self, variable_map: &mut HashMap<String, Variable>) -> Result<BlockItem> {
        if self.body.is_some() && !self.is_global {
            bail!("function definition in non-global scope");
        }

        if self
            .storage_class
            .is_some_and(|sc| sc == StorageClass::Static)
            && !self.is_global
        {
            bail!("storage class specifier in non-global scope");
        }

        // FIXME: stupid hack to prevent redeclaration of function parameters
        for param in &self.params {
            if self.body.is_some() {
                if let BlockItem::Statement(Statement::Compound(block)) = self.body.clone().unwrap()
                {
                    for stmt in &block.stmts {
                        if let BlockItem::Declaration(Declaration::Variable(var_decl)) = stmt {
                            if var_decl.name == *param {
                                bail!("parameter name cannot be the same as a variable name in the function body");
                            }
                        }
                    }
                }
            }
        }

        if variable_map.contains_key(&self.name) {
            let prev_entry = variable_map.get(&self.name).unwrap();
            if prev_entry.from_current_scope && !prev_entry.has_linkage {
                bail!("redeclaration of function: {}", self.name);
            }
        }

        variable_map.insert(
            self.name.clone(),
            Variable {
                from_current_scope: true,
                name: self.name.clone(),
                has_linkage: true,
            },
        );

        let mut inner_map = copy_variable_map(variable_map);
        let mut new_params = vec![];

        for param in &self.params {
            new_params.push(resolve_param(param, &mut inner_map)?);
        }

        let mut new_body = None;

        if self.body.is_some() {
            new_body = Some(self.body.clone().unwrap().resolve(&mut inner_map)?);
        }

        Ok(BlockItem::Declaration(Declaration::Function(
            FunctionDeclaration {
                name: self.name.clone(),
                params: new_params,
                body: new_body.into(),
                is_global: self.is_global,
                storage_class: self.storage_class,
                _type: self._type.clone(),
            },
        )))
    }
}

impl Resolve for BlockItem {
    fn resolve(&mut self, variable_map: &mut HashMap<String, Variable>) -> Result<BlockItem> {
        match self {
            BlockItem::Declaration(decl) => decl.resolve(variable_map),
            BlockItem::Statement(stmt) => stmt.resolve(variable_map),
        }
    }
}

impl Resolve for Statement {
    fn resolve(&mut self, variable_map: &mut HashMap<String, Variable>) -> Result<BlockItem> {
        match self {
            Statement::Expression(expr) => expr.resolve(variable_map),
            Statement::Return(ret) => ret.resolve(variable_map),
            Statement::Null => Ok(BlockItem::Statement(Statement::Null)),
            Statement::Program(prog) => prog.resolve(variable_map),
            Statement::If(if_stmt) => if_stmt.resolve(variable_map),
            Statement::Compound(block) => block.resolve(variable_map),
            Statement::For(for_stmt) => for_stmt.resolve(variable_map),
            Statement::DoWhile(do_while) => do_while.resolve(variable_map),
            Statement::While(while_stmt) => while_stmt.resolve(variable_map),
            Statement::Break(break_stmt) => break_stmt.resolve(variable_map),
            Statement::Continue(continue_stmt) => continue_stmt.resolve(variable_map),
        }
    }
}

impl Resolve for ExpressionStatement {
    fn resolve(&mut self, variable_map: &mut HashMap<String, Variable>) -> Result<BlockItem> {
        let resolved_exp = resolve_exp(&self.expr, variable_map)?;
        Ok(BlockItem::Statement(Statement::Expression(
            ExpressionStatement { expr: resolved_exp },
        )))
    }
}

impl Resolve for ReturnStatement {
    fn resolve(&mut self, variable_map: &mut HashMap<String, Variable>) -> Result<BlockItem> {
        let resolved_exp = resolve_exp(&self.expr, variable_map)?;
        Ok(BlockItem::Statement(Statement::Return(ReturnStatement {
            expr: resolved_exp,
            target_type: self.target_type.clone(),
        })))
    }
}

impl Resolve for ProgramStatement {
    fn resolve(&mut self, variable_map: &mut HashMap<String, Variable>) -> Result<BlockItem> {
        let resolved_block_items = self
            .block_items
            .iter_mut()
            .map(|block_item| block_item.resolve(variable_map))
            .collect::<Result<Vec<_>>>()?;
        Ok(BlockItem::Statement(Statement::Program(ProgramStatement {
            block_items: resolved_block_items,
        })))
    }
}

impl Resolve for IfStatement {
    fn resolve(&mut self, variable_map: &mut HashMap<String, Variable>) -> Result<BlockItem> {
        let resolved_condition = resolve_exp(&self.condition, variable_map)?;
        let resolved_then_branch = self.then_branch.resolve(variable_map)?;
        let mut resolved_else_branch = None;
        if self.else_branch.is_some() {
            resolved_else_branch =
                resolve_optional_block_item(&mut self.else_branch, variable_map)?;
        }
        Ok(BlockItem::Statement(Statement::If(IfStatement {
            condition: resolved_condition,
            then_branch: resolved_then_branch.into(),
            else_branch: resolved_else_branch.into(),
        })))
    }
}

impl Resolve for BlockStatement {
    fn resolve(&mut self, variable_map: &mut HashMap<String, Variable>) -> Result<BlockItem> {
        let mut new_variable_map = copy_variable_map(variable_map);
        let resolved_block_items = self
            .stmts
            .iter_mut()
            .map(|block_item| block_item.resolve(&mut new_variable_map))
            .collect::<Result<Vec<_>>>()?;
        Ok(BlockItem::Statement(Statement::Compound(BlockStatement {
            stmts: resolved_block_items,
        })))
    }
}

impl Resolve for ForStatement {
    fn resolve(&mut self, variable_map: &mut HashMap<String, Variable>) -> Result<BlockItem> {
        let mut new_variable_map = copy_variable_map(variable_map);
        let resolved_init = resolve_for_init(&mut self.init, &mut new_variable_map)?;
        let resolved_condition = resolve_optional_expr(&self.condition, &mut new_variable_map)?;
        let resolved_post = resolve_optional_expr(&self.post, &mut new_variable_map)?;
        let resolved_body = self.body.resolve(&mut new_variable_map)?;
        Ok(BlockItem::Statement(Statement::For(ForStatement {
            init: resolved_init,
            condition: resolved_condition,
            post: resolved_post,
            body: resolved_body.into(),
            label: self.label.clone(),
        })))
    }
}

impl Resolve for DoWhileStatement {
    fn resolve(&mut self, variable_map: &mut HashMap<String, Variable>) -> Result<BlockItem> {
        let mut new_variable_map = copy_variable_map(variable_map);
        let resolved_body = self.body.resolve(&mut new_variable_map)?;
        let resolved_condition = resolve_exp(&self.condition, &mut new_variable_map)?;
        Ok(BlockItem::Statement(Statement::DoWhile(DoWhileStatement {
            body: resolved_body.into(),
            condition: resolved_condition,
            label: self.label.clone(),
        })))
    }
}

impl Resolve for WhileStatement {
    fn resolve(&mut self, variable_map: &mut HashMap<String, Variable>) -> Result<BlockItem> {
        let mut new_variable_map = copy_variable_map(variable_map);
        let resolved_condition = resolve_exp(&self.condition, &mut new_variable_map)?;
        let resolved_body = self.body.resolve(&mut new_variable_map)?;
        Ok(BlockItem::Statement(Statement::While(WhileStatement {
            condition: resolved_condition,
            body: resolved_body.into(),
            label: self.label.clone(),
        })))
    }
}

impl Resolve for BreakStatement {
    fn resolve(&mut self, _variable_map: &mut HashMap<String, Variable>) -> Result<BlockItem> {
        Ok(BlockItem::Statement(Statement::Break(BreakStatement {
            label: self.label.clone(),
        })))
    }
}

impl Resolve for ContinueStatement {
    fn resolve(&mut self, _variable_map: &mut HashMap<String, Variable>) -> Result<BlockItem> {
        Ok(BlockItem::Statement(Statement::Continue(
            ContinueStatement {
                label: self.label.clone(),
            },
        )))
    }
}

fn resolve_exp(
    exp: &Expression,
    variable_map: &mut HashMap<String, Variable>,
) -> Result<Expression> {
    match exp.to_owned() {
        Expression::Assign(AssignExpression {
            op,
            ref lhs,
            rhs,
            _type,
        }) => {
            let resolved_lhs = resolve_exp(lhs, variable_map)?;
            let resolved_rhs = resolve_exp(&rhs, variable_map)?;

            Ok(Expression::Assign(AssignExpression {
                op,
                lhs: resolved_lhs.into(),
                rhs: resolved_rhs.into(),
                _type,
            }))
        }
        Expression::Variable(var) => {
            let variable = variable_map
                .get(&var.value)
                .ok_or_else(|| anyhow::anyhow!("undeclared variable: {}", var.value))?;
            Ok(Expression::Variable(VariableExpression {
                value: variable.name.clone(),
                _type: Type::Dummy,
            }))
        }
        Expression::Constant(konst) => Ok(Expression::Constant(konst)),
        Expression::Unary(UnaryExpression { kind, expr, _type }) => {
            let resolved_expr = resolve_exp(&expr, variable_map)?;

            Ok(Expression::Unary(UnaryExpression {
                kind,
                expr: resolved_expr.into(),
                _type,
            }))
        }
        Expression::Binary(BinaryExpression {
            kind,
            lhs,
            rhs,
            _type,
        }) => {
            let resolved_lhs = resolve_exp(&lhs, variable_map)?;
            let resolved_rhs = resolve_exp(&rhs, variable_map)?;

            Ok(Expression::Binary(BinaryExpression {
                kind,
                lhs: resolved_lhs.into(),
                rhs: resolved_rhs.into(),
                _type,
            }))
        }
        Expression::Conditional(ConditionalExpression {
            condition,
            then_expr,
            else_expr,
            _type,
        }) => {
            let resolved_condition = resolve_exp(&condition, variable_map)?;
            let resolved_then_expr = resolve_exp(&then_expr, variable_map)?;
            let resolved_else_expr = resolve_exp(&else_expr, variable_map)?;

            if let (Expression::Binary(_), Expression::Assign(_), Expression::Assign(_)) = (
                &resolved_condition,
                &resolved_then_expr,
                &resolved_else_expr,
            ) {
                bail!("invalid ternary assignment")
            }

            Ok(Expression::Conditional(ConditionalExpression {
                condition: resolved_condition.into(),
                then_expr: resolved_then_expr.into(),
                else_expr: resolved_else_expr.into(),
                _type,
            }))
        }
        Expression::Call(CallExpression { name, args, _type }) => {
            if variable_map.contains_key(&name) {
                let new_func_name = variable_map.get(&name).unwrap().name.clone();
                let resolved_args = args
                    .iter()
                    .map(|arg| resolve_exp(arg, variable_map))
                    .collect::<Result<Vec<_>>>()?;

                Ok(Expression::Call(CallExpression {
                    name: new_func_name,
                    args: resolved_args,
                    _type,
                }))
            } else {
                bail!("undeclared function");
            }
        }
        Expression::Cast(CastExpression {
            target_type,
            expr,
            _type,
        }) => {
            let resolved_expr = resolve_exp(&expr, variable_map)?;

            Ok(Expression::Cast(CastExpression {
                target_type,
                expr: resolved_expr.into(),
                _type,
            }))
        }
        Expression::AddrOf(AddrOfExpression { expr, _type }) => {
            let resolved_expr = resolve_exp(&expr, variable_map)?;

            Ok(Expression::AddrOf(AddrOfExpression {
                expr: resolved_expr.into(),
                _type,
            }))
        }
        Expression::Deref(DerefExpression { expr, _type }) => {
            let resolved_expr = resolve_exp(&expr, variable_map)?;

            Ok(Expression::Deref(DerefExpression {
                expr: resolved_expr.into(),
                _type,
            }))
        }
        Expression::Subscript(SubscriptExpression { expr, index, _type }) => {
            let resolved_expr = resolve_exp(&expr, variable_map)?;
            let resolved_index = resolve_exp(&index, variable_map)?;

            Ok(Expression::Subscript(SubscriptExpression {
                expr: resolved_expr.into(),
                index: resolved_index.into(),
                _type,
            }))
        }
        Expression::Literal(LiteralExpression { name, value, _type }) => {
            let resolved_init = resolve_init(&value, variable_map)?;
            Ok(Expression::Literal(LiteralExpression {
                name,
                value: resolved_init.into(),
                _type,
            }))
        }
    }
}

fn copy_variable_map(variable_map: &HashMap<String, Variable>) -> HashMap<String, Variable> {
    let spam = variable_map
        .iter()
        .map(|(k, v)| {
            (
                k.clone(),
                Variable {
                    from_current_scope: false,
                    name: v.name.clone(),
                    has_linkage: v.has_linkage,
                },
            )
        })
        .collect();
    spam
}

fn resolve_for_init(
    init: &mut ForInit,
    variable_map: &mut HashMap<String, Variable>,
) -> Result<ForInit> {
    match init {
        ForInit::Expression(expr) => {
            let resolved_expr = resolve_optional_expr(expr, variable_map)?;
            Ok(ForInit::Expression(resolved_expr))
        }
        ForInit::Declaration(ref mut decl) => {
            let resolved_decl = decl.resolve(variable_map)?;
            let var_decl = match resolved_decl {
                BlockItem::Declaration(Declaration::Variable(var_decl)) => var_decl,
                _ => bail!("for init must be a variable declaration"),
            };
            Ok(ForInit::Declaration(var_decl))
        }
    }
}

fn resolve_optional_expr(
    expr: &Option<Expression>,
    variable_map: &mut HashMap<String, Variable>,
) -> Result<Option<Expression>> {
    if expr.is_some() {
        let resolved_expr = resolve_exp(expr.as_ref().unwrap(), variable_map)?;
        Ok(Some(resolved_expr))
    } else {
        Ok(None)
    }
}

fn resolve_optional_block_item(
    block_item: &mut Option<BlockItem>,
    variable_map: &mut HashMap<String, Variable>,
) -> Result<Option<BlockItem>> {
    if block_item.is_some() {
        let resolved_block_item = block_item.as_mut().unwrap().resolve(variable_map)?;
        Ok(Some(resolved_block_item))
    } else {
        Ok(None)
    }
}

fn resolve_param(param: &str, variable_map: &mut HashMap<String, Variable>) -> Result<String> {
    if variable_map.contains_key(param) && variable_map.get(param).unwrap().from_current_scope {
        bail!("redeclaration of parameter: {}", param);
    }

    let unique_name = format!("var.{}.{}", param, make_temporary());

    variable_map.insert(
        param.to_string(),
        Variable {
            from_current_scope: true,
            name: unique_name.clone(),
            has_linkage: false,
        },
    );

    Ok(unique_name)
}
