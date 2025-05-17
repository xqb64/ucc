use anyhow::{bail, Ok, Result};
use std::collections::BTreeMap;

use crate::{
    ir::gen::make_temporary,
    parser::ast::{
        AddrOfExpression, ArrowExpression, AssignExpression, BinaryExpression, BlockItem,
        BlockStatement, BreakStatement, CallExpression, CaseStatement, CastExpression,
        CompoundExpression, ConditionalExpression, ContinueStatement, Declaration,
        DefaultStatement, DerefExpression, DoWhileStatement, DotExpression, Expression,
        ExpressionStatement, ForInit, ForStatement, FunctionDeclaration, GotoStatement,
        IfStatement, Initializer, LabeledStatement, MemberDeclaration, PostfixExpression, Program,
        ReturnStatement, SizeofExpression, SizeofTExpression, Statement, StorageClass,
        StringExpression, StructDeclaration, SubscriptExpression, SwitchStatement, Type,
        UnaryExpression, VariableDeclaration, VariableExpression, WhileStatement,
    },
};

pub trait Resolve {
    fn resolve(
        self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self>
    where
        Self: Sized;
}

impl Resolve for Program {
    fn resolve(
        self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self> {
        let resolved_block_items = self
            .block_items
            .into_iter()
            .map(|block_item| block_item.resolve(variable_map, struct_map))
            .collect::<Result<Vec<_>>>()?;

        Ok(Program {
            block_items: resolved_block_items,
        })
    }
}

impl Resolve for BlockItem {
    fn resolve(
        self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self> {
        match self {
            BlockItem::Declaration(decl) => {
                let resolved = decl.resolve(variable_map, struct_map)?;
                Ok(BlockItem::Declaration(resolved))
            }
            BlockItem::Statement(stmt) => {
                let resolved = stmt.resolve(variable_map, struct_map)?;
                Ok(BlockItem::Statement(resolved))
            }
        }
    }
}
impl Resolve for Declaration {
    fn resolve(
        self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self> {
        match self {
            Declaration::Variable(var_decl) => {
                let resolved = var_decl.resolve(variable_map, struct_map)?;
                Ok(Declaration::Variable(resolved))
            }
            Declaration::Function(func_decl) => {
                let resolved = func_decl.resolve(variable_map, struct_map)?;
                Ok(Declaration::Function(resolved))
            }
            Declaration::Struct(struct_decl) => {
                let resolved = struct_decl.resolve(variable_map, struct_map)?;
                Ok(Declaration::Struct(resolved))
            }
        }
    }
}

impl Resolve for VariableDeclaration {
    fn resolve(
        self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self> {
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

                let resolved_init = self
                    .init
                    .map(|init| init.resolve(variable_map, struct_map))
                    .transpose()?;

                let resolved_type = self.ty.resolve(variable_map, struct_map)?;

                Ok(VariableDeclaration {
                    init: resolved_init,
                    ty: resolved_type,
                    storage_class: self.storage_class.clone(),
                    is_global: self.is_global,
                    name: self.name.clone(),
                })
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

                    let resolved_init = self
                        .init
                        .map(|init| init.resolve(variable_map, struct_map))
                        .transpose()?;

                    let resolved_type = self.ty.resolve(variable_map, struct_map)?;

                    Ok(VariableDeclaration {
                        name: self.name.clone(),
                        ty: resolved_type,
                        init: resolved_init,
                        storage_class: self.storage_class.clone(),
                        is_global: self.is_global,
                    })
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

                    let resolved_init = self
                        .init
                        .map(|init| init.resolve(variable_map, struct_map))
                        .transpose()?;

                    let resolved_type = self.ty.resolve(variable_map, struct_map)?;

                    Ok(VariableDeclaration {
                        name: unique_name,
                        ty: resolved_type,
                        init: resolved_init,
                        storage_class: self.storage_class.clone(),
                        is_global: self.is_global,
                    })
                }
            }
        }
    }
}

impl Resolve for FunctionDeclaration {
    fn resolve(
        self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self> {
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
        let mut new_struct_map = copy_struct_map(struct_map);

        let resolved_params = self
            .params
            .iter()
            .map(|param| resolve_param(param, &mut inner_map))
            .collect::<Result<Vec<_>>>()?;

        let resolved_body = match *self.body {
            Some(body) => body.resolve(&mut inner_map, &mut new_struct_map)?.into(),
            None => None,
        };

        let resolved_type = self.ty.resolve(&mut inner_map, &mut new_struct_map)?;

        Ok(FunctionDeclaration {
            name: self.name.clone(),
            ty: resolved_type,
            params: resolved_params,
            body: resolved_body.into(),
            is_global: self.is_global,
            storage_class: self.storage_class.clone(),
        })
    }
}

impl Resolve for StructDeclaration {
    fn resolve(
        self,
        _variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self>
    where
        Self: Sized,
    {
        let prev_entry = struct_map.get(&self.tag);

        let unique_tag;
        if prev_entry.is_none() || !prev_entry.as_ref().unwrap().from_current_scope {
            unique_tag = format!("struct.{}.{}", self.tag.clone(), make_temporary());
            struct_map.insert(
                self.tag.clone(),
                StructTableEntry {
                    name: unique_tag.clone(),
                    from_current_scope: true,
                },
            );
        } else {
            unique_tag = prev_entry.unwrap().name.clone();
        }

        let mut processed_members = vec![];

        for member in self.members.into_iter() {
            let processed_type = member.ty.resolve(_variable_map, struct_map)?;
            let processed_member = MemberDeclaration {
                name: member.name.clone(),
                ty: processed_type,
            };

            processed_members.push(processed_member);
        }

        Ok(StructDeclaration {
            tag: unique_tag,
            members: processed_members,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    name: String,
    from_current_scope: bool,
    has_linkage: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructTableEntry {
    name: String,
    from_current_scope: bool,
}

impl Resolve for Initializer {
    fn resolve(
        self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self>
    where
        Self: Sized,
    {
        match self {
            Initializer::Single(name, single_init) => {
                let resolved_expr = single_init.resolve(variable_map, struct_map)?;
                Ok(Initializer::Single(name.to_owned(), resolved_expr))
            }
            Initializer::Compound(name, ty, compound_init) => {
                let resolved_inits = compound_init
                    .into_iter()
                    .map(|init| init.resolve(variable_map, struct_map))
                    .collect::<Result<Vec<_>>>()?;

                Ok(Initializer::Compound(
                    name.to_owned(),
                    ty.to_owned(),
                    resolved_inits,
                ))
            }
        }
    }
}

impl Resolve for Statement {
    fn resolve(
        self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self> {
        match self {
            Statement::Expression(stmt_expr) => {
                let resolved = stmt_expr.resolve(variable_map, struct_map)?;
                Ok(Statement::Expression(resolved))
            }

            Statement::Goto(stmt_goto) => {
                let resolved = stmt_goto.resolve(variable_map, struct_map)?;
                Ok(Statement::Goto(resolved))
            }

            Statement::Labeled(stmt_labeled) => {
                let resolved = stmt_labeled.resolve(variable_map, struct_map)?;
                Ok(Statement::Labeled(resolved))
            }

            Statement::Return(stmt_return) => {
                let resolved = stmt_return.resolve(variable_map, struct_map)?;
                Ok(Statement::Return(resolved))
            }
            Statement::If(stmt_if) => {
                let resolved = stmt_if.resolve(variable_map, struct_map)?;
                Ok(Statement::If(resolved))
            }

            Statement::Compound(block) => {
                let resolved = block.resolve(variable_map, struct_map)?;
                Ok(Statement::Compound(resolved))
            }

            Statement::For(stmt_for) => {
                let resolved = stmt_for.resolve(variable_map, struct_map)?;
                Ok(Statement::For(resolved))
            }

            Statement::DoWhile(stmt_do_while) => {
                let resolved = stmt_do_while.resolve(variable_map, struct_map)?;
                Ok(Statement::DoWhile(resolved))
            }

            Statement::While(stmt_while) => {
                let resolved = stmt_while.resolve(variable_map, struct_map)?;
                Ok(Statement::While(resolved))
            }

            Statement::Break(stmt_break) => {
                let resolved = stmt_break.resolve(variable_map, struct_map)?;
                Ok(Statement::Break(resolved))
            }

            Statement::Continue(stmt_continue) => {
                let resolved = stmt_continue.resolve(variable_map, struct_map)?;
                Ok(Statement::Continue(resolved))
            }

            Statement::Null => Ok(Statement::Null),

            Statement::Switch(stmt_switch) => {
                let resolved = stmt_switch.resolve(variable_map, struct_map)?;
                Ok(Statement::Switch(resolved))
            }

            Statement::Case(stmt_case) => {
                let resolved = stmt_case.resolve(variable_map, struct_map)?;
                Ok(Statement::Case(resolved))
            }

            Statement::Default(stmt_default) => {
                let resolved = stmt_default.resolve(variable_map, struct_map)?;
                Ok(Statement::Default(resolved))
            }
        }
    }
}

impl Resolve for SwitchStatement {
    fn resolve(
        self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self>
    where
        Self: Sized,
    {
        let resolved_condition = self.condition.resolve(variable_map, struct_map)?;
        let resolved_body = self.body.resolve(variable_map, struct_map)?;

        Ok(SwitchStatement {
            condition: resolved_condition,
            body: resolved_body.into(),
            label: self.label.clone(),
            cases: self.cases.clone(),
        })
    }
}

impl Resolve for CaseStatement {
    fn resolve(
        self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self>
    where
        Self: Sized,
    {
        let resolved_body = self.body.resolve(variable_map, struct_map)?;

        Ok(CaseStatement {
            value: self.value.clone(),
            body: resolved_body.into(),
            label: self.label.clone(),
        })
    }
}

impl Resolve for DefaultStatement {
    fn resolve(
        self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self>
    where
        Self: Sized,
    {
        let resolved_body = self.body.resolve(variable_map, struct_map)?;

        Ok(DefaultStatement {
            body: resolved_body.into(),
            label: self.label.clone(),
        })
    }
}

impl Resolve for LabeledStatement {
    fn resolve(
        self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self>
    where
        Self: Sized,
    {
        let resolved_body = self.body.resolve(variable_map, struct_map)?;

        Ok(LabeledStatement {
            label: self.label.clone(),
            body: resolved_body.into(),
        })
    }
}

impl Resolve for ExpressionStatement {
    fn resolve(
        self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self> {
        let resolved_expr = self.expr.resolve(variable_map, struct_map)?;

        Ok(ExpressionStatement {
            expr: resolved_expr,
        })
    }
}

impl Resolve for ReturnStatement {
    fn resolve(
        self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self> {
        let resolved_expr = self
            .expr
            .map(|expr| expr.resolve(variable_map, struct_map))
            .transpose()?;

        let resolved_target_type = self
            .target_type
            .map(|ty| ty.resolve(variable_map, struct_map))
            .transpose()?;

        Ok(ReturnStatement {
            expr: resolved_expr,
            target_type: resolved_target_type,
            belongs_to: self.belongs_to.clone(),
        })
    }
}

impl Resolve for IfStatement {
    fn resolve(
        self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self> {
        let resolved_condition = self.condition.resolve(variable_map, struct_map)?;
        let resolved_then_branch = self
            .then_branch
            .resolve(variable_map, struct_map)?
            .to_owned()
            .into();

        let resolved_else_branch = match *self.else_branch {
            Some(else_branch) => else_branch.resolve(variable_map, struct_map)?.into(),
            None => None,
        };

        Ok(IfStatement {
            condition: resolved_condition,
            then_branch: resolved_then_branch,
            else_branch: resolved_else_branch.into(),
        })
    }
}

impl Resolve for BlockStatement {
    fn resolve(
        self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self> {
        let mut new_variable_map = copy_variable_map(variable_map);
        let mut new_struct_map = copy_struct_map(struct_map);

        let resolved_stmts = self
            .stmts
            .into_iter()
            .map(|stmt| stmt.resolve(&mut new_variable_map, &mut new_struct_map))
            .collect::<Result<Vec<_>>>()?;

        Ok(BlockStatement {
            stmts: resolved_stmts,
        })
    }
}

impl Resolve for ForStatement {
    fn resolve(
        self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self> {
        let mut new_variable_map = copy_variable_map(variable_map);
        let mut new_struct_map = copy_struct_map(struct_map);

        let resolved_init = self
            .init
            .resolve(&mut new_variable_map, &mut new_struct_map)?;

        let resolved_condition = match self.condition {
            Some(condition) => condition.resolve(&mut new_variable_map, struct_map)?.into(),
            None => None,
        };

        let resolved_post = match self.post {
            Some(post) => post.resolve(&mut new_variable_map, struct_map)?.into(),
            None => None,
        };

        let resolved_body = self
            .body
            .resolve(&mut new_variable_map, &mut new_struct_map)?
            .to_owned()
            .into();

        Ok(ForStatement {
            init: resolved_init,
            condition: resolved_condition,
            post: resolved_post,
            body: resolved_body,
            label: self.label.clone(),
        })
    }
}

impl Resolve for DoWhileStatement {
    fn resolve(
        self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self> {
        let mut new_variable_map = copy_variable_map(variable_map);
        let mut new_struct_map = copy_struct_map(struct_map);

        let resolved_body = self
            .body
            .resolve(&mut new_variable_map, &mut new_struct_map)?
            .to_owned()
            .into();

        let resolved_condition = self.condition.resolve(&mut new_variable_map, struct_map)?;

        Ok(DoWhileStatement {
            condition: resolved_condition,
            body: resolved_body,
            label: self.label.clone(),
        })
    }
}

impl Resolve for WhileStatement {
    fn resolve(
        self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self> {
        let mut new_variable_map = copy_variable_map(variable_map);
        let mut new_struct_map = copy_struct_map(struct_map);

        let resolved_condition = self.condition.resolve(&mut new_variable_map, struct_map)?;

        let resolved_body = self
            .body
            .resolve(&mut new_variable_map, &mut new_struct_map)?
            .to_owned()
            .into();

        Ok(WhileStatement {
            condition: resolved_condition,
            body: resolved_body,
            label: self.label.clone(),
        })
    }
}

impl Resolve for GotoStatement {
    fn resolve(
        self,
        _variable_map: &mut BTreeMap<String, Variable>,
        _struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self> {
        Ok(self.to_owned())
    }
}

impl Resolve for BreakStatement {
    fn resolve(
        self,
        _variable_map: &mut BTreeMap<String, Variable>,
        _struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self> {
        Ok(self.to_owned())
    }
}

impl Resolve for ContinueStatement {
    fn resolve(
        self,
        _variable_map: &mut BTreeMap<String, Variable>,
        _struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self> {
        Ok(self.to_owned())
    }
}

impl Resolve for Expression {
    fn resolve(
        self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self>
    where
        Self: Sized,
    {
        match self.to_owned() {
            Expression::Compound(CompoundExpression {
                kind,
                lhs,
                rhs,
                result_t,
                ty,
            }) => {
                let resolved_lhs = lhs.resolve(variable_map, struct_map)?;
                let resolved_rhs = rhs.resolve(variable_map, struct_map)?;

                Ok(Expression::Compound(CompoundExpression {
                    kind,
                    lhs: resolved_lhs.into(),
                    rhs: resolved_rhs.into(),
                    result_t,
                    ty,
                }))
            }

            Expression::Assign(AssignExpression {
                op,
                lhs,
                rhs,
                ty,
            }) => {
                let resolved_lhs = lhs.resolve(variable_map, struct_map)?;
                let resolved_rhs = rhs.resolve(variable_map, struct_map)?;

                Ok(Expression::Assign(AssignExpression {
                    op,
                    lhs: resolved_lhs.into(),
                    rhs: resolved_rhs.into(),
                    ty,
                }))
            }

            Expression::Variable(var) => {
                let variable = variable_map
                    .get(&var.value)
                    .ok_or_else(|| anyhow::anyhow!("undeclared variable: {}", var.value))?;

                Ok(Expression::Variable(VariableExpression {
                    value: variable.name.clone(),
                    ty: Type::Dummy,
                }))
            }

            Expression::Constant(konst) => Ok(Expression::Constant(konst)),

            Expression::Unary(UnaryExpression { kind, expr, ty }) => {
                let resolved_expr = expr.resolve(variable_map, struct_map)?;

                Ok(Expression::Unary(UnaryExpression {
                    kind,
                    expr: resolved_expr.into(),
                    ty,
                }))
            }

            Expression::Binary(BinaryExpression {
                kind,
                lhs,
                rhs,
                ty,
            }) => {
                let resolved_lhs = lhs.resolve(variable_map, struct_map)?;
                let resolved_rhs = rhs.resolve(variable_map, struct_map)?;

                Ok(Expression::Binary(BinaryExpression {
                    kind,
                    lhs: resolved_lhs.into(),
                    rhs: resolved_rhs.into(),
                    ty,
                }))
            }

            Expression::Conditional(ConditionalExpression {
                condition,
                then_expr,
                else_expr,
                ty,
            }) => {
                let resolved_condition = condition.resolve(variable_map, struct_map)?;
                let resolved_then_expr = then_expr.resolve(variable_map, struct_map)?;
                let resolved_else_expr = else_expr.resolve(variable_map, struct_map)?;

                Ok(Expression::Conditional(ConditionalExpression {
                    condition: resolved_condition.into(),
                    then_expr: resolved_then_expr.into(),
                    else_expr: resolved_else_expr.into(),
                    ty,
                }))
            }

            Expression::Call(CallExpression { name, args, ty }) => {
                if variable_map.contains_key(&name) {
                    let new_func_name = variable_map.get(&name).unwrap().name.clone();
                    let resolved_args = args
                        .into_iter()
                        .map(|arg| arg.resolve(variable_map, struct_map))
                        .collect::<Result<Vec<_>>>()?;

                    Ok(Expression::Call(CallExpression {
                        name: new_func_name,
                        args: resolved_args,
                        ty,
                    }))
                } else {
                    bail!("undeclared function");
                }
            }

            Expression::Cast(CastExpression {
                target_type,
                expr,
                ty,
            }) => {
                let resolved_expr = expr.resolve(variable_map, struct_map)?;
                let resolved_type = target_type.resolve(variable_map, struct_map)?;

                Ok(Expression::Cast(CastExpression {
                    target_type: resolved_type,
                    expr: resolved_expr.into(),
                    ty,
                }))
            }

            Expression::AddrOf(AddrOfExpression { expr, ty }) => {
                let resolved_expr = expr.resolve(variable_map, struct_map)?;

                Ok(Expression::AddrOf(AddrOfExpression {
                    expr: resolved_expr.into(),
                    ty,
                }))
            }

            Expression::Deref(DerefExpression { expr, ty }) => {
                let resolved_expr = expr.resolve(variable_map, struct_map)?;

                Ok(Expression::Deref(DerefExpression {
                    expr: resolved_expr.into(),
                    ty,
                }))
            }

            Expression::Subscript(SubscriptExpression { expr, index, ty }) => {
                let resolved_expr = expr.resolve(variable_map, struct_map)?;
                let resolved_index = index.resolve(variable_map, struct_map)?;

                Ok(Expression::Subscript(SubscriptExpression {
                    expr: resolved_expr.into(),
                    index: resolved_index.into(),
                    ty,
                }))
            }

            Expression::String(StringExpression { value, ty }) => {
                Ok(Expression::String(StringExpression { value, ty }))
            }

            Expression::Sizeof(SizeofExpression { expr, ty }) => {
                let resolved_expr = expr.resolve(variable_map, struct_map)?;
                let resolved_type = ty.resolve(variable_map, struct_map)?;
                Ok(Expression::Sizeof(SizeofExpression {
                    expr: resolved_expr.into(),
                    ty: resolved_type,
                }))
            }

            Expression::Dot(DotExpression {
                structure,
                member,
                ty,
            }) => {
                let resolved_structure = structure.resolve(variable_map, struct_map)?;
                Ok(Expression::Dot(DotExpression {
                    structure: resolved_structure.into(),
                    member,
                    ty,
                }))
            }

            Expression::Arrow(ArrowExpression {
                pointer,
                member,
                ty,
            }) => {
                let resolved_pointer = pointer.resolve(variable_map, struct_map)?;
                Ok(Expression::Arrow(ArrowExpression {
                    pointer: resolved_pointer.into(),
                    member,
                    ty,
                }))
            }

            Expression::SizeofT(SizeofTExpression { t, ty }) => {
                let resolved_type = t.resolve(variable_map, struct_map)?;
                Ok(Expression::SizeofT(SizeofTExpression {
                    t: resolved_type,
                    ty,
                }))
            }

            Expression::Postfix(PostfixExpression { kind, expr, ty }) => {
                let resolved_type = ty.resolve(variable_map, struct_map)?;
                let resolved_expr = expr.resolve(variable_map, struct_map)?;

                Ok(Expression::Postfix(PostfixExpression {
                    expr: resolved_expr.into(),
                    kind,
                    ty: resolved_type,
                }))
            }

            _ => unreachable!(),
        }
    }
}

fn copy_variable_map(variable_map: &BTreeMap<String, Variable>) -> BTreeMap<String, Variable> {
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

fn copy_struct_map(
    struct_map: &BTreeMap<String, StructTableEntry>,
) -> BTreeMap<String, StructTableEntry> {
    let spam = struct_map
        .iter()
        .map(|(k, v)| {
            (
                k.clone(),
                StructTableEntry {
                    name: v.name.clone(),
                    from_current_scope: false,
                },
            )
        })
        .collect();
    spam
}

impl Resolve for ForInit {
    fn resolve(
        self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self>
    where
        Self: Sized,
    {
        match self {
            ForInit::Expression(expr) => {
                if let Some(expr) = expr {
                    let resolved_expr = expr.resolve(variable_map, struct_map)?;
                    Ok(ForInit::Expression(resolved_expr.into()))
                } else {
                    Ok(ForInit::Expression(expr.to_owned()))
                }
            }
            ForInit::Declaration(decl) => {
                let resolved_decl = decl.resolve(variable_map, struct_map)?;
                Ok(ForInit::Declaration(resolved_decl.to_owned()))
            }
        }
    }
}

fn resolve_param(param: &str, variable_map: &mut BTreeMap<String, Variable>) -> Result<String> {
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

impl Resolve for Type {
    fn resolve(
        self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self>
    where
        Self: Sized,
    {
        match self {
            Type::Struct { tag } => {
                if struct_map.contains_key(&tag) {
                    let unique_tag = struct_map.get(&tag).cloned().unwrap().name.clone();
                    Ok(Type::Struct { tag: unique_tag })
                } else {
                    bail!("Specified an undeclared structure tag.")
                }
            }

            Type::Pointer(referenced) => {
                let resolved_referenced = referenced.resolve(variable_map, struct_map)?;
                Ok(Type::Pointer(Box::new(resolved_referenced)))
            }

            Type::Array { element, size } => {
                let resolved_element = element.resolve(variable_map, struct_map)?;
                Ok(Type::Array {
                    element: Box::new(resolved_element),
                    size,
                })
            }

            Type::Func { params, ret } => {
                let mut resolved_params = vec![];
                for param in params {
                    resolved_params.push(param.resolve(variable_map, struct_map)?);
                }
                let resolved_ret = ret.resolve(variable_map, struct_map)?;
                Ok(Type::Func {
                    params: resolved_params,
                    ret: Box::new(resolved_ret),
                })
            }

            _ => Ok(self.clone()),
        }
    }
}
