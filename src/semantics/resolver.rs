use std::collections::BTreeMap;

use crate::{
    ir::gen::make_temporary,
    lexer::lex::Span,
    parser::ast::{
        AddrOfExpression, AggregateKind, ArrowExpression, AssignExpression, BinaryExpression,
        BlockItem, BlockStatement, BreakStatement, CallExpression, CaseStatement, CastExpression,
        CompoundExpression, ConditionalExpression, ContinueStatement, Declaration,
        DefaultStatement, DerefExpression, DoWhileStatement, DotExpression, EnumDeclaration,
        EnumMemberDeclaration, Expression, ExpressionStatement, ForInit, ForStatement,
        FunctionDeclaration, GotoStatement, IfStatement, Initializer, LabeledStatement,
        LiteralExpression, MemberDeclaration, PostfixExpression, Program, ReturnStatement,
        SizeofExpression, SizeofTExpression, Statement, StorageClass, StringExpression,
        StructDeclaration, SubscriptExpression, SwitchStatement, Type, TypedefDeclaration,
        UnaryExpression, VaArgExpression, VaCopyExpression, VaEndExpression, VaStartExpression,
        VariableDeclaration, VariableExpression, WhileStatement,
    },
    util::error::{ErrorKind, Result, UccError},
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
            Declaration::Union(union_decl) => {
                let resolved = union_decl.resolve(variable_map, struct_map)?;
                Ok(Declaration::Union(resolved))
            }
            Declaration::Enum(enum_decl) => {
                let resolved = enum_decl.resolve(variable_map, struct_map)?;
                Ok(Declaration::Enum(resolved))
            }
            Declaration::Typedef(typedef_decl) => {
                let resolved = typedef_decl.resolve(variable_map, struct_map)?;
                Ok(Declaration::Typedef(resolved))
            }
        }
    }
}

impl Resolve for TypedefDeclaration {
    fn resolve(
        self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self> {
        if let Some(prev_entry) = variable_map.get(&self.name) {
            if prev_entry.from_current_scope && !prev_entry.is_typedef {
                return Err(UccError {
                    kind: ErrorKind::Resolve,
                    msg: format!(
                        "typedef name conflicts with ordinary identifier: {}",
                        self.name
                    ),
                    span: self.span,
                });
            }
        }

        let resolved_type = self.ty.resolve(variable_map, struct_map)?;
        variable_map.insert(
            self.name.clone(),
            Variable {
                from_current_scope: true,
                name: self.name.clone(),
                has_linkage: false,
                is_typedef: true,
            },
        );

        Ok(TypedefDeclaration {
            name: self.name,
            ty: resolved_type,
            span: self.span,
        })
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
                if let Some(prev_entry) = variable_map.get(&self.name) {
                    if prev_entry.from_current_scope && prev_entry.is_typedef {
                        return Err(UccError {
                            kind: ErrorKind::Resolve,
                            msg: format!("conflicting global declarations: {}", self.name),
                            span: self.span,
                        });
                    }
                }

                variable_map.insert(
                    self.name.clone(),
                    Variable {
                        from_current_scope: true,
                        name: self.name.clone(),
                        has_linkage: false,
                        is_typedef: false,
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
                    storage_class: self.storage_class,
                    is_global: self.is_global,
                    name: self.name.clone(),
                    span: self.span,
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
                        return Err(UccError {
                            kind: ErrorKind::Resolve,
                            msg: format!("conflicting local declarations: {}", self.name),
                            span: self.span,
                        });
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
                            is_typedef: false,
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
                        storage_class: self.storage_class,
                        is_global: self.is_global,
                        span: self.span,
                    })
                } else {
                    let unique_name = format!("var.{}.{}", self.name, make_temporary());

                    variable_map.insert(
                        self.name.clone(),
                        Variable {
                            from_current_scope: true,
                            name: unique_name.clone(),
                            has_linkage: false,
                            is_typedef: false,
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
                        storage_class: self.storage_class,
                        is_global: self.is_global,
                        span: self.span,
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
            return Err(UccError {
                kind: ErrorKind::Resolve,
                msg: format!("function definition in non-global scope"),
                span: self.span,
            });
        }

        if self
            .storage_class
            .is_some_and(|sc| sc == StorageClass::Static)
            && !self.is_global
        {
            return Err(UccError {
                kind: ErrorKind::Resolve,
                msg: format!("storage class specifier in non-global scope"),
                span: self.span,
            });
        }

        for param in &self.params {
            if self.body.is_some() {
                if let BlockItem::Statement(Statement::Compound(block)) = self.body.clone().unwrap()
                {
                    for stmt in &block.stmts {
                        if let BlockItem::Declaration(Declaration::Variable(var_decl)) = stmt {
                            if var_decl.name == *param {
                                return Err(UccError { kind: ErrorKind::Resolve, msg: format!("parameter name cannot be the same as a variable name in the function body"), span: self.span });
                            }
                        }
                    }
                }
            }
        }

        if variable_map.contains_key(&self.name) {
            let prev_entry = variable_map.get(&self.name).unwrap();
            if prev_entry.from_current_scope && !prev_entry.has_linkage {
                return Err(UccError {
                    kind: ErrorKind::Resolve,
                    msg: format!("redeclaration of function: {}", self.name),
                    span: self.span,
                });
            }
        }

        variable_map.insert(
            self.name.clone(),
            Variable {
                from_current_scope: true,
                name: self.name.clone(),
                has_linkage: true,
                is_typedef: false,
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
            storage_class: self.storage_class,
            span: self.span,
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
        if let Some(prev_entry) = prev_entry {
            if prev_entry.from_current_scope {
                if prev_entry.kind != TagKind::from(self.kind) {
                    return Err(UccError {
                        kind: ErrorKind::Resolve,
                        msg: format!("Conflicting tag declaration"),
                        span: self.span,
                    });
                }
                unique_tag = prev_entry.name.clone();
            } else {
                let aggregate_prefix = match self.kind {
                    AggregateKind::Struct => "struct",
                    AggregateKind::Union => "union",
                };
                unique_tag = format!(
                    "{}.{}.{}",
                    aggregate_prefix,
                    self.tag.clone(),
                    make_temporary()
                );
                struct_map.insert(
                    self.tag.clone(),
                    StructTableEntry {
                        name: unique_tag.clone(),
                        kind: self.kind.into(),
                        from_current_scope: true,
                    },
                );
            }
        } else {
            let aggregate_prefix = match self.kind {
                AggregateKind::Struct => "struct",
                AggregateKind::Union => "union",
            };
            unique_tag = format!(
                "{}.{}.{}",
                aggregate_prefix,
                self.tag.clone(),
                make_temporary()
            );
            struct_map.insert(
                self.tag.clone(),
                StructTableEntry {
                    name: unique_tag.clone(),
                    kind: self.kind.into(),
                    from_current_scope: true,
                },
            );
        }

        let mut processed_members = vec![];

        for member in self.members.into_iter() {
            let processed_type = member.ty.resolve(_variable_map, struct_map)?;
            let processed_member = MemberDeclaration {
                name: member.name.clone(),
                ty: processed_type,
                span: member.span,
            };

            processed_members.push(processed_member);
        }

        Ok(StructDeclaration {
            tag: unique_tag,
            kind: self.kind,
            members: processed_members,
            span: self.span,
        })
    }
}

impl Resolve for EnumDeclaration {
    fn resolve(
        self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self>
    where
        Self: Sized,
    {
        let resolved_tag = if let Some(tag) = &self.tag {
            let prev_entry = struct_map.get(tag);
            if prev_entry.is_none() || !prev_entry.as_ref().unwrap().from_current_scope {
                let unique_tag = format!("enum.{}.{}", tag, make_temporary());
                struct_map.insert(
                    tag.clone(),
                    StructTableEntry {
                        name: unique_tag.clone(),
                        kind: TagKind::Enum,
                        from_current_scope: true,
                    },
                );
                Some(unique_tag)
            } else {
                let prev_entry = prev_entry.unwrap();
                if prev_entry.kind != TagKind::Enum {
                    return Err(UccError {
                        kind: ErrorKind::Resolve,
                        msg: format!("Conflicting tag declaration"),
                        span: self.span,
                    });
                }
                Some(prev_entry.name.clone())
            }
        } else {
            None
        };

        let mut resolved_members = vec![];
        for member in self.members {
            if let Some(prev_entry) = variable_map.get(&member.name) {
                if prev_entry.from_current_scope {
                    return Err(UccError {
                        kind: ErrorKind::Resolve,
                        msg: format!("redeclaration of enumerator: {}", member.name),
                        span: member.span,
                    });
                }
            }

            let resolved_value = member
                .value
                .map(|value| value.resolve(variable_map, struct_map))
                .transpose()?;
            let unique_name = format!("enum.{}.{}", member.name, make_temporary());
            variable_map.insert(
                member.name.clone(),
                Variable {
                    from_current_scope: true,
                    name: unique_name.clone(),
                    has_linkage: false,
                    is_typedef: false,
                },
            );
            resolved_members.push(EnumMemberDeclaration {
                name: unique_name,
                value: resolved_value,
                span: member.span,
            });
        }

        Ok(EnumDeclaration {
            tag: resolved_tag,
            members: resolved_members,
            span: self.span,
        })
    }
}

impl Resolve for Statement {
    fn resolve(
        self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self> {
        match self {
            Statement::Return(stmt_return) => {
                let resolved = stmt_return.resolve(variable_map, struct_map)?;
                Ok(Statement::Return(resolved))
            }

            Statement::Expression(stmt_expr) => {
                let resolved = stmt_expr.resolve(variable_map, struct_map)?;
                Ok(Statement::Expression(resolved))
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

            Statement::Goto(stmt_goto) => {
                let resolved = stmt_goto.resolve(variable_map, struct_map)?;
                Ok(Statement::Goto(resolved))
            }

            Statement::Labeled(stmt_labeled) => {
                let resolved = stmt_labeled.resolve(variable_map, struct_map)?;
                Ok(Statement::Labeled(resolved))
            }

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

            Statement::Null => Ok(Statement::Null),
        }
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
            span: self.span,
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
            span: self.span,
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
            span: self.span,
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
            span: self.span,
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
            span: self.span,
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
            span: self.span,
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
            span: self.span,
        })
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

impl Resolve for GotoStatement {
    fn resolve(
        self,
        _variable_map: &mut BTreeMap<String, Variable>,
        _struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<Self> {
        Ok(self.to_owned())
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
            span: self.span,
        })
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
            span: self.span,
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
        let resolved_value = self.value.resolve(variable_map, struct_map)?;

        Ok(CaseStatement {
            value: resolved_value,
            body: resolved_body.into(),
            label: self.label.clone(),
            span: self.span,
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
            span: self.span,
        })
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
                span,
            }) => {
                let resolved_lhs = lhs.resolve(variable_map, struct_map)?;
                let resolved_rhs = rhs.resolve(variable_map, struct_map)?;

                Ok(Expression::Compound(CompoundExpression {
                    kind,
                    lhs: resolved_lhs.into(),
                    rhs: resolved_rhs.into(),
                    result_t,
                    ty,
                    span,
                }))
            }

            Expression::Assign(AssignExpression {
                op,
                lhs,
                rhs,
                ty,
                span,
            }) => {
                let resolved_lhs = lhs.resolve(variable_map, struct_map)?;
                let resolved_rhs = rhs.resolve(variable_map, struct_map)?;

                Ok(Expression::Assign(AssignExpression {
                    op,
                    lhs: resolved_lhs.into(),
                    rhs: resolved_rhs.into(),
                    ty,
                    span,
                }))
            }

            Expression::Variable(var) => {
                let variable = variable_map.get(&var.value).ok_or_else(|| UccError {
                    kind: ErrorKind::Resolve,
                    msg: format!("undeclared variable: {}", var.value),
                    span: var.span,
                })?;

                if variable.is_typedef {
                    return Err(UccError {
                        kind: ErrorKind::Resolve,
                        msg: format!("typedef name used as variable: {}", var.value),
                        span: var.span,
                    });
                }

                Ok(Expression::Variable(VariableExpression {
                    value: variable.name.clone(),
                    ty: Type::Dummy,
                    span: var.span,
                }))
            }

            Expression::Constant(konst) => Ok(Expression::Constant(konst)),

            Expression::Unary(UnaryExpression {
                kind,
                expr,
                ty,
                span,
            }) => {
                let resolved_expr = expr.resolve(variable_map, struct_map)?;

                Ok(Expression::Unary(UnaryExpression {
                    kind,
                    expr: resolved_expr.into(),
                    ty,
                    span,
                }))
            }

            Expression::Binary(BinaryExpression {
                kind,
                lhs,
                rhs,
                ty,
                span,
            }) => {
                let resolved_lhs = lhs.resolve(variable_map, struct_map)?;
                let resolved_rhs = rhs.resolve(variable_map, struct_map)?;

                Ok(Expression::Binary(BinaryExpression {
                    kind,
                    lhs: resolved_lhs.into(),
                    rhs: resolved_rhs.into(),
                    ty,
                    span,
                }))
            }

            Expression::Conditional(ConditionalExpression {
                condition,
                then_expr,
                else_expr,
                ty,
                span,
            }) => {
                let resolved_condition = condition.resolve(variable_map, struct_map)?;
                let resolved_then_expr = then_expr.resolve(variable_map, struct_map)?;
                let resolved_else_expr = else_expr.resolve(variable_map, struct_map)?;

                Ok(Expression::Conditional(ConditionalExpression {
                    condition: resolved_condition.into(),
                    then_expr: resolved_then_expr.into(),
                    else_expr: resolved_else_expr.into(),
                    ty,
                    span,
                }))
            }

            Expression::Call(CallExpression {
                name,
                args,
                ty,
                span,
            }) => {
                if variable_map.contains_key(&name) {
                    let entry = variable_map.get(&name).unwrap();
                    if entry.is_typedef {
                        return Err(UccError {
                            kind: ErrorKind::Resolve,
                            msg: format!("typedef name used as function: {}", name),
                            span,
                        });
                    }
                    let new_func_name = entry.name.clone();
                    let resolved_args = args
                        .into_iter()
                        .map(|arg| arg.resolve(variable_map, struct_map))
                        .collect::<Result<Vec<_>>>()?;

                    Ok(Expression::Call(CallExpression {
                        name: new_func_name,
                        args: resolved_args,
                        ty,
                        span,
                    }))
                } else {
                    return Err(UccError {
                        kind: ErrorKind::Resolve,
                        msg: format!("undeclared function"),
                        span,
                    });
                }
            }

            Expression::VaStart(VaStartExpression {
                list,
                last_param,
                ty,
                span,
            }) => {
                let resolved_list = list.resolve(variable_map, struct_map)?;
                let resolved_last_param = last_param.resolve(variable_map, struct_map)?;

                Ok(Expression::VaStart(VaStartExpression {
                    list: resolved_list.into(),
                    last_param: resolved_last_param.into(),
                    ty,
                    span,
                }))
            }

            Expression::VaArg(VaArgExpression {
                list,
                arg_ty,
                ty,
                span,
            }) => {
                let resolved_list = list.resolve(variable_map, struct_map)?;
                let resolved_arg_ty = arg_ty.resolve(variable_map, struct_map)?;

                Ok(Expression::VaArg(VaArgExpression {
                    list: resolved_list.into(),
                    arg_ty: resolved_arg_ty,
                    ty,
                    span,
                }))
            }

            Expression::VaCopy(VaCopyExpression { dst, src, ty, span }) => {
                let resolved_dst = dst.resolve(variable_map, struct_map)?;
                let resolved_src = src.resolve(variable_map, struct_map)?;

                Ok(Expression::VaCopy(VaCopyExpression {
                    dst: resolved_dst.into(),
                    src: resolved_src.into(),
                    ty,
                    span,
                }))
            }

            Expression::VaEnd(VaEndExpression { list, ty, span }) => {
                let resolved_list = list.resolve(variable_map, struct_map)?;

                Ok(Expression::VaEnd(VaEndExpression {
                    list: resolved_list.into(),
                    ty,
                    span,
                }))
            }

            Expression::Cast(CastExpression {
                target_type,
                expr,
                ty,
                span,
            }) => {
                let resolved_expr = expr.resolve(variable_map, struct_map)?;
                let resolved_type = target_type.resolve(variable_map, struct_map)?;

                Ok(Expression::Cast(CastExpression {
                    target_type: resolved_type,
                    expr: resolved_expr.into(),
                    ty,
                    span,
                }))
            }

            Expression::AddrOf(AddrOfExpression { expr, ty, span }) => {
                let resolved_expr = expr.resolve(variable_map, struct_map)?;

                Ok(Expression::AddrOf(AddrOfExpression {
                    expr: resolved_expr.into(),
                    ty,
                    span,
                }))
            }

            Expression::Deref(DerefExpression { expr, ty, span }) => {
                let resolved_expr = expr.resolve(variable_map, struct_map)?;

                Ok(Expression::Deref(DerefExpression {
                    expr: resolved_expr.into(),
                    ty,
                    span,
                }))
            }

            Expression::Subscript(SubscriptExpression {
                expr,
                index,
                ty,
                span,
            }) => {
                let resolved_expr = expr.resolve(variable_map, struct_map)?;
                let resolved_index = index.resolve(variable_map, struct_map)?;

                Ok(Expression::Subscript(SubscriptExpression {
                    expr: resolved_expr.into(),
                    index: resolved_index.into(),
                    ty,
                    span,
                }))
            }

            Expression::String(StringExpression { value, ty, span }) => {
                Ok(Expression::String(StringExpression { value, ty, span }))
            }

            Expression::Literal(LiteralExpression {
                name,
                value,
                ty,
                span,
            }) => {
                let resolved_type = ty.resolve(variable_map, struct_map)?;
                let resolved_value = value.resolve(variable_map, struct_map)?;
                Ok(Expression::Literal(LiteralExpression {
                    name,
                    value: resolved_value.into(),
                    ty: resolved_type,
                    span,
                }))
            }

            Expression::Sizeof(SizeofExpression { expr, ty, span }) => {
                let resolved_expr = expr.resolve(variable_map, struct_map)?;
                let resolved_type = ty.resolve(variable_map, struct_map)?;
                Ok(Expression::Sizeof(SizeofExpression {
                    expr: resolved_expr.into(),
                    ty: resolved_type,
                    span,
                }))
            }

            Expression::Dot(DotExpression {
                structure,
                member,
                ty,
                span,
            }) => {
                let resolved_structure = structure.resolve(variable_map, struct_map)?;
                Ok(Expression::Dot(DotExpression {
                    structure: resolved_structure.into(),
                    member,
                    ty,
                    span,
                }))
            }

            Expression::Arrow(ArrowExpression {
                pointer,
                member,
                ty,
                span,
            }) => {
                let resolved_pointer = pointer.resolve(variable_map, struct_map)?;
                Ok(Expression::Arrow(ArrowExpression {
                    pointer: resolved_pointer.into(),
                    member,
                    ty,
                    span,
                }))
            }

            Expression::SizeofT(SizeofTExpression { t, ty, span }) => {
                let resolved_type = t.resolve(variable_map, struct_map)?;
                Ok(Expression::SizeofT(SizeofTExpression {
                    t: resolved_type,
                    ty,
                    span,
                }))
            }

            Expression::Postfix(PostfixExpression {
                kind,
                expr,
                ty,
                span,
            }) => {
                let resolved_type = ty.resolve(variable_map, struct_map)?;
                let resolved_expr = expr.resolve(variable_map, struct_map)?;

                Ok(Expression::Postfix(PostfixExpression {
                    expr: resolved_expr.into(),
                    kind,
                    ty: resolved_type,
                    span,
                }))
            }
        }
    }
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
                if tag == "__builtin_va_list_tag" {
                    return Ok(Type::Struct { tag });
                }

                if let Some(entry) = struct_map.get(&tag) {
                    if entry.kind == TagKind::Struct {
                        Ok(Type::Struct {
                            tag: entry.name.clone(),
                        })
                    } else {
                        return Err(UccError {
                            kind: ErrorKind::Resolve,
                            msg: format!("Specified a non-struct tag as a structure."),
                            span: Span { start: 0, end: 0 },
                        });
                    }
                } else {
                    // In C, a use like `struct Foo *p;` introduces `struct Foo`
                    // as an incomplete type in the current scope.  The parser used
                    // to synthesize a pending empty declaration for that, but pending
                    // declarations can be emitted in the wrong statement position
                    // inside blocks/for initializers.  Do the scope bookkeeping here
                    // instead, without injecting an AST item.
                    let unique_tag = format!("struct.{}.{}", tag, make_temporary());
                    struct_map.insert(
                        tag,
                        StructTableEntry {
                            name: unique_tag.clone(),
                            kind: TagKind::Struct,
                            from_current_scope: true,
                        },
                    );
                    Ok(Type::Struct { tag: unique_tag })
                }
            }

            Type::Union { tag } => {
                if let Some(entry) = struct_map.get(&tag) {
                    if entry.kind == TagKind::Union {
                        Ok(Type::Union {
                            tag: entry.name.clone(),
                        })
                    } else {
                        return Err(UccError {
                            kind: ErrorKind::Resolve,
                            msg: format!("Specified a non-union tag as a union."),
                            span: Span { start: 0, end: 0 },
                        });
                    }
                } else {
                    // Same incomplete-tag rule as `struct Foo *p;` above.
                    let unique_tag = format!("union.{}.{}", tag, make_temporary());
                    struct_map.insert(
                        tag,
                        StructTableEntry {
                            name: unique_tag.clone(),
                            kind: TagKind::Union,
                            from_current_scope: true,
                        },
                    );
                    Ok(Type::Union { tag: unique_tag })
                }
            }

            Type::Enum { tag } => {
                if let Some(entry) = struct_map.get(&tag) {
                    if entry.kind == TagKind::Enum {
                        // This compiler represents enum objects as signed int.
                        Ok(Type::Int)
                    } else {
                        return Err(UccError {
                            kind: ErrorKind::Resolve,
                            msg: format!("Specified a non-enum tag as an enum."),
                            span: Span { start: 0, end: 0 },
                        });
                    }
                } else {
                    return Err(UccError {
                        kind: ErrorKind::Resolve,
                        msg: format!("Specified an undeclared enum tag."),
                        span: Span { start: 0, end: 0 },
                    });
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

            Type::Func {
                params,
                ret,
                variadic,
            } => {
                let mut resolved_params = vec![];
                for param in params {
                    resolved_params.push(param.resolve(variable_map, struct_map)?);
                }
                let resolved_ret = ret.resolve(variable_map, struct_map)?;
                Ok(Type::Func {
                    params: resolved_params,
                    ret: Box::new(resolved_ret),
                    variadic,
                })
            }

            _ => Ok(self.clone()),
        }
    }
}

fn resolve_param(param: &str, variable_map: &mut BTreeMap<String, Variable>) -> Result<String> {
    if param.is_empty() {
        return Ok(String::new());
    }

    if variable_map.contains_key(param) && variable_map.get(param).unwrap().from_current_scope {
        return Err(UccError {
            kind: ErrorKind::Resolve,
            msg: format!("redeclaration of parameter: {}", param),
            span: Span { start: 0, end: 0 },
        });
    }

    let unique_name = format!("var.{}.{}", param, make_temporary());

    variable_map.insert(
        param.to_string(),
        Variable {
            from_current_scope: true,
            name: unique_name.clone(),
            has_linkage: false,
            is_typedef: false,
        },
    );

    Ok(unique_name)
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
                    is_typedef: v.is_typedef,
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
                    kind: v.kind,
                    from_current_scope: false,
                },
            )
        })
        .collect();
    spam
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    name: String,
    from_current_scope: bool,
    has_linkage: bool,
    is_typedef: bool,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TagKind {
    Struct,
    Union,
    Enum,
}

impl From<AggregateKind> for TagKind {
    fn from(kind: AggregateKind) -> Self {
        match kind {
            AggregateKind::Struct => TagKind::Struct,
            AggregateKind::Union => TagKind::Union,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructTableEntry {
    name: String,
    kind: TagKind,
    from_current_scope: bool,
}

#[cfg(test)]
mod enum_tests {
    use super::*;
    use crate::lexer::lex::Lexer;
    use crate::parser::ast::{BlockItem, Declaration, Type};
    use crate::parser::recursive_descent::Parser;
    use std::collections::{BTreeMap, VecDeque};

    fn parse(src: &str) -> crate::parser::ast::Program {
        let tokens: VecDeque<_> = Lexer::new(src.to_string()).collect();
        let mut parser = Parser::new(tokens);
        parser.parse().unwrap()
    }

    #[test]
    fn resolves_enum_tags_to_int_representation() {
        let program = parse("enum Color { RED, GREEN }; enum Color c;");
        let resolved = program
            .resolve(&mut BTreeMap::new(), &mut BTreeMap::new())
            .unwrap();

        assert!(matches!(
            &resolved.block_items[1],
            BlockItem::Declaration(Declaration::Variable(var)) if var.ty == Type::Int
        ));
    }

    #[test]
    fn rejects_conflicting_struct_union_enum_tags() {
        let program = parse("struct Tag; enum Tag { A };");
        assert!(program
            .resolve(&mut BTreeMap::new(), &mut BTreeMap::new())
            .is_err());
    }

    #[test]
    fn rejects_typedef_conflicts_in_the_ordinary_identifier_namespace() {
        let program = parse("int T; typedef int T;");
        assert!(program
            .resolve(&mut BTreeMap::new(), &mut BTreeMap::new())
            .is_err());

        let program = parse("typedef int T; int T;");
        assert!(program
            .resolve(&mut BTreeMap::new(), &mut BTreeMap::new())
            .is_err());
    }
    #[test]
    fn resolves_prototypes_with_unnamed_parameters() {
        let program = parse("int f(int, char *, struct Unknown *); struct Unknown { int x; };");
        program
            .resolve(&mut BTreeMap::new(), &mut BTreeMap::new())
            .unwrap();
    }

    #[test]
    fn resolves_compound_literal_initializers() {
        let program = parse(
            r#"
            struct Token { int kind; int len; };
            struct Token next(void) {
                return (struct Token){ .kind = 1, .len = 2 };
            }
            "#,
        );
        program
            .resolve(&mut BTreeMap::new(), &mut BTreeMap::new())
            .unwrap();
    }
}
