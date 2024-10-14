use anyhow::{bail, Ok, Result};
use std::collections::BTreeMap;

use crate::{
    ir::gen::make_temporary,
    parser::ast::{
        AddrOfExpression, ArrowExpression, AssignExpression, BinaryExpression, BlockItem,
        BlockStatement, BreakStatement, CallExpression, CastExpression, ConditionalExpression,
        ContinueStatement, Declaration, DerefExpression, DoWhileStatement, DotExpression,
        Expression, ExpressionStatement, ForInit, ForStatement, FunctionDeclaration, IfStatement,
        Initializer, LiteralExpression, MemberDeclaration, ProgramStatement, ReturnStatement,
        SizeofExpression, SizeofTExpression, Statement, StorageClass, StringExpression,
        StructDeclaration, SubscriptExpression, Type, UnaryExpression, VariableDeclaration,
        VariableExpression, WhileStatement,
    },
};

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

pub trait Resolve {
    fn resolve(
        &mut self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<&mut Self>
    where
        Self: Sized;
}

impl Resolve for Declaration {
    fn resolve(
        &mut self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<&mut Self> {
        match self {
            Declaration::Variable(var_decl) => {
                var_decl.resolve(variable_map, struct_map)?;
                Ok(self)
            }
            Declaration::Function(func_decl) => {
                func_decl.resolve(variable_map, struct_map)?;
                Ok(self)
            }
            Declaration::Struct(struct_decl) => {
                resolve_structure_declaration(struct_decl, struct_map)?;
                Ok(self)
            }
        }
    }
}

fn resolve_structure_declaration<'a>(
    decl: &'a mut StructDeclaration,
    struct_map: &'a mut BTreeMap<String, StructTableEntry>,
) -> Result<StructDeclaration> {
    let prev_entry = struct_map.get(&decl.tag).cloned();

    let unique_tag;
    if prev_entry.is_none() || !prev_entry.clone().unwrap().from_current_scope {
        unique_tag = format!("struct.{}.{}", decl.tag.clone(), make_temporary());
        struct_map.insert(
            decl.tag.clone(),
            StructTableEntry {
                name: unique_tag.clone(),
                from_current_scope: true,
            },
        );
    } else {
        unique_tag = prev_entry.unwrap().name.clone();
    }

    let mut processed_members = vec![];

    for member in &decl.members {
        let processed_type = resolve_type(&member._type, struct_map)?;
        let processed_member = MemberDeclaration {
            name: member.name.clone(),
            _type: processed_type,
        };

        processed_members.push(processed_member);
    }

    *decl = StructDeclaration {
        tag: unique_tag.clone(),
        members: processed_members.clone(),
    };

    Ok(StructDeclaration {
        tag: unique_tag.clone(),
        members: processed_members,
    })
}

fn optionally_resolve_init(
    init: &Option<Initializer>,
    variable_map: &mut BTreeMap<String, Variable>,
    struct_map: &mut BTreeMap<String, StructTableEntry>,
) -> Result<Option<Initializer>> {
    if init.is_some() {
        let resolved_init = resolve_init(init.as_ref().unwrap(), variable_map, struct_map)?;
        Ok(Some(resolved_init))
    } else {
        Ok(None)
    }
}

impl Resolve for VariableDeclaration {
    fn resolve(
        &mut self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<&mut Self> {
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

                self.init = optionally_resolve_init(&self.init, variable_map, struct_map)?;
                self._type = resolve_type(&self._type, struct_map)?;

                Ok(self)
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

                    self.init = optionally_resolve_init(&self.init, variable_map, struct_map)?;
                    self._type = resolve_type(&self._type, struct_map)?;

                    Ok(self)
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

                    self.name = unique_name;
                    self.init = optionally_resolve_init(&self.init, variable_map, struct_map)?;
                    self._type = resolve_type(&self._type, struct_map)?;

                    Ok(self)
                }
            }
        }
    }
}

fn resolve_init(
    init: &Initializer,
    variable_map: &mut BTreeMap<String, Variable>,
    struct_map: &mut BTreeMap<String, StructTableEntry>,
) -> Result<Initializer> {
    match init {
        Initializer::Single(name, single_init) => {
            let resolved_expr = resolve_exp(single_init, variable_map, struct_map)?;
            Ok(Initializer::Single(name.clone(), resolved_expr))
        }
        Initializer::Compound(name, _type, compound_init) => {
            let mut resolved_inits = vec![];
            for init in compound_init {
                resolved_inits.push(resolve_init(init, variable_map, struct_map)?);
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
    fn resolve(
        &mut self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<&mut Self> {
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

        let mut new_params = vec![];

        for param in &self.params {
            new_params.push(resolve_param(param, &mut inner_map)?);
        }

        self.body =
            resolve_optional_block_item(&mut self.body, &mut inner_map, &mut new_struct_map)?
                .into();
        self.params = new_params;
        self._type = resolve_type(&self._type, &mut new_struct_map)?;

        Ok(self)
    }
}

impl Resolve for BlockItem {
    fn resolve(
        &mut self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<&mut Self> {
        match self {
            BlockItem::Declaration(decl) => {
                decl.resolve(variable_map, struct_map)?;
                Ok(self)
            }
            BlockItem::Statement(stmt) => {
                stmt.resolve(variable_map, struct_map)?;
                Ok(self)
            }
        }
    }
}

impl Resolve for Statement {
    fn resolve(
        &mut self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<&mut Self> {
        match self {
            Statement::Expression(expr) => {
                expr.resolve(variable_map, struct_map)?;
            }

            Statement::Return(ret) => {
                ret.resolve(variable_map, struct_map)?;
            }

            Statement::Program(prog) => {
                prog.resolve(variable_map, struct_map)?;
            }

            Statement::If(if_stmt) => {
                if_stmt.resolve(variable_map, struct_map)?;
            }

            Statement::Compound(block) => {
                block.resolve(variable_map, struct_map)?;
            }

            Statement::For(for_stmt) => {
                for_stmt.resolve(variable_map, struct_map)?;
            }

            Statement::DoWhile(do_while) => {
                do_while.resolve(variable_map, struct_map)?;
            }

            Statement::While(while_stmt) => {
                while_stmt.resolve(variable_map, struct_map)?;
            }

            Statement::Break(break_stmt) => {
                break_stmt.resolve(variable_map, struct_map)?;
            }

            Statement::Continue(continue_stmt) => {
                continue_stmt.resolve(variable_map, struct_map)?;
            }

            Statement::Null => {},
        }

        Ok(self)
    }
}

impl Resolve for ExpressionStatement {
    fn resolve(
        &mut self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<&mut Self> {
        self.expr = resolve_exp(&self.expr, variable_map, struct_map)?;

        Ok(self)
    }
}

impl Resolve for ReturnStatement {
    fn resolve(
        &mut self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<&mut Self> {
        self.expr = resolve_optional_expr(&self.expr, variable_map, struct_map)?;
        self.target_type = optionally_resolve_type(&self.target_type, struct_map)?;

        Ok(self)
    }
}

fn optionally_resolve_type(
    target_type: &Option<Type>,
    struct_map: &mut BTreeMap<String, StructTableEntry>,
) -> Result<Option<Type>> {
    if target_type.is_some() {
        let resolved_type = resolve_type(target_type.as_ref().unwrap(), struct_map)?;

        Ok(Some(resolved_type))
    } else {
        Ok(None)
    }
}

impl Resolve for ProgramStatement {
    fn resolve(
        &mut self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<&mut Self> {
        for block_item in &mut self.block_items {
            block_item.resolve(variable_map, struct_map)?;
        }

        Ok(self)
    }
}

impl Resolve for IfStatement {
    fn resolve(
        &mut self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<&mut Self> {
        self.condition = resolve_exp(&self.condition, variable_map, struct_map)?;
        self.then_branch = self
            .then_branch
            .resolve(variable_map, struct_map)?
            .to_owned()
            .into();
        self.else_branch =
            resolve_optional_block_item(&mut self.else_branch, variable_map, struct_map)?.into();

        Ok(self)
    }
}

impl Resolve for BlockStatement {
    fn resolve(
        &mut self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<&mut Self> {
        let mut new_variable_map = copy_variable_map(variable_map);
        let mut new_struct_map = copy_struct_map(struct_map);

        for stmt in &mut self.stmts {
            stmt.resolve(&mut new_variable_map, &mut new_struct_map)?;
        }

        Ok(self)
    }
}

impl Resolve for ForStatement {
    fn resolve(
        &mut self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<&mut Self> {
        let mut new_variable_map = copy_variable_map(variable_map);
        let mut new_struct_map = copy_struct_map(struct_map);

        self.init = resolve_for_init(&mut self.init, &mut new_variable_map, &mut new_struct_map)?;
        self.condition = resolve_optional_expr(&self.condition, &mut new_variable_map, struct_map)?;
        self.post = resolve_optional_expr(&self.post, &mut new_variable_map, struct_map)?;
        self.body = self
            .body
            .resolve(&mut new_variable_map, &mut new_struct_map)?
            .to_owned()
            .into();

        Ok(self)
    }
}

impl Resolve for DoWhileStatement {
    fn resolve(
        &mut self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<&mut Self> {
        let mut new_variable_map = copy_variable_map(variable_map);
        let mut new_struct_map = copy_struct_map(struct_map);

        self.body = self
            .body
            .resolve(&mut new_variable_map, &mut new_struct_map)?
            .to_owned()
            .into();
        self.condition = resolve_exp(&self.condition, &mut new_variable_map, struct_map)?;

        Ok(self)
    }
}

impl Resolve for WhileStatement {
    fn resolve(
        &mut self,
        variable_map: &mut BTreeMap<String, Variable>,
        struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<&mut Self> {
        let mut new_variable_map = copy_variable_map(variable_map);
        let mut new_struct_map = copy_struct_map(struct_map);

        self.condition = resolve_exp(&self.condition, &mut new_variable_map, struct_map)?;
        self.body = self
            .body
            .resolve(&mut new_variable_map, &mut new_struct_map)?
            .to_owned()
            .into();

        Ok(self)
    }
}

impl Resolve for BreakStatement {
    fn resolve(
        &mut self,
        _variable_map: &mut BTreeMap<String, Variable>,
        _struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<&mut Self> {
        Ok(self)
    }
}

impl Resolve for ContinueStatement {
    fn resolve(
        &mut self,
        _variable_map: &mut BTreeMap<String, Variable>,
        _struct_map: &mut BTreeMap<String, StructTableEntry>,
    ) -> Result<&mut Self> {
        Ok(self)
    }
}

fn resolve_exp(
    exp: &Expression,
    variable_map: &mut BTreeMap<String, Variable>,
    struct_map: &mut BTreeMap<String, StructTableEntry>,
) -> Result<Expression> {
    match exp.to_owned() {
        Expression::Assign(AssignExpression {
            op,
            ref lhs,
            rhs,
            _type,
        }) => {
            let resolved_lhs = resolve_exp(lhs, variable_map, struct_map)?;
            let resolved_rhs = resolve_exp(&rhs, variable_map, struct_map)?;

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
            let resolved_expr = resolve_exp(&expr, variable_map, struct_map)?;

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
            let resolved_lhs = resolve_exp(&lhs, variable_map, struct_map)?;
            let resolved_rhs = resolve_exp(&rhs, variable_map, struct_map)?;

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
            let resolved_condition = resolve_exp(&condition, variable_map, struct_map)?;
            let resolved_then_expr = resolve_exp(&then_expr, variable_map, struct_map)?;
            let resolved_else_expr = resolve_exp(&else_expr, variable_map, struct_map)?;

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
                    .map(|arg| resolve_exp(arg, variable_map, struct_map))
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
            let resolved_expr = resolve_exp(&expr, variable_map, struct_map)?;
            let resolved_type = resolve_type(&target_type, struct_map)?;

            Ok(Expression::Cast(CastExpression {
                target_type: resolved_type,
                expr: resolved_expr.into(),
                _type,
            }))
        }

        Expression::AddrOf(AddrOfExpression { expr, _type }) => {
            let resolved_expr = resolve_exp(&expr, variable_map, struct_map)?;

            Ok(Expression::AddrOf(AddrOfExpression {
                expr: resolved_expr.into(),
                _type,
            }))
        }

        Expression::Deref(DerefExpression { expr, _type }) => {
            let resolved_expr = resolve_exp(&expr, variable_map, struct_map)?;

            Ok(Expression::Deref(DerefExpression {
                expr: resolved_expr.into(),
                _type,
            }))
        }

        Expression::Subscript(SubscriptExpression { expr, index, _type }) => {
            let resolved_expr = resolve_exp(&expr, variable_map, struct_map)?;
            let resolved_index = resolve_exp(&index, variable_map, struct_map)?;

            Ok(Expression::Subscript(SubscriptExpression {
                expr: resolved_expr.into(),
                index: resolved_index.into(),
                _type,
            }))
        }

        Expression::Literal(LiteralExpression { name, value, _type }) => {
            let resolved_init = resolve_init(&value, variable_map, struct_map)?;
            Ok(Expression::Literal(LiteralExpression {
                name,
                value: resolved_init.into(),
                _type,
            }))
        }

        Expression::String(StringExpression { value, _type }) => {
            Ok(Expression::String(StringExpression { value, _type }))
        }

        Expression::Sizeof(SizeofExpression { expr, _type }) => {
            let resolved_expr = resolve_exp(&expr, variable_map, struct_map)?;
            let resolved_type = resolve_type(&_type, struct_map)?;
            Ok(Expression::Sizeof(SizeofExpression {
                expr: resolved_expr.into(),
                _type: resolved_type,
            }))
        }

        Expression::Dot(DotExpression {
            structure,
            member,
            _type,
        }) => {
            let resolved_structure = resolve_exp(&structure, variable_map, struct_map)?;
            Ok(Expression::Dot(DotExpression {
                structure: resolved_structure.into(),
                member,
                _type,
            }))
        }

        Expression::Arrow(ArrowExpression {
            pointer,
            member,
            _type,
        }) => {
            let resolved_pointer = resolve_exp(&pointer, variable_map, struct_map)?;
            Ok(Expression::Arrow(ArrowExpression {
                pointer: resolved_pointer.into(),
                member,
                _type,
            }))
        }

        Expression::SizeofT(SizeofTExpression { t, _type }) => {
            let resolved_type = resolve_type(&t, struct_map)?;
            Ok(Expression::SizeofT(SizeofTExpression {
                t: resolved_type,
                _type,
            }))
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

fn resolve_for_init<'a>(
    init: &'a mut ForInit,
    variable_map: &'a mut BTreeMap<String, Variable>,
    struct_map: &'a mut BTreeMap<String, StructTableEntry>,
) -> Result<ForInit> {
    match init {
        ForInit::Expression(expr) => {
            let resolved_expr = resolve_optional_expr(expr, variable_map, struct_map)?;
            Ok(ForInit::Expression(resolved_expr))
        }
        ForInit::Declaration(ref mut decl) => {
            let resolved_decl = decl.resolve(variable_map, struct_map)?;
            Ok(ForInit::Declaration(resolved_decl.to_owned()))
        }
    }
}

fn resolve_optional_expr(
    expr: &Option<Expression>,
    variable_map: &mut BTreeMap<String, Variable>,
    struct_map: &mut BTreeMap<String, StructTableEntry>,
) -> Result<Option<Expression>> {
    if expr.is_some() {
        let resolved_expr = resolve_exp(expr.as_ref().unwrap(), variable_map, struct_map)?;
        Ok(Some(resolved_expr))
    } else {
        Ok(None)
    }
}

fn resolve_optional_block_item(
    block_item: &mut Option<BlockItem>,
    variable_map: &mut BTreeMap<String, Variable>,
    struct_map: &mut BTreeMap<String, StructTableEntry>,
) -> Result<Option<BlockItem>> {
    if block_item.is_some() {
        let resolved_block_item = block_item
            .as_mut()
            .unwrap()
            .resolve(variable_map, struct_map)?;
        Ok(Some(resolved_block_item.to_owned()))
    } else {
        Ok(None)
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

fn resolve_type(
    type_specifier: &Type,
    struct_map: &mut BTreeMap<String, StructTableEntry>,
) -> Result<Type> {
    match type_specifier {
        Type::Struct { tag } => {
            if struct_map.contains_key(tag) {
                let unique_tag = struct_map.get(tag).cloned().unwrap().name.clone();
                Ok(Type::Struct { tag: unique_tag })
            } else {
                bail!("Specified an undeclared structure tag.")
            }
        }

        Type::Pointer(referenced) => {
            let resolved_referenced = resolve_type(referenced, struct_map)?;
            Ok(Type::Pointer(Box::new(resolved_referenced)))
        }

        Type::Array { element, size } => {
            let resolved_element = resolve_type(element, struct_map)?;
            Ok(Type::Array {
                element: Box::new(resolved_element),
                size: *size,
            })
        }

        Type::Func { params, ret } => {
            let mut resolved_params = vec![];
            for param in params {
                resolved_params.push(resolve_type(param, struct_map)?);
            }
            let resolved_ret = resolve_type(ret, struct_map)?;
            Ok(Type::Func {
                params: resolved_params,
                ret: Box::new(resolved_ret),
            })
        }

        _ => Ok(type_specifier.clone()),
    }
}
