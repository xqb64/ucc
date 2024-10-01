use crate::{
    ir::make_temporary,
    lexer::Const,
    parser::{
        AddrOfExpression, ArrowExpression, AssignExpression, BinaryExpression,
        BinaryExpressionKind, BlockItem, BlockStatement, CallExpression, CastExpression,
        ConditionalExpression, ConstantExpression, Declaration, DerefExpression, DoWhileStatement,
        DotExpression, Expression, ExpressionStatement, ForInit, ForStatement, FunctionDeclaration,
        IfStatement, Initializer, ProgramStatement, ReturnStatement, SizeofExpression,
        SizeofTExpression, Statement, StorageClass, StringExpression, StructDeclaration,
        SubscriptExpression, Type, UnaryExpression, UnaryExpressionKind, VariableDeclaration,
        VariableExpression, WhileStatement,
    },
};
use anyhow::{bail, Result};
use std::{cmp::max, collections::HashMap, sync::Mutex};

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub _type: Type,
    pub attrs: IdentifierAttrs,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructEntry {
    pub alignment: usize,
    pub size: usize,
    pub members: Vec<MemberEntry>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberEntry {
    pub name: String,
    pub _type: Type,
    pub offset: usize,
}

lazy_static::lazy_static! {
    pub static ref SYMBOL_TABLE: Mutex<HashMap<String, Symbol>> = Mutex::new(HashMap::new());
    pub static ref TYPE_TABLE: Mutex<HashMap<String, StructEntry>> = Mutex::new(HashMap::new());
    pub static ref CURRENT_FN_RETURNS_ON_STACK: Mutex<isize> = Mutex::new(0);
}

pub trait Typecheck {
    fn typecheck(&mut self) -> Result<&mut Self>
    where
        Self: Sized;
}

impl Typecheck for BlockItem {
    fn typecheck(&mut self) -> Result<&mut Self> {
        match self {
            BlockItem::Declaration(decl) => {
                decl.typecheck()?;
                Ok(self)
            }
            BlockItem::Statement(stmt) => {
                stmt.typecheck()?;
                Ok(self)
            }
        }
    }
}

impl Typecheck for Declaration {
    fn typecheck(&mut self) -> Result<&mut Self> {
        match self {
            Declaration::Variable(var_decl) => {
                var_decl.typecheck()?;
                Ok(self)
            }
            Declaration::Function(func_decl) => {
                func_decl.typecheck()?;
                Ok(self)
            }
            Declaration::Struct(struct_decl) => {
                struct_decl.typecheck()?;
                Ok(self)
            }
        }
    }
}

fn alignment(t: &Type) -> usize {
    match t {
        Type::Char | Type::UChar | Type::SChar => 1,
        Type::Int | Type::Uint => 4,
        Type::Double | Type::Long | Type::Ulong | Type::Pointer(_) => 8,
        Type::Struct { tag } => TYPE_TABLE.lock().unwrap()[tag].alignment,
        Type::Array { element, size: _ } => alignment(element),
        Type::Dummy | Type::Void | Type::Func { .. } => unreachable!(),
    }
}

fn round_up(value: usize, alignment: usize) -> usize {
    (value + alignment - 1) & !(alignment - 1)
}

fn validate_struct_definition(definition: &mut StructDeclaration) -> Result<()> {
    use std::collections::HashSet;

    let tag = &definition.tag;

    if TYPE_TABLE.lock().unwrap().contains_key(tag) {
        panic!("Structure was already declared");
    } else {
        let mut member_names = HashSet::new();

        for member in &definition.members {
            let member_name = &member.name;

            if member_names.contains(member_name) {
                bail!(
                    "Duplicate declaration of member {} in structure {}",
                    member_name,
                    tag
                );
            } else {
                member_names.insert(member_name.clone());
            }

            validate_type_specifier(&member._type)?;

            match &member._type {
                Type::Func { .. } => {
                    bail!("Can't declare structure member with function type");
                }
                _ => {
                    if !is_complete(&member._type) {
                        bail!("Cannot declare structure member with incomplete type");
                    }
                }
            }
        }
    }

    Ok(())
}

impl Typecheck for StructDeclaration {
    fn typecheck(&mut self) -> Result<&mut Self>
    where
        Self: Sized,
    {
        if self.members.is_empty() {
            return Ok(self);
        }

        validate_struct_definition(self)?;

        let mut member_entries = vec![];
        let mut struct_size = 0;
        let mut struct_alignment = 1;

        for member in &self.members {
            let member_alignment = alignment(&member._type);
            let member_offset = round_up(struct_size, member_alignment);
            let m = MemberEntry {
                name: member.name.clone(),
                _type: member._type.clone(),
                offset: member_offset as usize,
            };

            member_entries.push(m);

            struct_alignment = max(struct_alignment, member_alignment);
            struct_size = member_offset + get_size_of_type(&member._type);
        }

        struct_size = round_up(struct_size, struct_alignment);
        let s = StructEntry {
            alignment: struct_alignment,
            size: struct_size,
            members: member_entries,
        };

        TYPE_TABLE.lock().unwrap().insert(self.tag.clone(), s);

        Ok(self)
    }
}

impl Typecheck for VariableDeclaration {
    fn typecheck(&mut self) -> Result<&mut Self> {
        if self._type == Type::Void {
            bail!("Variable declared with void type");
        } else {
            validate_type_specifier(&self._type)?;
        }

        match self.is_global {
            true => {
                if !is_complete(&self._type) && self.storage_class != Some(StorageClass::Extern) {
                    bail!("Variable declared with incomplete type");
                }

                let default_init = if self.storage_class == Some(StorageClass::Extern) {
                    InitialValue::NoInitializer
                } else {
                    InitialValue::Tentative
                };

                let static_init = match &self.init {
                    Some(init) => to_static_init(init, &self._type)?,
                    None => default_init,
                };

                let is_global = self.storage_class != Some(StorageClass::Static);

                let old_decl = SYMBOL_TABLE.lock().unwrap().get(&self.name).cloned();

                let check_against_previous = |old_d: &Symbol| -> Result<(bool, InitialValue)> {
                    if old_d._type != self._type {
                        bail!("Variable redeclaration with different type");
                    }

                    match &old_d.attrs {
                        IdentifierAttrs::StaticAttr {
                            initial_value: prev_init,
                            global: prev_global,
                        } => {
                            let global = if self.storage_class == Some(StorageClass::Extern) {
                                *prev_global
                            } else if is_global == *prev_global {
                                is_global
                            } else {
                                bail!("Conflicting variable linkage");
                            };

                            let init = match (&prev_init, &static_init) {
                                (InitialValue::Initial(_), InitialValue::Initial(_)) => {
                                    bail!("conflicting file-scope variable initializers")
                                }
                                (InitialValue::Initial(_), _) => prev_init,
                                (
                                    InitialValue::Tentative,
                                    InitialValue::Tentative | InitialValue::NoInitializer,
                                ) => &InitialValue::Tentative,
                                (_, InitialValue::Initial(_)) => &static_init,
                                (InitialValue::NoInitializer, _) => &static_init,
                            };

                            Ok((global, init.to_owned()))
                        }
                        _ => {
                            unreachable!()
                        }
                    }
                };

                let (global, init) = match old_decl {
                    Some(old_d) => check_against_previous(&old_d)?,
                    None => (is_global, static_init),
                };

                let symbol = Symbol {
                    _type: self._type.clone(),
                    attrs: IdentifierAttrs::StaticAttr {
                        initial_value: init,
                        global,
                    },
                };

                SYMBOL_TABLE
                    .lock()
                    .unwrap()
                    .insert(self.name.clone(), symbol);

                self.init = optionally_typecheck_init(&self.init, &self._type)?;

                Ok(self)
            }
            false => {
                if !is_complete(&self._type) {
                    bail!("Variable declared with incomplete type");
                }

                match self.storage_class {
                    Some(StorageClass::Extern) => {
                        if self.init.is_some() {
                            bail!("Extern variable with initializer");
                        }

                        // if an extern local var is already in the symbol table, don't need to add it again
                        let symbol = SYMBOL_TABLE.lock().unwrap().get(&self.name).cloned();

                        match symbol {
                            Some(sym) => {
                                if sym._type != self._type {
                                    bail!("Variable redeclaration with different type");
                                }
                            }
                            None => {
                                let symbol = Symbol {
                                    _type: self._type.clone(),
                                    attrs: IdentifierAttrs::StaticAttr {
                                        initial_value: InitialValue::NoInitializer,
                                        global: true,
                                    },
                                };

                                SYMBOL_TABLE
                                    .lock()
                                    .unwrap()
                                    .insert(self.name.clone(), symbol);
                            }
                        }

                        self.init = optionally_typecheck_init(&self.init, &self._type)?;

                        Ok(self)
                    }
                    Some(StorageClass::Static) => {
                        let zero_init = InitialValue::Initial(vec![StaticInit::Zero(
                            get_size_of_type(&self._type),
                        )]);
                        let static_init = match &self.init {
                            Some(init) => to_static_init(init, &self._type)?,
                            None => zero_init,
                        };

                        let symbol = Symbol {
                            _type: self._type.clone(),
                            attrs: IdentifierAttrs::StaticAttr {
                                initial_value: static_init,
                                global: false,
                            },
                        };
                        SYMBOL_TABLE
                            .lock()
                            .unwrap()
                            .insert(self.name.clone(), symbol);

                        self.init = optionally_typecheck_init(&self.init, &self._type)?;

                        Ok(self)
                    }
                    None => {
                        let symbol = Symbol {
                            _type: self._type.clone(),
                            attrs: IdentifierAttrs::LocalAttr,
                        };
                        SYMBOL_TABLE
                            .lock()
                            .unwrap()
                            .insert(self.name.clone(), symbol);

                        self.init = optionally_typecheck_init(&self.init, &self._type)?;

                        Ok(self)
                    }
                }
            }
        }
    }
}

fn const2type(konst: &Const, t: &Type) -> StaticInit {
    match konst {
        Const::Int(i) => match t {
            Type::Int => StaticInit::Int(*i),
            Type::Uint => StaticInit::UInt(*i as u32),
            Type::Long => StaticInit::Long(*i as i64),
            Type::Ulong => StaticInit::ULong(*i as u64),
            Type::Double => StaticInit::Double(*i as f64),
            Type::Pointer(_) => StaticInit::ULong(*i as u64),
            Type::Char | Type::SChar => StaticInit::Char(*i),
            Type::UChar => StaticInit::UChar(*i as u32),
            _ => unreachable!(),
        },
        Const::Long(l) => match t {
            Type::Int => StaticInit::Int(*l as i32),
            Type::Uint => StaticInit::UInt(*l as u32),
            Type::Long => StaticInit::Long(*l),
            Type::Ulong => StaticInit::ULong(*l as u64),
            Type::Double => StaticInit::Double(*l as f64),
            Type::Pointer(_) => StaticInit::ULong(*l as u64),
            Type::Char | Type::SChar => StaticInit::Char(*l as i32),
            Type::UChar => StaticInit::UChar(*l as u32),
            _ => unreachable!(),
        },
        Const::UInt(u) => match t {
            Type::Int => StaticInit::Int(*u as i32),
            Type::Uint => StaticInit::UInt(*u),
            Type::Long => StaticInit::Long(*u as i64),
            Type::Ulong => StaticInit::ULong(*u as u64),
            Type::Double => StaticInit::Double(*u as f64),
            Type::Pointer(_) => StaticInit::ULong(*u as u64),
            Type::Char | Type::SChar => StaticInit::Char(*u as i32),
            Type::UChar => StaticInit::UChar(*u),
            _ => unreachable!(),
        },
        Const::ULong(ul) => match t {
            Type::Int => StaticInit::Int(*ul as i32),
            Type::Uint => StaticInit::UInt(*ul as u32),
            Type::Long => StaticInit::Long(*ul as i64),
            Type::Ulong => StaticInit::ULong(*ul),
            Type::Double => StaticInit::Double(*ul as f64),
            Type::Pointer(_) => StaticInit::ULong(*ul),
            Type::Char | Type::SChar => StaticInit::Char(*ul as i32),
            Type::UChar => StaticInit::UChar(*ul as u32),
            _ => unreachable!(),
        },
        Const::Double(d) => match t {
            Type::Int => StaticInit::Int(*d as i32),
            Type::Uint => StaticInit::UInt(*d as u32),
            Type::Long => StaticInit::Long(*d as i64),
            Type::Ulong => StaticInit::ULong(*d as u64),
            Type::Double => StaticInit::Double(*d),
            Type::Pointer(_) => StaticInit::ULong(*d as u64),
            Type::Char | Type::SChar => StaticInit::Char(*d as i32),
            Type::UChar => StaticInit::UChar(*d as u32),
            _ => unreachable!(),
        },
        _ => todo!(),
    }
}

fn optionally_typecheck_init(init: &Option<Initializer>, t: &Type) -> Result<Option<Initializer>> {
    match init {
        Some(init) => {
            let typechecked_init = typecheck_init(t, init)?;
            Ok(Some(typechecked_init))
        }
        None => Ok(None),
    }
}

fn static_init_helper(init: &Initializer, t: &Type) -> Result<Vec<StaticInit>> {
    match (t, init) {
        (Type::Pointer(_), Initializer::Single(_, Expression::String(string_expr))) => {
            let str_id = format!("string.{}", make_temporary());
            let symbol = Symbol {
                _type: Type::Array {
                    element: Box::new(Type::Char),
                    size: string_expr.value.len() + 1,
                },
                attrs: IdentifierAttrs::ConstantAttr(StaticInit::String(
                    string_expr.value.to_owned(),
                    true,
                )),
            };
            SYMBOL_TABLE.lock().unwrap().insert(str_id.clone(), symbol);
            Ok(vec![StaticInit::Pointer(str_id)])
        }
        (Type::Struct { tag }, Initializer::Compound(_name, _type, compound_init)) => {
            let struct_def = TYPE_TABLE.lock().unwrap().get(tag).unwrap().clone();

            if compound_init.len() > struct_def.members.len() {
                bail!("Too many initializers");
            }

            let mut current_offset = 0;
            let mut i = 0;

            let mut static_inits = vec![];

            for init_elem in compound_init {
                println!("current init_elem: {:?}", init_elem);
                let member = struct_def.members[i].clone();
                if member.offset != current_offset {
                    static_inits.push(StaticInit::Zero(member.offset - current_offset));
                }

                let more_static_inits = static_init_helper(init_elem, &member._type)?;
                static_inits.extend(more_static_inits);

                current_offset = member.offset + get_size_of_type(&member._type);

                i += 1;
            }

            if struct_def.size != current_offset {
                static_inits.push(StaticInit::Zero(struct_def.size - current_offset));
            }

            Ok(static_inits)
        }
        (Type::Struct { .. }, Initializer::Single(_, _)) => {
            bail!("Single initializer for struct type");
        }
        (Type::Array { element, size }, Initializer::Single(_, expr)) => {
            if let Expression::String(string_expr) = expr {
                if !is_char_type(element) {
                    bail!("Can't initialize array with non-char type");
                }

                let len_diff = size - string_expr.value.len();
                match len_diff {
                    0 => Ok(vec![StaticInit::String(
                        string_expr.value.to_owned(),
                        false,
                    )]),
                    1 => Ok(vec![StaticInit::String(string_expr.value.to_owned(), true)]),
                    n if n > 0 => {
                        let mut initializers =
                            vec![StaticInit::String(string_expr.value.to_owned(), true)];
                        initializers.push(StaticInit::Zero(n - 1));
                        Ok(initializers)
                    }
                    _ => bail!("String too long for array"),
                }
            } else {
                bail!("Can't initialize array with non-string");
            }
        }
        (_, Initializer::Single(_, Expression::Constant(ConstantExpression { value, _type }))) => {
            if matches!(
                value,
                Const::Int(0)
                    | Const::Long(0)
                    | Const::UInt(0)
                    | Const::ULong(0)
                    | Const::Double(0.0)
            ) {
                Ok(vec![StaticInit::Zero(get_size_of_type(t))])
            } else {
                Ok(vec![const2type(value, t)])
            }
        }
        (Type::Pointer { .. }, _) => bail!("InvalidPointerInitializer"),
        (_, Initializer::Single(_, _)) => bail!("StaticInitError::NonConstantInitializer"),
        (Type::Array { element, size }, Initializer::Compound(_, _type, inits)) => {
            let mut static_inits = Vec::with_capacity(inits.len());
            for init in inits.iter() {
                let static_init = static_init_helper(init, element)?;
                static_inits.extend(static_init);
            }

            let padding_size = size.saturating_sub(inits.len());
            let padding = match padding_size.cmp(&0) {
                std::cmp::Ordering::Greater => {
                    vec![StaticInit::Zero(get_size_of_type(element) * padding_size)]
                }
                std::cmp::Ordering::Equal => vec![],
                std::cmp::Ordering::Less => bail!("Too many initializers"),
            };

            static_inits.extend(padding);
            Ok(static_inits)
        }
        (_, Initializer::Compound(_, _, _)) => {
            bail!("Compound initializer for scalar type");
        }
    }
}

fn to_static_init(init: &Initializer, t: &Type) -> Result<InitialValue> {
    let init_list = static_init_helper(init, t)?;
    Ok(InitialValue::Initial(init_list))
}

impl Typecheck for FunctionDeclaration {
    fn typecheck(&mut self) -> Result<&mut Self> {
        if self._type == Type::Void {
            bail!("Variable declared with void type");
        } else {
            validate_type_specifier(&self._type)?;
        }

        let adjust_param_type = |t: Type| -> Result<Type> {
            match t {
                Type::Array { element, .. } => Ok(Type::Pointer(element)),
                Type::Void => bail!("Function parameter has void type"),
                t => Ok(t),
            }
        };

        let (param_ts, _, fun_type) = match self._type.clone() {
            Type::Func { params, ret } => {
                if let Type::Array { .. } = *ret {
                    bail!("Function return type is an array");
                }
                let param_types: Vec<Type> = params
                    .into_iter()
                    .map(adjust_param_type)
                    .collect::<Result<Vec<_>>>()?;
                (
                    param_types.clone(),
                    ret.clone(),
                    Type::Func {
                        params: param_types.clone(),
                        ret: ret.clone(),
                    },
                )
            }
            _ => bail!("Function has non-function type"),
        };

        let has_body = self.body.is_some();

        if has_body {
            for param in param_ts.iter() {
                if !is_complete(param) {
                    bail!("Function parameter has incomplete type");
                }
            }

            let ret_type = match &self._type {
                Type::Func { ret, .. } => ret,
                _ => unreachable!(),
            };

            match &**ret_type {
                Type::Struct { tag } => {
                    if !TYPE_TABLE.lock().unwrap().contains_key(tag) {
                        bail!("Function return type is incomplete");
                    }
                }
                _ => {}
            }
        }

        let global = self.storage_class != Some(StorageClass::Static);

        // Helper function to reconcile current and previous declarations
        let check_against_previous = |prev: &Symbol| -> Result<(bool, bool)> {
            if prev._type != fun_type {
                bail!("RedeclaredFunction");
            }

            match &prev.attrs {
                IdentifierAttrs::FuncAttr {
                    global: prev_global,
                    defined: prev_defined,
                } => {
                    if *prev_defined && has_body {
                        bail!("FunctionDefinedTwice");
                    } else if *prev_global && self.storage_class == Some(StorageClass::Static) {
                        bail!("StaticFunctionDeclarationAfterNonStatic");
                    }

                    let defined = has_body || *prev_defined;
                    Ok((defined, *prev_global))
                }
                _ => bail!("Symbol has function type but not function attributes"),
            }
        };

        let old_decl = SYMBOL_TABLE.lock().unwrap().get(&self.name).cloned();
        let (defined, global) = match old_decl {
            Some(old_d) => check_against_previous(&old_d)?,
            None => (has_body, global),
        };

        SYMBOL_TABLE.lock().unwrap().insert(
            self.name.clone(),
            Symbol {
                _type: fun_type,
                attrs: IdentifierAttrs::FuncAttr { global, defined },
            },
        );

        if has_body {
            for (param, param_t) in self.params.iter().zip(param_ts) {
                let symbol = Symbol {
                    _type: param_t,
                    attrs: IdentifierAttrs::LocalAttr,
                };
                SYMBOL_TABLE.lock().unwrap().insert(param.clone(), symbol);
            }
        }

        optionally_typecheck_block_item(&mut self.body)?;

        Ok(self)
    }
}

impl Typecheck for Statement {
    fn typecheck(&mut self) -> Result<&mut Self> {
        match self {
            Statement::Program(ProgramStatement { block_items: stmts }) => {
                for block_item in stmts {
                    block_item.typecheck()?;
                }

                Ok(self)
            }
            Statement::Expression(ExpressionStatement { expr }) => {
                *expr = typecheck_and_convert(expr)?;

                Ok(self)
            }
            Statement::Compound(BlockStatement { stmts }) => {
                for stmt in stmts {
                    stmt.typecheck()?;
                }

                Ok(self)
            }
            Statement::If(IfStatement {
                condition,
                then_branch,
                else_branch,
            }) => {
                *condition = typecheck_scalar(condition)?;

                then_branch.typecheck()?;
                optionally_typecheck_block_item(else_branch)?;

                Ok(self)
            }
            Statement::While(WhileStatement {
                condition,
                body,
                label: _,
            }) => {
                *condition = typecheck_scalar(condition)?;

                body.typecheck()?;

                Ok(self)
            }
            Statement::DoWhile(DoWhileStatement {
                condition,
                body,
                label: _,
            }) => {
                *condition = typecheck_scalar(condition)?;
                body.typecheck()?;

                Ok(self)
            }
            Statement::For(ForStatement {
                init,
                condition,
                post,
                body,
                label: _,
            }) => {
                if let ForInit::Declaration(decl) = init {
                    if decl.storage_class.is_some() {
                        bail!("Storage class specifier in for loop initializer");
                    }
                }

                *init = optionally_typecheck_for_init(init)?;
                *condition = optionally_typecheck_scalar(condition)?;
                *post = optionally_typecheck_expression(post)?;

                body.typecheck()?;

                Ok(self)
            }
            Statement::Return(ReturnStatement {
                expr: Some(expression),
                target_type,
                belongs_to,
            }) => {
                if target_type == &Some(Type::Void) {
                    bail!("Return statement with expression in void function");
                } else {
                    let ret_type = SYMBOL_TABLE
                        .lock()
                        .unwrap()
                        .get(belongs_to)
                        .cloned()
                        .unwrap()
                        ._type;
                    let target_type = match ret_type {
                        Type::Func { ret, .. } => ret,
                        _ => unreachable!(),
                    };
                    let typechecked_expr = typecheck_and_convert(expression)?;
                    let converted_expr = convert_by_assignment(&typechecked_expr, &target_type)?;
                    *expression = converted_expr;
                }
                Ok(self)
            }
            Statement::Return(ReturnStatement {
                expr: None,
                target_type,
                belongs_to: _,
            }) => {
                if target_type == &Some(Type::Void) {
                    Ok(self)
                } else {
                    bail!("Return statement with no expression in non-void function");
                }
            }
            Statement::Break(_) | Statement::Continue(_) | Statement::Null => Ok(self),
        }
    }
}

fn optionally_typecheck_expression(expr: &mut Option<Expression>) -> Result<Option<Expression>> {
    match expr {
        Some(expr) => {
            *expr = typecheck_and_convert(expr)?;
            Ok(Some(expr.to_owned()))
        }
        None => Ok(None),
    }
}

fn optionally_typecheck_for_init(init: &mut ForInit) -> Result<ForInit> {
    match init {
        ForInit::Declaration(decl) => {
            decl.typecheck()?;
        }
        ForInit::Expression(Some(expr)) => {
            *expr = typecheck_and_convert(expr)?;
        }
        _ => {}
    }
    Ok(init.to_owned())
}

fn typecheck_init(target_type: &Type, init: &Initializer) -> Result<Initializer> {
    match (target_type, init) {
        (
            Type::Array { element, size },
            Initializer::Single(name, Expression::String(StringExpression { value, _type })),
        ) => {
            if !is_char_type(element) {
                bail!("Can't initialize array with non-char type");
            }

            if value.len() > *size {
                bail!("String too long for array");
            }

            Ok(Initializer::Single(
                name.to_owned(),
                Expression::String(StringExpression {
                    value: value.clone(),
                    _type: target_type.clone(),
                }),
            ))
        }
        (Type::Struct { tag }, Initializer::Compound(name, _type, compound_init)) => {
            let struct_def = TYPE_TABLE.lock().unwrap().get(tag).unwrap().clone();

            if compound_init.len() > struct_def.members.len() {
                bail!("Too many initializers");
            }

            let mut i = 0;
            let mut typechecked_inits = vec![];

            for init_elem in compound_init.iter() {
                let member = &struct_def.members[i];
                let typechecked_init = typecheck_init(&member._type, init_elem)?;
                typechecked_inits.push(typechecked_init);
                i += 1;
            }

            while i < struct_def.members.len() {
                let member = &struct_def.members[i];
                typechecked_inits.push(zero_initializer(&member._type));
                i += 1;
            }

            Ok(Initializer::Compound(
                name.clone(),
                Type::Struct { tag: tag.clone() },
                typechecked_inits,
            ))
        }
        (_, Initializer::Single(name, expr)) => {
            let typechecked_expr = typecheck_and_convert(expr)?;
            let converted_expr = convert_by_assignment(&typechecked_expr, target_type)?;
            Ok(Initializer::Single(name.clone(), converted_expr))
        }
        (Type::Array { element, size }, Initializer::Compound(name, _type, inits)) => {
            if inits.len() > *size {
                bail!("Too many initializers");
            }

            let mut typechecked_inits = vec![];

            for init in inits.iter() {
                let typechecked_init = typecheck_init(element, init)?;
                typechecked_inits.push(typechecked_init);
            }

            while typechecked_inits.len() < *size {
                typechecked_inits.push(zero_initializer(element));
            }

            Ok(Initializer::Compound(
                name.clone(),
                *element.clone(),
                typechecked_inits,
            ))
        }
        _ => {
            bail!("can't init a scalar object iwth a compound initializer");
        }
    }
}

fn optionally_typecheck_block_item(
    block_item: &mut Option<BlockItem>,
) -> Result<&mut Option<BlockItem>> {
    match block_item {
        Some(item) => {
            item.typecheck()?;
        }
        None => {}
    }
    Ok(block_item)
}

fn zero_initializer(t: &Type) -> Initializer {
    match t {
        Type::Int => Initializer::Single(
            String::new(),
            Expression::Constant(ConstantExpression {
                value: Const::Int(0),
                _type: Type::Int,
            }),
        ),
        Type::Uint => Initializer::Single(
            String::new(),
            Expression::Constant(ConstantExpression {
                value: Const::UInt(0),
                _type: Type::Uint,
            }),
        ),
        Type::Long => Initializer::Single(
            String::new(),
            Expression::Constant(ConstantExpression {
                value: Const::Long(0),
                _type: Type::Long,
            }),
        ),
        Type::Ulong => Initializer::Single(
            String::new(),
            Expression::Constant(ConstantExpression {
                value: Const::ULong(0),
                _type: Type::Ulong,
            }),
        ),
        Type::Double => Initializer::Single(
            String::new(),
            Expression::Constant(ConstantExpression {
                value: Const::Double(0.0),
                _type: Type::Double,
            }),
        ),
        Type::Char | Type::SChar => Initializer::Single(
            String::new(),
            Expression::Constant(ConstantExpression {
                value: Const::Char(0),
                _type: Type::Char,
            }),
        ),
        Type::UChar => Initializer::Single(
            String::new(),
            Expression::Constant(ConstantExpression {
                value: Const::UChar(0),
                _type: Type::UChar,
            }),
        ),
        Type::Pointer(_) => Initializer::Single(
            String::new(),
            Expression::Constant(ConstantExpression {
                value: Const::ULong(0),
                _type: Type::Int,
            }),
        ),
        Type::Array { element, size } => {
            let mut inits = vec![];

            for _ in 0..*size {
                inits.push(zero_initializer(element));
            }

            Initializer::Compound(String::new(), *element.clone(), inits)
        }
        Type::Struct { tag } => {
            let struct_def = TYPE_TABLE.lock().unwrap().get(tag).unwrap().clone();
            let mut inits = vec![];

            for member in struct_def.members.iter() {
                inits.push(zero_initializer(&member._type));
            }

            Initializer::Compound(String::new(), Type::Struct { tag: tag.clone() }, inits)
        }
        _ => unreachable!(),
    }
}

fn typecheck_logical(
    kind: &BinaryExpressionKind,
    lhs: &Expression,
    rhs: &Expression,
) -> Result<Expression> {
    let typed_lhs = typecheck_scalar(lhs)?;
    let typed_rhs = typecheck_scalar(rhs)?;

    Ok(Expression::Binary(BinaryExpression {
        kind: kind.clone(),
        lhs: Box::new(typed_lhs),
        rhs: Box::new(typed_rhs),
        _type: Type::Int,
    }))
}

fn typecheck_addition(lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    let typed_lhs = typecheck_and_convert(lhs)?;
    let typed_rhs = typecheck_and_convert(rhs)?;

    if is_arithmetic(get_type(&typed_lhs)) && is_arithmetic(get_type(&typed_rhs)) {
        let common_type = get_common_type(get_type(&typed_lhs), get_type(&typed_rhs));

        let converted_lhs = convert_to(&typed_lhs, common_type);
        let converted_rhs = convert_to(&typed_rhs, common_type);

        Ok(Expression::Binary(BinaryExpression {
            kind: BinaryExpressionKind::Add,
            lhs: Box::new(converted_lhs),
            rhs: Box::new(converted_rhs),
            _type: common_type.to_owned(),
        }))
    } else if is_ptr_to_complete(get_type(&typed_lhs)) && is_integer_type(get_type(&typed_rhs)) {
        let converted_rhs = convert_to(&typed_rhs, &Type::Long);

        Ok(Expression::Binary(BinaryExpression {
            kind: BinaryExpressionKind::Add,
            lhs: Box::new(typed_lhs.clone()),
            rhs: Box::new(converted_rhs),
            _type: get_type(&typed_lhs).to_owned(),
        }))
    } else if is_ptr_to_complete(get_type(&typed_rhs)) && is_integer_type(get_type(&typed_lhs)) {
        let converted_lhs = convert_to(&typed_lhs, &Type::Long);

        Ok(Expression::Binary(BinaryExpression {
            kind: BinaryExpressionKind::Add,
            lhs: Box::new(converted_lhs),
            rhs: Box::new(typed_rhs.clone()),
            _type: get_type(&typed_rhs).to_owned(),
        }))
    } else {
        bail!("Invalid operands for addition");
    }
}

fn typecheck_subtraction(lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    let typed_lhs = typecheck_and_convert(lhs)?;
    let typed_rhs = typecheck_and_convert(rhs)?;

    let t1 = get_type(&typed_lhs);
    let t2 = get_type(&typed_rhs);

    if is_arithmetic(t1) && is_arithmetic(t2) {
        let common_type = get_common_type(t1, t2);
        let converted_lhs = convert_to(&typed_lhs, common_type);
        let converted_rhs = convert_to(&typed_rhs, common_type);

        Ok(Expression::Binary(BinaryExpression {
            kind: BinaryExpressionKind::Sub,
            lhs: Box::new(converted_lhs),
            rhs: Box::new(converted_rhs),
            _type: common_type.to_owned(),
        }))
    } else if is_ptr_to_complete(t1) && is_integer_type(t2) {
        let converted_rhs = convert_to(&typed_rhs, &Type::Long);

        Ok(Expression::Binary(BinaryExpression {
            kind: BinaryExpressionKind::Sub,
            lhs: Box::new(typed_lhs.clone()),
            rhs: Box::new(converted_rhs),
            _type: t1.to_owned(),
        }))
    } else if is_ptr_to_complete(t1) && get_type(&typed_lhs) == get_type(&typed_rhs) {
        Ok(Expression::Binary(BinaryExpression {
            kind: BinaryExpressionKind::Sub,
            lhs: Box::new(typed_lhs.clone()),
            rhs: Box::new(typed_rhs.clone()),
            _type: Type::Long,
        }))
    } else {
        bail!("Invalid operands for subtraction");
    }
}

fn typecheck_multiplicative(
    kind: &BinaryExpressionKind,
    lhs: &Expression,
    rhs: &Expression,
) -> Result<Expression> {
    let typed_lhs = typecheck_and_convert(lhs)?;
    let typed_rhs = typecheck_and_convert(rhs)?;

    let t1 = get_type(&typed_lhs);
    let t2 = get_type(&typed_rhs);

    if is_arithmetic(t1) && is_arithmetic(t2) {
        let common_type = get_common_type(t1, t2);
        let converted_lhs = convert_to(&typed_lhs, common_type);
        let converted_rhs = convert_to(&typed_rhs, common_type);

        match kind {
            BinaryExpressionKind::Rem if common_type == &Type::Double => {
                bail!("remainder operator cannot be applied to floating-point types");
            }
            BinaryExpressionKind::Mul | BinaryExpressionKind::Div | BinaryExpressionKind::Rem => {
                Ok(Expression::Binary(BinaryExpression {
                    kind: kind.clone(),
                    lhs: Box::new(converted_lhs),
                    rhs: Box::new(converted_rhs),
                    _type: common_type.to_owned(),
                }))
            }
            _ => unreachable!(),
        }
    } else {
        bail!("Invalid operands for multiplication");
    }
}

fn typecheck_equality(
    kind: &BinaryExpressionKind,
    lhs: &Expression,
    rhs: &Expression,
) -> Result<Expression> {
    let typed_lhs = typecheck_and_convert(lhs)?;
    let typed_rhs = typecheck_and_convert(rhs)?;

    let t1 = get_type(&typed_lhs);
    let t2 = get_type(&typed_rhs);

    let common_type = if is_pointer_type(t1) || is_pointer_type(t2) {
        get_common_ptr_type(&typed_lhs, &typed_rhs)?
    } else if is_arithmetic(t1) && is_arithmetic(t2) {
        get_common_type(t1, t2).to_owned()
    } else {
        bail!("Invalid operands for equality operator");
    };

    let converted_lhs = convert_to(&typed_lhs, &common_type);
    let converted_rhs = convert_to(&typed_rhs, &common_type);

    Ok(Expression::Binary(BinaryExpression {
        kind: kind.clone(),
        lhs: Box::new(converted_lhs),
        rhs: Box::new(converted_rhs),
        _type: Type::Int,
    }))
}

fn typecheck_relational(
    kind: &BinaryExpressionKind,
    lhs: &Expression,
    rhs: &Expression,
) -> Result<Expression> {
    let typed_lhs = typecheck_and_convert(lhs)?;
    let typed_rhs = typecheck_and_convert(rhs)?;

    let t1 = get_type(&typed_lhs);
    let t2 = get_type(&typed_rhs);

    let common_type = if is_arithmetic(t1) && is_arithmetic(t2) {
        get_common_type(t1, t2)
    } else if is_pointer_type(t1) && t1 == t2 {
        t2
    } else {
        bail!("Invalid operands for relational operator");
    };

    let converted_lhs = convert_to(&typed_lhs, common_type);
    let converted_rhs = convert_to(&typed_rhs, common_type);

    Ok(Expression::Binary(BinaryExpression {
        kind: kind.clone(),
        lhs: Box::new(converted_lhs),
        rhs: Box::new(converted_rhs),
        _type: Type::Int,
    }))
}

fn typecheck_not(expr: &Expression) -> Result<Expression> {
    let typed_expr = typecheck_and_convert(expr)?;

    if !is_scalar(get_type(&typed_expr)) {
        bail!("Invalid operand for logical not");
    }

    Ok(Expression::Unary(UnaryExpression {
        kind: UnaryExpressionKind::Not,
        expr: Box::new(typed_expr),
        _type: Type::Int,
    }))
}

fn typecheck_complement(expr: &Expression) -> Result<Expression> {
    let typed_expr = typecheck_and_convert(expr)?;

    let t = get_type(&typed_expr);

    if !is_integer_type(t) {
        bail!("Invalid operand for bitwise complement");
    }

    if is_char_type(t) {
        let typed_expr = convert_to(&typed_expr, &Type::Int);
        return Ok(Expression::Unary(UnaryExpression {
            kind: UnaryExpressionKind::Complement,
            expr: Box::new(typed_expr),
            _type: Type::Int,
        }));
    }

    Ok(Expression::Unary(UnaryExpression {
        kind: UnaryExpressionKind::Complement,
        expr: Box::new(typed_expr.clone()),
        _type: t.to_owned(),
    }))
}

fn typecheck_negate(expr: &Expression) -> Result<Expression> {
    let typed_expr = typecheck_and_convert(expr)?;

    let inner_t = get_type(&typed_expr);

    let typed_expr = if is_arithmetic(inner_t) {
        if is_char_type(inner_t) {
            convert_to(&typed_expr, &Type::Int)
        } else {
            typed_expr.clone()
        }
    } else {
        bail!("Invalid operand for negation");
    };

    Ok(Expression::Unary(UnaryExpression {
        kind: UnaryExpressionKind::Negate,
        expr: Box::new(typed_expr.clone()),
        _type: get_type(&typed_expr).to_owned(),
    }))
}

fn typecheck_subscript(expr: &Expression, index: &Expression) -> Result<Expression> {
    let typed_e1 = typecheck_and_convert(expr)?;
    let typed_e2 = typecheck_and_convert(index)?;

    let t1 = get_type(&typed_e1);
    let t2 = get_type(&typed_e2);

    let (ptr_type, converted_lhs, converted_rhs) = if is_ptr_to_complete(t1) && is_integer_type(t2)
    {
        (t1, typed_e1.clone(), convert_to(&typed_e2, &Type::Long))
    } else if is_ptr_to_complete(t2) && is_integer_type(t1) {
        (t2, convert_to(&typed_e1, &Type::Long), typed_e2.clone())
    } else {
        bail!("Invalid operands for subscript");
    };

    let result_type = match ptr_type {
        Type::Pointer(ptr_type) => ptr_type,
        _ => unreachable!(),
    };

    Ok(Expression::Subscript(SubscriptExpression {
        expr: Box::new(converted_lhs),
        index: Box::new(converted_rhs),
        _type: *result_type.clone(),
    }))
}

fn typecheck_expr(expr: &Expression) -> Result<Expression> {
    match expr {
        Expression::Call(CallExpression { name, args, _type }) => {
            let f = SYMBOL_TABLE.lock().unwrap().get(name).cloned().unwrap();
            let f_type = f._type.clone();

            match f_type {
                Type::Func { params, ret } => {
                    if args.len() != params.len() {
                        bail!("Function called with the wrong number of arguments");
                    }

                    let mut converted_args = vec![];

                    let process_arg = |arg: &Expression, param_type: &Type| -> Result<Expression> {
                        let typed_arg = typecheck_and_convert(arg)?;
                        let converted_arg = convert_by_assignment(&typed_arg, param_type)?;

                        Ok(converted_arg)
                    };

                    for (arg, param_type) in args.iter().zip(params.iter()) {
                        converted_args.push(process_arg(arg, param_type)?);
                    }

                    Ok(Expression::Call(CallExpression {
                        name: name.clone(),
                        args: converted_args,
                        _type: *ret,
                    }))
                }
                _ => bail!("Variable used as function name"),
            }
        }
        Expression::Variable(VariableExpression { value, _type }) => {
            let v_type = SYMBOL_TABLE
                .lock()
                .unwrap()
                .get(value)
                .cloned()
                .unwrap()
                ._type;

            let some_fn_type = Type::Func {
                params: vec![Type::Int],
                ret: Type::Int.into(),
            };

            if std::mem::discriminant(&v_type) == std::mem::discriminant(&some_fn_type) {
                bail!("function used as a variable");
            }

            Ok(Expression::Variable(VariableExpression {
                value: value.clone(),
                _type: v_type,
            }))
        }
        Expression::Binary(BinaryExpression {
            kind,
            lhs,
            rhs,
            _type,
        }) => match kind {
            BinaryExpressionKind::And | BinaryExpressionKind::Or => {
                typecheck_logical(kind, lhs, rhs)
            }
            BinaryExpressionKind::Add => typecheck_addition(lhs, rhs),
            BinaryExpressionKind::Sub => typecheck_subtraction(lhs, rhs),
            BinaryExpressionKind::Mul | BinaryExpressionKind::Div | BinaryExpressionKind::Rem => {
                typecheck_multiplicative(kind, lhs, rhs)
            }
            BinaryExpressionKind::Equal | BinaryExpressionKind::NotEqual => {
                typecheck_equality(kind, lhs, rhs)
            }
            BinaryExpressionKind::Less
            | BinaryExpressionKind::LessEqual
            | BinaryExpressionKind::Greater
            | BinaryExpressionKind::GreaterEqual => typecheck_relational(kind, lhs, rhs),
        },
        Expression::Assign(AssignExpression {
            op,
            lhs,
            rhs,
            _type,
        }) => {
            if is_lvalue(lhs) {
                let typed_lhs = typecheck_and_convert(lhs)?;
                // if typed_lhs is not an lvalue
                if !is_lvalue(&typed_lhs) {
                    bail!("Invalid lvalue in assignment");
                }

                let typed_rhs = typecheck_and_convert(rhs)?;

                let left_type = get_type(&typed_lhs);

                let converted_right = convert_by_assignment(&typed_rhs, left_type)?;

                Ok(Expression::Assign(AssignExpression {
                    op: op.clone(),
                    lhs: Box::new(typed_lhs.to_owned()),
                    rhs: Box::new(converted_right),
                    _type: left_type.to_owned(),
                }))
            } else {
                bail!("Invalid lvalue in assignment");
            }
        }
        Expression::Conditional(ConditionalExpression {
            condition,
            then_expr,
            else_expr,
            _type,
        }) => {
            let typed_condition = typecheck_scalar(condition)?;

            let typed_then_expr = typecheck_and_convert(then_expr)?;
            let typed_else_expr = typecheck_and_convert(else_expr)?;

            if !is_scalar(get_type(&typed_condition)) {
                bail!("Non-scalar condition in conditional expression");
            }

            let t1 = get_type(&typed_then_expr);
            let t2 = get_type(&typed_else_expr);

            let common_type = match (t1.clone(), t2.clone()) {
                (Type::Void, Type::Void) => Type::Void,
                (Type::Pointer(_), Type::Pointer(_)) => {
                    get_common_ptr_type(&typed_then_expr, &typed_else_expr)?
                }
                (Type::Pointer(_), _) => get_common_ptr_type(&typed_then_expr, &typed_else_expr)?,
                (_, Type::Pointer(_)) => get_common_ptr_type(&typed_then_expr, &typed_else_expr)?,
                _ => get_common_type(t1, t2).to_owned(),
            };
            let converted_then_expr = convert_to(&typed_then_expr, &common_type);
            let converted_else_expr = convert_to(&typed_else_expr, &common_type);

            Ok(Expression::Cast(CastExpression {
                target_type: common_type.clone(),
                expr: Expression::Conditional(ConditionalExpression {
                    condition: typed_condition.into(),
                    then_expr: converted_then_expr.into(),
                    else_expr: converted_else_expr.into(),
                    _type: common_type.clone(),
                })
                .into(),
                _type: common_type.to_owned(),
            }))
        }
        Expression::Unary(UnaryExpression { kind, expr, _type }) => match kind {
            UnaryExpressionKind::Complement => typecheck_complement(expr),
            UnaryExpressionKind::Negate => typecheck_negate(expr),
            UnaryExpressionKind::Not => typecheck_not(expr),
        },
        Expression::Constant(ConstantExpression { value, _type }) => match value {
            Const::Int(i) => Ok(Expression::Constant(ConstantExpression {
                value: Const::Int(*i),
                _type: Type::Int,
            })),
            Const::Long(l) => Ok(Expression::Constant(ConstantExpression {
                value: Const::Long(*l),
                _type: Type::Long,
            })),
            Const::UInt(u) => Ok(Expression::Constant(ConstantExpression {
                value: Const::UInt(*u),
                _type: Type::Uint,
            })),
            Const::ULong(ul) => Ok(Expression::Constant(ConstantExpression {
                value: Const::ULong(*ul),
                _type: Type::Ulong,
            })),
            Const::Double(d) => Ok(Expression::Constant(ConstantExpression {
                value: Const::Double(*d),
                _type: Type::Double,
            })),
            Const::Char(c) => Ok(Expression::Constant(ConstantExpression {
                value: Const::Char(*c),
                _type: Type::Int,
            })),
            Const::UChar(uc) => Ok(Expression::Constant(ConstantExpression {
                value: Const::UChar(*uc),
                _type: Type::Uint,
            })),
        },
        Expression::Cast(CastExpression {
            target_type,
            expr,
            _type,
        }) => {
            validate_type_specifier(target_type)?;
            let typed_inner = typecheck_and_convert(expr)?;

            let t1 = get_type(&typed_inner);
            let t2 = target_type;

            if let Type::Array { .. } = t2 {
                bail!("Array type in cast");
            }

            if let Type::Pointer(_) = t1 {
                if let Type::Double = t2 {
                    bail!("Pointer to double cast");
                }
            }

            if let Type::Pointer(_) = t2 {
                if let Type::Double = t1 {
                    bail!("Double to pointer cast");
                }
            }

            if target_type == &Type::Void {
                return Ok(Expression::Cast(CastExpression {
                    target_type: target_type.clone(),
                    expr: Box::new(typed_inner),
                    _type: target_type.clone(),
                }));
            }

            if !is_scalar(target_type) {
                bail!("Non-scalar type in cast (can only cast to scalar or void)");
            }

            if !is_scalar(t1) {
                bail!("Cannot cast non-scalar expression to scalar type.")
            }

            Ok(Expression::Cast(CastExpression {
                target_type: target_type.clone(),
                expr: Box::new(typed_inner),
                _type: target_type.clone(),
            }))
        }
        Expression::Deref(DerefExpression { expr, _type }) => {
            let typed_inner = typecheck_and_convert(expr)?;

            let inner_type = get_type(&typed_inner);

            match inner_type {
                Type::Pointer(inner_type) => {
                    if inner_type == &Type::Void.into() {
                        bail!("Dereference of void pointer");
                    }
                    let deref_expr = Expression::Deref(DerefExpression {
                        expr: Box::new(typed_inner.to_owned()),
                        _type: *inner_type.to_owned(),
                    });
                    Ok(deref_expr)
                }
                _ => bail!("Dereference of non-pointer type"),
            }
        }
        Expression::AddrOf(AddrOfExpression { expr, _type }) => {
            if is_lvalue(expr) {
                let typed_inner = typecheck_expr(expr)?;
                let referenced_type = get_type(&typed_inner);
                Ok(Expression::AddrOf(AddrOfExpression {
                    expr: Box::new(typed_inner.to_owned()),
                    _type: Type::Pointer(Box::new(referenced_type.to_owned())),
                }))
            } else {
                bail!("Can't take address of a non-lvalue");
            }
        }
        Expression::Subscript(SubscriptExpression { expr, index, _type }) => {
            typecheck_subscript(expr, index)
        }
        Expression::String(StringExpression { value, _type }) => {
            Ok(Expression::String(StringExpression {
                value: value.clone(),
                _type: Type::Array {
                    element: Type::Char.into(),
                    size: value.len() + 1,
                },
            }))
        }
        Expression::SizeofT(SizeofTExpression { t, _type }) => {
            validate_type_specifier(t)?;
            if !is_complete(t) {
                bail!("Sizeof operator applied to incomplete type");
            }
            Ok(Expression::SizeofT(SizeofTExpression {
                t: t.clone(),
                _type: Type::Ulong,
            }))
        }
        Expression::Sizeof(SizeofExpression { expr, _type }) => {
            let typed_inner = typecheck_expr(expr)?;
            if !is_complete(get_type(&typed_inner)) {
                bail!("Sizeof operator applied to incomplete type");
            }
            Ok(Expression::Sizeof(SizeofExpression {
                expr: Box::new(typed_inner),
                _type: Type::Ulong,
            }))
        }
        Expression::Dot(DotExpression {
            structure,
            member,
            _type,
        }) => {
            let typed_structure = typecheck_and_convert(structure)?;
            match get_type(&typed_structure) {
                Type::Struct { tag } => {
                    let struct_def = TYPE_TABLE.lock().unwrap().get(tag).cloned().unwrap();

                    if !struct_def
                        .members
                        .to_vec()
                        .into_iter()
                        .any(|m| m.name == *member)
                    {
                        bail!("Unknown member in struct");
                    }

                    // find the type of the member
                    let member_def = struct_def
                        .members
                        .to_vec()
                        .into_iter()
                        .find(|m| m.name == *member)
                        .unwrap();

                    Ok(Expression::Dot(DotExpression {
                        structure: Box::new(typed_structure),
                        member: member.clone(),
                        _type: member_def._type.to_owned(),
                    }))
                }
                _ => bail!("Non-struct type in dot expression"),
            }
        }
        Expression::Arrow(ArrowExpression {
            pointer,
            member,
            _type,
        }) => {
            let typed_pointer = typecheck_and_convert(pointer)?;
            match get_type(&typed_pointer) {
                Type::Pointer(referenced) => {
                    if let Type::Struct { tag } = &**referenced {
                        let struct_def = TYPE_TABLE.lock().unwrap().get(tag).cloned().unwrap();

                        if !struct_def
                            .members
                            .to_vec()
                            .into_iter()
                            .any(|m| m.name == *member)
                        {
                            bail!("Unknown member in struct");
                        }

                        // find the type of the member
                        let member_def = struct_def
                            .members
                            .to_vec()
                            .into_iter()
                            .find(|m| m.name == *member)
                            .unwrap();

                        Ok(Expression::Arrow(ArrowExpression {
                            pointer: Box::new(typed_pointer),
                            member: member.clone(),
                            _type: member_def._type.to_owned(),
                        }))
                    } else {
                        bail!("Non-struct type in arrow expression");
                    }
                }
                _ => bail!("Non-struct type in arrow expression"),
            }
        }
        _ => todo!(),
    }
}

fn typecheck_and_convert(e: &Expression) -> Result<Expression> {
    let typed_expr = typecheck_expr(e)?;
    let type_of_expr = get_type(&typed_expr);
    match type_of_expr {
        Type::Array { element, .. } => Ok(Expression::AddrOf(AddrOfExpression {
            expr: typed_expr.to_owned().into(),
            _type: Type::Pointer(element.to_owned()),
        })),
        Type::Struct { .. } => {
            if !is_complete(type_of_expr) {
                bail!("Unknown struct type");
            }
            Ok(typed_expr)
        }
        _ => Ok(typed_expr),
    }
}

fn convert_by_assignment(e: &Expression, target_type: &Type) -> Result<Expression> {
    if get_type(e) == target_type {
        Ok(e.clone())
    } else if (is_arithmetic(get_type(e)) && is_arithmetic(target_type))
        || (is_null_ptr_constant(e) && is_pointer_type(target_type))
        || (target_type == &Type::Pointer(Type::Void.into()) && is_pointer_type(get_type(e)))
        || (is_pointer_type(target_type) && get_type(e) == &Type::Pointer(Type::Void.into()))
    {
        Ok(convert_to(e, target_type))
    } else {
        bail!("cannot convert");
    }
}

fn is_arithmetic(t: &Type) -> bool {
    matches!(
        t,
        Type::Int
            | Type::Uint
            | Type::Long
            | Type::Ulong
            | Type::Double
            | Type::Char
            | Type::UChar
            | Type::SChar
    )
}

pub fn is_integer_type(t: &Type) -> bool {
    matches!(
        t,
        Type::Int | Type::Uint | Type::Long | Type::Ulong | Type::Char | Type::UChar | Type::SChar
    )
}

pub fn is_pointer_type(t: &Type) -> bool {
    matches!(t, Type::Pointer(_))
}

fn is_null_ptr_constant(e: &Expression) -> bool {
    match e {
        Expression::Constant(ConstantExpression { value, _type }) => matches!(
            value,
            Const::Int(0)
                | Const::Long(0)
                | Const::UInt(0)
                | Const::ULong(0)
                | Const::Char(0)
                | Const::UChar(0)
        ),
        _ => false,
    }
}

fn get_common_ptr_type<'a>(e1: &'a Expression, e2: &'a Expression) -> Result<Type> {
    let e1_t = get_type(e1);
    let e2_t = get_type(e2);

    if e1_t == e2_t {
        Ok(e1_t.to_owned())
    } else if is_null_ptr_constant(e1) {
        Ok(e2_t.to_owned())
    } else if is_null_ptr_constant(e2) {
        Ok(e1_t.to_owned())
    } else if e1_t == &Type::Pointer(Type::Void.into()) && is_pointer_type(e2_t)
        || e2_t == &Type::Pointer(Type::Void.into()) && is_pointer_type(e1_t)
    {
        Ok(Type::Pointer(Type::Void.into()))
    } else {
        bail!("Incompatible pointer types");
    }
}

pub fn is_char_type(t: &Type) -> bool {
    matches!(t, Type::Char | Type::UChar | Type::SChar)
}

pub fn get_common_type<'a>(mut type1: &'a Type, mut type2: &'a Type) -> &'a Type {
    if is_char_type(type1) {
        type1 = &Type::Int;
    }

    if is_char_type(type2) {
        type2 = &Type::Int;
    }

    if type1 == type2 {
        return type1;
    }

    if type1 == &Type::Double || type2 == &Type::Double {
        return &Type::Double;
    }

    if get_size_of_type(type1) == get_size_of_type(type2) {
        if get_signedness(type1) {
            return type2;
        } else {
            return type1;
        }
    }

    if get_size_of_type(type1) > get_size_of_type(type2) {
        type1
    } else {
        type2
    }
}

pub fn get_size_of_type(t: &Type) -> usize {
    match t {
        Type::Char | Type::UChar | Type::SChar => 1,
        Type::Int => 4,
        Type::Uint => 4,
        Type::Long => 8,
        Type::Ulong => 8,
        Type::Double => 8,
        Type::Pointer(_) => 8,
        Type::Array { element, size } => get_size_of_type(element) * size,
        Type::Struct { tag } => {
            let struct_def = TYPE_TABLE.lock().unwrap().get(tag).unwrap().clone();
            struct_def.size
        }
        _ => {
            unreachable!()
        }
    }
}

pub fn get_signedness(t: &Type) -> bool {
    match t {
        Type::Int => true,
        Type::Uint => false,
        Type::Long => true,
        Type::Ulong => false,
        Type::Pointer(_) => false,
        Type::Char | Type::SChar => true,
        Type::UChar => false,
        _ => unreachable!(),
    }
}

fn convert_to(e: &Expression, _type: &Type) -> Expression {
    if get_type(e) == _type {
        return e.clone();
    }
    Expression::Cast(CastExpression {
        target_type: _type.clone(),
        expr: Box::new(e.clone()),
        _type: _type.clone(),
    })
}

pub fn get_type(e: &Expression) -> &Type {
    match e {
        Expression::Assign(assign) => &assign._type,
        Expression::Binary(binary) => &binary._type,
        Expression::Call(call) => &call._type,
        Expression::Cast(cast) => &cast._type,
        Expression::Conditional(conditional) => &conditional._type,
        Expression::Constant(constant) => &constant._type,
        Expression::Unary(unary) => &unary._type,
        Expression::Variable(variable) => &variable._type,
        Expression::Deref(deref) => &deref._type,
        Expression::AddrOf(addr_of) => &addr_of._type,
        Expression::Literal(literal) => &literal._type,
        Expression::Subscript(subscript) => &subscript._type,
        Expression::String(string) => &string._type,
        Expression::Sizeof(sizeof) => &sizeof._type,
        Expression::SizeofT(sizeof_t) => &sizeof_t._type,
        Expression::Arrow(arrow) => &arrow._type,
        Expression::Dot(dot) => &dot._type,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IdentifierAttrs {
    FuncAttr {
        defined: bool,
        global: bool,
    },
    StaticAttr {
        initial_value: InitialValue,
        global: bool,
    },
    ConstantAttr(StaticInit),
    LocalAttr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum InitialValue {
    Tentative,
    Initial(Vec<StaticInit>),
    NoInitializer,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StaticInit {
    Int(i32),
    Long(i64),
    UInt(u32),
    ULong(u64),
    Double(f64),
    Char(i32),
    UChar(u32),
    String(String, bool),
    Pointer(String),
    Zero(usize),
}

pub fn is_scalar(t: &Type) -> bool {
    !matches!(
        t,
        Type::Void | Type::Array { .. } | Type::Func { .. } | Type::Struct { .. }
    )
}

pub fn is_complete(t: &Type) -> bool {
    match t {
        Type::Void => false,
        Type::Struct { tag } => {
            if TYPE_TABLE.lock().unwrap().contains_key(tag) {
                true
            } else {
                false
            }
        }
        _ => true,
    }
}

fn is_ptr_to_complete(t: &Type) -> bool {
    match t {
        Type::Pointer(inner) => is_complete(inner),
        _ => false,
    }
}

fn validate_type_specifier(t: &Type) -> Result<()> {
    match t {
        Type::Array { element, size: _ } => {
            if !is_complete(element) {
                bail!("Incomplete type");
            }
            validate_type_specifier(element)?;
        }
        Type::Pointer(referenced) => {
            validate_type_specifier(referenced)?;
        }
        Type::Func { params, ret } => {
            for param in params {
                validate_type_specifier(param)?;
            }
            validate_type_specifier(ret)?;
        }
        _ => {}
    }

    Ok(())
}

fn typecheck_scalar(e: &Expression) -> Result<Expression> {
    let typechecked_expr = typecheck_and_convert(e)?;
    if is_scalar(get_type(&typechecked_expr)) {
        Ok(typechecked_expr)
    } else {
        bail!("Non-scalar expression used where scalar expression expected");
    }
}

fn optionally_typecheck_scalar(e: &Option<Expression>) -> Result<Option<Expression>> {
    match e {
        Some(expr) => {
            let typechecked_expr = typecheck_scalar(expr)?;
            Ok(Some(typechecked_expr))
        }
        None => Ok(None),
    }
}

fn is_lvalue(e: &Expression) -> bool {
    match e {
        Expression::Variable(_)
        | Expression::Deref(_)
        | Expression::Subscript(_)
        | Expression::String(_)
        | Expression::Arrow(_) => true,
        Expression::Dot(dot) => is_lvalue(&dot.structure),
        _ => false,
    }
}
