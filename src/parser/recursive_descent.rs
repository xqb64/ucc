use crate::{
    lexer::lex::{Const, Span, Token, TokenKind},
    parser::ast::{
        AbstractDeclarator, AggregateKind, AddrOfExpression, ArrowExpression, AssignExpression, BinaryExpression,
        BinaryExpressionKind, BlockItem, BlockStatement, BreakStatement, CallExpression,
        CastExpression, ConditionalExpression, ConstantExpression, ContinueStatement, Declaration,
        Declarator, DerefExpression, DoWhileStatement, DotExpression, EnumDeclaration,
        EnumMemberDeclaration, Expression, ExpressionStatement, ForInit, ForStatement, FunctionDeclaration, IfStatement, Initializer,
        LiteralExpression, MemberDeclaration, ParamInfo, Program, ReturnStatement,
        SizeofExpression, SizeofTExpression, Statement, StorageClass, StringExpression,
        StructDeclaration, SubscriptExpression, Type, TypedefDeclaration, UnaryExpression, UnaryExpressionKind,
        VariableDeclaration, VariableExpression, WhileStatement,
    },
    util::error::{ErrorKind, Result, UccError},
};
use std::collections::{BTreeMap, BTreeSet, VecDeque};

use super::ast::{
    CaseStatement, CompoundExpression, CompoundExpressionKind, DefaultStatement, GotoStatement,
    LabeledStatement, PostfixExpression, PostfixExpressionKind, SwitchStatement,
};

pub struct Parser {
    pub tokens: VecDeque<Token>,
    pub current: Option<Token>,
    pub previous: Option<Token>,
    pub depth: usize,
    pub current_target_type: Option<Type>,
    pub current_fn: Option<String>,
    pub typedef_scopes: Vec<BTreeMap<String, Type>>,
    pub typedef_shadows: Vec<BTreeSet<String>>,
}

impl Parser {
    pub fn new(tokens: VecDeque<Token>) -> Parser {
        Parser {
            tokens,
            current: None,
            previous: None,
            depth: 0,
            current_target_type: None,
            current_fn: None,
            typedef_scopes: vec![BTreeMap::new()],
            typedef_shadows: vec![BTreeSet::new()],
        }
    }

    fn advance(&mut self) -> Option<Token> {
        self.previous = self.current.take();
        self.current = self.tokens.pop_front();
        self.previous.clone()
    }

    fn consume(&mut self, token: &TokenKind) -> Result<Option<Token>> {
        if self.check(token) {
            return Ok(self.advance());
        }
        Err(UccError {
            msg: format!(
                "expected {:?}, got: prev: {:?}. curr: {:?}",
                token, self.previous, self.current
            ),
            kind: ErrorKind::Parse,
            span: self.current_span()?,
        })
    }

    fn check(&self, kind: &TokenKind) -> bool {
        if let Some(current) = &self.current {
            std::mem::discriminant(&current.kind) == std::mem::discriminant(kind)
        } else {
            false
        }
    }

    pub fn parse(&mut self) -> Result<Program> {
        self.advance();
        let mut stmts = vec![];
        while self.current.is_some() {
            stmts.push(self.parse_statement()?);
        }
        Ok(Program { block_items: stmts })
    }

    fn is_next(&mut self, kinds: &[TokenKind]) -> bool {
        for kind in kinds {
            if self.check(kind) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check_many(&self, kinds: &[TokenKind]) -> bool {
        for kind in kinds {
            if self.check(kind) {
                return true;
            }
        }
        false
    }

    fn enter_typedef_scope(&mut self) {
        self.typedef_scopes.push(BTreeMap::new());
        self.typedef_shadows.push(BTreeSet::new());
    }

    fn exit_typedef_scope(&mut self) {
        self.typedef_scopes.pop();
        self.typedef_shadows.pop();
    }

    fn lookup_typedef(&self, name: &str) -> Option<Type> {
        for (aliases, shadows) in self.typedef_scopes.iter().zip(self.typedef_shadows.iter()).rev() {
            if shadows.contains(name) {
                return None;
            }
            if let Some(ty) = aliases.get(name) {
                return Some(ty.clone());
            }
        }
        None
    }

    fn is_typedef_name(&self, token: &TokenKind) -> bool {
        match token {
            TokenKind::Identifier(name) => self.lookup_typedef(name).is_some(),
            _ => false,
        }
    }

    fn declare_typedef_name(&mut self, name: String, ty: Type) {
        if let Some(shadows) = self.typedef_shadows.last_mut() {
            shadows.remove(&name);
        }
        self.typedef_scopes.last_mut().unwrap().insert(name, ty);
    }

    fn shadow_typedef_name(&mut self, name: &str) {
        if self.lookup_typedef(name).is_some() && !self.typedef_scopes.last().unwrap().contains_key(name) {
            self.typedef_shadows
                .last_mut()
                .unwrap()
                .insert(name.to_string());
        }
    }

    fn starts_declaration(&self) -> bool {
        match self.current.as_ref().map(|token| &token.kind) {
            Some(kind) => self.is_specifier(kind) || self.is_typedef_name(kind),
            None => false,
        }
    }

    fn parse_statement(&mut self) -> Result<BlockItem> {
        if let Some(current) = &self.current {
            match current.kind {
                TokenKind::Identifier(_) => match self.tokens.front() {
                    Some(front) => match front.kind {
                        TokenKind::Colon => return self.parse_labeled_statement(),
                        _ => {}
                    },
                    _ => {}
                },
                _ => {}
            }
        }

        if self.starts_declaration() {
            self.parse_declaration()
        } else if self.is_next(&[TokenKind::Return]) {
            self.parse_return_statement()
        } else if self.is_next(&[TokenKind::If]) {
            self.parse_if_statement()
        } else if self.is_next(&[TokenKind::Do]) {
            self.parse_do_while_statement()
        } else if self.is_next(&[TokenKind::While]) {
            self.parse_while_statement()
        } else if self.is_next(&[TokenKind::For]) {
            self.parse_for_statement()
        } else if self.is_next(&[TokenKind::Break]) {
            self.parse_break_statement()
        } else if self.is_next(&[TokenKind::Continue]) {
            self.parse_continue_statement()
        } else if self.is_next(&[TokenKind::Goto]) {
            self.parse_goto_statement()
        } else if self.is_next(&[TokenKind::Switch]) {
            self.parse_switch_statement()
        } else if self.is_next(&[TokenKind::Case]) {
            self.parse_case_statement()
        } else if self.is_next(&[TokenKind::Default]) {
            self.parse_default_statement()
        } else if self.is_next(&[TokenKind::LBrace]) {
            self.parse_block_statement()
        } else if self.is_next(&[TokenKind::Semicolon]) {
            Ok(BlockItem::Statement(Statement::Null))
        } else {
            self.parse_expression_statement()
        }
    }

    fn parse_declaration(&mut self) -> Result<BlockItem> {
        match self
            .peek(3)
            .iter()
            .cloned()
            .map(|t| t.kind)
            .collect::<Vec<_>>()
            .as_slice()
        {
            [TokenKind::Struct | TokenKind::Union, TokenKind::Identifier(_), TokenKind::LBrace | TokenKind::Semicolon] => {
                self.parse_struct_or_union_decl()
            }
            [TokenKind::Enum, TokenKind::Identifier(_), TokenKind::LBrace | TokenKind::Semicolon]
            | [TokenKind::Enum, TokenKind::LBrace, _] => self.parse_enum_decl(),
            _ => self.parse_var_or_fn_decl(),
        }
    }

    fn parse_struct_or_union_decl(&mut self) -> Result<BlockItem> {
        let begin = self.current_span()?;
        let kind = match self.current.as_ref().unwrap().kind {
            TokenKind::Struct => {
                self.consume(&TokenKind::Struct)?;
                AggregateKind::Struct
            }
            TokenKind::Union => {
                self.consume(&TokenKind::Union)?;
                AggregateKind::Union
            }
            _ => unreachable!(),
        };
        let tag = self
            .consume(&TokenKind::Identifier("".to_owned()))?
            .unwrap()
            .as_string();
        let members = if self.is_next(&[TokenKind::LBrace]) {
            let mut members = vec![];
            loop {
                let next_member = self.parse_member_decl()?;
                if self.is_next(&[TokenKind::RBrace]) {
                    members.push(next_member);
                    break members;
                } else {
                    members.push(next_member);
                }
            }
        } else {
            vec![]
        };
        self.consume(&TokenKind::Semicolon)?;
        let end = self.current_span()?;
        Ok(BlockItem::Declaration(match kind {
            AggregateKind::Struct => Declaration::Struct(StructDeclaration {
                tag,
                kind,
                members,
                span: begin + end,
            }),
            AggregateKind::Union => Declaration::Union(StructDeclaration {
                tag,
                kind,
                members,
                span: begin + end,
            }),
        }))
    }

    fn parse_enum_decl(&mut self) -> Result<BlockItem> {
        let begin = self.current_span()?;
        self.consume(&TokenKind::Enum)?;

        let tag = if matches!(self.current.as_ref().map(|t| &t.kind), Some(TokenKind::Identifier(_))) {
            Some(
                self.consume(&TokenKind::Identifier(String::new()))?
                    .unwrap()
                    .as_string(),
            )
        } else {
            None
        };

        let members = if self.is_next(&[TokenKind::LBrace]) {
            let mut members = vec![];
            if self.is_next(&[TokenKind::RBrace]) {
                members
            } else {
                loop {
                    members.push(self.parse_enum_member_decl()?);
                    if self.is_next(&[TokenKind::Comma]) {
                        if self.is_next(&[TokenKind::RBrace]) {
                            break members;
                        }
                    } else {
                        self.consume(&TokenKind::RBrace)?;
                        break members;
                    }
                }
            }
        } else {
            vec![]
        };

        self.consume(&TokenKind::Semicolon)?;
        let end = self.current_span()?;

        Ok(BlockItem::Declaration(Declaration::Enum(EnumDeclaration {
            tag,
            members,
            span: begin + end,
        })))
    }

    fn parse_enum_member_decl(&mut self) -> Result<EnumMemberDeclaration> {
        let begin = self.current_span()?;
        let name = self
            .consume(&TokenKind::Identifier(String::new()))?
            .unwrap()
            .as_string();
        let value = if self.is_next(&[TokenKind::Equal]) {
            Some(self.parse_expression()?)
        } else {
            None
        };
        let end = self.current_span()?;

        Ok(EnumMemberDeclaration {
            name,
            value,
            span: begin + end,
        })
    }

    fn parse_member_decl(&mut self) -> Result<MemberDeclaration> {
        let begin = self.current_span()?;
        let specifier_list = self.consume_while_type_specifier();
        let base_type = self.parse_type(
            specifier_list
                .iter()
                .cloned()
                .map(|t| t.kind)
                .collect::<Vec<_>>(),
        )?;
        let declarator = self.parse_declarator()?;
        match declarator {
            Declarator::Func(_, _, _) => {
                return Err(UccError {
                    msg: format!("function declarations not allowed in struct"),
                    kind: ErrorKind::Parse,
                    span: self.current_span()?,
                })
            }
            _ => {
                self.consume(&TokenKind::Semicolon)?;
                let end = self.current_span()?;
                let (name, decl_type, _) = self.process_declarator(&declarator, &base_type)?;
                Ok(MemberDeclaration {
                    name,
                    ty: decl_type,
                    span: begin + end,
                })
            }
        }
    }

    fn is_type_specifier(&self, token: &TokenKind) -> bool {
        matches!(
            token,
            TokenKind::Char
                | TokenKind::Short
                | TokenKind::Int
                | TokenKind::Long
                | TokenKind::Unsigned
                | TokenKind::Signed
                | TokenKind::Float
                | TokenKind::Double
                | TokenKind::Void
                | TokenKind::Struct
                | TokenKind::Union
                | TokenKind::Enum
        ) || self.is_typedef_name(token)
    }

    fn is_storage_class_specifier(&self, token: &TokenKind) -> bool {
        matches!(token, TokenKind::Static | TokenKind::Extern | TokenKind::Typedef)
    }

    fn is_specifier(&self, token: &TokenKind) -> bool {
        self.is_type_specifier(token) || self.is_storage_class_specifier(token)
    }

    fn parse_var_or_fn_decl(&mut self) -> Result<BlockItem> {
        let begin = self.current_span()?;
        let specifier_list = self.consume_while_specifier()?;
        let (base_type, storage_class) = self.parse_type_and_storage_specifiers(
            &specifier_list
                .iter()
                .cloned()
                .map(|t| t.kind)
                .collect::<Vec<_>>(),
        )?;

        let declarator = self.parse_declarator()?;
        let (name, decl_type, params) = self.process_declarator(&declarator, &base_type)?;

        if storage_class == Some(StorageClass::Typedef) {
            self.consume(&TokenKind::Semicolon)?;
            let end = self.current_span()?;
            self.declare_typedef_name(name.clone(), decl_type.clone());
            return Ok(BlockItem::Declaration(Declaration::Typedef(TypedefDeclaration {
                name,
                ty: decl_type,
                span: begin + end,
            })));
        }

        match decl_type {
            Type::Func { .. } => {
                self.parse_function_declaration(&name, &params, decl_type, storage_class, begin)
            }
            _ => {
                self.shadow_typedef_name(&name);
                let init = if self.is_next(&[TokenKind::Equal]) {
                    let expr = self.parse_expression()?;
                    self.consume(&TokenKind::Semicolon)?;
                    Some(expr)
                } else if self.is_next(&[TokenKind::Semicolon]) {
                    None
                } else {
                    return Err(UccError {
                        msg: format!("internal error, parse var or fn decl"),
                        kind: ErrorKind::Parse,
                        span: self.current_span()?,
                    });
                };

                let unwrapped = self.unwrap_expression_to_initializer(&name, init);

                let end = self.current_span()?;

                Ok(BlockItem::Declaration(Declaration::Variable(
                    VariableDeclaration {
                        name,
                        ty: decl_type,
                        init: unwrapped,
                        storage_class,
                        is_global: self.depth == 0,
                        span: begin + end,
                    },
                )))
            }
        }
    }

    fn transform_initializer(&self, name: &str, init: &Initializer) -> Initializer {
        match init {
            Initializer::Single(_, expr) => {
                if let Expression::Literal(lit) = expr {
                    self.transform_initializer(name, &lit.value)
                } else {
                    Initializer::Single(name.to_string(), expr.clone())
                }
            }
            Initializer::Compound(_, ty, elems) => {
                let new_elems = elems
                    .iter()
                    .map(|elem| self.transform_initializer(name, elem))
                    .collect();
                Initializer::Compound(name.to_string(), ty.clone(), new_elems)
            }
        }
    }

    fn convert_expression_to_initializer(&self, name: &str, expr: Expression) -> Initializer {
        match expr {
            Expression::Literal(literal) => self.transform_initializer(name, &literal.value),
            _ => Initializer::Single(name.to_owned(), expr),
        }
    }

    fn unwrap_expression_to_initializer(
        &self,
        name: &str,
        expr_opt: Option<Expression>,
    ) -> Option<Initializer> {
        expr_opt.map(|expr| self.convert_expression_to_initializer(name, expr))
    }

    fn parse_declarator(&mut self) -> Result<Declarator> {
        match self.current.as_ref() {
            Some(token) => match token.kind {
                TokenKind::Star => {
                    self.consume(&TokenKind::Star)?;
                    let inner = self.parse_declarator()?;
                    Ok(Declarator::Pointer(Box::new(inner)))
                }
                _ => self.parse_direct_declarator(),
            },
            None => {
                return Err(UccError {
                    kind: ErrorKind::Parse,
                    msg: format!("internal error parse_declarator"),
                    span: self.current_span()?,
                })
            }
        }
    }

    fn parse_direct_declarator(&mut self) -> Result<Declarator> {
        let simple_declarator = self.parse_simple_declarator()?;
        match self.current.as_ref() {
            Some(token) => match token.kind {
                TokenKind::LParen => {
                    self.consume(&TokenKind::LParen)?;
                    let (params, variadic) = self.parse_param_list()?;
                    Ok(Declarator::Func(params, variadic, Box::new(simple_declarator)))
                }
                TokenKind::LBracket => {
                    let decl = self.parse_array_decl_suffix(&simple_declarator)?;
                    Ok(decl)
                }
                _ => Ok(simple_declarator),
            },
            None => {
                return Err(UccError {
                    kind: ErrorKind::Parse,
                    msg: format!("internal error, parse_direct_declarator"),
                    span: self.current_span()?,
                })
            }
        }
    }

    fn parse_simple_declarator(&mut self) -> Result<Declarator> {
        let token = self.advance().unwrap();
        match token.kind {
            TokenKind::LParen => {
                let decl = self.parse_declarator()?;
                self.consume(&TokenKind::RParen)?;
                Ok(decl)
            }
            TokenKind::Identifier(id) => Ok(Declarator::Ident(id)),
            _ => {
                return Err(UccError {
                    msg: format!("internal error, parse_simple_declarator"),
                    kind: ErrorKind::Parse,
                    span: self.current_span()?,
                });
            }
        }
    }

    fn parse_array_decl_suffix(&mut self, base_decl: &Declarator) -> Result<Declarator> {
        let dim = self.parse_dim()?;
        let mut new_decl = Declarator::Array(Box::new(base_decl.clone()), dim);

        if let Some(token) = self.current.as_ref() {
            if token.kind == TokenKind::LBracket {
                new_decl = self.parse_array_decl_suffix(&new_decl)?;
            }
        }

        Ok(new_decl)
    }

    fn consume_constant_or_char_literal(&mut self) -> Result<Option<Token>> {
        match self.current.as_ref() {
            Some(token) => match token.kind {
                TokenKind::Constant(_) | TokenKind::CharLiteral(_) => Ok(self.advance()),
                _ => {
                    return Err(UccError {
                        msg: format!("expected constant, got: {:?}", self.current),
                        kind: ErrorKind::Parse,
                        span: self.current_span()?,
                    })
                }
            },
            None => {
                return Err(UccError {
                    kind: ErrorKind::Parse,
                    msg: format!("internal error, consume_constant"),
                    span: self.current_span()?,
                })
            }
        }
    }

    fn parse_dim(&mut self) -> Result<usize> {
        self.consume(&TokenKind::LBracket)?;
        let dim = self.consume_constant_or_char_literal()?.unwrap();
        self.consume(&TokenKind::RBracket)?;
        Ok(match dim.kind {
            TokenKind::Constant(Const::Short(n)) => n as usize,
            TokenKind::Constant(Const::UShort(n)) => n as usize,
            TokenKind::Constant(Const::Int(n)) => n as usize,
            TokenKind::Constant(Const::Long(n)) => n as usize,
            TokenKind::Constant(Const::UInt(n)) => n as usize,
            TokenKind::Constant(Const::ULong(n)) => n as usize,
            TokenKind::Constant(Const::Char(n)) => n as usize,
            TokenKind::Constant(Const::UChar(n)) => n as usize,
            TokenKind::CharLiteral(ch) => ch as usize,
            _ => {
                return Err(UccError {
                    kind: ErrorKind::Parse,
                    msg: format!("expected const or char literal, got: {:?}", dim),
                    span: self.current_span()?,
                })
            }
        })
    }
    fn parse_param_list(&mut self) -> Result<(Vec<ParamInfo>, bool)> {
        let in_front_of_us = self
            .lookahead_until(&TokenKind::RParen)
            .iter()
            .cloned()
            .map(|t| t.kind)
            .collect::<Vec<_>>();

        if in_front_of_us == vec![TokenKind::Void] {
            self.consume(&TokenKind::Void)?;
            self.consume(&TokenKind::RParen)?;

            Ok((vec![], false))
        } else {
            let mut params = vec![];
            let mut variadic = false;
            loop {
                if self.is_next(&[TokenKind::Ellipsis]) {
                    if params.is_empty() {
                        return Err(UccError {
                            msg: format!("Variadic parameter list needs at least one named parameter"),
                            kind: ErrorKind::Parse,
                            span: self.current_span()?,
                        });
                    }
                    variadic = true;
                    break;
                }

                params.push(self.parse_param()?);
                if !self.is_next(&[TokenKind::Comma]) {
                    break;
                }
            }
            self.consume(&TokenKind::RParen)?;
            Ok((params, variadic))
        }
    }

    fn consume_while_type_specifier(&mut self) -> Vec<Token> {
        let mut specifier_list = vec![];
        while self.is_type_specifier(&self.current.as_ref().unwrap().kind) {
            match self.current.as_ref() {
                Some(token) => match token.kind {
                    TokenKind::Struct | TokenKind::Union | TokenKind::Enum => {
                        specifier_list.push(self.current.clone().unwrap());
                        self.advance();
                        if let Some(token) = self.current.as_ref() {
                            if let TokenKind::Identifier(_) = token.kind {
                                specifier_list.push(self.current.clone().unwrap());
                                self.advance();
                            }
                        }
                    }
                    _ => {
                        specifier_list.push(self.current.clone().unwrap());
                        self.advance();
                    }
                },
                None => return vec![],
            }
        }
        specifier_list
    }

    fn parse_param(&mut self) -> Result<ParamInfo> {
        let specifier_list = self.consume_while_type_specifier();
        let param_t = self.parse_type(
            specifier_list
                .iter()
                .cloned()
                .map(|t| t.kind)
                .collect::<Vec<_>>(),
        )?;
        let param_decl = self.parse_declarator()?;
        Ok((param_t, param_decl.into()))
    }

    fn consume_while_specifier(&mut self) -> Result<Vec<Token>> {
        let mut specifier_list = vec![];
        while self.is_specifier(&self.current.as_ref().unwrap().kind) {
            match self.current.as_ref() {
                Some(token) => match token.kind {
                    TokenKind::Struct | TokenKind::Union | TokenKind::Enum => {
                        specifier_list.push(self.current.clone().unwrap());
                        self.advance();

                        if let Some(token) = self.current.as_ref() {
                            if let TokenKind::Identifier(_) = token.kind {
                                specifier_list.push(self.current.clone().unwrap());
                                self.advance();
                            }
                        } else {
                            return Err(UccError {
                                kind: ErrorKind::Parse,
                                msg: format!("expected an identifier after tag specifier"),
                                span: self.current_span()?,
                            });
                        }
                    }
                    _ => {
                        specifier_list.push(self.current.clone().unwrap());
                        self.advance();
                    }
                },
                None => {
                    return Err(UccError {
                        kind: ErrorKind::Parse,
                        msg: format!("internal error, consume_while_spec 2"),
                        span: self.current_span()?,
                    })
                }
            }
        }

        Ok(specifier_list)
    }

    fn process_declarator(
        &self,
        declarator: &Declarator,
        base_type: &Type,
    ) -> Result<(String, Type, Vec<String>)> {
        let some_fn_type = Type::Func {
            params: vec![],
            ret: Box::new(Type::Int),
            variadic: false,
        };
        match declarator {
            Declarator::Ident(name) => Ok((name.clone(), base_type.clone(), vec![])),
            Declarator::Pointer(decl) => {
                let derived_type = Type::Pointer(base_type.clone().into());
                self.process_declarator(decl, &derived_type)
            }
            Declarator::Func(params, variadic, decl) => match *decl.clone() {
                Declarator::Ident(name) => {
                    let mut param_names = vec![];
                    let mut param_types = vec![];

                    for (param_base_type, param_declarator) in params {
                        let (param_name, param_type, _) =
                            self.process_declarator(param_declarator, param_base_type)?;
                        if std::mem::discriminant(&param_type)
                            == std::mem::discriminant(&some_fn_type)
                        {
                            return Err(UccError {
                                kind: ErrorKind::Parse,
                                msg: format!("Function pointers in parameters are not supported."),
                                span: self.current_span()?,
                            });
                        }
                        param_names.push(param_name);
                        param_types.push(param_type);
                    }

                    let derived_type = Type::Func {
                        params: param_types,
                        ret: base_type.clone().into(),
                        variadic: *variadic,
                    };
                    Ok((name.clone(), derived_type, param_names))
                }
                _ => {
                    return Err(UccError {
                        kind: ErrorKind::Parse,
                        msg: format!("Can't apply additional type derivations to a function type."),
                        span: self.current_span()?,
                    })
                }
            },
            Declarator::Array(inner, size) => {
                let derived_type = Type::Array {
                    element: Box::new(base_type.clone()),
                    size: *size,
                };
                self.process_declarator(inner, &derived_type)
            }
        }
    }

    fn parse_function_declaration(
        &mut self,
        name: &str,
        params: &[String],
        ty: Type,
        storage_class: Option<StorageClass>,
        begin: Span,
    ) -> Result<BlockItem> {
        self.current_target_type = Some(match ty.clone() {
            Type::Func { ret, .. } => *ret,
            _ => unreachable!(),
        });

        let body = if self.check(&TokenKind::Semicolon) {
            self.consume(&TokenKind::Semicolon)?;
            None
        } else if self.check(&TokenKind::LBrace) {
            self.consume(&TokenKind::LBrace)?;
            self.enter_typedef_scope();
            for param in params {
                self.shadow_typedef_name(param);
            }
            self.current_fn = Some(name.to_string());
            let block = Some(self.parse_block_statement()?);
            self.current_fn = None;
            self.exit_typedef_scope();

            block
        } else {
            return Err(UccError {
                kind: ErrorKind::Parse,
                msg: format!(
                    "Expected block statement or semicolon, got: {:?}",
                    self.current
                ),
                span: self.current_span()?,
            });
        };

        self.current_target_type = None;

        let end = self.current_span()?;

        Ok(BlockItem::Declaration(Declaration::Function(
            FunctionDeclaration {
                name: name.to_owned(),
                params: params.to_owned(),
                body: body.into(),
                is_global: self.depth == 0,
                storage_class,
                ty: ty.clone(),
                span: begin + end,
            },
        )))
    }

    fn parse_type(&self, specifier_list: Vec<TokenKind>) -> Result<Type> {
        let mut sorted_specifiers = specifier_list.clone();
        sorted_specifiers.sort();

        let invalid_type = || {
            Err(UccError {
                kind: ErrorKind::Parse,
                msg: format!("Invalid type specifier."),
                span: self.current_span()?,
            })
        };

        match &sorted_specifiers[..] {
            [TokenKind::Struct, TokenKind::Identifier(tag)] => {
                Ok(Type::Struct { tag: tag.clone() })
            }
            [TokenKind::Union, TokenKind::Identifier(tag)] => {
                Ok(Type::Union { tag: tag.clone() })
            }
            [TokenKind::Enum, TokenKind::Identifier(tag)] => {
                Ok(Type::Enum { tag: tag.clone() })
            }
            [TokenKind::Identifier(name)] => self.lookup_typedef(name).ok_or_else(|| UccError {
                kind: ErrorKind::Parse,
                msg: format!("Unknown typedef name: {}", name),
                span: self.current_span().unwrap_or(Span { start: 0, end: 0 }),
            }),
            [TokenKind::Void] => Ok(Type::Void),
            [TokenKind::Float] => Ok(Type::Float),
            [TokenKind::Double] => Ok(Type::Double),
            [TokenKind::Char] => Ok(Type::Char),
            [TokenKind::Char, TokenKind::Signed] => Ok(Type::SChar),
            [TokenKind::Char, TokenKind::Unsigned] => Ok(Type::UChar),
            _ => {
                let unique_specifiers: BTreeSet<_> = sorted_specifiers.iter().collect();
                if sorted_specifiers.is_empty()
                    || unique_specifiers.len() != sorted_specifiers.len()
                    || sorted_specifiers.contains(&TokenKind::Float)
                    || sorted_specifiers.contains(&TokenKind::Double)
                    || sorted_specifiers.contains(&TokenKind::Char)
                    || sorted_specifiers.contains(&TokenKind::Void)
                    || sorted_specifiers
                        .iter()
                        .any(|s| matches!(s, TokenKind::Identifier(_)))
                    || (sorted_specifiers.contains(&TokenKind::Signed)
                        && sorted_specifiers.contains(&TokenKind::Unsigned))
                    || (sorted_specifiers.contains(&TokenKind::Short)
                        && sorted_specifiers.contains(&TokenKind::Long))
                {
                    return invalid_type();
                }

                let has_unsigned = sorted_specifiers.contains(&TokenKind::Unsigned);
                let has_long = sorted_specifiers.contains(&TokenKind::Long);
                let has_short = sorted_specifiers.contains(&TokenKind::Short);

                // Aside from `signed`/`unsigned`, `int` may appear with either
                // `short` or `long`; no other type-specifier combinations are valid here.
                if sorted_specifiers
                    .iter()
                    .any(|specifier| {
                        !matches!(
                            specifier,
                            TokenKind::Signed
                                | TokenKind::Unsigned
                                | TokenKind::Int
                                | TokenKind::Short
                                | TokenKind::Long
                        )
                    })
                {
                    return invalid_type();
                }

                if has_unsigned && has_long {
                    Ok(Type::ULong)
                } else if has_unsigned && has_short {
                    Ok(Type::UShort)
                } else if has_unsigned {
                    Ok(Type::UInt)
                } else if has_long {
                    Ok(Type::Long)
                } else if has_short {
                    Ok(Type::Short)
                } else {
                    Ok(Type::Int)
                }
            }
        }
    }

    fn is_ident(&self, token: &TokenKind) -> bool {
        matches!(token, TokenKind::Identifier(_))
    }

    fn parse_type_and_storage_specifiers(
        &mut self,
        specifier_list: &[TokenKind],
    ) -> Result<(Type, Option<StorageClass>)> {
        let mut types = vec![];
        let mut storage_classes = vec![];

        for specifier in specifier_list {
            if self.is_type_specifier(specifier) || self.is_ident(specifier) {
                types.push(specifier.clone());
            } else {
                storage_classes.push(specifier.clone());
            }
        }

        let ty = self.parse_type(types)?;

        if storage_classes.len() > 1 {
            return Err(UccError {
                kind: ErrorKind::Parse,
                msg: format!(
                    "expected at most one storage class specifier, got: {:?}",
                    storage_classes
                ),
                span: self.current_span()?,
            });
        }

        let storage_class = if storage_classes.len() == 1 {
            match storage_classes[0] {
                TokenKind::Static => Some(StorageClass::Static),
                TokenKind::Extern => Some(StorageClass::Extern),
                TokenKind::Typedef => Some(StorageClass::Typedef),
                _ => {
                    unreachable!()
                }
            }
        } else {
            None
        };

        Ok((ty, storage_class))
    }

    fn parse_expression_statement(&mut self) -> Result<BlockItem> {
        let begin = self.current_span()?;
        let expr = self.parse_expression()?;
        self.consume(&TokenKind::Semicolon)?;
        let end = self.current_span()?;
        Ok(BlockItem::Statement(Statement::Expression(
            ExpressionStatement {
                expr,
                span: begin + end,
            },
        )))
    }

    fn parse_block_statement(&mut self) -> Result<BlockItem> {
        self.depth += 1;
        self.enter_typedef_scope();
        let begin = self.current_span()?;
        let mut stmts = vec![];
        while !self.check(&TokenKind::RBrace) {
            stmts.push(self.parse_statement()?);
        }
        self.consume(&TokenKind::RBrace)?;
        let end = self.current_span()?;
        self.depth -= 1;
        self.exit_typedef_scope();
        Ok(BlockItem::Statement(Statement::Compound(BlockStatement {
            stmts,
            span: begin + end,
        })))
    }

    fn parse_switch_statement(&mut self) -> Result<BlockItem> {
        let begin = self.current_span()?;
        self.consume(&TokenKind::LParen)?;
        let condition = self.parse_expression()?;
        self.consume(&TokenKind::RParen)?;
        let body = match self.parse_statement() {
            Ok(BlockItem::Statement(stmt)) => stmt,
            _ => {
                return Err(UccError {
                    kind: ErrorKind::Parse,
                    msg: format!("internal error, parse_switch"),
                    span: self.current_span()?,
                });
            }
        };
        let end = self.current_span()?;
        Ok(BlockItem::Statement(Statement::Switch(SwitchStatement {
            condition,
            body: body.into(),
            label: String::new(),
            cases: vec![],
            span: begin + end,
        })))
    }

    fn parse_case_statement(&mut self) -> Result<BlockItem> {
        let begin = self.current_span()?;
        let value = self.parse_expression()?;
        self.consume(&TokenKind::Colon)?;
        let body = match self.parse_statement() {
            Ok(BlockItem::Statement(stmt)) => stmt,
            _ => {
                return Err(UccError {
                    kind: ErrorKind::Parse,
                    msg: format!("internal error, parse case"),
                    span: self.current_span()?,
                });
            }
        };
        let end = self.current_span()?;
        Ok(BlockItem::Statement(Statement::Case(CaseStatement {
            value,
            body: body.into(),
            label: String::new(),
            span: begin + end,
        })))
    }

    fn parse_default_statement(&mut self) -> Result<BlockItem> {
        let begin = self.current_span()?;
        self.consume(&TokenKind::Colon)?;
        let body = match self.parse_statement() {
            Ok(BlockItem::Statement(stmt)) => stmt,
            _ => {
                return Err(UccError {
                    kind: ErrorKind::Parse,
                    msg: format!("internal error, parse_default"),
                    span: self.current_span()?,
                });
            }
        };
        let end = self.current_span()?;
        Ok(BlockItem::Statement(Statement::Default(DefaultStatement {
            body: body.into(),
            label: String::new(),
            span: begin + end,
        })))
    }

    fn parse_if_statement(&mut self) -> Result<BlockItem> {
        let begin = self.current_span()?;
        self.consume(&TokenKind::LParen)?;
        let condition = self.parse_expression()?;
        self.consume(&TokenKind::RParen)?;
        let then_branch = self.parse_statement()?;

        if let BlockItem::Declaration(_) = then_branch {
            return Err(UccError {
                kind: ErrorKind::Parse,
                msg: format!("Variable declarations not allowed in if body."),
                span: self.current_span()?,
            });
        }

        let else_branch = if self.is_next(&[TokenKind::Else]) {
            Some(self.parse_statement()?)
        } else {
            None
        };

        let end = self.current_span()?;

        Ok(BlockItem::Statement(Statement::If(IfStatement {
            condition,
            then_branch: then_branch.into(),
            else_branch: else_branch.into(),
            span: begin + end,
        })))
    }

    fn parse_do_while_statement(&mut self) -> Result<BlockItem> {
        let begin = self.current_span()?;
        let body = self.parse_statement()?;
        self.consume(&TokenKind::While)?;
        self.consume(&TokenKind::LParen)?;
        let condition = self.parse_expression()?;
        self.consume(&TokenKind::RParen)?;
        self.consume(&TokenKind::Semicolon)?;
        let end = self.current_span()?;

        Ok(BlockItem::Statement(Statement::DoWhile(DoWhileStatement {
            condition,
            body: body.into(),
            label: "".to_owned(),
            span: begin + end,
        })))
    }

    fn parse_while_statement(&mut self) -> Result<BlockItem> {
        let begin = self.current_span()?;
        self.consume(&TokenKind::LParen)?;
        let condition = self.parse_expression()?;
        self.consume(&TokenKind::RParen)?;
        let body = self.parse_statement()?;

        if let BlockItem::Declaration(_) = body {
            return Err(UccError {
                kind: ErrorKind::Parse,
                msg: format!("Variable declarations not allowed in while body."),
                span: self.current_span()?,
            });
        }

        let end = self.current_span()?;

        Ok(BlockItem::Statement(Statement::While(WhileStatement {
            condition,
            body: body.into(),
            label: "".to_owned(),
            span: begin + end,
        })))
    }

    fn parse_for_statement(&mut self) -> Result<BlockItem> {
        let begin = self.current_span()?;
        self.consume(&TokenKind::LParen)?;

        let init = if self.is_next(&[TokenKind::Semicolon]) {
            ForInit::Expression(None)
        } else if self.starts_declaration() {
            let decl = self.parse_var_or_fn_decl()?;
            ForInit::Declaration(match decl {
                BlockItem::Declaration(Declaration::Variable(var)) => var,
                _ => {
                    return Err(UccError {
                        kind: ErrorKind::Parse,
                        msg: format!("Function declarations are not allowed in for loop headers."),
                        span: self.current_span()?,
                    })
                }
            })
        } else {
            let expr = self.parse_expression()?;
            self.consume(&TokenKind::Semicolon)?;
            ForInit::Expression(Some(expr))
        };

        let condition = if self.is_next(&[TokenKind::Semicolon]) {
            None
        } else {
            let expr = self.parse_expression()?;
            self.consume(&TokenKind::Semicolon)?;
            Some(expr)
        };

        let post = if self.is_next(&[TokenKind::RParen]) {
            None
        } else {
            let expr = self.parse_expression()?;
            self.consume(&TokenKind::RParen)?;
            Some(expr)
        };

        let body = self.parse_statement()?;

        let end = self.current_span()?;

        Ok(BlockItem::Statement(Statement::For(ForStatement {
            init,
            condition,
            post,
            body: body.into(),
            label: "".to_owned(),
            span: begin + end,
        })))
    }

    fn parse_break_statement(&mut self) -> Result<BlockItem> {
        let begin = self.current_span()?;
        self.consume(&TokenKind::Semicolon)?;
        let end = self.current_span()?;
        Ok(BlockItem::Statement(Statement::Break(BreakStatement {
            label: "".to_owned(),
            span: begin + end,
        })))
    }

    fn parse_continue_statement(&mut self) -> Result<BlockItem> {
        let begin = self.current_span()?;
        self.consume(&TokenKind::Semicolon)?;
        let end = self.current_span()?;
        Ok(BlockItem::Statement(Statement::Continue(
            ContinueStatement {
                label: "".to_owned(),
                span: begin + end,
            },
        )))
    }

    fn parse_goto_statement(&mut self) -> Result<BlockItem> {
        let begin = self.current_span()?;
        let label_token = self.consume(&TokenKind::Identifier("".to_string()))?;
        let label = if let Some(label_name) = label_token {
            label_name.as_string()
        } else {
            return Err(UccError {
                kind: ErrorKind::Parse,
                msg: format!("internal error, parse_goto"),
                span: self.current_span()?,
            });
        };
        self.consume(&TokenKind::Semicolon)?;
        let end = self.current_span()?;
        Ok(BlockItem::Statement(Statement::Goto(GotoStatement {
            label,
            span: begin + end,
        })))
    }

    fn parse_labeled_statement(&mut self) -> Result<BlockItem> {
        let begin = self.current_span()?;
        let label_token = self.consume(&TokenKind::Identifier("".to_string()))?;
        let label = if let Some(label_name) = label_token {
            label_name.as_string()
        } else {
            return Err(UccError {
                kind: ErrorKind::Parse,
                msg: format!("Expected label identifier."),
                span: self.current_span()?,
            });
        };
        self.consume(&TokenKind::Colon)?;

        let inner = self.parse_statement()?;

        let end = self.current_span()?;

        match inner {
            BlockItem::Statement(stmt) => {
                Ok(BlockItem::Statement(Statement::Labeled(LabeledStatement {
                    label,
                    body: Box::new(stmt),
                    span: begin + end,
                })))
            }
            _ => {
                return Err(UccError {
                    kind: ErrorKind::Parse,
                    msg: format!("Label must precede a statement, not a declaration"),
                    span: self.current_span()?,
                })
            }
        }
    }

    fn parse_return_statement(&mut self) -> Result<BlockItem> {
        let begin = self.current_span()?;
        let expr = if self.is_next(&[TokenKind::Semicolon]) {
            None
        } else {
            let expr = Some(self.parse_expression()?);
            self.consume(&TokenKind::Semicolon)?;
            expr
        };
        let end = self.current_span()?;
        Ok(BlockItem::Statement(Statement::Return(ReturnStatement {
            expr,
            target_type: self.current_target_type.clone(),
            belongs_to: self
                .current_fn
                .clone()
                .unwrap_or_else(|| "no current function".to_owned()),
            span: begin + end,
        })))
    }

    fn parse_expression(&mut self) -> Result<Expression> {
        self.assignment()
    }

    fn current_span(&self) -> Result<Span> {
        match self.current.as_ref() {
            Some(token) => Ok(token.span),
            None => {
                return Ok(Span { start: 0, end: 0 });
            }
        }
    }

    fn assignment(&mut self) -> Result<Expression> {
        let begin = self.current_span()?;

        let mut result = self.conditional()?;
        while self.is_next(&[
            TokenKind::Equal,
            TokenKind::PlusEqual,
            TokenKind::MinusEqual,
            TokenKind::StarEqual,
            TokenKind::SlashEqual,
            TokenKind::ModEqual,
            TokenKind::AmpersandEqual,
            TokenKind::PipeEqual,
            TokenKind::CaretEqual,
            TokenKind::GreaterGreaterEqual,
            TokenKind::LessLessEqual,
        ]) {
            if let Some(token) = self.previous.as_ref() {
                if let TokenKind::Equal = token.kind {
                    let right = self.assignment()?;
                    let end = self.current_span()?;

                    result = Expression::Assign(AssignExpression {
                        lhs: result.into(),
                        rhs: right.into(),
                        op: TokenKind::Equal,
                        ty: Type::Dummy,
                        span: begin + end,
                    });
                } else {
                    let op = match self.previous.as_ref() {
                        Some(token) => match token.kind {
                            TokenKind::PlusEqual => CompoundExpressionKind::Add,
                            TokenKind::MinusEqual => CompoundExpressionKind::Sub,
                            TokenKind::StarEqual => CompoundExpressionKind::Mul,
                            TokenKind::SlashEqual => CompoundExpressionKind::Div,
                            TokenKind::CaretEqual => CompoundExpressionKind::BitwiseXor,
                            TokenKind::AmpersandEqual => CompoundExpressionKind::BitwiseAnd,
                            TokenKind::PipeEqual => CompoundExpressionKind::BitwiseOr,
                            TokenKind::GreaterGreaterEqual => CompoundExpressionKind::BitwiseShr,
                            TokenKind::LessLessEqual => CompoundExpressionKind::BitwiseShl,
                            TokenKind::ModEqual => CompoundExpressionKind::Mod,
                            _ => unreachable!(),
                        },
                        None => {
                            return Err(UccError {
                                kind: ErrorKind::Parse,
                                msg: format!("internal error, assignment"),
                                span: self.current_span()?,
                            })
                        }
                    };

                    let right = self.assignment()?;
                    let end = self.current_span()?;

                    result = Expression::Compound(CompoundExpression {
                        kind: op,
                        lhs: result.into(),
                        rhs: right.into(),
                        result_t: Type::Dummy,
                        ty: Type::Dummy,
                        span: begin + end,
                    });
                }
            }
        }
        Ok(result)
    }

    fn conditional(&mut self) -> Result<Expression> {
        let begin = self.current_span()?;
        let mut result = self.or()?;

        if self.is_next(&[TokenKind::QuestionMark]) {
            let then_expr = self.parse_expression()?;
            self.consume(&TokenKind::Colon)?;
            let else_expr = self.conditional()?;
            let end = self.current_span()?;
            result = Expression::Conditional(ConditionalExpression {
                condition: result.into(),
                then_expr: then_expr.into(),
                else_expr: else_expr.into(),
                ty: Type::Dummy,
                span: begin + end,
            });
        }
        Ok(result)
    }

    fn or(&mut self) -> Result<Expression> {
        let begin = self.current_span()?;
        let mut result = self.and()?;
        while self.is_next(&[TokenKind::DoublePipe]) {
            let right = self.and()?;
            let end = self.current_span()?;
            result = Expression::Binary(BinaryExpression {
                kind: BinaryExpressionKind::Or,
                lhs: result.into(),
                rhs: right.into(),
                ty: Type::Dummy,
                span: begin + end,
            });
        }
        Ok(result)
    }

    fn and(&mut self) -> Result<Expression> {
        let begin = self.current_span()?;
        let mut result = self.bitwise_or()?;
        while self.is_next(&[TokenKind::DoubleAmpersand]) {
            let right = self.bitwise_or()?;
            let end = self.current_span()?;
            result = Expression::Binary(BinaryExpression {
                kind: BinaryExpressionKind::And,
                lhs: result.into(),
                rhs: right.into(),
                ty: Type::Dummy,
                span: begin + end,
            });
        }
        Ok(result)
    }

    fn bitwise_or(&mut self) -> Result<Expression> {
        let begin = self.current_span()?;
        let mut result = self.bitwise_xor()?;
        while self.is_next(&[TokenKind::Pipe]) {
            let right = self.bitwise_xor()?;
            let end = self.current_span()?;
            result = Expression::Binary(BinaryExpression {
                kind: BinaryExpressionKind::BitwiseOr,
                lhs: result.into(),
                rhs: right.into(),
                ty: Type::Dummy,
                span: begin + end,
            });
        }
        Ok(result)
    }

    fn bitwise_xor(&mut self) -> Result<Expression> {
        let begin = self.current_span()?;
        let mut result = self.bitwise_and()?;
        while self.is_next(&[TokenKind::Caret]) {
            let right = self.bitwise_and()?;
            let end = self.current_span()?;
            result = Expression::Binary(BinaryExpression {
                kind: BinaryExpressionKind::BitwiseXor,
                lhs: result.into(),
                rhs: right.into(),
                ty: Type::Dummy,
                span: begin + end,
            });
        }
        Ok(result)
    }

    fn bitwise_and(&mut self) -> Result<Expression> {
        let begin = self.current_span()?;
        let mut result = self.equality()?;
        while self.is_next(&[TokenKind::Ampersand]) {
            let right = self.equality()?;
            let end = self.current_span()?;
            result = Expression::Binary(BinaryExpression {
                kind: BinaryExpressionKind::BitwiseAnd,
                lhs: result.into(),
                rhs: right.into(),
                ty: Type::Dummy,
                span: begin + end,
            });
        }
        Ok(result)
    }

    fn equality(&mut self) -> Result<Expression> {
        let begin = self.current_span()?;
        let mut result = self.relational()?;
        while self.is_next(&[TokenKind::DoubleEqual, TokenKind::BangEqual]) {
            let negation = match self.previous.as_ref().unwrap().kind {
                TokenKind::BangEqual => true,
                TokenKind::DoubleEqual => false,
                _ => unreachable!(),
            };
            let right = self.relational()?;
            let end = self.current_span()?;
            result = Expression::Binary(BinaryExpression {
                kind: match negation {
                    true => BinaryExpressionKind::NotEqual,
                    false => BinaryExpressionKind::Equal,
                },
                lhs: result.into(),
                rhs: right.into(),
                ty: Type::Dummy,
                span: begin + end,
            });
        }
        Ok(result)
    }

    fn relational(&mut self) -> Result<Expression> {
        let begin = self.current_span()?;
        let mut result = self.bitwise_shift()?;
        while self.is_next(&[
            TokenKind::Less,
            TokenKind::Greater,
            TokenKind::LessEqual,
            TokenKind::GreaterEqual,
        ]) {
            let kind = match self.previous.as_ref() {
                Some(token) => match token.kind {
                    TokenKind::Less => BinaryExpressionKind::Less,
                    TokenKind::Greater => BinaryExpressionKind::Greater,
                    TokenKind::LessEqual => BinaryExpressionKind::LessEqual,
                    TokenKind::GreaterEqual => BinaryExpressionKind::GreaterEqual,
                    _ => unreachable!(),
                },
                None => unreachable!(),
            };
            let right = self.bitwise_shift()?;
            let end = self.current_span()?;
            result = Expression::Binary(BinaryExpression {
                kind,
                lhs: result.into(),
                rhs: right.into(),
                ty: Type::Dummy,
                span: begin + end,
            });
        }
        Ok(result)
    }

    fn bitwise_shift(&mut self) -> Result<Expression> {
        let begin = self.current_span()?;
        let mut result = self.term()?;
        while self.is_next(&[TokenKind::GreaterGreater, TokenKind::LessLess]) {
            let kind = match self.previous.as_ref() {
                Some(token) => match token.kind {
                    TokenKind::GreaterGreater => BinaryExpressionKind::BitwiseShr,
                    TokenKind::LessLess => BinaryExpressionKind::BitwiseShl,
                    _ => unreachable!(),
                },
                None => unreachable!(),
            };
            let right = self.term()?;
            let end = self.current_span()?;
            result = Expression::Binary(BinaryExpression {
                kind,
                lhs: result.into(),
                rhs: right.into(),
                ty: Type::Dummy,
                span: begin + end,
            });
        }
        Ok(result)
    }

    fn term(&mut self) -> Result<Expression> {
        let begin = self.current_span()?;
        let mut result = self.factor()?;
        while self.is_next(&[TokenKind::Plus, TokenKind::Hyphen]) {
            let kind = match self.previous.as_ref() {
                Some(token) => match token.kind {
                    TokenKind::Plus => BinaryExpressionKind::Add,
                    TokenKind::Hyphen => BinaryExpressionKind::Sub,
                    _ => unreachable!(),
                },
                None => unreachable!(),
            };
            let right = self.factor()?;
            let end = self.current_span()?;
            result = Expression::Binary(BinaryExpression {
                kind,
                lhs: result.into(),
                rhs: right.into(),
                ty: Type::Dummy,
                span: begin + end,
            });
        }
        Ok(result)
    }

    fn factor(&mut self) -> Result<Expression> {
        let begin = self.current_span()?;
        let mut result = self.unary()?;
        while self.is_next(&[TokenKind::Star, TokenKind::Slash, TokenKind::Percent]) {
            let kind = match self.previous.as_ref() {
                Some(token) => match token.kind {
                    TokenKind::Star => BinaryExpressionKind::Mul,
                    TokenKind::Slash => BinaryExpressionKind::Div,
                    TokenKind::Percent => BinaryExpressionKind::Rem,
                    _ => unreachable!(),
                },
                None => unreachable!(),
            };
            let right = self.unary()?;
            let end = self.current_span()?;
            result = Expression::Binary(BinaryExpression {
                kind,
                lhs: result.into(),
                rhs: right.into(),
                ty: Type::Dummy,
                span: begin + end,
            });
        }
        Ok(result)
    }

    fn peek(&self, n: usize) -> Vec<Token> {
        if let Some(current) = &self.current {
            let mut v = vec![current.clone()];
            v.extend(self.tokens.iter().take(n - 1).cloned().collect::<Vec<_>>());
            v
        } else {
            vec![]
        }
    }

    fn unary(&mut self) -> Result<Expression> {
        let begin = self.current_span()?;

        if self.is_next(&[
            TokenKind::Hyphen,
            TokenKind::Tilde,
            TokenKind::Bang,
            TokenKind::DoublePlus,
            TokenKind::DoubleHyphen,
        ]) {
            let op = self.previous.clone().unwrap();
            let expr = self.unary()?;
            let end = self.current_span()?;
            return Ok(Expression::Unary(UnaryExpression {
                expr: expr.into(),
                kind: match op.kind {
                    TokenKind::Hyphen => UnaryExpressionKind::Negate,
                    TokenKind::Tilde => UnaryExpressionKind::Complement,
                    TokenKind::Bang => UnaryExpressionKind::Not,
                    TokenKind::DoublePlus => UnaryExpressionKind::Inc,
                    TokenKind::DoubleHyphen => UnaryExpressionKind::Dec,
                    _ => unreachable!(),
                },
                ty: Type::Dummy,
                span: begin + end,
            }));
        } else if self.is_next(&[TokenKind::Star]) {
            let expr = self.unary()?;
            let end = self.current_span()?;
            return Ok(Expression::Deref(DerefExpression {
                expr: expr.into(),
                ty: Type::Dummy,
                span: begin + end,
            }));
        } else if self.is_next(&[TokenKind::Ampersand]) {
            let expr = self.unary()?;
            let end = self.current_span()?;
            return Ok(Expression::AddrOf(AddrOfExpression {
                expr: expr.into(),
                ty: Type::Dummy,
                span: begin + end,
            }));
        } else {
            let next_three_tokens = self.peek(3);
            match next_three_tokens
                .iter()
                .cloned()
                .map(|t| t.kind)
                .collect::<Vec<_>>()
                .as_slice()
            {
                [TokenKind::Sizeof, TokenKind::LParen, _] => {
                    if self.is_type_specifier(&next_three_tokens.last().unwrap().kind) {
                        self.consume(&TokenKind::Sizeof)?;
                        self.consume(&TokenKind::LParen)?;
                        let base_type = self.parse_type_name()?;
                        self.consume(&TokenKind::RParen)?;
                        let end = self.current_span()?;
                        return Ok(Expression::SizeofT(SizeofTExpression {
                            t: base_type,
                            ty: Type::Dummy,
                            span: begin + end,
                        }));
                    } else {
                        self.consume(&TokenKind::Sizeof)?;
                        let expr = self.unary()?;
                        let end = self.current_span()?;
                        return Ok(Expression::Sizeof(SizeofExpression {
                            expr: expr.into(),
                            ty: Type::Dummy,
                            span: begin + end,
                        }));
                    }
                }
                [TokenKind::Sizeof, _, _] => {
                    self.consume(&TokenKind::Sizeof)?;
                    let expr = self.unary()?;
                    let end = self.current_span()?;
                    return Ok(Expression::Sizeof(SizeofExpression {
                        expr: expr.into(),
                        ty: Type::Dummy,
                        span: begin + end,
                    }));
                }
                [TokenKind::LParen, _, _] => {
                    if self.is_type_specifier(&next_three_tokens[1].kind) {
                        self.consume(&TokenKind::LParen)?;
                        let base_type = self.parse_type_name()?;
                        self.consume(&TokenKind::RParen)?;
                        let expr = self.unary()?;
                        let end = self.current_span()?;
                        return Ok(Expression::Cast(CastExpression {
                            target_type: base_type,
                            expr: expr.into(),
                            ty: Type::Dummy,
                            span: begin + end,
                        }));
                    }
                }
                _ => {}
            }
        }

        self.call()
    }

    fn parse_type_name(&mut self) -> Result<Type> {
        let specifier_list = self.consume_while_type_specifier();
        let base_type = self.parse_type(
            specifier_list
                .iter()
                .cloned()
                .map(|t| t.kind)
                .collect::<Vec<_>>(),
        )?;
        match self.current.as_ref().unwrap().kind {
            TokenKind::RParen => Ok(base_type),
            _ => {
                let decl = self.parse_abstract_declarator()?;
                Ok(self.process_abstract_declarator(&decl, &base_type))
            }
        }
    }

    fn lookahead_until(&mut self, token: &TokenKind) -> Vec<Token> {
        let v = [self.current.clone().unwrap()];
        v.iter()
            .chain(&self.tokens)
            .take_while(|t| t.kind != *token)
            .cloned()
            .collect()
    }

    fn call(&mut self) -> Result<Expression> {
        let begin = self.current_span()?;
        let mut expr = self.primary()?;
        loop {
            if self.is_next(&[TokenKind::LParen]) {
                let mut args = vec![];
                if !self.check(&TokenKind::RParen) {
                    loop {
                        args.push(self.parse_expression()?);
                        if !self.is_next(&[TokenKind::Comma]) {
                            break;
                        }
                    }
                }
                self.consume(&TokenKind::RParen)?;
                let end = self.current_span()?;
                expr = Expression::Call(CallExpression {
                    name: match expr {
                        Expression::Variable(var) => var.value,
                        _ => {
                            return Err(UccError {
                                msg: format!("expected a variable"),
                                kind: ErrorKind::Parse,
                                span: self.current_span()?,
                            })
                        }
                    },
                    args,
                    ty: Type::Dummy,
                    span: begin + end,
                });
            } else if self.is_next(&[TokenKind::LBracket]) {
                let index = self.parse_expression()?;
                let end = self.current_span()?;
                self.consume(&TokenKind::RBracket)?;
                expr = Expression::Subscript(SubscriptExpression {
                    expr: expr.into(),
                    index: index.into(),
                    ty: Type::Dummy,
                    span: begin + end,
                });
            } else if self.is_next(&[TokenKind::Dot]) {
                let member = self
                    .consume(&TokenKind::Identifier("".to_owned()))?
                    .unwrap()
                    .as_string();
                let end = self.current_span()?;
                expr = Expression::Dot(DotExpression {
                    structure: expr.into(),
                    member,
                    ty: Type::Dummy,
                    span: begin + end,
                });
            } else if self.is_next(&[TokenKind::Arrow]) {
                let member = self
                    .consume(&TokenKind::Identifier("".to_owned()))?
                    .unwrap()
                    .as_string();
                let end = self.current_span()?;
                expr = Expression::Arrow(ArrowExpression {
                    pointer: expr.into(),
                    member,
                    ty: Type::Dummy,
                    span: begin + end,
                });
            } else if self.is_next(&[TokenKind::DoublePlus, TokenKind::DoubleHyphen]) {
                let kind = match self.previous.as_ref() {
                    Some(token) => match token.kind {
                        TokenKind::DoublePlus => PostfixExpressionKind::Inc,
                        TokenKind::DoubleHyphen => PostfixExpressionKind::Dec,
                        _ => unreachable!(),
                    },
                    None => {
                        return Err(UccError {
                            kind: ErrorKind::Parse,
                            msg: format!("internal error, call"),
                            span: self.current_span()?,
                        })
                    }
                };

                let end = self.current_span()?;

                expr = Expression::Postfix(PostfixExpression {
                    kind,
                    expr: expr.into(),
                    ty: Type::Dummy,
                    span: begin + end,
                });
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expression> {
        let begin = self.current_span()?;
        if self.is_next(&[
            TokenKind::Constant(Const::Int(0)),
            TokenKind::Constant(Const::Long(0)),
        ]) {
            match self.previous.as_ref().unwrap().kind {
                TokenKind::Constant(n) => self.parse_number(&n, begin),
                _ => unreachable!(),
            }
        } else if self.is_next(&[TokenKind::LParen]) {
            self.parse_grouping()
        } else if self.is_next(&[TokenKind::Identifier("".to_owned())]) {
            match self.previous.as_ref().unwrap().kind {
                TokenKind::Identifier(ref var) => self.parse_variable(var, begin),
                _ => unreachable!(),
            }
        } else if self.is_next(&[TokenKind::LBrace]) {
            let mut inits = vec![];
            loop {
                if self.is_next(&[TokenKind::RBrace]) {
                    break;
                }
                inits.push(Initializer::Single(String::new(), self.parse_expression()?));

                if self.is_next(&[TokenKind::Comma]) {
                    continue;
                }
            }
            if inits.is_empty() {
                return Err(UccError {
                    msg: format!("empty compound literal"),
                    kind: ErrorKind::Parse,
                    span: self.current_span()?,
                });
            }
            let end = self.current_span()?;
            Ok(Expression::Literal(LiteralExpression {
                name: String::new(),
                value: Initializer::Compound(String::new(), Type::Dummy, inits).into(),
                ty: Type::Dummy,
                span: begin + end,
            }))
        } else if self.is_next(&[TokenKind::CharLiteral('a')]) {
            match self.previous.as_ref().unwrap().kind {
                TokenKind::CharLiteral(c) => self.parse_char(&c, begin),
                _ => unreachable!(),
            }
        } else if self.is_next(&[TokenKind::StringLiteral("".to_owned())]) {
            match self.previous.as_ref().cloned().unwrap().kind {
                TokenKind::StringLiteral(ref s) => self.parse_string(s, begin),
                _ => unreachable!(),
            }
        } else {
            return Err(UccError {
                msg: format!("expected primary"),
                kind: ErrorKind::Parse,
                span: self.current_span()?,
            });
        }
    }

    fn parse_number(&self, n: &Const, begin: Span) -> Result<Expression> {
        let end = self.current_span()?;
        Ok(Expression::Constant(ConstantExpression {
            value: *n,
            ty: Type::Dummy,
            span: begin + end,
        }))
    }

    fn parse_variable(&self, var: &str, begin: Span) -> Result<Expression> {
        let end = self.current_span()?;
        Ok(Expression::Variable(VariableExpression {
            value: var.to_owned(),
            ty: Type::Dummy,
            span: begin + end,
        }))
    }

    fn parse_char(&self, c: &char, begin: Span) -> Result<Expression> {
        let end = self.current_span()?;
        Ok(Expression::Constant(ConstantExpression {
            value: Const::Int(*c as i32),
            ty: Type::Dummy,
            span: begin + end,
        }))
    }

    fn parse_string(&mut self, s: &str, begin: Span) -> Result<Expression> {
        let mut s = s.to_owned().clone();
        while self.is_next(&[TokenKind::StringLiteral("".to_owned())]) {
            match self.previous.as_ref().unwrap().kind {
                TokenKind::StringLiteral(ref s2) => s.push_str(s2),
                _ => unreachable!(),
            }
        }

        let end = match self.previous.as_ref() {
            Some(token) => token.span,
            None => {
                return Err(UccError {
                    kind: ErrorKind::Parse,
                    msg: format!("internal error, parse_string"),
                    span: Span { start: 0, end: 0 },
                })
            }
        };

        Ok(Expression::String(StringExpression {
            value: s.to_owned(),
            ty: Type::Dummy,
            span: begin + end,
        }))
    }

    fn parse_grouping(&mut self) -> Result<Expression> {
        let expr = self.parse_expression();
        self.consume(&TokenKind::RParen)?;
        expr
    }

    fn parse_abstract_declarator(&mut self) -> Result<AbstractDeclarator> {
        match self.current.as_ref().unwrap().kind {
            TokenKind::Star => {
                self.consume(&TokenKind::Star)?;
                let inner = match self.current.as_ref().unwrap().kind {
                    TokenKind::Star | TokenKind::LParen | TokenKind::LBracket => {
                        self.parse_abstract_declarator()?
                    }
                    _ => AbstractDeclarator::Base,
                };
                Ok(AbstractDeclarator::Pointer(Box::new(inner)))
            }
            _ => self.parse_direct_abstract_declarator(),
        }
    }

    fn parse_direct_abstract_declarator(&mut self) -> Result<AbstractDeclarator> {
        match self.current.as_ref().unwrap().kind {
            TokenKind::LParen => {
                self.consume(&TokenKind::LParen)?;
                let inner = self.parse_abstract_declarator()?;
                self.consume(&TokenKind::RParen)?;

                if let TokenKind::LBracket = self.current.as_ref().unwrap().kind {
                    self.parse_abstract_array_decl_suffix(&inner)
                } else {
                    Ok(inner)
                }
            }
            _ => self.parse_abstract_array_decl_suffix(&AbstractDeclarator::Base),
        }
    }

    fn process_abstract_declarator(&self, decl: &AbstractDeclarator, base_type: &Type) -> Type {
        match decl {
            AbstractDeclarator::Base => base_type.clone(),
            AbstractDeclarator::Pointer(inner) => {
                let derived_type = Type::Pointer(base_type.clone().into());
                self.process_abstract_declarator(inner, &derived_type)
            }
            AbstractDeclarator::Array(inner, size) => {
                let derived_type = Type::Array {
                    element: Box::new(base_type.clone()),
                    size: *size,
                };
                self.process_abstract_declarator(inner, &derived_type)
            }
        }
    }

    fn parse_abstract_array_decl_suffix(
        &mut self,
        base_decl: &AbstractDeclarator,
    ) -> Result<AbstractDeclarator> {
        let dim = self.parse_dim()?;
        let new_decl = AbstractDeclarator::Array(Box::new(base_decl.clone()), dim);

        match self.current.as_ref() {
            Some(token) => match token.kind {
                TokenKind::LBracket => self.parse_abstract_array_decl_suffix(&new_decl),
                _ => Ok(new_decl),
            },
            None => {
                return Err(UccError {
                    kind: ErrorKind::Parse,
                    msg: format!("itnernnl error"),
                    span: self.current_span()?,
                })
            }
        }
    }
}

#[cfg(test)]
mod short_tests {
    use super::*;
    use crate::lexer::lex::{Const, Lexer, TokenKind};
    use crate::parser::ast::{AggregateKind, BlockItem, Declaration, Program, Type};
    use crate::semantics::typechecker::{get_common_type, get_signedness, get_size_of_type};
    use std::collections::VecDeque;

    fn parse(src: &str) -> Program {
        let tokens: VecDeque<_> = Lexer::new(src.to_string()).collect();
        let mut parser = Parser::new(tokens);
        parser.parse().unwrap()
    }

    fn parse_errs(src: &str) {
        let tokens: VecDeque<_> = Lexer::new(src.to_string()).collect();
        let mut parser = Parser::new(tokens);
        assert!(parser.parse().is_err());
    }

    fn top_level_var_types(src: &str) -> Vec<Type> {
        parse(src)
            .block_items
            .into_iter()
            .map(|item| match item {
                BlockItem::Declaration(Declaration::Variable(var)) => var.ty,
                other => panic!("expected variable declaration, got {other:?}"),
            })
            .collect()
    }

    #[test]
    fn parses_signed_short_spellings() {
        let tys = top_level_var_types(
            r#"
            short s;
            signed short ss;
            short int si;
            signed short int ssi;
            "#,
        );

        assert_eq!(tys, vec![Type::Short, Type::Short, Type::Short, Type::Short]);
    }

    #[test]
    fn parses_unsigned_short_spellings() {
        let tys = top_level_var_types(
            r#"
            unsigned short us;
            unsigned short int usi;
            short unsigned su;
            short unsigned int sui;
            "#,
        );

        assert_eq!(tys, vec![Type::UShort, Type::UShort, Type::UShort, Type::UShort]);
    }

    #[test]
    fn rejects_invalid_short_combinations() {
        parse_errs("short long x;");
        parse_errs("signed unsigned short x;");
        parse_errs("double short x;");
    }

    #[test]
    fn short_size_signedness_and_promotions() {
        assert_eq!(get_size_of_type(&Type::Short), 2);
        assert_eq!(get_size_of_type(&Type::UShort), 2);
        assert!(get_signedness(&Type::Short));
        assert!(!get_signedness(&Type::UShort));

        assert_eq!(get_common_type(&Type::Short, &Type::Short), &Type::Int);
        assert_eq!(get_common_type(&Type::Short, &Type::UShort), &Type::Int);
        assert_eq!(get_common_type(&Type::UShort, &Type::UShort), &Type::Int);
    }


    #[test]
    fn lexes_and_parses_union_declaration_and_type_references() {
        let tokens: Vec<_> = Lexer::new("union U { int i; double d; }; union U u;".to_string()).collect();
        assert!(tokens.iter().any(|token| matches!(token.kind, TokenKind::Union)));

        let program = parse("union U { int i; double d; }; union U u; union U *p;");
        assert!(matches!(
            &program.block_items[0],
            BlockItem::Declaration(Declaration::Union(decl))
                if decl.tag == "U" && decl.kind == AggregateKind::Union && decl.members.len() == 2
        ));

        assert!(matches!(
            &program.block_items[1],
            BlockItem::Declaration(Declaration::Variable(var))
                if var.ty == (Type::Union { tag: "U".to_string() })
        ));
        assert!(matches!(
            &program.block_items[2],
            BlockItem::Declaration(Declaration::Variable(var))
                if var.ty == Type::Pointer(Box::new(Type::Union { tag: "U".to_string() }))
        ));
    }


    #[test]
    fn lexes_and_parses_enum_declaration_and_type_references() {
        let tokens: Vec<_> = Lexer::new("enum Color { RED, GREEN = 5, BLUE, }; enum Color c;".to_string()).collect();
        assert!(tokens.iter().any(|token| matches!(token.kind, TokenKind::Enum)));

        let program = parse("enum Color { RED, GREEN = 5, BLUE, }; enum Color c; enum Color *p;");
        assert!(matches!(
            &program.block_items[0],
            BlockItem::Declaration(Declaration::Enum(decl))
                if decl.tag == Some("Color".to_string()) && decl.members.len() == 3
        ));

        assert!(matches!(
            &program.block_items[1],
            BlockItem::Declaration(Declaration::Variable(var))
                if var.ty == (Type::Enum { tag: "Color".to_string() })
        ));
        assert!(matches!(
            &program.block_items[2],
            BlockItem::Declaration(Declaration::Variable(var))
                if var.ty == Type::Pointer(Box::new(Type::Enum { tag: "Color".to_string() }))
        ));
    }

    #[test]
    fn parses_float_type_and_rejects_invalid_float_combinations() {
        let tys = top_level_var_types(
            r#"
            float f;
            double d;
            "#,
        );

        assert_eq!(tys, vec![Type::Float, Type::Double]);

        parse_errs("float double x;");
        parse_errs("unsigned float x;");
        parse_errs("float int x;");
        parse_errs("short float x;");
    }

    #[test]
    fn lexes_f_suffix_as_float_constant() {
        let tokens: Vec<_> = Lexer::new("float f = 1.25f; double d = 2.5;".to_string()).collect();

        assert!(tokens.iter().any(|token| {
            matches!(&token.kind, TokenKind::Constant(Const::Float(f)) if *f == 1.25)
        }));
        assert!(tokens.iter().any(|token| {
            matches!(&token.kind, TokenKind::Constant(Const::Double(d)) if *d == 2.5)
        }));
    }

    #[test]
    fn float_size_and_usual_arithmetic_conversions() {
        assert_eq!(get_size_of_type(&Type::Float), 4);
        assert_eq!(get_common_type(&Type::Float, &Type::Int), &Type::Float);
        assert_eq!(get_common_type(&Type::Float, &Type::Double), &Type::Double);
        assert_eq!(get_common_type(&Type::Float, &Type::UShort), &Type::Float);
    }


    #[test]
    fn lexes_typedef_and_variadic_ellipsis() {
        let tokens: Vec<_> = Lexer::new("typedef int I; int log(I code, ...);".to_string()).collect();

        assert!(tokens.iter().any(|token| matches!(token.kind, TokenKind::Typedef)));
        assert!(tokens.iter().any(|token| matches!(token.kind, TokenKind::Ellipsis)));
    }

    #[test]
    fn parses_typedef_aliases_as_type_specifiers() {
        let program = parse("typedef int I; I value; typedef I J; J *ptr;");

        assert!(matches!(
            &program.block_items[0],
            BlockItem::Declaration(Declaration::Typedef(decl))
                if decl.name == "I" && decl.ty == Type::Int
        ));
        assert!(matches!(
            &program.block_items[1],
            BlockItem::Declaration(Declaration::Variable(var))
                if var.ty == Type::Int
        ));
        assert!(matches!(
            &program.block_items[2],
            BlockItem::Declaration(Declaration::Typedef(decl))
                if decl.name == "J" && decl.ty == Type::Int
        ));
        assert!(matches!(
            &program.block_items[3],
            BlockItem::Declaration(Declaration::Variable(var))
                if var.ty == Type::Pointer(Box::new(Type::Int))
        ));
    }

    #[test]
    fn parses_variadic_function_types() {
        let program = parse("typedef int I; int log(I code, double value, ...);");

        assert!(matches!(
            &program.block_items[1],
            BlockItem::Declaration(Declaration::Function(func))
                if func.params == vec!["code".to_string(), "value".to_string()]
                    && func.ty == (Type::Func {
                        params: vec![Type::Int, Type::Double],
                        ret: Box::new(Type::Int),
                        variadic: true,
                    })
        ));
    }

    #[test]
    fn rejects_ellipsis_without_named_parameter() {
        parse_errs("int bad(...);");
    }

    #[test]
    fn ordinary_identifiers_can_shadow_typedef_names_in_inner_scopes() {
        parse("typedef int T; int main(void) { long T; T = 1L; return (int) T; }");
    }

}
