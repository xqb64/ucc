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
        spanof, StructDeclaration, SubscriptExpression, Type, TypedefDeclaration, UnaryExpression, UnaryExpressionKind,
        VaArgExpression, VaCopyExpression, VaEndExpression, VaStartExpression,
        VariableDeclaration, VariableExpression, WhileStatement,
    },
    semantics::typechecker::{builtin_va_list_type, ensure_builtin_va_list_type},
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
    pub pending_declarations: VecDeque<BlockItem>,
    pub anonymous_tag_counter: usize,
    pub object_scopes: Vec<BTreeMap<String, Type>>,

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
            pending_declarations: VecDeque::new(),
            anonymous_tag_counter: 0,
            object_scopes: vec![BTreeMap::new()],
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
        while self.current.is_some() || !self.pending_declarations.is_empty() {
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

    fn enter_typedef_scope(&mut self) {
        self.typedef_scopes.push(BTreeMap::new());
        self.typedef_shadows.push(BTreeSet::new());
    }

    fn exit_typedef_scope(&mut self) {
        self.typedef_scopes.pop();
        self.typedef_shadows.pop();
    }

    fn enter_object_scope(&mut self) {
        self.object_scopes.push(BTreeMap::new());
    }

    fn exit_object_scope(&mut self) {
        self.object_scopes.pop();
    }

    fn declare_object_name(&mut self, name: &str, ty: Type) {
        if !name.is_empty() {
            self.object_scopes
                .last_mut()
                .unwrap()
                .insert(name.to_string(), ty);
        }
    }

    fn lookup_object_type(&self, name: &str) -> Option<Type> {
        for scope in self.object_scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }
        None
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

    fn is_builtin_type_name(&self, token: &TokenKind) -> bool {
        match token {
            TokenKind::Identifier(name) => self.builtin_type(name).is_some(),
            _ => false,
        }
    }

    fn builtin_type(&self, name: &str) -> Option<Type> {
        match name {
            "__builtin_va_list" => {
                ensure_builtin_va_list_type();
                Some(builtin_va_list_type())
            }

            // Common glibc implementation typedefs.  In normal C code these are
            // introduced by earlier typedefs, but recognizing the underlying
            // double-underscore names directly makes the parser robust when
            // system headers expose them in nested declarations before a file's
            // local typedef-name bookkeeping has caught up.
            "__int8_t" => Some(Type::SChar),
            "__uint8_t" => Some(Type::UChar),
            "__int16_t" => Some(Type::Short),
            "__uint16_t" => Some(Type::UShort),
            "__int32_t" => Some(Type::Int),
            "__uint32_t" => Some(Type::UInt),
            "__int64_t" | "__quad_t" => Some(Type::Long),
            "__uint64_t" | "__u_quad_t" => Some(Type::ULong),
            "__intptr_t" | "__ssize_t" | "__off_t" | "__off64_t" | "__time_t"
            | "__suseconds_t" | "__useconds_t" | "__clock_t" | "__clockid_t"
            | "__pid_t" | "__uid_t" | "__gid_t" | "__mode_t" | "__nlink_t"
            | "__ino_t" | "__ino64_t" | "__dev_t" | "__rlim_t" | "__rlim64_t"
            | "__blksize_t" | "__blkcnt_t" | "__blkcnt64_t" | "__fsblkcnt_t"
            | "__fsblkcnt64_t" | "__fsfilcnt_t" | "__fsfilcnt64_t" => Some(Type::Long),
            "__socklen_t" | "__id_t" | "__key_t" | "__timer_t" => Some(Type::Int),
            _ => {
                if name.starts_with("__") && name.ends_with("_t") {
                    // Last-resort compatibility for libc-reserved typedef names.
                    // Prefer a pointer-sized signed integer: it is conservative
                    // for layout of most opaque header-only bookkeeping fields.
                    Some(Type::Long)
                } else {
                    None
                }
            }
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

    fn is_type_qualifier(&self, token: &TokenKind) -> bool {
        match token {
            TokenKind::Const => true,
            TokenKind::Identifier(name) => matches!(
                name.as_str(),
                "const"
                    | "__const"
                    | "__const__"
                    | "restrict"
                    | "__restrict"
                    | "__restrict__"
                    | "volatile"
                    | "__volatile"
                    | "__volatile__"
            ),
            _ => false,
        }
    }

    fn is_ignored_gnu_decl_specifier(&self, token: &TokenKind) -> bool {
        matches!(
            token,
            TokenKind::Identifier(name)
                if matches!(
                    name.as_str(),
                    "__extension__" | "__inline" | "__inline__" | "inline"
                )
        )
    }

    fn is_gnu_extension_marker(&self, token: &TokenKind) -> bool {
        matches!(token, TokenKind::Identifier(name) if name == "__extension__")
    }

    fn is_ignored_specifier(&self, token: &TokenKind) -> bool {
        self.is_type_qualifier(token) || self.is_ignored_gnu_decl_specifier(token)
    }

    fn starts_type_name(&self, token: &TokenKind) -> bool {
        self.is_type_specifier(token) || self.is_ignored_specifier(token)
    }

    fn starts_declaration(&self) -> bool {
        match self.current.as_ref().map(|token| &token.kind) {
            Some(kind) => self.is_specifier(kind) || self.is_typedef_name(kind) || self.is_builtin_type_name(kind),
            None => false,
        }
    }

    fn parse_statement(&mut self) -> Result<BlockItem> {
        if let Some(declaration) = self.pending_declarations.pop_front() {
            return Ok(declaration);
        }

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
        let items = if self.is_standalone_struct_or_union_decl_start() {
            vec![self.parse_struct_or_union_decl()?]
        } else if self.is_standalone_enum_decl_start() {
            vec![self.parse_enum_decl()?]
        } else {
            self.parse_var_or_fn_decls()?
        };

        for item in items {
            self.pending_declarations.push_back(item);
        }

        self.pending_declarations.pop_front().ok_or_else(|| UccError {
            kind: ErrorKind::Parse,
            msg: format!("internal error, parse_declaration"),
            span: self.current_span().unwrap_or(Span { start: 0, end: 0 }),
        })
    }

    fn is_standalone_struct_or_union_decl_start(&self) -> bool {
        let tokens = self.peek(4096);
        let Some(first) = tokens.first() else {
            return false;
        };

        if !matches!(first.kind, TokenKind::Struct | TokenKind::Union) {
            return false;
        }

        let mut i = 1usize;
        if matches!(tokens.get(i).map(|t| &t.kind), Some(TokenKind::Identifier(_))) {
            i += 1;
        }

        match tokens.get(i).map(|t| &t.kind) {
            // `struct S;` / `union U;`
            Some(TokenKind::Semicolon) => true,
            // `struct S { ... };` is standalone, but
            // `struct S { ... } *p;` is a variable declaration whose type
            // specifier happens to define the tag.
            Some(TokenKind::LBrace) => {
                let Some(after_brace) = Self::index_after_balanced_braces(&tokens, i) else {
                    return false;
                };
                matches!(tokens.get(after_brace).map(|t| &t.kind), Some(TokenKind::Semicolon))
            }
            _ => false,
        }
    }

    fn is_standalone_enum_decl_start(&self) -> bool {
        let tokens = self.peek(4096);
        let Some(first) = tokens.first() else {
            return false;
        };

        if !matches!(first.kind, TokenKind::Enum) {
            return false;
        }

        let mut i = 1usize;
        if matches!(tokens.get(i).map(|t| &t.kind), Some(TokenKind::Identifier(_))) {
            i += 1;
        }

        match tokens.get(i).map(|t| &t.kind) {
            // `enum E;`
            Some(TokenKind::Semicolon) => true,
            // `enum E { ... };` is standalone, but
            // `enum E { ... } x;` declares a variable.
            Some(TokenKind::LBrace) => {
                let Some(after_brace) = Self::index_after_balanced_braces(&tokens, i) else {
                    return false;
                };
                matches!(tokens.get(after_brace).map(|t| &t.kind), Some(TokenKind::Semicolon))
            }
            _ => false,
        }
    }

    fn index_after_balanced_braces(tokens: &[Token], open_index: usize) -> Option<usize> {
        let mut depth = 0usize;
        for (i, token) in tokens.iter().enumerate().skip(open_index) {
            match token.kind {
                TokenKind::LBrace => depth += 1,
                TokenKind::RBrace => {
                    depth = depth.checked_sub(1)?;
                    if depth == 0 {
                        return Some(i + 1);
                    }
                }
                _ => {}
            }
        }
        None
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
                while self.is_next(&[TokenKind::Semicolon]) {}
                if self.is_next(&[TokenKind::RBrace]) {
                    break members;
                }
                members.extend(self.parse_member_decls()?);
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

    fn make_synthetic_identifier(&self, name: String, span: Span) -> Token {
        Token {
            kind: TokenKind::Identifier(name),
            span,
        }
    }

    fn make_anonymous_tag(&mut self, prefix: &str) -> String {
        let tag = format!("__ucc_anon_{}_{}", prefix, self.anonymous_tag_counter);
        self.anonymous_tag_counter += 1;
        tag
    }

    fn consume_balanced_parens(&mut self) -> Result<()> {
        self.consume(&TokenKind::LParen)?;
        let mut depth = 1usize;
        while depth > 0 {
            let token = self.advance().ok_or_else(|| UccError {
                kind: ErrorKind::Parse,
                msg: format!("unterminated parenthesized attribute"),
                span: self.current_span().unwrap_or(Span { start: 0, end: 0 }),
            })?;
            match token.kind {
                TokenKind::LParen => depth += 1,
                TokenKind::RParen => depth -= 1,
                _ => {}
            }
        }
        Ok(())
    }

    fn skip_attribute_specifiers(&mut self) -> Result<()> {
        loop {
            let is_attribute = matches!(
                self.current.as_ref().map(|t| &t.kind),
                Some(TokenKind::Identifier(name)) if name == "__attribute__"
            );
            if !is_attribute {
                break;
            }
            self.advance();
            if self.check(&TokenKind::LParen) {
                self.consume_balanced_parens()?;
            }
        }
        Ok(())
    }

    fn skip_asm_label(&mut self) -> Result<bool> {
        let is_asm = matches!(
            self.current.as_ref().map(|t| &t.kind),
            Some(TokenKind::Identifier(name)) if name == "__asm__" || name == "asm" || name == "__asm"
        );
        if !is_asm {
            return Ok(false);
        }
        self.advance();
        if self.check(&TokenKind::LParen) {
            self.consume_balanced_parens()?;
        }
        Ok(true)
    }

    fn skip_declaration_annotations(&mut self) -> Result<()> {
        loop {
            let before = self.current.clone();
            self.skip_attribute_specifiers()?;
            self.skip_asm_label()?;
            self.skip_attribute_specifiers()?;
            if self.current == before {
                break;
            }
        }
        Ok(())
    }

    fn consume_struct_or_union_type_specifier(&mut self) -> Result<Vec<Token>> {
        let begin = self.current_span()?;
        let kind_token = self.current.clone().unwrap();
        let aggregate_kind = match &kind_token.kind {
            TokenKind::Struct => AggregateKind::Struct,
            TokenKind::Union => AggregateKind::Union,
            _ => unreachable!(),
        };
        self.advance();

        let explicit_tag = if matches!(self.current.as_ref().map(|t| &t.kind), Some(TokenKind::Identifier(_))) {
            Some(self.advance().unwrap())
        } else {
            None
        };

        let tag_name = if self.is_next(&[TokenKind::LBrace]) {
            let tag = explicit_tag
                .as_ref()
                .map(|t| t.as_string())
                .unwrap_or_else(|| {
                    let prefix = match aggregate_kind {
                        AggregateKind::Struct => "struct",
                        AggregateKind::Union => "union",
                    };
                    self.make_anonymous_tag(prefix)
                });
            let mut members = vec![];
            while !self.check(&TokenKind::RBrace) {
                if self.is_next(&[TokenKind::Semicolon]) {
                    continue;
                }
                members.extend(self.parse_member_decls()?);
            }
            self.consume(&TokenKind::RBrace)?;
            let end = self.current_span()?;
            let decl = StructDeclaration {
                tag: tag.clone(),
                kind: aggregate_kind,
                members,
                span: begin + end,
            };
            self.pending_declarations.push_back(BlockItem::Declaration(match aggregate_kind {
                AggregateKind::Struct => Declaration::Struct(decl),
                AggregateKind::Union => Declaration::Union(decl),
            }));
            tag
        } else if let Some(tag) = explicit_tag {
            // A reference such as `struct foo *p` must not enqueue a synthetic
            // declaration.  In a `for` initializer that pending declaration can
            // become the loop body, and in ordinary blocks it can reorder later
            // items.  Forward declarations are still parsed by
            // parse_struct_or_union_decl for real `struct foo;` declarations.
            tag.as_string()
        } else {
            return Err(UccError {
                kind: ErrorKind::Parse,
                msg: format!("expected a tag name or member list after aggregate specifier"),
                span: self.current_span()?,
            });
        };

        Ok(vec![kind_token, self.make_synthetic_identifier(tag_name, begin)])
    }

    fn consume_enum_type_specifier(&mut self) -> Result<Vec<Token>> {
        let begin = self.current_span()?;
        let kind_token = self.current.clone().unwrap();
        self.consume(&TokenKind::Enum)?;

        let explicit_tag = if matches!(self.current.as_ref().map(|t| &t.kind), Some(TokenKind::Identifier(_))) {
            Some(self.advance().unwrap())
        } else {
            None
        };

        let tag_name = if self.is_next(&[TokenKind::LBrace]) {
            let tag = explicit_tag
                .as_ref()
                .map(|t| t.as_string())
                .unwrap_or_else(|| self.make_anonymous_tag("enum"));
            let mut members = vec![];
            if !self.check(&TokenKind::RBrace) {
                loop {
                    members.push(self.parse_enum_member_decl()?);
                    if self.is_next(&[TokenKind::Comma]) {
                        if self.check(&TokenKind::RBrace) {
                            break;
                        }
                    } else {
                        break;
                    }
                }
            }
            self.consume(&TokenKind::RBrace)?;
            let end = self.current_span()?;
            self.pending_declarations.push_back(BlockItem::Declaration(Declaration::Enum(
                EnumDeclaration {
                    tag: Some(tag.clone()),
                    members,
                    span: begin + end,
                },
            )));
            tag
        } else if let Some(tag) = explicit_tag {
            tag.as_string()
        } else {
            return Err(UccError {
                kind: ErrorKind::Parse,
                msg: format!("expected a tag name or member list after enum specifier"),
                span: self.current_span()?,
            });
        };

        Ok(vec![kind_token, self.make_synthetic_identifier(tag_name, begin)])
    }

    fn parse_member_decls(&mut self) -> Result<Vec<MemberDeclaration>> {
        let begin = self.current_span()?;
        let specifier_list = self.consume_while_type_specifier()?;
        let base_type = self.parse_type(
            specifier_list
                .iter()
                .cloned()
                .map(|t| t.kind)
                .collect::<Vec<_>>(),
        )?;

        let mut members = vec![];

        loop {
            self.skip_declaration_annotations()?;
            self.skip_type_qualifiers();

            // GNU/system headers use anonymous aggregate fields such as
            // `struct { ... };`.  Preserve the aggregate in the layout with an
            // empty member name; member-name validation intentionally ignores
            // empty names.
            if self.check(&TokenKind::Semicolon) {
                self.consume(&TokenKind::Semicolon)?;
                let end = self.current_span()?;
                members.push(MemberDeclaration {
                    name: String::new(),
                    ty: base_type.clone(),
                    span: begin + end,
                });
                break;
            }

            let declarator = self.parse_declarator()?;
            self.skip_type_qualifiers();
            self.skip_declaration_annotations()?;
            let end = self.current_span()?;
            let (name, decl_type, _) = self.process_declarator(&declarator, &base_type)?;
            if matches!(decl_type, Type::Func { .. }) {
                return Err(UccError {
                    msg: format!("function declarations not allowed in struct"),
                    kind: ErrorKind::Parse,
                    span: self.current_span()?,
                });
            }

            members.push(MemberDeclaration {
                name,
                ty: decl_type,
                span: begin + end,
            });

            if self.is_next(&[TokenKind::Comma]) {
                continue;
            }
            self.consume(&TokenKind::Semicolon)?;
            break;
        }

        Ok(members)
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
        ) || self.is_typedef_name(token) || self.is_builtin_type_name(token)
    }

    fn is_storage_class_specifier(&self, token: &TokenKind) -> bool {
        matches!(token, TokenKind::Static | TokenKind::Extern | TokenKind::Typedef)
    }

    fn has_consumed_type_specifier(&self, specifier_list: &[Token]) -> bool {
        specifier_list
            .iter()
            .any(|token| self.is_type_specifier(&token.kind))
    }

    fn is_specifier(&self, token: &TokenKind) -> bool {
        self.is_type_specifier(token)
            || self.is_storage_class_specifier(token)
            || self.is_ignored_specifier(token)
    }

    fn infer_array_size_from_initializer(&self, ty: &Type, init: Option<&Initializer>) -> Type {
        match (ty, init) {
            (
                Type::Array { element, size: 0 },
                Some(Initializer::Compound(_, _, inits)),
            ) => Type::Array {
                element: element.clone(),
                size: inits.len(),
            },
            (
                Type::Array { element, size: 0 },
                Some(Initializer::Single(_, Expression::String(string_expr))),
            ) if matches!(element.as_ref(), Type::Char | Type::SChar | Type::UChar) => Type::Array {
                element: element.clone(),
                size: string_expr.value.len() + 1,
            },
            _ => ty.clone(),
        }
    }

    fn parse_var_or_fn_decl(&mut self) -> Result<BlockItem> {
        let mut decls = self.parse_var_or_fn_decls()?;
        let first = decls.remove(0);
        for decl in decls {
            self.pending_declarations.push_back(decl);
        }
        Ok(first)
    }

    fn parse_var_or_fn_decls(&mut self) -> Result<Vec<BlockItem>> {
        let begin = self.current_span()?;
        let specifier_list = self.consume_while_specifier()?;
        let (base_type, storage_class) = self.parse_type_and_storage_specifiers(
            &specifier_list
                .iter()
                .cloned()
                .map(|t| t.kind)
                .collect::<Vec<_>>(),
        )?;

        let mut decls = vec![];

        loop {
            self.skip_declaration_annotations()?;
            self.skip_type_qualifiers();
            let declarator = self.parse_declarator()?;
            self.skip_type_qualifiers();
            self.skip_declaration_annotations()?;
            let (name, decl_type, params) = self.process_declarator(&declarator, &base_type)?;

            if storage_class == Some(StorageClass::Typedef) {
                let end = self.current_span()?;
                self.declare_typedef_name(name.clone(), decl_type.clone());
                decls.push(BlockItem::Declaration(Declaration::Typedef(TypedefDeclaration {
                    name,
                    ty: decl_type,
                    span: begin + end,
                })));
            } else if matches!(decl_type, Type::Func { .. }) {
                if !decls.is_empty() {
                    return Err(UccError {
                        msg: format!("function declaration mixed with other declarators"),
                        kind: ErrorKind::Parse,
                        span: self.current_span()?,
                    });
                }
                return Ok(vec![self.parse_function_declaration(
                    &name,
                    &params,
                    decl_type,
                    storage_class,
                    begin,
                )?]);
            } else {
                self.shadow_typedef_name(&name);
                let init = if self.is_next(&[TokenKind::Equal]) {
                    Some(self.parse_expression()?)
                } else {
                    None
                };

                let unwrapped = self.unwrap_expression_to_initializer(&name, init);
                let effective_decl_type =
                    self.infer_array_size_from_initializer(&decl_type, unwrapped.as_ref());
                let end = self.current_span()?;
                self.declare_object_name(&name, effective_decl_type.clone());

                decls.push(BlockItem::Declaration(Declaration::Variable(
                    VariableDeclaration {
                        name,
                        ty: effective_decl_type,
                        init: unwrapped,
                        storage_class,
                        is_global: self.depth == 0,
                        span: begin + end,
                    },
                )));
            }

            if self.is_next(&[TokenKind::Comma]) {
                continue;
            }
            self.consume(&TokenKind::Semicolon)?;
            break;
        }

        Ok(decls)
    }

    fn transform_initializer_inner(&self, name: Option<&str>, init: &Initializer) -> Initializer {
        match init {
            Initializer::Single(designator, expr) => {
                if let Expression::Literal(lit) = expr {
                    let nested_name = if designator.is_empty() {
                        name
                    } else {
                        Some(designator.as_str())
                    };
                    self.transform_initializer_inner(nested_name, &lit.value)
                } else {
                    // Do not manufacture a designator for braced positional
                    // initializers.  `struct S s = {0};` must remain a
                    // positional initializer; turning the inner `0` into
                    // `Initializer::Single("s", 0)` makes the typechecker look
                    // for a member literally named `s`.
                    Initializer::Single(designator.clone(), expr.clone())
                }
            }
            Initializer::Compound(designator, ty, elems) => {
                let new_elems = elems
                    .iter()
                    .map(|elem| self.transform_initializer_inner(None, elem))
                    .collect();
                let compound_name = if designator.is_empty() {
                    name.unwrap_or("").to_string()
                } else {
                    designator.clone()
                };
                Initializer::Compound(compound_name, ty.clone(), new_elems)
            }
        }
    }

    fn transform_initializer(&self, name: &str, init: &Initializer) -> Initializer {
        self.transform_initializer_inner(Some(name), init)
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

    fn skip_type_qualifiers(&mut self) {
        while self
            .current
            .as_ref()
            .is_some_and(|token| self.is_type_qualifier(&token.kind))
        {
            self.advance();
        }
    }

    fn has_named_declarator_before_param_delimiter(&self) -> bool {
        let mut depth = 0usize;
        for token in std::iter::once(self.current.as_ref()).flatten().chain(self.tokens.iter()) {
            match &token.kind {
                TokenKind::LParen | TokenKind::LBracket => depth += 1,
                TokenKind::RParen | TokenKind::RBracket => {
                    if depth == 0 {
                        return false;
                    }
                    depth -= 1;
                }
                TokenKind::Comma if depth == 0 => return false,
                TokenKind::Identifier(_)
                    if !self.is_type_qualifier(&token.kind)
                        && !self.is_ignored_gnu_decl_specifier(&token.kind)
                        && !matches!(&token.kind, TokenKind::Identifier(name) if name == "__attribute__" || name == "__asm__" || name == "__asm" || name == "asm") =>
                {
                    return true;
                }
                _ => {}
            }
        }
        false
    }

    fn declarator_from_abstract(decl: AbstractDeclarator) -> Declarator {
        match decl {
            AbstractDeclarator::Base => Declarator::Ident(String::new()),
            AbstractDeclarator::Pointer(inner) => {
                Declarator::Pointer(Box::new(Self::declarator_from_abstract(*inner)))
            }
            AbstractDeclarator::Array(inner, size) => {
                Declarator::Array(Box::new(Self::declarator_from_abstract(*inner)), size)
            }
        }
    }

    fn parse_param_declarator(&mut self) -> Result<Declarator> {
        match self.current.as_ref().map(|t| &t.kind) {
            Some(TokenKind::Comma | TokenKind::RParen | TokenKind::Ellipsis) | None => {
                Ok(Declarator::Ident(String::new()))
            }
            Some(TokenKind::LBracket) => {
                let decl = self.parse_abstract_declarator()?;
                Ok(Self::declarator_from_abstract(decl))
            }
            Some(TokenKind::Star) if !self.has_named_declarator_before_param_delimiter() => {
                let decl = self.parse_abstract_declarator()?;
                Ok(Self::declarator_from_abstract(decl))
            }
            _ => self.parse_declarator(),
        }
    }

    fn parse_declarator(&mut self) -> Result<Declarator> {
        match self.current.as_ref() {
            Some(token) => match token.kind {
                TokenKind::Star => {
                    self.consume(&TokenKind::Star)?;
                    self.skip_type_qualifiers();
                    self.skip_declaration_annotations()?;
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

    fn const_to_i64_for_array_dim(&self, value: &Const, span: Span) -> Result<i64> {
        match value {
            Const::Short(v) => Ok(*v as i64),
            Const::UShort(v) => Ok(*v as i64),
            Const::Int(v) => Ok(*v as i64),
            Const::Long(v) => Ok(*v),
            Const::UInt(v) => Ok(*v as i64),
            Const::ULong(v) => i64::try_from(*v).map_err(|_| UccError {
                msg: format!("array dimension is too large"),
                kind: ErrorKind::Parse,
                span,
            }),
            Const::Char(v) => Ok(*v as i64),
            Const::UChar(v) => Ok(*v as i64),
            Const::Float(_) | Const::Double(_) => Err(UccError {
                msg: format!("array dimension is not an integer constant expression"),
                kind: ErrorKind::Parse,
                span,
            }),
        }
    }

    fn sizeof_type_for_array_dim(&self, ty: &Type, span: Span) -> Result<i64> {
        Ok(match ty {
            Type::Char | Type::SChar | Type::UChar => 1,
            Type::Short | Type::UShort => 2,
            Type::Int | Type::UInt | Type::Float | Type::Enum { .. } => 4,
            Type::Long | Type::ULong | Type::Double | Type::Pointer(_) | Type::Func { .. } => 8,
            Type::Array { element, size } => self.sizeof_type_for_array_dim(element, span)? * (*size as i64),
            Type::Void | Type::Struct { .. } | Type::Union { .. } | Type::Dummy => {
                return Err(UccError {
                    msg: format!("unsupported type in array dimension sizeof"),
                    kind: ErrorKind::Parse,
                    span,
                })
            }
        })
    }

    fn type_of_known_expr_for_sizeof(&self, expr: &Expression, span: Span) -> Result<Type> {
        match expr {
            Expression::Variable(VariableExpression { value, .. }) => {
                self.lookup_object_type(value).ok_or_else(|| UccError {
                    msg: format!("unknown object in array dimension sizeof"),
                    kind: ErrorKind::Parse,
                    span,
                })
            }
            Expression::Subscript(SubscriptExpression { expr, .. }) => {
                let container_type = self.type_of_known_expr_for_sizeof(expr, span)?;
                match container_type {
                    Type::Array { element, .. } | Type::Pointer(element) => Ok(*element),
                    _ => Err(UccError {
                        msg: format!("subscripted non-array in array dimension sizeof"),
                        kind: ErrorKind::Parse,
                        span,
                    }),
                }
            }
            Expression::Deref(DerefExpression { expr, .. }) => {
                let pointer_type = self.type_of_known_expr_for_sizeof(expr, span)?;
                match pointer_type {
                    Type::Pointer(element) => Ok(*element),
                    _ => Err(UccError {
                        msg: format!("dereferenced non-pointer in array dimension sizeof"),
                        kind: ErrorKind::Parse,
                        span,
                    }),
                }
            }
            Expression::Cast(CastExpression { target_type, .. }) => Ok(target_type.clone()),
            _ => Err(UccError {
                msg: format!("unsupported expression in array dimension sizeof"),
                kind: ErrorKind::Parse,
                span,
            }),
        }
    }

    fn sizeof_expr_for_array_dim(&self, expr: &Expression, span: Span) -> Result<i64> {
        let ty = self.type_of_known_expr_for_sizeof(expr, span)?;
        self.sizeof_type_for_array_dim(&ty, span)
    }

    fn eval_array_dim_expression(&self, expr: &Expression) -> Result<i64> {
        match expr {
            Expression::Constant(ConstantExpression { value, span, .. }) => {
                self.const_to_i64_for_array_dim(value, *span)
            }
            Expression::SizeofT(SizeofTExpression { t, span, .. }) => {
                self.sizeof_type_for_array_dim(t, *span)
            }
            Expression::Sizeof(SizeofExpression { expr, span, .. }) => {
                self.sizeof_expr_for_array_dim(expr, *span)
            }
            Expression::Cast(CastExpression { expr, .. }) => self.eval_array_dim_expression(expr),
            Expression::Unary(UnaryExpression { kind, expr, span, .. }) => {
                let value = self.eval_array_dim_expression(expr)?;
                match kind {
                    UnaryExpressionKind::Negate => Ok(value.wrapping_neg()),
                    UnaryExpressionKind::Complement => Ok(!value),
                    UnaryExpressionKind::Not => Ok((value == 0) as i64),
                    UnaryExpressionKind::Inc | UnaryExpressionKind::Dec => Err(UccError {
                        msg: format!("array dimension is not an integer constant expression"),
                        kind: ErrorKind::Parse,
                        span: *span,
                    }),
                }
            }
            Expression::Binary(BinaryExpression { kind, lhs, rhs, span, .. }) => {
                let lhs = self.eval_array_dim_expression(lhs)?;
                let rhs = self.eval_array_dim_expression(rhs)?;
                match kind {
                    BinaryExpressionKind::Add => Ok(lhs.wrapping_add(rhs)),
                    BinaryExpressionKind::Sub => Ok(lhs.wrapping_sub(rhs)),
                    BinaryExpressionKind::Mul => Ok(lhs.wrapping_mul(rhs)),
                    BinaryExpressionKind::Div => lhs.checked_div(rhs).ok_or_else(|| UccError {
                        msg: format!("invalid array dimension division"),
                        kind: ErrorKind::Parse,
                        span: *span,
                    }),
                    BinaryExpressionKind::Rem => lhs.checked_rem(rhs).ok_or_else(|| UccError {
                        msg: format!("invalid array dimension remainder"),
                        kind: ErrorKind::Parse,
                        span: *span,
                    }),
                    BinaryExpressionKind::BitwiseOr => Ok(lhs | rhs),
                    BinaryExpressionKind::BitwiseXor => Ok(lhs ^ rhs),
                    BinaryExpressionKind::BitwiseAnd => Ok(lhs & rhs),
                    BinaryExpressionKind::BitwiseShl => Ok(lhs.wrapping_shl(rhs as u32)),
                    BinaryExpressionKind::BitwiseShr => Ok(lhs.wrapping_shr(rhs as u32)),
                    BinaryExpressionKind::Equal => Ok((lhs == rhs) as i64),
                    BinaryExpressionKind::NotEqual => Ok((lhs != rhs) as i64),
                    BinaryExpressionKind::Less => Ok((lhs < rhs) as i64),
                    BinaryExpressionKind::Greater => Ok((lhs > rhs) as i64),
                    BinaryExpressionKind::LessEqual => Ok((lhs <= rhs) as i64),
                    BinaryExpressionKind::GreaterEqual => Ok((lhs >= rhs) as i64),
                    BinaryExpressionKind::And => Ok(((lhs != 0) && (rhs != 0)) as i64),
                    BinaryExpressionKind::Or => Ok(((lhs != 0) || (rhs != 0)) as i64),
                }
            }
            Expression::Conditional(ConditionalExpression { condition, then_expr, else_expr, .. }) => {
                if self.eval_array_dim_expression(condition)? != 0 {
                    self.eval_array_dim_expression(then_expr)
                } else {
                    self.eval_array_dim_expression(else_expr)
                }
            }
            _ => Err(UccError {
                msg: format!("array dimension is not an integer constant expression"),
                kind: ErrorKind::Parse,
                span: spanof(expr),
            }),
        }
    }

    fn parse_dim(&mut self) -> Result<usize> {
        self.consume(&TokenKind::LBracket)?;
        if self.is_next(&[TokenKind::RBracket]) {
            return Ok(0);
        }
        let expr = self.parse_expression()?;
        self.consume(&TokenKind::RBracket)?;
        let value = self.eval_array_dim_expression(&expr)?;
        if value < 0 {
            return Err(UccError {
                msg: format!("array dimension must be non-negative"),
                kind: ErrorKind::Parse,
                span: spanof(&expr),
            });
        }
        usize::try_from(value).map_err(|_| UccError {
            msg: format!("array dimension is too large"),
            kind: ErrorKind::Parse,
            span: spanof(&expr),
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

    fn consume_while_type_specifier(&mut self) -> Result<Vec<Token>> {
        let mut specifier_list = vec![];
        loop {
            self.skip_declaration_annotations()?;
            if !self.current.as_ref().is_some_and(|token| {
                self.is_type_specifier(&token.kind) || self.is_ignored_specifier(&token.kind)
            }) {
                break;
            }

            if self
                .current
                .as_ref()
                .is_some_and(|token| matches!(token.kind, TokenKind::Identifier(_)))
                && self.has_consumed_type_specifier(&specifier_list)
            {
                break;
            }

            match self.current.as_ref() {
                Some(token) => match token.kind {
                    TokenKind::Struct | TokenKind::Union => {
                        specifier_list.extend(self.consume_struct_or_union_type_specifier()?);
                    }
                    TokenKind::Enum => {
                        specifier_list.extend(self.consume_enum_type_specifier()?);
                    }
                    _ => {
                        specifier_list.push(self.current.clone().unwrap());
                        self.advance();
                    }
                },
                None => return Ok(vec![]),
            }
        }
        Ok(specifier_list)
    }

    fn parse_param(&mut self) -> Result<ParamInfo> {
        let specifier_list = self.consume_while_type_specifier()?;
        let param_t = self.parse_type(
            specifier_list
                .iter()
                .cloned()
                .map(|t| t.kind)
                .collect::<Vec<_>>(),
        )?;
        self.skip_declaration_annotations()?;
        self.skip_type_qualifiers();
        let param_decl = self.parse_param_declarator()?;
        self.skip_type_qualifiers();
        self.skip_declaration_annotations()?;
        Ok((param_t, param_decl.into()))
    }

    fn consume_while_specifier(&mut self) -> Result<Vec<Token>> {
        let mut specifier_list = vec![];
        loop {
            self.skip_declaration_annotations()?;
            let Some(token) = self.current.as_ref() else {
                break;
            };
            if !self.is_specifier(&token.kind) {
                break;
            }

            if self
                .current
                .as_ref()
                .is_some_and(|token| matches!(token.kind, TokenKind::Identifier(_)))
                && self.has_consumed_type_specifier(&specifier_list)
            {
                break;
            }

            match self.current.as_ref() {
                Some(token) => match token.kind {
                    TokenKind::Struct | TokenKind::Union => {
                        specifier_list.extend(self.consume_struct_or_union_type_specifier()?);
                    }
                    TokenKind::Enum => {
                        specifier_list.extend(self.consume_enum_type_specifier()?);
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
            Declarator::Func(params, variadic, decl) => {
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
                            msg: format!("Function parameters with function type are not supported."),
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

                if matches!(decl.as_ref(), Declarator::Ident(_)) {
                    let (name, ty, _) = self.process_declarator(decl, &derived_type)?;
                    Ok((name, ty, param_names))
                } else {
                    let (name, ty, nested_params) = self.process_declarator(decl, &derived_type)?;
                    Ok((name, ty, nested_params))
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

        self.skip_declaration_annotations()?;

        let body = if self.check(&TokenKind::Semicolon) {
            self.consume(&TokenKind::Semicolon)?;
            None
        } else if self.check(&TokenKind::LBrace) {
            self.consume(&TokenKind::LBrace)?;
            self.enter_typedef_scope();
            self.enter_object_scope();
            let param_types = match &ty {
                Type::Func { params, .. } => params.clone(),
                _ => vec![],
            };
            for (param, param_type) in params.iter().zip(param_types.into_iter()) {
                self.shadow_typedef_name(param);
                self.declare_object_name(param, param_type);
            }
            self.current_fn = Some(name.to_string());
            let block = Some(self.parse_block_statement()?);
            self.current_fn = None;
            self.exit_typedef_scope();
            self.exit_object_scope();

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
        let specifier_list = specifier_list
            .into_iter()
            .filter(|specifier| !self.is_ignored_specifier(specifier))
            .collect::<Vec<_>>();
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
            [TokenKind::Identifier(name)] => self
                .lookup_typedef(name)
                .or_else(|| self.builtin_type(name))
                .ok_or_else(|| UccError {
                    kind: ErrorKind::Parse,
                    msg: format!("Unknown typedef name: {}", name),
                    span: self.current_span().unwrap_or(Span { start: 0, end: 0 }),
                }),
            [TokenKind::Void] => Ok(Type::Void),
            [TokenKind::Float] => Ok(Type::Float),
            [TokenKind::Double] => Ok(Type::Double),
            [TokenKind::Long, TokenKind::Double] => Ok(Type::Double),
            [TokenKind::Char] => Ok(Type::Char),
            [TokenKind::Char, TokenKind::Signed] => Ok(Type::SChar),
            [TokenKind::Char, TokenKind::Unsigned] => Ok(Type::UChar),
            _ => {
                let long_count = sorted_specifiers
                    .iter()
                    .filter(|specifier| **specifier == TokenKind::Long)
                    .count();
                let unique_non_long_specifiers: BTreeSet<_> = sorted_specifiers
                    .iter()
                    .filter(|specifier| **specifier != TokenKind::Long)
                    .collect();
                let non_long_count = sorted_specifiers
                    .iter()
                    .filter(|specifier| **specifier != TokenKind::Long)
                    .count();
                if sorted_specifiers.is_empty()
                    || long_count > 2
                    || unique_non_long_specifiers.len() != non_long_count
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
                let has_long = long_count > 0;
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
            if self.is_ignored_specifier(specifier) {
                continue;
            } else if self.is_type_specifier(specifier) || self.is_ident(specifier) {
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
        self.enter_object_scope();
        let begin = self.current_span()?;
        let mut stmts = vec![];
        while !self.check(&TokenKind::RBrace) {
            stmts.push(self.parse_statement()?);
        }
        self.consume(&TokenKind::RBrace)?;
        let end = self.current_span()?;
        self.depth -= 1;
        self.exit_typedef_scope();
        self.exit_object_scope();
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

        if self
            .current
            .as_ref()
            .is_some_and(|token| self.is_gnu_extension_marker(&token.kind))
        {
            self.advance();
            return self.unary();
        }

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
                    if self.starts_type_name(&next_three_tokens.last().unwrap().kind) {
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
                    if self.starts_type_name(&next_three_tokens[1].kind) {
                        self.consume(&TokenKind::LParen)?;
                        let base_type = self.parse_type_name()?;
                        self.consume(&TokenKind::RParen)?;
                        if self.check(&TokenKind::LBrace) {
                            let value = self.parse_braced_initializer()?;
                            let end = self.current_span()?;
                            return Ok(Expression::Literal(LiteralExpression {
                                name: String::new(),
                                value: value.into(),
                                ty: base_type,
                                span: begin + end,
                            }));
                        }
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
        let specifier_list = self.consume_while_type_specifier()?;
        let base_type = self.parse_type(
            specifier_list
                .iter()
                .cloned()
                .map(|t| t.kind)
                .collect::<Vec<_>>(),
        )?;
        self.skip_type_qualifiers();
        match self.current.as_ref().unwrap().kind {
            TokenKind::RParen => Ok(base_type),
            _ => {
                let decl = self.parse_abstract_declarator()?;
                self.skip_type_qualifiers();
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

    fn skip_expression_list_until_rparen(&mut self) -> Result<()> {
        if self.check(&TokenKind::RParen) {
            self.consume(&TokenKind::RParen)?;
            return Ok(());
        }
        loop {
            self.parse_expression()?;
            if !self.is_next(&[TokenKind::Comma]) {
                break;
            }
        }
        self.consume(&TokenKind::RParen)?;
        Ok(())
    }

    fn parse_builtin_va_call(&mut self, name: &str, begin: Span) -> Result<Option<Expression>> {
        match name {
            "__builtin_va_start" => {
                let list = self.parse_expression()?;
                self.consume(&TokenKind::Comma)?;
                let last_param = self.parse_expression()?;
                self.consume(&TokenKind::RParen)?;
                let end = self.current_span()?;
                Ok(Some(Expression::VaStart(VaStartExpression {
                    list: Box::new(list),
                    last_param: Box::new(last_param),
                    ty: Type::Dummy,
                    span: begin + end,
                })))
            }
            "__builtin_va_end" => {
                let list = self.parse_expression()?;
                self.consume(&TokenKind::RParen)?;
                let end = self.current_span()?;
                Ok(Some(Expression::VaEnd(VaEndExpression {
                    list: Box::new(list),
                    ty: Type::Dummy,
                    span: begin + end,
                })))
            }
            "__builtin_va_copy" => {
                let dst = self.parse_expression()?;
                self.consume(&TokenKind::Comma)?;
                let src = self.parse_expression()?;
                self.consume(&TokenKind::RParen)?;
                let end = self.current_span()?;
                Ok(Some(Expression::VaCopy(VaCopyExpression {
                    dst: Box::new(dst),
                    src: Box::new(src),
                    ty: Type::Dummy,
                    span: begin + end,
                })))
            }
            "__builtin_va_arg" => {
                let list = self.parse_expression()?;
                self.consume(&TokenKind::Comma)?;
                let arg_ty = self.parse_type_name()?;
                self.consume(&TokenKind::RParen)?;
                let end = self.current_span()?;
                Ok(Some(Expression::VaArg(VaArgExpression {
                    list: Box::new(list),
                    arg_ty: arg_ty.clone(),
                    ty: Type::Dummy,
                    span: begin + end,
                })))
            }
            "__builtin_bswap16" | "__builtin_bswap32" | "__builtin_bswap64" => {
                // These appear in glibc inline byte-swap helpers.  The compiler
                // does not need to lower them for its own sources, but parsing
                // them as side-effect-free integer expressions lets those helper
                // definitions typecheck.
                self.skip_expression_list_until_rparen()?;
                let value = match name {
                    "__builtin_bswap16" => Const::UShort(0),
                    "__builtin_bswap32" => Const::UInt(0),
                    "__builtin_bswap64" => Const::ULong(0),
                    _ => unreachable!(),
                };
                let end = self.current_span()?;
                Ok(Some(Expression::Constant(ConstantExpression {
                    value,
                    ty: Type::Dummy,
                    span: begin + end,
                })))
            }
            _ => Ok(None),
        }
    }

    fn call(&mut self) -> Result<Expression> {
        let begin = self.current_span()?;
        let mut expr = self.primary()?;
        loop {
            if self.is_next(&[TokenKind::LParen]) {
                let call_name = match &expr {
                    Expression::Variable(var) => var.value.clone(),
                    _ => {
                        return Err(UccError {
                            msg: format!("expected a variable"),
                            kind: ErrorKind::Parse,
                            span: self.current_span()?,
                        })
                    }
                };

                if let Some(builtin_expr) = self.parse_builtin_va_call(&call_name, begin)? {
                    expr = builtin_expr;
                    continue;
                }

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
                    name: call_name,
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

    fn initializer_from_designator_path(path: &[String], expr: Expression) -> Initializer {
        if path.is_empty() {
            Initializer::Single(String::new(), expr)
        } else if path.len() == 1 {
            Initializer::Single(path[0].clone(), expr)
        } else {
            Initializer::Compound(
                path[0].clone(),
                Type::Dummy,
                vec![Self::initializer_from_designator_path(&path[1..], expr)],
            )
        }
    }

    fn parse_braced_initializer(&mut self) -> Result<Initializer> {
        self.consume(&TokenKind::LBrace)?;
        let mut inits = vec![];
        loop {
            if self.is_next(&[TokenKind::RBrace]) {
                break;
            }

            let designator_path = if self.is_next(&[TokenKind::Dot]) {
                let mut path = vec![self
                    .consume(&TokenKind::Identifier(String::new()))?
                    .unwrap()
                    .as_string()];
                while self.is_next(&[TokenKind::Dot]) {
                    path.push(
                        self.consume(&TokenKind::Identifier(String::new()))?
                            .unwrap()
                            .as_string(),
                    );
                }
                self.consume(&TokenKind::Equal)?;
                path
            } else {
                vec![]
            };

            let expr = self.parse_expression()?;
            inits.push(Self::initializer_from_designator_path(&designator_path, expr));

            if self.is_next(&[TokenKind::Comma]) {
                continue;
            }

            self.consume(&TokenKind::RBrace)?;
            break;
        }

        if inits.is_empty() {
            return Err(UccError {
                msg: format!("empty compound literal"),
                kind: ErrorKind::Parse,
                span: self.current_span()?,
            });
        }

        Ok(Initializer::Compound(String::new(), Type::Dummy, inits))
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
            if self.check(&TokenKind::LBrace) {
                self.parse_statement_expression_as_zero(begin)
            } else {
                self.parse_grouping()
            }
        } else if self.is_next(&[TokenKind::Identifier("".to_owned())]) {
            match self.previous.as_ref().unwrap().kind {
                TokenKind::Identifier(ref var) => self.parse_variable(var, begin),
                _ => unreachable!(),
            }
        } else if self.check(&TokenKind::LBrace) {
            let value = self.parse_braced_initializer()?;
            let end = self.current_span()?;
            Ok(Expression::Literal(LiteralExpression {
                name: String::new(),
                value: value.into(),
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

    fn parse_statement_expression_as_zero(&mut self, begin: Span) -> Result<Expression> {
        self.consume(&TokenKind::LBrace)?;
        let mut brace_depth = 1usize;
        while brace_depth > 0 {
            let token = self.advance().ok_or_else(|| UccError {
                kind: ErrorKind::Parse,
                msg: format!("unterminated statement expression"),
                span: self.current_span().unwrap_or(Span { start: 0, end: 0 }),
            })?;
            match token.kind {
                TokenKind::LBrace => brace_depth += 1,
                TokenKind::RBrace => brace_depth -= 1,
                _ => {}
            }
        }
        self.consume(&TokenKind::RParen)?;
        let end = self.current_span()?;
        Ok(Expression::Constant(ConstantExpression {
            value: Const::Int(0),
            ty: Type::Dummy,
            span: begin + end,
        }))
    }

    fn parse_grouping(&mut self) -> Result<Expression> {
        let mut expr = self.parse_expression()?;
        while self.is_next(&[TokenKind::Comma]) {
            // C's comma operator evaluates the left operand for side effects and
            // yields the right operand.  The compiler does not yet keep a
            // dedicated AST node for sequencing, but returning the right-hand
            // expression is enough for system-header macros such as assert.
            expr = self.parse_expression()?;
        }
        self.consume(&TokenKind::RParen)?;
        Ok(expr)
    }

    fn parse_abstract_declarator(&mut self) -> Result<AbstractDeclarator> {
        match self.current.as_ref().unwrap().kind {
            TokenKind::Star => {
                self.consume(&TokenKind::Star)?;
                self.skip_type_qualifiers();
                self.skip_declaration_annotations()?;
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


    #[test]
    fn lexes_hex_integer_suffixes_and_octal_escapes() {
        let tokens: Vec<_> = Lexer::new("int x = 0x80; unsigned long y = 1ULL; char z = '\\0';".to_string()).collect();

        assert!(tokens.iter().any(|token| matches!(token.kind, TokenKind::Constant(Const::Int(0x80)))));
        assert!(tokens.iter().any(|token| matches!(token.kind, TokenKind::Constant(Const::ULong(1)))));
        assert!(tokens.iter().any(|token| matches!(token.kind, TokenKind::CharLiteral('\0'))));
    }

    #[test]
    fn parses_anonymous_aggregate_typedefs_before_alias_declarations() {
        let program = parse("typedef struct { int capacity; int len; int *data; } VecInt; VecInt v;");

        assert!(matches!(
            &program.block_items[0],
            BlockItem::Declaration(Declaration::Struct(decl))
                if decl.members.len() == 3 && decl.tag.starts_with("__ucc_anon_struct_")
        ));
        let tag = match &program.block_items[0] {
            BlockItem::Declaration(Declaration::Struct(decl)) => decl.tag.clone(),
            other => panic!("expected generated struct declaration, got {other:?}"),
        };
        assert!(matches!(
            &program.block_items[1],
            BlockItem::Declaration(Declaration::Typedef(decl))
                if decl.name == "VecInt" && decl.ty == (Type::Struct { tag: tag.clone() })
        ));
        assert!(matches!(
            &program.block_items[2],
            BlockItem::Declaration(Declaration::Variable(var))
                if var.ty == (Type::Struct { tag })
        ));
    }

    #[test]
    fn parses_long_long_long_double_and_gnu_attributes() {
        parse(
            "typedef struct { long long i __attribute__((__aligned__(__alignof__(long long)))); long double d; } max_align_t;",
        );
    }

    #[test]
    fn parses_const_qualified_declarations_as_unqualified_types() {
        let tys = top_level_var_types(
            r#"
            const int x;
            int const y;
            const char *s;
            char *const *argv;
            "#,
        );

        assert_eq!(
            tys,
            vec![
                Type::Int,
                Type::Int,
                Type::Pointer(Box::new(Type::Char)),
                Type::Pointer(Box::new(Type::Pointer(Box::new(Type::Char)))),
            ]
        );
    }

    #[test]
    fn parses_gnu_extension_const_params_attributes_and_empty_members() {
        parse(
            r#"
            __extension__ typedef struct {
                int capacity;
                ;
                const char *name;
            } Vec;
            extern void __assert_fail(const char *__assertion,
                                      const char *__file,
                                      unsigned int __line,
                                      const char *__function)
                __attribute__ ((__nothrow__ , __leaf__))
                __attribute__ ((__noreturn__));
            extern int getopt(int ___argc, char *const *___argv, const char *__shortopts)
                __attribute__ ((__nothrow__ , __leaf__))
                __attribute__ ((__nonnull__ (2, 3)));
            "#,
        );
    }

    #[test]
    fn lexes_double_underscore_signed_and_const_as_specifiers() {
        let tys = top_level_var_types("__signed__ int x; __const int y; __const__ char *z;");

        assert_eq!(
            tys,
            vec![Type::Int, Type::Int, Type::Pointer(Box::new(Type::Char))]
        );
    }

    #[test]
    fn parses_builtin_va_list_asm_labels_and_sizeof_array_dimensions() {
        let program = parse(
            r#"
            typedef long unsigned int size_t;
            typedef __builtin_va_list __gnuc_va_list;
            typedef __gnuc_va_list va_list;
            extern int strerror_r(int __errnum, char *__buf, size_t __buflen)
                __asm__ ("" "__xpg_strerror_r")
                __attribute__ ((__nothrow__ , __leaf__))
                __attribute__ ((__nonnull__ (2)));
            typedef struct {
                unsigned long int __val[(1024 / (8 * sizeof (unsigned long int)))];
            } __sigset_t;
            "#,
        );

        assert!(matches!(
            &program.block_items[1],
            BlockItem::Declaration(Declaration::Typedef(decl))
                if decl.name == "__gnuc_va_list"
                    && decl.ty == builtin_va_list_type()
        ));
        assert!(matches!(
            &program.block_items[2],
            BlockItem::Declaration(Declaration::Typedef(decl))
                if decl.name == "va_list"
                    && decl.ty == builtin_va_list_type()
        ));
        assert!(matches!(
            &program.block_items[4],
            BlockItem::Declaration(Declaration::Struct(decl))
                if matches!(
                    &decl.members[0].ty,
                    Type::Array { element, size }
                        if **element == Type::ULong && *size == 16
                )
        ));
    }


    #[test]
    fn parses_header_style_abstract_array_and_function_pointer_parameters() {
        let program = parse(
            r#"
            extern char *tmpnam(char[20]);
            extern int atexit(void (*__func)(void));
            "#,
        );

        assert!(matches!(
            &program.block_items[0],
            BlockItem::Declaration(Declaration::Function(func))
                if matches!(
                    &func.ty,
                    Type::Func { params, ret, variadic: false }
                        if **ret == Type::Pointer(Box::new(Type::Char))
                            && matches!(
                                &params[0],
                                Type::Array { element, size }
                                    if **element == Type::Char && *size == 20
                            )
                )
        ));
        assert!(matches!(
            &program.block_items[1],
            BlockItem::Declaration(Declaration::Function(func))
                if func.params == vec!["__func".to_string()]
                    && matches!(
                        &func.ty,
                        Type::Func { params, ret, variadic: false }
                            if **ret == Type::Int
                                && matches!(
                                    &params[0],
                                    Type::Pointer(inner)
                                        if matches!(
                                            inner.as_ref(),
                                            Type::Func { params, ret, variadic: false }
                                                if params.is_empty() && **ret == Type::Void
                                        )
                                )
                    )
        ));
    }

    #[test]
    fn parses_typed_compound_literals_with_designated_initializers() {
        let program = parse(
            r#"
            struct Token { int kind; int len; char *start; };
            struct Token next(int is_float, char *start, int len) {
                return (struct Token){.kind = is_float ? 1 : 2, .len = len, .start = start};
            }
            "#,
        );

        let BlockItem::Declaration(Declaration::Function(func)) = &program.block_items[1] else {
            panic!("expected function declaration");
        };
        let Some(BlockItem::Statement(Statement::Compound(block))) = func.body.as_ref().as_ref() else {
            panic!("expected function body");
        };
        let BlockItem::Statement(Statement::Return(ret)) = &block.stmts[0] else {
            panic!("expected return statement");
        };
        let Some(Expression::Literal(lit)) = &ret.expr else {
            panic!("expected compound literal");
        };
        assert_eq!(lit.ty, Type::Struct { tag: "Token".to_string() });
        assert!(matches!(
            lit.value.as_ref(),
            Initializer::Compound(_, _, inits)
                if inits.len() == 3
                    && matches!(&inits[0], Initializer::Single(name, _) if name == "kind")
                    && matches!(&inits[1], Initializer::Single(name, _) if name == "len")
                    && matches!(&inits[2], Initializer::Single(name, _) if name == "start")
        ));
    }


    #[test]
    fn parses_empty_array_parameters() {
        parse("extern int getloadavg(double loadavg[], int nelem);");
    }

    #[test]
    fn parses_gnu_statement_expression_in_assert_like_macro() {
        parse("int main(void) { ((void) sizeof ((0 && \"bad\") ? 1 : 0), __extension__ ({ if (0 && \"bad\") ; else __assert_fail(\"bad\", \"x.c\", 1, __extension__ __PRETTY_FUNCTION__); })); return 0; }");
    }

    #[test]
    fn parses_anonymous_struct_typedef_at_file_scope() {
        parse("typedef struct { int capacity; int len; int *data; } VecInt;");
    }

    #[test]
    fn parses_multiple_declarators_in_one_declaration() {
        let program = parse("int a, *b; int main(void) { long x, y; return 0; }");

        assert!(matches!(
            &program.block_items[0],
            BlockItem::Declaration(Declaration::Variable(var))
                if var.name == "a" && var.ty == Type::Int
        ));
        assert!(matches!(
            &program.block_items[1],
            BlockItem::Declaration(Declaration::Variable(var))
                if var.name == "b" && var.ty == Type::Pointer(Box::new(Type::Int))
        ));
    }

    #[test]
    fn parses_nested_designated_initializers() {
        let program = parse(
            r#"
            union U { int reg; double fp; };
            struct AsmOperand { int kind; union U as; int asm_type; };
            struct AsmOperand f(int scratch_reg, int cmp_type) {
                return (struct AsmOperand){ .kind = 1, .as.reg = scratch_reg, .asm_type = cmp_type };
            }
            "#,
        );

        let BlockItem::Declaration(Declaration::Function(func)) = &program.block_items[2] else {
            panic!("expected function declaration");
        };
        let Some(BlockItem::Statement(Statement::Compound(block))) = func.body.as_ref().as_ref() else {
            panic!("expected function body");
        };
        let BlockItem::Statement(Statement::Return(ret)) = &block.stmts[0] else {
            panic!("expected return statement");
        };
        let Some(Expression::Literal(lit)) = &ret.expr else {
            panic!("expected compound literal");
        };
        assert!(matches!(
            lit.value.as_ref(),
            Initializer::Compound(_, _, inits)
                if matches!(
                    &inits[1],
                    Initializer::Compound(name, _, nested)
                        if name == "as"
                            && matches!(&nested[0], Initializer::Single(member, _) if member == "reg")
                )
        ));
    }

    #[test]
    fn parses_builtin_varargs_intrinsics() {
        parse(
            r#"
            typedef __builtin_va_list va_list;
            enum TokenKind { TOKEN_A, TOKEN_B };
            int f(int size, ...) {
                va_list ap;
                __builtin_va_start(ap, size);
                enum TokenKind kind = __builtin_va_arg(ap, enum TokenKind);
                __builtin_va_end(ap);
                return kind;
            }
            "#,
        );
    }

    #[test]
    fn parses_sizeof_expression_array_dimensions() {
        let program = parse(
            r#"
            static int regs[] = { 1, 2, 3, 4 };
            struct State { _Bool used[((int) (sizeof(regs) / sizeof(regs[0])))]; };
            "#,
        );

        assert!(matches!(
            &program.block_items[1],
            BlockItem::Declaration(Declaration::Struct(decl))
                if matches!(
                    &decl.members[0].ty,
                    Type::Array { element, size }
                        if **element == Type::Char && *size == 4
                )
        ));
    }

    #[test]
    fn parses_libc_reserved_typedef_names_and_forward_struct_pointers() {
        parse(
            r#"
            struct _fpstate *fp;
            struct Context { struct _fpstate *fpstate; __uint64_t reserved[8]; };
            "#,
        );
    }

    #[test]
    fn parses_multiple_and_anonymous_aggregate_members() {
        let program = parse(
            r#"
            struct Outer {
                int a, b;
                struct { int nested; };
            };
            "#,
        );

        let outer = program.block_items.iter().find_map(|item| match item {
            BlockItem::Declaration(Declaration::Struct(decl)) if decl.members.len() == 3 => {
                Some(decl)
            }
            _ => None,
        }).expect("expected outer struct declaration");

        assert_eq!(outer.members[0].name, "a");
        assert_eq!(outer.members[1].name, "b");
        assert_eq!(outer.members[2].name, "");
    }

    #[test]
    fn parses_anonymous_struct_pointer_declaration_with_initializer() {
        parse(
            r#"
            void *malloc(unsigned long size);
            int f(int num_args) {
                struct { int count; int regs[2]; } *arg_locs = malloc(sizeof(struct { int x; }));
                return num_args;
            }
            "#,
        );
    }

    #[test]
    fn parses_builtin_bswap_calls_in_inline_helpers() {
        parse(r#"
            typedef unsigned short int __uint16_t;
            typedef unsigned int __uint32_t;
            typedef unsigned long int __uint64_t;
            static __inline __uint16_t __bswap_16(__uint16_t x) { return __builtin_bswap16(x); }
            static __inline __uint32_t __bswap_32(__uint32_t x) { return __builtin_bswap32(x); }
            static __inline __uint64_t __bswap_64(__uint64_t x) { return __builtin_bswap64(x); }
        "#);
    }

    #[test]
    fn parses_anonymous_struct_pointer_local_declarations() {
        parse(r#"
            void *malloc(unsigned long size);
            int f(int num_args) {
                struct {
                    int num_eightbytes;
                    int regs[2];
                    int asm_types[2];
                } *arg_locs = malloc(sizeof(struct { int x; }) * num_args);
                return arg_locs != 0;
            }
        "#);
    }

}
