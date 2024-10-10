use crate::{
    lexer::lex::{Const, Token},
    parser::ast::{
        AbstractDeclarator, AddrOfExpression, ArrowExpression, AssignExpression, BinaryExpression,
        BinaryExpressionKind, BlockItem, BlockStatement, BreakStatement, CallExpression,
        CastExpression, ConditionalExpression, ConstantExpression, ContinueStatement, Declaration,
        Declarator, DerefExpression, DoWhileStatement, DotExpression, Expression,
        ExpressionStatement, ForInit, ForStatement, FunctionDeclaration, IfStatement, Initializer,
        LiteralExpression, MemberDeclaration, ParamInfo, ProgramStatement, ReturnStatement,
        SizeofExpression, SizeofTExpression, Statement, StorageClass, StringExpression,
        StructDeclaration, SubscriptExpression, Type, UnaryExpression, UnaryExpressionKind,
        VariableDeclaration, VariableExpression, WhileStatement,
    },
};
use anyhow::{bail, Result};
use std::collections::{BTreeSet, VecDeque};

pub struct Parser {
    pub tokens: VecDeque<Token>,
    pub current: Option<Token>,
    pub previous: Option<Token>,
    pub depth: usize,
    pub current_target_type: Option<Type>,
    pub current_fn: Option<String>,
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
        }
    }

    fn advance(&mut self) -> Option<Token> {
        self.previous = self.current.take();
        self.current = self.tokens.pop_front();
        self.previous.clone()
    }

    fn consume(&mut self, token: &Token) -> Result<Option<Token>> {
        if self.check(token) {
            return Ok(self.advance());
        }
        bail!(
            "expected {:?}, got: prev: {:?}. curr: {:?}",
            token,
            self.previous,
            self.current
        );
    }

    fn check(&self, token: &Token) -> bool {
        std::mem::discriminant(self.current.as_ref().unwrap()) == std::mem::discriminant(token)
    }

    pub fn parse(&mut self) -> Result<Statement> {
        self.advance();
        let mut stmts = vec![];
        while self.current.is_some() {
            stmts.push(self.parse_statement()?);
        }
        Ok(Statement::Program(ProgramStatement { block_items: stmts }))
    }

    fn is_next(&mut self, tokens: &[Token]) -> bool {
        for token in tokens {
            if self.check(token) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check_many(&self, tokens: &[Token]) -> bool {
        for token in tokens {
            if self.check(token) {
                return true;
            }
        }
        false
    }

    fn parse_statement(&mut self) -> Result<BlockItem> {
        if self.check_many(&[
            Token::Char,
            Token::Int,
            Token::Long,
            Token::Double,
            Token::Signed,
            Token::Unsigned,
            Token::Void,
            Token::Static,
            Token::Extern,
            Token::Struct,
        ]) {
            self.parse_declaration()
        } else if self.is_next(&[Token::Return]) {
            self.parse_return_statement()
        } else if self.is_next(&[Token::If]) {
            self.parse_if_statement()
        } else if self.is_next(&[Token::Do]) {
            self.parse_do_while_statement()
        } else if self.is_next(&[Token::While]) {
            self.parse_while_statement()
        } else if self.is_next(&[Token::For]) {
            self.parse_for_statement()
        } else if self.is_next(&[Token::Break]) {
            self.parse_break_statement()
        } else if self.is_next(&[Token::Continue]) {
            self.parse_continue_statement()
        } else if self.is_next(&[Token::LBrace]) {
            self.parse_block_statement()
        } else if self.is_next(&[Token::Semicolon]) {
            Ok(BlockItem::Statement(Statement::Null))
        } else {
            self.parse_expression_statement()
        }
    }

    fn parse_declaration(&mut self) -> Result<BlockItem> {
        match self.peek(3).as_slice() {
            [Token::Struct, Token::Identifier(_), Token::LBrace | Token::Semicolon] => {
                self.parse_struct_decl()
            }
            _ => self.parse_var_or_fn_decl(),
        }
    }

    fn parse_struct_decl(&mut self) -> Result<BlockItem> {
        self.consume(&Token::Struct)?;
        let tag = self
            .consume(&Token::Identifier("".to_owned()))?
            .unwrap()
            .as_string();
        let members = if self.is_next(&[Token::LBrace]) {
            let mut members = vec![];
            loop {
                let next_member = self.parse_member_decl()?;
                if self.is_next(&[Token::RBrace]) {
                    members.push(next_member);
                    break members;
                } else {
                    members.push(next_member);
                }
            }
        } else {
            vec![]
        };
        self.consume(&Token::Semicolon)?;
        Ok(BlockItem::Declaration(Declaration::Struct(
            StructDeclaration { tag, members },
        )))
    }

    fn parse_member_decl(&mut self) -> Result<MemberDeclaration> {
        let specifier_list = self.consume_while_type_specifier();
        let base_type = self.parse_type(specifier_list)?;
        let declarator = self.parse_declarator()?;
        match declarator {
            Declarator::Func(_, _) => bail!("function declarations not allowed in struct"),
            _ => {
                self.consume(&Token::Semicolon)?;
                let (name, decl_type, _) = self.process_declarator(&declarator, &base_type)?;
                Ok(MemberDeclaration {
                    name,
                    _type: decl_type,
                })
            }
        }
    }

    fn is_type_specifier(&self, token: &Token) -> bool {
        matches!(
            token,
            Token::Int
                | Token::Long
                | Token::Char
                | Token::Unsigned
                | Token::Signed
                | Token::Double
                | Token::Void
                | Token::Struct
        )
    }

    fn is_storage_class_specifier(&self, token: &Token) -> bool {
        matches!(token, Token::Static | Token::Extern)
    }

    fn is_specifier(&self, token: &Token) -> bool {
        self.is_type_specifier(token) || self.is_storage_class_specifier(token)
    }

    fn parse_var_or_fn_decl(&mut self) -> Result<BlockItem> {
        let specifier_list = self.consume_while_specifier();
        let (base_type, storage_class) = self.parse_type_and_storage_specifiers(&specifier_list)?;

        let declarator = self.parse_declarator()?;

        let (name, decl_type, params) = self.process_declarator(&declarator, &base_type)?;

        match decl_type {
            Type::Func { params: _, ret: _ } => {
                self.parse_function_declaration(&name, &params, decl_type, storage_class)
            }
            _ => {
                let init = if self.is_next(&[Token::Equal]) {
                    let expr = self.parse_expression()?;
                    self.consume(&Token::Semicolon)?;
                    Some(expr)
                } else if self.is_next(&[Token::Semicolon]) {
                    None
                } else {
                    bail!("some error")
                };

                let unwrapped = self.unwrap_expression_to_initializer(&name, init);

                Ok(BlockItem::Declaration(Declaration::Variable(
                    VariableDeclaration {
                        name,
                        _type: decl_type,
                        init: unwrapped,
                        storage_class,
                        is_global: self.depth == 0,
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
            Initializer::Compound(_, _type, elems) => {
                let new_elems = elems
                    .iter()
                    .map(|elem| self.transform_initializer(name, elem))
                    .collect();
                Initializer::Compound(name.to_string(), _type.clone(), new_elems)
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
        match self.current.as_ref().unwrap() {
            Token::Star => {
                self.consume(&Token::Star)?;
                let inner = self.parse_declarator()?;
                Ok(Declarator::Pointer(Box::new(inner)))
            }
            _ => self.parse_direct_declarator(),
        }
    }

    fn parse_direct_declarator(&mut self) -> Result<Declarator> {
        let simple_declarator = self.parse_simple_declarator()?;
        match self.current.as_ref().unwrap() {
            Token::LParen => {
                self.consume(&Token::LParen)?;
                let params = self.parse_param_list()?;
                Ok(Declarator::Func(params, Box::new(simple_declarator)))
            }
            Token::LBracket => {
                let decl = self.parse_array_decl_suffix(&simple_declarator)?;
                Ok(decl)
            }
            _ => Ok(simple_declarator),
        }
    }

    fn parse_simple_declarator(&mut self) -> Result<Declarator> {
        let token = self.advance().unwrap();
        match token {
            Token::LParen => {
                let decl = self.parse_declarator()?;
                self.consume(&Token::RParen)?;
                Ok(decl)
            }
            Token::Identifier(id) => Ok(Declarator::Ident(id)),
            _ => {
                bail!("a simple declarator");
            }
        }
    }

    fn parse_array_decl_suffix(&mut self, base_decl: &Declarator) -> Result<Declarator> {
        let dim = self.parse_dim()?;
        let mut new_decl = Declarator::Array(Box::new(base_decl.clone()), dim);

        if let Some(Token::LBracket) = self.current.as_ref() {
            new_decl = self.parse_array_decl_suffix(&new_decl)?;
        }

        Ok(new_decl)
    }

    fn consume_constant_or_char_literal(&mut self) -> Result<Option<Token>> {
        match self.current {
            Some(Token::Constant(_)) | Some(Token::CharLiteral(_)) => Ok(self.advance()),
            _ => bail!("expected constant, got: {:?}", self.current),
        }
    }

    fn parse_dim(&mut self) -> Result<usize> {
        self.consume(&Token::LBracket).unwrap();
        let dim = self.consume_constant_or_char_literal()?.unwrap();
        self.consume(&Token::RBracket).unwrap();
        Ok(match dim {
            Token::Constant(Const::Int(n)) => n as usize,
            Token::Constant(Const::Long(n)) => n as usize,
            Token::Constant(Const::UInt(n)) => n as usize,
            Token::Constant(Const::ULong(n)) => n as usize,
            Token::Constant(Const::Char(n)) => n as usize,
            Token::Constant(Const::UChar(n)) => n as usize,
            Token::CharLiteral(ch) => ch as usize,
            _ => unreachable!(),
        })
    }
    fn parse_param_list(&mut self) -> Result<Vec<ParamInfo>> {
        let in_front_of_us = self.lookahead_until(&Token::RParen);

        if in_front_of_us == vec![Token::Void] {
            self.consume(&Token::Void)?;
            self.consume(&Token::RParen)?;

            Ok(vec![])
        } else {
            let mut params = vec![];
            loop {
                params.push(self.parse_param()?);
                if !self.is_next(&[Token::Comma]) {
                    break;
                }
            }
            self.consume(&Token::RParen)?;
            Ok(params)
        }
    }

    fn consume_while_type_specifier(&mut self) -> Vec<Token> {
        let mut specifier_list = vec![];
        while self.is_type_specifier(self.current.as_ref().unwrap()) {
            match self.current {
                Some(Token::Struct) => {
                    specifier_list.push(self.current.clone().unwrap());
                    self.advance();
                    specifier_list.push(self.current.clone().unwrap());
                    self.advance();
                }
                _ => {
                    specifier_list.push(self.current.clone().unwrap());
                    self.advance();
                }
            }
        }
        specifier_list
    }

    fn parse_param(&mut self) -> Result<ParamInfo> {
        let specifier_list = self.consume_while_type_specifier();
        let param_t = self.parse_type(specifier_list)?;
        let param_decl = self.parse_declarator()?;
        Ok((param_t, param_decl.into()))
    }

    fn consume_while_specifier(&mut self) -> Vec<Token> {
        let mut specifier_list = vec![];
        while self.is_specifier(self.current.as_ref().unwrap()) {
            match self.current {
                Some(Token::Struct) => {
                    specifier_list.push(self.current.clone().unwrap());
                    self.advance();

                    if let Some(Token::Identifier(_)) = self.current {
                        specifier_list.push(self.current.clone().unwrap());
                        self.advance();
                    } else {
                        panic!("Expected an identifier after 'struct'");
                    }
                }
                _ => {
                    specifier_list.push(self.current.clone().unwrap());
                    self.advance();
                }
            }
        }
        specifier_list
    }

    fn process_declarator(
        &self,
        declarator: &Declarator,
        base_type: &Type,
    ) -> Result<(String, Type, Vec<String>)> {
        let some_fn_type = Type::Func {
            params: vec![],
            ret: Box::new(Type::Int),
        };
        match declarator {
            Declarator::Ident(name) => Ok((name.clone(), base_type.clone(), vec![])),
            Declarator::Pointer(decl) => {
                let derived_type = Type::Pointer(base_type.clone().into());
                self.process_declarator(decl, &derived_type)
            }
            Declarator::Func(params, decl) => match *decl.clone() {
                Declarator::Ident(name) => {
                    let mut param_names = vec![];
                    let mut param_types = vec![];

                    for (param_base_type, param_declarator) in params {
                        let (param_name, param_type, _) =
                            self.process_declarator(param_declarator, param_base_type)?;
                        if std::mem::discriminant(&param_type)
                            == std::mem::discriminant(&some_fn_type)
                        {
                            bail!("Function pointers in parameters are not supported.")
                        }
                        param_names.push(param_name);
                        param_types.push(param_type);
                    }

                    let derived_type = Type::Func {
                        params: param_types,
                        ret: base_type.clone().into(),
                    };
                    Ok((name.clone(), derived_type, param_names))
                }
                _ => bail!("Can't apply additional type derivations to a function type."),
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
        _type: Type,
        storage_class: Option<StorageClass>,
    ) -> Result<BlockItem> {
        self.current_target_type = Some(match _type.clone() {
            Type::Func { params: _, ret } => *ret,
            _ => unreachable!(),
        });

        let body = if self.check(&Token::Semicolon) {
            self.consume(&Token::Semicolon)?;
            None
        } else if self.check(&Token::LBrace) {
            self.consume(&Token::LBrace)?;
            self.current_fn = Some(name.to_string());
            let block = Some(self.parse_block_statement()?);
            self.current_fn = None;

            block
        } else {
            bail!(
                "Expected block statement or semicolon, got: {:?}",
                self.current
            );
        };

        self.current_target_type = None;

        Ok(BlockItem::Declaration(Declaration::Function(
            FunctionDeclaration {
                name: name.to_owned(),
                params: params.to_owned(),
                body: body.into(),
                is_global: self.depth == 0,
                storage_class,
                _type: _type.clone(),
            },
        )))
    }

    fn parse_type(&self, specifier_list: Vec<Token>) -> Result<Type> {
        let mut sorted_specifiers = specifier_list.clone();
        sorted_specifiers.sort();

        match &sorted_specifiers[..] {
            [Token::Struct, Token::Identifier(tag)] => Ok(Type::Struct { tag: tag.clone() }),
            [Token::Void] => Ok(Type::Void),
            [Token::Double] => Ok(Type::Double),
            [Token::Char] => Ok(Type::Char),
            [Token::Char, Token::Signed] => Ok(Type::SChar),
            [Token::Char, Token::Unsigned] => Ok(Type::UChar),
            _ => {
                let unique_specifiers: BTreeSet<_> = sorted_specifiers.iter().collect();
                if sorted_specifiers.is_empty()
                    || unique_specifiers.len() != sorted_specifiers.len()
                    || sorted_specifiers.contains(&Token::Double)
                    || sorted_specifiers.contains(&Token::Char)
                    || sorted_specifiers.contains(&Token::Void)
                    || sorted_specifiers
                        .iter()
                        .any(|s| matches!(s, Token::Identifier(_)))
                    || (sorted_specifiers.contains(&Token::Signed)
                        && sorted_specifiers.contains(&Token::Unsigned))
                {
                    bail!("Invalid type specifier");
                } else if sorted_specifiers.contains(&Token::Unsigned)
                    && sorted_specifiers.contains(&Token::Long)
                {
                    Ok(Type::Ulong)
                } else if sorted_specifiers.contains(&Token::Unsigned) {
                    Ok(Type::Uint)
                } else if sorted_specifiers.contains(&Token::Long) {
                    Ok(Type::Long)
                } else {
                    Ok(Type::Int)
                }
            }
        }
    }

    fn is_ident(&self, token: &Token) -> bool {
        matches!(token, Token::Identifier(_))
    }

    fn parse_type_and_storage_specifiers(
        &mut self,
        specifier_list: &[Token],
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

        let _type = self.parse_type(types)?;

        if storage_classes.len() > 1 {
            bail!(
                "expected at most one storage class specifier, got: {:?}",
                storage_classes
            );
        }

        let storage_class = if storage_classes.len() == 1 {
            match storage_classes[0] {
                Token::Static => Some(StorageClass::Static),
                Token::Extern => Some(StorageClass::Extern),
                _ => {
                    unreachable!()
                }
            }
        } else {
            None
        };

        Ok((_type, storage_class))
    }

    fn parse_expression_statement(&mut self) -> Result<BlockItem> {
        let expr = self.parse_expression()?;
        self.consume(&Token::Semicolon)?;
        Ok(BlockItem::Statement(Statement::Expression(
            ExpressionStatement { expr },
        )))
    }

    fn parse_block_statement(&mut self) -> Result<BlockItem> {
        self.depth += 1;
        let mut stmts = vec![];
        while !self.check(&Token::RBrace) {
            stmts.push(self.parse_statement()?);
        }
        self.consume(&Token::RBrace)?;
        self.depth -= 1;
        Ok(BlockItem::Statement(Statement::Compound(BlockStatement {
            stmts,
        })))
    }

    fn parse_if_statement(&mut self) -> Result<BlockItem> {
        self.consume(&Token::LParen)?;
        let condition = self.parse_expression()?;
        self.consume(&Token::RParen)?;
        let then_branch = self.parse_statement()?;

        if let BlockItem::Declaration(_) = then_branch {
            bail!("variable declaration not allowed in if body");
        }

        let else_branch = if self.is_next(&[Token::Else]) {
            Some(self.parse_statement()?)
        } else {
            None
        };

        Ok(BlockItem::Statement(Statement::If(IfStatement {
            condition,
            then_branch: then_branch.into(),
            else_branch: else_branch.into(),
        })))
    }

    fn parse_do_while_statement(&mut self) -> Result<BlockItem> {
        let body = self.parse_statement()?;
        self.consume(&Token::While)?;
        self.consume(&Token::LParen)?;
        let condition = self.parse_expression()?;
        self.consume(&Token::RParen)?;
        self.consume(&Token::Semicolon)?;
        Ok(BlockItem::Statement(Statement::DoWhile(DoWhileStatement {
            condition,
            body: body.into(),
            label: "".to_owned(),
        })))
    }

    fn parse_while_statement(&mut self) -> Result<BlockItem> {
        self.consume(&Token::LParen)?;
        let condition = self.parse_expression()?;
        self.consume(&Token::RParen)?;
        let body = self.parse_statement()?;

        if let BlockItem::Declaration(_) = body {
            bail!("variable declaration not allowed in while body");
        }

        Ok(BlockItem::Statement(Statement::While(WhileStatement {
            condition,
            body: body.into(),
            label: "".to_owned(),
        })))
    }

    fn parse_for_statement(&mut self) -> Result<BlockItem> {
        self.consume(&Token::LParen)?;

        let init = if self.is_next(&[Token::Semicolon]) {
            ForInit::Expression(None)
        } else if self.is_specifier(self.current.as_ref().unwrap()) {
            let decl = self.parse_var_or_fn_decl()?;
            ForInit::Declaration(match decl {
                BlockItem::Declaration(Declaration::Variable(var)) => var,
                _ => unreachable!(),
            })
        } else {
            let expr = self.parse_expression()?;
            self.consume(&Token::Semicolon)?;
            ForInit::Expression(Some(expr))
        };

        let condition = if self.is_next(&[Token::Semicolon]) {
            None
        } else {
            let expr = self.parse_expression()?;
            self.consume(&Token::Semicolon)?;
            Some(expr)
        };

        let post = if self.is_next(&[Token::RParen]) {
            None
        } else {
            let expr = self.parse_expression()?;
            self.consume(&Token::RParen)?;
            Some(expr)
        };

        let body = self.parse_statement()?;

        Ok(BlockItem::Statement(Statement::For(ForStatement {
            init,
            condition,
            post,
            body: body.into(),
            label: "".to_owned(),
        })))
    }

    fn parse_break_statement(&mut self) -> Result<BlockItem> {
        self.consume(&Token::Semicolon)?;
        Ok(BlockItem::Statement(Statement::Break(BreakStatement {
            label: "".to_owned(),
        })))
    }

    fn parse_continue_statement(&mut self) -> Result<BlockItem> {
        self.consume(&Token::Semicolon)?;
        Ok(BlockItem::Statement(Statement::Continue(
            ContinueStatement {
                label: "".to_owned(),
            },
        )))
    }

    fn parse_return_statement(&mut self) -> Result<BlockItem> {
        let expr = if self.is_next(&[Token::Semicolon]) {
            None
        } else {
            let expr = Some(self.parse_expression()?);
            self.consume(&Token::Semicolon)?;
            expr
        };
        Ok(BlockItem::Statement(Statement::Return(ReturnStatement {
            expr,
            target_type: self.current_target_type.clone(),
            belongs_to: self.current_fn.clone().unwrap(),
        })))
    }

    fn parse_expression(&mut self) -> Result<Expression> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expression> {
        let mut result = self.conditional()?;
        while self.is_next(&[Token::Equal]) {
            result = Expression::Assign(AssignExpression {
                lhs: result.into(),
                rhs: self.assignment()?.into(),
                op: Token::Equal,
                _type: Type::Dummy,
            });
        }
        Ok(result)
    }

    fn conditional(&mut self) -> Result<Expression> {
        let mut result = self.or()?;
        if self.is_next(&[Token::QuestionMark]) {
            let then_expr = self.parse_expression()?;

            if let Expression::Literal(_) = then_expr {
                bail!("expected expression, got literal");
            }

            self.consume(&Token::Colon)?;
            let else_expr = self.parse_expression()?;
            result = Expression::Conditional(ConditionalExpression {
                condition: result.into(),
                then_expr: then_expr.into(),
                else_expr: else_expr.into(),
                _type: Type::Dummy,
            });
        }
        Ok(result)
    }

    fn or(&mut self) -> Result<Expression> {
        let mut result = self.and()?;
        while self.is_next(&[Token::DoublePipe]) {
            result = Expression::Binary(BinaryExpression {
                kind: BinaryExpressionKind::Or,
                lhs: result.into(),
                rhs: self.and()?.into(),
                _type: Type::Dummy,
            });
        }
        Ok(result)
    }

    fn and(&mut self) -> Result<Expression> {
        let mut result = self.equality()?;
        while self.is_next(&[Token::DoubleAmpersand]) {
            result = Expression::Binary(BinaryExpression {
                kind: BinaryExpressionKind::And,
                lhs: result.into(),
                rhs: self.equality()?.into(),
                _type: Type::Dummy,
            });
        }
        Ok(result)
    }

    fn equality(&mut self) -> Result<Expression> {
        let mut result = self.relational()?;
        while self.is_next(&[Token::DoubleEqual, Token::BangEqual]) {
            let negation = match self.previous.as_ref().unwrap() {
                Token::BangEqual => true,
                Token::DoubleEqual => false,
                _ => unreachable!(),
            };
            result = Expression::Binary(BinaryExpression {
                kind: match negation {
                    true => BinaryExpressionKind::NotEqual,
                    false => BinaryExpressionKind::Equal,
                },
                lhs: result.into(),
                rhs: self.relational()?.into(),
                _type: Type::Dummy,
            });
        }
        Ok(result)
    }

    fn relational(&mut self) -> Result<Expression> {
        let mut result = self.term()?;
        while self.is_next(&[
            Token::Less,
            Token::Greater,
            Token::LessEqual,
            Token::GreaterEqual,
        ]) {
            let kind = match self.previous.as_ref() {
                Some(token) => match token {
                    Token::Less => BinaryExpressionKind::Less,
                    Token::Greater => BinaryExpressionKind::Greater,
                    Token::LessEqual => BinaryExpressionKind::LessEqual,
                    Token::GreaterEqual => BinaryExpressionKind::GreaterEqual,
                    _ => unreachable!(),
                },
                None => unreachable!(),
            };
            result = Expression::Binary(BinaryExpression {
                kind,
                lhs: result.into(),
                rhs: self.term()?.into(),
                _type: Type::Dummy,
            });
        }
        Ok(result)
    }

    fn term(&mut self) -> Result<Expression> {
        let mut result = self.factor()?;
        while self.is_next(&[Token::Plus, Token::Hyphen]) {
            let kind = match self.previous.as_ref() {
                Some(token) => match token {
                    Token::Plus => BinaryExpressionKind::Add,
                    Token::Hyphen => BinaryExpressionKind::Sub,
                    _ => unreachable!(),
                },
                None => unreachable!(),
            };
            result = Expression::Binary(BinaryExpression {
                kind,
                lhs: result.into(),
                rhs: self.factor()?.into(),
                _type: Type::Dummy,
            });
        }
        Ok(result)
    }

    fn factor(&mut self) -> Result<Expression> {
        let mut result = self.unary()?;
        while self.is_next(&[Token::Star, Token::Slash, Token::Percent]) {
            let kind = match self.previous.as_ref() {
                Some(token) => match token {
                    Token::Star => BinaryExpressionKind::Mul,
                    Token::Slash => BinaryExpressionKind::Div,
                    Token::Percent => BinaryExpressionKind::Rem,
                    _ => unreachable!(),
                },
                None => unreachable!(),
            };
            result = Expression::Binary(BinaryExpression {
                kind,
                lhs: result.into(),
                rhs: self.unary()?.into(),
                _type: Type::Dummy,
            });
        }
        Ok(result)
    }

    fn peek(&self, n: usize) -> Vec<Token> {
        let mut v = vec![self.current.clone().unwrap()];
        v.extend(self.tokens.iter().take(n - 1).cloned().collect::<Vec<_>>());
        v
    }

    fn unary(&mut self) -> Result<Expression> {
        if self.is_next(&[Token::Hyphen, Token::Tilde, Token::Bang]) {
            let op = self.previous.clone().unwrap();

            let expr = self.unary()?;
            return Ok(Expression::Unary(UnaryExpression {
                expr: expr.into(),
                kind: match op {
                    Token::Hyphen => UnaryExpressionKind::Negate,
                    Token::Tilde => UnaryExpressionKind::Complement,
                    Token::Bang => UnaryExpressionKind::Not,
                    _ => unreachable!(),
                },
                _type: Type::Dummy,
            }));
        } else if self.is_next(&[Token::Star]) {
            let expr = self.unary()?;
            return Ok(Expression::Deref(DerefExpression {
                expr: expr.into(),
                _type: Type::Dummy,
            }));
        } else if self.is_next(&[Token::Ampersand]) {
            let expr = self.unary()?;
            return Ok(Expression::AddrOf(AddrOfExpression {
                expr: expr.into(),
                _type: Type::Dummy,
            }));
        } else {
            let next_three_tokens = self.peek(3);
            match next_three_tokens.as_slice() {
                [Token::Sizeof, Token::LParen, _] => {
                    if self.is_type_specifier(next_three_tokens.last().unwrap()) {
                        self.consume(&Token::Sizeof)?;
                        self.consume(&Token::LParen)?;
                        let base_type = self.parse_type_name()?;
                        self.consume(&Token::RParen)?;
                        return Ok(Expression::SizeofT(SizeofTExpression {
                            t: base_type,
                            _type: Type::Dummy,
                        }));
                    } else {
                        self.consume(&Token::Sizeof)?;
                        let expr = self.unary()?;
                        return Ok(Expression::Sizeof(SizeofExpression {
                            expr: expr.into(),
                            _type: Type::Dummy,
                        }));
                    }
                }
                [Token::Sizeof, _, _] => {
                    self.consume(&Token::Sizeof)?;
                    let expr = self.unary()?;
                    return Ok(Expression::Sizeof(SizeofExpression {
                        expr: expr.into(),
                        _type: Type::Dummy,
                    }));
                }
                [Token::LParen, _, _] => {
                    if self.is_type_specifier(&next_three_tokens[1]) {
                        self.consume(&Token::LParen)?;
                        let base_type = self.parse_type_name()?;
                        self.consume(&Token::RParen)?;
                        let expr = self.unary()?;
                        return Ok(Expression::Cast(CastExpression {
                            target_type: base_type,
                            expr: expr.into(),
                            _type: Type::Dummy,
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
        let base_type = self.parse_type(specifier_list)?;
        match self.current.as_ref().unwrap() {
            Token::RParen => Ok(base_type),
            _ => {
                let decl = self.parse_abstract_declarator().unwrap();
                Ok(self.process_abstract_declarator(&decl, &base_type))
            }
        }
    }

    fn lookahead_until(&mut self, token: &Token) -> Vec<Token> {
        let v = [self.current.clone().unwrap()];
        v.iter()
            .chain(&self.tokens)
            .take_while(|&t| t != token)
            .cloned()
            .collect()
    }

    fn call(&mut self) -> Result<Expression> {
        let mut expr = self.primary()?;
        loop {
            if self.is_next(&[Token::LParen]) {
                let mut args = vec![];
                if !self.check(&Token::RParen) {
                    loop {
                        args.push(self.parse_expression()?);
                        if !self.is_next(&[Token::Comma]) {
                            break;
                        }
                    }
                }
                self.consume(&Token::RParen)?;
                expr = Expression::Call(CallExpression {
                    name: match expr {
                        Expression::Variable(var) => var.value,
                        _ => unreachable!(),
                    },
                    args,
                    _type: Type::Dummy,
                });
            } else if self.is_next(&[Token::LBracket]) {
                let index = self.parse_expression()?;
                self.consume(&Token::RBracket)?;
                expr = Expression::Subscript(SubscriptExpression {
                    expr: expr.into(),
                    index: index.into(),
                    _type: Type::Dummy,
                });
            } else if self.is_next(&[Token::Dot]) {
                let member = self
                    .consume(&Token::Identifier("".to_owned()))?
                    .unwrap()
                    .as_string();
                expr = Expression::Dot(DotExpression {
                    structure: expr.into(),
                    member,
                    _type: Type::Dummy,
                });
            } else if self.is_next(&[Token::Arrow]) {
                let member = self
                    .consume(&Token::Identifier("".to_owned()))?
                    .unwrap()
                    .as_string();
                expr = Expression::Arrow(ArrowExpression {
                    pointer: expr.into(),
                    member,
                    _type: Type::Dummy,
                });
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expression> {
        if self.is_next(&[
            Token::Constant(Const::Int(0)),
            Token::Constant(Const::Long(0)),
        ]) {
            match self.previous.as_ref().unwrap() {
                Token::Constant(n) => self.parse_number(n),
                _ => unreachable!(),
            }
        } else if self.is_next(&[Token::LParen]) {
            self.parse_grouping()
        } else if self.is_next(&[Token::Identifier("".to_owned())]) {
            match self.previous.as_ref().unwrap() {
                Token::Identifier(var) => self.parse_variable(var),
                _ => unreachable!(),
            }
        } else if self.is_next(&[Token::LBrace]) {
            let mut inits = vec![];
            loop {
                if self.is_next(&[Token::RBrace]) {
                    break;
                }
                inits.push(Initializer::Single(String::new(), self.parse_expression()?));

                if self.is_next(&[Token::Comma]) {
                    continue;
                }
            }
            if inits.is_empty() {
                bail!("empty compound literal");
            }
            Ok(Expression::Literal(LiteralExpression {
                name: String::new(),
                value: Initializer::Compound(String::new(), Type::Dummy, inits).into(),
                _type: Type::Dummy,
            }))
        } else if self.is_next(&[Token::CharLiteral('a')]) {
            match self.previous.as_ref().unwrap() {
                Token::CharLiteral(c) => self.parse_char(c),
                _ => unreachable!(),
            }
        } else if self.is_next(&[Token::StringLiteral("".to_owned())]) {
            match self.previous.as_ref().cloned().unwrap() {
                Token::StringLiteral(s) => self.parse_string(&s),
                _ => unreachable!(),
            }
        } else {
            println!("got token: {:?}, {:?}", self.current, self.previous);
            bail!("expected primary");
        }
    }

    fn parse_number(&self, n: &Const) -> Result<Expression> {
        Ok(Expression::Constant(ConstantExpression {
            value: *n,
            _type: Type::Dummy,
        }))
    }

    fn parse_variable(&self, var: &str) -> Result<Expression> {
        Ok(Expression::Variable(VariableExpression {
            value: var.to_owned(),
            _type: Type::Dummy,
        }))
    }

    fn parse_char(&self, c: &char) -> Result<Expression> {
        Ok(Expression::Constant(ConstantExpression {
            value: Const::Int(*c as i32),
            _type: Type::Dummy,
        }))
    }

    fn parse_string(&mut self, s: &str) -> Result<Expression> {
        let mut s = s.to_owned().clone();
        while self.is_next(&[Token::StringLiteral("".to_owned())]) {
            match self.previous.as_ref().unwrap() {
                Token::StringLiteral(s2) => s.push_str(s2),
                _ => unreachable!(),
            }
        }
        Ok(Expression::String(StringExpression {
            value: s.to_owned(),
            _type: Type::Dummy,
        }))
    }

    fn parse_grouping(&mut self) -> Result<Expression> {
        let expr = self.parse_expression();
        self.consume(&Token::RParen)?;
        expr
    }

    fn parse_abstract_declarator(&mut self) -> Result<AbstractDeclarator> {
        match self.current.as_ref().unwrap() {
            Token::Star => {
                self.consume(&Token::Star)?;
                let inner = match self.current.as_ref().unwrap() {
                    Token::Star | Token::LParen | Token::LBracket => {
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
        match self.current.as_ref().unwrap() {
            Token::LParen => {
                self.consume(&Token::LParen)?;
                let inner = self.parse_abstract_declarator()?;
                self.consume(&Token::RParen)?;

                if let Token::LBracket = self.current.as_ref().unwrap() {
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

        if let Some(Token::LBracket) = self.current {
            self.parse_abstract_array_decl_suffix(&new_decl)
        } else {
            Ok(new_decl)
        }
    }
}
