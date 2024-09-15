use crate::lexer::{Const, Token};
use anyhow::{bail, Result};
use std::collections::VecDeque;

pub struct Parser {
    pub tokens: VecDeque<Token>,
    pub current: Option<Token>,
    pub previous: Option<Token>,
    pub depth: usize,
    pub current_target_type: Option<Type>,
}

impl Parser {
    pub fn new(tokens: VecDeque<Token>) -> Parser {
        Parser {
            tokens,
            current: None,
            previous: None,
            depth: 0,
            current_target_type: None,
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

    fn parse_statement(&mut self) -> Result<BlockItem> {
        if self.is_next(&[
            Token::Int,
            Token::Long,
            Token::Double,
            Token::Signed,
            Token::Unsigned,
            Token::Static,
            Token::Extern,
        ]) {
            let mut spec_list = vec![];
            spec_list.push(self.previous.clone().unwrap());
            self.parse_specifier_list(&mut spec_list)?;
            self.parse_declaration(&spec_list)
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

    fn consume_while(&mut self, token: &Token) -> Result<Vec<Token>> {
        let mut tokens = vec![];
        while !self.check(token) {
            tokens.push(self.advance().unwrap());
        }
        Ok(tokens)
    }

    fn is_type_specifier(&self, token: &Token) -> bool {
        matches!(token, Token::Int | Token::Long | Token::Unsigned | Token::Signed | Token::Double)
    }

    fn is_storage_class_specifier(&self, token: &Token) -> bool {
        matches!(token, Token::Static | Token::Extern)
    }

    fn is_specifier(&self, token: &Token) -> bool {
        self.is_type_specifier(token) || self.is_storage_class_specifier(token)
    }

    fn parse_specifier_list(&mut self, spec_list: &mut Vec<Token>) -> Result<Vec<Token>> {
        while self.is_specifier(self.current.as_ref().unwrap()) {
            spec_list.push(self.current.clone().unwrap());
            self.advance();
        }
        println!("first 10 tokens after parsing specifier list: {:?}", self.tokens.iter().take(10).collect::<Vec<&Token>>());
        Ok(spec_list.to_vec())
    }

    fn parse_declaration(&mut self, specifier_list: &[Token]) -> Result<BlockItem> {
        let (base_type, storage_class) = self.parse_type_and_storage_specifiers(specifier_list)?;

        let declarator = self.parse_declarator()?;

        let (name, decl_type, params) = self.process_declarator(&declarator, &base_type)?;

        let some_fn_type = Type::Func { params: vec![], ret: Box::new(Type::Int) };

        match decl_type {
            Type::Func { params: _, ret: _ } => {
                self.parse_function_declaration(&name, &params, decl_type, storage_class)
            }
            _ => {
                let init = if self.is_next(&[Token::Equal]) {
                    Some(self.parse_expression()?)
                } else {
                    None
                };

                Ok(BlockItem::Declaration(Declaration::Variable(VariableDeclaration { name, _type: decl_type, init, storage_class, is_global: self.depth == 0 })))
            }
        }
    }

    fn parse_declarator(&mut self) -> Result<Declarator> {
        if self.is_next(&[Token::Star]) {
            let inner = self.parse_declarator()?;
            return Ok(Declarator::Pointer(Box::new(inner)));
        } else {
            self.parse_direct_declarator()
        }
    }

    fn parse_direct_declarator(&mut self) -> Result<Declarator> {
        let simple_declarator = self.parse_simple_declarator()?;
        if self.is_next(&[Token::LParen]) {
            let params = self.parse_param_list()?;
            Ok(Declarator::Func(params, Box::new(simple_declarator)))
        } else {
            Ok(simple_declarator)
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
                println!("got token: {:?}", token);
                bail!("a simple declarator");
            }
        }
    }

    fn parse_param_list(&mut self) -> Result<Vec<ParamInfo>> {
        // if we see '(', 'void', ')', then we have no params
        // use lookahed_until to check for this
        let in_front_of_us = self.lookahead_until(&Token::RParen);
        println!("in_front_of_us: {:?}", in_front_of_us);

        if in_front_of_us == vec![Token::Void] {
            // No params - consume these three tokens and return empty list
            self.consume(&Token::Void)?;
            self.consume(&Token::RParen)?;
            
            println!("no params");
            Ok(vec![])
        } else {
            let _ = self.consume(&Token::LParen);
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
    
    fn parse_param(&mut self) -> Result<ParamInfo> {
        let specifier_list = self.consume_while(&Token::Identifier("".to_owned()))?;
        let param_t = self.parse_type(&specifier_list)?;
        let param_decl = self.parse_declarator()?;
        Ok((param_t, param_decl.into()))
    }
    
    fn process_declarator(&mut self, declarator: &Declarator, base_type: &Type) -> Result<(String, Type, Vec<String>)> {
        let some_fn_type = Type::Func { params: vec![], ret: Box::new(Type::Int) };
        match declarator {
            Declarator::Ident(name) => Ok((name.clone(), base_type.clone(), vec![])),
            Declarator::Pointer(decl) => {
                let derived_type = Type::Pointer(base_type.clone().into());
                return self.process_declarator(decl, &derived_type);
            }
            Declarator::Func(params, decl) => {
                match *decl.clone() {
                    Declarator::Ident(name) => {
                        let mut param_names = vec![];
                        let mut param_types = vec![];

                        for (param_base_type, param_declarator) in params {
                            let (param_name, param_type, _) = self.process_declarator(&param_declarator, &param_base_type)?;
                            if std::mem::discriminant(&param_type) == std::mem::discriminant(&some_fn_type) {
                                bail!("Function pointers in parameters are not supported.")
                            }
                            param_names.push(param_name);
                            param_types.push(param_type);
                        }

                        let derived_type = Type::Func { params: param_types, ret: base_type.clone().into() };
                        return Ok((name.clone(), derived_type, param_names));
                    }
                    _ => bail!("Can't apply additional type derivations to a function type."),
                }
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
        self.current_target_type = Some(_type.clone());

        let body = if self.check(&Token::Semicolon) {
            self.consume(&Token::Semicolon)?;
            None
        } else if self.check(&Token::LBrace) {
            self.consume(&Token::LBrace)?;
            Some(self.parse_block_statement()?)
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

    fn parse_type(&mut self, specifier_list: &[Token]) -> Result<Type> {
        if specifier_list == &[Token::Double] {
            return Ok(Type::Double);
        }

        if specifier_list.contains(&Token::Double) {
            bail!(
                "can't combine double with other specifiers: {:?}",
                specifier_list
            );
        }

        if self.contains_no_specifiers(&specifier_list)
            || specifier_list.is_empty()
            || self.contains_same_specifier_twice(specifier_list)
            || self.contains_both_signed_and_unsigned(specifier_list)
        {
            bail!("invalid type specifier list: {:?}", specifier_list);
        }

        if specifier_list.contains(&Token::Unsigned) && specifier_list.contains(&Token::Long) {
            return Ok(Type::Ulong);
        }

        if specifier_list.contains(&Token::Unsigned) {
            return Ok(Type::Uint);
        }

        if specifier_list.contains(&Token::Long) {
            return Ok(Type::Long);
        }

        Ok(Type::Int)
    }

    fn contains_no_specifiers(&self, specifier_list: &[Token]) -> bool {
        !specifier_list.iter().all(|specifier| match specifier {
            Token::Int
            | Token::Long
            | Token::Double
            | Token::Signed
            | Token::Unsigned
            | Token::Void => true,
            _ => false,
        })
    }

    fn contains_same_specifier_twice(&self, specifier_list: &[Token]) -> bool {
        let mut seen = vec![];
        for specifier in specifier_list {
            if seen.contains(specifier) {
                return true;
            }
            seen.push(specifier.clone());
        }
        false
    }

    fn contains_both_signed_and_unsigned(&self, specifier_list: &[Token]) -> bool {
        specifier_list.contains(&Token::Signed) && specifier_list.contains(&Token::Unsigned)
    }

    fn parse_type_and_storage_specifiers(
        &mut self,
        specifier_list: &[Token],
    ) -> Result<(Type, Option<StorageClass>)> {
        let mut types = vec![];
        let mut storage_classes = vec![];

        for specifier in specifier_list {
            if specifier == &Token::Int
                || specifier == &Token::Long
                || specifier == &Token::Double
                || specifier == &Token::Unsigned
                || specifier == &Token::Signed
                || specifier == &Token::Void
            {
                types.push(specifier.clone());
            } else {
                storage_classes.push(specifier.clone());
            }
        }

        let _type = self.parse_type(&types)?;

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
                _ => unreachable!(),
            }
        } else {
            None
        };

        Ok((_type, storage_class))
    }

    fn parse_variable_declaration(
        &mut self,
        name: &str,
        _type: Type,
        storage_class: Option<StorageClass>,
    ) -> Result<BlockItem> {
        println!("first 10 tokens before parsing variable declaration: {:?}", self.tokens.iter().take(10).collect::<Vec<&Token>>());
        let init = Some(self.parse_expression()?);
        self.consume(&Token::Semicolon)?;
        Ok(BlockItem::Declaration(Declaration::Variable(
            VariableDeclaration {
                name: name.to_owned(),
                init,
                storage_class,
                is_global: self.depth == 0,
                _type,
            },
        )))
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
        } else if self.is_next(&[
            Token::Int,
            Token::Long,
            Token::Double,
            Token::Signed,
            Token::Unsigned,
            Token::Static,
            Token::Extern,
        ]) {
            let mut specifier_list = vec![];
            specifier_list.push(self.previous.clone().unwrap());
            specifier_list.extend(self.consume_while(&Token::Identifier("".to_owned()))?);
            let (_type, _storage_class) =
                self.parse_type_and_storage_specifiers(&specifier_list)?;
            let decl = self.parse_declaration(&specifier_list)?;
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
        let expr = self.parse_expression()?;
        self.consume(&Token::Semicolon)?;
        Ok(BlockItem::Statement(Statement::Return(ReturnStatement {
            expr,
            target_type: self.current_target_type.clone(),
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
        let result = self.or()?;
        if self.is_next(&[Token::QuestionMark]) {
            let then_expr = self.parse_expression()?;
            self.consume(&Token::Colon)?;
            let else_expr = self.parse_expression()?;
            Ok(Expression::Conditional(ConditionalExpression {
                condition: result.into(),
                then_expr: then_expr.into(),
                else_expr: else_expr.into(),
                _type: Type::Dummy,
            }))
        } else {
            Ok(result)
        }
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

    fn unary(&mut self) -> Result<Expression> {
        if self.is_next(&[Token::Hyphen, Token::Tilde, Token::Bang]) {
            let op = self.previous.clone().unwrap();

            // Handle unary operators
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
        } else if self.is_next(&[Token::LParen]) {
            let specifier_list = self.lookahead_until(&Token::RParen);

            let t = self.parse_type(&specifier_list);

            if t.is_err() {
                return self.parse_grouping();
            }

            let spam = self.consume_while(&Token::RParen)?;

            assert_eq!(specifier_list, spam);

            self.consume(&Token::RParen)?;

            let expr = self.unary()?;

            return Ok(Expression::Cast(CastExpression {
                target_type: t?,
                expr: expr.into(),
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
        }

        self.call()
    }

    fn lookahead_until(&mut self, token: &Token) -> Vec<Token> {
        let mut v = vec![];
        v.push(self.current.clone().unwrap());
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

    fn parse_grouping(&mut self) -> Result<Expression> {
        let expr = self.parse_expression();
        self.consume(&Token::RParen)?;
        expr
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockItem {
    Statement(Statement),
    Declaration(Declaration),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Variable(VariableDeclaration),
    Function(FunctionDeclaration),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub name: String,
    pub _type: Type,
    pub init: Option<Expression>,
    pub storage_class: Option<StorageClass>,
    pub is_global: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Long,
    Uint,
    Ulong,
    Double,
    Func { params: Vec<Type>, ret: Box<Type> },
    Pointer(Box<Type>),
    Dummy,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Program(ProgramStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    If(IfStatement),
    Compound(BlockStatement),
    DoWhile(DoWhileStatement),
    While(WhileStatement),
    For(ForStatement),
    Break(BreakStatement),
    Continue(ContinueStatement),
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProgramStatement {
    pub block_items: Vec<BlockItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    pub name: String,
    pub _type: Type,
    pub params: Vec<String>,
    pub body: Box<Option<BlockItem>>,
    pub is_global: bool,
    pub storage_class: Option<StorageClass>,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum StorageClass {
    Static,
    Extern,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStatement {
    pub expr: Expression,
    pub target_type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub condition: Expression,
    pub then_branch: Box<BlockItem>,
    pub else_branch: Box<Option<BlockItem>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStatement {
    pub expr: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement {
    pub stmts: Vec<BlockItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DoWhileStatement {
    pub condition: Expression,
    pub body: Box<BlockItem>,
    pub label: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Box<BlockItem>,
    pub label: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStatement {
    pub init: ForInit,
    pub condition: Option<Expression>,
    pub post: Option<Expression>,
    pub body: Box<BlockItem>,
    pub label: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ForInit {
    Declaration(VariableDeclaration),
    Expression(Option<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BreakStatement {
    pub label: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ContinueStatement {
    pub label: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Constant(ConstantExpression),
    Variable(VariableExpression),
    Unary(UnaryExpression),
    Binary(BinaryExpression),
    Assign(AssignExpression),
    Conditional(ConditionalExpression),
    Call(CallExpression),
    Cast(CastExpression),
    Deref(DerefExpression),
    AddrOf(AddrOfExpression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AddrOfExpression {
    pub expr: Box<Expression>,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DerefExpression {
    pub expr: Box<Expression>,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstantExpression {
    pub value: Const,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableExpression {
    pub value: String,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    pub kind: UnaryExpressionKind,
    pub expr: Box<Expression>,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryExpressionKind {
    Negate,
    Complement,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub kind: BinaryExpressionKind,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryExpressionKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignExpression {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub op: Token,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConditionalExpression {
    pub condition: Box<Expression>,
    pub then_expr: Box<Expression>,
    pub else_expr: Box<Expression>,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub name: String,
    pub args: Vec<Expression>,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CastExpression {
    pub target_type: Type,
    pub expr: Box<Expression>,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq)]
enum Declarator {
    Ident(String),
    Pointer(Box<Declarator>),
    Func(Vec<ParamInfo>, Box<Declarator>),
}

type ParamInfo = (Type, Box<Declarator>);