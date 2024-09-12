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
        if self.is_next(&[Token::Int, Token::Long, Token::Static, Token::Extern]) {
            let mut specifier_list = vec![];
            specifier_list.push(self.previous.clone().unwrap());
            specifier_list.extend(self.consume_while(&Token::Identifier("".to_owned()))?);
            self.parse_declaration(&specifier_list)
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

    fn parse_declaration(&mut self, specifier_list: &[Token]) -> Result<BlockItem> {
        let (_type, storage_class) = self.parse_type_and_storage_specifiers(specifier_list)?;
        let name = self
            .consume(&Token::Identifier("".to_owned()))?
            .unwrap()
            .as_string();
        if self.is_next(&[Token::LParen]) {
            self.parse_function_declaration(&name, _type, storage_class)
        } else if self.is_next(&[Token::Equal]) {
            self.parse_variable_declaration(&name, _type, storage_class)
        } else if self.is_next(&[Token::Semicolon]) {
            Ok(BlockItem::Declaration(Declaration::Variable(
                VariableDeclaration {
                    name,
                    init: None,
                    storage_class,
                    is_global: self.depth == 0,
                    _type,
                },
            )))
        } else {
            bail!("expected function or variable declaration");
        }
    }

    fn parse_function_declaration(
        &mut self,
        name: &str,
        _type: Type,
        storage_class: Option<StorageClass>,
    ) -> Result<BlockItem> {
        self.current_target_type = Some(_type.clone());

        let params = self.parse_parameters()?;

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
                params: params.iter().map(|(_, name)| name.to_owned()).collect(),
                body: body.into(),
                is_global: self.depth == 0,
                storage_class,
                _type: Type::Func {
                    params: params.iter().map(|(t, _)| t.clone()).collect(),
                    ret: Box::new(_type),
                },
            },
        )))
    }

    fn parse_type(&mut self, specifier_list: &[Token]) -> Result<Type> {
        if specifier_list == [Token::Int] {
            Ok(Type::Int)
        } else if specifier_list == [Token::Int, Token::Long]
            || specifier_list == [Token::Long, Token::Int]
            || specifier_list == [Token::Long]
        {
            Ok(Type::Long)
        } else {
            bail!("invalid type specifier: {:?}", specifier_list);
        }
    }

    fn parse_type_and_storage_specifiers(
        &mut self,
        specifier_list: &[Token],
    ) -> Result<(Type, Option<StorageClass>)> {
        let mut types = vec![];
        let mut storage_classes = vec![];

        for specifier in specifier_list {
            if specifier == &Token::Int || specifier == &Token::Long || specifier == &Token::Void {
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

    fn parse_parameters(&mut self) -> Result<Vec<(Type, String)>> {
        if self.is_next(&[Token::Void]) {
            self.consume(&Token::RParen)?;
            return Ok(vec![]);
        } else if self.is_next(&[Token::RParen]) {
            return Ok(vec![]);
        }

        let mut params = vec![];

        loop {
            let specifier_list = self.consume_while(&Token::Identifier("".to_owned()))?;

            let _type = self.parse_type(&specifier_list)?;
            let param = self
                .consume(&Token::Identifier("".to_owned()))?
                .unwrap()
                .as_string();
            params.push((_type, param));
            if self.is_next(&[Token::RParen]) {
                break;
            }
            self.consume(&Token::Comma)?;
        }

        Ok(params)
    }

    fn parse_variable_declaration(
        &mut self,
        name: &str,
        _type: Type,
        storage_class: Option<StorageClass>,
    ) -> Result<BlockItem> {
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
        } else if self.is_next(&[Token::Int, Token::Long, Token::Static, Token::Extern]) {
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
        }

        if self.is_next(&[Token::LParen]) {
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
            println!(
                "self.current, self.previous: {:?}, {:?}",
                self.current, self.previous
            );
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
    Func { params: Vec<Type>, ret: Box<Type> },
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
