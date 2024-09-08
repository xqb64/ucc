use crate::lexer::Token;
use anyhow::{bail, Result};
use std::collections::VecDeque;

pub struct Parser {
    pub tokens: VecDeque<Token>,
    pub current: Option<Token>,
    pub previous: Option<Token>,
    pub depth: usize,
}

impl Parser {
    pub fn new(tokens: VecDeque<Token>) -> Parser {
        Parser {
            tokens,
            current: None,
            previous: None,
            depth: 0,
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
        Ok(Statement::Program(ProgramStatement { stmts }))
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
        if self.is_next(&[Token::Int]) {
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
        let name = self
            .consume(&Token::Identifier("".to_owned()))?
            .unwrap()
            .as_string();
        if self.is_next(&[Token::LParen]) {
            self.parse_function_declaration(&name)
        } else if self.is_next(&[Token::Equal]) {
            self.parse_variable_declaration(&name)
        } else if self.is_next(&[Token::Semicolon]) {
            Ok(BlockItem::Declaration(Declaration::Variable(
                VariableDeclaration { name, init: None },
            )))
        } else {
            bail!("expected function or variable declaration");
        }
    }

    fn parse_function_declaration(&mut self, name: &str) -> Result<BlockItem> {
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
        Ok(BlockItem::Declaration(Declaration::Function(
            FunctionDeclaration {
                name: name.to_owned(),
                params,
                body: body.into(),
                is_global: self.depth == 0,
            },
        )))
    }

    fn parse_parameters(&mut self) -> Result<Vec<String>> {
        if self.is_next(&[Token::Void]) {
            self.consume(&Token::RParen)?;
            return Ok(vec![]);
        } else if self.is_next(&[Token::RParen]) {
            return Ok(vec![]);
        }

        let mut params = vec![];

        loop {
            self.consume(&Token::Int)?;
            let param = self
                .consume(&Token::Identifier("".to_owned()))?
                .unwrap()
                .as_string();
            params.push(param);
            if self.is_next(&[Token::RParen]) {
                break;
            }
            self.consume(&Token::Comma).unwrap();
        }

        Ok(params)
    }

    fn parse_variable_declaration(&mut self, name: &str) -> Result<BlockItem> {
        let init = Some(self.parse_expression()?);
        self.consume(&Token::Semicolon)?;
        Ok(BlockItem::Declaration(Declaration::Variable(
            VariableDeclaration {
                name: name.to_owned(),
                init,
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
        println!("here");
        let body = self.parse_statement()?;
        self.consume(&Token::While)?;
        println!("here2");
        self.consume(&Token::LParen)?;
        println!("here3");
        let condition = self.parse_expression()?;
        println!("here4");
        self.consume(&Token::RParen)?;
        println!("here5");
        self.consume(&Token::Semicolon)?;
        println!("here6");
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

        // FIXME: dirty test-passing hack
        match body {
            BlockItem::Declaration(_) => bail!("variable declaration not allowed in while body"),
            _ => {}
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
        } else if self.is_next(&[Token::Int]) {
            let decl = self.parse_declaration()?;
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
            });
        }
        Ok(result)
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
            }));
        }
        self.call()
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
                        Expression::Variable(var) => var,
                        _ => unreachable!(),
                    },
                    args,
                });
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expression> {
        if self.is_next(&[Token::Constant(0)]) {
            match self.previous.as_ref().unwrap() {
                Token::Constant(n) => self.parse_number(*n),
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

    fn parse_number(&self, n: i32) -> Result<Expression> {
        Ok(Expression::Constant(n))
    }

    fn parse_variable(&self, var: &str) -> Result<Expression> {
        Ok(Expression::Variable(var.to_owned()))
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
    pub init: Option<Expression>,
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
    pub stmts: Vec<BlockItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    pub name: String,
    pub params: Vec<String>,
    pub body: Box<Option<BlockItem>>,
    pub is_global: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStatement {
    pub expr: Expression,
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
    Constant(i32),
    Variable(String),
    Unary(UnaryExpression),
    Binary(BinaryExpression),
    Assign(AssignExpression),
    Conditional(ConditionalExpression),
    Call(CallExpression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    pub kind: UnaryExpressionKind,
    pub expr: Box<Expression>,
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConditionalExpression {
    pub condition: Box<Expression>,
    pub then_expr: Box<Expression>,
    pub else_expr: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub name: String,
    pub args: Vec<Expression>,
}
