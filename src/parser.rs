use std::collections::VecDeque;
use crate::lexer::Token;
use anyhow::{bail, Result};

pub struct Parser {
    pub tokens: VecDeque<Token>,
    pub current: Option<Token>,
    pub previous: Option<Token>,
}

impl Parser {
    pub fn new(tokens: VecDeque<Token>) -> Parser {
        Parser {
            tokens,
            current: None,
            previous: None,
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
        bail!("expected {:?}", token);
    }

    fn check(&self, token: &Token) -> bool {
        std::mem::discriminant(self.current.as_ref().unwrap()) == std::mem::discriminant(token)
    }

    pub fn parse(&mut self) -> Result<Statement> {
        self.advance();
        let mut stmts = vec![];
        while self.current.is_some() {
            stmts.push(match self.parse_statement()? {
                Statement::Function(func) => func,
                _ => unreachable!(),
            });
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

    fn parse_statement(&mut self) -> Result<Statement> {
        if self.is_next(&[Token::Int]) {
            self.parse_function_declaration()
        } else if self.is_next(&[Token::Return]) {
            self.parse_return_statement()
        } else {
            bail!("expected int or return");
        }
    }

    fn parse_function_declaration(&mut self) -> Result<Statement> {
        let name = self
            .consume(&Token::Identifier("".to_string()))?
            .unwrap()
            .as_string();
        self.consume(&Token::LParen)?;
        self.consume(&Token::Void)?;
        self.consume(&Token::RParen)?;
        self.consume(&Token::LBrace)?;
        let mut stmts = vec![];
        while !self.check(&Token::RBrace) {
            stmts.push(self.parse_statement()?);
        }
        self.consume(&Token::RBrace)?;
        Ok(Statement::Function(FunctionDeclaration { name, stmts }))
    }

    fn parse_return_statement(&mut self) -> Result<Statement> {
        let expr = self.parse_expression()?;
        self.consume(&Token::Semicolon)?;
        Ok(Statement::Return(expr))
    }

    fn parse_expression(&mut self) -> Result<Expression> {
        self.unary()
    }

    fn unary(&mut self) -> Result<Expression> {
        if self.is_next(&[Token::Hyphen, Token::Tilde]) {
            let op = self.previous.clone().unwrap();
            let expr = self.unary()?;
            return Ok(Expression::Unary(UnaryExpression {
                expr: expr.into(),
                kind: match op {
                    Token::Hyphen => UnaryExpressoinKind::Negate,
                    Token::Tilde => UnaryExpressoinKind::Complement,
                    _ => unreachable!(),
                },
            }));
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expression> {
        if self.is_next(&[Token::Constant(0)]) {
            match self.previous.as_ref().unwrap() {
                Token::Constant(n) => self.parse_number(*n),
                _ => unreachable!(),
            }
        } else if self.is_next(&[Token::LParen]) {
            self.parse_grouping()
        } else {
            bail!("expected primary");
        }
    }

    fn parse_number(&mut self, n: i32) -> Result<Expression> {
        Ok(Expression::Constant(n))
    }

    fn parse_grouping(&mut self) -> Result<Expression> {
        let expr = self.parse_expression();
        self.consume(&Token::RParen)?;
        expr
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Program(ProgramStatement),
    Function(FunctionDeclaration),
    Return(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProgramStatement {
    pub stmts: Vec<FunctionDeclaration>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    pub name: String,
    pub stmts: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Constant(i32),
    Unary(UnaryExpression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    pub kind: UnaryExpressoinKind,
    pub expr: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryExpressoinKind {
    Negate,
    Complement,
}