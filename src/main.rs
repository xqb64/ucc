use std::{collections::VecDeque, fs::File, path::PathBuf};

use anyhow::{bail, Result};
use structopt::StructOpt;

use std::io::{Write, Read};

fn main() {
    let opts = Opt::from_args();
    if let Err(e) = run(&opts) {
        eprintln!("ucc: {}", e);
        std::process::exit(1);
    }
}

fn run(opts: &Opt) -> Result<()> {
    let preprocessed = preprocess(&opts.path)?;
    let src = std::fs::read_to_string(preprocessed)?;

    let Some(tokens) = Lexer::new(src).map(|token| {
        if token != Token::Error {
            Some(token)
        } else {
            None
        }
    }).collect::<Option<VecDeque<_>>>()
    else {
        bail!("failed to tokenize");
    };

    if opts.lex {
        println!("{:?}", tokens);
        std::process::exit(0);
    }

    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    if opts.parse {
        println!("{:?}", ast);
        std::process::exit(0);
    }

    let asm_prog = codegen(ast)?;

    if opts.codegen {
        println!("{:?}", asm_prog);
        std::process::exit(0);
    }

    let mut f = File::create(opts.path.with_extension("s"))?;

    asm_prog.emit(&mut f)?;

    std::process::Command::new("gcc")
        .arg("-o")
        .arg(opts.path.with_extension(""))
        .arg(opts.path.with_extension("s"))
        .status()?;

    Ok(())
}

fn preprocess(path: &PathBuf) -> Result<PathBuf> {
    let new_path = path.clone().with_extension("i");
    
    std::process::Command::new("gcc")
        .arg("-E")
        .arg("-P")
        .arg(path)
        .arg("-o")
        .arg(new_path.clone())
        .status()?;

    Ok(new_path)
}

#[derive(Debug, StructOpt)]
struct Opt {
    path: PathBuf,

    #[structopt(name = "lex", long)]
    lex: bool,

    #[structopt(name = "parse", long)]
    parse: bool,

    #[structopt(name = "codegen", long)]
    codegen: bool,
}

struct Lexer {
    src: String,
    pos: usize,
}

impl Lexer {
    fn new(src: String) -> Lexer {
        Lexer { src, pos: 0 }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.src.chars().nth(self.pos).is_some_and(|ch| ch.is_whitespace()) {
                self.pos += 1;
            } else {
                break;
            }
        }

        let src = &self.src[self.pos..];

        let punctuation_re = regex::Regex::new(r"^[(){};]").unwrap();
        let keyword_re = regex::Regex::new(r"^int\b|^void\b|^return\b").unwrap();
        let constant_re = regex::Regex::new(r"^[0-9]+\b").unwrap();
        let identifier_re = regex::Regex::new(r"^[a-zA-Z_]\w*\b").unwrap();

        let token = if let Some(m) = punctuation_re.find(src) {
            self.pos += m.as_str().len();
            match m.as_str() {
                "(" => Token::LParen,
                ")" => Token::RParen,
                "{" => Token::LBrace,
                "}" => Token::RBrace,
                ";" => Token::Semicolon,
                _ => unreachable!(),
            }
        } else if let Some(m) = keyword_re.find(src) {
            self.pos += m.as_str().len();
            match m.as_str() {
                "int" => Token::Int,
                "void" => Token::Void,
                "return" => Token::Return,
                _ => unreachable!(),
            }
        } else if let Some(m) = constant_re.find(src) {
            self.pos += m.as_str().len();
            Token::Constant(m.as_str().parse().unwrap())
        } else if let Some(m) = identifier_re.find(src) {
            self.pos += m.as_str().len();
            Token::Identifier(m.as_str().to_string())
        } else {
            if src.is_empty() {
                return None;
            }
            Token::Error
        };
        
        Some(token)
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Token {
    Int,
    Void,
    Return,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Semicolon,
    Identifier(String),
    Constant(i32),
    Error,
}

impl Token {
    fn as_const(&self) -> i32 {
        match self {
            Token::Constant(n) => *n,
            _ => unreachable!(),
        }
    }

    fn as_string(&self) -> String {
        match self {
            Token::Identifier(s) => s.to_owned(),
            _ => unreachable!(),
        }
    }
}

struct Parser {
    tokens: VecDeque<Token>,
    current: Option<Token>,
    previous: Option<Token>,
}

impl Parser {
    fn new(tokens: VecDeque<Token>) -> Parser {
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

    fn parse(&mut self) -> Result<Statement> {
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
        let name = self.consume(&Token::Identifier("".to_string()))?.unwrap().as_string();
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
        let n = self.consume(&Token::Constant(0))?.unwrap().as_const();
        Ok(Expression::Constant(n))
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Statement {
    Program(ProgramStatement),
    Function(FunctionDeclaration),
    Return(Expression),
}

#[derive(Debug, Clone, PartialEq)]
struct ProgramStatement {
    stmts: Vec<FunctionDeclaration>,
}

#[derive(Debug, Clone, PartialEq)]
struct FunctionDeclaration {
    name: String,
    stmts: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
enum Expression {
    Constant(i32),
}

#[derive(Debug, Clone, PartialEq)]
struct AsmProgram {
    functions: Vec<AsmFunction>,
}

#[derive(Debug, Clone, PartialEq)]
struct AsmFunction {
    name: String,
    instructions: Vec<AsmInstruction>,
}

#[derive(Debug, Clone, PartialEq)]
enum AsmInstruction {
    Mov { src: AsmOperand, dst: AsmOperand },
    Ret,
}

#[derive(Debug, Clone, PartialEq)]
enum AsmOperand {
    Imm(i32),
    Register(Register),
}

#[derive(Debug, Clone, PartialEq)]
enum Register {
    AX,
}

fn codegen(ast: Statement) -> Result<AsmProgram> {
    match ast {
        Statement::Program(program) => {
            let functions = program.stmts.into_iter().map(|func| {
                codegen_function(func)
            }).collect();
            Ok(AsmProgram { functions })
        }
        _ => bail!("expected program"),
    }
}

fn codegen_function(func: FunctionDeclaration) -> AsmFunction {
    let instructions = func.stmts.into_iter().map(|stmt| {
        match stmt {
            Statement::Return(expr) => {
                let src = codegen_expression(expr);
                vec![AsmInstruction::Mov { src, dst: AsmOperand::Register(Register::AX) }, AsmInstruction::Ret]
            }
            _ => unreachable!(),
        }
    }).flatten().collect();
    AsmFunction { name: func.name, instructions }
}

fn codegen_expression(expr: Expression) -> AsmOperand {
    match expr {
        Expression::Constant(n) => AsmOperand::Imm(n),
    }
}

trait Emit {
    fn emit(&self, f: &mut File) -> Result<()>;
}

impl Emit for AsmProgram {
    fn emit(&self, f: &mut File) -> Result<()> {
        for func in &self.functions {
            writeln!(f, ".globl {}", func.name)?;
            writeln!(f, "{}:", func.name)?;
            
            for instr in &func.instructions {
                instr.emit(f)?;
            }
        }

        Ok(())
    }    
}

impl Emit for AsmInstruction {
    fn emit(&self, f: &mut File) -> Result<()> {
        write!(f, "  ")?;

        match self {
            AsmInstruction::Mov { src, dst } => {
                write!(f, "mov ")?;
                src.emit(f)?;
                write!(f, ", ")?;
                dst.emit(f)?;
            }
            AsmInstruction::Ret => {
                write!(f, "ret")?;
            }
        }

        writeln!(f)?;

        Ok(())
    }
}

impl Emit for AsmOperand {
    fn emit(&self, f: &mut File) -> Result<()> {
        match self {
            AsmOperand::Imm(n) => write!(f, "${}", n)?,
            AsmOperand::Register(reg) => reg.emit(f)?,
        }
        Ok(())
    }
}

impl Emit for Register {
    fn emit(&self, f: &mut File) -> Result<()> {
        match self {
            Register::AX => write!(f, "%eax")?,
        }
        Ok(())
    }
}