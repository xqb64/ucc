use std::{collections::VecDeque, fs::File, path::PathBuf};

use anyhow::{bail, Result};
use structopt::StructOpt;

use std::io::Write;

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

    let Some(tokens) = Lexer::new(src)
        .map(|token| {
            if token != Token::Error {
                Some(token)
            } else {
                None
            }
        })
        .collect::<Option<VecDeque<_>>>()
    else {
        bail!("failed to tokenize");
    };

    if opts.lex {
        println!("{:?}", tokens);
        std::process::exit(0);
    }

    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    let tac = ast.irfy();

    if opts.tacky {
        println!("{:?}", tac);
        std::process::exit(0);
    }

    if opts.parse {
        println!("{:?}", ast);
        std::process::exit(0);
    }

    let mut asm_prog = codegen(tac)?.replace_pseudo().fixup();

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

    #[structopt(name = "tacky", long)]
    tacky: bool,

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
            if self
                .src
                .chars()
                .nth(self.pos)
                .is_some_and(|ch| ch.is_whitespace())
            {
                self.pos += 1;
            } else {
                break;
            }
        }

        let src = &self.src[self.pos..];

        let punctuation_re = regex::Regex::new(r"^[-~(){};]").unwrap();
        let punctuation_double_re = regex::Regex::new(r"^--").unwrap();
        let keyword_re = regex::Regex::new(r"^int\b|^void\b|^return\b").unwrap();
        let constant_re = regex::Regex::new(r"^[0-9]+\b").unwrap();
        let identifier_re = regex::Regex::new(r"^[a-zA-Z_]\w*\b").unwrap();

        let token = if let Some(m) = punctuation_re.find(src) {
            self.pos += m.as_str().len();
            match m.as_str() {
                "-" => Token::Hyphen,
                "~" => Token::Tilde,
                "(" => Token::LParen,
                ")" => Token::RParen,
                "{" => Token::LBrace,
                "}" => Token::RBrace,
                ";" => Token::Semicolon,
                _ => unreachable!(),
            }
        } else if let Some(m) = punctuation_double_re.find(src) {
            self.pos += m.as_str().len();
            match m.as_str() {
                "--" => Token::DoubleHyphen,
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
    Hyphen,
    Tilde,
    DoubleHyphen,
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
    Unary(UnaryExpression),
}

#[derive(Debug, Clone, PartialEq)]
struct UnaryExpression {
    kind: UnaryExpressoinKind,
    expr: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
enum UnaryExpressoinKind {
    Negate,
    Complement,
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
    Unary { op: AsmUnaryOp, operand: AsmOperand },
    AllocateStack(usize),
    Ret,
}

#[derive(Debug, Clone, PartialEq)]
enum AsmOperand {
    Imm(i32),
    Pseudo(String),
    Stack(i32),
    Register(Register),
}

#[derive(Debug, Clone, PartialEq)]
enum Register {
    AX,
    R10,
}

#[derive(Debug, Clone, PartialEq, Copy)]
enum AsmUnaryOp {
    Neg,
    Not,
}

fn codegen(tac: IRNode) -> Result<AsmProgram> {
    if let AsmNode::Program(prog) = tac.codegen()? {
        Ok(prog)
    } else {
        unreachable!()
    }
}

trait Emit {
    fn emit(&mut self, f: &mut File) -> Result<()>;
}

impl Emit for AsmProgram {
    fn emit(&mut self, f: &mut File) -> Result<()> {
        for func in self.functions.iter_mut() {
            if let Some(instr) = func.instructions.get_mut(0) {
                let offset_manager = OFFSET_MANAGER.lock().unwrap();
                *instr = AsmInstruction::AllocateStack(offset_manager.offset.unsigned_abs() as usize + 4);
            }

            writeln!(f, ".globl {}", func.name)?;
            writeln!(f, "{}:", func.name)?;

            writeln!(f, "  pushq %rbp")?;
            writeln!(f, "  movq %rsp, %rbp")?;

            for instr in &mut func.instructions {
                instr.emit(f)?;
            }
        }

        Ok(())
    }
}

impl Emit for AsmInstruction {
    fn emit(&mut self, f: &mut File) -> Result<()> {
        write!(f, "  ")?;

        match self {
            AsmInstruction::Mov { src, dst } => {
                write!(f, "movl ")?;
                src.emit(f)?;
                write!(f, ", ")?;
                dst.emit(f)?;
            }
            AsmInstruction::Ret => {
                writeln!(f, "movq %rbp, %rsp")?;
                writeln!(f, "popq %rbp")?;
                write!(f, "ret")?;
            }
            AsmInstruction::Unary { op, operand } => {
                match op {
                    AsmUnaryOp::Neg => write!(f, "negl ")?,
                    AsmUnaryOp::Not => write!(f, "notl ")?,
                }
                operand.emit(f)?;
                writeln!(f)?;
            }
            AsmInstruction::AllocateStack(n) => {
                writeln!(f, "sub ${}, %rsp", n)?;
            }
        }

        writeln!(f)?;

        Ok(())
    }
}

impl Emit for AsmOperand {
    fn emit(&mut self, f: &mut File) -> Result<()> {
        match self {
            AsmOperand::Imm(n) => write!(f, "${}", n)?,
            AsmOperand::Register(reg) => reg.emit(f)?,
            AsmOperand::Stack(n) => write!(f, "{}(%rbp)", n)?,
            AsmOperand::Pseudo(_) => unreachable!(),
        }
        Ok(())
    }
}

impl Emit for Register {
    fn emit(&mut self, f: &mut File) -> Result<()> {
        match self {
            Register::AX => write!(f, "%eax")?,
            Register::R10 => write!(f, "%r10d")?,
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
struct IRProgram {
    functions: Vec<IRFunction>,
}

#[derive(Debug, Clone, PartialEq)]
struct IRFunction {
    name: String,
    body: Vec<IRInstruction>,
}

#[derive(Debug, Clone, PartialEq)]
enum IRInstruction {
    Unary {
        op: UnaryOp,
        src: IRValue,
        dst: IRValue,
    },
    Ret(IRValue),
}

#[derive(Debug, Clone, PartialEq)]
enum IRValue {
    Constant(i32),
    Var(String),
}

#[derive(Debug, Clone, PartialEq)]
enum UnaryOp {
    Negate,
    Complement,
}

fn emit_tacky(e: Expression, instructions: &mut Vec<IRInstruction>) -> IRValue {
    match e {
        Expression::Constant(konst) => IRValue::Constant(konst),
        Expression::Unary(UnaryExpression { kind, expr }) => {
            let src = emit_tacky(*expr, instructions);
            let dst_name = format!("var.{}", make_temporary());
            let dst = IRValue::Var(dst_name.clone());
            let op = match kind {
                UnaryExpressoinKind::Negate => UnaryOp::Negate,
                UnaryExpressoinKind::Complement => UnaryOp::Complement,
            };
            instructions.push(IRInstruction::Unary {
                op,
                src,
                dst: dst.clone(),
            });

            dst
        }
    }
}

fn make_temporary() -> usize {
    static mut TEMPORARY: usize = 0;
    unsafe {
        let temporary = TEMPORARY;
        TEMPORARY += 1;
        temporary
    }
}

#[derive(Debug, Clone, PartialEq)]
enum IRNode {
    Program(IRProgram),
    Function(IRFunction),
    Instructions(Vec<IRInstruction>),
}

trait Irfy {
    fn irfy(&self) -> IRNode;
}

impl Irfy for Statement {
    fn irfy(&self) -> IRNode {
        match self {
            Statement::Program(prog) => prog.irfy(),
            Statement::Function(func) => func.irfy(),
            Statement::Return(expr) => {
                let mut instructions = vec![];

                let expr = emit_tacky(expr.clone(), &mut instructions);
                instructions.push(IRInstruction::Ret(expr));

                IRNode::Instructions(instructions)
            }
        }
    }
}

impl Irfy for ProgramStatement {
    fn irfy(&self) -> IRNode {
        let mut functions = vec![];
        for func in &self.stmts {
            functions.push(match func.irfy() {
                IRNode::Function(func) => func,
                _ => unreachable!(),
            });
        }
        IRNode::Program(IRProgram { functions })
    }
}

impl Irfy for FunctionDeclaration {
    fn irfy(&self) -> IRNode {
        let mut instructions = vec![];
        for stmt in &self.stmts {
            instructions.extend(match stmt.irfy() {
                IRNode::Instructions(instrs) => instrs,
                _ => unreachable!(),
            });
        }
        IRNode::Function(IRFunction {
            name: self.name.clone(),
            body: instructions,
        })
    }
}

enum AsmNode {
    Program(AsmProgram),
    Function(AsmFunction),
    Operand(AsmOperand),
    Instructions(Vec<AsmInstruction>),
}

trait Codegen {
    fn codegen(&self) -> Result<AsmNode>;
}

impl Codegen for IRNode {
    fn codegen(&self) -> Result<AsmNode> {
        match self {
            IRNode::Program(prog) => prog.codegen(),
            IRNode::Function(func) => func.codegen(),
            IRNode::Instructions(instrs) => instrs.codegen(),
        }
    }
}

impl Codegen for IRProgram {
    fn codegen(&self) -> Result<AsmNode> {
        let mut functions = vec![];
        for func in &self.functions {
            functions.push(match func.codegen()? {
                AsmNode::Function(func) => func,
                _ => unreachable!(),
            });
        }
        Ok(AsmNode::Program(AsmProgram { functions }))
    }
}

impl Codegen for IRFunction {
    fn codegen(&self) -> Result<AsmNode> {
        let mut instructions = vec![];
        
        instructions.push(AsmInstruction::AllocateStack(0));
        
        for instr in &self.body {
            instructions.extend(match instr.codegen()? {
                AsmNode::Instructions(instrs) => instrs,
                _ => unreachable!(),
            });
        }
        Ok(AsmNode::Function(AsmFunction {
            name: self.name.clone(),
            instructions,
        }))
    }
}

impl Codegen for Vec<IRInstruction> {
    fn codegen(&self) -> Result<AsmNode> {
        let mut instructions = vec![];
        for instr in self {
            instructions.extend(match instr.codegen()? {
                AsmNode::Instructions(instrs) => instrs,
                _ => unreachable!(),
            });
        }
        Ok(AsmNode::Instructions(instructions))
    }
}

impl Codegen for IRValue {
    fn codegen(&self) -> Result<AsmNode> {
        match self {
            IRValue::Constant(n) => Ok(AsmNode::Operand(AsmOperand::Imm(*n))),
            IRValue::Var(name) => Ok(AsmNode::Operand(AsmOperand::Pseudo(name.to_owned()))),
        }
    }
}

impl Codegen for IRInstruction {
    fn codegen(&self) -> Result<AsmNode> {
        match self {
            IRInstruction::Unary { op, src, dst } => Ok(AsmNode::Instructions(vec![
                AsmInstruction::Mov {
                    src: match src.codegen()? {
                        AsmNode::Operand(op) => op,
                        _ => unreachable!(),
                    },
                    dst: match dst.codegen()? {
                        AsmNode::Operand(op) => op,
                        _ => unreachable!(),
                    },
                },
                AsmInstruction::Unary {
                    op: match op {
                        UnaryOp::Negate => AsmUnaryOp::Neg,
                        UnaryOp::Complement => AsmUnaryOp::Not,
                    },
                    operand: match dst.codegen()? {
                        AsmNode::Operand(op) => op,
                        _ => unreachable!(),
                    },
                },
            ])),
            IRInstruction::Ret(value) => Ok(AsmNode::Instructions(vec![
                AsmInstruction::Mov {
                    src: match value.codegen()? {
                        AsmNode::Operand(op) => op,
                        _ => unreachable!(),
                    },
                    dst: AsmOperand::Register(Register::AX),
                },
                AsmInstruction::Ret,
            ])),
        }
    }
}

// our task is to replace pseudo registers with stack slots
trait ReplacePseudo {
    fn replace_pseudo(&self) -> Self;
}

impl ReplacePseudo for AsmInstruction {
    fn replace_pseudo(&self) -> Self {
        match self {
            AsmInstruction::Mov { src, dst } => AsmInstruction::Mov {
                src: src.replace_pseudo(),
                dst: dst.replace_pseudo(),
            },
            AsmInstruction::Unary { op, operand } => AsmInstruction::Unary {
                op: *op,
                operand: operand.replace_pseudo(),
            },
            AsmInstruction::AllocateStack(n) => AsmInstruction::AllocateStack(*n),
            AsmInstruction::Ret => AsmInstruction::Ret,
        }
    }
}

lazy_static::lazy_static! {
    static ref OFFSET_MANAGER: std::sync::Mutex<OffsetManager> = std::sync::Mutex::new(OffsetManager::new());
}

struct OffsetManager {
    offsets: std::collections::HashMap<String, i32>,
    offset: i32,
}

impl OffsetManager {
    fn new() -> OffsetManager {
        OffsetManager {
            offsets: std::collections::HashMap::new(),
            offset: -4,
        }
    }

    fn get_offset(&mut self, name: &str) -> i32 {
        if !self.offsets.contains_key(name) {
            self.offsets.insert(name.to_owned(), self.offset);
            self.offset -= 4;
        }

        self.offsets[name]
    }
}

impl ReplacePseudo for AsmOperand {
    fn replace_pseudo(&self) -> Self {
        match self {
            AsmOperand::Imm(_) => self.clone(),
            AsmOperand::Pseudo(name) => {
                let mut offset_manager = OFFSET_MANAGER.lock().unwrap();
                AsmOperand::Stack(offset_manager.get_offset(name))
            }
            AsmOperand::Stack(_) => self.clone(),
            AsmOperand::Register(_) => self.clone(),
        }
    }
}

impl ReplacePseudo for AsmProgram {
    fn replace_pseudo(&self) -> Self {
        let mut functions = vec![];
        for func in &self.functions {
            functions.push(func.replace_pseudo());
        }
        AsmProgram { functions }
    }
}

impl ReplacePseudo for AsmFunction {
    fn replace_pseudo(&self) -> Self {
        let mut instructions = vec![];
        for instr in &self.instructions {
            instructions.push(instr.replace_pseudo());
        }
        AsmFunction {
            name: self.name.clone(),
            instructions,
        }
    }
}

trait Fixup {
    fn fixup(&mut self) -> Self;
}

impl Fixup for AsmProgram {
    fn fixup(&mut self) -> AsmProgram {
        let mut functions = vec![];

        for func in &mut self.functions {
            let mut instructions = vec![];
            
            for instr in &mut func.instructions {
                match instr {
                    AsmInstruction::Mov { src, dst } => {
                        match (src, dst) {
                            (AsmOperand::Stack(src_n), AsmOperand::Stack(dst_n)) => {
                                instructions.extend(vec![
                                    AsmInstruction::Mov {
                                        src: AsmOperand::Stack(*src_n),
                                        dst: AsmOperand::Register(Register::R10),
                                    },
                                    AsmInstruction::Mov {
                                        src: AsmOperand::Register(Register::R10),
                                        dst: AsmOperand::Stack(*dst_n),
                                    },
                                ]);
                            }
                            _ => instructions.push(instr.clone()),
                        }
                    }
                    _ => instructions.push(instr.clone()),
                }
            }

            functions.push(AsmFunction {
                name: func.name.clone(),
                instructions,
            });
        }

        AsmProgram { functions }
    }
}
