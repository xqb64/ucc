use std::collections::HashMap;
use std::{collections::VecDeque, fs::File, path::PathBuf};

use anyhow::{bail, Result};
use structopt::StructOpt;

use ucc::codegen::{build_asm_symbol_table, AsmType, Codegen, Fixup, ReplacePseudo};
use ucc::emitter::Emit;
use ucc::ir::{convert_symbols_to_tacky, IRNode, Irfy};
use ucc::lexer::{Lexer, Token};
use ucc::loop_label::LoopLabel;
use ucc::parser::Parser;
use ucc::resolver::Resolve;
use ucc::typechecker::Typecheck;

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
    let mut raw_ast = parser.parse()?;

    if opts.parse {
        println!("{:#?}", raw_ast);
        std::process::exit(0);
    }

    let mut variable_map = HashMap::new();

    let cooked_ast = raw_ast
        .resolve(&mut variable_map)?
        .loop_label("")?
        .typecheck()?;

    if opts.validate {
        println!("{:#?}", cooked_ast);
        std::process::exit(0);
    }

    let mut tac = cooked_ast.irfy().unwrap();
    let tacky_defs = convert_symbols_to_tacky();

    if let IRNode::Program(prog) = &mut tac {
        prog.static_vars = tacky_defs;
    }

    if opts.tacky {
        println!("tac: {:#?}", tac);
        std::process::exit(0);
    }

    build_asm_symbol_table();

    let mut asm_prog = tac.codegen().replace_pseudo().fixup();

    if opts.codegen {
        println!("{:#?}", asm_prog);
        std::process::exit(0);
    }

    let mut f = File::create(opts.path.with_extension("s"))?;

    asm_prog.emit(&mut f, &mut AsmType::Longword)?;

    if opts.c {
        std::process::Command::new("gcc")
            .arg("-c")
            .arg(opts.path.with_extension("s"))
            .arg("-o")
            .arg(opts.path.with_extension("o"))
            .status()?;
        std::process::exit(0);
    }

    let mut final_executable_cmd = std::process::Command::new("gcc");

    final_executable_cmd
        .arg("-o")
        .arg(opts.path.with_extension(""))
        .arg(opts.path.with_extension("s"));

    if let Some(ref lib) = opts.l {
        final_executable_cmd.arg("-l").arg(lib);
    }

    final_executable_cmd.status()?;

    Ok(())
}

fn preprocess(path: &PathBuf) -> Result<PathBuf> {
    let new_path = path.with_extension("i");

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

    #[structopt(name = "validate", long)]
    validate: bool,

    #[structopt(name = "tacky", long)]
    tacky: bool,

    #[structopt(name = "codegen", long)]
    codegen: bool,

    #[structopt(name = "c", short)]
    c: bool,

    #[structopt(name = "l", short)]
    l: Option<String>,
}
